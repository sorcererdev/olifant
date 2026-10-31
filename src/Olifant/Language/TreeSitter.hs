{-# LANGUAGE TemplateHaskell #-}

-- | Tree-sitter in process: the runtime and the grammars are C compiled into
-- the binary (@cbits/@, pinned to the versions ast-grep locks), reached
-- through a small shim (@cbits/olifant_ts.c@) that holds nodes by pointer.
-- 'capturesOf' parses a text, runs a grammar's queries over it (the
-- vendored and olifant-owned @library/queries/<grammar>/*.scm@) and returns
-- every capture as plain data ('Cap').
--
-- The read is pure: a parse and a query run are functions of (grammar,
-- queries, text), so 'capturesOf' wraps its IO in 'unsafePerformIO'.
module Olifant.Language.TreeSitter
  ( capturesOf
  , cacheSize
  , linkedGrammars
  , grammarNodeTypes
  , queriesFor
  , shippedQueries
  , treeSitterAnchors
  ) where

import Control.Concurrent.MVar (MVar, modifyMVar, newMVar)
import Control.Exception (bracket)
import Control.Monad (forM, when)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BU
import Data.FileEmbed (embedDir)
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef)
import Data.Maybe (listToMaybe)
import Data.List (sortOn)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Word (Word16, Word32, Word8)
import Data.Bits ((.&.))
import Foreign.C.String (CString, newCStringLen, peekCString, peekCStringLen)
import Foreign.C.Types (CInt (..), CSize (..))
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Array (allocaArray, peekArray, withArray)
import Foreign.Marshal.Utils (copyBytes, with)
import Foreign.Ptr (FunPtr, Ptr, castPtr, nullPtr, plusPtr)
import Foreign.Storable (peek, peekElemOff)
import Olifant.Language.Adapter (Grammar (..), Language (..))
import Olifant.Language.Captures (Cap (..), PNode (..), anchorFromCaptures, genericPhrase, phraseKind)
import Olifant.Language.Syntax (Match, SyntaxFailure (..))
import System.FilePath (splitDirectories, takeExtension)
import System.IO.Unsafe (unsafePerformIO)

data TSLanguage
data TSTree
data TSNode
data TSQuery
data TSQueryCursor

-- the grammars compiled in (cbits/tree-sitter-<grammar>/)
foreign import ccall "&tree_sitter_rust" p_rust :: FunPtr (IO (Ptr TSLanguage))
foreign import ccall "dynamic" callLanguage :: FunPtr (IO (Ptr TSLanguage)) -> IO (Ptr TSLanguage)

foreign import ccall unsafe "ts_language_symbol_count" c_symbolCount :: Ptr TSLanguage -> IO Word32
foreign import ccall unsafe "ts_language_symbol_name" c_symbolName :: Ptr TSLanguage -> Word16 -> IO CString
foreign import ccall unsafe "ts_language_symbol_type" c_symbolType :: Ptr TSLanguage -> Word16 -> IO CInt
foreign import ccall unsafe "ots_node_size" c_nodeSize :: IO CSize
foreign import ccall safe "ots_parse" c_parse :: Ptr TSLanguage -> CString -> Word32 -> IO (Ptr TSTree)
foreign import ccall unsafe "ts_tree_delete" c_treeDelete :: Ptr TSTree -> IO ()
foreign import ccall unsafe "ots_root" c_root :: Ptr TSTree -> Ptr TSNode -> IO ()
foreign import ccall unsafe "ots_parent" c_parent :: Ptr TSNode -> Ptr TSNode -> IO ()
foreign import ccall unsafe "ots_field" c_field :: Ptr TSNode -> CString -> Word32 -> Ptr TSNode -> IO ()
foreign import ccall unsafe "ots_child_count" c_childCount :: Ptr TSNode -> IO Word32
foreign import ccall unsafe "ots_child" c_child :: Ptr TSNode -> Word32 -> Ptr TSNode -> IO ()
foreign import ccall safe "ots_query" c_query :: Ptr TSLanguage -> CString -> Word32 -> Ptr Word32 -> Ptr Word32 -> IO (Ptr TSQuery)
foreign import ccall unsafe "ots_capture_name" c_captureName :: Ptr TSQuery -> Word32 -> Ptr Word32 -> IO CString
foreign import ccall unsafe "ts_query_pattern_count" c_patternCount :: Ptr TSQuery -> IO Word32
foreign import ccall unsafe "ts_query_delete" c_queryDelete :: Ptr TSQuery -> IO ()
foreign import ccall unsafe "ots_capture_count" c_captureCount :: Ptr TSQuery -> IO Word32
foreign import ccall unsafe "ots_prune" c_prune :: Ptr TSQuery -> Ptr Word8 -> Ptr Word8 -> IO ()
foreign import ccall unsafe "ots_facts" c_facts :: Ptr TSNode -> Ptr Word32 -> IO ()
foreign import ccall unsafe "ots_exec" c_exec :: Ptr TSQuery -> Ptr TSNode -> IO (Ptr TSQueryCursor)
foreign import ccall unsafe "ts_query_cursor_delete" c_cursorDelete :: Ptr TSQueryCursor -> IO ()
foreign import ccall unsafe "ots_next" c_next :: Ptr TSQueryCursor -> Word32 -> Ptr TSNode -> Ptr Word32 -> Ptr Word32 -> IO CInt

-- | The grammar table: a language file's `grammar` to its compiled-in
-- parser. Data, keyed by grammar, never by a language's name.
grammarTable :: [(Text, FunPtr (IO (Ptr TSLanguage)))]
grammarTable = [("rust", p_rust)]

-- | The grammars linked into this binary.
linkedGrammars :: [Text]
linkedGrammars = map fst grammarTable

-- | A linked grammar's named node types (none for a grammar not linked).
grammarNodeTypes :: Text -> [Text]
grammarNodeTypes g = M.findWithDefault [] g nodeTypesTable

-- | Read once per process.
nodeTypesTable :: M.Map Text [Text]
{-# NOINLINE nodeTypesTable #-}
nodeTypesTable = M.fromList [(g, unsafePerformIO (nodeTypes fp)) | (g, fp) <- grammarTable]
  where
    nodeTypes fp = do
      lang <- callLanguage fp
      count <- c_symbolCount lang
      fmap (nubOrd . concat) . forM (if count == 0 then [] else [0 .. fromIntegral count - 1]) $ \i -> do
        ty <- c_symbolType lang i
        -- 0: a regular (named) node type
        if ty /= 0 then pure [] else (\n -> [T.pack n]) <$> (c_symbolName lang i >>= peekCString)
    nubOrd = M.keys . M.fromList . map (,())

-- | The generic anchor rule for a language whose grammar is linked in (the
-- 'Olifant.Language.Syntax.anchorSyntax' of the real syntax): 'Nothing' when
-- the grammar is not linked, the phrase is not a generic one, or its kind
-- word names no node type of the grammar (`end of the imports`), so a
-- provider or "unknown target" answers instead.
treeSitterAnchors :: Applicative m => Language -> Maybe FilePath -> [Text] -> Text -> m (Maybe (Either SyntaxFailure [(Match, [Match])]))
treeSitterAnchors lang _ ws source = pure $ case langGrammar lang of
  BuiltInGrammar g
    | g `elem` linkedGrammars
    , genericPhrase ws
    , maybe True (kindKnown g) (phraseKind ws) ->
        case capturesOf g (queriesFor g) source of
          Left why -> Just (Left (SyntaxFailed why))
          Right (root, caps) -> first SyntaxFailed <$> anchorFromCaptures root caps ws
  _ -> Nothing
  where
    kindKnown g k = any (\t -> t == k || (k <> "_") `T.isPrefixOf` t) (grammarNodeTypes g)

-- | The query files shipped inside the binary: (grammar, file name, source).
shippedQueries :: [(Text, FilePath, Text)]
shippedQueries =
  sortOn (\(g, f, _) -> (g, f))
    [ (T.pack g, f, TE.decodeUtf8 b)
    | (path, b) <- $(embedDir "library/queries")
    , [g, f] <- [splitDirectories path]
    , takeExtension f == ".scm"
    ]

-- | A grammar's shipped query set: (file name, source).
queriesFor :: Text -> [(FilePath, Text)]
queriesFor g = [(f, s) | (g', f, s) <- shippedQueries, g' == g]

-- | The captures the anchor rule reads (and every @definition.*@). The
-- others are pruned from each compiled query, so they cost nothing.
-- @reference.implementation@ is not read: tags.scm names a trait impl by its
-- trait; impls.scm gives an inherent impl as @definition.impl@.
readCaptures :: [Text]
readCaptures = ["name", "function.inner", "class.inner", "parameter.inner", "call.outer", "import"]

-- | A query set compiled as one query (one walk of the tree for all of
-- them), its capture names, and each file's first pattern index.
type Compiled = (Ptr TSQuery, M.Map Word32 Text, [(Int, FilePath)])

-- | Each (grammar, query set), compiled once per process.
compiled :: MVar (M.Map (Text, [(FilePath, Text)]) (Either Text Compiled))
compiled = unsafePerformIO (newMVar M.empty)
{-# NOINLINE compiled #-}

compileQueries :: Ptr TSLanguage -> Text -> [(FilePath, Text)] -> IO (Either Text Compiled)
compileQueries lang g qs = modifyMVar compiled $ \cache -> case M.lookup (g, qs) cache of
  Just r -> pure (cache, r)
  Nothing -> do
    -- each file alone first, so an error names its file and byte
    counts <- fmap sequence . forM qs $ \(f, src) ->
      compileOne src >>= \case
        Left (o, t) -> pure (Left ("`" <> T.pack f <> "`: error at byte " <> T.pack (show o) <> " (" <> errorKind t <> ")"))
        Right q -> do
          n <- c_patternCount q
          c_queryDelete q
          pure (Right (fromIntegral n :: Int))
    r <- case counts of
      Left why -> pure (Left why)
      Right ns ->
        compileOne (T.intercalate "\n" (map snd qs)) >>= \case
          Left (o, t) -> pure (Left ("the query set: error at byte " <> T.pack (show o) <> " (" <> errorKind t <> ")"))
          Right q -> do
            names <- captureNames q
            let keep c = c `elem` readCaptures || "definition." `T.isPrefixOf` c
                flag b = if b then 1 else 0
                flags k = [flag (k (M.findWithDefault "" i names)) | i <- M.keys names]
            -- a bare @name keeps no pattern (tags.scm's references have one)
            withArray (flags keep) $ \kp -> withArray (flags (\c -> keep c && c /= "name")) (c_prune q kp)
            pure (Right (q, M.filter keep names, zip (scanl (+) 0 ns) (map fst qs)))
    pure (M.insert (g, qs) r cache, r)
  where
    compileOne src =
      BU.unsafeUseAsCStringLen (TE.encodeUtf8 src) $ \(p, n) ->
        with 0 $ \off -> with 0 $ \ty -> do
          q <- c_query lang p (fromIntegral n) off ty
          if q == nullPtr then Left <$> ((,) <$> peek off <*> peek ty) else pure (Right q)
    captureNames q = do
      count <- c_captureCount q
      fmap M.fromList . forM (if count == 0 then [] else [0 .. count - 1]) $ \i -> with 0 $ \len -> do
        str <- c_captureName q i len
        n <- peek len
        name <- peekCStringLen (str, fromIntegral n)
        pure (i, T.pack name)
    errorKind :: Word32 -> Text
    errorKind = \case
      1 -> "syntax"
      2 -> "node type"
      3 -> "field"
      4 -> "capture"
      5 -> "structure"
      6 -> "language"
      k -> "error " <> T.pack (show k)

-- | The last 'cacheBound' answers, most recent first, keyed by (grammar,
-- query set, text): the LSP asks again and again about one document, and a
-- request on the same text skips the parse and the marshalling. A changed
-- text is another key, so a stale tree never answers.
type Key = (Text, [(FilePath, Text)], Text)

answers :: IORef [(Key, Either Text (PNode, [Cap]))]
answers = unsafePerformIO (newIORef [])
{-# NOINLINE answers #-}

cacheBound :: Int
cacheBound = 16

-- | How many answers the cache holds (at most 'cacheBound').
cacheSize :: IO Int
cacheSize = length <$> readIORef answers

-- | Every capture of a query set over a text, with the node facts the anchor
-- rule reads, and the root node. Left: the grammar is not linked, or a query
-- does not compile. Pure: see the module header.
capturesOf :: Text -> [(FilePath, Text)] -> Text -> Either Text (PNode, [Cap])
capturesOf g qs content = unsafePerformIO $ do
  let key = (g, qs, content)
      -- the text first: a different text differs in length or bytes fast
      same (g', qs', t') = t' == content && g' == g && qs' == qs
  cached <- lookupBy same <$> readIORef answers
  case cached of
    Just r -> pure r
    Nothing -> do
      r <- capturesIO g qs content
      atomicModifyIORef' answers (\as -> (take cacheBound ((key, r) : filter (not . same . fst) as), ()))
      pure r
  where
    lookupBy p = fmap snd . listToMaybe . filter (p . fst)
{-# NOINLINE capturesOf #-}

capturesIO :: Text -> [(FilePath, Text)] -> Text -> IO (Either Text (PNode, [Cap]))
capturesIO g qs content = case lookup g grammarTable of
  Nothing -> pure (Left ("no tree-sitter grammar linked for `" <> g <> "` (linked: " <> T.intercalate ", " linkedGrammars <> ")"))
  Just fp -> do
    lang <- callLanguage fp
    compileQueries lang g qs >>= \case
      Left why -> pure (Left why)
      Right cqs -> do
        size <- fromIntegral <$> c_nodeSize
        let bytes = TE.encodeUtf8 content
            ctx = Ctx bytes size (M.findWithDefault M.empty g symbolTable)
        BU.unsafeUseAsCStringLen bytes $ \(p, n) ->
          bracket (c_parse lang p (fromIntegral n)) (\t -> when (t /= nullPtr) (c_treeDelete t)) $ \tree ->
            if tree == nullPtr
              then pure (Left ("tree-sitter could not take the `" <> g <> "` grammar (ABI)"))
              else allocaBytes size $ \root -> allocaArray 8 $ \facts -> do
                c_root tree root
                out <- newIORef []
                matchNo <- newIORef (0 :: Int)
                runQuery ctx facts root out matchNo cqs
                rootNode <- pnode ctx facts root
                Right . (rootNode,) . reverse <$> readIORef out
  where
    runQuery ctx facts root out matchNo (q, names, starts) =
      -- a pattern's file, and its index in that file
      let fileOf ix = case [(T.pack f, ix - s0) | (s0, f) <- reverse starts, s0 <= ix] of
            r : _ -> r
            [] -> ("", ix)
       in bracket (c_exec q root) c_cursorDelete $ \cur ->
            allocaBytes (ctxSize ctx * 64) $ \nodes -> allocaArray 64 $ \ids -> with 0 $ \pat ->
              let loop = do
                    k <- c_next cur 64 nodes ids pat
                    when (k >= 0) $ do
                      pIx <- peek pat
                      mi <- readIORef matchNo
                      modifyIORef' matchNo (+ 1)
                      is <- peekArray (fromIntegral k) ids
                      let (file, local) = fileOf (fromIntegral pIx)
                      caps <- forM (zip [0 ..] is) $ \(j, cid) ->
                        capture ctx facts root (nodes `plusPtr` (j * ctxSize ctx)) (M.findWithDefault "" cid names) file local mi
                      modifyIORef' out (reverse caps ++)
                      loop
               in loop

-- | What marshalling a node needs: the text's bytes, a node's size, and the
-- grammar's symbol names.
data Ctx = Ctx {ctxBytes :: ByteString, ctxSize :: Int, ctxSymbols :: M.Map Word32 Text}

-- | Each linked grammar's symbol names (named and anonymous), read once.
symbolTable :: M.Map Text (M.Map Word32 Text)
symbolTable = M.fromList [(g, unsafePerformIO (symbols fp)) | (g, fp) <- grammarTable]
  where
    symbols fp = do
      lang <- callLanguage fp
      count <- c_symbolCount lang
      fmap M.fromList . forM (if count == 0 then [] else [0 .. count - 1]) $ \i ->
        (i,) . T.pack <$> (c_symbolName lang (fromIntegral i) >>= peekCString)
{-# NOINLINE symbolTable #-}

-- | The field names the rule reads, as C strings, made once.
fieldNames :: [(Text, (CString, Int))]
fieldNames = unsafePerformIO (forM ["body", "parameters", "arguments"] (\f -> (f,) <$> newCStringLen (T.unpack f)))
{-# NOINLINE fieldNames #-}

-- | One capture's facts: the node, its parent, its top-level ancestor (a
-- direct child of the root, read for imports) and the fields the rule reads.
capture :: Ctx -> Ptr Word32 -> Ptr TSNode -> Ptr TSNode -> Text -> Text -> Int -> Int -> IO Cap
capture ctx facts root node name file pIx mi = do
  n <- pnode ctx facts node
  -- the parent only where the rule reads it: an inner node's container
  parent <- if ".inner" `T.isSuffixOf` name then allocaBytes size (\p -> c_parent node p >> pnodeMaybe ctx facts p) else pure Nothing
  top <- if name == "import" then topLevel else pure Nothing
  -- fields only where the rule reads them: definitions and calls
  let wanted
        | "definition." `T.isPrefixOf` name = ["body", "parameters"]
        | name == "call.outer" = ["arguments"]
        | otherwise = []
  fields <- fmap concat . forM [f | f@(fname, _) <- fieldNames, fname `elem` wanted] $ \(fname, (s, l)) ->
    allocaBytes size $ \fp -> do
      c_field node s (fromIntegral l) fp
      pnodeMaybe ctx facts fp >>= \case
        Nothing -> pure []
        -- a body's children are not read (its items are the inner
        -- captures), and are many: only a list's are marshalled
        Just fnode -> (\kids -> [(fname, fnode, kids)]) <$> (if fname == "body" then pure [] else children fp)
  pure (Cap name file pIx mi n parent top fields)
  where
    size = ctxSize ctx
    -- named, non-extra children
    children p = do
      cnt <- c_childCount p
      allocaBytes size $ \c ->
        fmap concat . forM (if cnt == 0 then [] else [0 .. cnt - 1]) $ \i -> do
          c_child p i c
          k <- pnode ctx facts c
          pure [k | pNamed k, not (pExtra k)]
    topLevel = do
      r <- pnode ctx facts root
      allocaBytes size $ \cur -> allocaBytes size $ \par -> do
        copyNode node cur
        let climb (d :: Int) = do
              c_parent cur par
              pnodeMaybe ctx facts par >>= \case
                Nothing -> pure Nothing
                Just pn
                  | d > 10000 -> pure Nothing
                  | pBytes pn == pBytes r && pType pn == pType r -> Just <$> pnode ctx facts cur
                  | otherwise -> copyNode par cur >> climb (d + 1)
        climb 0
    copyNode from to = copyBytes (castPtr to :: Ptr Word8) (castPtr from) size

pnodeMaybe :: Ctx -> Ptr Word32 -> Ptr TSNode -> IO (Maybe PNode)
pnodeMaybe ctx facts p = do
  c_facts p facts
  flags <- peekElemOff facts 7
  if flags .&. 4 /= 0 then pure Nothing else Just <$> fromFacts ctx facts

pnode :: Ctx -> Ptr Word32 -> Ptr TSNode -> IO PNode
pnode ctx facts p = c_facts p facts >> fromFacts ctx facts

-- | A node's facts, read in one call ('c_facts'). Tree-sitter counts columns
-- in bytes; they become characters here, as ast-grep reports them (lazily,
-- as is the text: most nodes are compared, never shown).
fromFacts :: Ctx -> Ptr Word32 -> IO PNode
fromFacts ctx facts = do
  let at i = fromIntegral <$> peekElemOff facts i :: IO Int
  s <- at 0
  e <- at 1
  sr <- at 2
  sc <- at 3
  er <- at 4
  ec <- at 5
  sym <- peekElemOff facts 6
  flags <- peekElemOff facts 7
  let bytes = ctxBytes ctx
      col row byteCol = T.length (TE.decodeUtf8Lenient (BS.take byteCol (BS.drop (lineStart row) bytes)))
      text = TE.decodeUtf8Lenient (BS.take (e - s) (BS.drop s bytes))
      -- the byte offset where row's line starts, from the node's own start
      lineStart row = if row == sr then s - sc else e - ec
  pure (PNode (M.findWithDefault "" sym (ctxSymbols ctx)) (sr, col sr sc) (er, col er ec) (s, e) text (flags .&. 1 /= 0) (flags .&. 2 /= 0))
