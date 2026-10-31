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
  , linkedGrammars
  , queriesFor
  , shippedQueries
  ) where

import Control.Concurrent.MVar (MVar, modifyMVar, newMVar)
import Control.Exception (bracket)
import Control.Monad (forM, when)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BU
import Data.FileEmbed (embedDir)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List (sortOn)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Word (Word32, Word8)
import Foreign.C.String (CString, peekCString, peekCStringLen, withCStringLen)
import Foreign.C.Types (CInt (..), CSize (..))
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Array (allocaArray, peekArray, withArray)
import Foreign.Marshal.Utils (copyBytes, with)
import Foreign.Ptr (FunPtr, Ptr, castPtr, nullPtr, plusPtr)
import Foreign.Storable (peek)
import Olifant.Language.Captures (Cap (..), PNode (..))
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

foreign import ccall unsafe "ots_node_size" c_nodeSize :: IO CSize
foreign import ccall safe "ots_parse" c_parse :: Ptr TSLanguage -> CString -> Word32 -> IO (Ptr TSTree)
foreign import ccall unsafe "ts_tree_delete" c_treeDelete :: Ptr TSTree -> IO ()
foreign import ccall unsafe "ots_root" c_root :: Ptr TSTree -> Ptr TSNode -> IO ()
foreign import ccall unsafe "ots_start_byte" c_startByte :: Ptr TSNode -> IO Word32
foreign import ccall unsafe "ots_end_byte" c_endByte :: Ptr TSNode -> IO Word32
foreign import ccall unsafe "ots_points" c_points :: Ptr TSNode -> Ptr Word32 -> IO ()
foreign import ccall unsafe "ots_is_null" c_isNull :: Ptr TSNode -> IO CInt
foreign import ccall unsafe "ots_is_named" c_isNamed :: Ptr TSNode -> IO CInt
foreign import ccall unsafe "ots_is_extra" c_isExtra :: Ptr TSNode -> IO CInt
foreign import ccall unsafe "ots_type" c_type :: Ptr TSNode -> IO CString
foreign import ccall unsafe "ots_parent" c_parent :: Ptr TSNode -> Ptr TSNode -> IO ()
foreign import ccall unsafe "ots_field" c_field :: Ptr TSNode -> CString -> Word32 -> Ptr TSNode -> IO ()
foreign import ccall unsafe "ots_child_count" c_childCount :: Ptr TSNode -> IO Word32
foreign import ccall unsafe "ots_child" c_child :: Ptr TSNode -> Word32 -> Ptr TSNode -> IO ()
foreign import ccall safe "ots_query" c_query :: Ptr TSLanguage -> CString -> Word32 -> Ptr Word32 -> Ptr Word32 -> IO (Ptr TSQuery)
foreign import ccall unsafe "ots_capture_name" c_captureName :: Ptr TSQuery -> Word32 -> Ptr Word32 -> IO CString
foreign import ccall unsafe "ots_capture_count" c_captureCount :: Ptr TSQuery -> IO Word32
foreign import ccall unsafe "ots_prune" c_prune :: Ptr TSQuery -> Ptr Word8 -> IO ()
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

type Compiled = [(FilePath, Ptr TSQuery, M.Map Word32 Text)]

-- | Each (grammar, query set), compiled once per process.
compiled :: MVar (M.Map (Text, [(FilePath, Text)]) (Either Text Compiled))
compiled = unsafePerformIO (newMVar M.empty)
{-# NOINLINE compiled #-}

compileQueries :: Ptr TSLanguage -> Text -> [(FilePath, Text)] -> IO (Either Text Compiled)
compileQueries lang g qs = modifyMVar compiled $ \cache -> case M.lookup (g, qs) cache of
  Just r -> pure (cache, r)
  Nothing -> do
    r <- fmap sequence . forM qs $ \(f, src) ->
      BU.unsafeUseAsCStringLen (TE.encodeUtf8 src) $ \(p, n) ->
        with 0 $ \off -> with 0 $ \ty -> do
          q <- c_query lang p (fromIntegral n) off ty
          if q == nullPtr
            then do
              o <- peek off
              t <- peek ty
              pure (Left ("`" <> T.pack f <> "`: error at byte " <> T.pack (show o) <> " (" <> errorKind t <> ")"))
            else do
              names <- captureNames q
              let keep c = c `elem` readCaptures || "definition." `T.isPrefixOf` c
              withArray [if keep (M.findWithDefault "" i names) then 1 else 0 | i <- M.keys names] (c_prune q)
              pure (Right (f, q, M.filter keep names))
    pure (M.insert (g, qs) r cache, r)
  where
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

-- | Every capture of a query set over a text, with the node facts the anchor
-- rule reads, and the root node. Left: the grammar is not linked, or a query
-- does not compile. Pure: see the module header.
capturesOf :: Text -> [(FilePath, Text)] -> Text -> Either Text (PNode, [Cap])
capturesOf g qs content = unsafePerformIO (capturesIO g qs content)
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
        BU.unsafeUseAsCStringLen bytes $ \(p, n) ->
          bracket (c_parse lang p (fromIntegral n)) (\t -> when (t /= nullPtr) (c_treeDelete t)) $ \tree ->
            if tree == nullPtr
              then pure (Left ("tree-sitter could not take the `" <> g <> "` grammar (ABI)"))
              else allocaBytes size $ \root -> do
                c_root tree root
                out <- newIORef []
                matchNo <- newIORef (0 :: Int)
                mapM_ (runQuery bytes size root out matchNo) cqs
                rootNode <- pnode bytes root
                Right . (rootNode,) . reverse <$> readIORef out
  where
    runQuery bytes size root out matchNo (f, q, names) =
      bracket (c_exec q root) c_cursorDelete $ \cur ->
        allocaBytes (size * 64) $ \nodes -> allocaArray 64 $ \ids -> with 0 $ \pat ->
          let loop = do
                k <- c_next cur 64 nodes ids pat
                when (k >= 0) $ do
                  pIx <- peek pat
                  mi <- readIORef matchNo
                  modifyIORef' matchNo (+ 1)
                  is <- peekArray (fromIntegral k) ids
                  caps <- forM (zip [0 ..] is) $ \(j, cid) ->
                    capture bytes size root (nodes `plusPtr` (j * size)) (M.findWithDefault "" cid names) (T.pack f) (fromIntegral pIx) mi
                  modifyIORef' out (reverse caps ++)
                  loop
           in loop

-- | One capture's facts: the node, its parent, its top-level ancestor (a
-- direct child of the root, read for imports) and the fields the rule reads.
capture :: ByteString -> Int -> Ptr TSNode -> Ptr TSNode -> Text -> Text -> Int -> Int -> IO Cap
capture bytes size root node name file pIx mi = do
  n <- pnode bytes node
  parent <- allocaBytes size $ \p -> c_parent node p >> orNull p (pnode bytes p)
  top <- if name == "import" then topLevel else pure Nothing
  -- fields only where the rule reads them: definitions and calls
  let wanted = any (`T.isPrefixOf` name) ["definition.", "call.outer"]
  fields <- fmap concat . forM (if wanted then ["body", "parameters", "arguments"] else []) $ \fname ->
    allocaBytes size $ \fp -> withCStringLen (T.unpack fname) $ \(s, l) -> do
      c_field node s (fromIntegral l) fp
      maybe [] id <$> orNull fp ((\fnode kids -> [(fname, fnode, kids)]) <$> pnode bytes fp <*> children fp)
  pure (Cap name file pIx mi n parent top fields)
  where
    orNull p k = c_isNull p >>= \z -> if z /= 0 then pure Nothing else Just <$> k
    -- named, non-extra children
    children p = do
      cnt <- c_childCount p
      fmap concat . forM (if cnt == 0 then [] else [0 .. cnt - 1]) $ \i -> allocaBytes size $ \c -> do
        c_child p i c
        named <- c_isNamed c
        extra <- c_isExtra c
        if named /= 0 && extra == 0 then (: []) <$> pnode bytes c else pure []
    topLevel = do
      rb <- (,) <$> c_startByte root <*> c_endByte root
      rt <- c_type root >>= peekCString
      allocaBytes size $ \cur -> allocaBytes size $ \par -> do
        copyNode node cur
        let climb (d :: Int) = do
              c_parent cur par
              z <- c_isNull par
              if z /= 0 || d > 10000
                then pure Nothing
                else do
                  pb <- (,) <$> c_startByte par <*> c_endByte par
                  pt <- c_type par >>= peekCString
                  if pb == rb && pt == rt
                    then Just <$> pnode bytes cur
                    else copyNode par cur >> climb (d + 1)
        climb 0
    copyNode from to = copyBytes (castPtr to :: Ptr Word8) (castPtr from) size

-- | A node's facts. Tree-sitter counts columns in bytes; they become
-- characters here, as ast-grep reports them.
pnode :: ByteString -> Ptr TSNode -> IO PNode
pnode bytes p = do
  s <- fromIntegral <$> c_startByte p
  e <- fromIntegral <$> c_endByte p
  pts <- allocaArray 4 $ \a -> c_points p a >> peekArray 4 a
  ty <- c_type p >>= peekCString
  named <- (/= 0) <$> c_isNamed p
  extra <- (/= 0) <$> c_isExtra p
  let (sr, sc, er, ec) = case map fromIntegral pts of
        [a, b, c, d] -> (a, b, c, d)
        _ -> (0, 0, 0, 0)
      col row byteCol = T.length (TE.decodeUtf8Lenient (BS.take byteCol (BS.drop (lineStart row) bytes)))
      text = TE.decodeUtf8Lenient (BS.take (e - s) (BS.drop s bytes))
      -- the byte offset where row's line starts, from the node's own start
      lineStart row = if row == sr then s - sc else e - ec
  pure (PNode (T.pack ty) (sr, col sr sc) (er, col er ec) (s, e) text named extra)
