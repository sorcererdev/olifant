-- | Models that fill holes (spec §6.4): `[model]` config, the request and
-- reply formats of each backend kind, and a `Filler` that calls them with
-- curl (no HTTP library). The Anthropic key is read from the environment
-- variable the config names and handed to curl in a private header file,
-- never on its command line, never in output.
module Olifant.Model
  ( Backend (..)
  , ModelConfig (..)
  , parseModelConfig
  , backendKinds
  , backendModel
  , systemPrompt
  , userPrompt
  , requestBody
  , decodeFills
  , curlFiller
  , curlFillerWith
  , probeBackend
  ) where

import Control.Monad ((>=>))
import Data.Aeson (Value (..), eitherDecodeStrict, encode, object, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BL
import Data.Foldable (toList)
import Data.Scientific (floatingOrInteger)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as TIO
import Olifant.Fill (FillHole (..), FillRequest (..), Filler (..))
import Olifant.Process (Cancel, Outcome (..), neverCancel, runBytes, runBytesCancellable)
import Olifant.Toml (Table, inKey, int, onlyKeys, optional, required, table, text)
import System.Directory (findExecutable)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)

data Backend
  = OpenAICompatible {beUrl :: Text, beModel :: Text}
  | Anthropic {beModel :: Text, beKeyEnv :: Text}
  deriving stock (Eq, Show)

-- | 'mcTimeout': seconds a model call may take (default 120).
data ModelConfig = ModelConfig {mcDefault :: Maybe Text, mcRetries :: Int, mcTimeout :: Int, mcBackends :: [(Text, Backend)]}
  deriving stock (Eq, Show)

backendKinds :: [Text]
backendKinds = ["openai-compatible", "anthropic"]

backendModel :: Backend -> Text
backendModel = beModel

-- | `[model]` from a config file's top table; absent means no model.
parseModelConfig :: Table -> Either Text ModelConfig
parseModelConfig top =
  optional "model" table top >>= \case
    Nothing -> Right (ModelConfig Nothing 2 120 [])
    Just m -> inKey "model" $ do
      onlyKeys ["default", "retries", "timeout", "backends"] m
      def <- optional "default" text m
      retries <- maybe 2 fromInteger <$> optional "retries" int m
      timeout <- maybe 120 fromInteger <$> optional "timeout" int m
      backends <- maybe (Right []) (traverse (\(k, v) -> inKey ("backends." <> k) ((k,) <$> (table >=> backend) v)) . M.toList) =<< optional "backends" table m
      pure (ModelConfig def retries timeout backends)
  where
    backend t =
      required "kind" text t >>= \case
        "openai-compatible" -> onlyKeys ["kind", "url", "model"] t >> OpenAICompatible <$> required "url" text t <*> required "model" text t
        "anthropic" -> onlyKeys ["kind", "model", "api_key_env"] t >> Anthropic <$> required "model" text t <*> (fromMaybe "ANTHROPIC_API_KEY" <$> optional "api_key_env" text t)
        other -> Left ("unknown backend kind `" <> other <> "` (kinds: " <> T.intercalate ", " backendKinds <> ")")

systemPrompt :: Text
systemPrompt =
  T.unlines
    [ "You fill typed holes in generated code. Each hole is marked \10214name\10215 in its file."
    , "Reply with one block per hole, nothing else:"
    , "<fill name=\"NAME\">"
    , "CODE"
    , "</fill>"
    , "CODE is plain source (no escaping) and replaces the marker exactly: only that code, valid as the hole's kind in the file's language,"
    , "matching the surrounding style. No surrounding code, no explanations, no markdown."
    ]

userPrompt :: FillRequest -> Text
userPrompt req =
  T.unlines $
    ["Generator inputs:"]
      ++ ["  " <> k <> " = " <> v | (k, v) <- frInputs req]
      ++ concat [["", "Related file " <> T.pack p <> ":", "```", c, "```"] | (p, c) <- frContext req]
      ++ ( if any fhImports (frHoles req)
             then
               [ ""
               , "Imports: if your code uses a name its file does not import or define, start the fill with the import line;"
               , "it is moved to the file's imports. For example, a fill that needs `date`:"
               , "<fill name=\"x\">"
               , "from datetime import date"
               , ""
               , "date.fromisoformat(text)"
               , "</fill>"
               ]
             else []
         )
      ++ concatMap hole (frHoles req)
      ++ ["", "Reply with " <> T.intercalate " " ["<fill name=\"" <> fhName h <> "\">…</fill>" | h <- frHoles req] <> "."]
  where
    hole h =
      [ ""
      , "Hole `" <> fhName h <> "` (kind: " <> fhKind h <> ") in " <> T.pack (fhFile h)
      , "Instruction: " <> fhInstruction h
      ]
        ++ ["The file around it:", "```", fhContext h, "```"]
        ++ maybe [] (\(f, why) -> ["Your previous fill was:", "```", f, "```", "It failed: " <> why, "Fix it." <> (if fhImports h then " If it failed on a name that is not defined, add that name's import line first." else "")]) (fhPrevious h)

requestBody :: Backend -> FillRequest -> Value
requestBody b req = case b of
  OpenAICompatible _ model ->
    object
      [ "model" .= model
      , "messages" .= [object ["role" .= ("system" :: Text), "content" .= systemPrompt], object ["role" .= ("user" :: Text), "content" .= userPrompt req]]
      , "temperature" .= (0 :: Int)
      ]
  Anthropic model _ ->
    object
      [ "model" .= model
      , "max_tokens" .= (4096 :: Int)
      , "system" .= systemPrompt
      , "messages" .= [object ["role" .= ("user" :: Text), "content" .= userPrompt req]]
      ]

-- | The fills in a backend's reply, or what was wrong with it.
decodeFills :: Backend -> ByteString -> Either Text (Map Text Text)
decodeFills b raw = do
  v <- either (\e -> Left ("the model's reply is not JSON: " <> T.pack e)) Right (eitherDecodeStrict raw)
  content <- case (b, v) of
    (_, Object o) | Just (Object e) <- KM.lookup "error" o, Just (String m) <- KM.lookup "message" e -> Left ("the model said: " <> m)
    (OpenAICompatible {}, Object o)
      | Just (Array cs) <- KM.lookup "choices" o
      , (Object c : _) <- toList cs
      , Just (Object m) <- KM.lookup "message" c
      , Just (String t) <- KM.lookup "content" m ->
          Right t
    (Anthropic {}, Object o)
      | Just (Array cs) <- KM.lookup "content" o
      , (Object c : _) <- toList cs
      , Just (String t) <- KM.lookup "text" c ->
          Right t
    _ -> Left "the model's reply has no message"
  let reply = T.strip (dropThinking content)
  -- Tagged blocks (v12: no escaping, no JSON mode); else JSON as it is, then
  -- the JSON inside a fence, or its braces.
  case tagged reply of
    fs | not (M.null fs) -> Right fs
    _ -> either (const (fills (jsonIn reply))) Right (fills reply)
  where
    -- `<fill name="NAME">CODE</fill>` blocks, anywhere in the reply; one
    -- newline either side of CODE is the block's, not the code's.
    tagged t = M.fromList
      [ (name, trimNewline body)
      | chunk <- drop 1 (T.splitOn "<fill name=\"" t)
      , let (name, rest) = T.breakOn "\">" chunk
      , not (T.null rest)
      , let (body, close) = T.breakOn "</fill>" (T.drop 2 rest)
      , not (T.null close)
      ]
    trimNewline block =
      let b' = maybe block id (T.stripPrefix "\n" block)
       in maybe b' id (T.stripSuffix "\n" b')
    -- Small models wrap the JSON: a <think> block, prose, a ``` fence.
    dropThinking t = case T.breakOn "</think>" t of
      (_, "") -> t
      (_, rest) -> T.drop (T.length "</think>") rest
    jsonIn t = case T.breakOn "```" t of
      (_, "") -> outerBraces t
      (_, fenced) -> outerBraces (fst (T.breakOn "```" (T.drop 1 (T.dropWhile (/= '\n') fenced))))
    outerBraces t = case (T.breakOn "{" t, T.breakOnEnd "}" t) of
      ((_, fromOpen), (toClose, _)) | not (T.null fromOpen), not (T.null toClose) -> T.take (T.length fromOpen - (T.length t - T.length toClose)) fromOpen
      _ -> t
    fills t = case eitherDecodeStrict (TE.encodeUtf8 t) of
      Right (Object o)
        | Just (Object fs) <- KM.lookup "fills" o -> values fs
        | otherwise -> values o
      _ -> Left ("the reply is not the JSON asked for ({\"fills\": {…}}): " <> T.take 200 t)
    values o = Right (M.fromList [(Key.toText k, s) | (k, v) <- KM.toList o, Just s <- [value v]])
    value = \case
      String s -> Just (unfence s)
      Number n -> Just (either (T.pack . show) (T.pack . show) (floatingOrInteger n :: Either Double Integer))
      Bool yes -> Just (if yes then "true" else "false")
      _ -> Nothing
    unfence s =
      let t = T.strip s
       in if "```" `T.isPrefixOf` t then T.strip (T.dropWhileEnd (== '`') (T.strip (T.drop 1 (T.dropWhile (/= '\n') t)))) else s

-- | A filler that posts to the backend with curl.
curlFiller :: Int -> Backend -> Filler IO
curlFiller = curlFillerWith neverCancel

-- | 'curlFiller', the request dropped when the token is tripped
-- ("cancelled").
curlFillerWith :: Cancel -> Int -> Backend -> Filler IO
curlFillerWith token timeout b = Filler $ \req ->
  findExecutable "curl" >>= \case
    Nothing -> pure (Left "curl is not installed")
    Just curl -> case b of
      OpenAICompatible url _ -> post curl (T.unpack (T.dropWhileEnd (== '/') url) <> "/chat/completions") [] req
      Anthropic _ keyEnv ->
        lookupEnv (T.unpack keyEnv) >>= \case
          Nothing -> pure (Left (keyEnv <> " is not set"))
          Just key -> withSystemTempFile "olifant-headers" $ \path h -> do
            TIO.hPutStr h ("x-api-key: " <> T.pack key <> "\nanthropic-version: 2023-06-01\n")
            hClose h
            post curl "https://api.anthropic.com/v1/messages" ["-H", "@" <> path] req
  where
    post curl url extra req = do
      let args = ["-sS", "-X", "POST", url, "-H", "content-type: application/json", "--max-time", show timeout, "--data-binary", "@-"] ++ extra
      runBytesCancellable token Nothing curl args Nothing (BL.toStrict (encode (requestBody b req))) >>= \case
        Left why -> pure (Left why)
        Right (Finished ExitSuccess out _) -> pure (decodeFills b out)
        Right (Finished (ExitFailure c) _ err) -> pure (Left ("curl exited " <> T.pack (show c) <> ": " <> T.strip (TE.decodeUtf8 err)))
        Right _ -> pure (Left "cancelled")

-- | Does the backend answer (`olifant doctor`)? openai-compatible: its model
-- list within 3 s, with the model in it. Anthropic is never called (that
-- would spend the user's key); doctor checks only that the key is set.
probeBackend :: Backend -> IO (Either Text ())
probeBackend = \case
  Anthropic {} -> pure (Right ())
  OpenAICompatible url model ->
    findExecutable "curl" >>= \case
      Nothing -> pure (Left "curl is not installed")
      Just curl -> do
        result <- runBytes curl ["-sS", "--max-time", "3", T.unpack (T.dropWhileEnd (== '/') url) <> "/models"] Nothing ""
        pure $ case result of
          Left why -> Left why
          Right (ExitFailure _, _, err) -> Left (T.strip (TE.decodeUtf8Lenient err))
          Right (ExitSuccess, out, _) -> case eitherDecodeStrict out :: Either String Value of
            Left _ -> Left "the endpoint did not answer with a model list"
            Right v
              | model `elem` modelIds v -> Right ()
              | otherwise -> Left ("the server answers but has no model `" <> model <> "` (pull it, e.g. `ollama pull " <> model <> "`)")
  where
    modelIds = \case
      Object o | Just (Array xs) <- KM.lookup "data" o -> [i | Object m <- foldr (:) [] xs, Just (String i) <- [KM.lookup "id" m]]
      _ -> []
