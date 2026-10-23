-- | Models that fill holes (spec §6.4): `[model]` config, the request and
-- reply formats of each backend kind, and a `Filler` that calls them with
-- curl (no HTTP library). The Anthropic key is read from the environment
-- variable the config names and handed to curl in a private header file,
-- never on its command line, never in output. A Jev backend answers typed
-- decisions only (`Olifant.Decide`), its key handled the same way.
module Olifant.Model
  ( Backend (..)
  , ModelConfig (..)
  , parseModelConfig
  , backendKinds
  , backendModel
  , systemPrompt
  , draftSystemPrompt
  , userPrompt
  , requestBody
  , decodeFills
  , curlFiller
  , curlFillerWith
  , askBackend
  , sendJev
  , probeBackend
  ) where

import Control.Concurrent (threadDelay)
import Control.Monad ((>=>))
import Data.Aeson (Value (..), eitherDecodeStrict, encode, object, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
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
import Olifant.Fill (FillHole (..), FillRequest (..), Filler (..), Stance (..))
import Olifant.Process (Cancel, Outcome (..), cancelled, neverCancel, runBytes, runBytesCancellable)
import Olifant.Toml (Table, inKey, int, onlyKeys, optional, required, table, text)
import System.Directory (findExecutable)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)

data Backend
  = OpenAICompatible {beUrl :: Text, beModel :: Text}
  | Anthropic {beModel :: Text, beKeyEnv :: Text}
  | Jev {beModel :: Text, beKeyEnv :: Text}
  deriving stock (Eq, Show)

-- | 'mcTimeout': seconds a model call may take (default 120).
data ModelConfig = ModelConfig {mcDefault :: Maybe Text, mcRetries :: Int, mcTimeout :: Int, mcBackends :: [(Text, Backend)]}
  deriving stock (Eq, Show)

backendKinds :: [Text]
backendKinds = ["openai-compatible", "anthropic", "jev"]

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
        "jev" -> onlyKeys ["kind", "model", "api_key_env"] t >> Jev . fromMaybe "jev-latest" <$> optional "model" text t <*> (fromMaybe "JEV_API_KEY" <$> optional "api_key_env" text t)
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

-- | The fill prompt for a draft: the least code that makes the test pass.
draftSystemPrompt :: Text
draftSystemPrompt =
  systemPrompt
    <> T.unlines
      [ "Write the least code that makes this test pass and no more: do not generalise beyond what the tests ask,"
      , "do not handle cases no test names. If the simplest passing code is a constant, write the constant."
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
        ++ previous h

    previous h
      | null (fhPrevious h) = []
      | otherwise =
          ["Previous attempts (do not repeat them):"]
            ++ concat [["attempt " <> T.pack (show n) <> ":", "```", f, "```", "why: " <> why] | (n, (f, why)) <- zip [1 :: Int ..] (fhPrevious h)]
            ++ ["Fix it." <> (if fhImports h then " If it failed on a name that is not defined, add that name's import line first." else "")]

requestBody :: Backend -> FillRequest -> Value
requestBody b req = chatBody b (case frStance req of DraftStance -> draftSystemPrompt; FillStance -> systemPrompt) (userPrompt req)

-- | A chat request: a system prompt and one user message.
chatBody :: Backend -> Text -> Text -> Value
chatBody b system user = case b of
  OpenAICompatible _ model ->
    object
      [ "model" .= model
      , "messages" .= [object ["role" .= ("system" :: Text), "content" .= system], object ["role" .= ("user" :: Text), "content" .= user]]
      , "temperature" .= (0 :: Int)
      ]
  Jev {} -> object []
  Anthropic model _ ->
    object
      [ "model" .= model
      , "max_tokens" .= (4096 :: Int)
      , "system" .= system
      , "messages" .= [object ["role" .= ("user" :: Text), "content" .= user]]
      ]

-- | The text of a backend's reply (a thinking block dropped), or what was
-- wrong with it.
replyText :: Backend -> ByteString -> Either Text Text
replyText b raw = do
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
  pure (T.strip (dropThinking content))
  where
    dropThinking t = case T.breakOn "</think>" t of
      (_, "") -> t
      (_, rest) -> T.drop (T.length "</think>") rest

-- | The fills in a backend's reply, or what was wrong with it.
decodeFills :: Backend -> ByteString -> Either Text (Map Text Text)
decodeFills b raw = do
  reply <- replyText b raw
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
    -- Small models wrap the JSON: prose, a ``` fence.
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
curlFillerWith token timeout b = Filler $ \req -> post (requestBody b req) (decodeFills b)
  where
    post body decode = sendChat token timeout b body >>= pure . (>>= decode)

-- | Ask a backend once: a system prompt and a question, the reply's text
-- (or why there is none). The request is dropped when the token trips.
askBackend :: Cancel -> Int -> Backend -> Text -> Text -> IO (Either Text Text)
askBackend token timeout b system question = sendChat token timeout b (chatBody b system question) >>= pure . (>>= replyText b)

-- | Post a chat body to the backend with curl: its raw reply. An Anthropic
-- key is read from its variable and passed in a temporary header file,
-- never on the command line.
sendChat :: Cancel -> Int -> Backend -> Value -> IO (Either Text ByteString)
sendChat token timeout b body =
  findExecutable "curl" >>= \case
    Nothing -> pure (Left "curl is not installed")
    Just curl -> case b of
      Jev {} -> pure (Left "a jev backend answers decisions, not prompts")
      OpenAICompatible url _ -> post curl (T.unpack (T.dropWhileEnd (== '/') url) <> "/chat/completions") []
      Anthropic _ keyEnv ->
        lookupEnv (T.unpack keyEnv) >>= \case
          Nothing -> pure (Left (keyEnv <> " is not set"))
          Just key -> withSystemTempFile "olifant-headers" $ \path h -> do
            TIO.hPutStr h ("x-api-key: " <> T.pack key <> "\nanthropic-version: 2023-06-01\n")
            hClose h
            post curl "https://api.anthropic.com/v1/messages" ["-H", "@" <> path]
  where
    post curl url extra = do
      let args = ["-sS", "-X", "POST", url, "-H", "content-type: application/json", "--max-time", show timeout, "--data-binary", "@-"] ++ extra
      runBytesCancellable token Nothing curl args Nothing (BL.toStrict (encode body)) >>= \case
        Left why -> pure (Left why)
        Right (Finished ExitSuccess out _) -> pure (Right out)
        Right (Finished (ExitFailure c) _ err) -> pure (Left ("curl exited " <> T.pack (show c) <> ": " <> T.strip (TE.decodeUtf8 err)))
        Right _ -> pure (Left "cancelled")

-- | Post a body of typed questions to Jev with curl: its raw reply. The key
-- is read from its variable and passed in a temporary header file, never on
-- the command line, never in a message. A 429 or 529 is retried twice
-- (after 0.5 s, then 2 s); any other status is an error.
sendJev :: Cancel -> Int -> Backend -> Value -> IO (Either Text ByteString)
sendJev token timeout b body = case b of
  Jev _ keyEnv ->
    findExecutable "curl" >>= \case
      Nothing -> pure (Left "curl is not installed")
      Just curl ->
        lookupEnv (T.unpack keyEnv) >>= \case
          Nothing -> pure (Left (keyEnv <> " is not set"))
          Just key -> withSystemTempFile "olifant-headers" $ \path h -> do
            TIO.hPutStr h ("Authorization: Bearer " <> T.pack key <> "\n")
            hClose h
            attempt curl path [500000, 2000000]
  _ -> pure (Left "not a jev backend")
  where
    attempt curl path backoff = do
      let args = ["-sS", "-X", "POST", "https://api.typesafe.ai/v1/systemone", "-H", "content-type: application/json", "-H", "@" <> path, "--max-time", show timeout, "-w", "\n%{http_code}", "--data-binary", "@-"]
      runBytesCancellable token Nothing curl args Nothing (BL.toStrict (encode body)) >>= \case
        Left why -> pure (Left why)
        Right (Finished ExitSuccess out _) ->
          let (reply, code) = BS.breakEnd (== 10) out
           in case (BC.unpack code, backoff) of
                (c, wait : rest) | c `elem` ["429", "529"] -> do
                  threadDelay wait
                  stop <- cancelled token
                  if stop then pure (Left "cancelled") else attempt curl path rest
                ('2' : _, _) -> pure (Right (BS.dropWhileEnd (== 10) reply))
                (c, _) -> pure (Left ("jev answered " <> T.pack c))
        Right (Finished (ExitFailure c) _ err) -> pure (Left ("curl exited " <> T.pack (show c) <> ": " <> T.strip (TE.decodeUtf8Lenient err)))
        Right _ -> pure (Left "cancelled")

-- | Does the backend answer (`olifant doctor`)? openai-compatible: its model
-- list within 3 s, with the model in it. Anthropic and Jev are never called
-- (that would spend the user's key); doctor checks only that the key is set.
probeBackend :: Backend -> IO (Either Text ())
probeBackend = \case
  Anthropic {} -> pure (Right ())
  Jev {} -> pure (Right ())
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
