{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Olifant.ActionsSpec (spec) where

import Data.Functor.Identity (Identity (..), runIdentity)
import Data.Text.IO qualified as TIO
import Olifant.GeneratorTesting (preparePack)
import Olifant.Generation (Tools (..))
import Olifant.Language.AstGrep (astGrepSyntax)
import Olifant.Workspace (Workspace (..), diskWorkspace)
import System.Directory (findExecutable, getTemporaryDirectory)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import Data.Text qualified as T
import Olifant.Actions
import Olifant.Generator
import Olifant.Language
import Olifant.Query (noQueries)
import Olifant.Template (Template)
import Test.Hspec

rust :: Language
rust = l
  where
    Right l = parseLanguage "rust" "/p" (T.unlines ["files = [\"**/*.rs\"]", "comment = \"//\"", "grammar = \"rust\""])

generator :: Text -> Generator
generator src = either (error . T.unpack) (fmap inline) (parseGenerator (GeneratorId "p/g") src)
  where
    inline :: SourceRef -> Template
    inline (InlineText t) = t
    inline other = error (show other)

failureArms :: Generator
failureArms = generator . T.unlines $
  [ "description = \"Add arms\"", "language = \"rust\""
  , "[inputs.service]", "type = \"ident\"", "[inputs.command]", "type = \"ident\"", "[inputs.failure]", "type = \"ident\""
  , "[applies_at]", "path = \"{service}/*-service/src/{command}.rs\""
  , "node = { kind = \"enum_variant\" }"
  , "bind = { failure = \"^[[:space:]]*([A-Za-z0-9_]+)\" }"
  , "label = \"Add {= failure =} to every match\"" ]

-- An outer enum (lines 0–3) and a variant (line 2, cols 4–19).
matches :: Syntax Identity
matches = Syntax $ \_ _ _ -> Identity (Right [Match (0, 0) (3, 1) "pub enum RefundInvoiceFailure {…}", Match (2, 4) (2, 19) "AlreadyRefunded"])

file :: FilePath
file = "billing/billing-service/src/refund_invoice.rs"

at :: Syntax Identity -> [(Text, Generator)] -> FilePath -> (Int, Int) -> [Action]
at syn gens path pos = runIdentity (actionsAt syn [rust] noQueries [] (const (pure Nothing)) gens path pos "text")

spec :: Spec
spec = describe "olifant actions (spec §4.6)" $ do
  it "(a) binds path slots" $ do
    matchPathSlots "{service}/*-service/src/{command}.rs" file `shouldBe` Just [("service", "billing"), ("command", "refund_invoice")]
    matchPathSlots "{service}/*-service/src/{command}.rs" "billing/lib.rs" `shouldBe` Nothing
    matchPathSlots "**/src/{command}.rs" file `shouldBe` Just [("command", "refund_invoice")]
  it "(b) offers a generator whose path and node match, with inputs bound and the label rendered" $
    at matches [("p/failure-arms", failureArms)] file (3, 6)
      `shouldBe` [Action "p/failure-arms" "Add AlreadyRefunded to every match" [("command", "refund_invoice"), ("failure", "AlreadyRefunded"), ("service", "billing")] [] []]
  it "(c) matches at a node's first and just past its last character; binds from the innermost node (Review Focus 1)" $ do
    map actSets (at matches [("g", failureArms)] file (3, 5)) `shouldBe` [[("command", "refund_invoice"), ("failure", "AlreadyRefunded"), ("service", "billing")]]
    map actSets (at matches [("g", failureArms)] file (3, 20)) `shouldBe` [[("command", "refund_invoice"), ("failure", "AlreadyRefunded"), ("service", "billing")]]
  it "(d) offers nothing outside every match, and nothing for another path (Review Focus 1)" $ do
    at matches [("g", failureArms)] file (9, 1) `shouldBe` []
    at matches [("g", failureArms)] "notes/readme.md" (1, 1) `shouldBe` []
  it "(e) parses the text it is given, not the file (Review Focus 2)" $ do
    seen <- newIORef []
    let recording = Syntax $ \_ _ content -> modifyIORef' seen (content :) >> pure (Right [])
    _ <- actionsAt recording [rust] noQueries [] (const (pure Nothing)) [("g", failureArms)] file (1, 1) "unsaved text"
    readIORef seen `shouldReturn` ["unsaved text"]
  it "(f) moves a bound value that is not a valid choice into missing (Review Focus 3)" $ do
    let withChoices' = generator . T.unlines $
          [ "description = \"d\"", "language = \"rust\""
          , "[inputs.service]", "type = \"enum\"", "one_of = \"services\""
          , "[inputs.command]", "type = \"ident\""
          , "[query.services]", "glob = \"*/*-service\"", "value = \"parent folder\""
          , "[applies_at]", "path = \"{service}/*-service/src/{command}.rs\"" ]
        acts = runIdentity (actionsAt matches [rust] noQueries ["shipping/shipping-service/lib.rs"] (const (pure Nothing)) [("g", withChoices')] file (1, 1) "text")
    map (\a -> (actSets a, actMissing a)) acts `shouldBe` [([("command", "refund_invoice")], ["service"])]
  it "(g) renders path slots into the node rule (a command's own struct)" $ do
    seen <- newIORef []
    let recording = Syntax $ \_ rule _ -> modifyIORef' seen (rule :) >> pure (Right [])
        own = generator . T.unlines $
          [ "description = \"d\"", "language = \"rust\""
          , "[inputs.service]", "type = \"ident\"", "[inputs.command]", "type = \"ident\""
          , "[applies_at]", "path = \"{service}/*-service/src/{command}.rs\""
          , "node = { kind = \"field_declaration\", inside = { kind = \"struct_item\", regex = \"struct {= command|pascal =} \" } }" ]
    _ <- actionsAt recording [rust] noQueries [] (const (pure Nothing)) [("g", own)] file (1, 1) "text"
    (T.pack . show <$> readIORef seen) >>= (`shouldSatisfy` T.isInfixOf "struct RefundInvoice ")
  it "(i) reads COL as a UTF-8 byte column (Neovim's), not a character column (review I1)" $ do
    -- "éé x yz": x is character 3 (0-based) but byte 5; ast-grep reports characters.
    let content = "\233\233 x yz\n"
        onX = Syntax $ \_ _ _ -> Identity (Right [Match (0, 3) (0, 4) "x"])
        acts c = runIdentity (actionsAt onX [rust] noQueries [] (const (pure Nothing)) [("g", failureArms)] file (1, c) content)
    map actGenerator (acts 6) `shouldBe` ["g"]
    acts 4 `shouldBe` []
  it "(h) binds the file's own path to an input with `file`" $ do
    let errorCase = generator . T.unlines $
          [ "description = \"d\"", "language = \"rust\""
          , "[inputs.path]", "type = \"text\"", "[inputs.name]", "type = \"ident\""
          , "[applies_at]", "path = \"**/*.rs\"", "file = \"path\"", "label = \"Add a case\"" ]
    map (\a -> (actSets a, actMissing a)) (at matches [("g", errorCase)] file (1, 1)) `shouldBe` [([("path", T.pack file)], ["name"])]
  it "lists snippets after the other actions here (what was written comes before what comes next)" $ do
    let snippetHere = generator . T.unlines $
          [ "description = \"A snippet\"", "language = \"rust\"", "[[edits]]", "cursor = true", "text = \"x\""
          , "[applies_at]", "path = \"**/*.rs\"" ]
    map actGenerator (at matches [("p/snippet", snippetHere), ("p/failure-arms", failureArms)] file (3, 6)) `shouldBe` ["p/failure-arms", "p/snippet"]
  describe "after: the cursor follows a node (a snippet's anticipation, spec §4.9)" $ do
    let afterClass = generator . T.unlines $
          [ "description = \"A failure\"", "language = \"rust\"", "[[edits]]", "cursor = true", "text = \"x\""
          , "[applies_at]", "path = \"**/*.rs\"", "after = { kind = \"class_definition\" }", "trigger = '^[[:space:]]*$'" ]
        -- a class on 0-based lines 2–4
        classAt = Syntax $ \_ _ _ -> Identity (Right [Match (2, 0) (4, 15) "class X:\n    reason: str"])
        src = T.unlines ["import x", "", "class X:", "    reason: str", "    more: int", "", "", "def f(): pass"]
        on line = runIdentity (actionsAt classAt [rust] noQueries [] (const (pure Nothing)) [("p/after", afterClass)] file (line, 1) src)
    it "applies on the line right after the node, and after blank lines only" $ do
      map actGenerator (on 6) `shouldBe` ["p/after"]
      map actGenerator (on 7) `shouldBe` ["p/after"]
    it "not inside the node, not before it, not past code that follows it" $ do
      on 4 `shouldBe` []
      on 1 `shouldBe` []
      on 9 `shouldBe` []
    it "--file: its range is every blank line that follows the node (where --at applies)" $
      map faRange (fst (runIdentity (actionsInFile classAt [rust] noQueries [] (const (pure Nothing)) [("p/after", afterClass)] file src)))
        `shouldBe` [Just (Range (6, 1) (7, 1))]
    it "reads `after` and refuses it with `node`" $ do
      let both = parseGenerator (GeneratorId "p/g") (T.unlines ["description = \"d\"", "language = \"x\"", "[applies_at]", "path = \"x\"", "node = { kind = \"a\" }", "after = { kind = \"b\" }"])
      both `shouldSatisfy` either ("`after` or `node`" `T.isInfixOf`) (const False)
      both `shouldSatisfy` either (not . ("`applies_at`: `applies_at`" `T.isInfixOf`)) (const False)
  describe "trigger: what was just typed (spec §4.9)" $ do
    let specCase = generator . T.unlines $
          [ "description = \"A spec case\"", "language = \"rust\""
          , "[[edits]]", "cursor = true", "text = \"x\""
          , "[applies_at]", "path = \"**/*.rs\"", "trigger = 'it \"'" ]
        on line col content = runIdentity (actionsAt matches [rust] noQueries [] (const (pure Nothing)) [("p/spec-case", specCase)] file (1, col) (line <> "\n" <> content))
    it "(b) applies when the text before the cursor ends with the trigger" $
      map actGenerator (on "  it \"" 7 "") `shouldBe` ["p/spec-case"]
    it "(b) not once more is typed, nor with the cursor before it" $ do
      on "  it \"x" 8 "" `shouldBe` []
      on "  it \"" 3 "" `shouldBe` []
    it "(review I1) never on a line with code after the cursor: the snippet would replace it" $ do
      let blank = generator . T.unlines $
            [ "description = \"d\"", "language = \"rust\"", "[[edits]]", "cursor = true", "text = \"x\""
            , "[applies_at]", "path = \"**/*.rs\"", "trigger = '^[[:space:]]*$'" ]
          onLine l c = runIdentity (actionsAt matches [rust] noQueries [] (const (pure Nothing)) [("p/blank", blank)] file (1, c) (l <> "\n"))
      onLine "    Unknown(String)," 1 `shouldBe` []
      onLine "    Unknown(String)," 3 `shouldBe` []
      map actGenerator (onLine "    " 5) `shouldBe` ["p/blank"]
      map actGenerator (onLine "" 1) `shouldBe` ["p/blank"]
    it "(b) a generator without a trigger is unchanged" $
      map actGenerator (at matches [("g", failureArms)] file (3, 6)) `shouldBe` ["g"]
  it "(d) lists the choices of a missing enum input" $ do
    let withPorts = generator . T.unlines $
          [ "description = \"d\"", "language = \"rust\""
          , "[inputs.port]", "type = \"enum\"", "one_of = \"ports\""
          , "[query.ports]", "glob = \"ports/*.rs\"", "value = \"file name\""
          , "[[edits]]", "cursor = true", "text = \"{= port =}\""
          , "[applies_at]", "path = \"**/*.rs\"" ]
        [a] = runIdentity (actionsAt matches [rust] noQueries ["ports/email_sender.rs", "ports/invoice_repository.rs"] (const (pure Nothing)) [("p/add-port", withPorts)] file (1, 1) "x")
    actMissing a `shouldBe` ["port"]
    actChoices a `shouldBe` [("port", ["email_sender", "invoice_repository"])]
  describe "completions: snippets for the menu (spec §4.9)" $ do
    let addPort = generator . T.unlines $
          [ "description = \"A port field\"", "language = \"rust\""
          , "[inputs.port]", "type = \"enum\"", "one_of = \"ports\""
          , "[query.ports]", "glob = \"ports/*.rs\"", "value = \"file name\""
          , "[[edits]]", "cursor = true", "text = \"{= port =}\""
          , "[applies_at]", "path = \"**/*.rs\"", "label = \"Add port {= port =}\"" ]
        specCase = generator . T.unlines $
          [ "description = \"A spec case\"", "language = \"rust\"", "[[edits]]", "cursor = true", "text = \"x\""
          , "[applies_at]", "path = \"**/*.rs\"", "trigger = 'it \"'", "label = \"Spec case\"" ]
        complete gens = runIdentity (completionsAt matches [rust] noQueries ["ports/email_sender.rs", "ports/invoice_repository.rs"] (const (pure Nothing)) gens file (1, 1) "")
    it "(a) one item per choice of the one missing enum input, labelled with it" $
      map (\c -> (cpLabel c, cpSets c)) (complete [("p/add-port", addPort)])
        `shouldBe` [("Add port email_sender", [("port", "email_sender")]), ("Add port invoice_repository", [("port", "invoice_repository")])]
    it "(b) a snippet whose trigger does not match is still listed (chosen ignores triggers)" $
      map cpGenerator (complete [("p/spec-case", specCase)]) `shouldBe` ["p/spec-case"]
    it "(b) a snippet whose one missing input has no choices offers nothing (no uncovered cases: no arm)" $
      runIdentity (completionsAt matches [rust] noQueries [] (const (pure Nothing)) [("p/add-port", addPort)] file (1, 1) "x") `shouldBe` []
    it "(review I6) chosen: only on a line that is blank but for the word being typed (it replaces the line)" $ do
      let onLine l c = runIdentity (completionsAt matches [rust] noQueries [] (const (pure Nothing)) [("p/spec-case", specCase)] file (1, c) (l <> "\n"))
      map cpGenerator (onLine "  spe" 6) `shouldBe` ["p/spec-case"]
      map cpGenerator (onLine "" 1) `shouldBe` ["p/spec-case"]
      onLine "  foo bar" 10 `shouldBe` []
      onLine "  spe x" 6 `shouldBe` []
    it "(review I4) a snippet whose missing enum input has no choices is not listed at the cursor" $ do
      let addPortNoPorts = runIdentity (actionsAt matches [rust] noQueries [] (const (pure Nothing)) [("p/add-port", addPort)] file (1, 1) "x")
      addPortNoPorts `shouldBe` []
    it "(c) only snippets: other generators that apply are left out" $
      complete [("g", failureArms)] `shouldBe` []
  describe "--file: every place in a file (spec §4.6)" $ do
    let inFile syn gens path content = runIdentity (actionsInFile syn [rust] noQueries [] (const (pure Nothing)) gens path content)
        threeVariants = Syntax $ \_ _ _ -> Identity (Right [Match (3, 4) (3, 9) "Third", Match (1, 4) (1, 9) "First", Match (2, 4) (2, 10) "Second"])
    it "(a) a generator without node applies to the whole file (no range)" $ do
      let whole = generator . T.unlines $ ["description = \"d\"", "language = \"rust\"", "[inputs.service]", "type = \"ident\"", "[inputs.command]", "type = \"ident\"", "[applies_at]", "path = \"{service}/*-service/src/{command}.rs\"", "label = \"Whole\""]
      fst (inFile matches [("w", whole)] file "x") `shouldBe` [FileAction (Action "w" "Whole" [("command", "refund_invoice"), ("service", "billing")] [] []) Nothing]
    it "(b) one entry per matching node, in file order, each with its own binds (Review Focus 2)" $
      map (\fa -> (lookup "failure" (actSets (faAction fa)), faRange fa)) (fst (inFile threeVariants [("g", failureArms)] file "enum E {\n    First,\n    Second,\n    Third,\n}\n"))
        `shouldBe` [ (Just "First", Just (Range (2, 5) (2, 10))), (Just "Second", Just (Range (3, 5) (3, 11))), (Just "Third", Just (Range (4, 5) (4, 10))) ]
    it "(b2) ranges are UTF-8 byte columns" $ do
      let onX = Syntax $ \_ _ _ -> Identity (Right [Match (0, 2) (0, 3) "x"])
      map faRange (fst (inFile onX [("g", failureArms)] file "\233 x\n")) `shouldBe` [Just (Range (1, 4) (1, 5))]
    it "(c) a path match with no node is nearby, not an action" $ do
      let (acts, near) = inFile (Syntax $ \_ _ _ -> Identity (Right [])) [("g", failureArms)] file "x"
      acts `shouldBe` []
      map (\n -> (nbGenerator n, nbLabel n)) near `shouldBe` [("g", "Add arms")]
    it "(c2) nearby says what the rule looks for: `where` rendered with the path, else the rule in words" $ do
      let none = Syntax $ \_ _ _ -> Identity (Right [])
          own w = generator . T.unlines $
            [ "description = \"d\"", "language = \"rust\""
            , "[inputs.service]", "type = \"ident\"", "[inputs.command]", "type = \"ident\""
            , "[applies_at]", "path = \"{service}/*-service/src/{command}.rs\""
            , "node = { kind = \"field_declaration\", inside = { kind = \"struct_item\", regex = \"^pub struct {= command|pascal =} \" } }" ] ++ w
      map nbWhy (snd (inFile none [("g", own ["where = \"on a field of the {= command|pascal =} struct\""])] file "x")) `shouldBe` ["on a field of the RefundInvoice struct"]
      map nbWhy (snd (inFile none [("g", own [])] file "x")) `shouldBe` ["a `field_declaration` inside a `struct_item` matching /^pub struct RefundInvoice /"]
    it "(c3) a search that failed says so, and suggests olifant doctor — not \"none yet\" (review I3)" $ do
      let broken = Syntax $ \_ _ _ -> Identity (Left (SyntaxToolMissing "ast-grep is not installed (brew install ast-grep)"))
      map nbWhy (snd (inFile broken [("g", failureArms)] file "x")) `shouldSatisfy` all (\w -> "cannot search this file" `T.isInfixOf` w && "ast-grep is not installed" `T.isInfixOf` w && "olifant doctor" `T.isInfixOf` w)
    it "(c4) no near miss when a path-bound value is not a valid choice (lib.rs is not a command) (review I4)" $ do
      let enumGen = generator . T.unlines $
            [ "description = \"d\"", "language = \"rust\""
            , "[inputs.service]", "type = \"enum\"", "one_of = \"services\""
            , "[inputs.command]", "type = \"ident\""
            , "[query.services]", "glob = \"*/*-service\"", "value = \"parent folder\""
            , "[applies_at]", "path = \"{service}/*-service/src/{command}.rs\"", "node = { kind = \"struct_item\" }" ]
          none = Syntax $ \_ _ _ -> Identity (Right [])
          near ps = snd (runIdentity (actionsInFile none [rust] noQueries ps (const (pure Nothing)) [("g", enumGen)] file "x"))
      near ["shipping/shipping-service/lib.rs"] `shouldBe` []
      map nbGenerator (near ["billing/billing-service/lib.rs"]) `shouldBe` ["g"]
    it "(d) another path: neither" $
      inFile threeVariants [("g", failureArms)] "notes/readme.md" "x" `shouldBe` ([], [])
  describe "the rust-htmx demo generators (real ast-grep)" $ do
    strict <- runIO ((== Just "1") <$> lookupEnv "OLIFANT_REQUIRE_TOOLS")
    let real gen given path pos = findExecutable "ast-grep" >>= \case
          Nothing -> if strict then expectationFailure "ast-grep is required" >> pure [] else pendingWith "ast-grep is not installed" >> pure []
          Just _ -> do
            tmp <- getTemporaryDirectory
            content <- TIO.readFile given
            Right (ws, tools, _, g) <- preparePack (diskWorkspace ".") (astGrepSyntax (tmp </> "olifant-test-cache")) noChecker "packs/rust-htmx" "rust-htmx" gen [(path, content)]
            paths <- listFiles ws "."
            actionsAt (toolSyntax tools) (toolLanguages tools) (toolQueries tools) paths (readText ws) [("rust-htmx/" <> gen, g)] path pos content
        cancel = "packs/rust-htmx/generators/wire-command/tests/cancel-invoice/given/billing/billing-service/src/cancel_invoice.rs"
        refund = "packs/rust-htmx/generators/failure-arms/tests/already-refunded/given/billing/billing-service/src/refund_invoice.rs"
    it "(python) failure-variant follows a failure class, not a decorated function with a `reason: str` parameter" $ findExecutable "ast-grep" >>= \case
      Nothing -> if strict then expectationFailure "ast-grep is required" else pendingWith "ast-grep is not installed"
      Just _ -> do
        tmp <- getTemporaryDirectory
        let route = "@app.get(\"/\")\ndef f(reason: str) -> str:\n    return reason\n\n"
            failure = "@dataclass(frozen=True)\nclass Gone:\n    reason: str\n\n"
            offered path content = do
              Right (ws, tools, _, g) <- preparePack (diskWorkspace ".") (astGrepSyntax (tmp </> "olifant-test-cache")) noChecker "packs/python-htmx" "python-htmx" "failure-variant" [(path, content)]
              paths <- listFiles ws "."
              map actGenerator <$> actionsAt (toolSyntax tools) (toolLanguages tools) (toolQueries tools) paths (readText ws) [("python-htmx/failure-variant", g)] path (4, 1) content
        offered "src/bistro/web/app.py" route `shouldReturn` []
        offered "src/bistro/booking/commands/create_booking.py" failure `shouldReturn` ["python-htmx/failure-variant"]
    it "(python) offers wire on an __init__ parameter, binding the port; not on handle's" $ findExecutable "ast-grep" >>= \case
      Nothing -> if strict then expectationFailure "ast-grep is required" else pendingWith "ast-grep is not installed"
      Just _ -> do
        tmp <- getTemporaryDirectory
        let given = "packs/python-htmx/generators/wire/tests/new-adapter/given/src/bistro/booking/queries/get_day_schedule.py"
            path = "src/bistro/booking/queries/get_day_schedule.py"
        content <- TIO.readFile given
        Right (ws, tools, _, g) <- preparePack (diskWorkspace ".") (astGrepSyntax (tmp </> "olifant-test-cache")) noChecker "packs/python-htmx" "python-htmx" "wire" [(path, content)]
        paths <- listFiles ws "."
        let lineOf needle = 1 + length (takeWhile (not . (needle `T.isInfixOf`)) (T.lines content))
            col needle = 1 + T.length (fst (T.breakOn needle (T.lines content !! (lineOf needle - 1))))
            offeredAt pos = map (\a -> (actLabel a, actSets a)) <$> actionsAt (toolSyntax tools) (toolLanguages tools) (toolQueries tools) paths (readText ws) [("python-htmx/wire", g)] path pos content
        offeredAt (lineOf "tables: TableRepository", col "tables: TableRepository" + 2) `shouldReturn`
          [("Wire tables (TableRepository)", [("app", "bistro"), ("command", "get_day_schedule"), ("kind", "queries"), ("param", "tables"), ("port", "TableRepository"), ("service", "booking")])]
        offeredAt (lineOf "def handle", col "day: date" + 1) `shouldReturn` []
    it "(python) offers add-dependency on __init__, not on handle" $ findExecutable "ast-grep" >>= \case
      Nothing -> if strict then expectationFailure "ast-grep is required" else pendingWith "ast-grep is not installed"
      Just _ -> do
        tmp <- getTemporaryDirectory
        let given = "packs/python-htmx/generators/add-dependency/tests/inline/given/src/bistro/booking/commands/cancel_booking.py"
            path = "src/bistro/booking/commands/cancel_booking.py"
        content <- TIO.readFile given
        Right (ws, tools, _, g) <- preparePack (diskWorkspace ".") (astGrepSyntax (tmp </> "olifant-test-cache")) noChecker "packs/python-htmx" "python-htmx" "add-dependency" [(path, content)]
        paths <- listFiles ws "."
        let lineOf needle = 1 + length (takeWhile (not . (needle `T.isInfixOf`)) (T.lines content))
            offeredAt pos = map (\a -> (actLabel a, actSets a)) <$> actionsAt (toolSyntax tools) (toolLanguages tools) (toolQueries tools) paths (readText ws) [("python-htmx/add-dependency", g)] path pos content
        offeredAt (lineOf "def __init__", 9) `shouldReturn` [("Add a port", [("app", "bistro"), ("command", "cancel_booking"), ("kind", "commands"), ("service", "booking")])]
        offeredAt (lineOf "def handle", 9) `shouldReturn` []
    it "(python) offers handle-body on handle's raise, not on another method's" $ findExecutable "ast-grep" >>= \case
      Nothing -> if strict then expectationFailure "ast-grep is required" else pendingWith "ast-grep is not installed"
      Just _ -> do
        tmp <- getTemporaryDirectory
        given <- TIO.readFile "packs/python-htmx/generators/handle-body/tests/create-booking/given/src/bistro/booking/commands/create_booking.py"
        let path = "src/bistro/booking/commands/create_booking.py"
            content = given <> "\n    def other(self) -> None:\n        raise NotImplementedError(\"later\")\n"
        Right (ws, tools, _, g) <- preparePack (diskWorkspace ".") (astGrepSyntax (tmp </> "olifant-test-cache")) noChecker "packs/python-htmx" "python-htmx" "handle-body" [(path, content)]
        paths <- listFiles ws "."
        let lineOf needle = 1 + length (takeWhile (not . (needle `T.isInfixOf`)) (T.lines content))
            offeredAt pos = map actLabel <$> actionsAt (toolSyntax tools) (toolLanguages tools) (toolQueries tools) paths (readText ws) [("python-htmx/handle-body", g)] path pos content
        offeredAt (lineOf "CreateBooking.handle: write", 9) `shouldReturn` ["Write handle with the model (until its tests pass)"]
        offeredAt (lineOf "\"later\"", 9) `shouldReturn` []
    it "(python) offers port-method-adapters on a Protocol method with a docstring body (pack-opens)" $ findExecutable "ast-grep" >>= \case
      Nothing -> if strict then expectationFailure "ast-grep is required" else pendingWith "ast-grep is not installed"
      Just _ -> do
        tmp <- getTemporaryDirectory
        let given = "packs/python-htmx/generators/repository/tests/scalar-fields/snapshot/src/bistro/booking/ports/table_repository/table_repository.py"
            path = "src/bistro/booking/ports/table_repository/table_repository.py"
        content <- TIO.readFile given
        Right (ws, tools, _, g) <- preparePack (diskWorkspace ".") (astGrepSyntax (tmp </> "olifant-test-cache")) noChecker "packs/python-htmx" "python-htmx" "port-method-adapters" [(path, content)]
        paths <- listFiles ws "."
        let lineOf needle = 1 + length (takeWhile (not . (needle `T.isInfixOf`)) (T.lines content))
        acts <- actionsAt (toolSyntax tools) (toolLanguages tools) (toolQueries tools) paths (readText ws) [("python-htmx/port-method-adapters", g)] path (lineOf "def save", 9) content
        [(actLabel a, lookup "name" (actSets a), lookup "params" (actSets a), lookup "returns" (actSets a)) | a <- acts]
          `shouldBe` [("Add save to table_repository's adapters", Just "save", Just "(self, table: Table)", Just "None")]
    it "offers wire-command on a field of the command's struct, not on another struct" $ do
      acts <- real "wire-command" cancel "billing/billing-service/src/cancel_invoice.rs" (9, 9)
      map (\a -> (actLabel a, actSets a, actMissing a)) acts `shouldBe` [("Wire cancel_invoice's dependencies", [("command", "cancel_invoice"), ("service", "billing")], [])]
      real "wire-command" cancel "billing/billing-service/src/cancel_invoice.rs" (13, 20) >>= (`shouldBe` [])
    it "the user's miss: a field in the Input struct gives no wire-command action but a nearby reason" $ findExecutable "ast-grep" >>= \case
      Nothing -> if strict then expectationFailure "ast-grep is required" else pendingWith "ast-grep is not installed"
      Just _ -> do
        tmp <- getTemporaryDirectory
        given <- TIO.readFile cancel
        let wrong = T.replace "pub struct CancelInvoice {\n    pub email_sender: Arc<dyn EmailSender>,\n}" "pub struct CancelInvoice {}" (T.replace "pub struct CancelInvoiceInput {}" "pub struct CancelInvoiceInput {\n    pub email_sender: Arc<dyn EmailSender>;\n}" given)
            path = "billing/billing-service/src/cancel_invoice.rs"
        wrong `shouldNotBe` given
        Right (ws, tools, _, g) <- preparePack (diskWorkspace ".") (astGrepSyntax (tmp </> "olifant-test-cache")) noChecker "packs/rust-htmx" "rust-htmx" "wire-command" [(path, wrong)]
        paths <- listFiles ws "."
        (acts, near) <- actionsInFile (toolSyntax tools) (toolLanguages tools) (toolQueries tools) paths (readText ws) [("rust-htmx/wire-command", g)] path wrong
        acts `shouldBe` []
        map nbWhy near `shouldBe` ["on a field of the CancelInvoice struct"]
    it "offers failure-arms on a Failure variant, binding the variant" $ do
      acts <- real "failure-arms" refund "billing/billing-service/src/refund_invoice.rs" (28, 7)
      map (\a -> (actLabel a, actSets a, actMissing a)) acts `shouldBe` [("Add AlreadyRefunded to every match", [("command", "refund_invoice"), ("failure", "AlreadyRefunded"), ("service", "billing")], [])]
