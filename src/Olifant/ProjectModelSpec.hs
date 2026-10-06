module Olifant.ProjectModelSpec (spec) where

import Data.Either (isLeft)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Olifant.Anchor (astQueries)
import Olifant.Catalog (catalogLanguages, catalogModelMaps, openPackCatalog)
import Olifant.Language.AstGrep (astGrepSyntax)
import Olifant.ProjectModel
import Olifant.Query (Queries (..), noQueries)
import Olifant.Workspace (diskWorkspace, listFiles)
import System.Directory (getTemporaryDirectory)
import System.FilePath ((</>))
import Test.Hspec

spec :: Spec
spec = describe "Olifant.ProjectModel (the project in the shared vocabulary)" $ do
  it "reads python-htmx's services, commands, queries, ports and adapters" $ do
    m <- modelOf "python-htmx" "packs/python-htmx/fixtures/app" "bistro" id
    pmProblems m `shouldBe` []
    idsOf Service m `shouldBe` ["bistro:booking"]
    idsOf Command m `shouldBe` ["bistro:booking/commands/create_booking"]
    idsOf Query m `shouldBe` ["bistro:booking/queries/get_day_schedule"]
    idsOf Port m `shouldBe` ["bistro:booking/ports/booking_repository", "bistro:booking/ports/id_generator"]
    idsOf Adapter m `shouldSatisfy` elem "bistro:booking/ports/booking_repository/sqlite_booking_repository"
    idsOf Adapter m `shouldNotSatisfy` elem "bistro:booking/ports/booking_repository/booking_repository"
    idsOf Resource m `shouldBe` ["bistro:web/bookings"]
    [elName e | e <- pmElements m, elKind e == Command] `shouldBe` ["create_booking"]
  it "relates them: adapters implement ports, commands use ports, resources call commands" $ do
    m <- modelOf "python-htmx" "packs/python-htmx/fixtures/app" "bistro" id
    let rels = [(relFrom r, relKind r, relTo r) | r <- pmRelations m]
    rels `shouldSatisfy` elem ("bistro:booking/ports/booking_repository/sqlite_booking_repository", Implements, "bistro:booking/ports/booking_repository")
    rels `shouldSatisfy` elem ("bistro:booking/commands/create_booking", Uses, "bistro:booking/ports/booking_repository")
    rels `shouldSatisfy` elem ("bistro:web/bookings", Calls, "bistro:booking/commands/create_booking")
    rels `shouldSatisfy` elem ("bistro:web/bookings", Calls, "bistro:booking/queries/get_day_schedule")
    rels `shouldSatisfy` elem ("bistro:booking/commands/create_booking__test", Covers, "bistro:booking/commands/create_booking")
  it "reads rust-htmx in the same vocabulary" $ do
    m <- modelOf "rust-htmx" "packs/rust-htmx/fixtures/app" "shop" id
    pmProblems m `shouldBe` []
    idsOf Service m `shouldSatisfy` elem "shop:billing"
    idsOf Command m `shouldSatisfy` elem "shop:billing/commands/cancel_invoice"
    idsOf Command m `shouldNotSatisfy` elem "shop:billing/commands/lib"
    idsOf Port m `shouldSatisfy` elem "shop:billing/ports/invoice_repository"
    [(relFrom r, relTo r) | r <- pmRelations m, relKind r == Implements]
      `shouldSatisfy` elem ("shop:billing/adapters/in_memory_invoice_repository", "shop:billing/ports/invoice_repository")
  it "is the same model whatever order the files are listed in (stable ids)" $ do
    a <- modelOf "python-htmx" "packs/python-htmx/fixtures/app" "bistro" id
    b <- modelOf "python-htmx" "packs/python-htmx/fixtures/app" "bistro" reverse
    (pmElements a, pmRelations a) `shouldBe` (pmElements b, pmRelations b)
  it "reads elm's pages, and keeps a pack's own kinds namespaced" $ do
    m <- modelOf "elm" "packs/elm/fixtures/app" "web" id
    idsOf Page m `shouldBe` ["web:Page/Home", "web:Page/SamplePage"]
    [elKind e | e <- pmElements m, elKind e `notElem` [Page]] `shouldSatisfy` all (== Custom "elm:route")
    idsOf (Custom "elm:route") m `shouldSatisfy` (not . null)
  it "reads olifant-dev's units and modules over this repository (Olifant's own model)" $ do
    m <- modelOf "olifant-dev" "." "olifant" id
    idsOf (Custom "haskell:unit") m `shouldSatisfy` elem "olifant:App"
    idsOf (Custom "haskell:module") m `shouldSatisfy` elem "olifant:Process/Cancel"
    [(relFrom r, relTo r) | r <- pmRelations m, relKind r == Covers] `shouldSatisfy` elem ("olifant:AppSpec", "olifant:App")
  it "refuses a kind outside the vocabulary unless it is namespaced" $ do
    isLeft (parseModelMap "[[elements]]\nkind = \"widget\"\nquery = { glob = \"*.x\" }\nid = \"{= stem =}\"\n") `shouldBe` True
    isLeft (parseModelMap "[[elements]]\nkind = \"elm:widget\"\nquery = { glob = \"*.x\" }\nid = \"{= stem =}\"\n") `shouldBe` False

idsOf :: Kind -> ProjectModel -> [Text]
idsOf k m = [elId e | e <- pmElements m, elKind e == k]

-- | A pack fixture's model: the pack's model.toml and languages, the
-- fixture's files (listed in the order @order@ gives).
modelOf :: Text -> FilePath -> Text -> ([FilePath] -> [FilePath]) -> IO ProjectModel
modelOf pack fixture project order = do
  tmp <- getTemporaryDirectory
  Right cat <- openPackCatalog (diskWorkspace ".") [(pack, "packs" </> T.unpack pack)]
  paths <- listFiles (diskWorkspace fixture) "."
  let queries = noQueries {queryAst = astQueries (astGrepSyntax (tmp </> "olifant-test-cache")) (catalogLanguages cat)}
      readFile' p = Just <$> TIO.readFile (fixture </> p)
  buildModel queries (order paths) readFile' (catalogModelMaps cat) project
