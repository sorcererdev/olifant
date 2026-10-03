-- | The tools a generation may use: the syntax and check ports, the language
-- adapters and owned regions from the catalog, and the command runner for
-- queries (commands, AST). `noTools` needs nothing installed.
module Olifant.Generation.Tools (Tools (..), noTools, basicTools) where

import Olifant.Fill (ModelFiller)
import Olifant.Language
import Olifant.Query (Queries, noQueries)
import Olifant.Region (RegionDef)

data Tools m = Tools
  { toolSyntax :: Syntax m
  , toolChecker :: Checker m
  , toolLanguages :: [Language]
  , toolRegions :: [RegionDef]
  , toolQueries :: Queries m
  , -- | the model that fills holes, if any (spec §6.4)
    toolFiller :: Maybe (ModelFiller m)
  }

noTools :: Applicative m => Tools m
noTools = basicTools noSyntax noChecker []

-- | Syntax, checker and languages, with no regions and no commands.
basicTools :: Applicative m => Syntax m -> Checker m -> [Language] -> Tools m
basicTools syntax checker langs = Tools syntax checker langs [] noQueries Nothing
