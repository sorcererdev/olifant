-- | The tools a generation may use: the syntax and check ports, the language
-- adapters and owned regions from the catalog, and the command runner for
-- queries. 'noTools' needs nothing installed.
module Olifant.Generation.Tools (Tools (..), noTools, basicTools) where

import Olifant.Language
import Olifant.Query (Commands, noCommands)
import Olifant.Region (RegionDef)

data Tools m = Tools
  { toolSyntax :: Syntax m
  , toolChecker :: Checker m
  , toolLanguages :: [Language]
  , toolRegions :: [RegionDef]
  , toolCommands :: Commands m
  }

noTools :: Applicative m => Tools m
noTools = basicTools noSyntax noChecker []

-- | Syntax, checker and languages, with no regions and no commands.
basicTools :: Applicative m => Syntax m -> Checker m -> [Language] -> Tools m
basicTools syntax checker langs = Tools syntax checker langs [] noCommands
