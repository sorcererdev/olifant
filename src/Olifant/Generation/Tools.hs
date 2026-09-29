-- | The tools a generation may use: the syntax and check ports, and the
-- language adapters from the catalog. 'noTools' needs nothing installed.
module Olifant.Generation.Tools (Tools (..), noTools) where

import Olifant.Language

data Tools m = Tools {toolSyntax :: Syntax m, toolChecker :: Checker m, toolLanguages :: [Language]}

noTools :: Applicative m => Tools m
noTools = Tools noSyntax noChecker []
