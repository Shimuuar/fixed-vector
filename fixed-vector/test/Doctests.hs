module Main
where

import Test.DocTest
import System.FilePath.Find ((==?), always, extension, find)

find_sources :: IO [FilePath]
find_sources = find always (extension ==? ".hs") "Data"

main :: IO ()
main = do
  sources <- find_sources
  doctest $ exts ++ sources


exts :: [String]
exts =
  [ "-XBangPatterns"
  , "-XConstraintKinds"
  , "-XDataKinds"
  , "-XDeriveDataTypeable"
  , "-XDeriveFoldable"
  , "-XDeriveFunctor"
  , "-XDeriveGeneric"
  , "-XDeriveLift"
  , "-XDeriveTraversable"
  , "-XDerivingStrategies"
  , "-XDisambiguateRecordFields"
  , "-XDoAndIfThenElse"
  , "-XEmptyCase"
  , "-XEmptyDataDecls"
  , "-XEmptyDataDeriving"
  , "-XExistentialQuantification"
  , "-XExplicitNamespaces"
  , "-XFlexibleContexts"
  , "-XFlexibleInstances"
  , "-XForeignFunctionInterface"
  , "-XGADTs"
  , "-XGADTSyntax"
  , "-XGeneralisedNewtypeDeriving"
  , "-XImplicitPrelude"
  , "-XImportQualifiedPost"
  , "-XInstanceSigs"
  , "-XKindSignatures"
  , "-XLambdaCase"
  , "-XMonoLocalBinds"
  , "-XMonomorphismRestriction"
  , "-XMultiParamTypeClasses"
  , "-XNamedFieldPuns"
  , "-XNamedWildCards"
  , "-XNumericUnderscores"
  , "-XPatternGuards"
  , "-XPostfixOperators"
  , "-XRankNTypes"
  , "-XRelaxedPolyRec"
  , "-XRoleAnnotations"
  , "-XScopedTypeVariables"
  , "-XStandaloneDeriving"
  , "-XStandaloneKindSignatures"
  , "-XTupleSections"
  , "-XTypeApplications"
  , "-XTypeOperators"
  , "-XTypeSynonymInstances"
    --
  , "-XDerivingVia"
  , "-XPatternSynonyms"
  , "-XViewPatterns"
  , "-XTypeFamilies"
  , "-XFunctionalDependencies"
  ]
