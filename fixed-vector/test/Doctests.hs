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
  , "-XBinaryLiterals"
  , "-XConstrainedClassMethods"
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
  , "-XExplicitForAll"
  , "-XExplicitNamespaces"
  , "-XFieldSelectors"
  , "-XFlexibleContexts"
  , "-XFlexibleInstances"
  , "-XForeignFunctionInterface"
  , "-XGADTs"
  , "-XGADTSyntax"
  , "-XGeneralisedNewtypeDeriving"
  , "-XHexFloatLiterals"
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
  , "-XStarIsType"
  , "-XTraditionalRecordSyntax"
  , "-XTupleSections"
  , "-XTypeApplications"
  , "-XTypeOperators"
  , "-XTypeSynonymInstances"
    --
  , "-XDerivingVia"
  , "-XPatternSynonyms"
  , "-XViewPatterns"
  , "-XTypeFamilies"
  ]
