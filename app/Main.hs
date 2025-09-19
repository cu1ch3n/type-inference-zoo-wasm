{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Alg (getAllAlgMeta, runAlgInferVariant, runAlgSubVariant)
import Data.Foldable (find)
import Language.Haskell.TH (runIO)
import Language.Haskell.TH.Syntax (qAddDependentFile)
import Lib (InferResult (..), algMetasToJson, inferResultToJson)
import Opt (Option (..), options)
import Parser (parseTrm, parseTyp)
import System.Console.GetOpt (ArgOrder (Permute), getOpt)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case getOpt Permute options args of
    (flags, [], [])
      | Meta `elem` flags -> putStrLn $ algMetasToJson getAllAlgMeta
      | Grammar `elem` flags -> putStr $(qAddDependentFile "lc.tmLanguage" >> runIO (readFile "lc.tmLanguage") >>= \content -> [|content|])
    (flags, [code], [])
      | Just (Typing algName) <- find (\case Typing _ -> True; _ -> False) flags -> do
          let variant = case find (\case Variant _ -> True; _ -> False) flags of
                Just (Variant v) -> Just v
                _ -> Nothing
          case parseTrm code of
            Left err -> putStrLn $ inferResultToJson $ InferResult False Nothing [] (Just err) False
            Right tm -> putStrLn $ inferResultToJson $ runAlgInferVariant algName variant tm
    (flags, [source, target], [])
      | Just (Subtyping algName) <- find (\case Subtyping _ -> True; _ -> False) flags -> do
          let variant = case find (\case Variant _ -> True; _ -> False) flags of
                Just (Variant v) -> Just v
                _ -> Nothing
          case (parseTyp source, parseTyp target) of
            (Left err, _) -> putStrLn $ inferResultToJson $ InferResult False Nothing [] (Just err) False
            (_, Left err) -> putStrLn $ inferResultToJson $ InferResult False Nothing [] (Just err) False
            (Right src, Right tgt) -> putStrLn $ inferResultToJson $ runAlgSubVariant algName variant src tgt
    (_, _, errs) -> print errs
