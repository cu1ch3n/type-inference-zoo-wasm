{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Alg (runAlgInferVariant, runAlgSubVariant, getAllAlgMeta)
import Data.Foldable (find)
import Lib (InferResult (..), inferResultToJson, algMetasToJson)
import Opt (Option (..), options)
import Parser (parseTrm, parseTyp)
import System.Console.GetOpt (ArgOrder (Permute), getOpt)
import System.Environment (getArgs)
import System.IO (readFile)

main :: IO ()
main = do
  args <- getArgs
  case getOpt Permute options args of
    (flags, [], [])
      | Meta `elem` flags -> putStrLn $ algMetasToJson getAllAlgMeta
      | Grammar `elem` flags -> do
          content <- readFile "lc.tmLanguage"
          putStr content
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
