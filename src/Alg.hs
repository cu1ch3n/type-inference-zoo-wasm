module Alg (runAlgInfer, runAlgSub, runAlgInferVariant, runAlgSubVariant, getAllAlgMeta) where

import Lib (AlgMeta (..), InferResult (..))
import Subtyping.Recursive.Fsubmu (fsubmuMeta)
import Subtyping.Recursive.Nominal (revisitingMeta, runNominalSubtyping)
import Syntax (Trm, Typ)
import Typing.DK.DK (dkMeta, runDK)
import Typing.DK.Worklist.Bounded (boundedMeta, runBounded)
import Typing.DK.Worklist.DK (runWorklist, worklistMeta)
import Typing.DK.Worklist.Elementary (elementaryMeta, runElementary)
import Typing.DK.Worklist.IU (iuMeta, runIU)
import Typing.HM.AlgR (algRMeta, runAlgR)
import Typing.HM.AlgW (algWMeta, runAlgW)
import Typing.Local.Contextual.Contextual (contextualMeta, runContextual)

-- | Algorithm types
data AlgEntry
  = InferAlg String AlgMeta (Trm -> InferResult)
  | SubAlg String AlgMeta (Typ -> Typ -> InferResult)

-- | Registry of all available algorithms
allAlgs :: [AlgEntry]
allAlgs =
  [ InferAlg "W" algWMeta runAlgW,
    InferAlg "R" algRMeta runAlgR,
    InferAlg "DK" dkMeta runDK,
    InferAlg "Contextual" contextualMeta runContextual,
    InferAlg "Worklist" worklistMeta runWorklist,
    InferAlg "Elementary" elementaryMeta runElementary,
    InferAlg "Bounded" boundedMeta runBounded,
    InferAlg "IU" iuMeta runIU,
    SubAlg "Revisiting" revisitingMeta runNominalSubtyping,
    SubAlg "Fsubmu" fsubmuMeta (\_ _ -> InferResult False Nothing [] (Just "Fsubmu not implemented") False)
  ]

-- | Extract algorithm ID
algId :: AlgEntry -> String
algId (InferAlg id' _ _) = id'
algId (SubAlg id' _ _) = id'

-- | Extract algorithm metadata
algMeta :: AlgEntry -> AlgMeta
algMeta (InferAlg _ meta _) = meta
algMeta (SubAlg _ meta _) = meta

-- | Run inference for a specific algorithm
runAlgInfer :: String -> Trm -> InferResult
runAlgInfer id' tm =
  case findAlg id' of
    Just (InferAlg _ _ runFn) -> runFn tm
    Just (SubAlg {}) -> InferResult False Nothing [] (Just $ id' ++ " is a sub alg, not an infer alg") False
    Nothing -> InferResult False Nothing [] (Just $ "Unknown alg: " ++ id') False

-- | Run subtyping for a specific algorithm
runAlgSub :: String -> Typ -> Typ -> InferResult
runAlgSub id' lty rty =
  case findAlg id' of
    Just (SubAlg _ _ runFn) -> runFn lty rty
    Just (InferAlg {}) -> InferResult False Nothing [] (Just $ id' ++ " is an infer alg, not a sub alg") False
    Nothing -> InferResult False Nothing [] (Just $ "Unknown alg: " ++ id') False

-- | Find algorithm by ID
findAlg :: String -> Maybe AlgEntry
findAlg targetId = foldr (\x acc -> if algId x == targetId then Just x else acc) Nothing allAlgs

-- | Run inference for a specific algorithm with variant (for now, ignore variant)
runAlgInferVariant :: String -> Maybe String -> Trm -> InferResult
runAlgInferVariant id' _variant = runAlgInfer id'

-- | Run subtyping for a specific algorithm with variant (for now, ignore variant)
runAlgSubVariant :: String -> Maybe String -> Typ -> Typ -> InferResult
runAlgSubVariant id' _variant = runAlgSub id'

-- | Get all algorithm metadata
getAllAlgMeta :: [AlgMeta]
getAllAlgMeta = map algMeta allAlgs
