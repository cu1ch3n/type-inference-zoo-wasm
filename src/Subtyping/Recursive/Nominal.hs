{-# LANGUAGE FlexibleInstances #-}

module Subtyping.Recursive.Nominal (runNominalSubtyping, runNominalSubtypingAlg, SubtypingResult (..), revisitingMeta) where

import Control.Monad.Writer (MonadTrans (lift), MonadWriter (tell))
import Lib (AlgMeta (..), Derivation (..), Example (..), InferMonad, InferResult (..), Paper (..), Rule (..), Variant (..), freshTVar, inferResultToJson, runInferMonad)
import Parser (parseTyp)
import Syntax (Typ (..))
import Unbound.Generics.LocallyNameless (subst, unbind)

-- Data structure for subtyping results
data SubtypingResult = SubtypingResult
  { isSubtype :: Bool,
    leftType :: Typ,
    rightType :: Typ,
    subtypingDerivation :: [Derivation],
    subtypingErrorMsg :: Maybe String
  }

-- Main entry point for recursive subtyping (unified interface: returns InferResult)
runNominalSubtyping :: Typ -> Typ -> InferResult
runNominalSubtyping source target =
  case runInferMonad $ do
    lift $ tell ["\\text{Recursive Subtyping: } " ++ show source ++ " <: " ++ show target]
    -- Run the subtyping algorithm producing a detailed derivation
    (result, drv) <- nominalSubDeriv source target
    return $
      SubtypingResult
        { isSubtype = result,
          leftType = source,
          rightType = target,
          subtypingDerivation = [drv],
          subtypingErrorMsg = Nothing
        } of
    Left errs -> InferResult False Nothing [] (Just $ unlines errs) False
    Right (res, msgs) ->
      let infoSteps = map (\msg -> Derivation "Info" msg []) msgs
       in InferResult
            (isSubtype res)
            (Just $ show (leftType res) ++ " <: " ++ show (rightType res))
            (infoSteps ++ subtypingDerivation res)
            (subtypingErrorMsg res)
            False

-- Build derivation tree and decision simultaneously
nominalSubDeriv :: Typ -> Typ -> InferMonad (Bool, Derivation)
nominalSubDeriv source target = do
  -- lift $ tell ["\\text{Derive: } " ++ show source ++ " <: " ++ show target]
  case (source, target) of
    -- Top
    (_, TTop) ->
      return
        ( True,
          Derivation {ruleId = "S-top", expression = show source ++ " <: \\top", children = []}
        )
    -- Refl on base types
    (TInt, TInt) ->
      return (True, Derivation {ruleId = "S-int", expression = "\\texttt{Int} <: \\texttt{Int}", children = []})
    (TBool, TBool) ->
      return (True, Derivation {ruleId = "S-bool", expression = "\\texttt{Bool} <: \\texttt{Bool}", children = []})
    -- Type variables
    (TVar v1, TVar v2) ->
      let ok = v1 == v2
       in return
            ( ok,
              Derivation
                { ruleId = "S-var",
                  expression = show (TVar v1) ++ " <: " ++ show (TVar v2),
                  children = []
                }
            )
    -- Arrows (contravariant in domain, covariant in codomain)
    (TArr s1 s2, TArr t1 t2) -> do
      (r1, d1) <- nominalSubDeriv t1 s1
      (r2, d2) <- nominalSubDeriv s2 t2
      let ok = r1 && r2
      return
        ( ok,
          Derivation
            { ruleId = "S-arrow",
              expression = show (TArr s1 s2) ++ " <: " ++ show (TArr t1 t2),
              children = [d1, d2]
            }
        )

    -- Intersection on the left: s1 & s2 <: t iff s1 <: t and s2 <: t
    (TIntersection s1 s2, t) -> do
      (r1, d1) <- nominalSubDeriv s1 t
      (r2, d2) <- nominalSubDeriv s2 t
      let ok = r1 && r2
      return
        ( ok,
          Derivation
            { ruleId = "S-inter-left",
              expression = show (TIntersection s1 s2) ++ " <: " ++ show t,
              children = if r1 then [d1] else if r2 then [d2] else [d1, d2]
            }
        )

    -- Intersection on the right: s <: t1 & t2 iff s <: t1 or s <: t2
    (s, TIntersection t1 t2) -> do
      (r1, d1) <- nominalSubDeriv s t1
      (r2, d2) <- nominalSubDeriv s t2
      let ok = r1 && r2
      return
        ( ok,
          Derivation
            { ruleId = "S-inter-right",
              expression = show s ++ " <: " ++ show (TIntersection t1 t2),
              children = [d1, d2]
            }
        )

    -- Recursive types: unfold with a shared fresh variable
    (TRecursive sBnd, TRecursive tBnd) -> do
      (sVar, sBody) <- unbind sBnd
      (tVar, tBody) <- unbind tBnd
      a <- freshTVar
      let sUnf = subst sVar (TVar a) sBody
          tUnf = subst tVar (TVar a) tBody
          sNUnf = subst sVar (TLabel a sUnf) sBody
          tNUnf = subst tVar (TLabel a tUnf) tBody
      (ok, d) <- nominalSubDeriv sNUnf tNUnf
      return
        ( ok,
          Derivation
            { ruleId = "S-mu",
              expression = show (TRecursive sBnd) ++ " <: " ++ show (TRecursive tBnd),
              children = [d]
            }
        )
    (TLabel slabel sty, TLabel rlabel rty) ->
      if slabel == rlabel
        then do
          (ok, d) <- nominalSubDeriv sty rty
          return
            ( ok,
              Derivation
                { ruleId = "S-label",
                  expression = show (TLabel slabel sty) ++ " <: " ++ show (TLabel rlabel rty),
                  children = [d]
                }
            )
        else
          return
            ( False,
              Derivation
                { ruleId = "S-fail",
                  expression = "label mismatch: " ++ show slabel ++ " \\textcolor{red}{\\nleq } " ++ show rlabel,
                  children = []
                }
            )
    -- the below two types should actually never appear in nominal subtyping

    -- Labeled recursive types: labels must match, then unfold together
    (TLabeled sLabel sBnd, TLabeled tLabel tBnd) ->
      if sLabel == tLabel
        then do
          (sVar, sBody) <- unbind sBnd
          (tVar, tBody) <- unbind tBnd
          a <- freshTVar
          let sUnf = subst sVar (TVar a) sBody
              tUnf = subst tVar (TVar a) tBody
          (ok, d) <- nominalSubDeriv sUnf tUnf
          return
            ( ok,
              Derivation
                { ruleId = "S-labeled",
                  expression = show (TLabeled sLabel sBnd) ++ " <: " ++ show (TLabeled tLabel tBnd),
                  children = [d]
                }
            )
        else
          return
            ( False,
              Derivation
                { ruleId = "S-fail",
                  expression = "label mismatch: " ++ show sLabel ++ " \\textcolor{red}{\\nleq } " ++ show tLabel,
                  children = []
                }
            )
    -- Translated mu with a type and a label variable; unfold both with shared fresh vars
    (TTranslatedMu sBnd, TTranslatedMu tBnd) -> do
      ((sTypeVar, sLabelVar), sBody) <- unbind sBnd
      ((tTypeVar, tLabelVar), tBody) <- unbind tBnd
      a <- freshTVar
      b <- freshTVar
      let sUnf = subst sTypeVar (TVar a) (subst sLabelVar (TVar b) sBody)
          tUnf = subst tTypeVar (TVar a) (subst tLabelVar (TVar b) tBody)
      (ok, d) <- nominalSubDeriv sUnf tUnf
      return
        ( ok,
          Derivation
            { ruleId = "S-transmu",
              expression = show (TTranslatedMu sBnd) ++ " <: " ++ show (TTranslatedMu tBnd),
              children = [d]
            }
        )

    -- Failure
    _ ->
      return
        ( False,
          Derivation
            { ruleId = "S-fail",
              expression = show source ++ "\\textcolor{red}{\\nleq}" ++ show target,
              children = []
            }
        )

-- Clean interface function (string inputs)
runNominalSubtypingAlg :: String -> String -> String
runNominalSubtypingAlg leftTypeStr rightTypeStr =
  case (parseTyp leftTypeStr, parseTyp rightTypeStr) of
    (Left err, _) -> inferResultToJson $ InferResult False Nothing [] (Just $ "Source type parse error: " ++ err) False
    (_, Left err) -> inferResultToJson $ InferResult False Nothing [] (Just $ "Target type parse error: " ++ err) False
    (Right source, Right target) -> inferResultToJson $ runNominalSubtyping source target

-- Subtyping algorithms metadata
revisitingMeta :: AlgMeta
revisitingMeta =
  AlgMeta
    { metaId = "Revisiting",
      metaName = "Revisiting Iso-Recursive Subtyping",
      metaLabels = ["Subtyping", "Recursive Types"],
      metaViewMode = "tree",
      metaMode = "subtyping",
      metaPaper =
        Paper
          { paperTitle = "Revisiting Iso-Recursive Subtyping",
            paperAuthors = ["Yaoda Zhou", "Jinxu Zhao", "Bruno C. d. S. Oliveira"],
            paperYear = 2022,
            paperUrl = "https://i.cs.hku.hk/~bruno/papers/toplas2022.pdf"
          },
      metaVariants =
        Just
          [ Variant "nominal" "Nominal" "Nominal Unfolding",
            Variant "double" "Recursive" "Double Unfolding"
          ],
      metaDefaultVariant = Just "nominal",
      metaRules =
        [ Rule "S-top" "S-top" [] "A <: \\top" Nothing Nothing,
          Rule "S-int" "S-int" [] "\\texttt{Int} <: \\texttt{Int}" Nothing Nothing,
          Rule "S-arrow" "S-arrow" ["B_1 <: A_1", "A_2 <: B_2"] "A_1 \\to A_2 <: B_1 \\to B_2" Nothing Nothing,
          Rule "S-mu" "S-mu" ["A [ \\{ a : A \\} / a ] <: B [ \\{ a : B \\} / a ]"] "\\mu a.~A <: \\mu a.~B" Nothing Nothing
        ],
      metaRuleGroups = Nothing,
      metaVariantRules = Nothing,
      metaExamples =
        [ Example
            { exampleName = "Positive Recursive Types",
              exampleExpression = "mu a. Top -> a <: mu a. Int -> a",
              exampleDescription = "Positive recursive subtyping"
            },
          Example
            { exampleName = "Negative Recursive Types (Fail)",
              exampleExpression = "mu a. a -> Int <: mu a. a -> Top",
              exampleDescription = "Negative recursive subtyping"
            },
          Example
            { exampleName = "Negative Recursive Types + Top",
              exampleExpression = "mu a. Top -> Int <: mu a. a -> Int",
              exampleDescription = "Recursive type subtyping"
            },
          Example
            { exampleName = "Nested Recursive Subtyping",
              exampleExpression = "mu a. Top -> (mu b. b -> a) <: mu a. Int -> (mu b. b -> a)",
              exampleDescription = "Nested recursive subtyping"
            }
        ]
    }
