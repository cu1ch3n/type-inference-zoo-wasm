{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}

module Typing.DK.Worklist.IU (runIU, iuMeta) where

import Control.Monad.Except (MonadError (throwError))
import Control.Monad.Writer (MonadTrans (lift), MonadWriter (tell))
import Data.Foldable (find)
import Lib (AlgMeta (..), Derivation (..), Example (..), InferMonad, InferResult (..), Paper (..), freshTVar)
import Syntax (Trm (..), Typ (..), latexifyVar, pattern TAll, pattern TLam)
import Typing.DK.Worklist.Common (Entry (..), Judgment (..), TBind (..), Worklist, initWL, runInfer, substWLOrd)
import Unbound.Generics.LocallyNameless
  ( Subst (subst),
    bind,
    fv,
    substBind,
    unbind,
  )
import Unbound.Generics.LocallyNameless.Internal.Fold (toListOf)

notAll :: Typ -> Bool
notAll TInt = True
notAll TBool = True
notAll TTop = True
notAll TBot = False
notAll (TArr _ _) = True
notAll (TVar _) = True
notAll (ETVar _) = True
notAll (STVar _) = False
notAll (TAll _) = False
notAll (TIntersection ty1 ty2) = notAll ty1 && notAll ty2
notAll (TUnion ty1 ty2) = notAll ty1 || notAll ty2
notAll _ = False

mono :: Typ -> Bool
mono TInt = True
mono TBool = True
mono TTop = False
mono TBot = False
mono (TArr ty1 ty2) = mono ty1 && mono ty2
mono (TVar _) = True
mono (ETVar _) = True
mono (STVar _) = False
mono (TAll _) = False
mono (TIntersection _ _) = False
mono (TUnion _ _) = False
mono _ = False

infer :: String -> Worklist -> InferMonad [Derivation]
infer rule ws = do
  lift $ tell [show ws]
  case ws of
    [] -> ret rule []
    WTVar _ _ : ws' -> do
      drvs <- infer "GCTVar" ws'
      ret "GCTVar" drvs
    WVar _ _ : ws' -> do
      drvs <- infer "GCVar" ws'
      ret "GCVar" drvs
    WJug (Sub TInt TInt) : ws' -> do
      drvs <- infer "SubReflInt" ws'
      ret "SubReflInt" drvs
    WJug (Sub TBool TBool) : ws' -> do
      drvs <- infer "SubReflBool" ws'
      ret "SubReflBool" drvs
    WJug (Sub (TVar a) (TVar b)) : ws' | a == b -> do
      drvs <- infer "SubReflTVar" ws'
      ret "SubReflTVar" drvs
    WJug (Sub (ETVar a) (ETVar b)) : ws' | a == b -> do
      drvs <- infer "SubReflETVar" ws'
      ret "SubReflETVar" drvs
    WJug (Sub (STVar a) (STVar b)) : ws' | a == b -> do
      drvs <- infer "SubReflSTVar" ws'
      ret "SubReflSTVar" drvs
    WJug (Sub _ TTop) : ws' -> do
      drvs <- infer "SubTop" ws'
      ret "SubTop" drvs
    WJug (Sub TBot _) : ws' -> do
      drvs <- infer "SubBot" ws'
      ret "SubBot" drvs
    WJug (Sub (TArr ty1 ty2) (TArr ty1' ty2')) : ws' -> do
      let ws'' = WJug (Sub ty1' ty1) : WJug (Sub ty2 ty2') : ws'
      drvs <- infer "SubArr" ws''
      ret "SubArr" drvs
    WJug (Sub (TAll bnd1) (TAll bnd2)) : ws' -> do
      a <- freshTVar
      let ty1 = substBind bnd1 (ETVar a)
          ty2 = substBind bnd2 (ETVar a)
          ws'' = WJug (Sub ty1 ty2) : WTVar a STVarBind : ws'
      drvs <- infer "SubAll" ws''
      ret "SubAll" drvs
    WJug (Sub (TAll bnd) ty2) : ws' | notAll ty2 -> do
      (a, ty1) <- unbind bnd
      let ty1' = subst a (ETVar a) ty1
          ws'' = WJug (Sub ty1' ty2) : WTVar a ETVarBind : ws'
      drvs <- infer "SubAllL" ws''
      ret "SubAllL" drvs
    WJug (Sub (ETVar a) ty) : ws'
      | a `notElem` toListOf fv ty,
        mono ty -> do
          ws'' <- substWLOrd a ty ws'
          drvs <- infer "SubInstETVar" ws''
          ret "SubInstETVar" drvs
    WJug (Sub ty (ETVar a)) : ws'
      | a `notElem` toListOf fv ty,
        mono ty -> do
          ws'' <- substWLOrd a ty ws'
          drvs <- infer "SubInstETVar" ws''
          ret "SubInstETVar" drvs
    -- Simplified intersection/union handling
    WJug (Sub (TIntersection ty1 _) ty) : ws' -> do
      let ws'' = WJug (Sub ty1 ty) : ws'
      drvs <- infer "SubIntersectionL" ws''
      ret "SubIntersectionL" drvs
    WJug (Sub ty (TIntersection ty1 ty2)) : ws' -> do
      let ws'' = WJug (Sub ty ty1) : WJug (Sub ty ty2) : ws'
      drvs <- infer "SubIntersection" ws''
      ret "SubIntersection" drvs
    WJug (Sub (TUnion ty1 ty2) ty) : ws' -> do
      let ws'' = WJug (Sub ty1 ty) : WJug (Sub ty2 ty) : ws'
      drvs <- infer "SubUnion" ws''
      ret "SubUnion" drvs
    WJug (Sub ty (TUnion ty1 _)) : ws' -> do
      let ws'' = WJug (Sub ty ty1) : ws'
      drvs <- infer "SubUnionL" ws''
      ret "SubUnionL" drvs
    WJug (Out _) : ws' -> do
      drvs <- infer "Out" ws'
      ret "Out" drvs
    WJug (Chk (Lam bnd) TTop) : ws' -> do
      (x, e) <- unbind bnd
      let ws'' = WJug (Chk e TTop) : WVar x TBot : ws'
      drvs <- infer "ChkLamTop" ws''
      ret "ChkLamTop" drvs
    WJug (Chk (Lam bnd) (TArr ty1 ty2)) : ws' -> do
      (x, e) <- unbind bnd
      let ws'' = WJug (Chk e ty2) : WVar x ty1 : ws'
      drvs <- infer "ChkLam" ws''
      ret "ChkLam" drvs
    WJug (Chk tm (TIntersection ty1 ty2)) : ws' -> do
      let ws'' = WJug (Chk tm ty1) : WJug (Chk tm ty2) : ws'
      drvs <- infer "ChkIntersection" ws''
      ret "ChkIntersection" drvs
    WJug (Chk tm ty) : ws' -> do
      a <- freshTVar
      let ws'' = WJug (Inf tm (bind a (Sub (TVar a) ty))) : ws'
      drvs <- infer "ChkSub" ws''
      ret "ChkSub" drvs
    WJug (Inf (Var x) j) : ws' -> do
      case find (\case WVar x' _ -> x == x'; _ -> False) ws' of
        Just (WVar _ ty) -> do
          let ws'' = WJug (substBind j ty) : ws'
          drvs <- infer "InfVar" ws''
          ret "InfVar" drvs
        _ -> throwError $ "\\text{Variable } " ++ latexifyVar x ++ " \\text{ is not found}"
    WJug (Inf (Ann tm ty) j) : ws' -> do
      let ws'' = WJug (Chk tm ty) : WJug (substBind j ty) : ws'
      drvs <- infer "InfAnn" ws''
      ret "InfAnn" drvs
    WJug (Inf (TLam bnd) j) : ws' -> do
      (a, tm) <- unbind bnd
      case tm of
        Ann tm' ty -> do
          let ws'' =
                WJug (Chk tm' ty)
                  : WTVar a TVarBind
                  : WJug (substBind j (TAll (bind a ty)))
                  : ws'
          drvs <- infer "InfTLam" ws''
          ret "InfTLam" drvs
        _ -> throwError $ "\\text{Term } " ++ show tm ++ " \\text{ is not an annotated term}"
    WJug (Inf (LitInt _) j) : ws' -> do
      let ws'' = WJug (substBind j TInt) : ws'
      drvs <- infer "InfLitInt" ws''
      ret "InfLitInt" drvs
    WJug (Inf (LitBool _) j) : ws' -> do
      let ws'' = WJug (substBind j TBool) : ws'
      drvs <- infer "InfLitBool" ws''
      ret "InfLitBool" drvs
    WJug (Inf (Lam bnd) j) : ws' -> do
      a <- freshTVar
      b <- freshTVar
      (x, e) <- unbind bnd
      let ws'' =
            WJug (Chk e (ETVar b))
              : WVar x (ETVar a)
              : WJug (substBind j (TArr (ETVar a) (ETVar b)))
              : WTVar a ETVarBind
              : WTVar b ETVarBind
              : ws'
      drvs <- infer "InfLam" ws''
      ret "InfLam" drvs
    WJug (Inf (App tm1 tm2) j) : ws' -> do
      a <- freshTVar
      b <- freshTVar
      let j' = Inf tm1 $ bind a $ Match (TVar a) $ bind b $ InfApp (TVar b) tm2 j
          ws'' = WJug j' : ws'
      drvs <- infer "InfApp" ws''
      ret "InfApp" drvs
    WJug (Match ty@(TArr _ _) j) : ws' -> do
      let ws'' = WJug (substBind j ty) : ws'
      drvs <- infer "MatchArr" ws''
      ret "MatchArr" drvs
    WJug (Match TBot j) : ws' -> do
      let ws'' = WJug (substBind j (TArr TBot TBot)) : ws'
      drvs <- infer "MatchBot" ws''
      ret "MatchBot" drvs
    WJug (Match (TAll bnd) j) : ws' -> do
      (a, ty) <- unbind bnd
      let j' = Match (subst a (ETVar a) ty) j
          ws'' = WJug j' : WTVar a ETVarBind : ws'
      drvs <- infer "MatchAll" ws''
      ret "MatchAll" drvs
    WJug (InfApp (TArr ty1 ty2) tm j) : ws' -> do
      let ws'' = WJug (Chk tm ty1) : WJug (substBind j ty2) : ws'
      drvs <- infer "InfApp" ws''
      ret "InfApp" drvs
    WJug (InfTApp (TAll bnd) ty2 j) : ws' -> do
      let ws'' = WJug (substBind j (substBind bnd ty2)) : ws'
      drvs <- infer "InfTAppAll" ws''
      ret "InfTAppAll" drvs
    WJug (InfTApp TBot _ j) : ws' -> do
      let ws'' = WJug (substBind j TBot) : ws'
      drvs <- infer "InfTAppBot" ws''
      ret "InfTAppBot" drvs
    _ -> throwError $ "\\text{No matching rule for } " ++ show ws
  where
    ret :: String -> [Derivation] -> InferMonad [Derivation]
    ret ruleName drvs = do
      lift $ tell ["\\text{" ++ ruleName ++ ": } " ++ show ws]
      return $ (Derivation ruleName (show ws) []) : drvs

runIU :: Trm -> InferResult
runIU tm = runInfer infer (initWL tm)

-- IU algorithm metadata
iuMeta :: AlgMeta
iuMeta =
  AlgMeta
    { metaId = "IU",
      metaName = "Worklist (Intersection and Union)",
      metaLabels = ["Global", "Unification", "Worklist", "System F", "Dunfield-Krishnaswami", "Higher-Rank", "Implicit", "Explicit Type Application", "Intersection-Union"],
      metaViewMode = "linear",
      metaMode = "inference",
      metaPaper =
        Paper
          { paperTitle = "Bidirectional Higher-Rank Polymorphism with Intersection and Union Types",
            paperAuthors = ["Shengyi Jiang", "Chen Cui", "Bruno C. d. S. Oliveira"],
            paperYear = 2025,
            paperUrl = "https://i.cs.hku.hk/~bruno/papers/popl25_hrp.pdf"
          },
      metaVariants = Nothing,
      metaDefaultVariant = Nothing,
      metaRules = [],
      metaRuleGroups = Nothing,
      metaVariantRules = Nothing,
      metaExamples =
        [ Example
            { exampleName = "Trivial Application",
              exampleExpression = "(\\x. x) 1",
              exampleDescription = "Trivial function application of identity function to integer literal"
            }
        ]
    }
