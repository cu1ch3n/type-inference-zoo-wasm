{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}

module Typing.DK.Worklist.Elementary (runElementary, elementaryMeta) where

import Control.Monad.Except (MonadError (throwError))
import Control.Monad.Writer (MonadTrans (lift), MonadWriter (tell))
import Data.Foldable (find)
import Lib (AlgMeta (..), Derivation (..), Example (..), InferMonad, InferResult (..), Paper (..), freshTVar)
import Syntax (Trm (..), Typ (..), latexifyVar, pattern TAll, pattern TLam)
import Typing.DK.Common (isAll)
import Typing.DK.Worklist.Common (Entry (..), Judgment (..), TBind (..), Worklist, before, initWL, runInfer, substWL)
import Unbound.Generics.LocallyNameless
  ( Fresh (fresh),
    Subst (subst),
    bind,
    fv,
    substBind,
    unbind,
  )
import Unbound.Generics.LocallyNameless.Internal.Fold (toListOf)

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
    WJug (Sub (TAll bnd) ty2) : ws' | not (isAll ty2) -> do
      -- ty2 is not Top as well
      (a, ty1) <- unbind bnd
      let ty1' = subst a (ETVar a) ty1
          ws'' = WJug (Sub ty1' ty2) : WTVar a ETVarBind : ws'
      drvs <- infer "SubAllL" ws''
      ret "SubAllL" drvs
    WJug (Sub (TAll bnd1) (TAll bnd2)) : ws' -> do
      a <- freshTVar
      let ty1 = substBind bnd1 (ETVar a)
          ty2 = substBind bnd2 (ETVar a)
          ws'' = WJug (Sub ty1 ty2) : WTVar a STVarBind : ws'
      drvs <- infer "SubAll" ws''
      ret "SubAll" drvs
    WJug (Sub (ETVar a) ty@(TArr ty1 ty2)) : ws'
      | a `notElem` toListOf fv ty -> do
          a1 <- fresh a
          a2 <- fresh a
          ws'' <- substWL a (TArr (ETVar a1) (ETVar a2)) [WTVar a1 ETVarBind, WTVar a2 ETVarBind] ws'
          let ws''' = WJug (Sub (TArr (ETVar a1) (ETVar a2)) (TArr ty1 ty2)) : ws''
          drvs <- infer "SubSplitL" ws'''
          ret "SubSplitL" drvs
    WJug (Sub ty@(TArr ty1 ty2) (ETVar a)) : ws'
      | a `notElem` toListOf fv ty -> do
          a1 <- fresh a
          a2 <- fresh a
          ws'' <- substWL a (TArr (ETVar a1) (ETVar a2)) [WTVar a1 ETVarBind, WTVar a2 ETVarBind] ws'
          let ws''' = WJug (Sub (TArr ty1 ty2) (TArr (ETVar a1) (ETVar a2))) : ws''
          drvs <- infer "SubSplitR" ws'''
          ret "SubSplitR" drvs
    WJug (Sub (ETVar a) (ETVar b)) : ws' | before ws' a b -> do
      ws'' <- substWL b (ETVar a) [] ws'
      drvs <- infer "SubInstETVar1" ws''
      ret "SubInstETVar1" drvs
    WJug (Sub (ETVar b) (ETVar a)) : ws' | before ws' a b -> do
      ws'' <- substWL b (ETVar a) [] ws'
      drvs <- infer "SubInstETVar2" ws''
      ret "SubInstETVar2" drvs
    WJug (Sub (TVar a) (ETVar b)) : ws' | before ws' a b -> do
      ws'' <- substWL b (TVar a) [] ws'
      drvs <- infer "SubInstETVar3" ws''
      ret "SubInstETVar3" drvs
    WJug (Sub (ETVar b) (TVar a)) : ws' | before ws' a b -> do
      ws'' <- substWL b (ETVar a) [] ws'
      drvs <- infer "SubInstETVar4" ws''
      ret "SubInstETVar4" drvs
    WJug (Sub TInt (ETVar b)) : ws' -> do
      ws'' <- substWL b TInt [] ws'
      drvs <- infer "SubInstETVar5" ws''
      ret "SubInstETVar5" drvs
    WJug (Sub (ETVar b) TInt) : ws' -> do
      ws'' <- substWL b TInt [] ws'
      drvs <- infer "SubInstETVar6" ws''
      ret "SubInstETVar6" drvs
    WJug (Sub TBool (ETVar b)) : ws' -> do
      ws'' <- substWL b TBool [] ws'
      drvs <- infer "SubInstETVar7" ws''
      ret "SubInstETVar7" drvs
    WJug (Sub (ETVar b) TBool) : ws' -> do
      ws'' <- substWL b TBool [] ws'
      drvs <- infer "SubInstETVar8" ws''
      ret "SubInstETVar8" drvs
    WJug (Out _) : ws' -> do
      drvs <- infer "Out" ws'
      ret "Out" drvs
    WJug (Chk _ TTop) : ws' -> do
      drvs <- infer "ChkTop" ws'
      ret "ChkTop" drvs
    WJug (Chk tm (TAll bnd)) : ws' -> do
      (a, ty) <- unbind bnd
      let ws'' = WJug (Chk tm ty) : WTVar a TVarBind : ws'
      drvs <- infer "ChkAll" ws''
      ret "ChkAll" drvs
    WJug (Chk (Lam bnd) (TArr ty1 ty2)) : ws' -> do
      (x, e) <- unbind bnd
      let ws'' = WJug (Chk e ty2) : WVar x ty1 : ws'
      drvs <- infer "ChkLam" ws''
      ret "ChkLam" drvs
    WJug (Chk (Lam bnd) (ETVar a)) : ws' -> do
      (x, e) <- unbind bnd
      a1 <- fresh a
      a2 <- fresh a
      ws'' <-
        substWL a (TArr (ETVar a1) (ETVar a2)) [WTVar a1 ETVarBind, WTVar a2 ETVarBind] $
          WJug (Chk e (ETVar a2)) : WVar x (ETVar a1) : ws'
      drvs <- infer "ChkLamSplit" ws''
      ret "ChkLamSplit" drvs
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
      case tm of -- to make my life easier
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
      let ws'' = WJug (Inf tm1 (bind a (InfApp (TVar a) tm2 j))) : ws'
      drvs <- infer "InfApp" ws''
      ret "InfApp" drvs
    WJug (Inf (TApp tm ty) j) : ws' -> do
      a <- freshTVar
      let ws'' = WJug (Inf tm (bind a (InfTApp (TVar a) ty j))) : ws'
      drvs <- infer "InfTApp" ws''
      ret "InfTApp" drvs
    WJug (InfTApp (TAll bnd) ty2 j) : ws' -> do
      (a, ty1) <- unbind bnd
      let ws'' = WJug (substBind j (subst a ty2 ty1)) : ws'
      drvs <- infer "InfTAppAll" ws''
      ret "InfTAppAll" drvs
    WJug (InfTApp TBot _ j) : ws' -> do
      let ws'' = WJug (substBind j TBot) : ws'
      drvs <- infer "InfTAppBot" ws''
      ret "InfTAppBot" drvs
    WJug (InfApp (TAll bnd) tm j) : ws' -> do
      (a, ty) <- unbind bnd
      let ws'' = WJug (InfApp (subst a (ETVar a) ty) tm j) : WTVar a ETVarBind : ws'
      drvs <- infer "InfAppAll" ws''
      ret "InfAppAll" drvs
    WJug (InfApp (TArr ty1 ty2) tm j) : ws' -> do
      let ws'' = WJug (Chk tm ty1) : WJug (substBind j ty2) : ws'
      drvs <- infer "InfAppArr" ws''
      ret "InfAppArr" drvs
    WJug (InfApp TBot _ j) : ws' -> do
      let ws'' = WJug (substBind j TBot) : ws'
      drvs <- infer "InfAppBot" ws''
      ret "InfAppBot" drvs
    WJug (InfApp (ETVar a) tm j) : ws' -> do
      a1 <- fresh a
      a2 <- fresh a
      ws'' <-
        substWL a (TArr (ETVar a1) (ETVar a2)) [WTVar a1 ETVarBind, WTVar a2 ETVarBind] $
          WJug (InfApp (TArr (ETVar a1) (ETVar a2)) tm j) : ws'
      drvs <- infer "InfAppETVar" ws''
      ret "InfAppETVar" drvs
    _ -> throwError $ "\\text{No matching rule for } " ++ show ws
  where
    ret :: String -> [Derivation] -> InferMonad [Derivation]
    ret ruleName drvs = do
      lift $ tell ["\\text{" ++ ruleName ++ ": } " ++ show ws]
      return $ (Derivation ruleName (show ws) []) : drvs

runElementary :: Trm -> InferResult
runElementary tm = runInfer infer (initWL tm)

-- Elementary algorithm metadata
elementaryMeta :: AlgMeta
elementaryMeta =
  AlgMeta
    { metaId = "Elementary",
      metaName = "Worklist (Elementary)",
      metaLabels = ["Global", "Unification", "Worklist", "Dunfield-Krishnaswami", "System F", "Higher-Rank", "Implicit", "Explicit Type Application"],
      metaViewMode = "linear",
      metaMode = "inference",
      metaPaper =
        Paper
          { paperTitle = "Elementary Type Inference",
            paperAuthors = ["Jinxu Zhao", "Bruno C. d. S. Oliveira"],
            paperYear = 2022,
            paperUrl = "https://drops.dagstuhl.de/entities/document/10.4230/LIPIcs.ECOOP.2022.2"
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
