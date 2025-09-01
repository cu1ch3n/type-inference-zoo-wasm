{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Typing.DK.Worklist.Bounded (runBounded, boundedMeta) where

import Control.Monad.Except (MonadError (throwError))
import Control.Monad.Writer (MonadTrans (lift), MonadWriter (tell))
import Data.Foldable (find)
import Lib (AlgMeta (..), Derivation (..), Example (..), InferMonad, InferResult (..), Paper (..), freshTVar)
import Syntax (Trm (..), Typ (..), latexifyVar)
import Typing.DK.Common (isAllB, isLam)
import Typing.DK.Worklist.Common (Entry (..), Judgment (..), TBind (..), Worklist, initWL, runInfer, substWLOrd)
import Unbound.Generics.LocallyNameless
  ( Fresh (fresh),
    Subst (subst),
    aeq,
    bind,
    fv,
    substBind,
    unbind,
  )
import Unbound.Generics.LocallyNameless.Internal.Fold (toListOf)

mono :: Worklist -> Typ -> Bool
mono _ TInt = True
mono _ TBool = True
mono ws (TArr ty1 ty2) = mono ws ty1 && mono ws ty2
mono _ (ETVar _) = True
mono ws (TVar a) = case find (\case WTVar a' (TVarBBind TTop) -> a == a'; _ -> False) ws of
  Just _ -> True
  Nothing -> False
mono _ _ = False

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
    WJug (Sub ty1@(TVar a) ty2) : ws'
      | Just (WTVar _ (TVarBBind b)) <-
          find (\case WTVar a' _ -> a == a'; _ -> False) ws',
        not (aeq ty1 ty2),
        not (aeq b TTop) -> do
          let ws'' = WJug (Sub b ty2) : ws'
          drvs <- infer "SubTVarTrans" ws''
          ret "SubTVarTrans" drvs
    WJug (Sub ty1@(STVar a) ty2) : ws'
      | Just (WTVar _ (STVarBBind b)) <-
          find (\case WTVar a' _ -> a == a'; _ -> False) ws',
        not (aeq ty1 ty2),
        not (aeq b TTop) -> do
          let ws'' = WJug (Sub b ty2) : ws'
          drvs <- infer "SubSTVarTrans" ws''
          ret "SubSTVarTrans" drvs
    WJug (Sub (TArr ty1 ty2) (TArr ty1' ty2')) : ws' -> do
      let ws'' = WJug (Sub ty1' ty1) : WJug (Sub ty2 ty2') : ws'
      drvs <- infer "SubArr" ws''
      ret "SubArr" drvs
    WJug (Sub (TAllB bnd b) ty2) : ws' | not (isAllB ty2) -> do
      -- ty2 is not Top as well
      (a, ty1) <- unbind bnd
      let ty1' = subst a (ETVar a) ty1
          ws'' =
            WJug (Sub ty1' ty2)
              : WJug (Sub (ETVar a) b)
              : WTVar a ETVarBind
              : ws'
      drvs <- infer "SubAllL" ws''
      ret "SubAllL" drvs
    WJug (Sub (TAllB bnd1 b1) (TAllB bnd2 b2)) : ws' -> do
      a <- freshTVar
      let ty1 = substBind bnd1 (ETVar a)
          ty2 = substBind bnd2 (ETVar a)
          ws'' =
            WJug (Sub ty1 ty2)
              : WJug (Sub b1 b2)
              : WJug (Sub b2 b1)
              : WTVar a (STVarBBind b1)
              : ws'
      drvs <- infer "SubAll" ws''
      ret "SubAll" drvs
    WJug (Sub (ETVar a) ty) : ws'
      | a `notElem` toListOf fv ty,
        mono ws ty -> do
          ws'' <- substWLOrd a ty ws'
          drvs <- infer "SubInstETVar" ws''
          ret "SubInstETVar" drvs
    WJug (Sub ty (ETVar a)) : ws'
      | a `notElem` toListOf fv ty,
        mono ws ty -> do
          ws'' <- substWLOrd a ty ws'
          drvs <- infer "SubInstETVar" ws''
          ret "SubInstETVar" drvs
    WJug (Sub (ETVar a) ty@(TArr ty1 ty2)) : ws'
      | a `notElem` toListOf fv ty -> do
          a1 <- fresh a
          a2 <- fresh a
          ws'' <-
            substWLOrd a (TArr (ETVar a1) (ETVar a2)) $
              WTVar a1 ETVarBind : WTVar a2 ETVarBind : ws'
          let ws''' = WJug (Sub (TArr (ETVar a1) (ETVar a2)) (TArr ty1 ty2)) : ws''
          drvs <- infer "SubSplitL" ws'''
          ret "SubSplitL" drvs
    WJug (Sub ty@(TArr ty1 ty2) (ETVar a)) : ws'
      | a `notElem` toListOf fv ty -> do
          a1 <- fresh a
          a2 <- fresh a
          ws'' <-
            substWLOrd a (TArr (ETVar a1) (ETVar a2)) $
              WTVar a1 ETVarBind : WTVar a2 ETVarBind : ws'
          let ws''' = WJug (Sub (TArr ty1 ty2) (TArr (ETVar a1) (ETVar a2))) : ws''
          drvs <- infer "SubSplitR" ws'''
          ret "SubSplitR" drvs
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
    entry@(WJug (Chk (Lam bnd) (ETVar a))) : ws' -> do
      (x, e) <- unbind bnd
      a1 <- fresh a
      a2 <- fresh a
      ws'' <-
        substWLOrd a (TArr (ETVar a1) (ETVar a2)) $
          entry : WJug (Chk e (ETVar a2)) : WVar x (ETVar a1) : ws'
      drvs <- infer "ChkLamSplit" ws''
      ret "ChkLamSplit" drvs
    WJug (Chk tm ty) : ws' | not $ isLam tm -> do
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
    WJug (Inf (TLamB bnd b) j) : ws' -> do
      (a, tm) <- unbind bnd
      case tm of -- to make my life easier
        Ann tm' ty -> do
          let ws'' =
                WJug (Chk tm' ty)
                  : WTVar a (TVarBBind b)
                  : WJug (substBind j (TAllB (bind a ty) b))
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
    WJug (Inf (App tm1 tm2) j) : ws' -> do
      a <- freshTVar
      b <- freshTVar
      let j' = Inf tm1 $ bind a $ Match (TVar a) $ bind b $ InfApp (TVar b) tm2 j
          ws'' = WJug j' : ws'
      drvs <- infer "InfApp" ws''
      ret "InfApp" drvs
    WJug (Inf (TApp tm ty) j) : ws' -> do
      a <- freshTVar
      let ws'' = WJug (Inf tm (bind a (InfTApp (TVar a) ty j))) : ws'
      drvs <- infer "InfTApp" ws''
      ret "InfTApp" drvs
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
    WJug (Match ty@(TArr _ _) j) : ws' -> do
      let ws'' = WJug (substBind j ty) : ws'
      drvs <- infer "MatchArr" ws''
      ret "MatchArr" drvs
    WJug (Match TBot j) : ws' -> do
      let ws'' = WJug (substBind j (TArr TBot TBot)) : ws'
      drvs <- infer "MatchBot" ws''
      ret "MatchBot" drvs
    WJug (Match (TAllB bnd b) j) : ws' -> do
      (a, ty) <- unbind bnd
      let j' = Match (subst a (ETVar a) ty) j
          ws'' = WJug j' : WJug (Sub (ETVar a) b) : WTVar a ETVarBind : ws'
      drvs <- infer "MatchAll" ws''
      ret "MatchAll" drvs
    WJug (Match (TVar a) j) : ws'
      | Just (WTVar _ (TVarBBind b)) <-
          find (\case WTVar a' _ -> a == a'; _ -> False) ws' -> do
          let ws'' = WJug (Match b j) : ws'
          drvs <- infer "MatchTVar" ws''
          ret "MatchTVar" drvs
    entry@(WJug (Match (ETVar a) _)) : ws' -> do
      a1 <- fresh a
      a2 <- fresh a
      ws'' <-
        substWLOrd a (TArr (ETVar a1) (ETVar a2)) $
          entry : WTVar a1 ETVarBind : WTVar a2 ETVarBind : ws'
      drvs <- infer "MatchETVar" ws''
      ret "MatchETVar" drvs
    WJug (InfApp (TArr ty1 ty2) tm j) : ws' -> do
      let ws'' = WJug (Chk tm ty1) : WJug (substBind j ty2) : ws'
      drvs <- infer "InfApp" ws''
      ret "InfApp" drvs
    WJug (InfTApp (TAllB bnd b) ty2 j) : ws' -> do
      (a, ty1) <- unbind bnd
      let j1 = substBind j (subst a ty2 ty1)
          j2 = Sub ty2 b
          ws'' = WJug j1 : WJug j2 : ws'
      drvs <- infer "InfTAppAll" ws''
      ret "InfTAppAll" drvs
    WJug (InfTApp TBot _ j) : ws' -> do
      let ws'' = WJug (substBind j TBot) : ws'
      drvs <- infer "InfTAppBot" ws''
      ret "InfTAppBot" drvs
    WJug (InfTApp (TVar a) ty2 j) : ws'
      | Just (WTVar _ (TVarBBind ty1)) <-
          find (\case WTVar a' _ -> a == a'; _ -> False) ws' -> do
          let ws'' = WJug (InfTApp ty1 ty2 j) : ws'
          drvs <- infer "InfTAppTVar" ws''
          ret "InfTAppTVar" drvs
    _ -> throwError $ "\\text{No matching rule for } " ++ show ws
  where
    ret :: String -> [Derivation] -> InferMonad [Derivation]
    ret ruleName drvs = do
      lift $ tell ["\\text{" ++ ruleName ++ ": } " ++ show ws]
      return $ (Derivation ruleName (show ws) []) : drvs

runBounded :: Trm -> InferResult
runBounded tm = runInfer infer (initWL tm)

-- Bounded algorithm metadata
boundedMeta :: AlgMeta
boundedMeta =
  AlgMeta
    { metaId = "Bounded",
      metaName = "Worklist (Bounded Quantification)",
      metaLabels = ["Global", "Unification", "Worklist", "System Fsub", "Dunfield-Krishnaswami", "Higher-Rank", "Implicit", "Explicit Type Application", "Bounded-Quantification"],
      metaViewMode = "linear",
      metaMode = "inference",
      metaPaper =
        Paper
          { paperTitle = "Greedy Implicit Bounded Quantification",
            paperAuthors = ["Chen Cui", "Shengyi Jiang", "Bruno C. d. S. Oliveira"],
            paperYear = 2023,
            paperUrl = "https://dl.acm.org/doi/10.1145/3622871"
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
