{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

module Typing.DK.DK (runDK, dkMeta) where

import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.Writer (MonadTrans (lift), MonadWriter (tell))
import Data.Foldable (find)
import Data.List (intercalate)
import Lib (AlgMeta (..), Derivation (..), Example (..), InferMonad, InferResult (..), Paper (..), break3, freshTVar, runInferMonad)
import Syntax (TmVar, Trm (..), TyVar, Typ (..), latexifyVar, wrapVar, pattern TAll)
import Typing.DK.Common (isAll)
import Unbound.Generics.LocallyNameless (bind, fv, subst, unbind)
import Unbound.Generics.LocallyNameless.Internal.Fold (toListOf)

data Entry
  = VarBnd TmVar Typ
  | TVarBnd TyVar
  | ETVarBnd TyVar
  | SETVarBnd TyVar Typ
  | Mark TyVar

type Ctx = [Entry]

instance Show Entry where
  show :: Entry -> String
  show (VarBnd x ty) = latexifyVar x ++ ": " ++ show ty
  show (TVarBnd a) = latexifyVar a
  show (ETVarBnd a) = wrapVar "hat" a
  show (SETVarBnd a ty) = wrapVar "hat" a ++ " = " ++ show ty
  show (Mark a) = "\\blacktriangleright " ++ latexifyVar a

instance {-# OVERLAPPING #-} Show [Entry] where
  show :: [Entry] -> String
  show = intercalate ", " . map show . reverse

mono :: Typ -> Bool
mono TInt = True
mono TBool = True
mono (TVar _) = True
mono (ETVar _) = True
mono (TArr ty1 ty2) = mono ty1 && mono ty2
mono (TAll _) = False
mono ty = error $ "mono: not implemented for " ++ show ty

appCtxTyp :: Ctx -> Typ -> InferMonad Typ
appCtxTyp ctx ty = do
  lift $ tell ["\\text{Substituting: } " ++ showAppCtxTypIn]
  case ty of
    TVar a -> ret $ TVar a
    TInt -> ret TInt
    TBool -> ret TBool
    ETVar a
      | Just (SETVarBnd _ ty') <- find (\case SETVarBnd a' _ -> a == a'; _ -> False) ctx -> ret ty'
      | otherwise -> ret $ ETVar a
    TArr ty1 ty2 -> do
      ty1' <- appCtxTyp ctx ty1
      ty2' <- appCtxTyp ctx ty2
      ret $ TArr ty1' ty2'
    TAll bnd -> do
      (a, ty') <- unbind bnd
      ty'' <- appCtxTyp ctx ty'
      ret $ TAll (bind a ty'')
    _ -> throwError $ "\\text{appCtxTyp: not implemented for } " ++ show ty
  where
    showAppCtxTypIn = "[" ++ show ctx ++ "](" ++ show ty ++ ")"
    showAppCtxTyp ty' = "[" ++ show ctx ++ "](" ++ show ty ++ ") = " ++ show ty'

    ret :: Typ -> InferMonad Typ
    ret ty' = do
      lift $ tell ["\\text{Substituted: } " ++ showAppCtxTyp ty']
      return ty'

before :: Ctx -> TyVar -> TyVar -> Bool
before ws a b =
  let (ws1, _) = break (\case ETVarBnd a' -> a == a'; _ -> False) ws
      (ws1', _) = break (\case ETVarBnd b' -> b == b'; _ -> False) ws
   in length ws1 > length ws1'

instL :: Ctx -> TyVar -> Typ -> InferMonad (Ctx, Derivation)
instL ctx a ty = do
  lift $ tell ["\\text{InstL: } " ++ showInstLIn]
  case ty of
    ETVar b | before ctx a b -> do
      let (ctx1, _, ctx2) = break3 (\case ETVarBnd b' -> b == b'; _ -> False) ctx
      ret "InstLReach" (ctx1 ++ SETVarBnd b (ETVar a) : ctx2) []
    TArr ty1 ty2 -> do
      a1 <- freshTVar
      a2 <- freshTVar
      let (ctx1, _, ctx2) = break3 (\case ETVarBnd a' -> a == a'; _ -> False) ctx
          ctx' = ctx1 ++ SETVarBnd a (TArr (ETVar a1) (ETVar a2)) : ETVarBnd a1 : ETVarBnd a2 : ctx2
      (ctx'', drv1) <- instR ctx' ty1 a1
      ty2' <- appCtxTyp ctx'' ty2
      (ctx''', drv2) <- instL ctx'' a2 ty2'
      ret "InstLArr" ctx''' [drv1, drv2]
    TAll bnd -> do
      (b, ty') <- unbind bnd
      (ctx', drv) <- instL (TVarBnd b : ctx) a ty'
      let (_, _, ctx2) = break3 (\case TVarBnd b' -> b == b'; _ -> False) ctx'
      ret "InstLAllR" ctx2 [drv]
    _ | mono ty -> do
      let (ctx1, _, ctx2) = break3 (\case ETVarBnd a' -> a == a'; _ -> False) ctx
      ret "InstLSolve" (ctx1 ++ SETVarBnd a ty : ctx2) []
    _ -> throwError $ "\\text{No rule matching: } " ++ showInstLIn
  where
    showInstLIn = show ctx ++ " \\vdash \\hat{" ++ latexifyVar a ++ "} \\sqsubseteq " ++ show ty
    showInstL ctx' = showInstLIn ++ " \\dashv " ++ show ctx'

    ret :: String -> Ctx -> [Derivation] -> InferMonad (Ctx, Derivation)
    ret rule ctx' drvs = do
      lift $ tell ["\\text{InstL[" ++ rule ++ "]: } " ++ showInstL ctx']
      return (ctx', Derivation rule (showInstL ctx') drvs)

instR :: Ctx -> Typ -> TyVar -> InferMonad (Ctx, Derivation)
instR ctx ty a = do
  lift $ tell ["\\text{InstR: } " ++ showInstRIn]
  case ty of
    ETVar b | before ctx a b -> do
      let (ctx1, _, ctx2) = break3 (\case ETVarBnd b' -> b == b'; _ -> False) ctx
      ret "InstRReach" (ctx1 ++ SETVarBnd b (ETVar a) : ctx2) []
    TArr ty1 ty2 -> do
      a1 <- freshTVar
      a2 <- freshTVar
      let (ctx1, _, ctx2) = break3 (\case ETVarBnd a' -> a == a'; _ -> False) ctx
          ctx' = ctx1 ++ SETVarBnd a (TArr (ETVar a1) (ETVar a2)) : ETVarBnd a1 : ETVarBnd a2 : ctx2
      (ctx'', drv1) <- instL ctx' a1 ty1
      ty2' <- appCtxTyp ctx'' ty2
      (ctx''', drv2) <- instR ctx'' ty2' a2
      ret "InstRArr" ctx''' [drv1, drv2]
    TAll bnd -> do
      (b, ty') <- unbind bnd
      let ty'' = subst b (ETVar b) ty'
      (ctx', drv) <- instR (ETVarBnd b : Mark b : ctx) ty'' a
      let (_, _, ctx2) = break3 (\case Mark b' -> b == b'; _ -> False) ctx'
      ret "InstRAllL" ctx2 [drv]
    _ | mono ty -> do
      let (ctx1, _, ctx2) = break3 (\case ETVarBnd a' -> a == a'; _ -> False) ctx
      ret "InstRSolve" (ctx1 ++ SETVarBnd a ty : ctx2) []
    _ -> throwError $ "\\text{No rule matching: } " ++ showInstRIn
  where
    showInstRIn = show ctx ++ " \\vdash " ++ show ty ++ " \\sqsubseteq \\hat{" ++ latexifyVar a ++ "}"
    showInstR ctx' = showInstRIn ++ " \\dashv " ++ show ctx'

    ret :: String -> Ctx -> [Derivation] -> InferMonad (Ctx, Derivation)
    ret rule ctx' drvs = do
      lift $ tell ["\\text{InstR[" ++ rule ++ "]: } " ++ showInstR ctx']
      return (ctx', Derivation rule (showInstR ctx') drvs)

sub :: Ctx -> Typ -> Typ -> InferMonad (Ctx, Derivation)
sub ctx ty1 ty2 = do
  lift $ tell ["\\text{Subtyping: } " ++ showSubIn]
  case (ty1, ty2) of
    (TInt, TInt) -> ret "SubInt" ctx []
    (TBool, TBool) -> ret "SubBool" ctx []
    (TVar a, TVar b) | a == b -> ret "SubReflTVar" ctx []
    (ETVar a, ETVar b) | a == b -> ret "SubReflETVar" ctx []
    (TArr ty1' ty2', TArr ty1'' ty2'') -> do
      (ctx1, drv1) <- sub ctx ty1'' ty1'
      (ctx2, drv2) <- sub ctx1 ty2' ty2''
      ret "SubArr" ctx2 [drv1, drv2]
    (_, TAll bnd) -> do
      -- it is always better to use SubAllR first
      (a, ty2') <- unbind bnd
      (ctx', drv) <- sub (TVarBnd a : ctx) ty1 ty2'
      let (_, _, ctx2) = break3 (\case TVarBnd a' -> a == a'; _ -> False) ctx'
      ret "SubAllR" ctx2 [drv]
    (TAll bnd, _) | not (isAll ty2) -> do
      -- of course ty2 is not forall
      (a, ty1') <- unbind bnd
      let ty1'' = subst a (ETVar a) ty1'
      (ctx', drv) <- sub (ETVarBnd a : Mark a : ctx) ty1'' ty2
      let (_, _, ctx2) = break3 (\case Mark a' -> a == a'; _ -> False) ctx'
      ret "SubAllL" ctx2 [drv]
    (ETVar a, _) | a `notElem` toListOf fv ty2 -> do
      (ctx', drv) <- instL ctx a ty2
      ret "SubInstL" ctx' [drv]
    (_, ETVar a) | a `notElem` toListOf fv ty1 -> do
      (ctx', drv) <- instR ctx ty1 a
      ret "SubInstR" ctx' [drv]
    _ -> throwError $ "\\text{No rule matching: } " ++ showSubIn
  where
    showSubIn = show ctx ++ " \\vdash " ++ show ty1 ++ " <: " ++ show ty2
    showSub ctx' = showSubIn ++ " \\dashv " ++ show ctx'

    ret :: String -> Ctx -> [Derivation] -> InferMonad (Ctx, Derivation)
    ret rule ctx' drvs = do
      lift $ tell ["\\text{Subtype[" ++ rule ++ "]: } " ++ showSub ctx']
      return (ctx', Derivation rule (showSub ctx') drvs)

check :: Ctx -> Trm -> Typ -> InferMonad (Ctx, Derivation)
check ctx tm ty = do
  lift $ tell ["\\text{Checking: } " ++ showCheckIn]
  case (tm, ty) of
    (LitInt _, TInt) -> ret "ChkLitInt" ctx []
    (LitBool _, TBool) -> ret "ChkLitBool" ctx []
    (Lam bnd, TArr ty1 ty2) -> do
      (x, tm') <- unbind bnd
      (ctx', drv) <- check (VarBnd x ty1 : ctx) tm' ty2
      let (_, _, ctx2) = break3 (\case VarBnd x' _ -> x == x'; _ -> False) ctx'
      ret "ChkLam" ctx2 [drv]
    (tm', TAll bnd) -> do
      (a, ty') <- unbind bnd
      (ctx', drv) <- check (TVarBnd a : ctx) tm' ty'
      let (_, _, ctx2) = break3 (\case TVarBnd a' -> a == a'; _ -> False) ctx'
      ret "ChkAll" ctx2 [drv]
    _ -> do
      (ty1, ctx', drv1) <- infer ctx tm
      ty1' <- appCtxTyp ctx' ty1
      ty' <- appCtxTyp ctx' ty
      (ctx'', drv2) <- sub ctx' ty1' ty'
      ret "ChkSub" ctx'' [drv1, drv2]
  where
    showCheckIn = show ctx ++ " \\vdash " ++ show tm ++ " \\Leftarrow " ++ show ty
    showCheck ctx' = showCheckIn ++ " \\dashv " ++ show ctx'

    ret :: String -> Ctx -> [Derivation] -> InferMonad (Ctx, Derivation)
    ret rule ctx' drvs = do
      lift $ tell ["\\text{Checked[" ++ rule ++ "]: } " ++ showCheck ctx']
      return (ctx', Derivation rule (showCheck ctx') drvs)

infer :: Ctx -> Trm -> InferMonad (Typ, Ctx, Derivation)
infer ctx tm = do
  lift $ tell ["\\text{Inferring: } " ++ showInferIn]
  case tm of
    Var x -> case find (\case VarBnd x' _ -> x == x'; _ -> False) ctx of
      Just (VarBnd _ ty) -> ret "InfVar" ty ctx []
      _ -> throwError $ "\\text{Variable } " ++ latexifyVar x ++ " \\text{ not found in context: } " ++ show ctx
    Ann tm' ty -> do
      (ctx', drv) <- check ctx tm' ty
      ret "InfAnn" ty ctx' [drv]
    LitInt _ -> ret "InfLitInt" TInt ctx []
    LitBool _ -> ret "InfLitBool" TBool ctx []
    Lam bnd -> do
      (x, tm') <- unbind bnd
      a <- freshTVar
      b <- freshTVar
      let ctx' = VarBnd x (ETVar a) : ETVarBnd b : ETVarBnd a : ctx
      (ctx'', drv) <- check ctx' tm' (ETVar b)
      let (_, _, ctx2) = break3 (\case VarBnd x' _ -> x == x'; _ -> False) ctx''
      ret "InfLam" (TArr (ETVar a) (ETVar b)) ctx2 [drv]
    App tm1 tm2 -> do
      (ty1, ctx', drv1) <- infer ctx tm1
      ty1' <- appCtxTyp ctx' ty1
      (ty2, ctx'', drv2) <- inferApp ctx' ty1' tm2
      ret "InfApp" ty2 ctx'' [drv1, drv2]
    _ -> throwError $ "\\text{No rule matching: } " ++ showInferIn
  where
    showInferIn = show ctx ++ " \\vdash " ++ show tm
    showInfer ty' ctx' = showInferIn ++ " \\Rightarrow " ++ show ty' ++ " \\dashv " ++ show ctx'

    ret :: String -> Typ -> Ctx -> [Derivation] -> InferMonad (Typ, Ctx, Derivation)
    ret rule ty ctx' drvs = do
      lift $ tell ["\\text{Inferred[" ++ rule ++ "]: } " ++ showInfer ty ctx']
      return (ty, ctx', Derivation rule (showInfer ty ctx') drvs)

inferApp :: Ctx -> Typ -> Trm -> InferMonad (Typ, Ctx, Derivation)
inferApp ctx ty tm = do
  lift $ tell ["\\text{InferApp: } " ++ showInferAppIn]
  case ty of
    TArr ty1 ty2 -> do
      (ctx', drv) <- check ctx tm ty1
      ret "InfAppArr" ty2 ctx' [drv]
    ETVar a -> do
      a1 <- freshTVar
      a2 <- freshTVar
      let (ctx1, _, ctx2) = break3 (\case ETVarBnd a' -> a == a'; _ -> False) ctx
          ctx' = ctx1 ++ SETVarBnd a (TArr (ETVar a1) (ETVar a2)) : ETVarBnd a1 : ETVarBnd a2 : ctx2
      (ctx'', drv) <- check ctx' tm (ETVar a1)
      ret "InfAppETVar" (ETVar a2) ctx'' [drv]
    TAll bnd -> do
      (a, ty') <- unbind bnd
      (ctx', drv) <- check ctx tm (subst a (ETVar a) ty')
      ret "InfAppAll" (subst a (ETVar a) ty') ctx' [drv]
    _ -> throwError $ "\\text{No rule matching: } " ++ showInferAppIn
  where
    showInferAppIn = show ctx ++ " \\vdash " ++ show tm ++ " \\bullet " ++ show ty
    showInferApp ty' ctx' = showInferAppIn ++ " \\Rrightarrow " ++ show ty' ++ " \\dashv " ++ show ctx'

    ret :: String -> Typ -> Ctx -> [Derivation] -> InferMonad (Typ, Ctx, Derivation)
    ret rule ty' ctx' drvs = do
      lift $ tell ["\\text{InferredApp[" ++ rule ++ "]: } " ++ showInferApp ty' ctx']
      return (ty', ctx', Derivation rule (showInferApp ty' ctx') drvs)

runDK :: Trm -> InferResult
runDK tm = case runInferMonad $ infer [] tm of
  Left [] -> InferResult False Nothing [] (Just "\\text{Unknown error}") True
  Left (err : drvs) -> InferResult False Nothing (map (\drv -> Derivation "Debug" drv []) drvs) (Just err) True
  Right ((ty, _, drv), _) -> InferResult True (Just $ show ty) [drv] Nothing False

dkMeta :: AlgMeta
dkMeta =
  AlgMeta
    { metaId = "DK",
      metaName = "Dunfield-Krishnaswami",
      metaLabels = ["Global", "Unification", "Bidirectional", "System F", "Higher-Rank", "Implicit"],
      metaViewMode = "tree",
      metaMode = "inference",
      metaPaper =
        Paper
          { paperTitle = "Complete and Easy Bidirectional Typechecking for Higher-rank Polymorphism",
            paperAuthors = ["Jana Dunfield", "Neelakantan R. Krishnaswami"],
            paperYear = 2013,
            paperUrl = "https://dl.acm.org/doi/10.1145/2500365.2500582"
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
