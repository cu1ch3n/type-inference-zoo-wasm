{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

module Typing.HM.AlgR (runAlgR, algRMeta) where

import Control.Monad.Except (MonadError (throwError))
import Control.Monad.Writer (MonadTrans (lift), MonadWriter (tell))
import Data.Bifunctor (bimap)
import Data.Foldable (find)
import Data.List (intercalate)
import Lib (AlgMeta (..), Derivation (..), Example (..), InferMonad, InferResult (..), Paper (..), freshTVar, runInferMonad)
import Syntax (TmVar, Trm (..), TyVar, Typ (..), latexifyVar, wrapVar, pattern TAll)
import Unbound.Generics.LocallyNameless

type ExCtx = [TyVar]

data TyCtxEntry = TVarBnd TyVar | ExCtx ExCtx | VarBnd TmVar Typ | Invis ExCtx Typ

type TyCtx = [TyCtxEntry]

type TyEqs = [(Typ, Typ)]

showExCtx :: ExCtx -> String
showExCtx = intercalate ", " . map (\a -> wrapVar "hat" a) . reverse

showExCtxTyp :: ExCtx -> Typ -> String
showExCtxTyp [] ty = "[\\bullet]" ++ show ty
showExCtxTyp exCtx ty = "[" ++ showExCtx exCtx ++ "]" ++ show ty

showTyEqs :: TyEqs -> String
showTyEqs = intercalate ", " . map (\(ty1, ty2) -> show ty1 ++ " \\sim " ++ show ty2)

instance Show TyCtxEntry where
  show (TVarBnd a) = latexifyVar a
  show (ExCtx exCtx) = "(" ++ showExCtx exCtx ++ ")"
  show (VarBnd x ty) = latexifyVar x ++ " : " ++ show ty
  show (Invis exCtx ty) = "\\{" ++ showExCtxTyp exCtx ty ++ "\\}"

instance {-# OVERLAPPING #-} Show TyCtx where
  show = intercalate "; " . map show . reverse

mono :: Typ -> Bool
mono (TVar _) = True
mono (ETVar _) = True
mono TInt = True
mono TBool = True
mono (TArr ty1 ty2) = mono ty1 && mono ty2
mono ty = error $ "\\text{mono: not implemented for } " ++ show ty

inst :: TyCtx -> Typ -> InferMonad (ExCtx, Typ, Derivation)
inst tyCtx ty = do
  lift $ tell ["\\text{Instantiating: } " ++ showInput]
  case ty of
    (TAll bnd) -> do
      (a, ty') <- unbind bnd
      (exCtx, ty'', drv) <- inst (ExCtx [a] : tyCtx) (subst a (ETVar a) ty')
      ret "InstPoly" (a : exCtx) ty'' [drv]
    _ | mono ty -> ret "InstMono" [] ty []
    _ -> throwError $ "\\text{No rule matching: } " ++ showInput
  where
    showInput = show tyCtx ++ " \\vdash " ++ show ty
    showOutput exCtx' ty' = showInput ++ " \\ge " ++ showExCtxTyp exCtx' ty'

    ret :: String -> ExCtx -> Typ -> [Derivation] -> InferMonad (ExCtx, Typ, Derivation)
    ret rule exCtx' ty' drvs = do
      lift $ tell ["\\text{Instantiated[" ++ rule ++ "]: } " ++ showOutput exCtx' ty']
      return (exCtx', ty', Derivation rule (showOutput exCtx' ty') drvs)

gen :: TyCtx -> Trm -> InferMonad (Typ, TyCtx, Derivation)
gen tyCtx tm = do
  lift $ tell ["\\text{Generalizing: } " ++ showInput]
  (exCtx, ty, tyCtx', drv) <- infer tyCtx tm
  let ty' = foldl (\ty'' a -> TAll $ bind a (subst a (TVar a) ty'')) ty exCtx
  lift $ tell ["\\text{Generalized: } " ++ showOutput ty' tyCtx']
  return (ty', tyCtx', Derivation "Gen" (showOutput ty' tyCtx') [drv])
  where
    showInput = show tyCtx ++ " \\vdash " ++ show tm
    showOutput ty' tyCtx' = showInput ++ " : " ++ show ty' ++ " \\dashv " ++ show tyCtx'

infer :: TyCtx -> Trm -> InferMonad (ExCtx, Typ, TyCtx, Derivation)
-- Note here the return Typ is actually mono
infer tyCtx tm = do
  lift $ tell ["\\text{Inferring: } " ++ showInput]
  case tm of
    Var x | Just (VarBnd _ ty) <- find (\case VarBnd x' _ -> x == x'; _ -> False) tyCtx -> do
      (exCtx, ty', drv) <- inst tyCtx ty
      ret "Var" exCtx ty' tyCtx [drv]
    LitInt _ -> ret "Int" [] TInt tyCtx []
    LitBool _ -> ret "Bool" [] TBool tyCtx []
    Lam bnd -> do
      (x, e) <- unbind bnd
      a <- freshTVar
      (exCtx2, ty2, tyCtx', drv) <- infer (VarBnd x (ETVar a) : ExCtx [a] : tyCtx) e
      case tyCtx' of
        VarBnd x' ty1 : ExCtx exCtx1 : tyCtx''
          | x == x' ->
              ret "Abs" (exCtx2 ++ exCtx1) (TArr ty1 ty2) tyCtx'' [drv]
        _ -> throwError $ show tyCtx' ++ " \\text{ is not of the right form}"
    App tm1 tm2 -> do
      (exCtx1, ty, tyCtx1, drv1) <- infer tyCtx tm1
      (exCtx2, ty1, tyCtx2, drv2) <- infer (Invis exCtx1 ty : tyCtx1) tm2
      case tyCtx2 of
        Invis exCtx1' ty' : tyCtx2' -> do
          a <- freshTVar
          (tyCtx', drv3) <-
            unify
              (Invis [] (ETVar a) : ExCtx (a : exCtx2 ++ exCtx1') : tyCtx2')
              [(ty', TArr ty1 (ETVar a))]
          case tyCtx' of
            Invis [] ty'' : ExCtx exCtx : tyCtx'' ->
              ret "App" exCtx ty'' tyCtx'' [drv1, drv2, drv3]
            _ -> throwError $ show tyCtx' ++ " \\text{ is not of the right form}"
        _ -> throwError $ show tyCtx2 ++ " \\text{ is not of the right form}"
    Let tm1 bnd -> do
      (x, tm2) <- unbind bnd
      (ty, tyCtx', drv1) <- gen tyCtx tm1
      (exCtx, ty', tyCtx'', drv2) <- infer (VarBnd x ty : tyCtx') tm2
      case tyCtx'' of
        VarBnd x' _ : tyCtx''' | x == x' -> do
          ret "Let" exCtx ty' tyCtx''' [drv1, drv2]
        _ -> throwError $ show tyCtx'' ++ " \\text{ is not of the right form}"
    _ -> throwError $ "\\text{No rule matching: } " ++ showInput
  where
    showInput = show tyCtx ++ " \\vdash " ++ show tm
    showOutput exCtx ty tyCtx' = showInput ++ " : " ++ showExCtxTyp exCtx ty ++ " \\dashv " ++ show tyCtx'

    ret :: String -> ExCtx -> Typ -> TyCtx -> [Derivation] -> InferMonad (ExCtx, Typ, TyCtx, Derivation)
    ret rule exCtx ty tyCtx' drvs = do
      lift $ tell ["\\text{Inferred[" ++ rule ++ "]: } " ++ showOutput exCtx ty tyCtx']
      return (exCtx, ty, tyCtx', Derivation rule (showOutput exCtx ty tyCtx') drvs)

unify :: TyCtx -> TyEqs -> InferMonad (TyCtx, Derivation)
unify tyCtx tyEqs = do
  lift $ tell ["\\text{Unifying: } " ++ showInput]
  case tyEqs of
    [] -> ret "SolNil" tyCtx []
    _ -> do
      (tyCtx', tyEqs'') <- unifySingleStep tyCtx tyEqs
      (tyCtx'', drv) <- unify tyCtx' tyEqs''
      ret "SolCons" tyCtx'' [drv]
  where
    showInput = show tyCtx ++ " \\vdash " ++ showTyEqs tyEqs
    showOutput tyCtx' = showInput ++ " \\dashv " ++ show tyCtx'

    ret :: String -> TyCtx -> [Derivation] -> InferMonad (TyCtx, Derivation)
    ret rule tyCtx' drvs = do
      lift $ tell ["\\text{Unified[" ++ rule ++ "]: } " ++ showOutput tyCtx']
      return (tyCtx', Derivation rule (showOutput tyCtx') drvs)

substExCtx :: TyVar -> [TyVar] -> ExCtx -> Maybe ExCtx
substExCtx _ _ [] = Nothing
substExCtx a exVars (a' : exCtx)
  | a == a' = Just $ exVars ++ exCtx
  | otherwise = do
      exCtx' <- substExCtx a exVars exCtx
      return $ a' : exCtx'

substTyCtx :: TyVar -> Typ -> [TyVar] -> TyCtx -> InferMonad TyCtx
substTyCtx a ty exVars tyCtx = case tyCtx of
  [] -> throwError $ latexifyVar a ++ " \\text{ is not found}"
  TVarBnd a' : tyCtx' -> do
    tyCtx'' <- substTyCtx a ty exVars tyCtx'
    return $ TVarBnd a' : tyCtx''
  ExCtx exCtx : tyCtx' ->
    case substExCtx a exVars exCtx of
      Just exCtx' ->
        return $ ExCtx exCtx' : tyCtx'
      Nothing -> do
        tyCtx'' <- substTyCtx a ty exVars tyCtx'
        return $ ExCtx exCtx : tyCtx''
  VarBnd x ty' : tyCtx' -> do
    tyCtx'' <- substTyCtx a ty exVars tyCtx'
    return $ VarBnd x (subst a ty ty') : tyCtx''
  Invis exCtx ty' : tyCtx' -> do
    case substExCtx a exVars exCtx of
      Just exCtx' ->
        return $ Invis exCtx' (subst a ty ty') : tyCtx'
      Nothing -> do
        tyCtx'' <- substTyCtx a ty exVars tyCtx'
        return $ Invis exCtx (subst a ty ty') : tyCtx''

substTyEqs :: TyVar -> Typ -> TyEqs -> TyEqs
substTyEqs a ty = map (bimap (subst a ty) (subst a ty))

before :: TyCtx -> TyVar -> TyVar -> Bool
before tyCtx a b =
  let (tyCtx1, _) = break (\case TVarBnd a' -> a == a'; _ -> False) tyCtx
      (tyCtx1', _) = break (\case TVarBnd b' -> b == b'; _ -> False) tyCtx
   in length tyCtx1 > length tyCtx1'

unifySingleStep :: TyCtx -> TyEqs -> InferMonad (TyCtx, TyEqs)
unifySingleStep tyCtx tyEqs = case tyEqs of
  (ty1, ty2) : tyEqs' -> do
    lift $ tell ["\\text{UnifyingSingleStep: } " ++ showInput]
    case (ty1, ty2) of
      (TInt, TInt) -> return (tyCtx, tyEqs')
      (TBool, TBool) -> return (tyCtx, tyEqs')
      (ETVar a, ETVar b) | a == b -> return (tyCtx, tyEqs')
      (TArr ty1' ty2', TArr ty1'' ty2'') -> return (tyCtx, (ty1', ty1'') : (ty2', ty2'') : tyEqs')
      (ETVar a, TArr _ _) -> do
        a1 <- freshTVar
        a2 <- freshTVar
        tyCtx' <- substTyCtx a (TArr (ETVar a1) (ETVar a2)) [a1, a2] tyCtx
        return (tyCtx', substTyEqs a (TArr (ETVar a1) (ETVar a2)) tyEqs')
      (TArr _ _, ETVar a) -> do
        a1 <- freshTVar
        a2 <- freshTVar
        tyCtx' <- substTyCtx a (TArr (ETVar a1) (ETVar a2)) [a1, a2] tyCtx
        return (tyCtx', substTyEqs a (TArr (ETVar a1) (ETVar a2)) tyEqs')
      (ETVar a, ETVar b) | before tyCtx a b -> do
        tyCtx' <- substTyCtx b (ETVar a) [] tyCtx
        return (tyCtx', substTyEqs b (ETVar a) tyEqs')
      (ETVar b, ETVar a) | before tyCtx a b -> do
        tyCtx' <- substTyCtx b (ETVar a) [] tyCtx
        return (tyCtx', substTyEqs b (ETVar a) tyEqs')
      (ETVar a, TInt) -> do
        tyCtx' <- substTyCtx a TInt [] tyCtx
        return (tyCtx', substTyEqs a TInt tyEqs')
      (TInt, ETVar a) -> do
        tyCtx' <- substTyCtx a TInt [] tyCtx
        return (tyCtx', substTyEqs a TInt tyEqs')
      (ETVar a, TBool) -> do
        tyCtx' <- substTyCtx a TBool [] tyCtx
        return (tyCtx', substTyEqs a TBool tyEqs')
      (TBool, ETVar a) -> do
        tyCtx' <- substTyCtx a TBool [] tyCtx
        return (tyCtx', substTyEqs a TBool tyEqs')
      _ -> throwError $ "\\text{No rule matching: } " ++ showInput
  [] -> throwError "\\text{Impossible}"
  where
    showInput = show tyCtx ++ " \\vdash " ++ showTyEqs tyEqs

runAlgR :: Trm -> InferResult
runAlgR tm = case runInferMonad $ infer [] tm of
  Left [] -> InferResult False Nothing [] (Just "\\text{Unknown error}") True
  Left (err : drvs) -> InferResult False Nothing (map (\drv -> Derivation "Debug" drv []) drvs) (Just err) True
  Right ((_, ty, _, drv), _) -> InferResult True (Just $ show ty) [drv] Nothing False

algRMeta :: AlgMeta
algRMeta =
  AlgMeta
    { metaId = "R",
      metaName = "Algorithm R",
      metaLabels = ["Global", "Unification", "Hindley-Milner"],
      metaViewMode = "tree",
      metaMode = "inference",
      metaPaper =
        Paper
          { paperTitle = "No Unification Variable Left Behind: Fully Grounding Type Inference for the HDM System",
            paperAuthors = ["Roger Bosman", "Georgios Karachalias", "Tom Schrijvers"],
            paperYear = 2023,
            paperUrl = "https://drops.dagstuhl.de/entities/document/10.4230/LIPIcs.ITP.2023.8"
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
