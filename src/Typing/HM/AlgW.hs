{-# LANGUAGE PatternSynonyms #-}

module Typing.HM.AlgW (runAlgW, algWMeta) where

import Control.Monad (foldM)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.Writer (MonadTrans (lift), MonadWriter (tell))
import Data.List (intercalate)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Lib (AlgMeta (..), Derivation (..), Example (..), InferMonad, InferResult (..), Paper (..), Rule (..), freshTVar, runInferMonad)
import Syntax (TmVar, Trm (..), TyVar, Typ (..), latexifyVar, pattern TAll)
import Unbound.Generics.LocallyNameless hiding (Subst)
import Unbound.Generics.LocallyNameless.Internal.Fold (toListOf)

type Env = Map.Map TmVar Typ

type Subst = Map.Map TyVar Typ

apply :: Subst -> Typ -> Typ
apply = substs . Map.toList

nullSubst :: Subst
nullSubst = Map.empty

compSubst :: Subst -> Subst -> Subst
compSubst s1 s2 = Map.map (apply s1) s2 `Map.union` s1

inst :: Typ -> InferMonad Typ
inst (TAll bnd) = unbind bnd >>= inst . snd
inst ty = return ty

gen :: Env -> Typ -> Typ
gen env ty = foldl (\ty' x -> TAll $ bind x ty') ty ftv
  where
    tFtv = Set.fromList $ toListOf fv ty
    envFtv = Set.fromList $ concatMap (toListOf fv) $ Map.elems env
    ftv = Set.toList $ tFtv `Set.difference` envFtv

mgu :: Typ -> Typ -> InferMonad (Subst, Derivation)
mgu ty1 ty2 = do
  lift $ tell ["\\text{Unifying: } " ++ showInput]
  case (ty1, ty2) of
    (TArr ty1' ty1'', TArr ty2' ty2'') -> do
      (s1, drv1) <- mgu ty1' ty2'
      (s2, drv2) <- mgu (apply s1 ty1'') (apply s1 ty2'')
      ret (s1 `compSubst` s2) [drv1, drv2]
    (TVar a, ty) -> varBind a ty >>= \s -> ret s []
    (ty, TVar a) -> varBind a ty >>= \s -> ret s []
    (TInt, TInt) -> ret nullSubst []
    (TBool, TBool) -> ret nullSubst []
    (TTuple tys, TTuple tys')
      | length tys == length tys' ->
          foldM
            ( \(s, drvs) (ty, ty') -> do
                (s', drv) <- mgu (apply s ty) (apply s ty')
                return (s' `compSubst` s, drvs ++ [drv])
            )
            (nullSubst, [])
            (zip tys tys')
            >>= uncurry ret
    _ -> throwError $ "\\text{cannot unify } " ++ show ty1 ++ " \\text{ with } " ++ show ty2
  where
    showInput :: String
    showInput = show ty1 ++ " \\sim " ++ show ty2

    showOutput :: Subst -> String
    showOutput s = showInput ++ " \\leadsto " ++ showSubst s

    ret :: Subst -> [Derivation] -> InferMonad (Subst, Derivation)
    ret s drvs = do
      lift $ tell ["\\text{Unified: } " ++ showOutput s]
      return (s, Derivation "Unify" (showOutput s) drvs)

varBind :: TyVar -> Typ -> InferMonad Subst
varBind a ty
  | aeq ty (TVar a) = return nullSubst
  | a `elem` toListOf fv ty = throwError $ latexifyVar a ++ " \\text{ occurs in } " ++ show ty
  | otherwise = return $ Map.singleton a ty

algW :: Env -> Trm -> InferMonad (Subst, Typ, Derivation)
algW env tm = do
  lift $ tell ["\\text{Inferring: } " ++ showInput]
  case tm of
    LitInt _ -> ret "LitInt" nullSubst TInt []
    LitBool _ -> ret "LitBool" nullSubst TBool []
    Var x -> case Map.lookup x env of
      Nothing -> throwError $ "\\text{unbound variable } " ++ latexifyVar x
      Just poly -> do
        mono <- inst poly
        ret "Var" nullSubst mono []
    Lam bnd -> do
      (x, tm') <- unbind bnd
      a <- freshTVar
      let env' = env `Map.union` Map.singleton x (TVar a)
      (s1, ty1, drv) <- algW env' tm'
      ret "Abs" s1 (TArr (apply s1 (TVar a)) ty1) [drv]
    App tm1 tm2 -> do
      a <- freshTVar
      (s1, ty1, drv1) <- algW env tm1
      (s2, ty2, drv2) <- algW (Map.map (apply s1) env) tm2
      (s3, drv3) <- mgu (apply s2 ty1) (TArr ty2 (TVar a))
      ret "App" (s3 `compSubst` s2 `compSubst` s1) (apply s3 (TVar a)) [drv1, drv2, drv3]
    Let tm1 bnd -> do
      (x, tm2) <- unbind bnd
      (s1, ty1, drv1) <- algW env tm1
      let ty' = gen (Map.map (apply s1) env) ty1
          env' = Map.insert x ty' env
      (s2, ty2, drv2) <- algW (Map.map (apply s1) env') tm2
      ret "Let" (s2 `compSubst` s1) ty2 [drv1, drv2]
    Tuple tms -> do
      (s, tys, drvs) <-
        foldM
          ( \(s', tys', drvs') tm' -> do
              (s'', ty', drv') <- algW (Map.map (apply s') env) tm'
              return (s'' `compSubst` s', tys' ++ [ty'], drvs' ++ [drv'])
          )
          (nullSubst, [], [])
          tms
      ret "Tuple" s (TTuple tys) drvs
    _ -> throwError $ "\\text{No rule matching } " ++ show tm
  where
    showInput :: String
    showInput = showEnv env ++ " \\vdash " ++ show tm

    showOutput :: Subst -> Typ -> String
    showOutput s ty = showInput ++ " : " ++ show ty ++ ", " ++ showSubst s
    ret :: String -> Subst -> Typ -> [Derivation] -> InferMonad (Subst, Typ, Derivation)
    ret rule s ty drvs = do
      lift $ tell ["\\text{Inferred: } " ++ showOutput s ty]
      return (s, ty, Derivation rule (showOutput s ty) drvs)

runAlgW :: Trm -> InferResult
runAlgW tm = case runInferMonad $ algW Map.empty tm of
  Left [] -> InferResult False Nothing [] (Just "\\text{Unknown error}") True
  Left (err : drvs) -> InferResult False Nothing (map (\drv -> Derivation "Debug" drv []) drvs) (Just err) True
  Right ((_, ty, drv), _) -> InferResult True (Just $ show ty) [drv] Nothing False

showEnv :: Env -> String
showEnv env = intercalate ", " $ map (\(x, ty) -> latexifyVar x ++ ": " ++ show ty) (Map.toList env)

showSubst :: Subst -> String
showSubst s
  | Map.null s = "\\emptyset"
  | otherwise = "\\{" ++ intercalate ", " (map (\(a, ty) -> show ty ++ " / " ++ latexifyVar a) (Map.toList s)) ++ "\\}"

algWMeta :: AlgMeta
algWMeta =
  AlgMeta
    { metaId = "W",
      metaName = "Algorithm W",
      metaLabels = ["Global", "Unification", "Hindley-Milner"],
      metaViewMode = "tree",
      metaMode = "inference",
      metaPaper =
        Paper
          { paperTitle = "A Theory of Type Polymorphism in Programming",
            paperAuthors = ["Robin Milner"],
            paperYear = 1978,
            paperUrl = "https://doi.org/10.1016%2F0022-0000%2878%2990014-4"
          },
      metaVariants = Nothing,
      metaDefaultVariant = Nothing,
      metaRules =
        [ Rule
            { metaRuleId = "Var",
              metaRuleName = "Var",
              metaRulePremises = ["x : \\sigma \\in \\Gamma", "\\tau = inst(\\sigma)"],
              metaRuleConclusion = "\\Gamma \\vdash x : \\sigma, \\emptyset",
              metaRuleDescription = Nothing,
              metaRuleReduction = Nothing
            },
          Rule
            { metaRuleId = "App",
              metaRuleName = "App",
              metaRulePremises = ["\\Gamma \\vdash e_0 : \\tau_0, S_0", "S_0\\Gamma \\vdash e_1 : \\tau_1, S_1", "\\tau' = newvar", "S_2 = mgu(S_1\\tau_0, \\tau_1 \\rightarrow \\tau')"],
              metaRuleConclusion = "\\Gamma \\vdash e_0 ~ e_1 : S_2\\tau', S_2S_1S_0",
              metaRuleDescription = Nothing,
              metaRuleReduction = Nothing
            },
          Rule
            { metaRuleId = "Abs",
              metaRuleName = "Abs",
              metaRulePremises = ["r = newvar", "\\Gamma, x : \\tau \\vdash e : \\tau', S"],
              metaRuleConclusion = "\\Gamma \\vdash \\lambda x.~e : S\\tau \\rightarrow \\tau', S",
              metaRuleDescription = Nothing,
              metaRuleReduction = Nothing
            },
          Rule
            { metaRuleId = "Let",
              metaRuleName = "Let",
              metaRulePremises = ["\\Gamma \\vdash e_0 : \\tau, S_0", "S_0\\Gamma, x : \\overline{S_0\\Gamma}(\\tau) \\vdash e_1 : \\tau', S_1"],
              metaRuleConclusion = "\\Gamma \\vdash \\text{let } x = e_0 \\text{ in } e_1 : \\tau', S_1 S_0",
              metaRuleDescription = Nothing,
              metaRuleReduction = Nothing
            }
        ],
      metaRuleGroups = Nothing,
      metaVariantRules = Nothing,
      metaExamples =
        [ Example
            { exampleName = "Trivial Application",
              exampleExpression = "(\\x. x) 1",
              exampleDescription = "Trivial function application of identity function to integer literal"
            },
          Example
            { exampleName = "Identity",
              exampleExpression = "\\x. x",
              exampleDescription = "The identity function"
            },
          Example
            { exampleName = "Let-Polymorphism",
              exampleExpression = "let id = (\\x. x) in (id 1, id True)",
              exampleDescription = "id can be instantiated with different types"
            }
        ]
    }
