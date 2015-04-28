{-# OPTIONS_GHC -Wall #-}

module Check where

import Exp
import Types

import Data.Char
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Unique
import System.IO.Unsafe

-- | Quantify type variables free in t but not c
generalize :: Context -> Type -> Scheme
generalize c t = Set.fold Forall (Mono t) vars
  where vars = freeVars t `Set.difference` freeVars c


-- | Generate a fresh type variable identifier
newVar :: IO String
newVar =
  do offset <- newUnique
     return ['_', chr $ 96 + hashUnique offset] -- start with 'a'

-- | Replace quantified type variables by fresh ones
instantiate :: Scheme -> Type
instantiate t = replaceFree s t
  where s = Set.fold newsym Map.empty vars
        vars = allVars t `Set.difference` freeVars t  -- bound vars
        newsym old updates =
          let new = unsafePerformIO newVar in
          Map.insert old (TVar new) updates
        replaceFree s' (Mono t') = subst s' t'
        replaceFree s' (Forall _ t') = replaceFree s' t'


-- | Find most general unifier of two types
unify :: Type -> Type -> Subst
unify TInt TInt   = empty
unify TBool TBool = empty
unify (TVar a) t = bindVar a t
unify t (TVar a) = bindVar a t
unify (TArrow s s') (TArrow t t') = compose lhs rhs
    where lhs = unify s t
          rhs = unify (subst lhs s') (subst lhs t')
unify _ _ = error "structural mismatch: can't unify"

-- | Bind a type variable to a type
bindVar :: String -> Type -> Subst
bindVar a t
  | t == TVar a               = empty
  | Set.member a $ freeVars t = error "failed occurs check: can't unify"
  | otherwise                 = Map.singleton a t


-- | Implementation of Algorithm W for finding principal type
inferTypes :: Context -> Exp -> (Subst, Type)

inferTypes c (Ident i) = (empty, instantiate t)
  where t = fromMaybe (error "unknown identifier") $ Map.lookup i c

inferTypes c (Lambda v e) = (s1, TArrow (subst s1 t') t1)
  where (s1, t1) = inferTypes c' e
        c' = Map.insert v (Mono t') $ remove c v
        t' = TVar $ unsafePerformIO newVar

inferTypes c (Apply e e') = (compose s3 $ compose s2 s1, subst s3 t')
  where (s1, t1) = inferTypes c e
        (s2, t2) = inferTypes (subst s1 c) e'
        s3 = unify (subst s2 t1) (TArrow t2 t')
        t' = TVar $ unsafePerformIO newVar

inferTypes c (Let v e e') = (compose s2 s1, t2)
  where (s1, t1) = inferTypes c e
        (s2, t2) = inferTypes c' e'
        c' = Map.insert v xt $ subst s1 $ remove c v
        xt = generalize (subst s1 c) t1

-- | A default context, for testing
ctx :: Context
ctx = Map.fromList [
      ("i", Mono TInt)
    , ("id", Forall "a" $ Mono $ TArrow (TVar "a") (TVar "a"))
    , ("b", Mono TBool)
    , ("add", Mono $ TArrow TInt TInt)
    , ("eq", Forall "a" $ Mono $ TArrow (TVar "a") (TArrow (TVar "a") TBool))
  ]
