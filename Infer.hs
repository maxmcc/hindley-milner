{-# OPTIONS_GHC -Wall #-}

module Infer where

import Exp
import Types

import Data.Maybe
import Data.IORef
import Control.Monad
import qualified Data.Set as Set
import qualified Data.Map as Map

-- | Quantify type variables free in t but not c
generalize :: Context -> Type -> Scheme
generalize c t = Set.fold Forall (Mono t) vars
  where vars = freeVars t `Set.difference` freeVars c


-- | Generate a fresh type variable identifier
makeNewVar :: IO (IO String)
makeNewVar =
  do r <- newIORef 'a'
     return $ do
       v <- readIORef r
       writeIORef r $ succ v
       return [v]


-- | Replace quantified type variables by fresh ones
inst :: IO String -> Scheme -> IO Type
inst newVar t = ms >>= \s -> return $ replaceFree s t
  where ms = foldM update Map.empty $ Set.toList boundVars
        boundVars = allVars t `Set.difference` freeVars t
        replaceFree s (Mono t')     = subst s t'
        replaceFree s (Forall _ t') = replaceFree s t'
        update acc a =
          do n <- newVar
             return $ Map.insert a (TVar n) acc


-- | Find most general unifier of two types
unify :: Type -> Type -> Subst
unify TInt TInt                   = empty
unify TBool TBool                 = empty
unify (TVar a) t                  = bindVar a t
unify t (TVar a)                  = bindVar a t
unify (TArrow s s') (TArrow t t') = compose s1 s2
  where s1 = unify s t
        s2 = subst s1 s' `unify` subst s1 t'
unify _ _                         = error "structural mismatch: can't unify"

-- | Bind a type variable to a type
bindVar :: String -> Type -> Subst
bindVar a t | t == TVar a               = empty
            | a `Set.member` freeVars t = error "occurs check: can't unify"
            | otherwise                 = Map.singleton a t


-- | Implementation of Algorithm W for finding principal type
inferTypes :: IO String -> Context -> Exp -> IO (Subst, Type)

inferTypes newVar c (Ident i) = inst newVar t >>= \t' -> return (empty, t')
  where t = fromMaybe (error "unknown identifier") $ Map.lookup i c

inferTypes newVar c (Lambda v e) =
  do n <- newVar
     let t' = TVar n
         c' = Map.insert v (Mono t') $ remove c v
     (s1, t1) <- inferTypes newVar c' e
     return (s1, subst s1 t' `TArrow` t1)

inferTypes newVar c (Apply e e') =
  do (s1, t1) <- inferTypes newVar c e
     (s2, t2) <- inferTypes newVar (subst s1 c) e'
     n <- newVar
     let t' = TVar n
     let s3 = unify (subst s2 t1) (TArrow t2 t')
     return (compose s3 $ compose s2 s1, subst s3 t')

inferTypes newVar c (Let v e e') =
  do (s1, t1) <- inferTypes newVar c e
     let c' = Map.insert v t' $ subst s1 $ remove c v
         t' = generalize (subst s1 c) t1
     (s2, t2) <- inferTypes newVar c' e'
     return (compose s2 s1, t2)

