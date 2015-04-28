{-# OPTIONS_GHC -Wall #-}

module Check where

import Exp
import Types

import Data.Char
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Unique
import System.IO.Unsafe

-- | Quantify type variables free in t but not c
generalize :: Context -> Type -> Scheme
generalize c t = Set.fold Forall (Mono t) vars
  where vars = freeVars t `Set.difference` freeVars c


-- |
newVar :: IO String
newVar =
  do offset <- newUnique
     return [chr $ 96 + hashUnique offset] -- start with 'a'

-- | Replace quantified type variables by fresh ones
instantiate :: Scheme -> Scheme
instantiate t = replaceFree s t
  where s = Set.fold newsym Map.empty vars
        vars = allVars t `Set.difference` freeVars t
        newsym old updates =
          let new = unsafePerformIO newVar in
          Map.insert old (TVar new) updates
        replaceFree s' (Mono t') = Mono $ subst s' t'
        replaceFree s' (Forall a t') =
          let newA = case Map.lookup a s' of
                       Nothing -> a
                       Just (TVar t'') -> t''
                       Just _ -> error "not valid substitution"
          in
          Forall newA $ replaceFree s' t'


-- | Find most general unifier of two types
unify :: Type -> Type -> Subst
unify = undefined


-- | Implementation of Algorithm W for finding principal type
inferTypes :: Context -> Exp -> (Subst, Type)
inferTypes = undefined
