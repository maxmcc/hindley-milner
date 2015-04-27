{-# OPTIONS_GHC -Wall #-}

module Check where

import Exp
import Types

import Data.IORef

-- | Generate a fresh type variable
fresh :: IO (IO String)
fresh = undefined


-- | Quantify type variables free in t but not c
generalize :: Context -> Type -> Scheme
generalize = undefined


-- | Replace quantified type variables by fresh ones
instantiate :: Scheme -> Scheme
instantiate = undefined


-- | Find most general unifier of two types
unify :: Type -> Type -> Subst
unify = undefined


-- | Implementation of Algorithm W for finding principal type
inferTypes :: Context -> Exp -> (Subst, Type)
inferTypes = undefined
