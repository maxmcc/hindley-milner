{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Types where

import qualified Exp

import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map


type Name = String    -- name of a type variable

-- | Types (Monotypes)
data Type =
    TInt
  | TBool
  | TVar Name               -- type variable
  | TArrow Type Type        -- function type
    deriving (Eq, Ord)

instance Show Type where
  show TInt          = "Int"
  show TBool         = "Bool"
  show (TVar a)      = a
  show (TArrow t t') = show t ++ " -> " ++ show t'

-- | Construct an arrow type from two others
arrow :: Type -> Type -> Type
t `arrow` t' = TArrow t t'


-- | Type schemes (Polytypes)
data Scheme =
    Mono Type               -- a monotype
  | Forall Name Scheme      -- quantified type
    deriving (Eq, Ord)

instance Show Scheme where
  show (Mono t)     = show t
  show (Forall a t) = "∀" ++ a ++ ". " ++ show t


-- | A substitution of types for type variables
type Subst = Map Name Type

-- | The empty (Id) substitution
empty :: Subst
empty = Map.empty

-- | Compose two substitutions
compose :: Subst -> Subst -> Subst
compose s s' = Map.map (subst s) s' `Map.union` s


-- | Typeclass for structures containing polymorphic types
class TypeVars a where
  -- | Return a set of all free variables in a type structure
  free :: a -> Set Name
  -- | Apply a substitution to a type structure
  subst :: Subst -> a -> a


instance TypeVars Type where
  free (TVar a)     = Set.singleton a
  free (TArrow s t) = free s `Set.union` free t
  free _            = Set.empty

  subst s v@(TVar a)    = fromMaybe v $ Map.lookup a s
  subst s (TArrow t t') = subst s t `arrow` subst s t'
  subst _ t             = t


instance TypeVars Scheme where
  free (Mono t)     = free t
  free (Forall a t) = Set.delete a $ free t

  subst s (Mono t)     = Mono $ subst s t
  subst s (Forall a t) = Forall a $ subst (Map.delete a s) t


-- | Typing context associating an identifier with a type scheme
type Context = Map Exp.Id Scheme

instance TypeVars Context where
  free = Map.foldl (\fv t -> Set.union fv $ free t) Set.empty
  subst s = Map.map (subst s)
    -- TODO: investigate occur check? [BN98]

-- | Remove a variable from the context
remove :: Context -> Name -> Context
remove = flip Map.delete
