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
  show (TArrow t t') = "(" ++ show t ++ " -> " ++ show t' ++ ")"


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
  -- | Return a set of all type variables in a type structure (free and bound)
  allVars :: a -> Set Name
  -- | Return a set of all free type variables in a type structure
  freeVars :: a -> Set Name
  -- | Apply a substitution to a type structure
  subst :: Subst -> a -> a


instance TypeVars Type where
  allVars (TVar a)      = Set.singleton a
  allVars (TArrow t t') = allVars t `Set.union` allVars t'
  allVars _             = Set.empty

  freeVars = allVars

  subst s v@(TVar a)    = fromMaybe v $ Map.lookup a s
  subst s (TArrow t t') = subst s t `TArrow` subst s t'
  subst _ t             = t


instance TypeVars Scheme where
  allVars (Mono t)      = allVars t
  allVars (Forall a t)  = Set.insert a $ allVars t

  freeVars (Mono t)     = freeVars t
  freeVars (Forall a t) = Set.delete a $ freeVars t

  subst s (Mono t)     = Mono $ subst s t
  subst s (Forall a t) = Forall a $ subst (Map.delete a s) t


-- | Typing context associating an identifier with a type scheme
type Context = Map Exp.Id Scheme

instance TypeVars Context where
  allVars  = Map.foldl (\av t -> Set.union av $ allVars t) Set.empty
  freeVars = Map.foldl (\fv t -> Set.union fv $ freeVars t) Set.empty
  subst s  = Map.map (subst s)

-- | Remove a variable from the context
remove :: Context -> Name -> Context
remove = flip Map.delete
