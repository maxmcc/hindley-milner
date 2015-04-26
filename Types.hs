{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Types where

import qualified Exp

import Data.Functor
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
  show TInt         = "Int"
  show TBool        = "Bool"
  show (TVar a)     = a
  show (TArrow s t) = show s ++ " -> " ++ show t

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


type Subst = Maybe (Map Name Type)

empty :: Subst
empty = Just Map.empty

errorSubst :: Subst
errorSubst = Nothing

class TypeVariables a where
  free  :: TypeVariables a => a -> Set Name
  subst :: TypeVariables a => Subst -> a -> Maybe a

instance TypeVariables Type where
  free (TVar a)     = Set.singleton a
  free (TArrow s t) = Set.union (free s) (free t)
  free _            = Set.empty

  subst ms v@(TVar a) =
    do s <- ms
       return $ case Map.lookup a s of
         Just a  -> a
         Nothing -> v

  subst ms (TArrow t t') =
    do subT  <- subst ms t
       subT' <- subst ms t'
       return $ TArrow subT subT'

  subst ms t = ms >>= (\_ -> return t)

instance TypeVariables Scheme where
  free (Mono t)     = free t
  free (Forall a t) = Set.delete a $ free t

  subst ms (Mono t) =
    do subT <- subst ms t
       return $ Mono subT

  subst ms (Forall a t) =
    do let ms' = Map.delete a <$> ms
       subT <- subst ms' t
       return $ Forall a subT


-- | Typing context: associates an identifier with a type scheme
type Context = Map Exp.Id Scheme

instance TypeVariables Context where
  free = Map.foldl (\fv t -> Set.union fv $ free t) Set.empty
  subst ms c = undefined -- Map.map (subst ms) c

    -- need to substitute all the values in this context (map) with
    -- all the values in the other context map.
       -- other context map is a Maybe
    -- TODO: investigate occur check? [BN98]
