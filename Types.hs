{-# OPTIONS_GHC -Wall #-}

module Types where

import Exp

import Data.Set (Set)
import qualified Data.Set as Set

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
s `arrow` t = TArrow s t


-- | Type schemes (Polytypes)
data Scheme =
    Mono Type               -- a monotype
  | Forall Name Scheme      -- quantified type
    deriving (Eq, Ord)

instance Show Scheme where
  show (Mono t)     = show t
  show (Forall a s) = "∀" ++ a ++ ". " ++ show s


-- | Given a type, find all free type variables
freeType :: Type -> Set Type
freeType (TVar a)     = Set.singleton $ TVar a
freeType (TArrow s t) = Set.union (freeType s) (freeType t)
freeType _            = Set.empty

-- | Given a type scheme, find all free type varibles
freeScheme :: Scheme -> Set Type
freeScheme (Mono t)     = freeType t
freeScheme (Forall a s) = Set.delete (TVar a) $ freeScheme s


-- | Typing context: associates an identifier with a type scheme
type Context = [(Exp.Id, Scheme)]

-- | Given a typing context, find all free type variables
free :: Context -> Set Type
free c = Set.unions $ map (\(_, s) -> freeScheme s) c

