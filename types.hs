{-# OPTIONS_GHC -Wall #-}

module Types where

import Exp

import Data.List
import Data.Set (Set)
import qualified Data.Set as Set

type Name = String    -- name of a type variable

-- | Types (Monotypes)
data Type =
    TVar Name               -- type variable
  | TApp String [Type]      -- type application
    deriving (Eq, Ord)

instance Show Type where
  show (TVar a) = a
  show (TApp "Arrow" [t1, t2]) = show t1 ++ " -> " ++ show t2
  show (TApp d ts) = d ++ "(" ++ intercalate ", " (map show ts) ++ ")"


-- | Type schemes (Polytypes)
data Scheme =
    Mono Type               -- a monotype
  | Forall Name Scheme      -- quantified type
    deriving (Eq, Ord)

instance Show Scheme where
  show (Mono t) = show t
  show (Forall a s) = "∀" ++ a ++ ". " ++ show s

-- | Construct an arrow type from two others
arrow :: Type -> Type -> Type
t `arrow` t' = TApp "Arrow" [t, t']


-- | Given a type, find all free type variables
freeType :: Type -> Set Type
freeType (TVar a) = Set.singleton $ TVar a
freeType (TApp _ ts) = Set.unions $ map freeType ts

-- | Given a type scheme, find all free type varibles
freeScheme :: Scheme -> Set Type
freeScheme (Mono t) = freeType t
freeScheme (Forall a s) = Set.delete (TVar a) $ freeScheme s


-- | Typing context: associates an identifier with a type scheme
type Context = [(Exp.Id, Scheme)]

-- | Given a typing context, find all free variables it contains
free :: Context -> Set Type
free c = Set.unions $ map (\(_, s) -> freeScheme s) c

