{-# OPTIONS_GHC -Wall #-}

module Exp where

type Id = String    -- name of an identifier

-- | Exp language (restricted System F)
data Exp =
    Ident Id
  | Apply Exp Exp
  | Lambda String Exp
  | Let String Exp Exp

instance Show Exp where
  show (Ident v)    = v
  show (Apply e e') = show e ++ " " ++ show e'
  show (Lambda v e) = "λ" ++ v ++ " -> " ++ show e
  show (Let v e e') = "let " ++ v ++ " = " ++ show e ++ " in " ++ show e'

