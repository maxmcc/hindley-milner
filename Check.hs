{-# OPTIONS_GHC -Wall #-}

module Check where

import Exp
import Types

import Control.Monad


-- | Less specific / more general than relation (⊑)
inst :: Scheme -> Scheme -> Bool
inst = undefined

-- | Given a context and an expression, does it typecheck?
typecheck :: Context -> Exp -> Maybe Scheme

-- [Var] rule
typecheck c (Ident v) = lookup v c

-- [App] rule
typecheck c (Apply e0 e1) =
  do t0 <- typecheck c e0
     t1 <- typecheck c e1
     undefined

-- [Abs] rule
typecheck c (Lambda v e) =
  do let t = TVar "a" -- FIXME what do we say is v's type?
         c' = (v, Mono t) : c
     (Mono t') <- typecheck c' e
     return $ Mono $ t `arrow` t'
  -- it has to be a newly-generated and then constrained type variable

-- [Let] rule
typecheck c (Let v e0 e1) =
  do s <- typecheck c e0
     let c' = (v, s) : c
     typecheck c' e1


-- some tests!

identity :: (Scheme, Exp)
identity = (t, e)
  where t = Forall "a" $ Mono $ TVar "a" `arrow` TVar "a"
        e = Lambda "x" $ Ident "x"

tx :: Maybe Scheme
tx = typecheck c x where
  x = Ident "x"
  c = [("x", Mono $ TVar "int")]

letx :: Maybe Scheme
letx = typecheck c e where
  c = [("x", Mono $ TVar "int"), ("y", Mono $ TVar "bazgo")]
  e = Let "r" (Ident "x") (Ident "x")

let_ugh :: Maybe Scheme
let_ugh = typecheck c e where
  c = []
  e = Lambda "x" $ Ident "x"
