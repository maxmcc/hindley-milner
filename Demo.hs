{-# OPTIONS_GHC -Wall #-}

module Demo where

import Exp
import Types
import Infer

import qualified Data.Map as Map

-- | A default context
stdLib :: Context
stdLib = Map.fromList [
      ("zero", Mono TInt)
    , ("one", Mono TInt)
    , ("x", Mono TInt)
    , ("true", Mono TBool)
    , ("false", Mono TBool)
    , ("not", Mono $ TBool `TArrow` TBool)
    , ("add", Mono $ TArrow TInt $ TArrow TInt TInt)
    , ("and", Mono $ TArrow TBool $ TArrow TBool TBool)
    , ("id", Forall "a" $ Mono $ TVar "a" `TArrow` TVar "a")
    , ("eq", Forall "a" $ Mono $ TArrow (TVar "a") $ TArrow (TVar "a") TBool)
    , ("compose", Forall "a" $ Forall "b" $ Forall "c" $ Mono $
        (TVar "b" `TArrow` TVar "c") `TArrow`
        ((TVar "a" `TArrow` TVar "b") `TArrow` (TVar "a" `TArrow` TVar "c")))
  ]

-- | Expose a nice type inference interface
typeOf :: Exp -> IO Type
typeOf e =
  do newVar <- makeNewVar
     t <- inferTypes newVar stdLib e
     return $ snd t

demo :: Exp -> IO ()
demo e = typeOf e >>= \t -> putStrLn $ show e ++ " : " ++ show t

-- Some example expressions

ex1 :: Exp      -- id(id(x)) : Int
ex1 = Apply (Ident "id") $ Apply (Ident "id") (Ident "x")

ex2 :: Exp      -- eq(false)(true) : Bool
ex2 = Apply (Apply (Ident "eq") (Ident "false")) (Ident "true")

ex3 :: Exp      -- compose : (b -> c) -> (a -> b) -> (a -> c)
ex3 = Ident "compose"

ex4 :: Exp      -- compose(not) : (a -> Bool) -> (a -> Bool)
ex4 = Apply (Ident "compose") (Ident "not")

ex5 :: Exp      -- compose(not)(eq(x)) : Int -> Bool
ex5 = Apply (Apply (Ident "compose") (Ident "not")) (Apply (Ident "eq") (Ident "x"))

ex6 :: Exp      -- compose(add(x)) : (a -> Int) -> (a -> Int)
ex6 = Apply (Ident "compose") (Apply (Ident "add") (Ident "x"))

ex7 :: Exp
ex7 = Apply (Apply (Apply (Ident "compose") (Ident "eq")) (Ident "add")) (Ident "x")

-- | Run all the demos
demoAll :: IO ()
demoAll = mapM_ demo [ex1, ex2, ex3, ex4, ex5, ex6]

-- Things that don't typecheck

err1 :: Exp     -- apply Int to Int
err1 = Apply (Ident "x") (Ident "x")

err2 :: Exp     -- add Bool to Bool
err2 = Apply (Apply (Ident "add") (Ident "true")) (Ident "false")

err3 :: Exp     -- unknown variable
err3 = Ident "q"
