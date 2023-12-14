module Interpreter where

import AbsLambdaNat (Exp(..), Id(..), Program(..))
import ErrM
import Control.Monad.State

exec :: Program -> [Exp]
exec (Prog []) = []
exec (Prog (e:es)) = eval e : exec (Prog es)

eval :: Exp -> Exp
eval e = evalState (eval' e) 0

eval' :: Exp -> State Int Exp
-- pure lambda calculus
eval' (App e1 e2) = do
  e1' <- eval' e1
  case e1' of
    (Abs i e3) -> do
      rhs <- subst i e2 e3
      eval' rhs
    e3 -> return (App e3 e2)
eval' (Var id) = return (Var id)
eval' (Abs id e) = return (Abs id e)
-- conditional
eval' (If e1 e2 e3 e4) = do
  e1' <- eval' e1
  e2' <- eval' e2
  if e1' == e2' then eval' e3 else eval' e4
-- arithmetic
eval' (Plus e1 e2) = do
  e1' <- eval' e1
  e2' <- eval' e2
  case (e1', e2') of
    (Int n1, Int n2) -> return (Int (n1 + n2))
    _ -> return (Plus e1' e2')

eval' (Power e1 e2) = do
  e1' <- eval' e1
  e2' <- eval' e2
  case (e1', e2') of
    (Int n1, Int n2) -> return (Int (n1 ^ n2))
    _ -> return (Power e1' e2')

-- catch-all (can be deleted in the final version)
eval' x = return x

fresh :: State Int Id
fresh = do
  i <- get
  put (i + 1)
  return (Id ("X" ++ show i))

-- substitute a variable id by an expression s in an expression
subst :: Id -> Exp -> Exp -> State Int Exp
-- pure lambda calculus
subst id s (Var id1) | id == id1 = return s
                     | otherwise = return (Var id1)
subst id s (App e1 e2) = do
  e1' <- subst id s e1
  e2' <- subst id s e2
  return (App e1' e2')
subst id s (Abs id1 e1) = do
  f <- fresh
  e1' <- subst id1 (Var f) e1
  e2' <- subst id s e1'
  return (Abs f e2')
-- conditional
subst id s (If e1 e2 e3 e4) = do
  e1' <- subst id s e1
  e2' <- subst id s e2
  e3' <- subst id s e3
  e4' <- subst id s e4
  return (If e1' e2' e3' e4')
-- arithmetic
subst id s (Plus e1 e2) = do
  e1' <- subst id s e1
  e2' <- subst id s e2
  return (Plus e1' e2')
subst id s (Int n) = return (Int n)

subst id s (Minus e1 e2) = do
  e1' <- subst id s e1
  e2' <- subst id s e2
  return (Minus e1' e2')

subst id s (Times e1 e2) = do
  e1' <- subst id s e1
  e2' <- subst id s e2
  return (Times e1' e2')

subst id s (Power e1 e2) = do
  e1' <- subst id s e1
  e2' <- subst id s e2
  return (Power e1' e2')

