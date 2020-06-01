module Interp where

import Declare
import Prelude hiding (LT, GT, EQ)
import Data.Maybe (fromJust)
import Parser

unary :: UnaryOp -> Value -> Value
unary Not (BoolV b) = BoolV (not b)
unary Neg (IntV i)  = IntV (-i)

binary :: BinaryOp -> Value -> Value -> Maybe Value
binary Add (IntV a) (IntV b) = Just $ IntV (a + b)
binary Sub (IntV a) (IntV b) = Just $ IntV (a - b)
binary Mult (IntV a) (IntV b) = Just $ IntV (a * b)
binary Div (IntV a) (IntV b) = if (b == 0) then Nothing else Just $ IntV (a `div` b)
binary And (BoolV a) (BoolV b) = Just $ BoolV (a && b)
binary Or (BoolV a) (BoolV b) = Just $ BoolV (a || b)
binary LT (IntV a) (IntV b) = Just $ BoolV (a < b)
binary LE (IntV a) (IntV b) = Just $ BoolV (a <= b)
binary GE (IntV a) (IntV b) = Just $ BoolV (a >= b)
binary GT (IntV a) (IntV b) = Just $ BoolV (a > b)
binary EQ a b = Just $ BoolV (a == b)

execute :: Program -> Value
execute (Program tenv funEnv main) = evaluate main [] tenv funEnv

evaluate :: Exp -> Env -> TypeEnv -> FunEnv -> Value
evaluate e env tenv fenv = eval e env
  where
    eval :: Exp -> Env -> Value
    eval (Try e1 e2) env =
      case eval e1 env of
        RaiseV v -> eval e2 env
        v -> v
    eval (Raise exp) env = RaiseV $ eval exp env
    eval (CaseV exp xs) env =
      let
        label = getlabel (eval exp env)
      in labelMatching label xs
      where
        labelMatching :: String -> [(String, String, Exp)] -> Value
        labelMatching _ [] = RaiseV (IntV 1)
        labelMatching label ((str1, str2, e) : xss) =
          if label == str1
            then eval e ((str2, v) : env)
            else labelMatching label xss
            where v = eval exp env
    eval (Varnt str exp t) env = VarntV str v t
      where v = eval exp env
    eval (RcdProj exp label) env =
      case eval exp env of
        RcdV xs ->
          case findrcdValue xs label of
            Just v -> v
            Nothing -> RaiseV (IntV 1)
    eval (Rcd xs) env = RcdV xs'
      where xs' = [(label, eval exp env) | (label, exp) <- xs]
    eval (Fun (x, t) body) env = ClosureV (x, t) body env
    eval (CallFC fun arg) env =
      case eval fun env of
        ClosureV (name, _) body env' -> eval body ((name, eval arg env) : env')
    eval (Call fun args) env =
      case (findFunction fun fenv) of
        Just (Function xs body) -> eval body newEnv
          where newEnv = zip (map fst xs) [eval a env | a <- args]
        Nothing -> RaiseV (IntV 1)
    eval (Lit n) _ = n
    eval (Unary op ex) env = unary op (eval ex env)
    eval (Bin op e1 e2) env =
      case (binary op (eval e1 env) (eval e2 env)) of
        Just v -> v
        Nothing -> RaiseV (IntV 0)
    eval (If e1 e2 e3) env =
      let BoolV test = eval e1 env
      in if test
         then eval e2 env
         else eval e3 env
    eval (Var v) env =
      case (lookup v env) of
        Just v1 -> v1
        Nothing -> RaiseV (IntV 2)
    eval (Decl v t a b) env =
      let
        a' = eval a env
        env' = (v, a') : env
      in eval b env'
