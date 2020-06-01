module Declare where

import Data.Maybe (fromJust)
import Prelude hiding (LT, GT, EQ)

data BinaryOp = Add | Sub | Mult | Div
              | And | Or  | GT   | LT  | LE
              | GE  | EQ
              deriving Eq

data UnaryOp
  = Neg
  | Not
  deriving Eq

  --- raise as value

data Value
  = IntV Int    
  | BoolV Bool  
  | ClosureV (String, Type) Exp Env        -- added
  | RcdV [(String, Value)]                 -- added
  | VarntV String Value Type               -- added
  | RaiseV Value                           -- added
  deriving Eq

data Type
  = TInt
  | TBool
  | TFun Type Type                   -- added
  | TRcd [(String, Type)]            -- added
  | TVarnt [(String, Type)]          -- added
  | TypDecl String                   -- added
  deriving Eq

--data Declr = FunDecl (String, Function)
--           | TypDecl (String, Type)

data Exp
  = Lit Value
  | Unary UnaryOp Exp
  | Bin BinaryOp Exp Exp
  | If Exp Exp Exp
  | Var String
  | Decl String Type Exp Exp
  | Call String [Exp]
  | CallFC Exp Exp                      -- added
  | Fun (String, Type) Exp              -- added
  | Rcd [(String, Exp)]                 -- added
  | RcdProj Exp String                  -- added
  | Varnt String Exp Type               -- added
  | CaseV Exp [(String, String, Exp)]   -- added
  | Raise Exp                           -- added
  | Try Exp Exp                         -- added
  deriving Eq

data Program = Program TypeEnv FunEnv Exp

type TypeEnv = [(String, Type)]

type FunEnv = [(Type, String, Function)]

data Function = Function [(String, Type)] Exp

type Env = [(String, Value)]

-- Pretty printer

instance Show Type where
  show TInt = "Int"
  show TBool = "Bool"
  show (TFun t1 t2) = paren $ show t1 ++ " -> " ++ show t2
  show (TRcd xs) = "{" ++ showSep ", " (map (\(n, t) -> n ++ " : " ++ show t) xs) ++ "}"
  show (TVarnt xs) = "<" ++ showSep ", " (map (\(n, t) -> n ++ " : " ++ show t) xs) ++ ">"
  show (TypDecl str) = str

instance Show Value where
  show (IntV n) = show n
  show (BoolV True) = "true"
  show (BoolV False) = "false"
  show (ClosureV{}) = "Closure"
  show (RcdV xs) = "{" ++ showSep ", " (map (\(n, v) -> n ++ " = " ++ show v) xs) ++ "}"
  show (VarntV str v t) = "<" ++ str ++ "=" ++ show v ++ " : " ++ show t ++ ">"
  show (RaiseV v) = error "TODO Question 11"

instance Show Program where
  show (Program tenv fenv e) = (showTyp tenv) ++ "\n" ++ showSep "\n" (map showFun fenv) ++ "\n" ++ show e

showTyp :: [(String, Type)] -> String
showTyp [] = ""
showTyp (x:xs) = "type " ++ fst (x) ++ " = " ++ show (snd (x)) ++ "\n" ++ showTyp xs

showFun :: (Type, String, Function) -> String
showFun (typ, name, Function args body) =
  "function " ++ show typ ++ " " ++
  name ++
  "(" ++
  showSep ", " (map (\(n, t) -> n ++ " : " ++ show t) args) ++
  ") {" ++ "\n" ++
  show body ++ "\n" ++
  "}"

showSep :: String -> [String] -> String
showSep _ [] = ""
showSep _ [e] = e
showSep sep (e:es) = e ++ sep ++ showSep sep es

instance Show Exp where
  show = showExp 0

showExp :: Int -> Exp -> String
showExp _ (Try exp1 exp2) = "try " ++ show exp1 ++ " with " ++ show exp2
showExp _ (Raise exp) = error "TODO Question 11"
showExp _ (CaseV exp xs) = "  case " ++ show exp ++ " of\n  | " ++ showSep "\n  | " (map (\(n, v, e) -> "<" ++ n ++ "=" ++ v ++ "> => " ++ show e)  xs)
showExp _ (Varnt str exp t) = "<" ++ str ++ "=" ++ show exp ++ " : " ++ show t ++ ">"
showExp _ (RcdProj exp str) = showExp 0 exp ++ "." ++ str
showExp _ (Rcd r) = "{" ++ showSep ", " (map (\(n, e) -> n ++ " = " ++ show e) r) ++ "}"
showExp _ (Fun (n, t) e) = "function(" ++ n ++ " : " ++ show t ++ ") {\n " ++ show e ++ "\n}"
showExp _ (CallFC f arg) = show f ++ "(" ++ show arg ++ ")"
showExp _ (Call f args) = "@" ++ f ++ "(" ++ showSep ", " (map show args) ++ ")"
showExp _ (Lit i) = show i
showExp _ (Var x) = x
showExp level (Decl x t a b) =
  if level > 0
    then paren result
    else result
  where
    result = "var " ++ x ++ " : " ++ show t ++ " = " ++ showExp 0 a ++ "; " ++ showExp 0 b
showExp level (If a b c) =
  if level > 0
    then paren result
    else result
  where
    result = "if (" ++ showExp 0 a ++ ") " ++ showExp 0 b ++ "; else " ++ showExp 0 c
showExp _ (Unary Neg a) = "-" ++ showExp 99 a
showExp _ (Unary Not a) = "!" ++ showExp 99 a
showExp level (Bin op a b) =
  showBin level (fromJust (lookup op levels)) a (fromJust (lookup op names)) b
  where
    levels =
      [ (Or, 1)
      , (And, 2)
      , (GT, 3)
      , (LT, 3)
      , (LE, 3)
      , (GE, 3)
      , (EQ, 3)
      , (Add, 4)
      , (Sub, 4)
      , (Mult, 5)
      , (Div, 5)
      ]
    names =
      [ (Or, "||")
      , (And, "&&")
      , (GT, ">")
      , (LT, "<")
      , (LE, "<=")
      , (GE, ">=")
      , (EQ, "==")
      , (Add, "+")
      , (Sub, "-")
      , (Mult, "*")
      , (Div, "/")
      ]

showBin :: Int -> Int -> Exp -> String -> Exp -> String
showBin outer inner a op b =
  if inner < outer
    then paren result
    else result
  where
    result = showExp inner a ++ " " ++ op ++ " " ++ showExp inner b

paren :: String -> String
paren x = "(" ++ x ++ ")"


-- Helper functions for evaluator and type checker
-- You may want to use these helper functions

first :: (String, String, Exp) -> String
first (str, _, _) = str

second :: (String, String, Exp) -> String
second (_, str, _) = str

third :: (String, String, Exp) -> Exp
third (_, _, exp) = exp

findrcd :: [(String, Exp)] -> String -> Maybe Exp
findrcd [] _ = Nothing
findrcd (x:xs) str = if (str == (fst x)) then (Just (snd x)) else (findrcd xs str)

findrcdValue :: [(String, Value)] -> String -> Maybe Value
findrcdValue [] _ = Nothing
findrcdValue (x:xs) str = if (str == (fst x)) then (Just (snd x)) else (findrcdValue xs str)

findrcdType :: [(String, Type)] -> String -> Maybe Type
findrcdType [] _ = Nothing
findrcdType (x:xs) str = if (str == (fst x)) then (Just (snd x)) else (findrcdType xs str)

getlabel :: Value -> String
getlabel (VarntV str v1 t) = str

findFunction :: String -> FunEnv -> Maybe Function
findFunction name [] = Nothing
findFunction name ((typ, funName, function):xs) = if (name == funName) then Just function else findFunction name xs

findFunctionType :: String -> FunEnv -> Maybe Type
findFunctionType name [] = Nothing
findFunctionType name ((typ, funName, function):xs) = if (name == funName) then Just typ else findFunctionType name xs


-- code for testing

-- | Examples:
-- >>> showSep "; " ["hello", "to", "world"]
-- "hello; to; world"
--
-- >>> showSep "; " ["hello"]
-- "hello"
--
-- >>> showSep "; " []
-- ""
--
-- >>> show (Call "max" [(Lit (IntV 3)), (Lit (IntV 4))])
-- "@max(3, 4)"
--
-- >>> show (Call "abs" [(Lit (IntV 3))])
-- "@abs(3)"
--
-- >>> showFun (TInt, "foo", Function [("x",TInt),("y",TInt)] (Bin Add (Var "x") (Var "y")))
-- "function Int foo(x : Int, y : Int) {\nx + y\n}"
--

prog1 :: Program
prog1 =
  Program
    []
    [ (TInt, "absolute", Function [("x", TInt)]
                     (If (Bin GT (Var "x") (Lit (IntV 0))) (Var "x") (Unary Neg (Var "x"))))
    , (TInt, "max", Function [("x", TInt), ("y", TInt)]
                (If (Bin GT (Var "x") (Var "y")) (Var "x") (Var "y")))
    ]
    (Call "max" [Call "absolute" [Lit (IntV (-5))], Lit (IntV 4)])

-- >>> show prog1
-- "\nfunction Int absolute(x : Int) {\nif (x > 0) x; else -x\n}\nfunction Int max(x : Int, y : Int) {\nif (x > y) x; else y\n}\n@max(@absolute(-5), 4)"
--

test1 :: Exp
test1 = Lit (RcdV [("age", (IntV 34)), ("ismale", (BoolV True))])

-- >>> show (Lit (RcdV [("age", (IntV 34)), ("ismale", (BoolV True))]))
-- "{age = 34, ismale = true}"
--

test2 :: String
test2 = "function Int getAge (x : {age : Int, ismale : Bool}) {x.age} var john : {age : Int, ismale : Bool} = {age = 30, ismale = true}; @getAge(john)"

test3 :: String
test3 = "type Person = {age : Int, ismale : Bool} function Int getAge (x : Person) {x.age} var john : Person = {age = 30, ismale = true}; @getAge(john)"

-- >>> evaluate (Rcd [("a", Bin Add (Lit (IntV 1)) (Lit (IntV 2)))]) [] [] []
-- >>> evaluate (Rcd [("a",Var "x")]) [("x",(IntV 1))] [] []
-- {a = 3}
-- {a = 1}
--

test4 :: String
test4 = "type Shape = <Circle : Int, Square : Int> function Bool isCircle (x : Shape){ case x of | <Circle=diameter> => true| <Square=length> => false}var myShape : Shape = <Circle=30 : Int>; @isCircle(myShape)"

test5 :: String
test5 = "function Int divide (x : Int, y : Int) { x/y } function Bool isZero (x : Int) { if (x ==0) true; else false } var x : Int = 1; var y : Int = 0; try @divide(x,y) with @isZero(y)"

test6 :: String
test6 = "var x : Int = 5; var f : Int -> Int = function(foo : Int -> Int) { foo(x) }; var add : Int -> Int = function(x : Int) {function(y : Int) {    x + y  }};var x : Int = 4;f(add(x))"