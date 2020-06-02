module TypeCheck where

import Debug.Trace

import Declare
import Prelude hiding (LT, GT, EQ)

type TEnv = [(String, Type)]

type TFunEnv = [(String, (TEnv, Type))]

tunary :: UnaryOp -> Type -> Either String Type
tunary Neg TInt = Right TInt
tunary Not TBool = Right TBool
tunary op t = Left $ "No match for specified operation with type \"" ++ show t ++ "\""

tbinary :: BinaryOp -> Type -> Type -> Either String Type
tbinary Add TInt TInt = Right TInt
tbinary Sub TInt TInt = Right TInt
tbinary Mult TInt TInt = Right TInt
tbinary Div TInt TInt = Right TInt
tbinary And TBool TBool = Right TBool
tbinary Or TBool TBool = Right TBool
tbinary LT TInt TInt = Right TBool
tbinary LE TInt TInt = Right TBool
tbinary GE TInt TInt = Right TBool
tbinary GT TInt TInt = Right TBool
tbinary EQ t1 t2 | t1 == t2 = Right TBool
tbinary op t1 t2 = Left $ "No match for specified operation with types \"" ++ show t1 ++ "\" and \"" ++ show t2 ++ "\""

checkProgram :: Program -> Either String Type
checkProgram (Program tenv fds main) =
  case checkFunEnv tenv fds of
    Left msg -> Left $ "Type error in function definitions:\n" ++ msg
    Right fenv ->
      case tcheck main [] tenv fenv of
        Left msg -> Left $ "Type error in program body:\n" ++ msg
        Right t -> Right t

checkFunEnv :: TEnv -> FunEnv -> Either String TFunEnv
checkFunEnv tenv fds = checkFunEnv1 fds []
  where
    checkFunEnv1 :: FunEnv -> TFunEnv -> Either String TFunEnv
    checkFunEnv1 [] fenv = Right fenv
    checkFunEnv1 ((rtype, name, Function paras body) : fs) fenv =
      case tcheck body paras tenv ((name, (paras, rtype)) : fenv) of
        Left msg -> Left $ "In definition of function \"" ++ name ++ "\":\n" ++ msg
        Right t ->
          case typRslvr rtype tenv of
            Left msg -> Left $ "In decleration of function \"" ++ name ++ "\":\n" ++ msg
            Right rtype' ->
              if rtype' == t
                then checkFunEnv1 fs ((name, (paras, rtype)) : fenv)
                else Left $ "In decleration of function \"" ++ name ++ "\":\nExpected return type: " ++ show rtype' ++ "\nResolved return type: " ++ show t

typRslvr :: Type -> TEnv -> Either String Type
typRslvr (TypDecl name) tenv =
  case lookup name tenv of
    Nothing -> Left $ "\"Type " ++ name ++ "\" not found in Type Environment"
    Just t -> Right t
typRslvr t _ = Right t

tcheck :: Exp -> TEnv -> TEnv -> TFunEnv -> Either String Type

tcheck (Lit v) _ _ _ =
  case v of
    IntV _ -> Right TInt
    BoolV _ -> Right TBool
    _ -> Left $ "Only \"Int\" and \"Bool\" Types are considered literals"

tcheck (Unary op e) env tenv fenv =
  case tcheck e env tenv fenv of
    Left msg -> Left $ "In argument of \"" ++ show (Unary op e) ++ "\":\n" ++ msg
    Right t ->
      case tunary op t of
        Left msg -> Left $ "In \"" ++ show (Unary op e) ++ "\":\n" ++ msg
        Right t' -> Right t'

tcheck (Bin op e1 e2) env tenv fenv =
  case (tcheck e1 env tenv fenv, tcheck e2 env tenv fenv) of
    (Left msg, _) -> Left $ "In first argument of \"" ++ show (Bin op e1 e2) ++ "\":\n" ++ msg
    (_, Left msg) -> Left $ "In second argument of \"" ++ show (Bin op e1 e2) ++ "\":\n" ++ msg
    (Right t1, Right t2) ->
      case tbinary op t1 t2 of
        Left msg -> Left $ "In \"" ++ show (Bin op e1 e2) ++ "\":\n" ++ msg
        Right t -> Right t

tcheck (If e1 e2 e3) env tenv fenv =
  case tcheck e1 env tenv fenv of
    Left msg -> Left $ "In \"if\" expression of \"" ++ show (If e1 e2 e3) ++ "\":\n" ++ msg
    Right TBool ->
      case tcheck e2 env tenv fenv of
        Left msg -> Left $ "In \"then\" expression of \"" ++ show (If e1 e2 e3) ++ "\":\n" ++ msg
        Right t2 ->
          case tcheck e3 env tenv fenv of
            Left msg -> Left $ "In \"else\" expression of \"" ++ show (If e1 e2 e3) ++ "\":\n" ++ msg
            Right t3 ->
              if t2 == t3
                then Right t2
                else Left $ "In If statement \"" ++ show (If e1 e2 e3) ++ "\":\n" ++ "\"" ++ show e2 ++ "\" and \"" ++ show e3 ++ "\" must resolve to same Type"
    Right _ -> Left $ "In If statement \"" ++ show (If e1 e2 e3) ++ "\":\n" ++ "\"" ++ show e1 ++ "\" must resolve to Type \"Bool\""

tcheck (Var v) env _ _ =
  case lookup v env of
    Nothing -> Left $ "\"Variable " ++ v ++ "\" not found in Environment"
    Just t -> Right t

tcheck (Decl v t e body) env tenv fenv =
  case typRslvr t tenv of
    Left msg -> Left $ "In Var Declaration \"var " ++ v ++ " : " ++ show t ++ " = " ++ showExp 0 e ++ "\":\n" ++ msg
    Right t' ->
      case tcheck e env tenv fenv of
        Left msg -> Left $ "In Var Declaration \"var " ++ v ++ " : " ++ show t ++ " = " ++ showExp 0 e ++ "\":\n" ++ msg
        Right t1 ->
          if t' == t1
            then tcheck body ((v, t) : env) tenv fenv
            else Left $ "In Var Declaration \"var " ++ v ++ " : " ++ show t ++ " = " ++ showExp 0 e ++ "\":\nExpected type: " ++ show t' ++ "\nResolved espression type: " ++ show t1

-- tcheck (Decl v t e body) env tenv fenv =
--   case t of
--     (TypDecl name) ->
--       case typRslvr t tenv of
--         Left msg -> Left $ "In Var Declaration \"" ++ show (Decl v t e body) ++ "\":\n" ++ msg
--         Right t' -> tcheck body ((v, t) : env) tenv fenv
--     _ ->
--       case tcheck e env tenv fenv of
--         Left msg -> Left $ "In Var Declaration \"" ++ show (Decl v t e body) ++ "\":\n" ++ msg
--         Right t1 -> tcheck body ((v, t1) : env) tenv fenv



tcheck (Call fname args) env tenv fenv =
  case lookup fname fenv of
    Nothing -> Left $ "\"Function " ++ fname ++ "\" not found in Function Environment"
    Just (paras, t) ->
      if paras' == args'
        then Right t
        else Left $ "In function call \"" ++ show (Call fname args) ++ "\":\nExpected arguments type: [" ++ showSep ", " (map (\(Right t1) -> show t1) paras') ++ "]\nResolved arguments type: " ++ showSep ", " (map (\(Right t1) -> show t1) args')
      where
        paras' = map (Right . snd) paras
        args' = map (\e -> tcheck e env tenv fenv) args

tcheck (CallFC e1 e2) env tenv fenv =
  case (tcheck e1 env tenv fenv, tcheck e2 env tenv fenv) of
    (Right (TFun t1 t2), Right t3)
      | t1 == t3 -> Right t2
    (_, Left msg) -> Left $ "In argument of \"" ++ show (CallFC e1 e2) ++ "\":\n" ++ msg
    (_, _) -> Left $ "In first-class function call of \"" ++ show (CallFC e1 e2) ++ "\":\n\"" ++ show e1 ++ "\" must resolve to Type \"Fun Type Type\""

tcheck (Fun (x, t) e) env tenv fenv =
  case tcheck e ((x, t) : env) tenv fenv of
    Left msg -> Left $ "In first-class function \"" ++ show (Fun (x, t) e) ++ "\":\n" ++ msg
    Right t2 -> Right $ (TFun t t2)

tcheck (Rcd xs) env tenv fenv =
  case rcdtcheck xs [] of
    Left msg -> Left $ "In record \"" ++ show (Rcd xs) ++ "\":\n" ++ msg
    Right ts -> Right $ TRcd ts
  where
    rcdtcheck :: [(String, Exp)] -> TEnv -> Either String TEnv
    rcdtcheck [] ys = Right (reverse ys)
    rcdtcheck ((label, e) : xss) ys =
      case tcheck e env tenv fenv of
        Left msg -> Left $ "In field \"" ++ label ++ " : " ++ show e ++ "\":\n" ++ msg
        Right t -> rcdtcheck xss ((label, t) : ys)

tcheck (RcdProj e label) env tenv fenv =
  case tcheck e env tenv fenv of
    Left msg -> Left $ "In record projection \"" ++ show (RcdProj e label) ++ "\":\n" ++ msg
    Right (TRcd xs) ->
      case lookup label xs of
        Nothing -> Left $ "In record projection \"" ++ show (RcdProj e label) ++ "\":\n" ++ "Label \"" ++ label ++ "\" not found in Record \"" ++ show (TRcd xs) ++ "\""
        Just t -> Right t
    Right (TypDecl name) ->
      case typRslvr (TypDecl name) tenv of
        Left msg -> Left $ "In record projection \"" ++ show (RcdProj e label) ++ "\":\n" ++ msg
        Right (TRcd ys) ->
          case lookup label ys of
            Nothing -> Left $ "In record projection \"" ++ show (RcdProj e label) ++ "\":\n" ++ "Label \"" ++ label ++ "\" not found in Record \"" ++ show (TRcd ys) ++ "\""
            Just t -> Right t
        Right _ -> Left $ "In record projection \"" ++ show (RcdProj e label) ++ "\":\n" ++ show e ++ "\" must resolve to Type \"TRcd [(String, Type)]\""
    Right _ -> Left $ "In record projection \"" ++ show (RcdProj e label) ++ "\":\n" ++ show e ++ "\" must resolve to Type \"TRcd [(String, Type)]\""

tcheck (Varnt name e t) env tenv fenv =
  case tcheck e env tenv fenv of
    Left msg -> Left $ "In variant expression \"" ++ show (Varnt name e t) ++ "\":\n" ++ msg
    Right t1 ->
      if t1 == t
        then Right $ TVarnt [(name, t)]
        else Left $ "In variant expression \"" ++ show (Varnt name e t) ++ "\":\nExpected type: " ++ show t ++ "\nResolved type: " ++ show t1

tcheck (CaseV e xs) env tenv fenv =
  case tcheck e env tenv fenv of
    Left msg -> Left $ "In case expression \"" ++ show (CaseV e xs) ++ "\":\n" ++ msg
    Right t ->
      case typRslvr t tenv of
        Left msg -> Left $ "In case expression \"" ++ show (CaseV e xs) ++ "\":\n" ++ msg
        Right (TVarnt ((name, t') : ys)) -> caseChkr xs
          where
            caseChkr :: [(String, String, Exp)] -> Either String Type
            caseChkr [] = Left $ "In case expression \"" ++ show (CaseV e xs) ++ "\":\nCase list cannot be empty"
            caseChkr ((l1, v1, e1) : (l2, v2, e2) : xss) =
              case (tcheck e1 ((v1, t') : env) tenv fenv, tcheck e2 ((v2, t') : env) tenv fenv) of
                (Left msg, _) -> Left $ "In case expression \"" ++ show (CaseV e xs) ++ "\":\nIn case \"" ++ "<" ++ l1 ++ "=" ++ v1 ++ "> => " ++ show e1 ++ "\":\n" ++ msg
                (_, Left msg) -> Left $ "In case expression \"" ++ show (CaseV e xs) ++ "\":\nIn case \"" ++ "<" ++ l2 ++ "=" ++ v2 ++ "> => " ++ show e2 ++ "\":\n" ++ msg
                (Right t1, Right t2) ->
                  if t1 == t2
                    then caseChkr ((l2, v2, e2) : xss)
                    else Left $ "In case expression \"" ++ show (CaseV e xs) ++ "\":\nCases \"" ++ "<" ++ l1 ++ "=" ++ v1 ++ "> => " ++ show e1 ++ "\" and \"" ++ "<" ++ l2 ++ "=" ++ v2 ++ "> => " ++ show e2 ++ "\" have different resolved types (" ++ show t1 ++ ", " ++ show t2 ++ ")"
            caseChkr ((l1, v1, e1) : xss) =
              case tcheck e1 env tenv fenv of
                Left msg -> Left $ "In case expression \"" ++ show (CaseV e xs) ++ "\":\nIn case \"" ++ "<" ++ l1 ++ "=" ++ v1 ++ "> => " ++ show e1 ++ "\":\n" ++ msg
                Right t1 -> Right t1
        _ -> Left $ "In case expression \"" ++ show (CaseV e xs) ++ "\":\n" ++ show e ++ " must resolve to Type \"TVarnt [(String, Type)]\"\n Currently get: "  ++ show (tcheck e env tenv fenv)

tcheck (Raise e) env tenv fenv = tcheck e env tenv fenv

tcheck (Try e1 e2) env tenv fenv =
  case (tcheck e1 env tenv fenv, tcheck e2 env tenv fenv) of
    (Left msg, _) -> Left $ "In \"try\" expression of \"" ++ show (Try e1 e2) ++ "\":\n" ++ msg
    (_, Left msg) -> Left $ "In \"catch\" expression of \"" ++ show (Try e1 e2) ++ "\":\n" ++ msg
    (Right t1, Right t2) -> Right t1
