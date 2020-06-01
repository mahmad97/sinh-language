module TypeCheck where

import Debug.Trace

import Declare
import Prelude hiding (LT, GT, EQ)

type TEnv = [(String, Type)]

type TFunEnv = [(String, (TEnv, Type))]

tunary :: UnaryOp -> Type -> Maybe Type
tunary Neg TInt = Just TInt
tunary Not TBool = Just TBool
tunary _ _ = Nothing

tbinary :: BinaryOp -> Type -> Type -> Maybe Type
tbinary Add TInt TInt = Just TInt
tbinary Sub TInt TInt = Just TInt
tbinary Mult TInt TInt = Just TInt
tbinary Div TInt TInt = Just TInt
tbinary And TBool TBool = Just TBool
tbinary Or TBool TBool = Just TBool
tbinary LT TInt TInt = Just TBool
tbinary LE TInt TInt = Just TBool
tbinary GE TInt TInt = Just TBool
tbinary GT TInt TInt = Just TBool
tbinary EQ t1 t2 | t1 == t2 = Just TBool
tbinary _ _ _ = Nothing

checkProgram :: Program -> Bool
checkProgram (Program tenv fds main) =
  case checkFunEnv tenv fds of
    Nothing -> False
    Just fenv ->
      case tcheck main [] tenv fenv of
        Nothing -> False
        Just _ -> True  

checkFunEnv :: TEnv -> FunEnv -> Maybe TFunEnv
checkFunEnv tenv fds = checkFunEnv1 fds []
  where
    checkFunEnv1 :: FunEnv -> TFunEnv -> Maybe TFunEnv
    checkFunEnv1 [] fenv = Just fenv
    checkFunEnv1 ((rtype, name, Function paras body) : fs) fenv =
      case tcheck body paras tenv ((name, (paras, rtype)) : fenv) of
        Nothing -> Nothing
        Just t ->
          if rtype' == t
            then checkFunEnv1 fs ((name, (paras, rtype)) : fenv)
            else Nothing
            where
              rtype' = typRslvr rtype tenv

typRslvr :: Type -> TEnv -> Type
typRslvr (TypDecl struct) tenv =
  case lookup struct tenv of
    Just t -> t
    _ -> error "Type reference not found"
typRslvr t _ = t  

recTypRslvr :: Type -> TEnv -> Type
recTypRslvr TInt tenv = TInt
recTypRslvr TBool tenv = TBool
recTypRslvr (TFun t1 t2) tenv = (TFun (recTypRslvr t1 tenv) (recTypRslvr t2 tenv))
recTypRslvr (TRcd ts) tenv =
  (TRcd [(label, recTypRslvr t tenv) | (label, t) <- ts])
recTypRslvr (TVarnt ts) tenv = error "TODO Question 10"
recTypRslvr (TypDecl struct) tenv =
  case lookup struct tenv of
    Just t -> recTypRslvr t tenv
    _ -> error "Struct reference not found"

tcheck :: Exp -> TEnv -> TEnv -> TFunEnv -> Maybe Type

tcheck (Lit v) _ _ _ =
  case v of
    IntV _ -> Just TInt
    BoolV _ -> Just TBool

tcheck (Unary op e) env tenv fenv =
  case tcheck e env tenv fenv of
    Just t -> tunary op t
    _ -> Nothing

tcheck (Bin op e1 e2) env tenv fenv =
  case (tcheck e1 env tenv fenv, tcheck e2 env tenv fenv) of
    (Just t1, Just t2) -> tbinary op t1 t2
    _ -> Nothing

tcheck (If e1 e2 e3) env tenv fenv =
  case tcheck e1 env tenv fenv of
    Just TBool ->
      case tcheck e2 env tenv fenv of
        Nothing -> Nothing
        Just t1 ->
          case tcheck e3 env tenv fenv of
            Nothing -> Nothing
            Just t2 ->
              if t1 == t2
                then Just t1
                else Nothing
    _ -> Nothing

tcheck (Var v) env _ _ = lookup v env

-- tcheck (Decl v t e body) env tenv fenv =
--   case tcheck e env tenv fenv of
--     Nothing -> Nothing
--     Just t2 ->
--       if (trace (show t') t') == (trace (show t2) t2)
--         then tcheck body ((v, t) : env) tenv fenv
--         else Nothing
--         where t' = typRslvr t tenv

tcheck (Decl v t e body) env tenv fenv =
  case t of
    (TypDecl name) ->
      case lookup name tenv of
        Just t' -> tcheck body ((v, t) : env) tenv fenv
        Nothing -> Nothing
    _ ->
      case tcheck e env tenv fenv of
        Just t1 -> tcheck body ((v, t1) : env) tenv fenv
        Nothing -> Nothing

tcheck (Call fname args) env tenv fenv =
  case lookup fname fenv of
    Nothing -> Nothing
    Just (paras, t) ->
      if map (Just . snd) paras == map (\e -> tcheck e env tenv fenv) args
        then Just t
        else Nothing


tcheck (CallFC e1 e2) env tenv fenv =
  case (tcheck e1 env tenv fenv, tcheck e2 env tenv fenv) of
    (Just (TFun t1 t2), Just t3)
      | t1 == t3 -> Just t2
    _ -> Nothing

tcheck (Fun (x, t) e) env tenv fenv = do
  t2 <- tcheck e ((x, t) : env) tenv fenv
  Just $ (TFun t t2)

tcheck (Rcd xs) env tenv fenv =
  case rcdtcheck xs [] of
    Just ts -> Just $ TRcd ts
    Nothing -> Nothing
  where
    rcdtcheck :: [(String, Exp)] -> TEnv -> Maybe TEnv
    rcdtcheck [] ys = Just (reverse ys)
    rcdtcheck ((label, e) : xss) ys =
      case tcheck e env tenv fenv of
        Just t -> rcdtcheck xss ((label, t) : ys)
        Nothing -> Nothing

tcheck (RcdProj e label) env tenv fenv =
  case tcheck e env tenv fenv of
    Just (TypDecl name) ->
      case typRslvr (TypDecl name) tenv of
        TRcd ys -> lookup label ys
        _ -> Nothing
    Just (TRcd xs) -> lookup label xs
    _ -> Nothing

tcheck (Varnt name e t) env tenv fenv =
  case tcheck e env tenv fenv of
    Just t1 | t1 == t -> Just t
    _ -> Nothing

tcheck (CaseV e xs) env tenv fenv =
  case tcheck e env tenv fenv of
    Nothing -> Nothing
    Just t -> caseChkr xs
      where
        caseChkr :: [(String, String, Exp)] -> Maybe Type
        caseChkr [] = Nothing
        caseChkr ((_, v1, e1) : (l2, v2, e2) : xss) =
          case (tcheck e1 ((v1, t) : env) tenv fenv, tcheck e2 ((v2, t) : env) tenv fenv) of
            (Just t1, Just t2) | t1 == t2 -> caseChkr ((l2, v2, e2) : xss)
            _ -> Nothing
        caseChkr ((_, _, e1) : xss) = tcheck e1 env tenv fenv

tcheck (Raise e) env tenv fenv = tcheck e env tenv fenv

tcheck (Try e1 e2) env tenv fenv =
  case (tcheck e1 env tenv fenv, tcheck e2 env tenv fenv) of
    (Just t1, Just t2) -> Just t1
    _ -> Nothing
















