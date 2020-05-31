module TypeCheck where

import Declare
import Prelude hiding (LT, GT, EQ)
import Data.Either(rights, lefts)
import Debug.Trace (trace)

type TEnv = [(String,Type)]

type TFunEnv = [(String, (TEnv, Type))]

tunary :: UnaryOp -> Type -> Either String Type
tunary Neg TInt = Right TInt
tunary Not TBool = Right TBool

tunary Neg t = Left $ "Type Error: Wrong type used with Neg Operator\nExpected Int, got " ++ show t
tunary Not t = Left $ "Type Error: Wrong type used with Not Operator\nExpected Bool, got " ++ show t

tbinary :: BinaryOp -> Type -> Type -> Either String Type
tbinary Add TInt  TInt  = Right TInt
tbinary Add TString  TString = Right TString
tbinary Add TString  TInt = Right TString
tbinary Add TInt  TString = Right TString
tbinary Sub TInt  TInt  = Right TInt
tbinary Mult TInt TInt  = Right TInt
tbinary Div TInt  TInt  = Right TInt
tbinary And TBool TBool = Right TBool
tbinary Or  TBool TBool = Right TBool
tbinary LT  TInt  TInt  = Right TBool
tbinary LE  TInt  TInt  = Right TBool
tbinary GE  TInt  TInt  = Right TBool
tbinary GT  TInt  TInt  = Right TBool
tbinary EQ  t1    t2    | t1 == t2 = Right TBool
-- Custom Error Messages
tbinary Add t1  t2  = Left $ "Type Error: Wrong type used with Addition Operator.\nCannot add " ++ show t1 ++ " and " ++ show t2
tbinary Sub t1  t2  = Left $ "Type Error: Wrong type used with Subtraction Operator.\nExpected Int and Int, got " ++ show t1 ++ " and " ++ show t2
tbinary Mult t1 t2  = Left $ "Type Error: Wrong type used with Multiplication Operator.\nExpected Int and Int, got " ++ show t1 ++ " and " ++ show t2
tbinary Div t1  t2  = Left $ "Type Error: Wrong type used with Division Operator.\nExpected Int and Int, got " ++ show t1 ++ " and " ++ show t2
tbinary And t1  t2 = Left $ "Type Error: Wrong type used with And Operator.\nExpected Bool and Bool, got " ++ show t1 ++ " and " ++ show t2
tbinary Or  t1  t2 = Left $ "Type Error: Wrong type used with Or Operator.\nExpected Bool and Bool, got " ++ show t1 ++ " and " ++ show t2
tbinary LT  t1  t2  = Left $ "Type Error: Wrong type used with '<' Operator.\nExpected Int and Int, got " ++ show t1 ++ " and " ++ show t2
tbinary LE  t1  t2  = Left $ "Type Error: Wrong type used with '<=' Operator.\nExpected Int and Int, got " ++ show t1 ++ " and " ++ show t2
tbinary GE  t1  t2  = Left $ "Type Error: Wrong type used with '>=' Operator.\nExpected Int and Int, got " ++ show t1 ++ " and " ++ show t2
tbinary GT  t1  t2  = Left $ "Type Error: Wrong type used with '>' Operator.\nExpected Int and Int, got " ++ show t1 ++ " and " ++ show t2
tbinary EQ  t1  t2
        | t1 /= t2 = Left $ "Type Error: Different types used in Equality.\nGot " ++ show t1 ++ " and " ++ show t2
tbinary _ _ _ = Left "Type Error: Wrong type used with Binary Operator" -- Redundant


substituteType :: (String, Type) -> TypeEnv -> Either String (String, Type)
substituteType (name, (TypDecl str)) typeEnv = case lookup str typeEnv of
  Just t -> Right (name, t)
  Nothing -> Left $ "Undeclared type: " ++ str
substituteType a _ = Right a

checkFunEnv :: TypeEnv -> FunEnv -> Either String TFunEnv
checkFunEnv typeEnv fds = checkFunEnv1 fds [] -- starts with an empty function type environment
  where
    checkFunEnv1 :: FunEnv -> TFunEnv -> Either String TFunEnv
    checkFunEnv1 [] fenv = Right fenv
    checkFunEnv1 ((typ, name, Function paras body):fs) fenv = do
      (_, typ') <- substituteType ("", typ) typeEnv -- Substitute return type
      let paras' = [ substituteType p typeEnv | p <- paras ] -- Substitute parameters' type
      let n = lefts paras'
      if (length n) > 0 then 
        Left $ head n
      else do
        let newParas = rights paras'
        t <- tcheck typeEnv body newParas ((name, (newParas, typ')) : fenv)
        checkFunEnv1 fs ((name, (newParas, t)) : fenv)


tcheck :: TypeEnv -> Exp -> TEnv -> TFunEnv -> Either String Type

tcheck typeEnv (Try exp1 exp2) env fenv = case tcheck typeEnv exp1 env fenv of
  (Right t) -> (Right t)
  Left _ -> tcheck typeEnv exp2 env fenv

tcheck typeEnv (Raise exp) env fenv = tcheck typeEnv exp env fenv

tcheck typeEnv (CaseV exp caseList) env fenv = do
      t1 <- tcheck typeEnv exp env fenv
      matchCaseType t1 caseList Nothing
      where
        matchCaseType :: Type -> [(String, String, Exp)] -> Maybe Type -> Either String Type
        matchCaseType _ [] (Just t) = Right t
        matchCaseType _ [] Nothing = Left "Type Error: No case in the Case Variant"
        matchCaseType t1 ((label, varName, e) : xxs) t = do
          case t1 of
            (TVarnt variantTypeList) -> (do
              case lookup label variantTypeList of
                Just t3 -> (do
                  t2 <- tcheck typeEnv e ((varName, t3) : env) fenv
                  if t == Nothing || t == (Just t2) then
                    matchCaseType t1 xxs (Just t2)
                  else
                    Left "Type Error: Incosistant expression types in Case Variant")
                Nothing -> matchCaseType t1 xxs t)
            _ -> Left "Type Error: Variant not used in case analysis"

tcheck typeEnv (Varnt str exp t) env fenv = do
  t1 <- tcheck typeEnv exp env fenv
  if t == t1 then Right (TVarnt [(str, t)]) 
  else Left $ "Type Error: Types in variant " ++ show str ++ " do not match."
    
tcheck typeEnv (RcdProj exp str) env fenv = do
  t <- tcheck typeEnv exp env fenv
  case t of
    (TRcd record) -> case lookup str record of
      Just t -> Right t
      _ -> Left $ "Record has no attribute " ++ show str
    te -> Left $ "Type Error: Projection seems to be done a variable of type " ++ show te

tcheck typeEnv (Rcd xs) env fenv = do
  let xxs = checkRcd xs
  let n = lefts xxs
  if (length n) == 0 then 
    Right $ TRcd (rights xxs)
  else
    Left $ head n
  where
    checkRcd :: [(String,Exp)] -> [Either String (String,Type)]
    checkRcd [] = []
    checkRcd ((key, exp) : xss) = 
      case tcheck typeEnv exp env fenv of
        Right t -> (Right (key, t)) : checkRcd xss
        Left err -> [Left err]
    
tcheck typeEnv (Fun (x, t1) e) env fenv = do
  t2 <- tcheck typeEnv e ((x, t1) : env) fenv 
  Right (TFun t1 t2)

tcheck typeEnv (CallFC e1 e2) env fenv = do
  t <- tcheck typeEnv e1 env fenv
  t3 <- tcheck typeEnv e2 env fenv
  case t of
    (TFun t1 t2)
      | t1 == t3 -> Right t2
    _ -> Left $ "Type Error: Unexpected error at " ++ show (CallFC e1 e2)

tcheck typeEnv (Call name args) tenv fenv =
  case lookup name fenv of
    Nothing -> Left $ "Undeclared function call at " ++ show (Call name args)
    Just (paras, t) ->
      -- check if types of arguments match the types of parameters
      if map (Right . snd) paras == map (\e -> tcheck typeEnv e tenv fenv) args
        then Right t
        else Left $ "Type Error: Parameter type and expression don't match in function call " ++ show (Call name args)

tcheck typeEnv (Lit v) _ _ =
  case v of
    IntV _ -> Right TInt
    BoolV _ -> Right TBool
    StringV _ -> Right TString

tcheck typeEnv (Unary op e) tenv fenv =
  case tcheck typeEnv e tenv fenv of
    Right t  -> tunary op t
    err -> err

tcheck typeEnv (Bin op e1 e2) tenv fenv = do
  t1 <- tcheck typeEnv e1 tenv fenv
  t2 <- tcheck typeEnv e2 tenv fenv
  tbinary op t1 t2

tcheck typeEnv (If e1 e2 e3) tenv fenv =
  case tcheck typeEnv e1 tenv fenv of
    Right TBool ->
      case tcheck typeEnv e2 tenv fenv of
        Right t1 ->
          if Right t1 ==
             tcheck typeEnv e3 tenv fenv
            then Right t1
            else Left $ "Type Error: The return type of IF and ELSE statements don't match at\n" ++ show (If e1 e2 e3)
        err -> err
    Right te -> Left $ "Type Error: The condition in if statement is not boolean.\n Expected: Bool, got " ++ show te ++ " at\n" ++ show (If e1 e2 e3)
    err -> err
    

tcheck typeEnv (Var v) tenv _ = case lookup v (trace (debug $ "Env: " ++ show tenv) tenv) of
  Just t -> Right t
  Nothing -> Left $ "Variable " ++ v ++ " is not declared"

tcheck typeEnv (Decl v t e1 e2) tenv fenv =
  case (trace (debug $ "Var Decl: " ++ v ++ " : " ++ show t) t) of
    (TypDecl str) -> (do
      case lookup str typeEnv of
        Just t2 -> tcheck typeEnv (Decl v t2 e1 e2) tenv fenv
        Nothing -> Left $ "Type " ++ str ++ " has not been declared")
    _              -> do t1 <- tcheck typeEnv e1 tenv fenv
                         tcheck typeEnv e2 ((v, t) : tenv) fenv
 

checkProgram :: Program -> Either String Type
checkProgram (Program typeEnv fds main) = do
  fenv <- checkFunEnv typeEnv fds
  tcheck typeEnv main [] fenv






