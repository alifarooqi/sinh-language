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


substituteType :: TypeEnv ->(String, Type) ->  Either String (String, Type)
substituteType typeEnv (name, (TypDecl str)) = 
  case lookup str typeEnv of
    Just t -> Right (name, t)
    Nothing -> Left $ "Undeclared type: " ++ str
substituteType _ a = Right a

checkFunEnv :: TypeEnv -> FunEnv -> Either String TFunEnv
checkFunEnv typeEnv fds = checkFunEnv1 fds [] -- starts with an empty function type environment
  where
    checkFunEnv1 :: FunEnv -> TFunEnv -> Either String TFunEnv
    checkFunEnv1 [] fenv = Right fenv
    checkFunEnv1 ((typ, name, Function paras body):fs) fenv = do
      (_, typ') <- substituteType typeEnv ("", typ)  -- Substitute return type
      let paras' = [ substituteType typeEnv p | p <- paras ] -- Substitute parameters' type
      let n = lefts paras'
      if (length n) > 0 then 
        Left $ head n
      else do
        let newParas = rights paras'
        t <- tcheck typeEnv body newParas ((name, (newParas, typ')) : fenv)
        checkFunEnv1 fs ((name, (newParas, t)) : fenv)

extractRights :: [Either a b] -> Either a [b]
extractRights xs = do
  let n = lefts xs
  if (length n) == 0 then 
    Right (rights xs)
  else
    Left $ head n

tcheck :: TypeEnv -> Exp -> TEnv -> TFunEnv -> Either String Type

tcheck typeEnv (Try exp1 exp2) env fenv = do
  tcheck typeEnv exp2 env fenv
  tcheck typeEnv exp1 env fenv

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
  xxs <- extractRights $ checkRcd xs
  let s = map (substituteType typeEnv) xxs
  ss <- extractRights s
  Right $ TRcd ss
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
    _ -> Left $ "Type Error: Unexpected parameter type at " ++ show (CallFC e1 e2)

tcheck typeEnv (Call name args) tenv fenv =
  case lookup name fenv of
    Just (paras, t) ->
      -- check if types of arguments match the types of parameters
      if map (Right . snd) paras == map (\e -> tcheck typeEnv e tenv fenv) args
        then Right t
        else Left $ "Type Error: Parameter type and expression don't match in function call " ++ show (Call name args)
    Nothing -> tcheck typeEnv (CallFC (Var name) (args !! 0)) tenv fenv

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
    
tcheck typeEnv (Var v) tenv _ = case lookup v tenv of
  Just t -> Right t
  Nothing -> Left $ "Variable " ++ v ++ " is not declared"

tcheck typeEnv (Decl v t e1 e2) tenv fenv =
  case t of
    (TypDecl str) -> (do
      case lookup str typeEnv of
        Just t2 -> tcheck typeEnv (Decl v t2 e1 e2) tenv fenv
        Nothing -> Left $ "Type " ++ str ++ " has not been declared")
    _              -> do t1 <- tcheck typeEnv e1 tenv fenv
                         if t == t1 then
                            tcheck typeEnv e2 ((v, t) : tenv) fenv
                         else
                            Left $ "Type of the variable " ++ show v ++ " don't match.\nExpected " ++ show t ++ ", got " ++ show t1

tcheck typeEnv (Mutable e) tenv fenv = do
  t <- tcheck typeEnv e tenv fenv
  Right $ TMutable t

tcheck typeEnv (Access e) tenv fenv = tcheck typeEnv e tenv fenv

tcheck typeEnv (Assign e1 e2 e3) tenv fenv = do
  t1 <- tcheck typeEnv e1 tenv fenv
  t2 <- tcheck typeEnv e2 tenv fenv
  if t1 == t2 then 
    tcheck typeEnv e3 tenv fenv 
  else 
    Left $ "Cannot assign " ++ show t2 ++ " to a mutable of type " ++ show t2


checkProgram :: Program -> Either String Type
checkProgram (Program typeEnv fds main) = do
  fenv <- checkFunEnv typeEnv fds
  main' <- replaceTypeInExp typeEnv main
  tcheck typeEnv main' [] fenv

replaceTypeInExp :: TypeEnv -> Exp -> Either String Exp
replaceTypeInExp typeEnv (Lit v) = Right $ Lit v
replaceTypeInExp typeEnv (Unary op e) = do
  e1 <- replaceTypeInExp typeEnv e
  Right $ Unary op e1
replaceTypeInExp typeEnv (Bin op e1 e2) = do
  e3 <- replaceTypeInExp typeEnv e1
  e4 <- replaceTypeInExp typeEnv e2
  Right $ Bin op e3 e4
replaceTypeInExp typeEnv (If e1 e2 e3) = do
  e11 <- replaceTypeInExp typeEnv e1
  e22 <- replaceTypeInExp typeEnv e2
  e33 <- replaceTypeInExp typeEnv e3
  Right $ If e11 e22 e33
replaceTypeInExp typeEnv (Var v) = Right $ Var v
replaceTypeInExp typeEnv (Decl v t e1 e2) = do
  t1 <- replaceType typeEnv t
  e3 <- replaceTypeInExp typeEnv e1
  e4 <- replaceTypeInExp typeEnv e2
  Right $ Decl v t1 e3 e4
replaceTypeInExp typeEnv (Call v exps) = do
  let m = (map (replaceTypeInExp typeEnv) exps)
  xs <- extractRights m
  Right $ Call v xs
replaceTypeInExp typeEnv (CallFC e1 e2) = do
  e3 <- replaceTypeInExp typeEnv e1
  e4 <- replaceTypeInExp typeEnv e2
  Right $ CallFC e3 e4
replaceTypeInExp typeEnv (Fun (name, t) e) = do
  t1 <- replaceType typeEnv t
  e2 <- replaceTypeInExp typeEnv e
  Right $ Fun (name, t1) e2
replaceTypeInExp typeEnv (Rcd xs) = do
  let m = (map (mapperFunc typeEnv) xs)
  xss <- extractRights m
  Right $ Rcd xss
replaceTypeInExp typeEnv (RcdProj e key) = do
  e2 <- replaceTypeInExp typeEnv e
  Right $ RcdProj e2 key
replaceTypeInExp typeEnv (Varnt n e t) = do
  t1 <- replaceType typeEnv t
  e2 <- replaceTypeInExp typeEnv e
  Right $ Varnt n e2 t1
replaceTypeInExp typeEnv (CaseV e xs) = do
  e2 <- replaceTypeInExp typeEnv e
  let m = (map (mapperFunc2 typeEnv) xs)
  xss <- extractRights m
  Right $ CaseV e2 xss
replaceTypeInExp typeEnv (Raise e) = do
  e2 <- replaceTypeInExp typeEnv e
  Right $ Raise e2
replaceTypeInExp typeEnv (Try e1 e2) = do
  e3 <- replaceTypeInExp typeEnv e1
  e4 <- replaceTypeInExp typeEnv e2
  Right $ Try e3 e4
replaceTypeInExp typeEnv (Mutable e) = do
  e2 <- replaceTypeInExp typeEnv e
  Right $ Mutable e2
replaceTypeInExp typeEnv (Access e) = do
  e2 <- replaceTypeInExp typeEnv e
  Right $ Access e2
replaceTypeInExp typeEnv (Assign e1 e2 e3) = do
  e11 <- replaceTypeInExp typeEnv e1
  e22 <- replaceTypeInExp typeEnv e2
  e33 <- replaceTypeInExp typeEnv e3
  Right $ Assign e11 e22 e33


mapperFunc :: TypeEnv -> (String,Exp) -> Either String (String,Exp)
mapperFunc typeEnv (s, e) = do
  e1 <- replaceTypeInExp typeEnv e
  Right $ (s, e1)

mapperFunc2 :: TypeEnv -> (String, String,Exp) -> Either String (String, String,Exp)
mapperFunc2 typeEnv (s1, s2, e) = do
  e1 <- replaceTypeInExp typeEnv e
  Right $ (s1, s2, e1)

mapperFunc3 :: TypeEnv -> (String, Type) -> Either String (String, Type)
mapperFunc3 typeEnv (s, t) = do
  t1 <- replaceType typeEnv t
  Right $ (s, t1)


replaceType :: TypeEnv -> Type -> Either String Type
replaceType typeEnv (TypDecl name) = do
  case lookup name typeEnv of
    Just t -> replaceType typeEnv t
    _ -> Left $ "Type " ++ name ++ " has not been declared"
replaceType typeEnv (TRcd xs) = do
  let xss = map (mapperFunc3 typeEnv) xs
  a <- extractRights xss
  Right $ TRcd a
replaceType typeEnv (TVarnt xs) = do
  let xss = map (mapperFunc3 typeEnv) xs
  a <- extractRights xss
  Right $ TVarnt a
replaceType typeEnv (TFun t1 t2) = do
  t3 <- replaceType typeEnv t1
  t4 <- replaceType typeEnv t2
  Right $ TFun t3 t4
replaceType _ a = Right a








