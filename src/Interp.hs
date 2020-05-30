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

--type Binding = (String, Value)
--type Env = [Binding]

-- Record test cases:
-- evaluate (Rcd [("a", Bin Add (Lit (IntV 1)) (Lit (IntV 2)))]) [] []
-- evaluate (Rcd [("a",Var "x")]) [("x",(IntV 1))] []


execute :: Program -> Value
execute (Program tenv funEnv main) = evaluate main [] funEnv 

evaluate :: Exp -> Env -> FunEnv -> Value
evaluate e env fenv = eval e env
  where
    eval :: Exp -> Env -> Value
    eval (Try exp1 exp2) env = case eval exp1 env of
      (RaiseV r) -> eval exp2 env
      v -> v

    eval (Raise exp) env = (RaiseV (eval exp env))
    eval (CaseV exp xs) env = do
      let (VarntV label1 value1 _) = eval exp env
      matchCase label1 value1 xs
      where
        matchCase :: String -> Value -> [(String, String, Exp)] -> Value
        matchCase name _ [] = error $ "No case match for " ++ name
        matchCase name value ((label, varName, e) : xxs) = 
          if name == label then
            eval e ((varName, value) : env)
          else
            matchCase name value xxs

    eval (Varnt str exp t) env = VarntV str (eval exp env) t
    eval (RcdProj exp str) env = case (lookup str record) of
                                Just v -> v
                                Nothing                 -> RaiseV (IntV 3)
                                where
                                  (RcdV record) = eval exp env
                                      

    eval (Rcd xs) env = RcdV $ map (\(key, exp) -> (key, eval exp env)) xs
    eval (Fun (x, t) body) env = ClosureV (x, t) body env
    eval (CallFC fun arg) env = 
      case eval fun env of
        ClosureV (name, _) body env' -> eval body ((name, eval arg env) : env')

    eval (Call fun args) env = case (findFunction fun fenv) of
                                Just (Function xs body) -> eval body newEnv
                                                             where
                                                               newEnv = zip (map fst xs) [eval a env | a <- args]
                                Nothing                 -> RaiseV (IntV 1)
    eval (Lit n) _ = n
    eval (Unary op ex) env = unary op (eval ex env)
    eval (Bin op e1 e2) env = case (binary op (eval e1 env) (eval e2 env)) of
                                Just v -> v
                                Nothing -> RaiseV (IntV 0)
    eval (If e1 e2 e3) env =
      let BoolV test = eval e1 env
      in if test
         then eval e2 env
         else eval e3 env
    eval (Var v) env = case (lookup v env) of
                         Just v1 -> v1
                         Nothing -> RaiseV (IntV 2)
    eval (Decl v t a b) env =
      let a' = eval a env
          env' = (v, a') : env
      in eval b env'