module Interp where

import Declare
import Prelude hiding (LT, GT, EQ)
import Data.Maybe (fromJust)
import Parser
import Debug.Trace (trace)

unary :: UnaryOp -> Value -> Value
unary Not (BoolV b) = BoolV (not b)
unary Neg (IntV i)  = IntV (-i)

binary :: BinaryOp -> Value -> Value -> Maybe Value
binary Add (IntV a) (IntV b) = Just $ IntV (a + b)
binary Add (StringV a) (StringV b) = Just $ StringV (a ++ b)
binary Add (StringV a) (IntV b) = Just $ StringV (a ++ show b)
binary Add (IntV a) (StringV b) = Just $ StringV (show a ++ b)
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
evaluate e env fenv = evalState [] $ eval e env
  where
    eval :: Exp -> Env -> Stateful Value
    eval (Try exp1 exp2) env = do
      v1 <- eval exp1 env 
      case v1 of
        (RaiseV _) -> eval exp2 env
        v -> return (v)

    eval (Raise exp) env = do 
      v <- eval exp env
      return (RaiseV v)

    eval (CaseV exp xs) env = do
      v <- eval exp env
      case v of
        (VarntV label1 value1 _) -> matchCase label1 value1 xs
      where
        matchCase :: String -> Value -> [(String, String, Exp)] -> Stateful Value
        matchCase name _ [] = return $ RaiseV (IntV 4)
        matchCase name value ((label, varName, e) : xxs) = 
          if name == label then
            eval e ((varName, value) : env)
          else
            matchCase name value xxs

    eval (Varnt str exp t) env = do
      v <- eval exp env
      return (VarntV str v t)
    
    eval (RcdProj exp str) env = do
      v <- eval exp env
      case v of
        (RcdV record) -> case (lookup str record) of
                            Just v -> return (v)
                            Nothing -> return (RaiseV (IntV 3))                        

    -- eval (Rcd xs) env = return (RcdV $ map (\(key, exp) -> (key, (eval exp env))) xs)
    eval (Rcd xs) env = evaluateRcd xs []
      where
        evaluateRcd :: [(String,Exp)] -> [(String,Value)] -> Stateful Value
        evaluateRcd [] rcdvs = return (RcdV rcdvs)
        evaluateRcd ((key, exp) : xss) rcdvs = do
          v <- eval exp env
          evaluateRcd xss ((key, v) : rcdvs)

    eval (Fun (x, t) body) env = return $ ClosureV (x, t) body env
    
    eval (CallFC fun arg) env = do
      v <- eval fun env
      case v of
        (ClosureV (name, _) body env') -> do
          argV <- eval arg env
          eval body ((name, argV) : env')

    eval (Call fun args) env = 
      case findFunction fun fenv of
        Just (Function xs body) -> do
          argVs <- evaluateArgs args []
          let newEnv = zip (map fst xs) argVs
          eval body newEnv
         where 
          evaluateArgs :: [Exp] -> [Value] -> Stateful [Value]
          evaluateArgs [] vs = return vs
          evaluateArgs (x:xs) vs = do
            v <- eval x env
            evaluateArgs xs (vs ++ [v])

                                        
        _ -> eval (CallFC (Var fun) (args !! 0)) env
    
    eval (Lit n) _ = return n
    
    eval (Unary op ex) env = do
      v <- eval ex env
      return $ unary op v
    
    eval (Bin op e1 e2) env = do
      v1 <- eval e1 env
      v2 <- eval e2 env
      case (binary op v1 v2) of
                                Just v -> return v
                                Nothing -> return (RaiseV (IntV 0))
    
    eval (If e1 e2 e3) env = do
      v <- eval e1 env
      case v of
        (BoolV test) -> if test
                         then eval e2 env
                         else eval e3 env
    
    eval (Var v) env = case (lookup v env) of
                         Just v1 -> return v1
                         Nothing -> return (RaiseV (IntV 2))
    
    eval (Decl v t a b) env = do
      a' <- eval a env
      let env' = (v, a') : env
      eval b env'

    -- Mutation operation
    eval (Mutable e) env = do
      ev <- eval e env
      newMemory ev

    eval (Access a) env  = do
      v <- eval a env
      case v of
        AddressV i -> readMemory i

    eval (Assign a e b) env = do
      v <- eval a env
      case v of
        AddressV i -> do
          ev <- eval e env
          updateMemory ev i
          eval b env
      


-- Utility Functions
access :: Int -> Memory -> Value
access n m = m !! n

update :: Int -> Value -> Memory -> Memory
update n v m =
  let (before,(_:after)) = splitAt n m in
  before ++ [v] ++ after

newMemory :: Value -> Stateful Value
newMemory val = ST (\mem -> (AddressV (length mem), mem ++ [val]))

readMemory :: Int -> Stateful Value
readMemory i = ST (\mem -> (access i mem, mem))

updateMemory :: Value -> Int -> Stateful ()
updateMemory val i = ST (\mem -> ((), update i val mem))

evalState :: Memory -> Stateful Value -> Value
evalState m (ST c) =
  let (v, _) = c m
  in v



