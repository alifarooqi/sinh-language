module Declare where

import Data.Maybe (fromJust)
import Data.List (intercalate, intersect)
import Prelude hiding (LT, GT, EQ)
import Control.Monad (ap, liftM)

-- import Debug.Trace (trace)



-- Colors and debugging
colorReset = "\x1b[0m"
debugColor = "\x1b[36m"
red = "\x1b[31m"
yellow = "\x1b[33m"

debug p = (debugColor ++ show p ++ colorReset)

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
  | StringV String
  | AddressV Int        -- new
  deriving Eq

data Type
  = TInt
  | TBool
  | TFun Type Type                   -- added
  | TRcd [(String,Type)]             -- added
  | TVarnt [(String,Type)]           -- added
  | TypDecl String                   -- added
  | TString             -- new
  | TMutable Type
  -- deriving Eq

--data Declr = FunDecl (String, Function)
--           | TypDecl (String, Type)

type Env = [(String, Value)]

data Program = Program TypeEnv FunEnv Exp

type TypeEnv = [(String,Type)]

type FunEnv = [(Type, String, Function)]

data Function = Function [(String, Type)] Exp

data Exp = Lit Value
         | Unary UnaryOp Exp
         | Bin BinaryOp Exp Exp
         | If Exp Exp Exp
         | Var String
         | Decl String Type Exp Exp
         | Call String [Exp]
         | CallFC Exp Exp                      -- added
         | Fun (String, Type) Exp              -- added
         | Rcd [(String,Exp)]                  -- added
         | RcdProj Exp String                  -- added
         | Varnt String Exp Type               -- added
         | CaseV Exp [(String, String, Exp)]   -- added
         | Raise Exp                           -- added
         | Try Exp Exp                         -- added
         -- | Seq Exp Exp          -- new
         | Mutable Exp          -- new
         | Access Exp           -- new
         | Assign Exp Exp Exp      -- new

         deriving Eq

-- <Implementing Mutable Variables>

data Stateful t = ST (Memory -> (t, Memory))

instance Monad Stateful where
  return val = ST (\m -> (val, m))
  (ST c) >>= f =
    ST
      (\m -> let (val, m') = c m
                 ST f' = f val
             in f' m')

type Memory = [Value]

-- Make GHC 7.10 happy
instance Functor Stateful where
  fmap = liftM

instance Applicative Stateful where
  pure = return
  (<*>) = ap


-- </Implementing Mutable Variables>


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

-- Pretty printer

instance Show Type where
  show TInt = "Int"
  show TBool = "Bool"
  show (TFun t1 t2) = paren $ show t1 ++ " -> " ++ show t2
  show (TRcd xs)    = "{" ++ intercalate ", " (map (\(key, t) -> key ++ ": " ++ show t) xs) ++ "}"
  show (TVarnt xs)  = "<" ++ intercalate ", " (map (\(key, t) -> key ++ ": " ++ show t) xs) ++ ">"
  show (TypDecl str) = str
  show (TString) = "String"
  show (TMutable a) = "Mutable " ++ show a

instance Show Value where
  show (IntV n) = show n
  show (BoolV True) = "true"
  show (BoolV False) = "false"
  show (ClosureV{}) = "Closure"
  show (RcdV xs)    =  "{" ++ intercalate ", " (map (\(key, v) -> key ++ ": " ++ show v) xs) ++ "}"
  show (VarntV str v t) = "<" ++ str ++ "=" ++ (show v) ++ " : " ++ (show t) ++ ">"
  show (RaiseV v) = "Runtime Error: " ++
    case v of
      (IntV 0) -> "Division with zero!"
      (IntV 1) -> "Function not declared!" -- Redundant: Chacked during typecheck
      (IntV 2) -> "Variable not declared!" -- Redundant: Chacked during typecheck
      (IntV 3) -> "Record not declared!"   -- Redundant: Chacked during typecheck
      (StringV msg) -> msg
  show (StringV str) = str
  show (AddressV n) = show $ "Address: " ++ show n

instance Eq Type where
  TInt == TInt = True
  TBool == TBool = True
  TFun a1 b1 == TFun a2 b2 = a1 == a2 && b1 == b2
  TRcd a == TRcd b = a == b
  TVarnt a == TVarnt b = not $ null $ intersect a b
  TypDecl a == TypDecl b = a == b
  TString == TString = True
  TMutable a == TMutable b = a == b
  TMutable a == b = a == b
  a == TMutable b = a == b
  _ == _ = False


-- | Examples:
--
-- >>> show prog1
-- "function Int absolute(x : Int) {\n if (x > 0) x; else -x\n}\nfunction max(x : Int, y : Int) {\n if (x > y) x; else y\n}\nmax(absolute(-5), 4)"
instance Show Program where
  show (Program tenv fenv e) = (showTyp tenv) ++ "\n" ++ showSep "\n" (map showFun fenv) ++ "\n" ++ show e

showTyp :: [(String, Type)] -> String
showTyp [] = ""
showTyp (x:xs) = "type " ++ fst (x) ++ " = " ++ show (snd (x)) ++ "\n" ++ showTyp xs

-- | Examples:
--
-- >>> showFun ("foo",Function [("x",TInt),("y",TInt)] (Bin Add (Var "x") (Var "y")))
-- "function foo(x : Int, y : Int) {\n x + y\n}"
showFun :: (Type, String, Function) -> String
showFun (typ, name, Function args body) =
  "function " ++ show typ ++ " " ++
  name ++
  "(" ++
  showSep ", " (map (\(n, t) -> n ++ " : " ++ show t) args) ++
  ") {" ++ "\n" ++
  show body ++ "\n" ++
  "}"

-- | Examples:
--
-- >>> showSep "; " ["hello", "to", "world"]
-- "hello; to; world"
--
-- >>> showSep "; " ["hello"]
-- "hello"
--
-- showSep "; " []
-- ""
showSep :: String -> [String] -> String
showSep _ [] = ""
showSep _ [e] = e
showSep sep (e:es) = e ++ sep ++ showSep sep es

-- | Examples:
--
-- >>> show (Call "max" [(Lit (IntV 3)), (Lit (IntV 4))])
-- "max(3, 4)"
--
-- >>> show (Call "abs" [(Lit (IntV 3))])
-- "abs(3)"

showCaseV :: Exp -> [(String, String, Exp)] -> String
showCaseV e xs = "case " ++ show e ++ " of" ++ showCases xs
  where
    showCases :: [(String, String, Exp)] -> String
    showCases [] = ""
    showCases ((label, variant, exp) : xxs) = 
      "\n| <" ++ label ++ "=" ++ variant ++ "> => " ++ show exp ++ showCases xxs

instance Show Exp where
  show = showExp 0

showExp :: Int -> Exp -> String
-- showExp _ (Seq e1 e2) = show e1 ++ ";\n" ++ show e2
showExp _ (Mutable e) = show e
showExp _ (Access e) = show e
showExp _ (Assign e1 e2 e3) = show e1 ++ " <- " ++ show e2 ++ ";\n" ++ show e3
showExp _ (Try exp1 exp2) = "try " ++ show exp1 ++ " with " ++ show exp2
showExp _ (Raise exp) = show exp
showExp _ (CaseV exp xs) = showCaseV exp xs
showExp _ (Varnt str exp t) = "<" ++ str ++ "=" ++ (show exp) ++ " : " ++ (show t) ++ ">"
showExp _ (RcdProj exp str) = showRcdProject exp str
showExp _ (Rcd r) = "{" ++ intercalate ", " (map (\(key, v) -> key ++ ": " ++ show v) r) ++ "}"
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

getlabel :: Value -> String
getlabel (VarntV str v1 t) = str

findFunction :: String -> FunEnv -> Maybe Function
findFunction name [] = Nothing
findFunction name ((typ, funName, function):xs) = if (name == funName) then Just function else findFunction name xs

findFunctionType :: String -> FunEnv -> Maybe Type
findFunctionType name [] = Nothing
findFunctionType name ((typ, funName, function):xs) = if (name == funName) then Just typ else findFunctionType name xs

showRcdProject :: Exp -> String -> String
showRcdProject (Rcd record) str = show $ fromJust $ lookup str record

e1 :: Exp
e1 = Rcd [("age", Lit(IntV(23)))]
e2 :: Exp
e2 = RcdProj e1 "age"


