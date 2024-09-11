module APL.Eval (
  Val (..),
  eval,
  envEmpty,
  envExtend,
  envLookup)

where

import APL.AST (
  Exp (..),
  VName)  

data Val
  = ValInt Integer
  | ValBool Bool
  deriving (Eq, Show)

type Error = String
type Env = [(VName, Val)]

envEmpty :: Env
envEmpty = []

envExtend :: VName -> Val -> Env -> Env
envExtend name val env = (name, val) : env

envLookup :: VName -> Env -> Maybe Val
envLookup name env = 
  case env of
    [] -> Nothing
    (x:xs) ->
      let (v, val) = x
      in
        if name == v then Just val
        else envLookup name xs


eval :: Env -> Exp -> Either Error Val
eval env (CstInt x)  = Right $ ValInt x
eval env (CstBool b) = Right $ ValBool b


eval env (Var vname) =
  case envLookup vname env of
    Just val -> Right val
    Nothing -> Left $ "Unknown variable: " ++ vname


eval env (Let vname e1 e2) =
  case eval env e1 of
    Left err -> Left err
    Right val -> eval (envExtend vname val env) e2 


eval env (Eql e1 e2) =
  case (eval env e1, eval env e2) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right (ValInt x), Right (ValInt y)) -> Right $ ValBool $ x == y
    (Right (ValBool x), Right (ValBool y)) -> Right $ ValBool $ x == y
    (Right _, Right _) -> Left $ "Invalid operands to equality"
  

eval env (If cond e1 e2) =
  case (eval env cond) of
    Left err -> Left err
    Right (ValBool True) -> eval env e1
    Right (ValBool False) -> eval env e2
    _ -> Left $ "Condition has wrong type"


eval env (Add e1 e2) =
  case (eval env e1, eval env e2) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right (ValInt x), Right (ValInt y)) -> Right $ ValInt $ x + y
    _ -> Left $ "Types do not match"

eval env (Sub e1 e2) =
  case (eval env e1, eval env e2) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right (ValInt x), Right (ValInt y)) -> Right $ ValInt $ x - y
    _ -> Left $ "Types do not match"


eval env (Mul e1 e2) =
  case (eval env e1, eval env e2) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right (ValInt x), Right (ValInt y)) -> Right $ ValInt $ x * y
    _ -> Left $ "Types do not match"


eval env (Div e1 e2) =
  case (eval env e1, eval env e2) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right (ValInt x), Right (ValInt y)) ->
      if y == 0
        then Left "Division by zero"
        else Right $ ValInt $ x `div` y
    _ -> Left $ "Types do not match"


eval env (Pow e1 e2) =
  case (eval env e1, eval env e2) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right (ValInt x), Right (ValInt y)) ->
      if y < 0
        then Left "Negative exponent"
        else Right $ ValInt $ x ^ y
    _ -> Left $ "Types do not match"
