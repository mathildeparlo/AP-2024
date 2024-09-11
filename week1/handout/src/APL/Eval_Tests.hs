module APL.Eval_Tests (tests) where

import APL.AST (Exp (..), VName)
import APL.Eval (Val (..), eval, envEmpty, envExtend, envLookup)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertEqual)
import Data.Either (Either(Left))

tests :: TestTree
tests =
  testGroup
    "Evaluation"
    [
      testCase "Int" $
        eval envEmpty (CstInt 1)
        @?= Right (ValInt 1),
      
      testCase "Add" $
        eval envEmpty (Add (CstInt 2) (CstInt 3))
        @?= Right (ValInt 5),
      
      testCase "Add (wrong type)" $
        eval envEmpty (Add (CstInt 2) (CstBool True))
        @?= Left "Types do not match",
      
      testCase "Sub" $
        eval envEmpty (Sub (CstInt 3) (CstInt 3))
        @?= Right (ValInt 0),

      testCase "Sub (wrong type)" $
        eval envEmpty (Sub (CstInt 2) (CstBool True))
        @?= Left "Types do not match",
      
      testCase "Mul" $
        eval envEmpty (Mul (CstInt 2) (CstInt 3))
        @?= Right (ValInt 6),

      testCase "Mul (wrong type)" $
        eval envEmpty (Mul (CstInt 2) (CstBool True))
        @?= Left "Types do not match",
      
      testCase "Div" $
        eval envEmpty (Div (CstInt 6) (CstInt 3))
        @?= Right (ValInt 2),

      testCase "Div (wrong type)" $
        eval envEmpty (Div (CstInt 2) (CstBool True))
        @?= Left "Types do not match",
      
      testCase "Pow" $
        eval envEmpty (Pow (CstInt 2) (CstInt 3))
        @?= Right (ValInt 8),

      testCase "Pow (wrong type)" $
        eval envEmpty (Pow (CstInt 2) (CstBool True))
        @?= Left "Types do not match",
      
      testCase "DivByZero" $
        eval envEmpty (Div (CstInt 6) (CstInt 0))
        @?= Left "Division by zero",
      
      testCase "NegativeExp" $
        eval envEmpty (Pow (CstInt 2) (CstInt (-3)))
        @?= Left "Negative exponent",
      
      testCase "Bool False" $
        eval envEmpty (CstBool False)
        @?= Right (ValBool False),
      
      testCase "Bool True" $
        eval envEmpty (CstBool True)
        @?= Right (ValBool True),
      
      testCase "Equal Int" $
        eval envEmpty (Eql (CstInt 1) (CstInt 1))
        @?= Right (ValBool True),
      
      testCase "Not Equal Int" $
        eval envEmpty (Eql (CstInt 2) (CstInt 1))
        @?= Right (ValBool False),
      
      testCase "Equal Bool" $
        eval envEmpty (Eql (CstBool True) (CstBool True))
        @?= Right (ValBool True),
      
      testCase "Not Equal Bool" $
        eval envEmpty (Eql (CstBool True) (CstBool False))
        @?= Right (ValBool False),
      
      testCase "Invalid comparison" $
        eval envEmpty (Eql (CstBool True) (CstInt 1))
        @?= Left "Invalid operands to equality",
      
      testCase "If wrong cond" $
        eval envEmpty (If (CstInt 3) (CstBool True) (CstInt 1))
        @?= Left "Condition has wrong type",
      
      testCase "If True" $
        eval envEmpty (If (CstBool True) (CstInt 1) (CstInt 2))
        @?= Right (ValInt 1),
      
      testCase "If False" $
        eval envEmpty (If (CstBool False) (CstInt 1) (CstInt 2))
        @?= Right (ValInt 2),

      testCase "envEmpty" $
        envEmpty
        @?= [],

      testCase "envExtend" $
        envExtend "some_name" (ValInt 3) envEmpty
        @?= [("some_name", ValInt 3)],

      testCase "envLookup found" $
        envLookup "some_name" [("some_name", ValInt 3)]
        @?= Just (ValInt 3),

      testCase "envLookup not found" $
        envLookup "wrong_name" [("some_name", ValInt 3)]
        @?= Nothing,

      testCase "Var not found" $
        eval envEmpty (Var "not_found")
        @?= Left "Unknown variable: not_found",

      testCase "Let correct" $
        eval envEmpty (Let "x" (CstInt 3) (Add (Var "x") (Var "x")))
        @?= Right (ValInt 6),

      testCase "Let incorrect" $
        eval envEmpty (Let "x" (CstInt 3) (Add (Var "x") (Var "y")))
        @?= Left "Unknown variable: y"
    ]
