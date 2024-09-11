module APL.AST_Tests (tests) where

import APL.AST (Exp (..), printExp)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Prettyprinting"
    [
      testCase "Add" $
        printExp (Add (CstInt 2) (CstInt 5))
          @?= "(2 + 5)",

      testCase "Sub" $
        printExp (Sub (CstInt 7) (CstInt 3))
          @?= "(7 - 3)",

      testCase "Mul" $
        printExp (Mul (CstInt 4) (CstInt 6))
          @?= "(4 * 6)",

      testCase "Div" $
        printExp (Div (CstInt 8) (CstInt 2))
          @?= "(8 / 2)",

      testCase "Pow" $
        printExp (Pow (CstInt 3) (CstInt 2))
          @?= "(3 ** 2)",

      testCase "Eql" $
        printExp (Eql (CstInt 5) (CstInt 5))
          @?= "(5 == 5)",

      testCase "If" $
        printExp (If (CstBool True) (CstInt 2) (CstInt 0))
        @?= "(if True then 2 else 0)",

      testCase "Let" $
        printExp (Let "x" (Add (CstInt 2) (CstInt 3)) (Var "x"))
        @?= "(let x = (2 + 3) in x)"
      ,
      testCase "Lamdba" $
        printExp (Lambda "y" (Add (Var "x") (Var "y")))
        @?= "(\\y -> (x + y))"
      ,
      testCase "Apply" $
        printExp
        ( Apply
          ( Let
            "x"
            (CstInt 2)
            (Lambda "y" (Add (Var "x") (Var "y")))
          )           -- e1
          (CstInt 3)  -- e2
      )
      @?= "((let x = 2 in (\\y -> (x + y))) 3)"
      ,
      testCase "Try Catch" $
        printExp
          ( TryCatch
            (CstInt 0)
            (CstInt 1)
          )
        @?= "(try 0 catch 1)"
    ]
