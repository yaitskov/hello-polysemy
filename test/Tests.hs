import Test.Tasty
import Test.Tasty.HUnit


------------------------------------------------------------------------------
main :: IO ()
main = defaultMain =<< myTestTree


myTestTree :: IO TestTree
myTestTree =
  pure
  $ testGroup "myMainGroup"
      [
        testCase "passing test case"
        $ assertEqual "True == True" True True

        -- , testCase "failing test case"
        -- $ assertEqual "True == False" True False
      ]
