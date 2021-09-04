import Test.Tasty
import Test.Tasty.HUnit


------------------------------------------------------------------------------
main :: IO ()
main = defaultMain =<< myTestTree


myTestTree :: IO TestTree
myTestTree =
  pure
  $ testGroup "myMainGroup"
      [ testCase "failing test case"
        $ assertEqual "True /= False" True False
      ]
