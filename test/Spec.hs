import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck ()

import Lib

main :: IO ()
main = defaultMain tests

tests = [
        testGroup "Base magic functions"
           [ testProperty "pmod" prop_pmod
           , testProperty "prop2" prop1
           ]
      ]

prop1 b = b == (not (not b))
  where types = (b :: Bool)

prop_pmod a b 0 = True
  where types = (a :: Int, b :: Int)
prop_pmod a b n = ((a `pmod` n) + (b `pmod` n)) `pmod` n == (a + b) `pmod` n
  where types = (a :: Int, b :: Int)
