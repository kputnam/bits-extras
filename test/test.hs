{-# LANGUAGE ForeignFunctionInterface, RankNTypes, FlexibleContexts, ExistentialQuantification #-}
-- Test framework imports
import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test, Testable)
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

-- Code to test
import Data.Bits.Extras
import Data.Word
import Data.Int
import Data.Bits

tests :: [Test]
tests = [ testGroup "Test Cases" 
          [ testCase "trailingZeros" test_trailingZeros_all
          ]
        ]

main :: IO ()
main = defaultMain tests

-- TODO: Use Quickcheck to produce arbitrary values?
testPattern :: Integral a => a
testPattern = 0xdeadbeef


type PolyTest = (ExtraBits a, Integral a, Bounded a) => a -> Assertion

testTypes :: PolyTest -> Assertion
testTypes t = do
    t (testPattern :: Int)
    t (testPattern :: Word)
    t (testPattern :: Int8)
    t (testPattern :: Int16)
    t (testPattern :: Int32)
    t (testPattern :: Int64)
    t (testPattern :: Word8)
    t (testPattern :: Word16)
    t (testPattern :: Word32)
    t (testPattern :: Word64)

test_trailingZeros :: PolyTest
test_trailingZeros i = do
    let t0 = i - i
    -- No test for 0, as the result would be undefined
    trailingZeros (t0 + 1) @?= fromIntegral t0
    trailingZeros (t0 + 3) @?= fromIntegral t0
    trailingZeros (t0 + 2) @?= fromIntegral t0 + 1
    let hbitshift = bitSize i - 1
        hbitset = trailingZeros $ (t0 + 1) `shiftL` hbitshift
    assertEqual "High bit set" (fromIntegral hbitset) hbitshift
test_trailingZeros_all :: Assertion
test_trailingZeros_all = testTypes test_trailingZeros
