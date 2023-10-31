{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications   #-}

module Main where

import Sample
import DeltaStepping

import Test.Similar
import qualified Test.Gen                                           as Gen

import Data.Vector.Storable                                         ( Vector )
import Hedgehog
import Test.Tasty                                                   hiding ( defaultMain )
import Test.Tasty.Bench
import Test.Tasty.Hedgehog
import Test.Tasty.Runners
import qualified Data.Graph.Inductive                               as G
import qualified Data.Vector.Generic                                as V
import qualified Hedgehog.Gen                                       as Gen
import qualified Hedgehog.Internal.Gen                              as Gen
import qualified Hedgehog.Internal.Seed                             as Seed
import qualified Hedgehog.Internal.Tree                             as Tree
import qualified Hedgehog.Range                                     as Range

import GHC.Conc                                                     ( setNumCapabilities )


main :: IO ()
main = defaultMain
  [ localOption (NumThreads 1)                            -- run each test sequentially with many cores
  $ localOption (mkTimeout 60_000_000)                    -- timeout each test after 60 s
  $ localOption (HedgehogTestLimit (Just 1000))           -- number of each test to run
  $ localOption (HedgehogShrinkLimit (Just 100))          -- number of shrinks allowed while searching for minimal reproducible test case
  $ localOption (HedgehogDiscardLimit (Just 1000))        -- maximum number of discard cases before a test fails
  $ testGroup "test"
      [ testProperty "sample1"   $ prop_sample sample1 3
      , testProperty "sample2"   $ prop_sample sample2 3
      , testProperty "line"      $ prop_arbitrary (Gen.line (Range.linear 2 Gen._MAX_NODES) Gen.enumerated Gen.distance) 0.7
      , testProperty "loop"      $ prop_arbitrary (Gen.loop (Range.linear 2 Gen._MAX_NODES) Gen.enumerated Gen.distance) 0.7
      , testProperty "arbitrary" $ prop_arbitrary (Gen.arbitrary (Range.linear 1 Gen._MAX_NODES) (Range.linear 1 Gen._MAX_EDGES) Gen.enumerated Gen.distance) 0.7
      , localOption (HedgehogTestLimit (Just 1))
      $ testProperty "large"     $ prop_sample large 0.7  -- sanity check for the benchmarks
      ]
  , localOption WallTime                                  -- benchmark using wall-time rather than CPU-time
  $ localOption (NumThreads 1)                            -- run each test sequentially with many cores
  $ localOption (mkTimeout 60_000_000)                    -- timeout each test after 60 s
  $ bgroup "bench"
      [                               bench "N1" $ nfAppIO (bench_parallel large 0.7) 1
      , bcompareWithin 0.4 0.8 "N1" $ bench "N2" $ nfAppIO (bench_parallel large 0.7) 2
      , bcompareWithin 0.2 0.6 "N1" $ bench "N4" $ nfAppIO (bench_parallel large 0.7) 4
      ]
  ]

prop_sample :: Graph -> Distance -> Property
prop_sample graph delta = property $ do
  let nodes = G.nodes graph
      order = G.order graph
  --
  src <- forAll $ Gen.element nodes
  dst <- forAll $ Gen.list (Range.linear 1 order) (Gen.element nodes)
  test_sssp graph delta src dst

prop_arbitrary :: Gen Graph -> Distance -> Property
prop_arbitrary graph delta = property $ do
  g     <- forAll $ graph
  let order = G.order g
  src   <- forAll $ Gen.int (Range.constant 0 (order-1))
  dst   <- forAll $ Gen.list (Range.linear 1 order) (Gen.int (Range.constant 0 (order-1)))
  test_sssp g delta src dst

test_sssp :: Graph -> Distance -> Node -> [Node] -> PropertyT IO ()
test_sssp graph delta src dst = do
  sp  <- evalIO $ delta_stepping graph delta src
  let actual   = map (\d -> let x = sp V.! d in if isInfinite x then Nothing else Just x) dst
      expected = map (\d -> G.spLength src d graph) dst
  actual ~~~ expected

bench_parallel :: Graph -> Distance -> Int -> IO (Vector Distance)
bench_parallel graph delta n = do
  setNumCapabilities n
  delta_stepping graph delta 0

large :: Graph
large =
  let g = Gen.arbitrary (Range.singleton 4096) (Range.singleton 16384) Gen.enumerated Gen.distance
   in case Gen.evalGen 99 (Seed.from 42) g of
        Nothing -> error "Could not generate sample graph"
        Just x  -> Tree.treeValue x

