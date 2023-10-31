{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE OverloadedLists  #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
--
-- INFOB3CC Concurrency
-- Practical 2: Single Source Shortest Path
--
--    Δ-stepping: A parallelisable shortest path algorithm
--    https://www.sciencedirect.com/science/article/pii/S0196677403000762
--
-- http://www.cs.uu.nl/docs/vakken/b3cc/assessment.html
--
-- https://cs.iupui.edu/~fgsong/LearnHPC/sssp/deltaStep.html
--
-- TODO: DELETE
-- TODO: delete all commented out prints
-- https://hackage.haskell.org/package/fgl-5.8.0.0/docs/Data-Graph-Inductive-Graph.html
-- https://hackage.haskell.org/package/containers-0.6.6/docs/Data-IntMap-Internal.html
-- https://hackage.haskell.org/package/vector-0.13.0.0/docs/Data-Vector-Storable-Mutable.html
-- https://hackage.haskell.org/package/vector-0.13.0.0/docs/Data-Vector-Mutable.html

module DeltaStepping (

  Graph, Node, Distance,
  delta_stepping,
  -- TODO: Delete the following exports:
  initialise, allBucketsEmpty, Buckets (bucket_array), findRequests, relax, relaxRequests,
  printCurrentBucket, printCurrentState, printBucket

) where

import Sample

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Data.Bits
import Data.Graph.Inductive                                         ( Gr )
import Data.IORef
import Data.IntMap.Strict                                           ( IntMap )
import Data.IntSet                                                  ( IntSet )
import Data.Vector.Storable                                         ( Vector )
import Data.Word
import Foreign.Ptr
import Foreign.Storable
import Text.Printf
import qualified Data.Graph.Inductive                               as G
import qualified Data.IntMap.Strict                                 as Map
import qualified Data.IntSet                                        as Set
import qualified Data.Vector.Mutable                                as V
import qualified Data.Vector.Storable                               as M ( unsafeFreeze )
import qualified Data.Vector.Storable.Mutable                       as M
import Data.List.Split ( chunksOf )

type Graph    = Gr String Distance  -- Graphs have nodes labelled with Strings and edges labelled with their distance
type Node     = Int                 -- Nodes (vertices) in the graph are integers in the range [0..]
type Distance = Float               -- Distances between nodes are (positive) floating point values


-- | Find the length of the shortest path from the given node to all other nodes
-- in the graph. If the destination is not reachable from the starting node the
-- distance is 'Infinity'.
--
-- Nodes must be numbered [0..]
--
-- Negative edge weights are not supported.
--
-- NOTE: The type of the 'delta_stepping' function should not change (since that
-- is what the test suite expects), but you are free to change the types of all
-- other functions and data structures in this module as you require.
--
delta_stepping
    :: Graph                            -- graph to analyse
    -> Distance                         -- delta (step width / bucket width)
    -> Node                             -- index of the starting node
    -> IO (Vector Distance)
delta_stepping graph delta source = do
  num_threads   <- getNumCapabilities             -- the number of (kernel) threads to use: the 'x' in '+RTS -Nx'
  (buck, tent)  <- initialise graph delta source

  -- tent is already filled with infinites at this point
  relax buck tent delta (source, 0)
  
  -- deltaStep repeats until all buckets are empty
  deltaStep buck tent num_threads

  -- Ait we good
  M.unsafeFreeze tent

  where
    deltaStep :: Buckets -> TentativeDistances -> Int -> IO ()
    deltaStep buck tent  threads = do
      weGood <- allBucketsEmpty buck
      unless weGood $ do --unless all buckets are empty, do this. 
        step graph delta buck tent threads
        deltaStep buck tent threads


-- Initialise algorithm state
--
initialise
      :: PrintfArg a
      => Gr a Distance
      -> Distance
      -> Node
      -> IO (Buckets, TentativeDistances)
initialise graph delta source = do
  -- Generate buckets
  let noOfBuckets = 200 -- Yet to be determined. A cyclic array of buckets with l = longest edge (/delta?) should be implemented

  firstBucketIndex <- newIORef 0
  bucketArray <- V.replicate noOfBuckets Set.empty

  let buckets = Buckets firstBucketIndex bucketArray

  -- Generate tents
  tentDistances <- M.replicate (G.noNodes graph) (1/0) -- 1/0 == infinite

  return (buckets, tentDistances)

-- Take a single step of the algorithm
-- 
step :: Graph
     -> Distance
     -> Buckets
     -> TentativeDistances
     -> Int
     -> IO ()
step graph delta buck@(Buckets i arr) tent threads = do
  --first non-empty bucket
  smallestBuckIndex <- findNextBucket buck
  --update index 
  writeIORef i smallestBuckIndex

  r <- innerWhile graph delta buck tent (Set.empty) threads

  --heavy requests
  req <- findRequests (>= delta) graph r tent
  relaxRequests buck tent delta req threads -- rustig aan swa

--this function is the inner-while loop from the pseudocode from the paper
innerWhile :: Graph -> Distance -> Buckets -> TentativeDistances -> IntSet -> Int -> IO (IntSet)
innerWhile graph delta buck@(Buckets i arr) tent toDelete threads = do
  bucketIndex <- readIORef i
  currentIntSet <- V.read arr bucketIndex
  
  if (not $ Set.null currentIntSet)
    then do
      --these are the light requests
      req <- findRequests (< delta) graph currentIntSet tent

      --union of previous deleted and current deleted to make new list of deleted
      let onion = Set.union toDelete currentIntSet --the union
    
      V.write arr bucketIndex (Set.empty) --make current bucket empty
      
      relaxRequests buck tent delta req threads -- rustig aan swa
      
      innerWhile graph delta buck tent onion threads --recursion go brrr
    else do
      return toDelete --end recursion

-- Once all buckets are empty, the tentative distances are finalised and the
-- algorithm terminates.
--
allBucketsEmpty :: Buckets -> IO Bool
allBucketsEmpty (Buckets i arr) = readIORef i >>= \x -> go $ V.drop x arr
  where
    go :: V.IOVector IntSet -> IO Bool
    go v | V.null v  = return True -- We checked everything
         | otherwise = do
             h <- V.read v 0 -- Give head
             if Set.null h
               then go $ V.drop 1 v
               else return False 

-- Return the index of the smallest non-empty bucket. Assumes that there is at
-- least one non-empty bucket remaining.
--
findNextBucket :: Buckets -> IO Int
findNextBucket bucks = do
  findIndex bucks 0
    where 
      findIndex :: Buckets -> Int -> IO Int
      findIndex buck@(Buckets fi bucktits) index = do
        currentBucket <- V.read bucktits index
        --predicates for the if-statement, i find this more readable
        let bucketLength = V.length bucktits - 1 --the -1 is necessary to avoid recursion in nono-zone
        let setEmpty = Set.null currentBucket --check if current is empty, if yes we wanna go brr in recursion
        let noNegativeBuckets = index >= 0 --assignment says we cannot have negative buckets :)
        if setEmpty && index < bucketLength && noNegativeBuckets
          then do findIndex buck (index + 1)
          else return index

-- Create requests of (node, distance) pairs that fulfil the given predicate
--
findRequests
    :: (Distance -> Bool)
    -> Gr a Distance
    -> IntSet
    -> TentativeDistances
    -> IO (IntMap Distance)
findRequests p graph v' tent = do
  let bucketContents = Set.elems v'
  let filteredLEdges = filter
        (\(from, _, d) -> p d && from `elem` bucketContents) -- Originates from bucket node and matches predicate
        (G.labEdges graph)
  requests <- mapM createRequest filteredLEdges
  return (Map.unionsWith min requests)
    where
      --actually make the requests
      createRequest :: G.LEdge Float -> IO (IntMap Distance)
      createRequest (v, w, c) = do
        tentativeDistanceV <- M.read tent v
        return (Map.singleton w (tentativeDistanceV + c))

-- Execute requests for each of the given (node, distance) pairs
--
relaxRequests
    :: Buckets
    -> TentativeDistances
    -> Distance
    -> IntMap Distance
    -> Int
    -> IO ()
relaxRequests buckets tent delta req threads =
  let requests = Map.toAscList req
      requestChunks = chunksOf ((length requests `div` threads) + 1) requests
        -- Divides requests over all threads
   in do
    -- MULTITHREADING GO BRRRRRRRRRR
    -- MISTER WHITE WE GOT MULTITHREADING BITCH
    forkThreads (length requestChunks)
      (\i -> mapM_ (relax buckets tent delta) (requestChunks !! i))


-- Execute a single relaxation, moving the given node to the appropriate bucket
-- as necessary
--
relax :: Buckets
      -> TentativeDistances
      -> Distance
      -> (Node, Distance)
      -> IO ()
relax buckets tent delta (w, x) = do
  let bs = bucket_array buckets
  tentW <- M.read tent w
  if x >= tentW then return () else do
    unless (isInfinite tentW) $ --infinity not good no
      V.modify bs (Set.delete w) (round $ tentW / delta) -- Remove from old bucket
    V.modify bs (Set.insert w) (round $     x / delta) -- Add to new bucket
    M.write tent w x

-- -----------------------------------------------------------------------------
-- Starting framework
-- -----------------------------------------------------------------------------
--
-- Here are a collection of (data)types and utility functions that you can use.
-- You are free to change these as necessary.
--

type TentativeDistances = M.IOVector Distance

data Buckets = Buckets
  { first_index   :: {-# UNPACK #-} !(IORef Int)                -- real index of the first bucket (j)
  , bucket_array  :: {-# UNPACK #-} !(V.IOVector IntSet)        -- non!-cyclic array of buckets
  }


-- Forks 'n' threads. Waits until those threads have finished. Each thread
-- runs the supplied function given its thread ID in the range [0..n).
--
forkThreads :: Int -> (Int -> IO ()) -> IO ()
forkThreads n action = do
  -- Fork the threads and create a list of the MVars which per thread tell
  -- whether the action has finished.
  finishVars <- mapM @[] work [0 .. n - 1]
  -- Once all the worker threads have been launched, now wait for them all to
  -- finish by blocking on their signal MVars.
  mapM_ takeMVar finishVars
  where
    -- Create a new empty MVar that is shared between the main (spawning) thread
    -- and the worker (child) thread. The main thread returns immediately after
    -- spawning the worker thread. Once the child thread has finished executing
    -- the given action, it fills in the MVar to signal to the calling thread
    -- that it has completed.
    --
    work :: Int -> IO (MVar ())
    work index = do
      done <- newEmptyMVar
      _    <- forkOn index (action index >> putMVar done ())  -- pin the worker to a given CPU core
      return done


-- Print the current state of the algorithm (tentative distance to all nodes)
--
printCurrentState
    :: PrintfArg a
    => Gr a Distance
    -> TentativeDistances
    -> IO ()
printCurrentState graph tent = do
  printf "  Node  |  Label  |  Tent\n"
  printf "--------+---------+--------\n"
  forM_ (G.labNodes graph) $ \(v, l) -> do
    x <- M.read tent v
    if isInfinite x
       then printf "  %4d  |  %5v  |  -\n" v l
       else printf "  %4d  |  %5v  |  %f\n" v l x
  --
  printf "\n"

-- Print the current bucket
--
printCurrentBucket
    :: PrintfArg a
    => Gr a Distance
    -> Distance
    -> Buckets
    -> TentativeDistances
    -> IO ()
printCurrentBucket graph delta Buckets{..} tent = do
  j <- readIORef first_index
  b <- V.read bucket_array (j `rem` V.length bucket_array)
  printf "Bucket %d: [%f, %f)\n" j (fromIntegral j * delta) (fromIntegral (j+1) * delta)
  printBucket graph b tent

-- Print a given bucket
--
printBucket
    :: PrintfArg a
    => Gr a Distance
    -> IntSet
    -> TentativeDistances
    -> IO ()
printBucket graph bucket tent = do
  printf "  Node  |  Label  |  Tent\n"
  printf "--------+---------+--------\n"
  forM_ (Set.toAscList bucket) $ \v -> do
    let ml = G.lab graph v
    x <- M.read tent v
    case ml of
      Nothing -> printf "  %4d  |   -   |  %f\n" v x
      Just l  -> printf "  %4d  |  %5v  |  %f\n" v l x
  --
  printf "\n"

