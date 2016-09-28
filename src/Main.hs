-- {-# LANGUAGE BangPatterns #-}

module Main where

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

import System.Random
import Data.Time

type WTime = Double
type TimingDataBlock = (WTime, WTime, WTime, WTime)

type TimingData = V.Vector TimingDataBlock

main :: IO ()
main = do
  -- Uncomment these lines to create a db in first run
  -- d <- createRandomData
  -- writeFile "data.db" (show d)

  str <- readFile "data.db"
  let d = read str
      bigd = V.concat (replicate 1000 d)

  -- This should evaluate the whole 'bigd'
  print $ addTimingData bigd

  -- Profile just the average API
  startTime <- getCurrentTime
  print $ averageTimingData bigd
  endTime <- getCurrentTime

  print $ diffUTCTime endTime startTime

-- Add all the data, useful just to evaluate the input completely
addTimingData :: TimingData -> TimingDataBlock
addTimingData timingData = addTuple addBins
  -- trace ("Timing data " ++ show accVec)
  where
    -- Split the work and then converge
    bins :: [TimingData]
    bins = binTimingData 20 timingData

    -- This step can be done in parallel
    addBins :: [TimingDataBlock]
    addBins = fmap addTuple bins

addTuple :: (Foldable f) => f TimingDataBlock -> TimingDataBlock
addTuple = foldl1
  (\(a1,b1,c1,d1) (a2,b2,c2,d2) -> (a1+a2,b1+b2,c1+c2,d1+d2))

-- Take average of bins of data
averageTimingData :: TimingData -> [TimingDataBlock]
averageTimingData timingData = map (applyF (/(fromIntegral len))) addBins
  where
    -- Split the work and then converge
    bins :: [TimingData]
    bins = binTimingData 10 timingData

    len = length (head bins)

    -- This step can be done in parallel
    addBins :: [TimingDataBlock]
    addBins = fmap addTuple bins

    applyF :: (WTime -> WTime) -> TimingDataBlock -> TimingDataBlock
    applyF f (a1,b1,c1,d1) = (f a1, f b1, f c1, f d1)

-- Split the timing data in intervals
binTimingData :: Int -> V.Vector a -> [V.Vector a]
binTimingData binCount td = bin td
  where
    splitLength = ceiling (fromIntegral (length td)/
                        fromIntegral binCount)
    bin :: V.Vector a -> [V.Vector a]
    bin i 
      | V.null i = []
      | otherwise = f : bin r
      where (f,r) = V.splitAt splitLength i

createRandomData :: IO TimingData
createRandomData = V.replicateM 1000 tdb
  where rgen = randomRIO (0,1000.0)
        tdb :: IO TimingDataBlock
        tdb = (,,,) <$> rgen <*> rgen <*> rgen <*> rgen
