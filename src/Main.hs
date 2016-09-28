module Main where

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

import System.Random

type WTime = Double
type TimingDataBlock = (WTime, WTime, WTime, WTime)

type TimingData = V.Vector TimingDataBlock

main :: IO ()
main = do
  putStrLn "hello world"


addTimingData :: TimingData -> TimingDataBlock
addTimingData timingData = addTuple addBins
  -- trace ("Timing data " ++ show accVec)
  where
    -- Split the work and then converge
    bins :: [TimingData]
    bins = binTimingData 200 timingData

    addBins :: [TimingDataBlock]
    addBins = fmap addTuple bins

    addTuple :: (Foldable f) => f TimingDataBlock -> TimingDataBlock
    addTuple = foldl1
      (\(a1,b1,c1,d1) (a2,b2,c2,d2) -> (a1+a2,b1+b2,c1+c2,d1+d2))

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
createRandomData = V.replicateM 100000 tdb
  where rgen = randomRIO (0,1000.0)
        tdb :: IO TimingDataBlock
        tdb = (,,,) <$> rgen <*> rgen <*> rgen <*> rgen
