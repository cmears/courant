{-# LANGUAGE OverloadedStrings #-}
module Run where

import Control.Applicative
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy as BS
import qualified Data.Foldable as F
import Data.Maybe
import qualified Data.Vector as V
import Data.Time
import Data.Time.Clock.POSIX
import qualified Data.Traversable as T
import qualified Geo.Computations as GPS
import System.IO

type UUID = String

data Run = Run { runUUID :: UUID
               , runSegments :: V.Vector Segment }
  deriving (Show)

instance FromJSON Run where
  parseJSON (Object v) = do
    Array segmentsArray <- v .: "segments"
    segments <- V.mapM parseJSON segmentsArray
    uuid <- v .: "uuid"
    return $ Run uuid segments

instance ToJSON Run where
  toJSON run =
    object [ "uuid" .= runUUID run
           , "segments" .= runSegments run ]

data Segment = Segment { segmentSamples :: V.Vector Sample }
  deriving (Show)

instance FromJSON Segment where
  parseJSON (Object v) = do
    Array samplesArray <- v .: "samples"
    samples <- V.mapM parseJSON samplesArray
    return $ Segment samples

instance ToJSON Segment where
  toJSON segment =
    object [ "samples" .= segmentSamples segment ]

data Sample = Sample {
      sampleTime :: Integer
    , sampleLatitude :: Double
    , sampleLongitude :: Double
    , sampleAccuracy :: Maybe Double
    , sampleBearing :: Maybe Double
    , sampleSpeed :: Maybe Double
    , sampleAltitude :: Maybe Double
    }
  deriving (Show)

instance FromJSON Sample where
  parseJSON (Object v) =
    Sample <$> v .: "time"
           <*> v .: "latitude"
           <*> v .: "longitude"
           <*> v .:? "accuracy"
           <*> v .:? "bearing"
           <*> v .:? "speed"
           <*> v .:? "altitude"

instance ToJSON Sample where
  toJSON sample =
    object' [ Just $ "time" .= sampleTime sample
            , Just $ "latitude" .= sampleLatitude sample
            , Just $ "longitude" .= sampleLongitude sample
            , "accuracy" .? sampleAccuracy sample
            , "bearing" .? sampleBearing sample
            , "speed" .? sampleSpeed sample
            , "altitude" .? sampleAltitude sample
            ]

object' = object . catMaybes

key .? Nothing = Nothing
key .? Just x = Just $ key .= x
  

-- main :: IO ()
-- main = do
--   c <- last . filter (not . BS.null) . BS.split 0xa <$> BS.readFile "../test-run"
--   let j = eitherDecode c :: Either String Run
--   let Right run = j
--   let segs = V.map smoothSegment $ runSegments run
--   let samps = V.concatMap (\seg -> segmentSamples seg) segs

--   let seg' = Segment { segmentSamples = samps }
--   let run' = Run { runUUID = "abc"
--                  , runSegments = V.singleton seg' }
--   BS.putStr $ encode run'

--   -- forAdj_ samps $ \s1 s2 -> do
--   --   let d = goodDistance s1 s2
--   --       dtms = sampleTime s2 - sampleTime s1
--   --       t = avgTime s1 s2
--   --       s = (d / fromIntegral dtms) * 3600
--   --   putStrLn $ show t ++ " " ++ show s ++ " " ++ show d

--   -- V.forM samps $ \sample -> do
--   --   -- T.forM (sampleSpeed sample) $ \speed -> do
--   --     putStrLn $ show sample
--   return ()

forAdj_ :: Monad m => V.Vector a -> (a -> a -> m ()) -> m ()
forAdj_ v f = do
  let xs = V.toList v
  forM_ (zip xs (tail xs)) $ \ (x1,x2) -> do
    f x1 x2

avgTime :: Sample -> Sample -> Integer
avgTime s1 s2 =
  (sampleTime s1 + sampleTime s2) `div` 2

avgLat :: Sample -> Sample -> Double
avgLat s1 s2 =
  (sampleLatitude s1 + sampleLatitude s2) / 2

avgLon :: Sample -> Sample -> Double
avgLon s1 s2 =
  (sampleLongitude s1 + sampleLongitude s2) / 2


-- this doesn't do what we want
smoothSegment2 :: Segment -> Segment
smoothSegment2 seg =
  let trail = map sampleToPoint (V.toList (segmentSamples seg))
      trail2 = GPS.smoothRests trail
  in seg { segmentSamples = V.fromList (map pointToSample trail2) }

smoothRun :: Run -> Run
smoothRun run = run { runSegments = V.map smoothSegment (runSegments run) }

smoothSegment :: Segment -> Segment
smoothSegment seg =
  seg { segmentSamples = V.fromList (smoothSamples (V.toList (segmentSamples seg))) }

smoothSamples :: [Sample] -> [Sample]
smoothSamples [] = []
smoothSamples [s] = [s]
smoothSamples (s:ss) =
  -- Take the next ten seconds.
  let (thisBucket, rest) = span (\s2 -> sampleTime s2 - sampleTime s < 10000) (s:ss)
      bucketDistance = goodDistance s (last thisBucket)
      bucketTime = sampleTime (last thisBucket) - sampleTime s
      bucketSpeed = bucketDistance * 3600 / fromIntegral bucketTime
      bucketSample = Sample { sampleTime = avgTime s (last thisBucket)
                            , sampleLatitude = avgLat s (last thisBucket)
                            , sampleLongitude = avgLon s (last thisBucket)
                            , sampleAccuracy = Nothing
                            , sampleBearing = Nothing
                            , sampleSpeed = Just bucketSpeed
                            , sampleAltitude = Nothing
                            }
  in bucketSample : smoothSamples rest

crapDistance :: Sample -> Sample -> Double
crapDistance s1 s2 =
  sqrt $ (sampleLatitude s1 - sampleLatitude s2)^2 +
         (sampleLongitude s1 - sampleLongitude s2)^2

goodDistance :: Sample -> Sample -> Double
goodDistance s1 s2 =
  let p1 = sampleToPoint s1
      p2 = sampleToPoint s2
  in GPS.distance p1 p2

sampleToPoint :: Sample -> GPS.Point
sampleToPoint s =
  GPS.Point { GPS.pntLat = sampleLatitude s
            , GPS.pntLon = sampleLongitude s
            , GPS.pntEle = sampleAltitude s
            , GPS.pntTime = Just $ epochMillisecondsToUTC (sampleTime s) }

pointToSample :: GPS.Point -> Sample
pointToSample p =
  Sample { sampleTime = utcToEpochMilliseconds (fromJust (GPS.pntTime p))
         , sampleLatitude = GPS.pntLat p
         , sampleLongitude = GPS.pntLon p
         , sampleAltitude = GPS.pntEle p
         , sampleBearing = Nothing
         , sampleSpeed = Nothing
         , sampleAccuracy = Nothing
         }

epochMillisecondsToUTC :: Integer -> UTCTime
epochMillisecondsToUTC ms =
  posixSecondsToUTCTime (realToFrac ms / 1000)

utcToEpochMilliseconds :: UTCTime -> Integer
utcToEpochMilliseconds utc =
  round $ 1000 * utcTimeToPOSIXSeconds utc
