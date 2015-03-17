--module Utilities
module UtilitiesGhci
(  ns2s, ns2ms,  ns2mics,  percentage,  percentageFloat, meanDuration
)
where

import Data.Word (Word64)


-- Convert nanoseconds to seconds (na to s)
ns2s :: Word64 -> Float
ns2s nstime = 0.001 * (ns2ms nstime)

--Convert nanoseconds to miliseconds (ns to ms)
ns2ms :: Word64 -> Float
ns2ms nstime = (fromIntegral nstime) * 10^^(-6)

--Convert nanoseconds to microseconds (ns to μs)
ns2mics :: Word64 -> Float
ns2mics nstime = (fromIntegral nstime) * 10^^(-3)

-- calculate the percentage between two numbers
percentage :: Int -> Int -> Float
percentage first second =
   ((fromIntegral second)/(fromIntegral first) ) * 100

-- calculate the percentage between two numbers
percentageFloat :: Float -> Float -> Float
percentageFloat first second =
   (second/ first ) * 100


--TODO | should be removed, use percentage instead.
-- calculate the conflict percentage 
confPerc :: Int -> Int -> Float
confPerc enters conflicts =  
 ((fromIntegral conflicts)/ (fromIntegral enters)) * 100

-- calculate the mean duration
meanDuration :: Float -> Int -> Float
meanDuration sum_dur times 
 | times > 0 && sum_dur > 0
   = sum_dur / (fromIntegral times)
 | otherwise = 0.0

