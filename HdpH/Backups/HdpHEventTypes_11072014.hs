
-- 17 May 2012
-- Majed Al Saeed

{-# OPTIONS_GHC -funbox-strict-fields #-}
--module HdpHEventTypes where
module HdpH.HdpHEventTypes where

import GHC.RTS.Events
import Data.List 
import Data.Word (Word16, Word64)


--HdpHEvent
--type EventDescription = String
--type Timestamp = Word64
type SchedulerId = Word16


data HdpHData = HdpHData {
   hevents :: [HdpHEvent]
   }
   


data HdpHEvent = 
   HdpHEvent {
     e_time :: {-# UNPACK #-}! Timestamp,
     e_spec :: HdpHEventInfo
   } deriving (Show, Eq, Ord)


data HdpHEventInfo 
   -- sparks
   = SparkCreated     {  scheduleId ::  {-# UNPACK #-}! SchedulerId
                      }
   | ConvertSpark     {  scheduleId ::  {-# UNPACK #-}! SchedulerId
                      }
   | NothingToSpark   {  scheduleId ::  {-# UNPACK #-}! SchedulerId
                      }
   | EnterSparkPool   {  scheduleId ::  {-# UNPACK #-}! SchedulerId
                      }
   | PutSpark         {  scheduleId ::  {-# UNPACK #-}! SchedulerId

   -- Globale References
                      }
   | GlobaliseGRef    {  scheduleId ::  {-# UNPACK #-}! SchedulerId
                      }
   | GRefGlobalised   {  scheduleId ::  {-# UNPACK #-}! SchedulerId
                      }
   | FreeGRef         {  scheduleId ::  {-# UNPACK #-}! SchedulerId
                      }
   | GRefFreed        {  scheduleId ::  {-# UNPACK #-}! SchedulerId
                      }
   | FreeGRefNow      {  scheduleId ::  {-# UNPACK #-}! SchedulerId
                      }
   | GRefFreedNow     {  scheduleId ::  {-# UNPACK #-}! SchedulerId
                      }
   | DereferenceGRef  {  scheduleId ::  {-# UNPACK #-}! SchedulerId
                      }
   | DeadGRef         {  scheduleId ::  {-# UNPACK #-}! SchedulerId
                      }
   | GRefDereferenced {  scheduleId ::  {-# UNPACK #-}! SchedulerId
                      }
   | HdpHStartup      {}
   | HdpHShutdown     {}
   | NotHdpHEvent     {}                    

   deriving (Show, Eq, Ord, Read)



-- Hdphes = HdpHEvent 12336 (CSpark 2)

-- hdata = [HdpHEvent 12336 (CreateSpark 2),
--          HdpHEvent 12332 (CreateSpark 1),
--          HdpHEvent 12334 (ConvertSpark 3),
--          HdpHEvent 12335 (NothingToSpark 4),
--          HdpHEvent 12355 (EnterSparkPool 4),
--          HdpHEvent 00120 HdpHStartup,
--          HdpHEvent 12337 HdpHShutdown,
--          HdpHEvent 00000 NotHdpHEvent
--         ]
-- note = HdpHEvent 00000 NotHdpHEvent

