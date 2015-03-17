{-# OPTIONS_GHC -funbox-strict-fields #-}

module HdpH.Synch (synchEventLogs) where

import GHC.RTS.Events
import Data.List 
import Data.Word (Word64)



synchEventLogs :: EventLog -> EventLog
synchEventLogs (EventLog h d) = EventLog h (synchData d)


synchData :: Data -> Data
synchData d@(Data (x:xs))  = 
        let sv = synchValue d
            evts = syn sv (x:xs)
                where syn s (y:ys) = Event (time y + sv )(synchBlock s (spec y)) : syn s ys
                      syn s [] = []
            
            in Data evts
-------------------------
-- calculate the synch value for an eventlog
synchValue :: Data -> Word64
synchValue d =  1500000000 - ( strEvnTime d)


-- Test if an event is a UserMessage event or not
isUserMsgEvent (UserMessage ms) = True
isUserMsgEvent _ = False 

strEvnTime :: Data -> Word64
strEvnTime (Data xs) = getTime xs

getTime (t:ts) | processBlock t > 0 = processBlock t
               | otherwise = getTime ts 

processBlock t =
      let b_events = block_events (spec t)                                         
          st = userM b_events
                 where userM (e:es) | isUserMsgEvent (spec e) &&  "StartupRTS" `isInfixOf` msg (spec e) = time e
                                    | otherwise = userM es
                       userM _ = 0
      in  st

synchBlock :: Word64 -> EventInfo -> EventInfo
synchBlock sv b@(EventBlock t c ex) = 
       EventBlock   (t + sv)  c  (synchEvents sv (block_events b) )
synchBlock sv ei@_ = ei 

synchEvents :: Word64 -> [Event] -> [Event]
synchEvents s (e:ex) = Event (time e + s) (spec e) : synchEvents s ex
synchEvents s [] = []








