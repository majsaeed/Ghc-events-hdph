
-- 3 May 2012
-- Majed Al Saeed

--module Analysis
module AnalysisGhci 
 (showStrEvt, 
  showEventLog,
  getLogEvents,
  getHdpHEvents,
  searchForPairOf,
  findMaxSchedId,
  schedEnterGlobData,
  countSchedDuration,
  totaConfDuration,
  countGropDuration,
  groupOccurence,
  maxDuration, )  where

--import Testdata
--import RegData
--import RegData2
--import RegData3
import HdpHEventTypesGhci
--import HdpHEventTypes
--import HdpH.HdpHEventTypes
import GHC.RTS.Events
import Data.List 
import Data.Word (Word16, Word64)
import qualified Data.Set (fromList, size)
import Text.Printf 


showStrEvt :: EventLog -> IO ()
showStrEvt  (EventLog h (Data d)) = do
       let t = strEvnTime (Data d)
           e = HdpHEvent t HdpHStartup
       putStrLn (show e)
               

showEventLog :: EventLog -> IO ()
--showEventLog  e@(EventLog h (Data d)) = do putStrLn (show (e))
--showEventLog  (EventLog h (Data d)) = do putStrLn (show (Data d))
--showEventLog  e@(EventLog h (Data d)) = do putStrLn (show (sort(concat ( sparkPoolEvents (Data d)))))
showEventLog  e@(EventLog h (Data d)) = do 
       let log_events = (sort( getLogEvents (Data d)))
           --s_events = (sort(concat ( sparkPoolEvents (Data d))))
           hdph_events = getHdpHEvents log_events
           --h_events = getHdpHEvents s_events
       --putStrLn (show log_events)
       putStrLn (show hdph_events)




-- Count the max duration of a conflict

maxDuration :: Word64 -> [[HdpHEvent]] -> Word64
maxDuration maxdur [] = maxdur
maxDuration maxdur (y@(x:x':xs):[])
 | ((e_time x') - (e_time x)) > maxdur =
   maxDuration  ((e_time x') - (e_time x)) []
 | otherwise =  maxDuration maxdur []
maxDuration maxdur (y@(x:x':xs):ys)
 |((e_time x') - (e_time x)) > maxdur =
   maxDuration  ((e_time x') - (e_time x)) ys
 | otherwise =  maxDuration maxdur ys



-- This function count the total  duration for a particuler scheduler
-- involved in  a conflict or causing  waiting
countSchedDuration :: Word16 -> Word16-> [[HdpHEvent]]-> [(Word16, Word64)] 
countSchedDuration maxsid sid []
    | sid < maxsid =  (sid, 0): countSchedDuration maxsid (sid + 1) []
    | otherwise =  (sid, 0): []
countSchedDuration maxsid sid l@(y@(x:x':_):ys) =
 let  dur = count 0 sid l
 in if sid < maxsid
    then  (sid, dur): countSchedDuration maxsid (sid + 1) l
    else (sid, dur): []
 where count dur sid [] = dur
       count dur sid  (y@(x:x':_):ys)
        | scheduleId (e_spec x) == sid =
          count (dur + ((e_time x') - (e_time x))) sid ys
        | otherwise = count dur sid ys


-- this function returns the total conflict duration for all schedulers
totaConfDuration :: Word64 -> [(Word16, Word64)] -> Word64
totaConfDuration dur [] = dur
totaConfDuration dur ((xsched, xdur):xs) = totaConfDuration (dur + xdur) xs


-- this function count the total duration for conflicts with a particuler 
-- number of schedulers 
countGropDuration :: Word16 -> [[HdpHEvent]]-> [(Word16, Word64)]
countGropDuration maxsid [] 
 | maxsid > 2 = (maxsid, 0): countGropDuration (maxsid -1) []
 | otherwise = (maxsid, 0) : []
countGropDuration maxsid  l@(y@(x:x':_):ys) =
 let dur = count maxsid 0 l 
 in if maxsid > 2
    then (maxsid, dur): countGropDuration (maxsid -1) l
    else (maxsid, dur): []
  where -- count :: Word16 -> Word64 -> [[HdphEvent]] -> Word64
       count maxsid dur [] = dur
       count maxsid dur (y@(x:x':_):ys)
        | countSchedulerId y == maxsid = 
          count maxsid (dur + ((e_time x') - (e_time x))) ys
        | otherwise = count maxsid dur ys



-- to fin the maximum number of schedulers in HdpHEvent list
findMaxSchedId :: Word16 -> [HdpHEvent] -> Word16
findMaxSchedId max [] = max
findMaxSchedId max (x:xs)
 | scheduleId (e_spec x) > max = findMaxSchedId (scheduleId (e_spec x)) xs
 | otherwise =  findMaxSchedId max xs

 

-- this function takes a list of pair lists e.g. r conflict and
-- return list of tubles with number of schedulers and how many times
-- this number of scheduler has been in a  conflict.
groupOccurence :: Word16 -> Int -> [[HdpHEvent]]-> [(Word16,Int)]
groupOccurence max_sched acc [] 
 | max_sched > 2 = (max_sched, acc): groupOccurence (max_sched - 1) 0 []
 | otherwise =  (max_sched, acc):[]
groupOccurence max_sched acc l@(x:xs) =
 let (a,b) = count max_sched 0 l
 in if max_sched < 2
    then []
    else (a,b) :  groupOccurence (max_sched -1 ) 0 l
 where   
  count :: Word16 -> Int -> [[HdpHEvent]]-> (Word16,Int)
  count schedulers acc [] = (schedulers, acc)
  count schedulers acc (y:ys)
   |  countSchedulerId y == schedulers =
      count schedulers (acc + 1) ys
   |  otherwise = count schedulers acc  ys


countSchedulerId :: [HdpHEvent]-> Word16
countSchedulerId x = 
 fromIntegral $ Data.Set.size $ Data.Set.fromList $ projectSchedulerId x
 where
 projectSchedulerId :: [HdpHEvent] -> [SchedulerId]
 projectSchedulerId [] =[]
 projectSchedulerId (x:xs) = 
  case  e_spec x  of 
      SparkCreated id -> id: projectSchedulerId xs 
      ConvertSpark id -> id:  projectSchedulerId xs
      NothingToSpark id -> id : projectSchedulerId xs
      EnterSparkPool id -> id : projectSchedulerId xs
      PutSpark id -> id : projectSchedulerId xs
      GlobaliseGRef id -> id : projectSchedulerId xs
      FreeGRef id -> id : projectSchedulerId xs
      DereferenceGRef id -> id : projectSchedulerId xs
      _ -> projectSchedulerId xs



-- This function should take an HdpHEvent  and a list of HdpHEvent
-- and search the list for the next HdpHEvent in the list with the same
-- schedule ID and pairs the two events into a list
searchForPairOf :: HdpHEvent -> [HdpHEvent] -> [HdpHEvent]
searchForPairOf e (y:ys) = 
     let sid1 = scheduleId (e_spec e)
         sid2 = scheduleId (e_spec y)
     in if sid1 == sid2 then pair e y
        else searchForPairOf e ys

-- Pair two HdpHEvents inot a list of two elements
pair :: HdpHEvent -> HdpHEvent-> [HdpHEvent]
pair x z = x : z :[]


-- This function count how many time a scheduler has entered the global data
-- structure or it can be used to count how many time a scheduler involved 
-- in conflict accessing the global data structuer

schedEnterGlobData :: Word16 -> Word16 ->[[HdpHEvent]]-> [(Word16, Int)]
schedEnterGlobData maxsid sid []=
    if sid < maxsid
    then (sid, 0) : schedEnterGlobData maxsid (sid + 1) [] 
    else (sid, 0) :[]

schedEnterGlobData maxsid sid  l@(y@(x:x':_):ys) = 
 let acc = count 0 sid l --ys
 in if sid < maxsid
    then (sid, acc) : schedEnterGlobData maxsid (sid + 1) l 
    else (sid, acc) :[]
 where
  count acc sid [] = acc
  count acc sid (y@(x:x':_):ys) 
    | scheduleId (e_spec x) == sid = count (acc + 1) sid ys
    | otherwise = count acc  sid ys


-- [Event] at this stage is a list of GHC UserMessage events only, this
-- function will extract all the HdpHEvents which are encapsulated insid
-- UserMessage events and return a list of HdpHEvent
getHdpHEvents :: [Event]-> [HdpHEvent]
getHdpHEvents []=[]
getHdpHEvents (x:xs) = 
    let evt = liftEvent x
    in case  e_spec evt of
       NotHdpHEvent ->  getHdpHEvents xs
       _            ->  evt :  getHdpHEvents xs

-- this function lift an Event of UserMessage type to an HdpHEvent, HdpHEvents 
-- are encapsulated inside GHC UserMessage events 

liftEvent :: Event -> HdpHEvent
liftEvent e = let
  eventtime = time e
  eventspec =  isHdpHEventInfo (reads (msg $ spec $ e) :: [(HdpHEventInfo, String)]) 
  in HdpHEvent {e_time = eventtime , e_spec = eventspec}
--  we need to check the message contints is an HdpHEventInfo
isHdpHEventInfo ::  [(HdpHEventInfo, String)] -> HdpHEventInfo
isHdpHEventInfo []=  NotHdpHEvent
isHdpHEventInfo [(x, y)] = x

-- Concat and sort the events extracted from the EventLog and return them 
-- into a list of Event [Event]
getLogEvents :: Data -> [Event]
getLogEvents a = sort ( concat (extractLogEvents a))

-- Extract events from the eventlog and put them into a list of list of event
extractLogEvents :: Data -> [[Event]]
extractLogEvents d@(Data (x:xs)) =  extractBlockEvents x : extractLogEvents (Data xs)
extractLogEvents (Data []) = []

-- Extract the  events form an event_block
-- Onaly UserMessages events will be extracted
extractBlockEvents :: Event -> [Event]
extractBlockEvents x = 
     let bk_events = block_events (spec x)
         userMsgEvents = userMessageEvents bk_events 
     in  userMsgEvents
           
userMessageEvents :: [Event]-> [Event]
userMessageEvents [] = []
userMessageEvents (e:es) 
  | isUserMsgEvent ( spec e) =  e : userMessageEvents es
  | otherwise = userMessageEvents es
 

-- Test if an event is a UserMessage event or not
isUserMsgEvent (UserMessage ms) = True
isUserMsgEvent _ = False 

-- To get the HdpH Start-up time
strEvnTime :: Data -> Word64
strEvnTime (Data xs) = getTime xs

getTime [] = 0
getTime (t:ts) | processBlock t > 0 = processBlock t
               | otherwise = getTime ts 

processBlock t =
      let b_events = block_events (spec t)                                         
          st = userM b_events
                 where userM (e:es) | isUserMsgEvent (spec e) &&  "STARTUP" `isInfixOf` msg (spec e) = time e
                                    | otherwise = userM es
                       userM _ = 0
      in  st




