
-- 3 May 2012
-- Majed Al Saeed

-- {-# OPTIONS_GHC -funbox-strict-fields #-}

module HdpH.Analysis 
 (showStrEvt, 
  showEventLog, 
  showContention,
  showRegContention
 ) where

--import Testdata
--import RegData
import RegData2
import RegData3
--import HdpH.HdpHEventTypes
import HdpHEventTypesGhci
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

showContention :: EventLog -> IO ()
showContention e@(EventLog h (Data d)) = do
  let log_events = (sort( getLogEvents (Data d)))
      hdph_events = getHdpHEvents log_events
      paired  = pairHdpHEvents hdph_events
      allConfs = conflictEvents  paired
      prodConfs= prodConfEvents allConfs
      total_entry = totalEnterSparkPool 0  hdph_events
      total_coverted = totalConvertSpark 0 hdph_events
      total_nothing = totalNothingToSpark 0 hdph_events
      maxSchedId = findMaxSchedId 0  hdph_events 
      enter_persi = schedEnterGlobData maxSchedId 1 paired
      sidConf = schedEnterGlobData maxSchedId 1  allConfs
      proSidConf = schedEnterGlobData  maxSchedId 1 prodConfs
      confDurSid = countSchedDuration maxSchedId 1 allConfs
      proConfDurSid = countSchedDuration maxSchedId  1 prodConfs
      --maxScInConf = maxSchedInConf 0  prodConfs 
      schConf = groupOccurence maxSchedId  0  prodConfs

  printf "------------------------------------------------------------------------------\n"
  printf "                                 HdpHprof\n" 
  printf "                           Contention analysis\n\n"

  putStr  "Total entry to sparkpool: "  
  putStrLn (show total_entry )
  putStr "Total sparks converted: " 
  putStrLn (show total_coverted)
  putStr "Total No spark to convert: "
  putStrLn (show total_nothing )
  putStrLn "------------------------------------------------------------------------------"
  printf "%-10s %-10s %-10s %-10s %-10s %-10s\n" "SID" "Enter" "All C.""Pro. C.""All.%" "Pro.%"    
  display1  enter_persi sidConf proSidConf
  printf "All times displayed are in microseconds \n"
  --putStrLn ("Max number of schedulers in a productive conflict: "++ (show (maxSchedInConf 0  prodConfs)))
  printf "%-10s %-10s %-10s %-10s %-10s \n" "SID""P.Conf.""C.Dur.""P.C.Dur""Mean dur C."
  display2 proSidConf confDurSid proConfDurSid
  putStr "Max duration  in a productive conflict: "
  printf "%07.5f\n" ( pc2mcs (maxDuration  0 prodConfs ))
  printf "-----------------------------------------------------------------------------\n"
  printf "Conflicts grouped by total number of schedulers involved \n"
  printf "%-10s %-10s\n" "No. Sid""No. Conf."
  display3 schConf
  printf "------------------------------------------------------------------------------\n"
  
-- -- to here
-- Don't change to printf or compilation error will occur.
display1 [] [] [] = putStrLn "------------------------------------------------------------------------------"

display1 ((a,b):[]) ((_,c):[]) ((_,d):[]) = do 
 let -- a  scheduler id
     -- b  total enters
     -- c  All conflicts
     -- d  productive conflicts
     allconf  =  confPerc  b c
     prodconf =  confPerc  b d
 printf "%-10d %-10d %-10d %-10d %-10.2f %-10.2f\n" a b c d allconf prodconf
 display1 [] [] []
display1 ((a,b):as) ((_,c):cs) ((_,d):ds)  = do 
 let -- a  scheduler id
     -- b  total enters
     -- c  All conflicts
     -- d  productive conflicts
     allconf  =  confPerc  b c
     prodconf =  confPerc  b d
 printf "%-10d %-10d %-10d %-10d %-10.2f %-10.2f\n" a b c d allconf prodconf
 display1 as cs ds 
  

display2 [] [] [] = do 
 putStrLn "" -- Don't change to printf or compilation error will occur.
display2 ((a,b):[]) ((_,c):[]) ((_,d):[])= do
 let c' = pc2mcs c        -- all conflict durtion
     d' = pc2mcs d        -- Productive conflict duration
     md = meanDuration d'  b -- mean duration of conflect
 printf "%-10d %-10d %-10.4f %-10.4f %-10.4f\n" a b c' d' md
 display2 [] [] [] 
display2 ((a,b):as) ((_,c):cs) ((_,d):ds) = do
 let c' = pc2mcs c        -- all conflict durtion
     d' = pc2mcs d        -- Productive conflict duration
     md = meanDuration d'  b -- mean duration of conflect
 printf "%-10d %-10d %-10.4f %-10.4f %-10.4f\n" a b c' d' md
 display2 as cs ds

display3 [] = do
 putStrLn ""
display3 ((a,b):[])= do 
 printf "%-10d %-10d \n" a b
 display3 []
display3 ((a,b):as)= do 
 printf "%-10d %-10d \n" a b
 display3 as
  
------------------------------------------------------------------------------
--                                   Utilities
------------------------------------------------------------------------------
-- Convert picoseconds to milliseconds (ps to ms)
pc2ms :: Word64 -> Float
pc2ms pc = (fromIntegral pc) * 10^^(-9)

--Convert picoseconds to microseconds (ps to μs)
pc2mcs :: Word64 -> Float
pc2mcs pc = (fromIntegral pc) * 10^^(-6)

-- calculate the percentage between two numbers
percentage :: Int -> Int -> Float
percentage first second =
   ((fromIntegral second)/(fromIntegral first) ) * 100

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


-- This function count the total conflict duration for a particuler scheduler
confSchedDuration :: Word16 -> Word16-> [[HdpHEvent]]-> [(Word16, Word64)] 
confSchedDuration maxsid sid []=
    if sid < maxsid
    then  (sid, 0): confSchedDuration maxsid (sid + 1) []
    else (sid, 0): []

confSchedDuration maxsid sid l@(y@(x:x':_):ys) =
 let  dur = countConfSchedDuration 0 sid l
 in if sid < maxsid
    then  (sid, dur): confSchedDuration maxsid (sid + 1) l
    else (sid, dur): []

countConfSchedDuration :: Word64 -> Word16 ->[[HdpHEvent]]-> Word64
countConfSchedDuration dur sid [] = dur
countConfSchedDuration dur sid  (y@(x:x':_):ys)=
 if scheduleId (e_spec x) == sid
 then countConfSchedDuration (dur + ((e_time x') - (e_time x))) sid ys
 else countConfSchedDuration dur sid ys

-- this to re-implement confSchedDuration when finished should be remove
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
          countConfSchedDuration (dur + ((e_time x') - (e_time x))) sid ys
        | otherwise = countConfSchedDuration dur sid ys


-- -- This function count how many time a scheduler has entered the saprkpool
-- -- or it can be used to count how many time a scheduler involved in conflict
-- schedESPool :: Word16 -> [[HdpHEvent]]-> [(Word16, Int)]
-- schedESPool sid []=[]
-- schedESPool sid  l@(y@(x:x':_):ys) = 
--  let maxsid = findMaxSid 0 l 
--      acc = countSchedESPool 0 sid l --ys
--  in if sid < maxsid
--     then (sid, acc) : schedESPool (sid + 1) l 
--     else (sid, acc) :[]

-- -- created 15-11-14
-- -- rename 
-- schedESPool2 :: Word16 -> Word16 ->[[HdpHEvent]]-> [(Word16, Int)]
-- schedESPool2 maxsid sid []=
--     if sid < maxsid
--     then (sid, 0) : schedESPool2 maxsid (sid + 1) [] 
--     else (sid, 0) :[]

-- schedESPool2 maxsid sid  l@(y@(x:x':_):ys) = 
--  let acc = countSchedESPool 0 sid l --ys
--  in if sid < maxsid
--     then (sid, acc) : schedESPool2 maxsid (sid + 1) l 
--     else (sid, acc) :[]

-- countSchedESPool :: Int -> Word16 -> [[HdpHEvent]]-> Int
-- countSchedESPool acc sid [] = acc
-- countSchedESPool acc sid (y@(x:x':_):ys) = -- cc
--  if scheduleId (e_spec x) == sid
--  then countSchedESPool (acc + 1) sid ys
--  else countSchedESPool acc  sid ys

-- -- Find the max scheduler id in the paired List
-- findMaxSid :: Word16 -> [[HdpHEvent]] -> Word16
-- findMaxSid max [] = max
-- findMaxSid max (y@(x:x':_):ys) =
--   let sid = scheduleId (e_spec x)
--   in if max <  sid
--   then findMaxSid sid ys
--   else findMaxSid max ys

-- versiton two of findMaxSchedId
-- to fin the maximum number of schedulers in HdpHEvent list
findMaxSchedId :: Word16 -> [HdpHEvent] -> Word16
findMaxSchedId max [] = max
findMaxSchedId max (x:xs)
 | scheduleId (e_spec x) > max = findMaxSchedId (scheduleId (e_spec x)) xs
 | otherwise =  findMaxSchedId max xs

  -- This function counts how many schedulers enter the spark pool after the
  -- current scheduler and before it exit the spark pool
countEnterSparkPool :: [[HdpHEvent]]-> [(Word16, Int)]
countEnterSparkPool []=[]
countEnterSparkPool (y@(x:x':_):ys) =  
   let sid = scheduleId(e_spec x)
       esp = e_time x
       xsp = e_time x'
       acc = enterSparkPool 0 esp xsp ys
   in (sid, acc): countEnterSparkPool ys

enterSparkPool :: Int -> Timestamp -> Timestamp -> [[HdpHEvent]]-> Int
enterSparkPool acc esp xsp [] = acc
enterSparkPool acc esp xsp (y@(x:x':_):ys) = 
   if (e_time x) < xsp && isEnterSparkPool x 
   then enterSparkPool (acc + 1) esp xsp ys
   else enterSparkPool acc esp xsp ys

-- This function counts how many schedulers enter affter the current scheduler
-- and get spark befor it can get one.
countConvertSpark ::  [[HdpHEvent]]-> [(Word16, Int)]
countConvertSpark []=[]
countConvertSpark (y@(x:x':_):ys) =
   let sid = scheduleId(e_spec x)
       esp = e_time x
       xsp = e_time x'
       acc = getConvertSpark 0 esp xsp ys
   in (sid, acc):countConvertSpark ys

getConvertSpark :: Int -> Timestamp -> Timestamp -> [[HdpHEvent]]-> Int 
getConvertSpark  acc esp xsp []= acc
getConvertSpark  acc esp xsp (y@(x:x':_):ys) =
 if (e_time x') < xsp && isConvertSpark x'
 then getConvertSpark (acc + 1) esp xsp ys
 else getConvertSpark acc esp xsp ys

-- to grup the ConvertSpark by scheduleId
groupPairs :: [(Word16, Int)] -> [(Word16, Int)]
groupPairs []=[]
groupPairs ((s,c):[]) = ((s,c):[])
--groupConvertSpark pairs@(y@(s,c):y'@(s',c'):ys) = 
groupPairs (y@(s,c):y'@(s',c'):ys) =
   if s == s' 
   then  groupPairs ((s,(c + c')):ys)
   else (s,c) : groupPairs ys

-- -- No. scheduler in a conflict or conflicts that has this number of schedulers
-- schedulersConflicts :: Int -> Int -> [[HdpHEvent]]-> [(Int,Int)]
-- schedulersConflicts maxscs acc [] = []
-- schedulersConflicts maxscs acc l@(x:xs) =
--  let sidconf = countSchedConf maxscs 0 l
--  in if maxscs < 2
--     then []
--     else 
--       if  checkSchCon sidconf  
--       then sidconf : schedulersConflicts (maxscs -1 ) 0 l
--       else schedulersConflicts (maxscs -1 ) 0 l
   
-- countSchedConf :: Int -> Int -> [[HdpHEvent]]-> (Int,Int)
-- countSchedConf schs acc [] = (schs, acc)
-- countSchedConf schs acc (y:[]) 
--  |  countSchedulerId y == schs =
--     countSchedConf schs (acc + 1) []
--  |  otherwise = countSchedConf schs acc  []
-- countSchedConf schs acc (y:ys)
--  |  countSchedulerId y == schs =
--     countSchedConf schs (acc + 1) ys
--  |  otherwise = countSchedConf schs acc  ys

-- checkSchCon :: (Int,Int) -> Bool
-- checkSchCon (a,b)= 
--  if (b > 0) && (a > 0)
--  then True
--  else False

-- re-implement schedulersConflicts
-- this function takes a list of pair lists e.g. waitng or conflict and
-- return list of tubles with number of schedulers and how many times
-- this number of scheduler has been in a waiting or a conflict.
groupOccurence :: Word16 -> Int -> [[HdpHEvent]]-> [(Word16,Int)]
groupOccurence max_sched acc [] = []
groupOccurence max_sched acc l@(x:xs) =
 let (a,b) = count max_sched 0 l
 in if max_sched < 2
    then []
    else (a,b) :  groupOccurence (max_sched -1 ) 0 l
 where   
  --count :: Word16 -> Int -> [[HdpHEvent]]-> (Word16,Int)
  count schedulers acc [] = (schedulers, acc)
  count schedulers acc (y:[]) 
   |  countSchedulerId y == schedulers =
      count schedulers (acc + 1) []
   |  otherwise = count schedulers acc  []
  count schedulers acc (y:ys)
   |  countSchedulerId y == schedulers =
      count schedulers (acc + 1) ys
   |  otherwise = count schedulers acc  ys



countSchedulerId :: [HdpHEvent]-> Word16
countSchedulerId x = 
 fromIntegral $ Data.Set.size $ Data.Set.fromList $ projectSchedulerId x
 where
 -- projectSchedulerId :: [HdpHEvent] -> [SchedulerId]
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



-- -- This function showes the max number of schedulers in conflict 
-- maxSchedInConf :: Int -> [[HdpHEvent]] -> Int
-- maxSchedInConf max [] =  div max 2
-- maxSchedInConf max (y@(x:x':xs):[])
--  | length y > max =
--    maxSchedInConf (length y)  []
--  | otherwise = maxSchedInConf max [] 
-- maxSchedInConf max (y@(x:x':xs):ys)
--  | length y > max =
--     maxSchedInConf (length y)  ys
--  | otherwise =  maxSchedInConf max  ys

-- This functiont to count productive conflict, basicly it taks the result of
-- conflictEvents and remove the non-productive events
prodConfEvents :: [[HdpHEvent]] -> [[HdpHEvent]]
prodConfEvents []=[]
prodConfEvents (y@(x:x':xs):[])
  | checkProdConflict (x':xs) == True = 
   y : []
 | otherwise =   prodConfEvents  []
prodConfEvents (y@(x:x':xs):ys) 
 | checkProdConflict (x':xs) == True = 
   y : prodConfEvents ys
 | otherwise = prodConfEvents ys

checkProdConflict :: [HdpHEvent]-> Bool
checkProdConflict [] = False
checkProdConflict (x:[])
 | isConvertSpark x = True
 | otherwise = checkProdConflict []
checkProdConflict (x:xs)
 | isConvertSpark x = True
 | otherwise = checkProdConflict xs

-- This function groub Conflict with a scheduler
conflictEvents :: [[HdpHEvent]] -> [[HdpHEvent]]
conflictEvents []=[]
conflictEvents (y@(x:x':_):[]) 
 | length y > 2 = y:[]
 | otherwise = []
conflictEvents (y@(x:x':_):y'@(z:z':_):ys) 
 | x < z &&  z' < x'  =
   conflictEvents ((y ++ y' ):ys)
 | length y > 2 =
   y : conflictEvents  (y':ys)
 | otherwise =  
   conflictEvents  (y':ys)


-- Takes a list of HdpHEvent and return a list of list of pair each pair
-- has the events of entering the spark pool and leaving the sparkpool
-- for the same scheduleId
pairHdpHEvents :: [HdpHEvent]-> [[HdpHEvent]]
pairHdpHEvents []=[]
pairHdpHEvents (x:xs) | isEnterSparkPool x = searchForPairOf x xs : pairHdpHEvents xs
                      | otherwise = pairHdpHEvents xs 

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


-- Total number of entering sparkpool
totalEnterSparkPool :: Int -> [HdpHEvent]-> Int
totalEnterSparkPool acc [] = acc 
totalEnterSparkPool acc (x:xs) = 
  if    isEnterSparkPool x 
  then  totalEnterSparkPool (acc + 1) xs
  else  totalEnterSparkPool acc  xs

-- Total number of ConvertSpark
totalConvertSpark :: Int ->  [HdpHEvent]-> Int
totalConvertSpark acc [] = acc
totalConvertSpark acc (x:xs) =
  if    isConvertSpark x 
  then totalConvertSpark (acc + 1) xs
  else totalConvertSpark acc  xs 

-- Total number of NothingToSpark
totalNothingToSpark :: Int ->  [HdpHEvent]-> Int
totalNothingToSpark acc [] = acc
totalNothingToSpark acc (x:xs) =
  if   isNothingToSpark x
  then totalNothingToSpark (acc + 1) xs
  else totalNothingToSpark acc  xs

-- Test if an HdpHEvent is an EnterSparkPool event
isEnterSparkPool :: HdpHEvent -> Bool 
isEnterSparkPool e = checkEnterSparkPool (e_spec e)
checkEnterSparkPool :: HdpHEventInfo -> Bool 
checkEnterSparkPool  (EnterSparkPool id) = True
checkEnterSparkPool _  = False

-- Test if an HdpHEvent is a ConvertSpark event
isConvertSpark :: HdpHEvent -> Bool 
isConvertSpark e = checkConvertSpark (e_spec e)
checkConvertSpark :: HdpHEventInfo -> Bool 
checkConvertSpark  (ConvertSpark id) = True
checkConvertSpark _  = False

-- Test if an HdpHEvent is a NothingToSpark event
isNothingToSpark :: HdpHEvent -> Bool 
isNothingToSpark e = checkNothingToSpark (e_spec e)
checkNothingToSpark :: HdpHEventInfo -> Bool 
checkNothingToSpark  (NothingToSpark id) = True
checkNothingToSpark _  = False


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


-----------
-- 09-09-2013
-- Registry analysi

-- this function return the total number of times the Registry has been
-- enterd by schedulers for globalising, dereferencing and freeing GlobIVars.

totalEnterRegistry :: Int -> [HdpHEvent] -> Int
totalEnterRegistry acc [] = acc
totalEnterRegistry acc (x:xs)
   | isGlobaliseGRef x = totalEnterRegistry (acc + 1) xs
   | isDereferenceGRef x  = totalEnterRegistry (acc + 1) xs
   | isFreeGRef  x = totalEnterRegistry (acc + 1) xs
   | otherwise = totalEnterRegistry acc  xs

-- this function return the total number of time the registry has been enterd
-- for globalising an GIVar

totalGlobaliseGRef ::  Int -> [HdpHEvent] -> Int
totalGlobaliseGRef acc [] = acc
totalGlobaliseGRef acc (x:xs)
 | isGlobaliseGRef x = totalGlobaliseGRef (acc + 1) xs
 | otherwise = totalGlobaliseGRef acc xs

-- this function return the total number of time the registry has been enterd
-- for dereferencing an GIVar

totalDereferenceGRef ::  Int -> [HdpHEvent] -> Int
totalDereferenceGRef acc [] = acc
totalDereferenceGRef acc (x:xs)
 | isDereferenceGRef x = totalDereferenceGRef (acc + 1) xs
 | otherwise = totalDereferenceGRef acc xs

-- this function return the total number of times the registry has been enterd
-- for freeing  an GIVar

totalFreeGRef ::  Int -> [HdpHEvent] -> Int
totalFreeGRef acc [] = acc
totalFreeGRef acc (x:xs)
 | isFreeGRef x = totalFreeGRef (acc + 1) xs
 | otherwise = totalFreeGRef acc xs

isGlobaliseGRef :: HdpHEvent -> Bool
isGlobaliseGRef e = check (e_spec e)
 where check (GlobaliseGRef id) = True
       check _ = False

isDereferenceGRef :: HdpHEvent -> Bool
isDereferenceGRef e = check (e_spec e)
 where check (DereferenceGRef id) = True
       check _ = False

isFreeGRef :: HdpHEvent -> Bool
isFreeGRef e = check (e_spec e)
 where check (FreeGRef id) = True
       check _ = False

-- this function takes a list of HdpHEvent and returns a list of pairs in
-- a list of registry events  i.e globalise and free.

pairRegEvents ::  [HdpHEvent] -> [[HdpHEvent]]
pairRegEvents  [] = []
pairRegEvents  (x:xs)
  | isGlobaliseGRef x = searchForPairOf x xs : pairRegEvents  xs
  | isFreeGRef x = searchForPairOf x xs : pairRegEvents  xs
  | otherwise = pairRegEvents xs

-- this function takes HdpHEvent and returns a list of  pairs in a list  
-- of globaliseGRef and GRefGlobalised events 

pairGlobGRefEvents :: [HdpHEvent] -> [[HdpHEvent]]
pairGlobGRefEvents [] = []
pairGlobGRefEvents (x:xs) 
  | isGlobaliseGRef x = searchForPairOf x xs : pairGlobGRefEvents xs
  | otherwise =  pairGlobGRefEvents xs

-- this function takes HdpHEvent and returns a list of  pairs in a list  
-- of DereferenceGRef and GRefDereferenced events 
pairDerefGRefEvents :: [HdpHEvent] -> [[HdpHEvent]]
pairDerefGRefEvents[] = []
pairDerefGRefEvents  (x:xs) 
  | isDereferenceGRef x = searchForPairOf x xs : pairDerefGRefEvents xs
  | otherwise =  pairDerefGRefEvents xs

pairFreeGRefEvents :: [HdpHEvent] -> [[HdpHEvent]]
pairFreeGRefEvents [] = []
pairFreeGRefEvents (x:xs) 
  | isFreeGRef x = searchForPairOf x xs : pairFreeGRefEvents xs
  | otherwise =  pairFreeGRefEvents xs


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

-- This function return list of list of events waiting for a scheduler to finsh
-- so they can access the registry
waitingEvents :: [[HdpHEvent]] -> [[HdpHEvent]]
waitingEvents []=[]
waitingEvents (y@(x:x':_):[]) 
 | length y > 2 = y:[]
 | otherwise = []
waitingEvents (y@(x:x':_):y'@(z:z':_):ys) 
 | x' > z && z' > x' =
   waitingEvents ((y ++ y' ):ys)
 | length y > 2 =
   y : waitingEvents  (y':ys)
 | otherwise =  
   waitingEvents  (y':ys)



--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- showRegContention :: EventLog -> IO ()
-- showRegContention e@(EventLog h (Data d)) = do
--   let log_events = (sort( getLogEvents (Data d)))
--       hdph_events = getHdpHEvents log_events     
showRegContention :: [HdpHEvent] -> IO ()
showRegContention hdph_events = do
  let     
      -- get the total number of entries to the registry
      total_entries = totalEnterRegistry 0 hdph_events
      -- get the total number of Globalise events 
      total_glob = totalGlobaliseGRef  0 hdph_events
      -- get the total number of Dereference events
      total_deref = totalDereferenceGRef   0 hdph_events
      -- get the total number of Free events
      total_free = totalFreeGRef  0 hdph_events
      -- get registry events in pair on entering and exiting for ech scheduler
      pairReg = pairRegEvents hdph_events
      -- get paired globalise events
      pairGlob = pairGlobGRefEvents  hdph_events
      -- get paired dereference event
      pairDeref = pairDerefGRefEvents hdph_events
      -- get paired free events
      pairFree = pairFreeGRefEvents hdph_events
      -- get the max number of schedulers in the eventlog
      --max_sched = fromIntegral (countSchedulerId hdph_events):: Word16
      max_sched = findMaxSchedId 0  hdph_events
      -- get the number of times a scheduler x enters the registry
      totalSchedReg = schedEnterGlobData max_sched 1 pairReg
      -- get the number of times a scheduler x globalise a GRef
      totalSchedGlob = schedEnterGlobData max_sched 1  pairGlob
      -- get the number of times a scheduler x dereference a GRef
      totalSchedDeref = schedEnterGlobData max_sched 1 pairDeref
      -- get the number of times a scheduler x free a GRef
      totalSchedFree =  schedEnterGlobData max_sched 1 pairFree
      -- get the number of time a scheduler x enter the registry and 
      -- cause waiting for other schedulers.
      waitReg = schedEnterGlobData max_sched 1 (waitingEvents  pairReg)
      -- get the number of time a scheduler x enter the registry and 
      -- another scheduler y enterd after and exit before scheduler x.
      conflictReg = schedEnterGlobData max_sched 1 (conflictEvents  pairReg)
      -- get the number of time a scheduler x enter the registry for 
      -- globalising a GRef and cause waiting for other schedulers.
      waitGlob =  schedEnterGlobData max_sched 1 (waitingEvents  pairGlob)
      -- get the number of time a scheduler x enter the registry for 
      -- globalising and another scheduler y enterd after and globalised
      -- before scheduler x.
      conflictGlob = schedEnterGlobData max_sched 1 (conflictEvents  pairGlob)
      -- get the number of times a scheduler x enter the registery for
      -- dereferencing a GRef and cause waiting for other schedulers
      waitDeref = schedEnterGlobData max_sched 1 (waitingEvents  pairDeref)
      -- get the number of time a scheduler x enter the registry for 
      -- dereferencing and another scheduler y enterd after and dereferenced
      -- a GRef before scheduler x.
      conflictDeref = schedEnterGlobData max_sched 1 (conflictEvents  pairDeref)
      -- get the number of times a scheduler x enter the registery for
      -- freeing a GRef and cause waiting for other schedulers
      waitFree = schedEnterGlobData max_sched 1 (waitingEvents  pairFree)
      -- get the number of time a scheduler x enter the registry for 
      -- freeing a GRef and another scheduler y enterd after and freeed
      -- a GRef before scheduler x.
      conflictFree = schedEnterGlobData max_sched 1(conflictEvents  pairFree)
      -- get the total duration of waitng caused by each scheduler
      -- for othr schedulers
      durWaitReg = countSchedDuration  max_sched 1 (waitingEvents  pairReg)
      -- get the total duration a scheduler spent in conflicts
      durConfReg = countSchedDuration  max_sched 1 (conflictEvents  pairReg)
      -- get the total duration of waitng caused by each scheduler
      -- for othr schedulers during gloabalising a GRef
      durWaitGlob = countSchedDuration  max_sched 1 (waitingEvents  pairGlob)
      -- get the total duration a scheduler spent in conflicts
      -- while globalising a GRef
      durConfGlob = countSchedDuration  max_sched 1 (conflictEvents  pairGlob)
      -- get the total duration of waitng caused by each scheduler
      -- for othr schedulers during derefrenceing a GRef
      durWaitDeref = countSchedDuration  max_sched 1 (waitingEvents  pairDeref)
      -- get the total duration a scheduler spent in conflicts while 
      -- dereferencing a GRef
      durConfDeref = countSchedDuration  max_sched 1 (conflictEvents  pairDeref)
      -- get the total duration of waitng caused by each scheduler
      -- for othr schedulers during freeing a GRef
      durWaitFree = countSchedDuration  max_sched 1 (waitingEvents  pairFree)
      -- get the total duration a scheduler spent in conflicts while 
      -- freeing a GRef
      durConfFree = countSchedDuration  max_sched 1 (conflictEvents  pairFree)
      -- get how many times wating has happend involving this 
      -- number of scheduler
      groupWaitReg = groupOccurence max_sched 0 (waitingEvents  pairReg)
      -- get how many times conflict has occured involving this
      -- numbr of schedulers 
      groupConfReg = groupOccurence max_sched 0 (conflictEvents pairReg)
      -- get how many times wating has happend involving this 
      -- number of scheduler wihle globalising a GRef
      groupWaitGlob = groupOccurence max_sched 0 (waitingEvents  pairGlob)
      -- get how many times conflict has occured involving this
      -- numbr of schedulers wihle globalising a GRef
      groupConfGlob = groupOccurence max_sched 0 (conflictEvents pairGlob)
      -- get how many times wating has happend involving this 
      -- number of scheduler wihle dereferencing a GRef
      groupWaitDeref = groupOccurence max_sched 0 (waitingEvents  pairDeref)
      -- get how many times conflict has occured involving this
      -- numbr of schedulers wihle dereferencing a GRef
      groupConfDeref = groupOccurence max_sched 0 (conflictEvents pairDeref)
      -- get how many times wating has happend involving this 
      -- number of scheduler wihle Freeing a GRef
      groupWaitFree = groupOccurence max_sched 0 (waitingEvents  pairFree)
      -- get how many times conflict has occured involving this
      -- numbr of schedulers wihle Freeing a GRef
      groupConfFree = groupOccurence max_sched 0 (conflictEvents pairFree)   

  printf "------------------------------------------------------------------------------\n"
  printf "                                 HdpHprof\n" 
  printf "                       Registry Contention analysis\n\n"

  putStr  "Total entries to registry: "  
  putStrLn (show total_entries)
  putStr "Total globalise entries: " 
  putStrLn (show total_glob)
  putStr "Total dereference entries: "
  putStrLn (show total_deref)
  putStr "Total free entries: "
  putStrLn (show total_free)
  -- putStr "Total number of schedulers is " 
  -- putStrLn (show max_sched)
  --putStrLn "------------------------------------------------------------------------------"
  --putStrLn "What fields stand for:"
  --putStrLn "Enter = the number of times a scheduler has enterd the registery"
  --putStrLn "Wait = the number of times a scheduler causes other schedulers to wait"
  --putStrLn "Conflict = the number of times where a scheduler has been involved\n      in conflict, a conflict happens when scheduler x tries access the registry\n     before scheduler y and scheduler y granted access befor scheduler x"


  putStrLn "------------------------------------------------------------------------------"

  putStrLn "Registry Analysis"
  putStrLn "------------------------------------------------------------------------------"
  printf "%-10s %-10s %-10s %-10s %-10s %-10s\n" "SID" "Enter" "Wait" "Wait%" "Conflict" "Conflict%"    
  displayReg  totalSchedReg waitReg conflictReg
  putStrLn "Displayed times are in microseconds."
  printf "%-10s %-10s %-10s %-10s %-10s\n" "SID" "Wait Dur." "Mean" "Conf. Dur." "Mean" 
  displayDurReg waitReg  durWaitReg conflictReg durConfReg
  putStr "Max waiting duration : "
  printf "%07.5f\n" ( pc2mcs (maxDuration  0 (waitingEvents  pairGlob)))
  putStr "Max conflict duration : "
  printf "%07.5f\n" ( pc2mcs (maxDuration  0 (conflictEvents pairGlob)))
  putStrLn "------------------------------------------------------------------------------"

  putStrLn "Number of times waitng or conflict occured with this number of schedulers"
  printf "%-15s %-20s %-20s\n" "No.Schedulers" "Wait. Occurance " "Conf. Occurance" 
  displayGroupReg groupWaitReg groupConfReg

  putStrLn "Registry Analysis for Globalising Global Reference Events"
  putStrLn "------------------------------------------------------------------------------"
  printf "%-10s %-10s %-10s %-10s %-10s %-10s\n" "SID" "Enter" "Wait" "Wait%" "Conflict" "Conflict%"    
  displayReg  totalSchedGlob waitGlob conflictGlob
  putStrLn "Displayed times are in microseconds."
  printf "%-10s %-10s %-10s %-10s %-10s\n" "SID" "Wait Dur." "Mean" "Conf. Dur." "Mean" 
  displayDurReg waitGlob  durWaitGlob conflictGlob durConfGlob
  putStr "Max waiting duration : "
  printf "%07.5f\n" ( pc2mcs (maxDuration  0 (waitingEvents  pairGlob)))
  putStr "Max conflict duration : "
  printf "%07.5f\n" ( pc2mcs (maxDuration  0 (conflictEvents pairGlob)))
  putStrLn "------------------------------------------------------------------------------"

  putStrLn "Number of times waitng or conflict occured with this number of schedulers"
  printf "%-15s %-20s %-20s\n" "No.Schedulers" "Wait. Occurance " "Conf. Occurance" 
  displayGroupReg groupWaitGlob groupConfGlob


  putStrLn "Registry Analysis for Dereferencing Global Reference Events"
  putStrLn "------------------------------------------------------------------------------"
  printf "%-10s %-10s %-10s %-10s %-10s %-10s\n" "SID" "Enter" "Wait" "Wait%" "Conflict" "Conflict%"   
  displayReg  totalSchedDeref waitDeref conflictDeref 
  putStrLn "Displayed times are in microseconds."
  printf "%-10s %-10s %-10s %-10s %-10s\n" "SID" "Wait Dur." "Mean" "Conf. Dur." "Mean" 
  displayDurReg waitDeref  durWaitDeref conflictDeref durConfDeref

  putStr "Max waiting duration : "
  printf "%07.5f\n" ( pc2mcs (maxDuration  0 (waitingEvents  pairDeref)))
  putStr "Max conflict duration : "
  printf "%07.5f\n" ( pc2mcs (maxDuration  0 (conflictEvents pairDeref)))
  putStrLn "------------------------------------------------------------------------------"

  putStrLn "Number of times waitng or conflict occured with this number of schedulers"
  printf "%-15s %-20s %-20s\n" "No.Schedulers" "Wait. Occurance " "Conf. Occurance" 
  displayGroupReg groupWaitDeref groupConfDeref



  putStrLn "Registry Analysis for Freeing Global Reference Events"
  putStrLn "------------------------------------------------------------------------------"
  printf "%-10s %-10s %-10s %-10s %-10s %-10s\n" "SID" "Enter" "Wait" "Wait%" "Conflict" "Conflict%" 
  displayReg  totalSchedFree waitFree conflictFree
  putStrLn "Displayed times are in microseconds."
  printf "%-10s %-10s %-10s %-10s %-10s\n" "SID" "Wait Dur." "Mean" "Conf. Dur." "Mean" 
  displayDurReg waitFree  durWaitFree conflictFree durConfFree
  putStr "Max waiting duration : "
  printf "%07.5f\n" ( pc2mcs (maxDuration  0 (waitingEvents  pairFree)))
  putStr "Max conflict duration : "
  printf "%07.5f\n" ( pc2mcs (maxDuration  0 (conflictEvents pairFree)))
  putStrLn "------------------------------------------------------------------------------"

  putStrLn "Number of times waitng or conflict occured with this number of schedulers"
  printf "%-15s %-20s %-20s\n" "No.Schedulers" "Wait. Occurance " "Conf. Occurance" 
  displayGroupReg groupWaitFree groupConfFree




displayReg [] [] [] = putStrLn "------------------------------------------------------------------------------"

displayReg ((a,b):[]) ((_,c):[]) ((_,d):[]) = do 
 let -- a  scheduler id
     -- b  total enters
     -- c  total cause in waiting
     -- d  total involve in conflicts
     waitPercentage  = percentage  b c
     confPercentage  = percentage  b d
 printf "%-10d %-10d %-10d %-10.2f %-10d %-10.2f\n" a b c waitPercentage d confPercentage
 displayReg [] [] []
displayReg ((a,b):as) ((_,c):cs) ((_,d):ds)  = do 
 let -- a  scheduler id
     -- b  total enters
     -- c  total cause in waiting
     -- d  total involve in conflicts
     waitPercentage  = percentage  b c
     confPercentage  = percentage  b d
 printf "%-10d %-10d %-10d %-10.2f %-10d %-10.2f\n" a b c waitPercentage d confPercentage
 displayReg as cs ds 


displayDurReg [] [] [] [] = do 
 putStrLn "------------------------------------------------------------------------------" -- Don't change to printf or compilation error will occur.
displayDurReg ((a,b):[]) ((_,c):[]) ((_,d):[]) ((_,e):[])= do
 let c' = pc2mcs c        -- waiting durtion
     e' = pc2mcs e        -- conflict duration
     cmd = meanDuration c'  b -- mean duration of waiting
     emd = meanDuration e'  d -- mean duration of conflict
 printf "%-10d %-10.4f %-10.4f %-10.4f %-10.4f\n" a  c' cmd e' emd
 displayDurReg [] [] [] []
displayDurReg ((a,b):as) ((_,c):cs) ((_,d):ds)((_,e):es)  = do
 let c' = pc2mcs c        -- waiting durtion
     e' = pc2mcs e        -- conflict duration
     cmd = meanDuration c'  b -- mean duration of waiting
     emd = meanDuration e'  d -- mean duration of conflict
 printf "%-10d %-10.4f %-10.4f %-10.4f %-10.4f\n" a  c' cmd e' emd
 displayDurReg as cs ds es


displayGroupReg [] []= do
 putStrLn "------------------------------------------------------------------------------"
displayGroupReg((a,b):[]) ((_,c):[])= do 
 printf "%-15d %-20d %-20d \n" a b c
 displayGroupReg [] []
displayGroupReg ((a,b):ys) ((_,c):zs) = do 
 printf "%-15d %-20d %-20d \n" a b c
 displayGroupReg ys zs
