
-- 3 May 2012
-- Majed Al Saeed

{-# OPTIONS_GHC -funbox-strict-fields #-}

module HdpH.Analysis 
 (showStrEvt, 
  showEventLog, 
  showContention
 ) where

--import Testdata
import HdpH.HdpHEventTypes
--import HdpHEventTypesGhci
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
      enter_persi = schedESPool 1 paired
      maxSchedId = findMaxSid 1 paired
      sidConf = schedESPool2 maxSchedId 1  allConfs
      proSidConf = schedESPool2  maxSchedId 1 prodConfs
      confDurSid = confSchedDuration maxSchedId 1 allConfs
      proConfDurSid = confSchedDuration maxSchedId  1 prodConfs
      maxScInConf = maxSchedInConf 0  prodConfs 
      schConf = schedulersConflicts maxScInConf 0  prodConfs

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
  printf "%07.5f\n" ( pc2mcs (maxConfDuration  0 prodConfs ))
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
     md = mDurConf d'  b -- mean duration of conflect
 printf "%-10d %-10d %-10.4f %-10.4f %-10.4f\n" a b c' d' md
 display2 [] [] [] 
display2 ((a,b):as) ((_,c):cs) ((_,d):ds) = do
 let c' = pc2mcs c        -- all conflict durtion
     d' = pc2mcs d        -- Productive conflict duration
     md = mDurConf d'  b -- mean duration of conflect
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
  
-- Convert picoseconds to milliseconds (ps to ms)
pc2ms :: Word64 -> Float
pc2ms pc = (fromIntegral pc) * 10^^(-9)

--Convert picoseconds to microseconds (ps to μs)
pc2mcs :: Word64 -> Float
pc2mcs pc = (fromIntegral pc) * 10^^(-6)


-- calculate the conflict percentage 
confPerc :: Int -> Int -> Float
confPerc enters conflicts =  
 ((fromIntegral conflicts)/ (fromIntegral enters)) * 100

-- calculate the mean duration
mDurConf :: Float -> Int -> Float
mDurConf dur conf 
 | conf > 0 && dur > 00 
   = dur / (fromIntegral conf)
 | otherwise = 0.0

-- Count the max duration of a conflict
maxConfDuration :: Word64 -> [[HdpHEvent]] -> Word64
maxConfDuration maxdur [] = maxdur
maxConfDuration maxdur (y@(x:x':xs):[])
 | ((e_time x') - (e_time x)) > maxdur =
   maxConfDuration  ((e_time x') - (e_time x)) []
 | otherwise =  maxConfDuration maxdur []
maxConfDuration maxdur (y@(x:x':xs):ys)
 |((e_time x') - (e_time x)) > maxdur =
   maxConfDuration  ((e_time x') - (e_time x)) ys
 | otherwise =  maxConfDuration maxdur ys

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

-- This function count how many time a scheduler has entered the saprkpool
-- or it can be used to count how many time a scheduler involved in conflict
schedESPool :: Word16 -> [[HdpHEvent]]-> [(Word16, Int)]
schedESPool sid []=[]
schedESPool sid  l@(y@(x:x':_):ys) = 
 let maxsid = findMaxSid 0 l 
     acc = countSchedESPool 0 sid l --ys
 in if sid < maxsid
    then (sid, acc) : schedESPool (sid + 1) l 
    else (sid, acc) :[]

-- created 15-11-14
-- rename 
schedESPool2 :: Word16 -> Word16 ->[[HdpHEvent]]-> [(Word16, Int)]
schedESPool2 maxsid sid []=
    if sid < maxsid
    then (sid, 0) : schedESPool2 maxsid (sid + 1) [] 
    else (sid, 0) :[]

schedESPool2 maxsid sid  l@(y@(x:x':_):ys) = 
 let acc = countSchedESPool 0 sid l --ys
 in if sid < maxsid
    then (sid, acc) : schedESPool2 maxsid (sid + 1) l 
    else (sid, acc) :[]

countSchedESPool :: Int -> Word16 -> [[HdpHEvent]]-> Int
countSchedESPool acc sid [] = acc
countSchedESPool acc sid (y@(x:x':_):ys) = -- cc
 if scheduleId (e_spec x) == sid
 then countSchedESPool (acc + 1) sid ys
 else countSchedESPool acc  sid ys

-- Find the max scheduler id in the paired List
findMaxSid :: Word16 -> [[HdpHEvent]] -> Word16
findMaxSid max [] = max
findMaxSid max (y@(x:x':_):ys) =
  let sid = scheduleId (e_spec x)
  in if max <  sid
  then findMaxSid sid ys
  else findMaxSid max ys

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

-- No. scheduler in a conflict or conflicts that has this number of schedulers
schedulersConflicts :: Int -> Int -> [[HdpHEvent]]-> [(Int,Int)]
schedulersConflicts maxscs acc [] = []
schedulersConflicts maxscs acc l@(x:xs) =
 let sidconf = countSchedConf maxscs 0 l
 in if maxscs < 2
    then []
    else 
      if  checkSchCon sidconf  
      then sidconf : schedulersConflicts (maxscs -1 ) 0 l
      else schedulersConflicts (maxscs -1 ) 0 l
   
countSchedConf :: Int -> Int -> [[HdpHEvent]]-> (Int,Int)
countSchedConf schs acc [] = (schs, acc)
countSchedConf schs acc (y:[]) 
 |  countSchedulerId y == schs =
    countSchedConf schs (acc + 1) []
 |  otherwise = countSchedConf schs acc  []
countSchedConf schs acc (y:ys)
 |  countSchedulerId y == schs =
    countSchedConf schs (acc + 1) ys
 |  otherwise = countSchedConf schs acc  ys

projectSchedulerId :: [HdpHEvent] -> [SchedulerId]
projectSchedulerId [] =[]
projectSchedulerId (x:xs) = 
 case  e_spec x  of 
      SparkCreated id -> id: projectSchedulerId xs 
      ConvertSpark id -> id:  projectSchedulerId xs
      NothingToSpark id -> id : projectSchedulerId xs
      EnterSparkPool id -> id : projectSchedulerId xs
      PutSpark id -> id : projectSchedulerId xs
      _ -> projectSchedulerId xs

countSchedulerId :: [HdpHEvent]-> Int
countSchedulerId x = Data.Set.size $ Data.Set.fromList $ projectSchedulerId x 

checkSchCon :: (Int,Int) -> Bool
checkSchCon (a,b)= 
 if (b > 0) && (a > 0)
 then True
 else False



-- This function showes the max number of schedulers in conflict 
maxSchedInConf :: Int -> [[HdpHEvent]] -> Int
maxSchedInConf max [] = div max 2
maxSchedInConf max (y@(x:x':xs):[])
 | length y > max =
   maxSchedInConf (length y)  []
 | otherwise = maxSchedInConf max [] 
maxSchedInConf max (y@(x:x':xs):ys)
 | length y > max =
    maxSchedInConf (length y)  ys
 | otherwise =  maxSchedInConf max  ys

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
 | x' > z' =
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

-- This function should take HdpHEvent of type EnterSparkPool and a list
-- and search the list for the next HdpHEvent in the list with the same
-- schedule ID
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
