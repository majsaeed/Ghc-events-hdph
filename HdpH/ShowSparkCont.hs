
module HdpH.ShowSparkCont ( showSparkContention ) where

import GHC.RTS.Events
import Data.List (sort)
import Text.Printf
import HdpHEventTypesGhci
--import HdpH.HdpHEventTypes
import UtilitiesGhci
--import HdpH.Utilities
import AnalysisGhci
--import HdpH.Analysis
     ( getLogEvents,
       getHdpHEvents,
       searchForPairOf,
       findMaxSchedId,
       schedEnterGlobData,
       countSchedDuration,
       totaConfDuration,
       countGropDuration,
       groupOccurence,
       maxDuration 
  )

import SparkPoolTestdata

-- Takes a list of HdpHEvent and return a list of list of pair each pair
-- has the events of entering the spark pool and leaving the sparkpool
-- for the same scheduleId
pairSparkEvents :: [HdpHEvent]-> [[HdpHEvent]]
pairSparkEvents []=[]
pairSparkEvents (x:xs) 
  | isEnterSparkPool x = searchForPairOf x xs : pairSparkEvents xs
  | otherwise = pairSparkEvents xs 


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

-- / Type checking functions 
-- Test if an HdpHEvent is an EnterSparkPool event
isEnterSparkPool :: HdpHEvent -> Bool 
isEnterSparkPool e = checkEnterSparkPool (e_spec e)
 where 
  checkEnterSparkPool :: HdpHEventInfo -> Bool 
  checkEnterSparkPool  (EnterSparkPool id) = True
  checkEnterSparkPool _  = False

-- Test if an HdpHEvent is a ConvertSpark event
isConvertSpark :: HdpHEvent -> Bool 
isConvertSpark e = checkConvertSpark (e_spec e)
 where
  checkConvertSpark :: HdpHEventInfo -> Bool 
  checkConvertSpark  (ConvertSpark id) = True
  checkConvertSpark _  = False

-- Test if an HdpHEvent is a NothingToSpark event
isNothingToSpark :: HdpHEvent -> Bool 
isNothingToSpark e = checkNothingToSpark (e_spec e)
 where
  checkNothingToSpark :: HdpHEventInfo -> Bool 
  checkNothingToSpark  (NothingToSpark id) = True
  checkNothingToSpark _  = False


-- / Returning total numbers of operations on spark pool

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



-- / Spark contention analysis function

--showSparkContention :: EventLog -> IO ()
--showSparkContention e@(EventLog h (Data d)) = do             
--  let log_events = (sort( getLogEvents (Data d)))
--      hdph_events = getHdpHEvents log_events
showSparkContention :: [HdpHEvent] -> IO ()
showSparkContention hdph_events_ = do
  let 
      hdph_events = sort hdph_events_
      paired  = pairSparkEvents hdph_events
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
      totalProConfDur = totaConfDuration 0  proConfDurSid
      --maxScInConf = maxSchedInConf 0  prodConfs 
      -- count the total number of productive conflcit
      totalConflicts = length  prodConfs
      -- get how many times conflict has happend involving this 
      -- number of scheduler
      groupConfSpark = groupOccurence maxSchedId  0  prodConfs
      -- this function calculate the total duration for conflict involve
      -- a particuler number of schedulers
      durGroupSched =  countGropDuration  maxSchedId  prodConfs

  printf "------------------------------------------------------------------------------\n"
  printf "                                 HdpHprof\n" 
  printf "                     Spark Pool Contention analysis\n\n"

  putStr  "Total entry to sparkpool: "  
  putStrLn (show total_entry )
  putStr "Total sparks converted: " 
  putStrLn (show total_coverted)
  putStr "Total No spark to convert: "
  putStrLn (show total_nothing )
  putStrLn "------------------------------------------------------------------------------"
  printf "%-10s %-10s %-10s %-10s %-10s %-10s\n" "SID" "Enter" "All.Cof.""All.C.%" "Pro.Conf." "Pro.C.%"    
  displaySpark 0 0 0 enter_persi sidConf proSidConf
  printf "All times displayed are in milliseconds \n"
  --putStrLn ("Max number of schedulers in a productive conflict: "++ (show (maxSchedInConf 0  prodConfs)))
  printf "%-10s %-10s %-10s %-10s %-10s \n" "SID" "All.C.Dur" "Mean" "Pro.C.Dur" "Mean"
  displayDurSpark 0 0 0 0 sidConf  confDurSid  proSidConf proConfDurSid
  putStr "Max duration  in a productive conflict: "
  printf "%07.5f\n" ( ns2ms (maxDuration  0 prodConfs ))
  printf "-----------------------------------------------------------------------------\n"
  printf "Conflicts grouped by total number of schedulers involved \n"

  printf "%-15s %-20s %-20s %-20s\n" "No.Schedulers" "Conf. Occurance% " "Conf. Duration%" "Mean"
  displayGroupSpark totalConflicts totalProConfDur groupConfSpark durGroupSched
  
-- -- to here
-- Don't change to printf or compilation error will occur.
displaySpark tenter tconf tpconf [] [] [] = do
 let allconfper = percentage tenter tconf
     proconfper = percentage tenter tpconf
 putStrLn "------------------------------------------------------------"
 printf "%-10s %-10d %-10d %-10.2f %-10d %-10.2f\n" "Total"  tenter tconf allconfper tpconf  proconfper
 putStrLn "------------------------------------------------------------------------------"

displaySpark tenter tconf tpconf ((a,b):[]) ((_,c):[]) ((_,d):[]) = do 
 let -- a  scheduler id
     -- b  total enters
     -- c  All conflicts
     -- d  productive conflicts
     allconf  =   percentage  b c
     prodconf =   percentage  b d
 printf "%-10d %-10d %-10d %-10.2f %-10d %-10.2f\n" a b c allconf d prodconf
 displaySpark (tenter + b) (tconf + c) (tpconf + d) [] [] []
displaySpark tenter tconf tpconf ((a,b):as) ((_,c):cs) ((_,d):ds)  = do 
 let -- a  scheduler id
     -- b  total enters
     -- c  All conflicts
     -- d  productive conflicts
     allconf  =   percentage  b c
     prodconf =   percentage  b d
 printf "%-10d %-10d %-10d %-10.2f %-10d %-10.2f\n" a b c allconf d prodconf
 displaySpark  (tenter + b) (tconf + c) (tpconf + d) as cs ds 
  

displayDurSpark tconf tconfdur tpconf tpconfdur [] [] [] []= do 
 let tconfdur' = ns2ms tconfdur
     amd = meanDuration tconfdur' tconf
     tpconfdur' = ns2ms tpconfdur
     pmd = meanDuration  tpconfdur'  tpconf
 putStrLn "------------------------------------------------------------"
 printf "%-10s %-10.4f %-10.4f %-10.4f %-10.4f\n" "Total" tconfdur' amd tpconfdur'  pmd
 putStrLn "" -- Don't change to printf or compilation error will occur.

displayDurSpark  tconf tconfdur tpconf tpconfdur ((a,b):[]) ((_,c):[]) ((_,d):[]) ((_,e):[]) = do
 let c' = ns2ms c        -- all conflict durtion
     e' = ns2ms e        -- Productive conflict duration
     amd = meanDuration c'  b -- All conf mean duration 
     pmd = meanDuration e'  d -- Productive conf mean duration 
 printf "%-10d %-10.4f %-10.4f %-10.4f %-10.4f\n" a c' amd e' pmd
 displayDurSpark  (tconf + b)  (tconfdur + c) (tpconf + d) (tpconfdur + e) [] [] [] [] 

displayDurSpark  tconf tconfdur tpconf tpconfdur ((a,b):as) ((_,c):cs) ((_,d):ds)  ((_,e):es) = do
 let c' = ns2ms c        -- all conflict durtion
     e' = ns2ms e        -- Productive conflict duration
     amd = meanDuration c'  b -- All conf mean duration 
     pmd = meanDuration e'  d -- Productive conf mean duration 
 printf "%-10d %-10.4f %-10.4f %-10.4f %-10.4f\n" a c' amd e' pmd
 displayDurSpark (tconf + b)  (tconfdur + c) (tpconf + d) (tpconfdur + e) as cs ds es


displayGroupSpark tc td  [] []= do
 putStrLn "------------------------------------------------------------------------------"

displayGroupSpark tc td ((a,b):[]) ((_,c):[]) = do 
 let -- a = number of schedulers in a conflict
     -- b = conflicts total occurance
     b' = percentage  tc b -- conflict percentage
     -- c = total conflict duration for that particuler number of schedulers
     c' = ns2ms c  -- conflict duration
     cp = percentageFloat (ns2ms td) (ns2ms c) -- conflict duration percentage
     cmd = meanDuration c'  b -- mean duration of conflicts
 printf "%-15d %-20.2f %-20.2f %-20.4f\n" a b' cp cmd
 displayGroupSpark tc td [] []
displayGroupSpark tc td ((a,b):ys) ((_,c):cs) = do 
 let -- a = number of schedulers in a conflict
     -- b = conflicts total occurance
     b' = percentage  tc b -- conflict percentage
     -- c = total conflict duration for that particuler number of schedulers
     c' = ns2ms c  -- conflict duration
     cp = percentageFloat (ns2ms td) (ns2ms c) -- conflict duration percentage
     cmd = meanDuration c'  b -- mean duration of conflicts
 printf "%-15d %-20.2f %-20.2f %-20.4f\n" a b' cp cmd
 displayGroupSpark tc td ys cs

