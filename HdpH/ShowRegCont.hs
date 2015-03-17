
module HdpH.ShowRegCont ( showRegContention ) where

import GHC.RTS.Events
import Data.List (sort)
import Data.Word ( Word64)
import Text.Printf 
import HdpHEventTypesGhci
--import HdpH.HdpHEventTypes
import UtilitiesGhci
--import HdpH.Utilities
import AnalysisGhci
--import HdpH.Analysis 
 ( 
 getLogEvents,
 getHdpHEvents,
 searchForPairOf,
 findMaxSchedId,
 schedEnterGlobData,
 countSchedDuration,
 totaConfDuration,
 countGropDuration,
 groupOccurence,
 maxDuration,   )


import RegistryTestdata

-- This function return list of list of events waiting for a scheduler to finsh
-- so they can access the registry
waitingEvents :: [[HdpHEvent]] -> [[HdpHEvent]]
waitingEvents []=[]
waitingEvents (y@(x:x':_):[]) 
 | length y > 2 = y:[]
 | otherwise = []
waitingEvents (y@(x:x':_):y'@(z:z':_):ys) 
 | x' > z = -- && z' > x' =
   waitingEvents ((y ++ y' ):ys)
 | length y > 2 =
   y : waitingEvents  (y':ys)
 | otherwise =  
   waitingEvents  (y':ys)


-- this function takes a list of HdpHEvent and returns a list of pairs in
-- a list of registry events  i.e globalise and free.

pairRegEvents ::  [HdpHEvent] -> [[HdpHEvent]]
pairRegEvents  [] = []
pairRegEvents  (x:xs)
  | isGlobaliseGRef x = searchForPairOf x xs : pairRegEvents  xs
  | isFreeGRef x = searchForPairOf x xs : pairRegEvents  xs
  | isDereferenceGRef x =  searchForPairOf x xs : pairRegEvents  xs
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


-- this function return the total number of times the Registry has been
-- enterd by schedulers for globalising, dereferencing and freeing GlobIVars.

totalEnterRegistry :: Int -> [HdpHEvent] -> Int
totalEnterRegistry acc [] = acc
totalEnterRegistry acc (x:xs)
   | isGlobaliseGRef x = totalEnterRegistry (acc + 1) xs
   | isFreeGRef  x = totalEnterRegistry (acc + 1) xs
   | isDereferenceGRef x = totalEnterRegistry (acc + 1) xs
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


-- / Type checking functions

isGlobaliseGRef :: HdpHEvent -> Bool
isGlobaliseGRef e = check (e_spec e)
 where check (GlobaliseGRef id) = True
       check _ = False

isGRefGlobalised :: HdpHEvent -> Bool
isGRefGlobalised e = check (e_spec e)
 where check (GRefGlobalised id) = True
       check _ = False

isDereferenceGRef :: HdpHEvent -> Bool
isDereferenceGRef e = check (e_spec e)
 where check (DereferenceGRef id) = True
       check _ = False

isGRefDereferenced :: HdpHEvent -> Bool
isGRefDereferenced e = check (e_spec e)
 where check (GRefDereferenced id) = True
       check _ = False

isFreeGRef :: HdpHEvent -> Bool
isFreeGRef e = check (e_spec e)
 where check (FreeGRef id) = True
       check _ = False

isGRefFreed :: HdpHEvent -> Bool
isGRefFreed e = check (e_spec e)
 where check (GRefFreed id) = True
       check _ = False



-- this function counts how many times conflicts of type all globalise,
-- all free, and glob&free events occured during accessing the registry
groupConflictByType :: Int -> Int -> Int -> Int ->[[HdpHEvent]] -> [(String, Int)]
groupConflictByType gacc facc dacc macc [] = [("Glob", gacc), ("Free", facc), ("Deref", dacc), ("Mixture", macc)]
groupConflictByType gacc facc dacc macc (x:xs)
 | allGlob x = groupConflictByType (1 + gacc) facc dacc macc xs
 | allFree x = groupConflictByType gacc (1 + facc) dacc macc xs
 | allDeref x = groupConflictByType gacc facc (1 + dacc) macc xs
 | otherwise = groupConflictByType gacc facc dacc (1 +macc) xs

--this function return true if all events in a list are of globalise a GRef
allGlob :: [HdpHEvent] -> Bool
allGlob [] = False
allGlob (x:[])
  | isGlobaliseGRef x = True
  | isGRefGlobalised x = True
  | otherwise = False
allGlob (x:xs)
  | isGlobaliseGRef x = allGlob xs
  | isGRefGlobalised x = allGlob xs
  | otherwise = False

-- this function return true if all events in a list are of freeing a GRef
allFree :: [HdpHEvent] -> Bool
allFree [] = False
allFree (x:[])
  | isFreeGRef x = True
  | isGRefFreed x = True
  | otherwise = False
allFree (x:xs)
  | isFreeGRef x = allFree xs
  | isGRefFreed x = allFree xs
  | otherwise = False

-- this function return true if all events in a list are of Dereference a GRef
allDeref :: [HdpHEvent] -> Bool
allDeref [] = False
allDeref (x:[])
  | isDereferenceGRef x = True
  | isGRefDereferenced x = True
  | otherwise = False
allDeref (x:xs)
  | isDereferenceGRef x = allDeref xs
  | isGRefDereferenced x = allDeref xs
  | otherwise = False

-- this function calculate the total duration for conflict of type all glob,
-- all free and glob & free events occured during accessing the registry.
durConfByType :: Word64 -> Word64 -> Word64  -> Word64 -> [[HdpHEvent]] -> [(String, Word64)]
durConfByType  gdur fdur ddur mdur [] = [("Glob", gdur), ("Free", fdur), ("Deref", ddur), ("Mixture", mdur)]
durConfByType  gdur fdur ddur mdur (y@(x:x':xs):ys)
 | allGlob y = durConfByType (gdur + ((e_time x') - (e_time x))) fdur ddur mdur ys
 | allFree y = durConfByType gdur (fdur + ((e_time x') - (e_time x))) ddur mdur ys
 | allDeref y = durConfByType gdur fdur (ddur + ((e_time x') - (e_time x))) mdur ys
 | otherwise = durConfByType gdur fdur ddur (mdur + ((e_time x') - (e_time x))) ys




-- / Show registry contention analysis 

--showRegContention :: EventLog -> IO ()
--showRegContention e@(EventLog h (Data d)) = do
--  let log_events = (sort( getLogEvents (Data d)))
--      hdph_events = getHdpHEvents log_events    
 
showRegContention :: [HdpHEvent] -> IO ()
showRegContention hdph_events_ = do
  let
      hdph_events  = sort  hdph_events_

      -- get the total number of entries to the registry
      total_entries = totalEnterRegistry 0 hdph_events
      -- get the total number of Globalise events 
      total_glob = totalGlobaliseGRef  0 hdph_events
      -- get the total number of Free events
      total_free = totalFreeGRef  0 hdph_events
      -- get the total number of Dereference events
      total_deref = totalDereferenceGRef   0 hdph_events
      -- get registry events in pair on entering and exiting for ech scheduler
      pairReg = pairRegEvents hdph_events
      -- get the max number of schedulers in the eventlog
      --max_sched = fromIntegral (countSchedulerId hdph_events):: Word16
      max_sched = findMaxSchedId 0  hdph_events
      --max_sched =  countSchedulerId hdph_events
      -- get the number of times a scheduler x enters the registry
      totalSchedReg = schedEnterGlobData max_sched 0 pairReg
      -- get the number of time a scheduler x enter the registry and 
      -- cause waiting for other schedulers.
      waitReg = schedEnterGlobData max_sched 0 (waitingEvents  pairReg)
      -- get the total duration of waitng caused by each scheduler
      -- for othr schedulers
      durWaitReg = countSchedDuration  max_sched 0 (waitingEvents  pairReg)
      -- calculate total conflicts duration
      totalConfDur =  totaConfDuration 0 durWaitReg
      -- get how many times wating has happend involving this 
      -- number of scheduler
      groupConfReg = groupOccurence (max_sched +1) 0 (waitingEvents  pairReg)
      -- this function calculate the total duration for conflict involve
      -- a particuler number of schedulers
      durGroupSched =  countGropDuration (max_sched +1) (waitingEvents  pairReg)
      -- count the total number of conflicts 
      totalConflicts = length   (waitingEvents pairReg)
      -- get how many times conflicts with different type of events have occured
      groupConfType = groupConflictByType 0 0 0 0 (waitingEvents pairReg)
      -- get the duration of conflict with specfic type of events
      durConfType =  durConfByType 0 0 0 0 (waitingEvents pairReg)

--displayGroupReg totalConflicts groupConfReg durGroupSched

  printf "------------------------------------------------------------------------------\n"
  printf "                                 HdpHprof\n" 
  printf "                       Registry Contention analysis\n\n"

  putStr  "Total entries to registry: "  
  putStrLn (show total_entries)
  putStr "Total globalise entries: " 
  putStrLn (show total_glob)
  putStr "Total free entries: "
  putStrLn (show total_free)
  putStr "Total dereference entries: "
  putStrLn (show total_deref)


  putStrLn "------------------------------------------------------------------------------"

  --putStrLn "Registry Analysis"
  --putStrLn "------------------------------------------------------------------------------"
  printf "%-10s %-10s %-10s %-10s\n" "SID" "Enter" "Conflict" "Conflict%"     
  displayReg 0 0  totalSchedReg waitReg
  putStrLn "Displayed times are in milliseconds."
  printf "%-10s %-20s %-10s\n" "SID" "Conf. Duration" "Mean"  
  displayDurReg 0 0 waitReg  durWaitReg
  putStr "Max conflict duration : "
  printf "%07.5f\n" ( ns2ms (maxDuration  0 (waitingEvents  pairReg)))
  putStrLn "------------------------------------------------------------------------------"
  
  putStrLn "Number of times a conlflict occured with this number of schedulers"
  printf "%-15s %-20s %-20s %-20s\n" "No.Schedulers" "Conf. Occurance% " "Conf. Duration%" "Mean"
  displayGroupReg totalConflicts totalConfDur groupConfReg durGroupSched 
  
  putStrLn "Conflict grouped by events type"
  printf "%-15s %-20s %-20s %-20s \n" "Events type" "Conf. Occurance% " "Conf. Duration%" "Mean"
  displayGroupConfType totalConflicts totalConfDur groupConfType durConfType



displayReg tenter  tconf [] [] = do 
 let tpercentage =  percentage  tenter tconf
 putStrLn "----------------------------------------"
 printf "%-10s %-10d %-10d %-10.2f\n" "Total"  tenter tconf tpercentage
 putStrLn "------------------------------------------------------------------------------"
displayReg tenter  tconf  ((a,b):[]) ((_,c):[]) = do 
 let -- a  scheduler id
     -- b  total enters
     -- c  total involve in conflicts
     confPercentage  = percentage  b c
 printf "%-10d %-10d %-10d %-10.2f\n" a b c confPercentage
 displayReg (tenter + b)  (tconf + c)  [] []
displayReg  tenter  tconf ((a,b):as) ((_,c):cs)  = do 
 let -- a  scheduler id
     -- b  total enters
     -- c  total involve in conflicts
     confPercentage  = percentage  b c
 printf "%-10d %-10d %-10d %-10.2f\n" a b c confPercentage
 displayReg (tenter + b)  (tconf + c)  as cs


displayDurReg tconf tdur [] [] = do 
 let tcmd = meanDuration tdur tconf
 putStrLn "----------------------------------------"
 printf "%-10s %-20.4f %-10.4f\n" "Total" tdur tcmd
 --putStrLn "------------------------------------------------------------------------------" -- Don't change to printf or compilation error will occur.
displayDurReg tconf tdur ((a,b):[]) ((_,c):[]) = do
 let c' = ns2ms c        -- conflict durtion
     cmd = meanDuration c'  b -- mean duration of conflicts
 printf "%-10d %-20.4f %-10.4f\n" a  c' cmd 
 displayDurReg (tconf + b) (tdur + c') [] [] 
displayDurReg  tconf tdur ((a,b):as) ((_,c):cs)  = do
 let c' = ns2ms c        -- waiting durtion
     cmd = meanDuration c'  b -- mean duration of conflicts
 printf "%-10d %-20.4f %-10.4f\n" a  c' cmd
 displayDurReg (tconf + b) (tdur + c') as cs


displayGroupReg tc td [] []= do
 putStrLn "------------------------------------------------------------------------------"
displayGroupReg tc td ((a,b):[]) ((_,c):[]) = do 
 let -- a = number of schedulers in a conflict
     -- b = conflicts total occurance
     b' = percentage  tc b -- conflict percentage
     -- c = total conflict duration for that particuler number of schedulers
     c' = ns2ms c -- conflict duration
     cp = percentageFloat (ns2ms td) (ns2ms c) -- conflict duration percentage
     cmd = meanDuration c'  b -- mean duration of conflicts
 printf "%-15d %-20.2f %-20.2f %-20.4f\n" a b' cp cmd
 displayGroupReg tc td [] []
displayGroupReg tc td ((a,b):ys) ((_,c):cs) = do 
 let -- a = number of schedulers in a conflict
     -- b = conflicts total occurance
     b' = percentage  tc b -- conflict percentage
     -- c = total conflict duration for that particuler number of schedulers
     c' = ns2ms c -- conflict duration
     cp = percentageFloat (ns2ms td) (ns2ms c) -- conflict duration percentage
     cmd = meanDuration c'  b -- mean duration of conflicts
 printf "%-15d %-20.2f %-20.2f %-20.4f\n" a b' cp cmd
 displayGroupReg tc td ys cs


displayGroupConfType tc td [] [] = do
 putStrLn "------------------------------------------------------------------------------"
displayGroupConfType tc td ((a,b):[]) ((_,c):[]) = do
 let -- a = events type
     -- b = number of occurance
     b' = percentage  tc b
     -- c = total duration of these conflicts
     c' = ns2ms c  -- conflict duration
     cp = percentageFloat (ns2ms td) (ns2ms c) -- conflict duration percentage
     cmd = meanDuration c'  b -- conflicts mean duration 
 printf "%-15s %-20.2f %-20.2f %-20.4f\n" a b' cp cmd 
 displayGroupConfType tc td [] []
displayGroupConfType  tc td ((a,b):ys) ((_,c):cs) =  do
 let -- a = events type
     -- b = number of occurance
     b' = percentage  tc b
     -- c = total duration of these conflicts
     c' = ns2ms c  -- conflict duration
     cp = percentageFloat (ns2ms td) (ns2ms c) -- conflict duration percentage
     cmd = meanDuration c'  b -- conflicts mean duration 
 printf "%-15s %-20.2f %-20.2f %-20.4f\n" a b' cp cmd  
 displayGroupConfType tc td ys cs
