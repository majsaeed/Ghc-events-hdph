
module HdpH.ShowSparkCont ( showSparkContention ) where

import GHC.RTS.Events
import Data.List (sort)
import Text.Printf 
import HdpH.HdpHEventTypes
import HdpH.Utilities
import HdpH.Analysis
     ( getLogEvents,
       getHdpHEvents,
       searchForPairOf,
       findMaxSchedId,
       schedEnterGlobData,
       countSchedDuration,
       groupOccurence,
       maxDuration 
  )

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

showSparkContention :: EventLog -> IO ()
showSparkContention e@(EventLog h (Data d)) = do             
  let log_events = (sort( getLogEvents (Data d)))
      hdph_events = getHdpHEvents log_events
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
      --maxScInConf = maxSchedInConf 0  prodConfs 
      schConf = groupOccurence maxSchedId  0  prodConfs

  putStrLn "test 2"

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
  display1 0 0 0 enter_persi sidConf proSidConf
  printf "All times displayed are in microseconds \n"
  --putStrLn ("Max number of schedulers in a productive conflict: "++ (show (maxSchedInConf 0  prodConfs)))
  printf "%-10s %-10s %-10s %-10s %-10s \n" "SID" "All.C.Dur" "Mean" "Pro.C.Dur" "Mean"
  display2 0 0 0 0 sidConf  confDurSid  proSidConf proConfDurSid
  putStr "Max duration  in a productive conflict: "
  printf "%07.5f\n" ( pc2mcs (maxDuration  0 prodConfs ))
  printf "-----------------------------------------------------------------------------\n"
  printf "Conflicts grouped by total number of schedulers involved \n"
  printf "%-10s %-10s\n" "No. Sid""No. Conf."
  display3 schConf
  printf "------------------------------------------------------------------------------\n"
  
-- -- to here
-- Don't change to printf or compilation error will occur.
display1 tenter tconf tpconf [] [] [] = do
 let allconfper = percentage tenter tconf
     proconfper = percentage tenter tpconf
 putStrLn "------------------------------------------------------------"
 printf "%-10s %-10d %-10d %-10.2f %-10d %-10.2f\n" "Total"  tenter tconf allconfper tpconf  proconfper
 putStrLn "------------------------------------------------------------------------------"

display1 tenter tconf tpconf ((a,b):[]) ((_,c):[]) ((_,d):[]) = do 
 let -- a  scheduler id
     -- b  total enters
     -- c  All conflicts
     -- d  productive conflicts
     allconf  =   percentage  b c
     prodconf =   percentage  b d
 printf "%-10d %-10d %-10d %-10.2f %-10d %-10.2f\n" a b c allconf d prodconf
 display1 (tenter + b) (tconf + c) (tpconf + d) [] [] []
display1 tenter tconf tpconf ((a,b):as) ((_,c):cs) ((_,d):ds)  = do 
 let -- a  scheduler id
     -- b  total enters
     -- c  All conflicts
     -- d  productive conflicts
     allconf  =   percentage  b c
     prodconf =   percentage  b d
 printf "%-10d %-10d %-10d %-10.2f %-10d %-10.2f\n" a b c allconf d prodconf
 display1  (tenter + b) (tconf + c) (tpconf + d) as cs ds 
  

display2 tconf tconfdur tpconf tpconfdur [] [] [] []= do 
 let tconfdur' = pc2mcs tconfdur
     amd = meanDuration tconfdur' tconf
     tpconfdur' = pc2mcs tpconfdur
     pmd = meanDuration  tpconfdur'  tpconf
 putStrLn "------------------------------------------------------------"
 printf "%-10s %-10.4f %-10.4f %-10.4f %-10.4f\n" "Total" tconfdur' amd tpconfdur'  pmd
 putStrLn "" -- Don't change to printf or compilation error will occur.

display2  tconf tconfdur tpconf tpconfdur ((a,b):[]) ((_,c):[]) ((_,d):[]) ((_,e):[]) = do
 let c' = pc2mcs c        -- all conflict durtion
     e' = pc2mcs e        -- Productive conflict duration
     amd = meanDuration c'  b -- All conf mean duration 
     pmd = meanDuration e'  d -- Productive conf mean duration 
 printf "%-10d %-10.4f %-10.4f %-10.4f %-10.4f\n" a c' amd e' pmd
 display2  (tconf + b)  (tconfdur + c) (tpconf + d) (tpconfdur + e) [] [] [] [] 

display2  tconf tconfdur tpconf tpconfdur ((a,b):as) ((_,c):cs) ((_,d):ds)  ((_,e):es) = do
 let c' = pc2mcs c        -- all conflict durtion
     e' = pc2mcs e        -- Productive conflict duration
     amd = meanDuration c'  b -- All conf mean duration 
     pmd = meanDuration e'  d -- Productive conf mean duration 
 printf "%-10d %-10.4f %-10.4f %-10.4f %-10.4f\n" a c' amd e' pmd
 display2 (tconf + b)  (tconfdur + c) (tpconf + d) (tpconfdur + e) as cs ds es


display3 [] = do
 putStrLn ""
display3 ((a,b):[])= do 
 printf "%-10d %-10d \n" a b
 display3 []
display3 ((a,b):as)= do 
 printf "%-10d %-10d \n" a b
 display3 as