module SparkPoolTestdata where 

import HdpHEventTypesGhci

scenarioN = [
 HdpHEvent {e_time = 101, e_spec = EnterSparkPool {scheduleId = 1}},
 HdpHEvent {e_time = 102, e_spec = EnterSparkPool {scheduleId = 2}},
 HdpHEvent {e_time = 103, e_spec = EnterSparkPool {scheduleId = 5}}, 
 HdpHEvent {e_time = 104, e_spec = NothingToSpark {scheduleId = 1}},
 HdpHEvent {e_time = 105, e_spec = NothingToSpark {scheduleId = 2}},
 HdpHEvent {e_time = 106, e_spec = EnterSparkPool {scheduleId = 3}},
 HdpHEvent {e_time = 107, e_spec = EnterSparkPool {scheduleId = 4}},
 HdpHEvent {e_time = 108, e_spec = NothingToSpark {scheduleId = 3}},
 HdpHEvent {e_time = 109, e_spec = ConvertSpark {scheduleId = 5}},
 HdpHEvent {e_time = 110, e_spec = NothingToSpark {scheduleId = 4}},
 HdpHEvent {e_time = 111, e_spec = EnterSparkPool {scheduleId = 1}},
 HdpHEvent {e_time = 112, e_spec = EnterSparkPool {scheduleId = 2}},
 HdpHEvent {e_time = 113, e_spec = EnterSparkPool {scheduleId = 3}},
 HdpHEvent {e_time = 114, e_spec = ConvertSpark {scheduleId = 2}},
 HdpHEvent {e_time = 115, e_spec = ConvertSpark {scheduleId = 3}},
 HdpHEvent {e_time = 116, e_spec = EnterSparkPool {scheduleId = 4}},
 HdpHEvent {e_time = 117, e_spec = ConvertSpark {scheduleId = 1}},
 HdpHEvent {e_time = 118, e_spec = EnterSparkPool {scheduleId = 5}},
 HdpHEvent {e_time = 119, e_spec = EnterSparkPool {scheduleId = 1}},
 HdpHEvent {e_time = 120,  e_spec = ConvertSpark {scheduleId = 4}},
 HdpHEvent {e_time = 121, e_spec = ConvertSpark {scheduleId = 5}},
 HdpHEvent {e_time = 122, e_spec = ConvertSpark {scheduleId = 1}}]

scenario1 = [
 HdpHEvent {e_time = 100030000, e_spec = NothingToSpark {scheduleId = 1}},
 HdpHEvent {e_time = 100010000, e_spec = EnterSparkPool {scheduleId = 2}},
 HdpHEvent {e_time = 100000000, e_spec = EnterSparkPool {scheduleId = 1}},
 HdpHEvent {e_time = 100020000, e_spec = ConvertSpark {scheduleId = 2}}]


scenario2 = [
 HdpHEvent {e_time = 100030000, e_spec = NothingToSpark {scheduleId = 1}},
 HdpHEvent {e_time = 100010000, e_spec = EnterSparkPool {scheduleId = 2}},
 HdpHEvent {e_time = 100000000, e_spec = EnterSparkPool {scheduleId = 1}},
 HdpHEvent {e_time = 100020000, e_spec = ConvertSpark {scheduleId = 2}},
 HdpHEvent {e_time = 100070000, e_spec = NothingToSpark {scheduleId = 4}},
 HdpHEvent {e_time = 100040000, e_spec = EnterSparkPool {scheduleId = 3}},
 HdpHEvent {e_time = 100050000, e_spec = EnterSparkPool {scheduleId = 4}},
 HdpHEvent {e_time = 100080000, e_spec = NothingToSpark {scheduleId = 3}}]

scenario3 = [
 HdpHEvent {e_time = 100030000, e_spec = NothingToSpark {scheduleId = 1}},
 HdpHEvent {e_time = 100010000, e_spec = EnterSparkPool {scheduleId = 2}},
 HdpHEvent {e_time = 100000000, e_spec = EnterSparkPool {scheduleId = 1}},
 HdpHEvent {e_time = 100020000, e_spec = ConvertSpark {scheduleId = 2}},
 HdpHEvent {e_time = 100070000, e_spec = NothingToSpark {scheduleId = 4}},
 HdpHEvent {e_time = 100040000, e_spec = EnterSparkPool {scheduleId = 3}},
 HdpHEvent {e_time = 100050000, e_spec = EnterSparkPool {scheduleId = 4}},
 HdpHEvent {e_time = 100080000, e_spec = NothingToSpark {scheduleId = 3}},
 HdpHEvent {e_time = 100090000, e_spec = EnterSparkPool {scheduleId = 5}},
 HdpHEvent {e_time = 100900000, e_spec = ConvertSpark {scheduleId = 5}},
 HdpHEvent {e_time = 100100000, e_spec = EnterSparkPool {scheduleId = 6}},
 HdpHEvent {e_time = 100110000, e_spec = ConvertSpark {scheduleId = 6}},
 HdpHEvent {e_time = 100120000, e_spec = EnterSparkPool {scheduleId = 4}},
 HdpHEvent {e_time = 100130000, e_spec = ConvertSpark {scheduleId = 4}}]

scenario4 = [
 HdpHEvent {e_time = 100030000, e_spec = NothingToSpark {scheduleId = 1}},
 HdpHEvent {e_time = 100010000, e_spec = EnterSparkPool {scheduleId = 2}},
 HdpHEvent {e_time = 100000000, e_spec = EnterSparkPool {scheduleId = 1}},
 HdpHEvent {e_time = 100020000, e_spec = ConvertSpark {scheduleId = 2}},
 HdpHEvent {e_time = 100070000, e_spec = NothingToSpark {scheduleId = 4}},
 HdpHEvent {e_time = 100040000, e_spec = EnterSparkPool {scheduleId = 3}},
 HdpHEvent {e_time = 100050000, e_spec = EnterSparkPool {scheduleId = 4}},
 HdpHEvent {e_time = 100080000, e_spec = NothingToSpark {scheduleId = 3}},
 HdpHEvent {e_time = 100090000, e_spec = EnterSparkPool {scheduleId = 5}},
 HdpHEvent {e_time = 100900000, e_spec = ConvertSpark {scheduleId = 5}},
 HdpHEvent {e_time = 100100000, e_spec = EnterSparkPool {scheduleId = 6}},
 HdpHEvent {e_time = 100110000, e_spec = ConvertSpark {scheduleId = 6}},
 HdpHEvent {e_time = 100120000, e_spec = EnterSparkPool {scheduleId = 4}},
 HdpHEvent {e_time = 100130000, e_spec = ConvertSpark {scheduleId = 4}},
 HdpHEvent {e_time = 100910000, e_spec = EnterSparkPool {scheduleId = 2}},
 HdpHEvent {e_time = 100990000, e_spec = ConvertSpark {scheduleId = 2}},
 HdpHEvent {e_time = 100920000, e_spec = EnterSparkPool {scheduleId = 1}},
 HdpHEvent {e_time = 100930000, e_spec = ConvertSpark {scheduleId = 1}},
 HdpHEvent {e_time = 100940000, e_spec = EnterSparkPool {scheduleId = 3}},
 HdpHEvent {e_time = 100950000, e_spec = NothingToSpark {scheduleId = 3}},
 HdpHEvent {e_time = 100960000, e_spec = EnterSparkPool {scheduleId = 4}},
 HdpHEvent {e_time = 100970000, e_spec = NothingToSpark {scheduleId = 4}}]


scenario5 = [
-- productiv conflict for schedId 1
-- conflict duration 30000ns, group of 2 schedulers.
 HdpHEvent {e_time = 100030000, e_spec = NothingToSpark {scheduleId = 1}},
 HdpHEvent {e_time = 100010000, e_spec = EnterSparkPool {scheduleId = 2}},
 HdpHEvent {e_time = 100000000, e_spec = EnterSparkPool {scheduleId = 1}},
 HdpHEvent {e_time = 100020000, e_spec = ConvertSpark {scheduleId = 2}},
-- new non-productive conflict for schedId 3
-- conflict duration 40000ns, group of 2 schedulers.
 HdpHEvent {e_time = 100070000, e_spec = NothingToSpark {scheduleId = 4}},
 HdpHEvent {e_time = 100040000, e_spec = EnterSparkPool {scheduleId = 3}},
 HdpHEvent {e_time = 100050000, e_spec = EnterSparkPool {scheduleId = 4}},
 HdpHEvent {e_time = 100080000, e_spec = NothingToSpark {scheduleId = 3}},
-- new productive conflict for schedId 5
-- conflict duration 810000ns, group of 3 schedulers.
 HdpHEvent {e_time = 100090000, e_spec = EnterSparkPool {scheduleId = 5}},
 HdpHEvent {e_time = 100900000, e_spec = ConvertSpark {scheduleId = 5}},
 HdpHEvent {e_time = 100100000, e_spec = EnterSparkPool {scheduleId = 6}},
 HdpHEvent {e_time = 100110000, e_spec = ConvertSpark {scheduleId = 6}},
 HdpHEvent {e_time = 100120000, e_spec = EnterSparkPool {scheduleId = 4}},
 HdpHEvent {e_time = 100130000, e_spec = ConvertSpark {scheduleId = 4}},
-- new productive conflict for schedId 2
-- conflict duration 80000ns, group of 4 schedulers.
 HdpHEvent {e_time = 100910000, e_spec = EnterSparkPool {scheduleId = 2}},
 HdpHEvent {e_time = 100990000, e_spec = ConvertSpark {scheduleId = 2}},
 HdpHEvent {e_time = 100920000, e_spec = EnterSparkPool {scheduleId = 1}},
 HdpHEvent {e_time = 100930000, e_spec = ConvertSpark {scheduleId = 1}},
 HdpHEvent {e_time = 100940000, e_spec = EnterSparkPool {scheduleId = 3}},
 HdpHEvent {e_time = 100950000, e_spec = NothingToSpark {scheduleId = 3}},
 HdpHEvent {e_time = 100960000, e_spec = EnterSparkPool {scheduleId = 4}},
 HdpHEvent {e_time = 100970000, e_spec = NothingToSpark {scheduleId = 4}},
-- new productive conflict for schedId 2
-- conflict duration 8000ns, group of 2 schedulers.
 HdpHEvent {e_time = 100991000, e_spec = EnterSparkPool {scheduleId = 2}},
 HdpHEvent {e_time = 100999000, e_spec = NothingToSpark {scheduleId = 2}},
 HdpHEvent {e_time = 100992000, e_spec = EnterSparkPool {scheduleId = 3}},
 HdpHEvent {e_time = 100993000, e_spec = ConvertSpark {scheduleId = 3}},
-- new productive conflict for schedId 6
-- conflict duration 1000000ns, group of 6 schedulers, maximum conflict.
 HdpHEvent {e_time = 101000000, e_spec = EnterSparkPool {scheduleId = 6}},
 HdpHEvent {e_time = 102000000, e_spec = NothingToSpark {scheduleId = 6}},
 HdpHEvent {e_time = 101100000, e_spec = EnterSparkPool {scheduleId = 7}},
 HdpHEvent {e_time = 101110000, e_spec = ConvertSpark {scheduleId = 7}},
 HdpHEvent {e_time = 101120000, e_spec = EnterSparkPool {scheduleId = 8}},
 HdpHEvent {e_time = 101130000, e_spec = ConvertSpark {scheduleId = 8}},
 HdpHEvent {e_time = 101140000, e_spec = EnterSparkPool {scheduleId = 1}},
 HdpHEvent {e_time = 101150000, e_spec = ConvertSpark {scheduleId = 1}},
 HdpHEvent {e_time = 101160000, e_spec = EnterSparkPool {scheduleId = 2}},
 HdpHEvent {e_time = 101170000, e_spec = NothingToSpark {scheduleId = 2}},
 HdpHEvent {e_time = 101180000, e_spec = EnterSparkPool {scheduleId = 3}},
 HdpHEvent {e_time = 101190000, e_spec = NothingToSpark {scheduleId = 3}}
 ]


-- Acual data
scenario6 = [
 HdpHEvent {e_time = 13486253, e_spec = EnterSparkPool {scheduleId = 1}},
 HdpHEvent {e_time = 13497172, e_spec = NothingToSpark {scheduleId = 1}},
 HdpHEvent {e_time = 13517980, e_spec = EnterSparkPool {scheduleId = 7}},
 HdpHEvent {e_time = 13523601, e_spec = EnterSparkPool {scheduleId = 2}},
 HdpHEvent {e_time = 13523617, e_spec = EnterSparkPool {scheduleId = 4}},
 HdpHEvent {e_time = 13524712, e_spec = NothingToSpark {scheduleId = 7}},
 HdpHEvent {e_time = 13535033, e_spec = NothingToSpark {scheduleId = 2}},
 HdpHEvent {e_time = 13535293, e_spec = NothingToSpark {scheduleId = 4}},
 HdpHEvent {e_time = 13541943, e_spec = EnterSparkPool {scheduleId = 3}},
 HdpHEvent {e_time = 13549652, e_spec = NothingToSpark {scheduleId = 3}},
 HdpHEvent {e_time = 13610573, e_spec = EnterSparkPool {scheduleId = 3}},
 HdpHEvent {e_time = 13624786, e_spec = ConvertSpark {scheduleId = 3}},
 HdpHEvent {e_time = 13749315, e_spec = EnterSparkPool {scheduleId = 2}},
 HdpHEvent {e_time = 13750045, e_spec = EnterSparkPool {scheduleId = 4}},
 HdpHEvent {e_time = 13779336, e_spec = ConvertSpark {scheduleId = 2}},
 HdpHEvent {e_time = 13793243, e_spec = EnterSparkPool {scheduleId = 1}},
 HdpHEvent {e_time = 13806233, e_spec = ConvertSpark {scheduleId = 4}},
 HdpHEvent {e_time = 13807299, e_spec = ConvertSpark {scheduleId = 1}},
 HdpHEvent {e_time = 13865036, e_spec = EnterSparkPool {scheduleId = 5}},
 HdpHEvent {e_time = 13886017, e_spec = EnterSparkPool {scheduleId = 7}},
 HdpHEvent {e_time = 13889180, e_spec = ConvertSpark {scheduleId = 5}},
 HdpHEvent {e_time = 13904741, e_spec = ConvertSpark {scheduleId = 7}}
 ]

-- Acual data
scenario7 = [
 HdpHEvent {e_time = 13992402, e_spec = EnterSparkPool {scheduleId = 6}},
 HdpHEvent {e_time = 14004535, e_spec = ConvertSpark {scheduleId = 6}},
 HdpHEvent {e_time = 15004080, e_spec = EnterSparkPool {scheduleId = 4}},
 HdpHEvent {e_time = 15030682, e_spec = ConvertSpark {scheduleId = 4}},
 HdpHEvent {e_time = 15036647, e_spec = EnterSparkPool {scheduleId = 7}},
 HdpHEvent {e_time = 15036830, e_spec = EnterSparkPool {scheduleId = 2}},
 HdpHEvent {e_time = 15040253, e_spec = EnterSparkPool {scheduleId = 1}},
 HdpHEvent {e_time = 15044637, e_spec = ConvertSpark {scheduleId = 2}},
 HdpHEvent {e_time = 15045103, e_spec = ConvertSpark {scheduleId = 7}},
 HdpHEvent {e_time = 15047948, e_spec = ConvertSpark {scheduleId = 1}}
 ]

