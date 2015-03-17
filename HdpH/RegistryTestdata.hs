module RegistryTestdata where 

import HdpHEventTypesGhci


scenario0 = [
 HdpHEvent {e_time = 84908000, e_spec = GlobaliseGRef {scheduleId = 6}},
 HdpHEvent {e_time = 84918000, e_spec = GRefGlobalised {scheduleId = 6}},
 HdpHEvent {e_time = 84928000, e_spec = DereferenceGRef {scheduleId = 6}},
 HdpHEvent {e_time = 84938000, e_spec = GRefDereferenced {scheduleId = 6}},
 HdpHEvent {e_time = 95553000, e_spec = FreeGRef {scheduleId = 5}},
 HdpHEvent {e_time = 95556000, e_spec = GRefFreed {scheduleId = 5}},
 HdpHEvent {e_time = 95553000, e_spec = FreeGRef {scheduleId = 5}},
 HdpHEvent {e_time = 95556000, e_spec = GRefFreed {scheduleId = 5}}]

scenario1 = [
 HdpHEvent {e_time = 000010000, e_spec = GlobaliseGRef {scheduleId = 1}},
 HdpHEvent {e_time = 000012000, e_spec = DereferenceGRef {scheduleId = 0}},
 HdpHEvent {e_time = 000019000, e_spec = GRefGlobalised {scheduleId = 1}},
 HdpHEvent {e_time = 000013000, e_spec = GRefDereferenced {scheduleId = 0}}]

scenario2 = [
 HdpHEvent {e_time = 000010000, e_spec = GlobaliseGRef {scheduleId = 1}},
 HdpHEvent {e_time = 000012000, e_spec = DereferenceGRef {scheduleId = 0}},
 HdpHEvent {e_time = 000019000, e_spec = GRefGlobalised {scheduleId = 1}},
 HdpHEvent {e_time = 000013000, e_spec = GRefDereferenced {scheduleId = 0}},
 HdpHEvent {e_time = 000020000, e_spec = GlobaliseGRef {scheduleId = 2}},
 HdpHEvent {e_time = 000030000, e_spec = GRefGlobalised {scheduleId = 2}},
 HdpHEvent {e_time = 000021000, e_spec = GlobaliseGRef {scheduleId = 3}},
 HdpHEvent {e_time = 000022000, e_spec = GRefGlobalised {scheduleId = 3}}]

scenario3 = [
 HdpHEvent {e_time = 000010000, e_spec = GlobaliseGRef {scheduleId = 1}},
 HdpHEvent {e_time = 000012000, e_spec = DereferenceGRef {scheduleId = 0}},
 HdpHEvent {e_time = 000019000, e_spec = GRefGlobalised {scheduleId = 1}},
 HdpHEvent {e_time = 000013000, e_spec = GRefDereferenced {scheduleId = 0}},
 HdpHEvent {e_time = 000020000, e_spec = GlobaliseGRef {scheduleId = 2}},
 HdpHEvent {e_time = 000030000, e_spec = GRefGlobalised {scheduleId = 2}},
 HdpHEvent {e_time = 000021000, e_spec = GlobaliseGRef {scheduleId = 3}},
 HdpHEvent {e_time = 000022000, e_spec = GRefGlobalised {scheduleId = 3}},
 HdpHEvent {e_time = 000031000, e_spec = GlobaliseGRef {scheduleId = 4}},
 HdpHEvent {e_time = 000035000, e_spec = GRefGlobalised {scheduleId = 4}},
 HdpHEvent {e_time = 000032000, e_spec = GlobaliseGRef {scheduleId = 5}},
 HdpHEvent {e_time = 000034000, e_spec = GRefGlobalised {scheduleId = 5}}]


scenario4 = [
 HdpHEvent {e_time = 000010000, e_spec = GlobaliseGRef {scheduleId = 1}},
 HdpHEvent {e_time = 000012000, e_spec = DereferenceGRef {scheduleId = 0}},
 HdpHEvent {e_time = 000019000, e_spec = GRefGlobalised {scheduleId = 1}},
 HdpHEvent {e_time = 000013000, e_spec = GRefDereferenced {scheduleId = 0}},
 HdpHEvent {e_time = 000020000, e_spec = GlobaliseGRef {scheduleId = 2}},
 HdpHEvent {e_time = 000030000, e_spec = GRefGlobalised {scheduleId = 2}},
 HdpHEvent {e_time = 000021000, e_spec = GlobaliseGRef {scheduleId = 3}},
 HdpHEvent {e_time = 000022000, e_spec = GRefGlobalised {scheduleId = 3}},
 HdpHEvent {e_time = 000031000, e_spec = GlobaliseGRef {scheduleId = 4}},
 HdpHEvent {e_time = 000035000, e_spec = GRefGlobalised {scheduleId = 4}},
 HdpHEvent {e_time = 000032000, e_spec = GlobaliseGRef {scheduleId = 5}},
 HdpHEvent {e_time = 000034000, e_spec = GRefGlobalised {scheduleId = 5}},
 HdpHEvent {e_time = 000036000, e_spec = FreeGRef {scheduleId = 6}},
 HdpHEvent {e_time = 000041000, e_spec = GRefFreed {scheduleId = 6}},
 HdpHEvent {e_time = 000037000, e_spec = FreeGRef {scheduleId = 4}},
 HdpHEvent {e_time = 000038000, e_spec = GRefFreed {scheduleId = 4}},
 HdpHEvent {e_time = 000039000, e_spec = FreeGRef {scheduleId = 3}},
 HdpHEvent {e_time = 000040000, e_spec = GRefFreed {scheduleId = 3}}]


scenario5 = [
-- no conflict
 HdpHEvent {e_time = 000001000, e_spec = GlobaliseGRef {scheduleId = 1}},
 HdpHEvent {e_time = 000002000, e_spec = GRefGlobalised {scheduleId = 1}},
 HdpHEvent {e_time = 000003000, e_spec = GlobaliseGRef {scheduleId = 3}},
 HdpHEvent {e_time = 000004000, e_spec = GRefGlobalised {scheduleId = 3}},
-- conflict of type mixture for schedId 1
-- conflict duration 9000ns, group of 2 schedulers. 
 HdpHEvent {e_time = 000010000, e_spec = GlobaliseGRef {scheduleId = 1}},
 HdpHEvent {e_time = 000012000, e_spec = DereferenceGRef {scheduleId = 0}},
 HdpHEvent {e_time = 000019000, e_spec = GRefGlobalised {scheduleId = 1}},
 HdpHEvent {e_time = 000013000, e_spec = GRefDereferenced {scheduleId = 0}},
-- new coflict of type globalise for schedId 2
-- conflict duration 10000ns, group of 2 schedulers, maximum duration.
 HdpHEvent {e_time = 000020000, e_spec = GlobaliseGRef {scheduleId = 2}},
 HdpHEvent {e_time = 000030000, e_spec = GRefGlobalised {scheduleId = 2}},
 HdpHEvent {e_time = 000021000, e_spec = GlobaliseGRef {scheduleId = 3}},
 HdpHEvent {e_time = 000022000, e_spec = GRefGlobalised {scheduleId = 3}},
-- new coflict of type globalise for schedId 4
-- conflict duration 4000ns, group of 2 schedulers.
 HdpHEvent {e_time = 000031000, e_spec = GlobaliseGRef {scheduleId = 4}},
 HdpHEvent {e_time = 000035000, e_spec = GRefGlobalised {scheduleId = 4}},
 HdpHEvent {e_time = 000032000, e_spec = GlobaliseGRef {scheduleId = 5}},
 HdpHEvent {e_time = 000034000, e_spec = GRefGlobalised {scheduleId = 5}},
-- new conflict of type free for schedId 6
-- conflict duration 5000ns, group of 3 schedulers.
 HdpHEvent {e_time = 000036000, e_spec = FreeGRef {scheduleId = 6}},
 HdpHEvent {e_time = 000041000, e_spec = GRefFreed {scheduleId = 6}},
 HdpHEvent {e_time = 000037000, e_spec = FreeGRef {scheduleId = 4}},
 HdpHEvent {e_time = 000038000, e_spec = GRefFreed {scheduleId = 4}},
 HdpHEvent {e_time = 000039000, e_spec = FreeGRef {scheduleId = 3}},
 HdpHEvent {e_time = 000040000, e_spec = GRefFreed {scheduleId = 3}},
-- no conflict
 HdpHEvent {e_time = 000042000, e_spec = GlobaliseGRef {scheduleId = 2}},
 HdpHEvent {e_time = 000043000, e_spec = GRefGlobalised {scheduleId = 2}},
 HdpHEvent {e_time = 000043100, e_spec = GlobaliseGRef {scheduleId = 4}},
 HdpHEvent {e_time = 000043200, e_spec = GRefGlobalised {scheduleId = 4}},
-- new conflict of type deref for schedId 1
-- conflict duration 6000ns, group of 4 schedulers.
 HdpHEvent {e_time = 000044000, e_spec = DereferenceGRef {scheduleId = 1}},
 HdpHEvent {e_time = 000050000, e_spec = GRefDereferenced {scheduleId = 1}},
 HdpHEvent {e_time = 000045000, e_spec = DereferenceGRef {scheduleId = 2}},
 HdpHEvent {e_time = 000046000, e_spec = GRefDereferenced {scheduleId = 2}},
 HdpHEvent {e_time = 000047000, e_spec = DereferenceGRef {scheduleId = 3}},
 HdpHEvent {e_time = 000048000, e_spec = GRefDereferenced {scheduleId = 3}},
 HdpHEvent {e_time = 000049000, e_spec = DereferenceGRef {scheduleId = 5}},
 HdpHEvent {e_time = 000049900, e_spec = GRefDereferenced {scheduleId = 5}}
 ]

-- actual data
scenario6 = [
 HdpHEvent {e_time = 13535920, e_spec = GlobaliseGRef {scheduleId = 6}},
 HdpHEvent {e_time = 13550349, e_spec = GRefGlobalised {scheduleId = 6}},
 HdpHEvent {e_time = 13588537, e_spec = GlobaliseGRef {scheduleId = 6}},
 HdpHEvent {e_time = 13594801, e_spec = GRefGlobalised {scheduleId = 6}},
 HdpHEvent {e_time = 13621124, e_spec = GlobaliseGRef {scheduleId = 6}},
 HdpHEvent {e_time = 13627388, e_spec = GRefGlobalised {scheduleId = 6}},
 HdpHEvent {e_time = 13638676, e_spec = GlobaliseGRef {scheduleId = 3}},
 HdpHEvent {e_time = 13646654, e_spec = GRefGlobalised {scheduleId = 3}},
 HdpHEvent {e_time = 13654097, e_spec = GlobaliseGRef {scheduleId = 6}},
 HdpHEvent {e_time = 13660175, e_spec = GRefGlobalised {scheduleId = 6}},
 HdpHEvent {e_time = 13681581, e_spec = GlobaliseGRef {scheduleId = 6}},
 HdpHEvent {e_time = 13682326, e_spec = GlobaliseGRef {scheduleId = 3}},
 HdpHEvent {e_time = 13687459, e_spec = GRefGlobalised {scheduleId = 6}},
 HdpHEvent {e_time = 13687784, e_spec = GRefGlobalised {scheduleId = 3}},
 HdpHEvent {e_time = 13706990, e_spec = GlobaliseGRef {scheduleId = 3}},
 HdpHEvent {e_time = 13707868, e_spec = GlobaliseGRef {scheduleId = 6}},
 HdpHEvent {e_time = 13714589, e_spec = GRefGlobalised {scheduleId = 3}},
 HdpHEvent {e_time = 13732185, e_spec = GlobaliseGRef {scheduleId = 3}},
 HdpHEvent {e_time = 13737388, e_spec = GRefGlobalised {scheduleId = 3}},
 HdpHEvent {e_time = 13757082, e_spec = GlobaliseGRef {scheduleId = 3}},
 HdpHEvent {e_time = 13762508, e_spec = GRefGlobalised {scheduleId = 3}},
 HdpHEvent {e_time = 13787632, e_spec = GlobaliseGRef {scheduleId = 3}},
 HdpHEvent {e_time = 13789103, e_spec = GlobaliseGRef {scheduleId = 2}},
 HdpHEvent {e_time = 13793446, e_spec = GRefGlobalised {scheduleId = 3}},
 HdpHEvent {e_time = 13813890, e_spec = GlobaliseGRef {scheduleId = 3}},
 HdpHEvent {e_time = 13815845, e_spec = GlobaliseGRef {scheduleId = 4}},
 HdpHEvent {e_time = 13818818, e_spec = GlobaliseGRef {scheduleId = 1}},
 HdpHEvent {e_time = 13819795, e_spec = GRefGlobalised {scheduleId = 3}},
 HdpHEvent {e_time = 13822291, e_spec = GRefGlobalised {scheduleId = 4}},
 HdpHEvent {e_time = 13822670, e_spec = GRefGlobalised {scheduleId = 6}},
 HdpHEvent {e_time = 13825925, e_spec = GRefGlobalised {scheduleId = 1}},
 HdpHEvent {e_time = 13838986, e_spec = GlobaliseGRef {scheduleId = 3}},
 HdpHEvent {e_time = 13839657, e_spec = GRefGlobalised {scheduleId = 2}},
 HdpHEvent {e_time = 13846131, e_spec = GlobaliseGRef {scheduleId = 6}},
 HdpHEvent {e_time = 13846492, e_spec = GlobaliseGRef {scheduleId = 4}},
 HdpHEvent {e_time = 13849076, e_spec = GRefGlobalised {scheduleId = 3}},
 HdpHEvent {e_time = 13852109, e_spec = GlobaliseGRef {scheduleId = 1}},
 HdpHEvent {e_time = 13852743, e_spec = GRefGlobalised {scheduleId = 6}},
 HdpHEvent {e_time = 13852956, e_spec = GRefGlobalised {scheduleId = 4}},
 HdpHEvent {e_time = 13858530, e_spec = GRefGlobalised {scheduleId = 1}}
 ]

scenario100 = [
 HdpHEvent {e_time = 000010000, e_spec = GlobaliseGRef {scheduleId = 1}},
 HdpHEvent {e_time = 000020000, e_spec = GRefGlobalised {scheduleId = 1}},
 HdpHEvent {e_time = 000030000, e_spec = GlobaliseGRef {scheduleId = 2}},
 HdpHEvent {e_time = 000040000, e_spec = GlobaliseGRef {scheduleId = 3}},
 HdpHEvent {e_time = 000050000, e_spec = GlobaliseGRef {scheduleId = 4}},
 HdpHEvent {e_time = 000060000, e_spec = GRefGlobalised {scheduleId = 2}},
 HdpHEvent {e_time = 000070000, e_spec = GRefGlobalised {scheduleId = 3}},
 HdpHEvent {e_time = 000080000, e_spec = GRefGlobalised {scheduleId = 4}},
 HdpHEvent {e_time = 000090000, e_spec = GlobaliseGRef {scheduleId = 2}},
 HdpHEvent {e_time = 000100000, e_spec = GlobaliseGRef {scheduleId = 3}},
 HdpHEvent {e_time = 000110000, e_spec = GlobaliseGRef {scheduleId = 1}},
 HdpHEvent {e_time = 000120000, e_spec = GRefGlobalised {scheduleId = 2}},
 HdpHEvent {e_time = 000130000, e_spec = GRefGlobalised {scheduleId = 3}},
 HdpHEvent {e_time = 000140000, e_spec = GRefGlobalised {scheduleId = 1}},
 HdpHEvent {e_time = 000150000, e_spec = GlobaliseGRef {scheduleId = 2}},
 HdpHEvent {e_time = 000160000, e_spec = GlobaliseGRef {scheduleId = 3}},
 HdpHEvent {e_time = 000170000, e_spec = GlobaliseGRef {scheduleId = 1}},
 HdpHEvent {e_time = 000180000, e_spec = GRefGlobalised {scheduleId = 3}},
 HdpHEvent {e_time = 000190000, e_spec = GRefGlobalised {scheduleId = 2}},
 HdpHEvent {e_time = 000200000, e_spec = GRefGlobalised {scheduleId = 1}},
 HdpHEvent {e_time = 000210000, e_spec = DereferenceGRef {scheduleId = 1}},
 HdpHEvent {e_time = 000220000, e_spec = DereferenceGRef {scheduleId = 2}},
 HdpHEvent {e_time = 000230000, e_spec = DereferenceGRef {scheduleId = 3}},
 HdpHEvent {e_time = 000240000, e_spec = GRefDereferenced {scheduleId = 1}},
 HdpHEvent {e_time = 000250000, e_spec = GRefDereferenced {scheduleId = 2}},
 HdpHEvent {e_time = 000200006, e_spec = GRefDereferenced {scheduleId = 3}},
 HdpHEvent {e_time = 000270000, e_spec = DereferenceGRef {scheduleId = 1}},
 HdpHEvent {e_time = 000280000, e_spec = DereferenceGRef {scheduleId = 2}},
 HdpHEvent {e_time = 000290000, e_spec = DereferenceGRef {scheduleId = 3}},
 HdpHEvent {e_time = 000300000, e_spec = GRefDereferenced {scheduleId = 2}},
 HdpHEvent {e_time = 000310000, e_spec = GRefDereferenced {scheduleId = 1}},
 HdpHEvent {e_time = 000320000, e_spec = GRefDereferenced {scheduleId = 3}}]



