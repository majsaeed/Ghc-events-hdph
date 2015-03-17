module RegData2 where 

import HdpHEventTypesGhci

data1 = [
 HdpHEvent {e_time = 84908000, e_spec = GlobaliseGRef {scheduleId = 6}},
 HdpHEvent {e_time = 84918000, e_spec = GRefGlobalised {scheduleId = 6}},
 HdpHEvent {e_time = 84928000, e_spec = DereferenceGRef {scheduleId = 6}},
 HdpHEvent {e_time = 84938000, e_spec = GRefDereferenced {scheduleId = 6}},
 HdpHEvent {e_time = 95553000, e_spec = FreeGRef {scheduleId = 5}},
 HdpHEvent {e_time = 95556000, e_spec = GRefFreed {scheduleId = 5}},
 HdpHEvent {e_time = 95553000, e_spec = FreeGRef {scheduleId = 5}},
 HdpHEvent {e_time = 95556000, e_spec = GRefFreed {scheduleId = 5}}]

data2 = [
 HdpHEvent {e_time = 00000001, e_spec = GlobaliseGRef {scheduleId = 1}},
 HdpHEvent {e_time = 00000002, e_spec = GRefGlobalised {scheduleId = 1}},
 HdpHEvent {e_time = 00000003, e_spec = GlobaliseGRef {scheduleId = 2}},
 HdpHEvent {e_time = 00000004, e_spec = GlobaliseGRef {scheduleId = 3}},
 HdpHEvent {e_time = 00000005, e_spec = GlobaliseGRef {scheduleId = 4}},
 HdpHEvent {e_time = 00000006, e_spec = GRefGlobalised {scheduleId = 2}},
 HdpHEvent {e_time = 00000007, e_spec = GRefGlobalised {scheduleId = 3}},
 HdpHEvent {e_time = 00000008, e_spec = GRefGlobalised {scheduleId = 4}},
 HdpHEvent {e_time = 00000009, e_spec = GlobaliseGRef {scheduleId = 2}},
 HdpHEvent {e_time = 00000010, e_spec = GlobaliseGRef {scheduleId = 3}},
 HdpHEvent {e_time = 00000011, e_spec = GlobaliseGRef {scheduleId = 1}},
 HdpHEvent {e_time = 00000012, e_spec = GRefGlobalised {scheduleId = 2}},
 HdpHEvent {e_time = 00000013, e_spec = GRefGlobalised {scheduleId = 3}},
 HdpHEvent {e_time = 00000014, e_spec = GRefGlobalised {scheduleId = 1}},
 HdpHEvent {e_time = 00000015, e_spec = GlobaliseGRef {scheduleId = 2}},
 HdpHEvent {e_time = 00000016, e_spec = GlobaliseGRef {scheduleId = 3}},
 HdpHEvent {e_time = 00000017, e_spec = GlobaliseGRef {scheduleId = 1}},
 HdpHEvent {e_time = 00000018, e_spec = GRefGlobalised {scheduleId = 3}},
 HdpHEvent {e_time = 00000019, e_spec = GRefGlobalised {scheduleId = 2}},
 HdpHEvent {e_time = 00000020, e_spec = GRefGlobalised {scheduleId = 1}},
 HdpHEvent {e_time = 00000021, e_spec = DereferenceGRef {scheduleId = 1}},
 HdpHEvent {e_time = 00000022, e_spec = DereferenceGRef {scheduleId = 2}},
 HdpHEvent {e_time = 00000023, e_spec = DereferenceGRef {scheduleId = 3}},
 HdpHEvent {e_time = 00000024, e_spec = GRefDereferenced {scheduleId = 1}},
 HdpHEvent {e_time = 00000025, e_spec = GRefDereferenced {scheduleId = 2}},
 HdpHEvent {e_time = 00000026, e_spec = GRefDereferenced {scheduleId = 3}},
 HdpHEvent {e_time = 00000027, e_spec = DereferenceGRef {scheduleId = 1}},
 HdpHEvent {e_time = 00000028, e_spec = DereferenceGRef {scheduleId = 2}},
 HdpHEvent {e_time = 00000029, e_spec = DereferenceGRef {scheduleId = 3}},
 HdpHEvent {e_time = 00000030, e_spec = GRefDereferenced {scheduleId = 2}},
 HdpHEvent {e_time = 00000031, e_spec = GRefDereferenced {scheduleId = 1}},
 HdpHEvent {e_time = 00000032, e_spec = GRefDereferenced {scheduleId = 3}}]

data3 = [
 HdpHEvent {e_time = 1, e_spec = GlobaliseGRef {scheduleId = 6}},
 HdpHEvent {e_time = 2, e_spec = GRefGlobalised {scheduleId = 6}},
 HdpHEvent {e_time = 3, e_spec = DereferenceGRef {scheduleId = 6}},
 HdpHEvent {e_time = 6, e_spec = GRefDereferenced {scheduleId = 6}},
 HdpHEvent {e_time = 4, e_spec = DereferenceGRef {scheduleId = 5}},
 HdpHEvent {e_time = 7, e_spec = GRefDereferenced {scheduleId = 5}},
 HdpHEvent {e_time = 8, e_spec = FreeGRef {scheduleId = 5}},
 HdpHEvent {e_time = 9, e_spec = GRefFreed {scheduleId = 5}},
 HdpHEvent {e_time = 10, e_spec = FreeGRef {scheduleId = 5}},
 HdpHEvent {e_time = 10, e_spec = GRefFreed {scheduleId = 5}}]
