/* This file is created automatically.  Do not edit by hand.*/

#define CONTROL_GROUP_CONST_291 291
#define STD_HDR_SIZE 1
#define PROF_HDR_SIZE 2
#define BLOCK_SIZE 4096
#define MBLOCK_SIZE 1048576
#define BLOCKS_PER_MBLOCK 252
#define TICKY_BIN_COUNT 9
#define OFFSET_StgRegTable_rR1 0
#define OFFSET_StgRegTable_rR2 8
#define OFFSET_StgRegTable_rR3 16
#define OFFSET_StgRegTable_rR4 24
#define OFFSET_StgRegTable_rR5 32
#define OFFSET_StgRegTable_rR6 40
#define OFFSET_StgRegTable_rR7 48
#define OFFSET_StgRegTable_rR8 56
#define OFFSET_StgRegTable_rR9 64
#define OFFSET_StgRegTable_rR10 72
#define OFFSET_StgRegTable_rF1 80
#define OFFSET_StgRegTable_rF2 84
#define OFFSET_StgRegTable_rF3 88
#define OFFSET_StgRegTable_rF4 92
#define OFFSET_StgRegTable_rF5 96
#define OFFSET_StgRegTable_rF6 100
#define OFFSET_StgRegTable_rD1 104
#define OFFSET_StgRegTable_rD2 112
#define OFFSET_StgRegTable_rD3 120
#define OFFSET_StgRegTable_rD4 128
#define OFFSET_StgRegTable_rD5 136
#define OFFSET_StgRegTable_rD6 144
#define OFFSET_StgRegTable_rXMM1 152
#define OFFSET_StgRegTable_rXMM2 168
#define OFFSET_StgRegTable_rXMM3 184
#define OFFSET_StgRegTable_rXMM4 200
#define OFFSET_StgRegTable_rXMM5 216
#define OFFSET_StgRegTable_rXMM6 232
#define OFFSET_StgRegTable_rYMM1 248
#define OFFSET_StgRegTable_rYMM2 280
#define OFFSET_StgRegTable_rYMM3 312
#define OFFSET_StgRegTable_rYMM4 344
#define OFFSET_StgRegTable_rYMM5 376
#define OFFSET_StgRegTable_rYMM6 408
#define OFFSET_StgRegTable_rZMM1 440
#define OFFSET_StgRegTable_rZMM2 504
#define OFFSET_StgRegTable_rZMM3 568
#define OFFSET_StgRegTable_rZMM4 632
#define OFFSET_StgRegTable_rZMM5 696
#define OFFSET_StgRegTable_rZMM6 760
#define OFFSET_StgRegTable_rL1 824
#define OFFSET_StgRegTable_rSp 832
#define OFFSET_StgRegTable_rSpLim 840
#define OFFSET_StgRegTable_rHp 848
#define OFFSET_StgRegTable_rHpLim 856
#define OFFSET_StgRegTable_rCCCS 864
#define OFFSET_StgRegTable_rCurrentTSO 872
#define OFFSET_StgRegTable_rCurrentNursery 888
#define OFFSET_StgRegTable_rHpAlloc 904
#define OFFSET_StgRegTable_rRet 912
#define REP_StgRegTable_rRet b64
#define StgRegTable_rRet(__ptr__) REP_StgRegTable_rRet[__ptr__+OFFSET_StgRegTable_rRet]
#define OFFSET_StgRegTable_rNursery 880
#define REP_StgRegTable_rNursery b64
#define StgRegTable_rNursery(__ptr__) REP_StgRegTable_rNursery[__ptr__+OFFSET_StgRegTable_rNursery]
#define OFFSET_stgEagerBlackholeInfo -24
#define OFFSET_stgGCEnter1 -16
#define OFFSET_stgGCFun -8
#define OFFSET_Capability_r 24
#define OFFSET_Capability_lock 1096
#define OFFSET_Capability_no 944
#define REP_Capability_no b32
#define Capability_no(__ptr__) REP_Capability_no[__ptr__+OFFSET_Capability_no]
#define OFFSET_Capability_mut_lists 1016
#define REP_Capability_mut_lists b64
#define Capability_mut_lists(__ptr__) REP_Capability_mut_lists[__ptr__+OFFSET_Capability_mut_lists]
#define OFFSET_Capability_context_switch 1064
#define REP_Capability_context_switch b32
#define Capability_context_switch(__ptr__) REP_Capability_context_switch[__ptr__+OFFSET_Capability_context_switch]
#define OFFSET_Capability_interrupt 1068
#define REP_Capability_interrupt b32
#define Capability_interrupt(__ptr__) REP_Capability_interrupt[__ptr__+OFFSET_Capability_interrupt]
#define OFFSET_Capability_sparks 1176
#define REP_Capability_sparks b64
#define Capability_sparks(__ptr__) REP_Capability_sparks[__ptr__+OFFSET_Capability_sparks]
#define OFFSET_Capability_total_allocated 1072
#define REP_Capability_total_allocated b64
#define Capability_total_allocated(__ptr__) REP_Capability_total_allocated[__ptr__+OFFSET_Capability_total_allocated]
#define OFFSET_Capability_weak_ptr_list_hd 1048
#define REP_Capability_weak_ptr_list_hd b64
#define Capability_weak_ptr_list_hd(__ptr__) REP_Capability_weak_ptr_list_hd[__ptr__+OFFSET_Capability_weak_ptr_list_hd]
#define OFFSET_Capability_weak_ptr_list_tl 1056
#define REP_Capability_weak_ptr_list_tl b64
#define Capability_weak_ptr_list_tl(__ptr__) REP_Capability_weak_ptr_list_tl[__ptr__+OFFSET_Capability_weak_ptr_list_tl]
#define OFFSET_bdescr_start 0
#define REP_bdescr_start b64
#define bdescr_start(__ptr__) REP_bdescr_start[__ptr__+OFFSET_bdescr_start]
#define OFFSET_bdescr_free 8
#define REP_bdescr_free b64
#define bdescr_free(__ptr__) REP_bdescr_free[__ptr__+OFFSET_bdescr_free]
#define OFFSET_bdescr_blocks 48
#define REP_bdescr_blocks b32
#define bdescr_blocks(__ptr__) REP_bdescr_blocks[__ptr__+OFFSET_bdescr_blocks]
#define OFFSET_bdescr_gen_no 40
#define REP_bdescr_gen_no b16
#define bdescr_gen_no(__ptr__) REP_bdescr_gen_no[__ptr__+OFFSET_bdescr_gen_no]
#define OFFSET_bdescr_link 16
#define REP_bdescr_link b64
#define bdescr_link(__ptr__) REP_bdescr_link[__ptr__+OFFSET_bdescr_link]
#define OFFSET_bdescr_flags 46
#define REP_bdescr_flags b16
#define bdescr_flags(__ptr__) REP_bdescr_flags[__ptr__+OFFSET_bdescr_flags]
#define SIZEOF_generation 376
#define OFFSET_generation_n_new_large_words 56
#define REP_generation_n_new_large_words b64
#define generation_n_new_large_words(__ptr__) REP_generation_n_new_large_words[__ptr__+OFFSET_generation_n_new_large_words]
#define OFFSET_generation_weak_ptr_list 112
#define REP_generation_weak_ptr_list b64
#define generation_weak_ptr_list(__ptr__) REP_generation_weak_ptr_list[__ptr__+OFFSET_generation_weak_ptr_list]
#define SIZEOF_CostCentreStack 96
#define OFFSET_CostCentreStack_ccsID 0
#define REP_CostCentreStack_ccsID b64
#define CostCentreStack_ccsID(__ptr__) REP_CostCentreStack_ccsID[__ptr__+OFFSET_CostCentreStack_ccsID]
#define OFFSET_CostCentreStack_mem_alloc 72
#define REP_CostCentreStack_mem_alloc b64
#define CostCentreStack_mem_alloc(__ptr__) REP_CostCentreStack_mem_alloc[__ptr__+OFFSET_CostCentreStack_mem_alloc]
#define OFFSET_CostCentreStack_scc_count 48
#define REP_CostCentreStack_scc_count b64
#define CostCentreStack_scc_count(__ptr__) REP_CostCentreStack_scc_count[__ptr__+OFFSET_CostCentreStack_scc_count]
#define OFFSET_CostCentreStack_prevStack 16
#define REP_CostCentreStack_prevStack b64
#define CostCentreStack_prevStack(__ptr__) REP_CostCentreStack_prevStack[__ptr__+OFFSET_CostCentreStack_prevStack]
#define OFFSET_CostCentre_ccID 0
#define REP_CostCentre_ccID b64
#define CostCentre_ccID(__ptr__) REP_CostCentre_ccID[__ptr__+OFFSET_CostCentre_ccID]
#define OFFSET_CostCentre_link 56
#define REP_CostCentre_link b64
#define CostCentre_link(__ptr__) REP_CostCentre_link[__ptr__+OFFSET_CostCentre_link]
#define OFFSET_StgHeader_info 0
#define REP_StgHeader_info b64
#define StgHeader_info(__ptr__) REP_StgHeader_info[__ptr__+OFFSET_StgHeader_info]
#define OFFSET_StgHeader_ccs 8
#define REP_StgHeader_ccs b64
#define StgHeader_ccs(__ptr__) REP_StgHeader_ccs[__ptr__+OFFSET_StgHeader_ccs]
#define OFFSET_StgHeader_ldvw 16
#define REP_StgHeader_ldvw b64
#define StgHeader_ldvw(__ptr__) REP_StgHeader_ldvw[__ptr__+OFFSET_StgHeader_ldvw]
#define SIZEOF_StgSMPThunkHeader 8
#define OFFSET_StgClosure_payload 0
#define StgClosure_payload(__ptr__,__ix__) W_[__ptr__+SIZEOF_StgHeader+OFFSET_StgClosure_payload + WDS(__ix__)]
#define OFFSET_StgEntCounter_allocs 48
#define REP_StgEntCounter_allocs b64
#define StgEntCounter_allocs(__ptr__) REP_StgEntCounter_allocs[__ptr__+OFFSET_StgEntCounter_allocs]
#define OFFSET_StgEntCounter_allocd 16
#define REP_StgEntCounter_allocd b64
#define StgEntCounter_allocd(__ptr__) REP_StgEntCounter_allocd[__ptr__+OFFSET_StgEntCounter_allocd]
#define OFFSET_StgEntCounter_registeredp 0
#define REP_StgEntCounter_registeredp b64
#define StgEntCounter_registeredp(__ptr__) REP_StgEntCounter_registeredp[__ptr__+OFFSET_StgEntCounter_registeredp]
#define OFFSET_StgEntCounter_link 56
#define REP_StgEntCounter_link b64
#define StgEntCounter_link(__ptr__) REP_StgEntCounter_link[__ptr__+OFFSET_StgEntCounter_link]
#define OFFSET_StgEntCounter_entry_count 40
#define REP_StgEntCounter_entry_count b64
#define StgEntCounter_entry_count(__ptr__) REP_StgEntCounter_entry_count[__ptr__+OFFSET_StgEntCounter_entry_count]
#define SIZEOF_StgUpdateFrame_NoHdr 8
#define SIZEOF_StgUpdateFrame (SIZEOF_StgHeader+8)
#define SIZEOF_StgCatchFrame_NoHdr 16
#define SIZEOF_StgCatchFrame (SIZEOF_StgHeader+16)
#define SIZEOF_StgStopFrame_NoHdr 0
#define SIZEOF_StgStopFrame (SIZEOF_StgHeader+0)
#define SIZEOF_StgMutArrPtrs_NoHdr 16
#define SIZEOF_StgMutArrPtrs (SIZEOF_StgHeader+16)
#define OFFSET_StgMutArrPtrs_ptrs 0
#define REP_StgMutArrPtrs_ptrs b64
#define StgMutArrPtrs_ptrs(__ptr__) REP_StgMutArrPtrs_ptrs[__ptr__+SIZEOF_StgHeader+OFFSET_StgMutArrPtrs_ptrs]
#define OFFSET_StgMutArrPtrs_size 8
#define REP_StgMutArrPtrs_size b64
#define StgMutArrPtrs_size(__ptr__) REP_StgMutArrPtrs_size[__ptr__+SIZEOF_StgHeader+OFFSET_StgMutArrPtrs_size]
#define SIZEOF_StgSmallMutArrPtrs_NoHdr 8
#define SIZEOF_StgSmallMutArrPtrs (SIZEOF_StgHeader+8)
#define OFFSET_StgSmallMutArrPtrs_ptrs 0
#define REP_StgSmallMutArrPtrs_ptrs b64
#define StgSmallMutArrPtrs_ptrs(__ptr__) REP_StgSmallMutArrPtrs_ptrs[__ptr__+SIZEOF_StgHeader+OFFSET_StgSmallMutArrPtrs_ptrs]
#define SIZEOF_StgArrBytes_NoHdr 8
#define SIZEOF_StgArrBytes (SIZEOF_StgHeader+8)
#define OFFSET_StgArrBytes_bytes 0
#define REP_StgArrBytes_bytes b64
#define StgArrBytes_bytes(__ptr__) REP_StgArrBytes_bytes[__ptr__+SIZEOF_StgHeader+OFFSET_StgArrBytes_bytes]
#define OFFSET_StgArrBytes_payload 8
#define StgArrBytes_payload(__ptr__,__ix__) W_[__ptr__+SIZEOF_StgHeader+OFFSET_StgArrBytes_payload + WDS(__ix__)]
#define OFFSET_StgTSO__link 0
#define REP_StgTSO__link b64
#define StgTSO__link(__ptr__) REP_StgTSO__link[__ptr__+SIZEOF_StgHeader+OFFSET_StgTSO__link]
#define OFFSET_StgTSO_global_link 8
#define REP_StgTSO_global_link b64
#define StgTSO_global_link(__ptr__) REP_StgTSO_global_link[__ptr__+SIZEOF_StgHeader+OFFSET_StgTSO_global_link]
#define OFFSET_StgTSO_what_next 24
#define REP_StgTSO_what_next b16
#define StgTSO_what_next(__ptr__) REP_StgTSO_what_next[__ptr__+SIZEOF_StgHeader+OFFSET_StgTSO_what_next]
#define OFFSET_StgTSO_why_blocked 26
#define REP_StgTSO_why_blocked b16
#define StgTSO_why_blocked(__ptr__) REP_StgTSO_why_blocked[__ptr__+SIZEOF_StgHeader+OFFSET_StgTSO_why_blocked]
#define OFFSET_StgTSO_block_info 32
#define REP_StgTSO_block_info b64
#define StgTSO_block_info(__ptr__) REP_StgTSO_block_info[__ptr__+SIZEOF_StgHeader+OFFSET_StgTSO_block_info]
#define OFFSET_StgTSO_blocked_exceptions 80
#define REP_StgTSO_blocked_exceptions b64
#define StgTSO_blocked_exceptions(__ptr__) REP_StgTSO_blocked_exceptions[__ptr__+SIZEOF_StgHeader+OFFSET_StgTSO_blocked_exceptions]
#define OFFSET_StgTSO_id 40
#define REP_StgTSO_id b32
#define StgTSO_id(__ptr__) REP_StgTSO_id[__ptr__+SIZEOF_StgHeader+OFFSET_StgTSO_id]
#define OFFSET_StgTSO_cap 64
#define REP_StgTSO_cap b64
#define StgTSO_cap(__ptr__) REP_StgTSO_cap[__ptr__+SIZEOF_StgHeader+OFFSET_StgTSO_cap]
#define OFFSET_StgTSO_saved_errno 44
#define REP_StgTSO_saved_errno b32
#define StgTSO_saved_errno(__ptr__) REP_StgTSO_saved_errno[__ptr__+SIZEOF_StgHeader+OFFSET_StgTSO_saved_errno]
#define OFFSET_StgTSO_trec 72
#define REP_StgTSO_trec b64
#define StgTSO_trec(__ptr__) REP_StgTSO_trec[__ptr__+SIZEOF_StgHeader+OFFSET_StgTSO_trec]
#define OFFSET_StgTSO_flags 28
#define REP_StgTSO_flags b32
#define StgTSO_flags(__ptr__) REP_StgTSO_flags[__ptr__+SIZEOF_StgHeader+OFFSET_StgTSO_flags]
#define OFFSET_StgTSO_dirty 48
#define REP_StgTSO_dirty b32
#define StgTSO_dirty(__ptr__) REP_StgTSO_dirty[__ptr__+SIZEOF_StgHeader+OFFSET_StgTSO_dirty]
#define OFFSET_StgTSO_bq 88
#define REP_StgTSO_bq b64
#define StgTSO_bq(__ptr__) REP_StgTSO_bq[__ptr__+SIZEOF_StgHeader+OFFSET_StgTSO_bq]
#define OFFSET_StgTSO_alloc_limit 96
#define REP_StgTSO_alloc_limit b64
#define StgTSO_alloc_limit(__ptr__) REP_StgTSO_alloc_limit[__ptr__+SIZEOF_StgHeader+OFFSET_StgTSO_alloc_limit]
#define OFFSET_StgTSO_cccs 112
#define REP_StgTSO_cccs b64
#define StgTSO_cccs(__ptr__) REP_StgTSO_cccs[__ptr__+SIZEOF_StgHeader+OFFSET_StgTSO_cccs]
#define OFFSET_StgTSO_stackobj 16
#define REP_StgTSO_stackobj b64
#define StgTSO_stackobj(__ptr__) REP_StgTSO_stackobj[__ptr__+SIZEOF_StgHeader+OFFSET_StgTSO_stackobj]
#define OFFSET_StgStack_sp 8
#define REP_StgStack_sp b64
#define StgStack_sp(__ptr__) REP_StgStack_sp[__ptr__+SIZEOF_StgHeader+OFFSET_StgStack_sp]
#define OFFSET_StgStack_stack 16
#define OFFSET_StgStack_stack_size 0
#define REP_StgStack_stack_size b32
#define StgStack_stack_size(__ptr__) REP_StgStack_stack_size[__ptr__+SIZEOF_StgHeader+OFFSET_StgStack_stack_size]
#define OFFSET_StgStack_dirty 4
#define REP_StgStack_dirty b32
#define StgStack_dirty(__ptr__) REP_StgStack_dirty[__ptr__+SIZEOF_StgHeader+OFFSET_StgStack_dirty]
#define SIZEOF_StgTSOProfInfo 8
#define OFFSET_StgUpdateFrame_updatee 0
#define REP_StgUpdateFrame_updatee b64
#define StgUpdateFrame_updatee(__ptr__) REP_StgUpdateFrame_updatee[__ptr__+SIZEOF_StgHeader+OFFSET_StgUpdateFrame_updatee]
#define OFFSET_StgCatchFrame_handler 8
#define REP_StgCatchFrame_handler b64
#define StgCatchFrame_handler(__ptr__) REP_StgCatchFrame_handler[__ptr__+SIZEOF_StgHeader+OFFSET_StgCatchFrame_handler]
#define OFFSET_StgCatchFrame_exceptions_blocked 0
#define REP_StgCatchFrame_exceptions_blocked b64
#define StgCatchFrame_exceptions_blocked(__ptr__) REP_StgCatchFrame_exceptions_blocked[__ptr__+SIZEOF_StgHeader+OFFSET_StgCatchFrame_exceptions_blocked]
#define SIZEOF_StgPAP_NoHdr 16
#define SIZEOF_StgPAP (SIZEOF_StgHeader+16)
#define OFFSET_StgPAP_n_args 4
#define REP_StgPAP_n_args b32
#define StgPAP_n_args(__ptr__) REP_StgPAP_n_args[__ptr__+SIZEOF_StgHeader+OFFSET_StgPAP_n_args]
#define OFFSET_StgPAP_fun 8
#define REP_StgPAP_fun gcptr
#define StgPAP_fun(__ptr__) REP_StgPAP_fun[__ptr__+SIZEOF_StgHeader+OFFSET_StgPAP_fun]
#define OFFSET_StgPAP_arity 0
#define REP_StgPAP_arity b32
#define StgPAP_arity(__ptr__) REP_StgPAP_arity[__ptr__+SIZEOF_StgHeader+OFFSET_StgPAP_arity]
#define OFFSET_StgPAP_payload 16
#define StgPAP_payload(__ptr__,__ix__) W_[__ptr__+SIZEOF_StgHeader+OFFSET_StgPAP_payload + WDS(__ix__)]
#define SIZEOF_StgAP_NoThunkHdr 16
#define SIZEOF_StgAP_NoHdr 24
#define SIZEOF_StgAP (SIZEOF_StgHeader+24)
#define OFFSET_StgAP_n_args 12
#define REP_StgAP_n_args b32
#define StgAP_n_args(__ptr__) REP_StgAP_n_args[__ptr__+SIZEOF_StgHeader+OFFSET_StgAP_n_args]
#define OFFSET_StgAP_fun 16
#define REP_StgAP_fun gcptr
#define StgAP_fun(__ptr__) REP_StgAP_fun[__ptr__+SIZEOF_StgHeader+OFFSET_StgAP_fun]
#define OFFSET_StgAP_payload 24
#define StgAP_payload(__ptr__,__ix__) W_[__ptr__+SIZEOF_StgHeader+OFFSET_StgAP_payload + WDS(__ix__)]
#define SIZEOF_StgAP_STACK_NoThunkHdr 16
#define SIZEOF_StgAP_STACK_NoHdr 24
#define SIZEOF_StgAP_STACK (SIZEOF_StgHeader+24)
#define OFFSET_StgAP_STACK_size 8
#define REP_StgAP_STACK_size b64
#define StgAP_STACK_size(__ptr__) REP_StgAP_STACK_size[__ptr__+SIZEOF_StgHeader+OFFSET_StgAP_STACK_size]
#define OFFSET_StgAP_STACK_fun 16
#define REP_StgAP_STACK_fun gcptr
#define StgAP_STACK_fun(__ptr__) REP_StgAP_STACK_fun[__ptr__+SIZEOF_StgHeader+OFFSET_StgAP_STACK_fun]
#define OFFSET_StgAP_STACK_payload 24
#define StgAP_STACK_payload(__ptr__,__ix__) W_[__ptr__+SIZEOF_StgHeader+OFFSET_StgAP_STACK_payload + WDS(__ix__)]
#define SIZEOF_StgSelector_NoThunkHdr 8
#define SIZEOF_StgSelector_NoHdr 16
#define SIZEOF_StgSelector (SIZEOF_StgHeader+16)
#define OFFSET_StgInd_indirectee 0
#define REP_StgInd_indirectee gcptr
#define StgInd_indirectee(__ptr__) REP_StgInd_indirectee[__ptr__+SIZEOF_StgHeader+OFFSET_StgInd_indirectee]
#define SIZEOF_StgMutVar_NoHdr 8
#define SIZEOF_StgMutVar (SIZEOF_StgHeader+8)
#define OFFSET_StgMutVar_var 0
#define REP_StgMutVar_var b64
#define StgMutVar_var(__ptr__) REP_StgMutVar_var[__ptr__+SIZEOF_StgHeader+OFFSET_StgMutVar_var]
#define SIZEOF_StgAtomicallyFrame_NoHdr 24
#define SIZEOF_StgAtomicallyFrame (SIZEOF_StgHeader+24)
#define OFFSET_StgAtomicallyFrame_code 0
#define REP_StgAtomicallyFrame_code b64
#define StgAtomicallyFrame_code(__ptr__) REP_StgAtomicallyFrame_code[__ptr__+SIZEOF_StgHeader+OFFSET_StgAtomicallyFrame_code]
#define OFFSET_StgAtomicallyFrame_next_invariant_to_check 8
#define REP_StgAtomicallyFrame_next_invariant_to_check b64
#define StgAtomicallyFrame_next_invariant_to_check(__ptr__) REP_StgAtomicallyFrame_next_invariant_to_check[__ptr__+SIZEOF_StgHeader+OFFSET_StgAtomicallyFrame_next_invariant_to_check]
#define OFFSET_StgAtomicallyFrame_result 16
#define REP_StgAtomicallyFrame_result b64
#define StgAtomicallyFrame_result(__ptr__) REP_StgAtomicallyFrame_result[__ptr__+SIZEOF_StgHeader+OFFSET_StgAtomicallyFrame_result]
#define OFFSET_StgInvariantCheckQueue_invariant 0
#define REP_StgInvariantCheckQueue_invariant b64
#define StgInvariantCheckQueue_invariant(__ptr__) REP_StgInvariantCheckQueue_invariant[__ptr__+SIZEOF_StgHeader+OFFSET_StgInvariantCheckQueue_invariant]
#define OFFSET_StgInvariantCheckQueue_my_execution 8
#define REP_StgInvariantCheckQueue_my_execution b64
#define StgInvariantCheckQueue_my_execution(__ptr__) REP_StgInvariantCheckQueue_my_execution[__ptr__+SIZEOF_StgHeader+OFFSET_StgInvariantCheckQueue_my_execution]
#define OFFSET_StgInvariantCheckQueue_next_queue_entry 16
#define REP_StgInvariantCheckQueue_next_queue_entry b64
#define StgInvariantCheckQueue_next_queue_entry(__ptr__) REP_StgInvariantCheckQueue_next_queue_entry[__ptr__+SIZEOF_StgHeader+OFFSET_StgInvariantCheckQueue_next_queue_entry]
#define OFFSET_StgAtomicInvariant_code 0
#define REP_StgAtomicInvariant_code b64
#define StgAtomicInvariant_code(__ptr__) REP_StgAtomicInvariant_code[__ptr__+SIZEOF_StgHeader+OFFSET_StgAtomicInvariant_code]
#define OFFSET_StgTRecHeader_enclosing_trec 0
#define REP_StgTRecHeader_enclosing_trec b64
#define StgTRecHeader_enclosing_trec(__ptr__) REP_StgTRecHeader_enclosing_trec[__ptr__+SIZEOF_StgHeader+OFFSET_StgTRecHeader_enclosing_trec]
#define SIZEOF_StgCatchSTMFrame_NoHdr 16
#define SIZEOF_StgCatchSTMFrame (SIZEOF_StgHeader+16)
#define OFFSET_StgCatchSTMFrame_handler 8
#define REP_StgCatchSTMFrame_handler b64
#define StgCatchSTMFrame_handler(__ptr__) REP_StgCatchSTMFrame_handler[__ptr__+SIZEOF_StgHeader+OFFSET_StgCatchSTMFrame_handler]
#define OFFSET_StgCatchSTMFrame_code 0
#define REP_StgCatchSTMFrame_code b64
#define StgCatchSTMFrame_code(__ptr__) REP_StgCatchSTMFrame_code[__ptr__+SIZEOF_StgHeader+OFFSET_StgCatchSTMFrame_code]
#define SIZEOF_StgCatchRetryFrame_NoHdr 24
#define SIZEOF_StgCatchRetryFrame (SIZEOF_StgHeader+24)
#define OFFSET_StgCatchRetryFrame_running_alt_code 0
#define REP_StgCatchRetryFrame_running_alt_code b64
#define StgCatchRetryFrame_running_alt_code(__ptr__) REP_StgCatchRetryFrame_running_alt_code[__ptr__+SIZEOF_StgHeader+OFFSET_StgCatchRetryFrame_running_alt_code]
#define OFFSET_StgCatchRetryFrame_first_code 8
#define REP_StgCatchRetryFrame_first_code b64
#define StgCatchRetryFrame_first_code(__ptr__) REP_StgCatchRetryFrame_first_code[__ptr__+SIZEOF_StgHeader+OFFSET_StgCatchRetryFrame_first_code]
#define OFFSET_StgCatchRetryFrame_alt_code 16
#define REP_StgCatchRetryFrame_alt_code b64
#define StgCatchRetryFrame_alt_code(__ptr__) REP_StgCatchRetryFrame_alt_code[__ptr__+SIZEOF_StgHeader+OFFSET_StgCatchRetryFrame_alt_code]
#define OFFSET_StgTVarWatchQueue_closure 0
#define REP_StgTVarWatchQueue_closure b64
#define StgTVarWatchQueue_closure(__ptr__) REP_StgTVarWatchQueue_closure[__ptr__+SIZEOF_StgHeader+OFFSET_StgTVarWatchQueue_closure]
#define OFFSET_StgTVarWatchQueue_next_queue_entry 8
#define REP_StgTVarWatchQueue_next_queue_entry b64
#define StgTVarWatchQueue_next_queue_entry(__ptr__) REP_StgTVarWatchQueue_next_queue_entry[__ptr__+SIZEOF_StgHeader+OFFSET_StgTVarWatchQueue_next_queue_entry]
#define OFFSET_StgTVarWatchQueue_prev_queue_entry 16
#define REP_StgTVarWatchQueue_prev_queue_entry b64
#define StgTVarWatchQueue_prev_queue_entry(__ptr__) REP_StgTVarWatchQueue_prev_queue_entry[__ptr__+SIZEOF_StgHeader+OFFSET_StgTVarWatchQueue_prev_queue_entry]
#define SIZEOF_StgTVar_NoHdr 24
#define SIZEOF_StgTVar (SIZEOF_StgHeader+24)
#define OFFSET_StgTVar_current_value 0
#define REP_StgTVar_current_value b64
#define StgTVar_current_value(__ptr__) REP_StgTVar_current_value[__ptr__+SIZEOF_StgHeader+OFFSET_StgTVar_current_value]
#define OFFSET_StgTVar_first_watch_queue_entry 8
#define REP_StgTVar_first_watch_queue_entry b64
#define StgTVar_first_watch_queue_entry(__ptr__) REP_StgTVar_first_watch_queue_entry[__ptr__+SIZEOF_StgHeader+OFFSET_StgTVar_first_watch_queue_entry]
#define OFFSET_StgTVar_num_updates 16
#define REP_StgTVar_num_updates b64
#define StgTVar_num_updates(__ptr__) REP_StgTVar_num_updates[__ptr__+SIZEOF_StgHeader+OFFSET_StgTVar_num_updates]
#define SIZEOF_StgWeak_NoHdr 40
#define SIZEOF_StgWeak (SIZEOF_StgHeader+40)
#define OFFSET_StgWeak_link 32
#define REP_StgWeak_link b64
#define StgWeak_link(__ptr__) REP_StgWeak_link[__ptr__+SIZEOF_StgHeader+OFFSET_StgWeak_link]
#define OFFSET_StgWeak_key 8
#define REP_StgWeak_key b64
#define StgWeak_key(__ptr__) REP_StgWeak_key[__ptr__+SIZEOF_StgHeader+OFFSET_StgWeak_key]
#define OFFSET_StgWeak_value 16
#define REP_StgWeak_value b64
#define StgWeak_value(__ptr__) REP_StgWeak_value[__ptr__+SIZEOF_StgHeader+OFFSET_StgWeak_value]
#define OFFSET_StgWeak_finalizer 24
#define REP_StgWeak_finalizer b64
#define StgWeak_finalizer(__ptr__) REP_StgWeak_finalizer[__ptr__+SIZEOF_StgHeader+OFFSET_StgWeak_finalizer]
#define OFFSET_StgWeak_cfinalizers 0
#define REP_StgWeak_cfinalizers b64
#define StgWeak_cfinalizers(__ptr__) REP_StgWeak_cfinalizers[__ptr__+SIZEOF_StgHeader+OFFSET_StgWeak_cfinalizers]
#define SIZEOF_StgCFinalizerList_NoHdr 40
#define SIZEOF_StgCFinalizerList (SIZEOF_StgHeader+40)
#define OFFSET_StgCFinalizerList_link 0
#define REP_StgCFinalizerList_link b64
#define StgCFinalizerList_link(__ptr__) REP_StgCFinalizerList_link[__ptr__+SIZEOF_StgHeader+OFFSET_StgCFinalizerList_link]
#define OFFSET_StgCFinalizerList_fptr 8
#define REP_StgCFinalizerList_fptr b64
#define StgCFinalizerList_fptr(__ptr__) REP_StgCFinalizerList_fptr[__ptr__+SIZEOF_StgHeader+OFFSET_StgCFinalizerList_fptr]
#define OFFSET_StgCFinalizerList_ptr 16
#define REP_StgCFinalizerList_ptr b64
#define StgCFinalizerList_ptr(__ptr__) REP_StgCFinalizerList_ptr[__ptr__+SIZEOF_StgHeader+OFFSET_StgCFinalizerList_ptr]
#define OFFSET_StgCFinalizerList_eptr 24
#define REP_StgCFinalizerList_eptr b64
#define StgCFinalizerList_eptr(__ptr__) REP_StgCFinalizerList_eptr[__ptr__+SIZEOF_StgHeader+OFFSET_StgCFinalizerList_eptr]
#define OFFSET_StgCFinalizerList_flag 32
#define REP_StgCFinalizerList_flag b64
#define StgCFinalizerList_flag(__ptr__) REP_StgCFinalizerList_flag[__ptr__+SIZEOF_StgHeader+OFFSET_StgCFinalizerList_flag]
#define SIZEOF_StgMVar_NoHdr 24
#define SIZEOF_StgMVar (SIZEOF_StgHeader+24)
#define OFFSET_StgMVar_head 0
#define REP_StgMVar_head b64
#define StgMVar_head(__ptr__) REP_StgMVar_head[__ptr__+SIZEOF_StgHeader+OFFSET_StgMVar_head]
#define OFFSET_StgMVar_tail 8
#define REP_StgMVar_tail b64
#define StgMVar_tail(__ptr__) REP_StgMVar_tail[__ptr__+SIZEOF_StgHeader+OFFSET_StgMVar_tail]
#define OFFSET_StgMVar_value 16
#define REP_StgMVar_value b64
#define StgMVar_value(__ptr__) REP_StgMVar_value[__ptr__+SIZEOF_StgHeader+OFFSET_StgMVar_value]
#define SIZEOF_StgMVarTSOQueue_NoHdr 16
#define SIZEOF_StgMVarTSOQueue (SIZEOF_StgHeader+16)
#define OFFSET_StgMVarTSOQueue_link 0
#define REP_StgMVarTSOQueue_link b64
#define StgMVarTSOQueue_link(__ptr__) REP_StgMVarTSOQueue_link[__ptr__+SIZEOF_StgHeader+OFFSET_StgMVarTSOQueue_link]
#define OFFSET_StgMVarTSOQueue_tso 8
#define REP_StgMVarTSOQueue_tso b64
#define StgMVarTSOQueue_tso(__ptr__) REP_StgMVarTSOQueue_tso[__ptr__+SIZEOF_StgHeader+OFFSET_StgMVarTSOQueue_tso]
#define SIZEOF_StgBCO_NoHdr 32
#define SIZEOF_StgBCO (SIZEOF_StgHeader+32)
#define OFFSET_StgBCO_instrs 0
#define REP_StgBCO_instrs b64
#define StgBCO_instrs(__ptr__) REP_StgBCO_instrs[__ptr__+SIZEOF_StgHeader+OFFSET_StgBCO_instrs]
#define OFFSET_StgBCO_literals 8
#define REP_StgBCO_literals b64
#define StgBCO_literals(__ptr__) REP_StgBCO_literals[__ptr__+SIZEOF_StgHeader+OFFSET_StgBCO_literals]
#define OFFSET_StgBCO_ptrs 16
#define REP_StgBCO_ptrs b64
#define StgBCO_ptrs(__ptr__) REP_StgBCO_ptrs[__ptr__+SIZEOF_StgHeader+OFFSET_StgBCO_ptrs]
#define OFFSET_StgBCO_arity 24
#define REP_StgBCO_arity b32
#define StgBCO_arity(__ptr__) REP_StgBCO_arity[__ptr__+SIZEOF_StgHeader+OFFSET_StgBCO_arity]
#define OFFSET_StgBCO_size 28
#define REP_StgBCO_size b32
#define StgBCO_size(__ptr__) REP_StgBCO_size[__ptr__+SIZEOF_StgHeader+OFFSET_StgBCO_size]
#define OFFSET_StgBCO_bitmap 32
#define StgBCO_bitmap(__ptr__,__ix__) W_[__ptr__+SIZEOF_StgHeader+OFFSET_StgBCO_bitmap + WDS(__ix__)]
#define SIZEOF_StgStableName_NoHdr 8
#define SIZEOF_StgStableName (SIZEOF_StgHeader+8)
#define OFFSET_StgStableName_sn 0
#define REP_StgStableName_sn b64
#define StgStableName_sn(__ptr__) REP_StgStableName_sn[__ptr__+SIZEOF_StgHeader+OFFSET_StgStableName_sn]
#define SIZEOF_StgBlockingQueue_NoHdr 32
#define SIZEOF_StgBlockingQueue (SIZEOF_StgHeader+32)
#define OFFSET_StgBlockingQueue_bh 8
#define REP_StgBlockingQueue_bh b64
#define StgBlockingQueue_bh(__ptr__) REP_StgBlockingQueue_bh[__ptr__+SIZEOF_StgHeader+OFFSET_StgBlockingQueue_bh]
#define OFFSET_StgBlockingQueue_owner 16
#define REP_StgBlockingQueue_owner b64
#define StgBlockingQueue_owner(__ptr__) REP_StgBlockingQueue_owner[__ptr__+SIZEOF_StgHeader+OFFSET_StgBlockingQueue_owner]
#define OFFSET_StgBlockingQueue_queue 24
#define REP_StgBlockingQueue_queue b64
#define StgBlockingQueue_queue(__ptr__) REP_StgBlockingQueue_queue[__ptr__+SIZEOF_StgHeader+OFFSET_StgBlockingQueue_queue]
#define OFFSET_StgBlockingQueue_link 0
#define REP_StgBlockingQueue_link b64
#define StgBlockingQueue_link(__ptr__) REP_StgBlockingQueue_link[__ptr__+SIZEOF_StgHeader+OFFSET_StgBlockingQueue_link]
#define SIZEOF_MessageBlackHole_NoHdr 24
#define SIZEOF_MessageBlackHole (SIZEOF_StgHeader+24)
#define OFFSET_MessageBlackHole_link 0
#define REP_MessageBlackHole_link b64
#define MessageBlackHole_link(__ptr__) REP_MessageBlackHole_link[__ptr__+SIZEOF_StgHeader+OFFSET_MessageBlackHole_link]
#define OFFSET_MessageBlackHole_tso 8
#define REP_MessageBlackHole_tso b64
#define MessageBlackHole_tso(__ptr__) REP_MessageBlackHole_tso[__ptr__+SIZEOF_StgHeader+OFFSET_MessageBlackHole_tso]
#define OFFSET_MessageBlackHole_bh 16
#define REP_MessageBlackHole_bh b64
#define MessageBlackHole_bh(__ptr__) REP_MessageBlackHole_bh[__ptr__+SIZEOF_StgHeader+OFFSET_MessageBlackHole_bh]
#define SIZEOF_StgCompactNFData_NoHdr 64
#define SIZEOF_StgCompactNFData (SIZEOF_StgHeader+64)
#define OFFSET_StgCompactNFData_totalW 0
#define REP_StgCompactNFData_totalW b64
#define StgCompactNFData_totalW(__ptr__) REP_StgCompactNFData_totalW[__ptr__+SIZEOF_StgHeader+OFFSET_StgCompactNFData_totalW]
#define OFFSET_StgCompactNFData_autoBlockW 8
#define REP_StgCompactNFData_autoBlockW b64
#define StgCompactNFData_autoBlockW(__ptr__) REP_StgCompactNFData_autoBlockW[__ptr__+SIZEOF_StgHeader+OFFSET_StgCompactNFData_autoBlockW]
#define OFFSET_StgCompactNFData_nursery 32
#define REP_StgCompactNFData_nursery b64
#define StgCompactNFData_nursery(__ptr__) REP_StgCompactNFData_nursery[__ptr__+SIZEOF_StgHeader+OFFSET_StgCompactNFData_nursery]
#define OFFSET_StgCompactNFData_last 40
#define REP_StgCompactNFData_last b64
#define StgCompactNFData_last(__ptr__) REP_StgCompactNFData_last[__ptr__+SIZEOF_StgHeader+OFFSET_StgCompactNFData_last]
#define OFFSET_StgCompactNFData_hp 16
#define REP_StgCompactNFData_hp b64
#define StgCompactNFData_hp(__ptr__) REP_StgCompactNFData_hp[__ptr__+SIZEOF_StgHeader+OFFSET_StgCompactNFData_hp]
#define OFFSET_StgCompactNFData_hpLim 24
#define REP_StgCompactNFData_hpLim b64
#define StgCompactNFData_hpLim(__ptr__) REP_StgCompactNFData_hpLim[__ptr__+SIZEOF_StgHeader+OFFSET_StgCompactNFData_hpLim]
#define OFFSET_StgCompactNFData_hash 48
#define REP_StgCompactNFData_hash b64
#define StgCompactNFData_hash(__ptr__) REP_StgCompactNFData_hash[__ptr__+SIZEOF_StgHeader+OFFSET_StgCompactNFData_hash]
#define OFFSET_StgCompactNFData_result 56
#define REP_StgCompactNFData_result b64
#define StgCompactNFData_result(__ptr__) REP_StgCompactNFData_result[__ptr__+SIZEOF_StgHeader+OFFSET_StgCompactNFData_result]
#define SIZEOF_StgCompactNFDataBlock 24
#define OFFSET_StgCompactNFDataBlock_self 0
#define REP_StgCompactNFDataBlock_self b64
#define StgCompactNFDataBlock_self(__ptr__) REP_StgCompactNFDataBlock_self[__ptr__+OFFSET_StgCompactNFDataBlock_self]
#define OFFSET_StgCompactNFDataBlock_owner 8
#define REP_StgCompactNFDataBlock_owner b64
#define StgCompactNFDataBlock_owner(__ptr__) REP_StgCompactNFDataBlock_owner[__ptr__+OFFSET_StgCompactNFDataBlock_owner]
#define OFFSET_StgCompactNFDataBlock_next 16
#define REP_StgCompactNFDataBlock_next b64
#define StgCompactNFDataBlock_next(__ptr__) REP_StgCompactNFDataBlock_next[__ptr__+OFFSET_StgCompactNFDataBlock_next]
#define OFFSET_RtsFlags_ProfFlags_showCCSOnException 269
#define REP_RtsFlags_ProfFlags_showCCSOnException b8
#define RtsFlags_ProfFlags_showCCSOnException(__ptr__) REP_RtsFlags_ProfFlags_showCCSOnException[__ptr__+OFFSET_RtsFlags_ProfFlags_showCCSOnException]
#define OFFSET_RtsFlags_DebugFlags_apply 210
#define REP_RtsFlags_DebugFlags_apply b8
#define RtsFlags_DebugFlags_apply(__ptr__) REP_RtsFlags_DebugFlags_apply[__ptr__+OFFSET_RtsFlags_DebugFlags_apply]
#define OFFSET_RtsFlags_DebugFlags_sanity 206
#define REP_RtsFlags_DebugFlags_sanity b8
#define RtsFlags_DebugFlags_sanity(__ptr__) REP_RtsFlags_DebugFlags_sanity[__ptr__+OFFSET_RtsFlags_DebugFlags_sanity]
#define OFFSET_RtsFlags_DebugFlags_weak 202
#define REP_RtsFlags_DebugFlags_weak b8
#define RtsFlags_DebugFlags_weak(__ptr__) REP_RtsFlags_DebugFlags_weak[__ptr__+OFFSET_RtsFlags_DebugFlags_weak]
#define OFFSET_RtsFlags_GcFlags_initialStkSize 16
#define REP_RtsFlags_GcFlags_initialStkSize b32
#define RtsFlags_GcFlags_initialStkSize(__ptr__) REP_RtsFlags_GcFlags_initialStkSize[__ptr__+OFFSET_RtsFlags_GcFlags_initialStkSize]
#define OFFSET_RtsFlags_MiscFlags_tickInterval 176
#define REP_RtsFlags_MiscFlags_tickInterval b64
#define RtsFlags_MiscFlags_tickInterval(__ptr__) REP_RtsFlags_MiscFlags_tickInterval[__ptr__+OFFSET_RtsFlags_MiscFlags_tickInterval]
#define SIZEOF_StgFunInfoExtraFwd 32
#define OFFSET_StgFunInfoExtraFwd_slow_apply 24
#define REP_StgFunInfoExtraFwd_slow_apply b64
#define StgFunInfoExtraFwd_slow_apply(__ptr__) REP_StgFunInfoExtraFwd_slow_apply[__ptr__+OFFSET_StgFunInfoExtraFwd_slow_apply]
#define OFFSET_StgFunInfoExtraFwd_fun_type 0
#define REP_StgFunInfoExtraFwd_fun_type b32
#define StgFunInfoExtraFwd_fun_type(__ptr__) REP_StgFunInfoExtraFwd_fun_type[__ptr__+OFFSET_StgFunInfoExtraFwd_fun_type]
#define OFFSET_StgFunInfoExtraFwd_arity 4
#define REP_StgFunInfoExtraFwd_arity b32
#define StgFunInfoExtraFwd_arity(__ptr__) REP_StgFunInfoExtraFwd_arity[__ptr__+OFFSET_StgFunInfoExtraFwd_arity]
#define OFFSET_StgFunInfoExtraFwd_bitmap 16
#define REP_StgFunInfoExtraFwd_bitmap b64
#define StgFunInfoExtraFwd_bitmap(__ptr__) REP_StgFunInfoExtraFwd_bitmap[__ptr__+OFFSET_StgFunInfoExtraFwd_bitmap]
#define SIZEOF_StgFunInfoExtraRev 32
#define OFFSET_StgFunInfoExtraRev_slow_apply_offset 0
#define REP_StgFunInfoExtraRev_slow_apply_offset b32
#define StgFunInfoExtraRev_slow_apply_offset(__ptr__) REP_StgFunInfoExtraRev_slow_apply_offset[__ptr__+OFFSET_StgFunInfoExtraRev_slow_apply_offset]
#define OFFSET_StgFunInfoExtraRev_fun_type 24
#define REP_StgFunInfoExtraRev_fun_type b32
#define StgFunInfoExtraRev_fun_type(__ptr__) REP_StgFunInfoExtraRev_fun_type[__ptr__+OFFSET_StgFunInfoExtraRev_fun_type]
#define OFFSET_StgFunInfoExtraRev_arity 28
#define REP_StgFunInfoExtraRev_arity b32
#define StgFunInfoExtraRev_arity(__ptr__) REP_StgFunInfoExtraRev_arity[__ptr__+OFFSET_StgFunInfoExtraRev_arity]
#define OFFSET_StgFunInfoExtraRev_bitmap 8
#define REP_StgFunInfoExtraRev_bitmap b64
#define StgFunInfoExtraRev_bitmap(__ptr__) REP_StgFunInfoExtraRev_bitmap[__ptr__+OFFSET_StgFunInfoExtraRev_bitmap]
#define OFFSET_StgFunInfoExtraRev_bitmap_offset 8
#define REP_StgFunInfoExtraRev_bitmap_offset b32
#define StgFunInfoExtraRev_bitmap_offset(__ptr__) REP_StgFunInfoExtraRev_bitmap_offset[__ptr__+OFFSET_StgFunInfoExtraRev_bitmap_offset]
#define OFFSET_StgLargeBitmap_size 0
#define REP_StgLargeBitmap_size b64
#define StgLargeBitmap_size(__ptr__) REP_StgLargeBitmap_size[__ptr__+OFFSET_StgLargeBitmap_size]
#define OFFSET_StgLargeBitmap_bitmap 8
#define SIZEOF_snEntry 24
#define OFFSET_snEntry_sn_obj 16
#define REP_snEntry_sn_obj b64
#define snEntry_sn_obj(__ptr__) REP_snEntry_sn_obj[__ptr__+OFFSET_snEntry_sn_obj]
#define OFFSET_snEntry_addr 0
#define REP_snEntry_addr b64
#define snEntry_addr(__ptr__) REP_snEntry_addr[__ptr__+OFFSET_snEntry_addr]
#define SIZEOF_spEntry 8
#define OFFSET_spEntry_addr 0
#define REP_spEntry_addr b64
#define spEntry_addr(__ptr__) REP_spEntry_addr[__ptr__+OFFSET_spEntry_addr]
