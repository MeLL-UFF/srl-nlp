fpd_set(Variable, Value) :- 
	uw_indexed_set(fpd, Variable, Value).
fpd_set_all_defaults :- 
	uw_indexed_set_all_defaults(fpd).

fpd_setting(Variable, Value) :-
	 uw_indexed_setting(fpd, Variable, Value, false).
fpd_setting(Variable, Value, OkNotSet) :-
	 uw_indexed_setting(fpd, Variable, Value, OkNotSet).
fpd_noset(Variable) :- 
	uw_indexed_noset(fpd, Variable).

fpd_get_all_settings(Variable, Value) :- 
	uw_indexed_get_all_settings(fpd, Variable, Value).

fpd_increment(Variable) :- 
	uw_indexed_increment(fpd, Variable).
fpd_increment(Variable, Delta) :- 
	uw_indexed_increment(fpd, Variable, Delta).

fpd_decrement(Variable) :- 
	uw_indexed_decrement(fpd, Variable).
fpd_decrement(Variable, Delta) :- 
	uw_indexed_decrement(fpd, Variable, Delta).

%
%  inc(+Var) .. increment variable "Var".  If var des not exist, reset to 0.
fpd_inc(Var) :-
	(fpd_setting(Var, VVal, ok) -> 
		Value is VVal+1; 
		Value is 1),
	fpd_noset(Var),
	fpd_set(Var, Value), 
	!.

%
%  reset(+Var) .. reset counter variable "Var".  
fpd_reset(Var) :-
	fpd_noset(Var),
	fpd_set(Var, 0), !.

% Need to grab the "call backs" from the uw-generic set/noset functions.
uw_indexed_check_setting(fpd, Variable, Value) :- 
	fpd_check_setting(Variable, Value).
uw_indexed_default_setting(fpd, Variable, Value) :- 
	fpd_default_setting(Variable, Value).
uw_indexed_set_special_consideration(fpd, Variable, Value) :- 
	fpd_set_special_consideration(Variable, Value).
uw_indexed_rm_special_consideration( fpd, Variable, Value) :- 
	fpd_rm_special_consideration( Variable, Value).

% Need these "terminators" but they are fpd_aleph_extension.yap instead
% fpd_check_setting(_, _).
% fpd_set_special_consideration(_, _).
% fpd_rm_special_consideration(_, _).

%
% sum a list
fpd_sum_list([],0).
fpd_sum_list([H|T],C):-
	sum_list(T,C1),
	C is C1+H.

%
% report progress every N acceptable rules (to do: let N be a global?  Or not worth doing a lookup every time?)
fpd_report_search_progress(Head, Body, RawLabel1) :-
	fpd_setting(acceptableRules, AcceptableRules, ok),
	fpd_setting(modForAcceptableRuleReporting, ModCounter),
	PrintMessageLoop is (AcceptableRules mod ModCounter),
	(PrintMessageLoop = 0
	    -> fpd_setting(rulesConsidered,      RulesConsidered,      ok),
	       jws_setting(totalRulesConsidered, TotalRulesConsidered, ok),
	       fpd_setting(annEvaluatedRules, ANN_Eval, ok),
	       setting(minpos, MinPos),
	       setting(noise,  Noise),
	       setting(minacc, MinAcc),
	       setting(nodes,  MaxNodes),
	       jws_setting(current_seed_example, [Type, ExampleNum]),
	       jws_open_size(OpenSize),
%	       recorded(search,current(_,CurrentNodes,_),_),
	       uw_format("~n[******************************]~n"),
	       uw_format("UPDATE: ~D acceptable of ~D (~D total) clauses considered for seed ~p~d |OPEN|=~D~n",
	                 [AcceptableRules, RulesConsidered, TotalRulesConsidered, Type, ExampleNum, OpenSize]),
	       (ANN_Eval = 0 -> true;
	           uw_format("        (~D approximated on neural network)~n", [ANN_Eval])), 
	           uw_format("        (for minpos=~D, maxneg=~D, minacc=~5f, maxnodes=~D)~n",
	                     [MinPos, Noise, MinAcc, MaxNodes]),
	       HeapSpace is heapused // 1000000, uw_format("        Heap space = ~p MB~n", [HeapSpace]),
	       jws_setting(count_of_pruned_search_paths, PrunedSearchPaths, ok_if_not_set),
	       jws_setting(start_clock, StartClock),
	       stopwatch(CurrentClock),
	       TimeSinceStart is (CurrentClock - StartClock) / 3600,
	       jws_setting(count_of_clause_search_counter_thrown,      TotalThrownClauseSearches),
	       jws_setting(count_of_clause_evaluation_counter_thrown,  ThrownClauseEvals),
	       jws_setting(count_of_example_evaluation_counter_thrown, ThrownExampleEvals),
	       (TotalThrownClauseSearches < 1 -> true
	          ; uw_format("TotalThrownClauseSearches = ~D   ", [TotalThrownClauseSearches])),
	       (ThrownClauseEvals < 1 -> true
	          ; uw_format("ThrownClauseEvals = ~D  ",	   [ThrownClauseEvals])),
	       (ThrownExampleEvals < 1 -> true
	          ; uw_format("ThrownExampleEvals = ~D",	   [ThrownExampleEvals])),
	       uw_format("~nLast rule evaluated: [cputime = ~3fhrs] ", [TimeSinceStart]),
	       (PrunedSearchPaths > 0 -> uw_format(" [pruned ~D search paths] ", [PrunedSearchPaths]) ; true),
	       once( (setting(searchtime, SearchTime) ; SearchTime is 0) ),
	       ((SearchTime > 0, SearchTime < inf) % If call-counting on for clause-learning, report counts recorded so far.
		 -> call_count_data(_, _, NegVitorCalls),          
		    VitorCalls is -NegVitorCalls / 1000000,        
	            (VitorCalls > 0 -> uw_format(" [Vitor calls = ~3fM]", VitorCalls) ;  true)
		 ;  true),
	       uw_format("~n"),
	       jws_setting(typevars, TypeVars),
	       (TypeVars
	          -> jws_pp_dclause_with_typed_vars((Head :- Body))
		   ; pp_dclause((Head :- Body))),
	       once( (uw_setting(global_stream, Stream, ok_if_not_set) ; Stream = fail) ),
	       ((Stream == false ; Stream == fail; var(Stream)) % Also print to file if requested.
		  -> true
		  ;  set_output(Stream),
		     (TypeVars
			-> jws_pp_dclause_with_typed_vars((Head :- Body))
			;  pp_dclause((Head :- Body))),
		     flush_output(Stream),
		     set_output(user_output)),
	       safe_label(RawLabel1, Label1),
	       uw_format("LABEL: ~p", [Label1]),
	       once( ((jws_setting(current_label_when_produced, BestLabelSoFarRaw, if_not_set_just_fail),
		       safe_label(BestLabelSoFarRaw, BestLabelSoFar),
	               uw_format("   BEST: ~p", [BestLabelSoFar]))
		      ;
		      (jws_setting(bestScoreWithMinPos,      BestScoreWithMinPos),
		       jws_setting(bestScoreWithMinPosLabel, BestScoreWithMinPosLabelRaw),
		       safe_label(BestScoreWithMinPosLabelRaw, BestScoreWithMinPosLabel),
		       (BestScoreWithMinPos >= 0.0
		          -> uw_format("~n BEST: [none acceptable yet; closest found = ~p]", [BestScoreWithMinPosLabel]))
			  ;  uw_format("   BEST: [none acceptable yet]")) )),
	       uw_format("~n[******************************]~n"),
	       jws_report_best_ins(update) % JWS added 
	     ; true),
	!.
fpd_report_search_progress(_, _, _).  % If there is a failure in the above, just go on (ie, no need to backtrack in the calling function).  JWS JWS    

fpd_write_rules(RulesFile, SingletonsFile) :-
	aleph_open(RulesFile,write,RulesStream),
	set_output(RulesStream),
	show(rules),
	close(RulesStream),
	aleph_open(SingletonsFile,write,SingletonsStream),
	set_output(SingletonsStream),
	show(singletons),
	close(SingletonsStream),
	set_output(user_output).

/*
%
% set custom_tracefile by number (currently unused)
fpd_set_trace(R) :-
	fpd_setting(custom_record, true, ok),
	fpd_setting(custom_trace_fileprefix, Pre, ok),
	fpd_setting(custom_trace_filepostfix, Post, ok),
	concat([Pre,R],FilePrefix),
	concat([FilePrefix,Post],File),
	format("[custom output set to ~p]~n", [File]),
	fpd_noset(custom_trace_stream),   % close current stream 
	aleph_open(File,write,Stream),
	set(custom_trace_stream,Stream),
	!.
fpd_set_trace(_) :- !.
*/


%%
%%  write rules/singletons to trace file
%%
show(rules):-
	format("% RULES INDUCED~n", []),
	format("%~n", []),
	nl,
	recorded(aleph,rules(L),_),
	aleph_reverse(L,L1),
	aleph_member(ClauseNum,L1),
	recorded(aleph,theory(ClauseNum,_,_,PCover,_),_),
	interval_count(PCover,P),
	P>1,
	eval_rule_for_trace(ClauseNum,_),
	fail.
show(rules):-
	get_performance_for_trace.

show(singletons):-
	nl,
	format("% singletons", []),
	nl,
	recorded(aleph,rules(L),_),
	aleph_reverse(L,L1),
	aleph_member(ClauseNum,L1),
	recorded(aleph,theory(ClauseNum,_,Clause,PCover,_),_),
	interval_count(PCover,P),
	P=1,
	% eval_rule_for_trace(ClauseNum,_),
	pp_dclause(Clause),
	fail.
show(singletons).
