%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% FPD-specific globals.
%%

%%
%% Progress reporting
%%
fpd_default_setting(modForAcceptableRuleReporting, 1000).

%%
%% neural network parameters
%%
fpd_default_setting(ann_prob_remove_iopair, 0.1).
fpd_default_setting(ann_sample, 20).
fpd_default_setting(ann_hidden_units, 10).
fpd_default_setting(ann_learning_function, accuracy).
fpd_default_setting(ann_learning_rate, 0.2).
fpd_default_setting(ann_eval_if_best_so_far, false).
                          % if a neural-net-approximated score is
                          %   better than the best we've seen so far,
                          %   should we evaluate that clause?

fpd_default_setting(softness,1).

%%
%% check clause legality after every added literals (random methods only)
%%
fpd_default_setting(construct_legal_only,false).

%%
%% use a tuning set to evaluate NN performance
%%
fpd_default_setting(use_tuning_set,false).
fpd_default_setting(tuning_set_examples,100).
fpd_default_setting(evaluate_tuneset_every,10).
fpd_default_setting(burnin, false).
fpd_default_setting(burnin_exs, 100).

%%
%% use ANN for EVALUATION ONLY!
%%
fpd_default_setting(use_neural_net, false).
fpd_default_setting(neural_net_initial_training, 100).
fpd_default_setting(prob_bypass_neural_net, 0.1).

%%
%% custom out files - unimplemented (for now)
%%
/*
fpd_rm_special_consideration(custom_trace_stream,_):-
	(fpd_setting(custom_trace_stream,S) -> close(S); true), !.
fpd_rm_special_consideration(clauselength_dist_stream,_):-
	(fpd_setting(clauselength_dist_stream,S) -> close(S); true), !.
*/

%%
%% write clauselength distribution to a file
%%
fpd_default_setting(write_clauselength_dist, false).
fpd_default_setting(clauselength_dist_file,  'trace\\TRACE_clauselength_dist.txt').

fpd_set_special_consideration(write_clauselength_dist,true):-
	fpd_noset(clauselength_dist_stream),
	(fpd_setting(clauselength_dist_file,F,ok) -> 
		aleph_open(F,write,Stream),
		fpd_set(clauselength_dist_stream,Stream);
		true), !.
fpd_set_special_consideration(write_clauselength_dist,false):-
	fpd_noset(clauselength_dist_stream), !.
fpd_set_special_consideration(clauselength_dist_file,File):-
	fpd_noset(clauselength_dist_stream), 
	(fpd_setting(write_clauselength_dist,true,ok) -> 
		aleph_open(File,write,Stream),
		set(clauselength_dist_stream,Stream);
		true), !.

%%
%% fpd_normalize_score(compression, Acc, Acc1) :-
%%    * normalize evalfn scores to [-100,100]
%%          ... may be lower than -100 if #neg << #pos
%%    * scaling is done so linear output unit does not take
%%          too long to converge
%%
fpd_normalize_score(compression, Acc, Acc1) :-
	recorded(aleph,size(pos,P),_),
	Acc1 is (Acc/P)*100,
	!.
fpd_normalize_score(coverage, Acc, Acc1) :-
	recorded(aleph,size(pos,P),_),
	Acc1 is (Acc/P)*100,
	!.
%%
%% TO DO: for others.
%%
fpd_normalize_score(_, Acc, Acc) :- !.


%%%%%%%%
%%%%%%%%
%%
%% Artificial Neural Network clause selection 
%%    ... use a NN for clause generation
%%
fpd_reduce_ann :-
	!,
	%
	% init NN
	%
	bottom_keys(_,_,Keys,_),
	arg(1,Keys,SatKey),
	recorded(SatKey,last_lit(Last),_),
	fpd_setting(ann_hidden_units,HUs,ok),
	initialize_ANN(Last,HUs),
	%
	retract_all(ann),
	store_values([tries,moves,rls_type,clauselength_distribution]),
	stopwatch(Start),
	call_count_reset,
	fpd_setting(ann_sample,SampleSize,ok),
	%
	% for now, just use SCS settings for clauselength distribution
	(setting(scs_type,informed)->
		(setting(clauselength_distribution,D) -> true;
			setting(clauselength,CL),
			estimate_clauselength_distribution(CL,100,_,D),
			% max_in_list(D,Prob-Length),
			% p1_message('using clauselength distribution'),
			% p_message([Prob-Length]),
			% set(clauselength_distribution,[Prob-Length]));
			p1_message('using clauselength distribution'),
			p_message(D),
			set(clauselength_distribution,D));
		true),
	set(tries,SampleSize),
	set(moves,0),     % maybe later allow local search via gsat, wsat, etc...?
	set(rls_type,gsat),
	fpd_build_tuning_sets,
	reduce(rls),
	stopwatch(Stop),
	Time is Stop - Start,
	recorded(rls,nodes(Nodes),_),
	recorded(rls,selected(BestLabel,RBest,_,_),_),
	p1_message('ann nodes constructed'), p_message(Nodes),
	%p1_message('ann search time'), p_message(Time),
	call_count_data(_,_,NegCalls),
	Calls is -NegCalls,
	uw_format("~n~nsearch time: ~p~n",[Time]),
	uw_format("call count : ~p~n",[Calls]),
	p_message('ann best result'),
	pp_dclause(RBest),
	setting(evalfn,Evalfn),
	show_stats(Evalfn,BestLabel),
	record_search_stats(RBest,Nodes,Time),
	%p1_message('ann search time'), p_message(Time),
	uw_format("~n~nsearch time: ~p~n",[Time]),
	reinstate_values([tries,moves,rls_type,clauselength_distribution]),
	%
	% free ANN static arrays / cleanup
	delete_ANN,
	eraseall(ann).
%% end addition
%%
%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Special version of estimate_proportion
%%       to handle legal_only clause construction
%%
fpd_estimate_proportion(N,L,legal,P,Clauses):-
 	fpd_setting(construct_legal_only,true,ok),
	!,
	retract_all(random,rselect(_)),
	retract_all(random,rselect_legal(_,_,_,_,_)),
	L1 is L-1,
	reset_prob_counts(L1),
	fpd_get_random_legal_wo_repl(N,L,Clauses),
	collect_prob_counts(L1,P),
	recorded(sat,sat(E,T),_),
	retract_all(random,rselect(_)),
	store_legal_clauses(Clauses,L,E,T),
	%
%jws	format("[Estimated P(legal)=~p]~n",P),!.
	uw_format("%  Estimated P(legal,~d)=~3e]~n", [L, P]),!.

reset_prob_counts(0) :- !.
reset_prob_counts(L) :-
	L1 is L-1,
	recorda(ann,len_prob_num(L1,0),_),!,
	recorda(ann,len_prob_denom(L1,0),_),!,
	reset_prob_counts(L1).

collect_prob_counts(0,1) :- !.
collect_prob_counts(L,P) :-
	L1 is L-1,
	recorded(ann,len_prob_num(L1,N),DbRef1),
	recorded(ann,len_prob_denom(L1,D),DbRef2),
	(D > 0 ->
		collect_prob_counts(L1,P1),
		P is P1*(N/D)
	;
		collect_prob_counts(L1,_),
		P is 0),
	erase(DbRef1), erase(DbRef2), !.

%%
%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%
%%
%%  
%%
fpd_get_random_legal_wo_repl(0,_,[]):- !.
fpd_get_random_legal_wo_repl(N,L,[S/[C,C1]|Clauses]):-
	randclause_legal(L,C,S,C1), !,
	N1 is N - 1,
	fpd_get_random_legal_wo_repl(N1,L,Clauses).
fpd_get_random_legal_wo_repl(_,_,[]).
%%
%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%
%%
%%  (possibly) use the NN for speedy clause evaluation
%%
fpd_prove_examples_NN(_,_,_,_,_,CL,_,_,Pos,Neg,Pos,Neg,Label,_,Lits) :-
	fpd_setting(clauses_evaluated, CEval, ok), % ALWAYS evaluate first clause on data
	%
	fpd_setting(neural_net_initial_training, TrainTime, ok),
	CEval >= TrainTime,
	Rand is random,
	fpd_setting(prob_bypass_neural_net, P,ok),
	Rand > P, 
	%
	approximate_proof_on_NN(Lits,Label,CL).

approximate_proof_on_NN(Lits,[-1,-1,CL,Score],CL) :-
	run_example_list(Lits,Score),
	!.

fpd_prove_examples_NN(S,Flag,Contradiction,Entry,Best,CL,EL,
                          (Head1:-Body1),Pos,Neg,PCvr,NCvr,Label,C,Lits) :-
	prove_examples(S,Flag,Contradiction,Entry,Best,CL,EL,
	               (Head1:-Body1),Pos,Neg,PCvr,NCvr,Label),
	(fpd_setting(clauses_evaluated, CEval, ok) -> 
		true;
		CEval is 0),
	CEval1 is CEval+1,
	fpd_set(clauses_evaluated, CEval1),
	%
	% add IO pair to NN, train
	fpd_setting(ann_learning_function, LearnFn,ok),
	complete_label(LearnFn,C,Label,AccLabel),
	AccLabel = [_,_,_,Acc1|_],
	normalize_score(LearnFn,Acc1,Acc),
	% uw_format("[~p: I/O Pair: <~p,~p>]~n", [CEval1,Lits,Acc]),
	recordz(ann,iopair(Lits,Acc),_),
	train_ANN_on_all_IO_pairs,     % train the network
	!.
%%
%%
%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%
%%
%%   * special case for NN-predicted clauses
%%   * if an APPROXIMATELY-scored clause is the best 
%%     we've seen so far, then perform actual evaluation
fpd_evaluate_if_best_so_far(S,Flag,Contradiction,Entry,CL,EL,Clause,Lits,PCover,NCover,
                            Label,Label1,Label1ActComp,PCAct,NCAct) :-
	arg(7,S,ClauseLength),
	(fpd_setting(ann_eval_if_best_so_far, true, ok) ;
	 CL = ClauseLength),    % if CL == Clauselength, this won't get placed on
	                        %    the openlist, so now is our last chance to
							%    evaluate (if it is the best so far)
	Label = [_,_,_,Gain|_],
	Label1 = [-1,-1,_,Gain1|_],  % NN predicted
	Gain1 > Gain, 
	%%
	jws_setting(report_details_level, JWS_LEVEL),
	(JWS_LEVEL > 0 ->
		uw_format("[NN-approximated clause ~p is the new best!]~n", [Label1]);
		true),
	fpd_set(forced_eval, true),
	prove_examples(S,Flag,Contradiction,Entry,Label,CL,EL,
	               (Clause),PCover,NCover,PCAct,NCAct,Label1Actual),
	fpd_noset(forced_eval),
	arg(4,S,_/Evalfn),
	complete_label(Evalfn,Clause,Label1Actual,Label1ActComp),
	(JWS_LEVEL > 0 ->
		uw_format("[Clause's actual label: ~p]~n", [Label1ActComp]);
		true),
	Label1ActComp=[L1,L2,L3|_],
	LabelPartial = [L1,L2,L3],
	%%
	%% add IO pair to NN, train
	(fpd_setting(clauses_evaluated, CEval, ok) -> 
		true;
		CEval is 0),
	CEval1 is CEval+1,
	fpd_set(clauses_evaluated, CEval1),
	%
	fpd_setting(ann_learning_function, LearnFn,ok),
	complete_label(LearnFn,Clause,LabelPartial,AccLabel),
	AccLabel = [_,_,_,Acc1|_],
	normalize_score(LearnFn,Acc1,Acc),
	% uw_format("[~p: I/O Pair: <~p,~p>]~n", [CEval1,Lits,Acc]),
	recordz(ann,iopair(Lits,Acc),_),
	train_ANN_on_all_IO_pairs,     % train the network
	!.

fpd_evaluate_if_best_so_far(_,_,_,_,_,_,_,_,PC,NC,_,Label,Label,PC,NC) :- !.
%%
%%
%%%%%%%%%%%%%
%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% NEURAL NETWORK

%%
%% fpd_get_selected_lits_from_path(+L1, +Path, -Lits)
%%     because the var 'Path' in get_gain1 may take different formats
%%     dependend upon the search type, we need a function to extract
%%     a list of the currently selected lits from this variable
%%

% randomized search case
fpd_get_selected_lits_from_path([], _-[_,_,Lits,_], Lits) :- !.

% heuristic (+bf, df (?))
fpd_get_selected_lits_from_path(L1, Lits, [L1|Lits]) :- !.


%%
%% train the NN / print debugging msg
%%
fpd_train_network(C, Label, Lits) :-
	fpd_setting(ann_learning_function, LearnFn,ok),
	complete_label(LearnFn,C,Label,AccLabel),
	AccLabel = [_,_,_,Acc1|_],
	%
	% scale scoring function to [0,1]
	fpd_normalize_score(LearnFn,Acc1,Acc),
	jws_setting(report_details_level, JWS_LEVEL), % Added JWS
	(JWS_LEVEL > 1 ->       % only print out at levels 2+
		uw_format("[Candidate Clause Label: ~p]~n", [Label]),
		uw_format("[Candidate Clause ~p: ~p]~n", [LearnFn,Acc])
		% uw_format("~p,~p~n", [Acc,Label1])
		% uw_format("[I/O Pair: <~p,~p>]~n", [Lits,Acc])
	;
		true),
	recordz(ann,iopair(Lits,Acc),_),
	train_ANN_on_all_IO_pairs,    % train the network
	!.


%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% INITIALIZATION/CLEANUP ROUTINES

%%
%% fpd_initialize_reduce
%%     *  prepare to run reduce
fpd_initialize_counters :-
	(jws_setting(acceptableRules_saved,   AcceptableRules,   ok_if_not_set)
	  -> jws_noset(acceptableRules_saved),
	     fpd_set(acceptableRules,   AcceptableRules)
	  ;  fpd_reset(acceptableRules)),
	(jws_setting(rulesConsidered_saved,   RulesConsidered,   ok_if_not_set)
	  -> jws_noset(rulesConsidered_saved),
	     fpd_set(rulesConsidered,   RulesConsidered)
	  ;  fpd_reset(rulesConsidered)),
	(jws_setting(annEvaluatedRules_saved, AnnEvaluatedRules, ok_if_not_set)
	  -> jws_noset(annEvaluatedRules_saved),
	     fpd_set(annEvaluatedRules, AnnEvaluatedRules)
	  ;  fpd_reset(annEvaluatedRules)),
	fpd_setting(acceptableRules,   AR), % Want to see if these are getting reset in the experiment where only one seed should be processed.
	fpd_setting(rulesConsidered,   RC),
	fpd_setting(annEvaluatedRules, AER),
	uw_format("~n% fpd_initialize_reduce: acceptableRules = ~p, rulesConsidered = ~p, annEvaluatedRules = ~p~n",
		  [AR, RC, AER]).

fpd_initialize_reduce :-
	once(fpd_initialize_counters), fail.
fpd_initialize_reduce :-
	%%
	%% init NN
	%%
	fpd_setting(use_neural_net, true, ok),
	bottom_keys(_,_,Keys,_),
	arg(1,Keys,SatKey),
	recorded(SatKey,last_lit(Last),_),
	fpd_setting(ann_hidden_units,HUs,ok),
	initialize_ANN(Last,HUs),
	fpd_set(clauses_evaluated, 0),
	!.

fpd_initialize_reduce :- !.


%%
%% fpd_cleanup_reduce
%%     * cleanup after running reduce
fpd_cleanup_reduce :-
	fpd_setting(use_neural_net, true, ok),
	delete_ANN,
	% delete I/O pairs
	eraseall(ann),
	!.
fpd_cleanup_reduce :- !.


%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  PRINT TRACE MESSAGES

%%
%%  write clauselength distribution to a file
%%
fpd_trace_clauselength_dist_heading :-
	(fpd_setting(write_clauselength_dist,true,ok) ->
		fpd_setting(clauselength_dist_stream, CDS,ok),
		set_output(CDS),
		% heading
		recorded(sat,sat(SatNum,_),_),
		format("~n~n[sat] [~p]~n", [SatNum]),
		set_output(user_output)
	;
		true),
	!.
fpd_trace_clauselength_dist_heading :- !.

fpd_trace_clauselength_dist_body(L,Mess) :-
	(fpd_setting(write_clauselength_dist,true,ok) ->
		fpd_setting(clauselength_dist_stream, CDS, ok),
		set_output(CDS),
		format("[Estimate legal clauses with length] [~p]~n[~p]~n",[L,Mess]),
		set_output(user_output)
	;
		true),
	!.
fpd_trace_clauselength_dist_body(_,_) :- !.


%%
%% fpd_print_status_of_selected_clause
%%      print a status message to the screen
fpd_print_status_of_selected_clause(Clause) :-
	jws_setting(report_details_level, JWS_LEVEL),
	(JWS_LEVEL > 1 ->    % only print out individual selected clauses at level 2+
		%  ANN SPECIFIC
		(setting(search,ann) ->
			Clause = _-[_,_,Lits,_],
			uw_format("[Clause Randomly Selected: ~p]~n", [Lits]),
			run_example_list(Lits, Out),
			uw_format("[Predicted Output: ~p]~n", [Out])
		;
			true),
		%
		%  SCS SPECIFIC
		(setting(search,scs) ->
			Clause = _-[_,_,Lits,_],
			uw_format("[Clause Randomly Selected: ~p]~n", [Lits])
			%uw_format("~p,", [Lits])
		;
			true),
		true
	;
	true),
	!.

%%
%% fpd_print_status_on_old_move
%%       - prints a status message when we've regenerated 
%%         an old clause
fpd_print_status_on_old_move :-
	jws_setting(report_details_level, JWS_LEVEL), % Added JWS
	(JWS_LEVEL > 1 ->
		%   
		%   ANN SPECIFIC
		(setting(search,ann) ->
			uw_format("[OLD_MOVE]~n", []);
			true),
		%   
		%   SCS SPECIFIC
		(setting(search,scs) ->
			uw_format("[OLD_MOVE]~n", []);
			true)
	;
		true),
	!.
	

fpd_inc_softness :-
	setting(search,ann),
	fpd_setting(softness,S,ok),
	S1 is S+1,
	format("[Softness now ~p]~n", [S1]),
	fpd_set(softness,S1).
fpd_inc_softness.

	
fpd_get_performance_for_trace:-
	(setting(train_pos,PFile) ->
		test(PFile,noshow,Tp,TotPos),
		Fn is TotPos - Tp;
		TotPos = 0, Tp = 0, Fn = 0),
	(setting(train_neg,NFile) ->
		test(NFile,noshow,Fp,TotNeg),
		Tn is TotNeg - Fp;
		TotNeg = 0, Tn = 0, Fp = 0),
	TotPos + TotNeg > 0,
	format("~n%  Training set performance~n%~n", []),
	fpd_write_cmatrix_for_trace([Tp,Fp,Fn,Tn]),
	format("~n% [Training set summary] ",[]), 
	format("  [~p]~n%~n%",[[Tp,Fp,Fn,Tn]]),
	fail.
fpd_get_performance_for_trace:-
	(setting(test_pos,PFile) ->
		test(PFile,noshow,Tp,TotPos),
		Fn is TotPos - Tp;
		TotPos = 0, Tp = 0, Fn = 0),
	(setting(test_neg,NFile) ->
		test(NFile,noshow,Fp,TotNeg),
		Tn is TotNeg - Fp;
		TotNeg = 0, Tn = 0, Fp = 0),
	TotPos + TotNeg > 0,
	format("~n%  Test set performance~n%~n", []),
	fpd_write_cmatrix_for_trace([Tp,Fp,Fn,Tn]),
	format("~n% [Test set summary] ",[]), 
	format("  [~p]~n",[[Tp,Fp,Fn,Tn]]),
	fail.
fpd_get_performance_for_trace.

fpd_write_cmatrix_for_trace([Tp,Fp,Fn,Tn]):-
	P is Tp + Fn, N is Fp + Tn,
	PP is Tp + Fp, PN is Fn + Tn,
	Total is PP + PN,
	(Total = 0 -> Accuracy is 0.5; Accuracy is (Tp + Tn)/Total),
	find_max_width([Tp,Fp,Fn,Tn,P,N,PP,PN,Total],0,W1),
	W is W1 + 2,
	write('%   '),tab(5), write(' '), tab(W), write('Actual'), nl,
	write('%   '),tab(5), write(' '), write_entry(W,'+'), tab(6), write_entry(W,'-'), nl,
	write('%   '),tab(5), write('+'),
	write_entry(W,Tp), tab(6), write_entry(W,Fp), tab(6), write_entry(W,PP), nl,
	write('%   '),write('Pred '), nl,
	write('%   '),tab(5), write('-'),
	write_entry(W,Fn), tab(6), write_entry(W,Tn), tab(6), write_entry(W,PN), nl,
	write('%   '), nl,
	write('%   '),tab(5), write(' '), write_entry(W,P), tab(6), write_entry(W,N),
	tab(6), write_entry(W,Total), nl, 
	write('%   '), nl,
	write('%   '),write('Accuracy = '), write(Accuracy), nl,
	write('%   ').

fpd_eval_rule_for_trace(0,Label):-
	recorded(aleph,hypothesis(_,Clause,_,_),_), !,
	label_create(Clause,Label),
	format("% [Rule 0]",[]),
	pp_dclause(Clause),
	extract_count(pos,Label,PC),
	extract_count(neg,Label,NC),
	extract_length(Label,L),
	label_print_eval([PC,NC,L]),
	nl.
fpd_eval_rule_for_trace(ClauseNum,Label):-
	integer(ClauseNum),
	ClauseNum > 0,
	recorded(aleph,theory(ClauseNum,_,Clause,_,_),_),
	!,
	label_create(Clause,Label),
	extract_count(pos,Label,PC),
	extract_count(neg,Label,NC),
	format("%~n", []),
	format("%  ------------ Rule #~p ------------ ~n", [ClauseNum]),
	format("%     positive cover: ~p~n", [PC]),
	format("%     negative cover: ~p~n", [NC]),
	format("%~n", []),

	pp_dclause(Clause),
	setting(verbosity,V),
	(V >= 2 ->
		format("/*~n",[]),
		p_message('positive examples covered'),
		label_print_examples(pos,Label),
		p_message('negative examples covered'),
		label_print_examples(neg,Label),
		format("*/~n",[])
	   ;
		true),
	nl,nl.
fpd_eval_rule_for_trace(_,_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  TUNING SETS

%%
%% fpd_eval_tuning_set(+R)
%%      Every K restarts, test on our train set on the NN
%%      We should see improvement over time...
%%
fpd_eval_tuning_set(R) :-
	((setting(search,ann), fpd_setting(use_tuning_set,true,ok)) ->
		fpd_setting(evaluate_tuneset_every,K,ok),
		Rmod is (R-1) mod K,
		(Rmod = 0 ->
			recorded(sat,sat(SatNum,pos),_),
			%
			% random
			%
			format("[testing random tune set ...]~n",[]),
			recorded(ann,tune_set(random,TuneSet),_),
			recorded(ann,tune_set_scores(random,TuneSetScores),_),
			fpd_evaluate_clauses_on_NN(TuneSet,TuneSetPredicted),
			fpd_write_lists_to_file(random,SatNum,R,TuneSetScores,TuneSetPredicted),
			%
			% good
			%
			format("[testing good tune set ...]~n",[]),
			recorded(ann,tune_set(good,GoodTuneSet),_),
			recorded(ann,tune_set_scores(good,GoodTuneSetScores),_),
			fpd_evaluate_clauses_on_NN(GoodTuneSet,GoodTuneSetPredicted),
			fpd_write_lists_to_file(good,SatNum,R,GoodTuneSetScores,GoodTuneSetPredicted)
		;
			true)
	;
		true),
	!.


%%
%% fpd_build_tuning_sets
%%      Build _2_ "tuning" sets (if flag set)
%%         #1 - random sample of the space
%%         #2 - best 10% of random sample
%%      Each tuning set is equal in size.
%%      Evaluate (and record) the performance of these theories 
%%         on the dataset
%%
fpd_build_tuning_sets :-
	(fpd_setting(use_tuning_set,true,ok) ->
		noset(search),
		set(refineop,rls),
		p_message('evaluating tuning set'),
		%%%%%%%%%%%%%%%%
		%
		%  build 'random' set
		format("[building 'RANDOM' tune set ...]~n",[]),
		fpd_setting(tuning_set_examples,NumExs,ok),
		sample_clauses(NumExs,TuneSet),
		fpd_evaluate_clauses(TuneSet,TuneSetScores),
		%%%%%%%%%%%%%%%%
		%
		%  build 'good' set
		format("[building 'GOOD' tune set ...]~n",[]),
		NumExsNeededForGood is 10*NumExs,
		sample_clauses(NumExsNeededForGood,GoodTuneSetComplete),
		fpd_evaluate_clauses(GoodTuneSetComplete,GoodTuneSetCompleteScores),
		select_good_subset(GoodTuneSetComplete,GoodTuneSetCompleteScores,
		                   GoodTuneSetTrimmed,GoodTuneSetTrimmedScores),
		recorda(ann,tune_set(random,TuneSet),_),
		recorda(ann,tune_set_scores(random,TuneSetScores),_),
		recorda(ann,tune_set(good,GoodTuneSetTrimmed),_),
		recorda(ann,tune_set_scores(good,GoodTuneSetTrimmedScores),_),
		length(TuneSet,RandLen),
		length(GoodTuneSetTrimmed,GoodLen),
		format("[tuning set 'RANDOM' contains ~p clauses]~n", [RandLen]),
		format("[tuning set 'GOOD' contains ~p clauses]~n", [GoodLen]),
		%read(_),
		set(search,ann)
	;
		true),
	!.


%%
%% fpd_evaluate_clauses(+TuneSet,-Scores)
%%     see how well the NN is learning the concept by running a test
%%     set of clauses and recording the accuracy.
%%
fpd_evaluate_clauses([],[]) :- !.
fpd_evaluate_clauses([Ex|TuneSet],[Score|Scores]) :-
	fpd_evaluate_clause(Ex,Score),
	%Ex = _-[_,_,Lits,_],
	%format("[Clause ~p scored ~p]~n",[Lits,Score]),
	fpd_evaluate_clauses(TuneSet,Scores), !.

fpd_evaluate_clause(Ex,Score) :-
	get_search_settings(S),
	recorded(aleph,atoms(pos,P),_),
	recorded(aleph,atoms(neg,N),_),
	Ex = CL-[_,_,_,Clause],
	(Clause = (Head:-Body) ->
		true
	;
		Clause = Head,
		Body = true
	),
	arg(34,S,Proof),
	arg(37,S,Optim),
	rewrite_clause(Proof,Optim,(Head:-Body),(Head1:-Body1)),
	prove_examples(S,upper,false,false,[1,0,2,0],CL,CL,
	               (Head1:-Body1),P,N,_,_,Label),
	fpd_setting(ann_learning_function, LearnFn,ok),
	complete_label(LearnFn,false,Label,AccLabel),
	AccLabel = [_,_,_,Acc1|_],
	%
	% scale scoring function to [0,1]
	fpd_normalize_score(LearnFn,Acc1,Score).


%%
%% fpd_evaluate_clauses_on_NN(+TuneSet,-Scores) 
%%       Score a set of clauses aganist the NN
%%
fpd_evaluate_clauses_on_NN([],[]) :- !.
fpd_evaluate_clauses_on_NN([_-[_,_,Lits,_]|TuneSet],[Score|TuneSetPredicted]) :-
		run_example_list(Lits, Score),
		fpd_evaluate_clauses_on_NN(TuneSet,TuneSetPredicted).

%
%  Output comparison to file
fpd_write_lists_to_file(Type,S,R,TuneSetScores,TuneSetPredicted) :-
	concat(['tune_set_accuracy\\tune_set_accuracy_', Type, '_', S, '_', R],Prefix),
	concat([Prefix, '.txt'],FileName),
	(aleph_open(FileName,'write',OutStream) ->
		true
	;
		format("[Can't open file ~p!!!]~n",[FileName]),
		read(_)
	),
	set_output(OutStream),
	fpd_write_lists(TuneSetScores,TuneSetPredicted),
	flush_output(OutStream),
	set_output(user_output),
	close(OutStream).

fpd_write_lists([],[]) :- !.
fpd_write_lists([H1|L1],[H2|L2]) :-
	format("~p, ~p~n", [H1,H2]),
	fpd_write_lists(L1,L2).

%
%  select_good_subset(+Full,+FullScores,-Subset,-SubsetScores)
%       ... select the top N% of scores  (for now 10%)
select_good_subset(Full,FullScores,Subset,SubsetScores) :-
	length(Full, F_len),
	S_len is integer(0.1*F_len)+1,
	quicksort(descending,FullScores,SortedScores),
	remove_nth(F_len,SortedScores,Min,_),
	remove_nth(S_len,SortedScores,ScoreCutoff,_),
	SortedScores = [Max|_],
	format("[good clause cutoff at ~2f in [~2f - ~2f]]~n",
	           [ScoreCutoff, Min, Max]),
	fpd_select_gte(ScoreCutoff,Full,FullScores,Subset,SubsetScores),
	!.

fpd_select_gte(_,[],_ ,[],[]) :- !.
fpd_select_gte(_,_ ,[],[],[]) :- !.

fpd_select_gte(Cut,[C|Clauses],[S|Scores],[C|SubClauses],[S|SubScores]) :- 
	S >= Cut, 
	!,
	fpd_select_gte(Cut,Clauses,Scores,SubClauses,SubScores).
fpd_select_gte(Cut,[_|Clauses],[_|Scores],SubClauses,SubScores) :-
	fpd_select_gte(Cut,Clauses,Scores,SubClauses,SubScores).	




fpd_check_setting(_, _).
fpd_set_special_consideration(_, _). 
fpd_rm_special_consideration(_, _).

:- fpd_set_all_defaults.   % Note: can't call until 'fpd_utils.yap' loaded.



