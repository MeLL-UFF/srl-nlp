% Useful scripts.


:- source. % Want to access clauses, etc.
:- style_check(single_var). % Help catch a lot of typos.

call_random(N) :- integer(N), N < 1, !.
call_random(N) :- integer(N), !, 
		  recorda(call_random, 0, _),
		  repeat, 
		  _ is random, 
		  recorded(call_random, Counter, DB),
		  CounterPlus1 is Counter + 1,
		  erase(DB),
		  recorda(call_random, CounterPlus1, DB2),
		  CounterPlus1 >= N,
		  erase(DB2).
call_random(N) :- uw_format("Illegal call: call_random(~p)~n  Waiting ... ", [N]), read(_).
:- srandom(1129).  % Set the seed random number so runs are deterministic.  (Can be reset below if different seed wanted.)
:- call_random(1). % Call random at least once due to apparent yap bug.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Learning rules.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

uw_initialize_for_training(InduceType, EvalFn, Task, TuneFilePrefix, TestFilePrefix, Marker, SearchType, MinPosToCover, MaxNegToCover, 
			   MinAccuracy, MaxClauseLength, MaxNodes, Ivalue, ProofDepth, RandomStarts, LocalMoves, UseANN) :-
	prepare_for_training(Task, TuneFilePrefix, TestFilePrefix, true, false),
	(MinPosToCover  < 1 % Intepret as FRACTION of pos to cover.
	  -> MinPercentagePosToCover is 100 * MinPosToCover,
	     jws_set_minpos_percentage(MinPercentagePosToCover)
	  ;  set(minpos, MinPosToCover)),
	set(noise,        MaxNegToCover),
	set(minacc,       MinAccuracy),
	set(nodes,        MaxNodes),
%	set(prune_defs,   true), % Look for redundant terms in the body.  Aleph checks if this needs to be set to true.
%	set(lazy_negs,    true), % BUGGY? Dont compute more negs than needed to "exceed" noise and minacc.
	set(clauselength, MaxClauseLength),
	set(i,            Ivalue),
	set(evalfn,       EvalFn),
	set(depth,        ProofDepth),
	uw_experiment_using(SearchType, RandomStarts, LocalMoves, UseANN),

	jws_short_search_name(SearchType, SearchShort),
	jws_short_evalfn_name(EvalFn,     EvalFnShort),

	((uw_isa_randomized_search(SearchType),
          jws_setting(generate_uniform_clauselength_dist, true))
	   -> jws_generate_uniform_clauselength_dist ; true),
	( jws_setting(randomly_generate_seeds, true)	
	   -> set(samplesize, 1) % Randomly generate seeds.  [Check out gen_sample(Type,SampleSize)]
	   ;  set(samplesize, 0)),
	
	%  special flag (FPD) ... check legality of clauses after each added literal
	%             useful in situations where there are very few long legal clauses,
	%             and the likelihood of constructing one randomly is essentially 0.
	%
	fpd_set(construct_legal_only, true),

	(recorded(uw_condor_run_number, condor(CondorRunNumber), _)
	  -> uw_concat(['_condor', CondorRunNumber, '_'], CondorMarker)
	  ;  CondorMarker = '_'),
	(recorded(jws, extra_prefix(ExtraPrefix), _)
	  -> uw_concat([Task, Marker, CondorMarker, ExtraPrefix], Prefix)
	  ;  uw_concat([Task, Marker, CondorMarker], Prefix)),
	% write('Prefix: '), write(Prefix), nl,

	jws_create_file_signature(InduceType, PostfixA, _),
	(uw_isa_randomized_search(SearchType)
	  -> uw_concat([PostfixA, '_', RandomStarts, 'starts_', LocalMoves, 'moves'], Postfix)
	  ;  Postfix = PostfixA),
	% write('Postfix: '), write(Postfix), nl,

	% Create the file that will collect the "good clauses" produced by aleph.
	%uw_concat(['trace/', Prefix, 'GOOD_',  Postfix, '.yap'], GoodFile),
	%% write('GoodFile: '), write(GoodFile), nl,
	%set(good, true), set(goodfile, GoodFile), % Turned off for now.

	% Create the file that will collect the rules file produced by aleph.
	uw_concat(['learned/', Prefix, 'RULES_',  Postfix, '.yap'], RuleFile),
	set(rulefile, RuleFile),

	MinAccuracyPercentage is integer(100 * MinAccuracy),
	MaxNodesInK is integer(MaxNodes / 1000),
	% Create the prefixes for the (UW-formatted) RULES and SINGLETONS files (plus the PR riles_.
	(uw_isa_randomized_search(SearchType)
	  -> uw_concat(['learned/', Prefix, RandomStarts, 'starts_', LocalMoves, 'moves_'], JWS_Prefix_rules)
	  ;  uw_concat(['learned/', Prefix],                                                JWS_Prefix_rules)),
	(uw_isa_randomized_search(SearchType) % Dont make this a subdir of "learned" so one can easily delete the contents of "learned."
	  -> uw_concat(['singletons/', Prefix, RandomStarts, 'starts_', LocalMoves, 'moves_'], JWS_Prefix_singletons)
	  ;  uw_concat(['singletons/', Prefix],                                                JWS_Prefix_singletons)),
	(uw_isa_randomized_search(SearchType)
	  -> uw_concat(['prec_recall/', Prefix, SearchShort, '_', EvalFnShort, '_cl', MaxClauseLength, '_', MaxNodesInK, 'n_', RandomStarts, 'starts_', LocalMoves, 'moves'], JWS_precision_recall)
	  ;  uw_concat(['prec_recall/', Prefix, SearchShort, '_', EvalFnShort, '_cl', MaxClauseLength, '_', MaxNodesInK, 'n_', MinAccuracyPercentage, 'minacc'],              JWS_precision_recall)),
	uw_concat([JWS_precision_recall, '.yap'],              JWS_precision_recall2),
	uw_concat([JWS_precision_recall, '_checkpoint.chkpt'], JWS_precision_recall_checkpoint_file),
	% write('JWS_Prefix: '), write(JWS_Prefix), nl,
	jws_set(rule_file_prefix_rules,           JWS_Prefix_rules),
	jws_set(rule_file_prefix_singletons,      JWS_Prefix_singletons),
	jws_set(precision_recall_file,            JWS_precision_recall2),
	jws_set(precision_recall_checkpoint_file, JWS_precision_recall_checkpoint_file),

	% Define the file that will serve as the "UW" trace file.
	uw_concat(['trace/', Prefix, 'TRACE_', Postfix, '.uw'], GlobalFile),
	% write('GlobalFile: '), write(GlobalFile), nl,
	uw_set(global_file, GlobalFile),

	uw_do_any_local_overrides_for_initialize_for_training,

	(exists(JWS_precision_recall_checkpoint_file)
	  -> uw_format("~n% Reading checkpoint file:~n%   ~p~n", [JWS_precision_recall_checkpoint_file]),
	     consult(JWS_precision_recall_checkpoint_file),
%	     jws_report_best_ins(first_time), % REWRITE FILES TO CORRECT BUG IN THEM.
/*	% can turn this off now ...
	     (jws_setting(first_seed_example, [SeedType, SeedExampleNum], ok_if_not_set)
		-> true ; true),
	     uw_format("~nSeedType=~p, SeedExampleNum=~p~n", [SeedType, SeedExampleNum]),
	     (uw_isa_randomized_search(SearchType) -> once(confirm_consistent_seed_numbers(SeedType, SeedExampleNum)) ; true),
*/	     
	     jws_set(starting_from_scratch, false),
	     jws_increment(checkpoint_reloads),
	     jws_setting(checkpoint_reloads, ReloadCount),
	     ExtraRandomCalls is (5 * ReloadCount) + 100,
	     call_random(ExtraRandomCalls), % Add additional calls so dont get the same sequence of random numbers as before (to get around repeated crashes).  How to get a random component in case crashes before next checkpoint? Calling random wont work.  Maybe should write a file immediately - now done by jws_consider_halting.
	     uw_format("% Reload #~D of the checkpoint file.~n", [ReloadCount])
	  ;  % uw_format("% No checkpoint file found:~n%   ~p~n", [JWS_precision_recall_checkpoint_file]),
	     jws_set(starting_from_scratch, true),
	     jws_set(checkpoint_reloads, 0)),
	jws_initialize_total_rules_considered, % Needs to be set AFTER checkpoint file loaded and BEFORE jws_consider_halting.
	jws_consider_halting(first_time), % See if even a need to run.  If so, will write out the checkpoint file (so will have new checkpoint_reloads counter in case crash occurs early).
	garbage_collect.

confirm_consistent_seed_numbers(SeedType, SeedExampleNum) :-
	(var(SeedExampleNum)
	  -> recorded(jws_best_in_column, best(_, SeedType/SeedExampleNum, _, _, _, _, _, _), _),
	     uw_format("***** SeedExampleNum was unbound: set to ~p~n", [SeedExampleNum]),
	     (jws_noset(first_seed_example) -> true ; true), % Might not be set.
	     jws_set(first_seed_example, [SeedType, SeedExampleNum])),
	(see_if_other_seeds_used(SeedType, SeedExampleNum)
	       -> uw_format("***** USED MORE THAN 1 SEED! *****~n"), halt ; true).
confirm_consistent_seed_numbers(_, _).

see_if_other_seeds_used(SeedType, SeedExampleNum) :-
	recorded(jws_best_in_column, best(_, SeedType/SeedExampleNum2, _, _, _, _, _, _), _),
	SeedExampleNum \= SeedExampleNum2.
see_if_other_seeds_used(SeedType, SeedExampleNum) :-
	recorded(jws_best_in_row,    best(_, SeedType/SeedExampleNum2, _, _, _, _, _, _), _),
	SeedExampleNum \= SeedExampleNum2.
	

uw_train(                          InduceType, EvalFn, Task, TuneFilePrefix, TestFilePrefix, Marker, SearchType, MinPosToCover, MaxNegToCover, MinAccuracy, MaxClauseLength, MaxNodes, Ivalue, ProofDepth, RandomStarts, LocalMoves, UseANN) :-
	not jws_setting(have_read_all_once, _, ok_if_not_set), !,
	uw_initialize_for_training(InduceType, EvalFn, Task, TuneFilePrefix, TestFilePrefix, Marker, SearchType, MinPosToCover, MaxNegToCover, MinAccuracy, MaxClauseLength, MaxNodes, Ivalue, ProofDepth, RandomStarts, LocalMoves, UseANN),
	uw_do_training(InduceType),
	uw_wrapup_training.
uw_train(                          InduceType, EvalFn, Task,                                                 Marker, SearchType, MinPosToCover, MaxNegToCover, MinAccuracy, MaxClauseLength, MaxNodes, Ivalue, ProofDepth, RandomStarts, LocalMoves, UseANN) :-
	uw_train(                  InduceType, EvalFn, Task, not_given_by_caller___, not_given_by_caller___, Marker, SearchType, MinPosToCover, MaxNegToCover, MinAccuracy, MaxClauseLength, MaxNodes, Ivalue, ProofDepth, RandomStarts, LocalMoves, UseANN).


uw_do_training(induce)       :- jws_induce.
uw_do_training(induce_cover) :- jws_induce_cover.
uw_do_training(induce_max)   :- jws_induce_max.


uw_wrapup_training :-
	jws_report_best_ins(final_wrapup), % Make sure this is called one last time when done.
	setting(rulefile, RuleFile),
	(RuleFile = false -> true ; write_rules),
	jws_noset(global_file).

prepare_for_training(Task, TuneFilePrefix, TestFilePrefix,
		     SettingFor_use_target_stuff_trainset,
		     SettingFor_use_target_stuff_testset) :-
	ensure_loaded('../aleph'),
%	ensure_loaded('../original_aleph_v4'), % For debugging.
	jws_set(use_target_stuff_trainset, SettingFor_use_target_stuff_trainset),
	(TuneFilePrefix = 'none'
	 -> jws_set(use_target_stuff_tuneset, false) ; jws_set(use_target_stuff_tuneset, true)),
	jws_set(use_target_stuff_testset, SettingFor_use_target_stuff_testset), % Should turn this on when enough RAM ... (to do)
	once(read_all(Task)),
	jws_set(have_read_all_once, true),
	(TuneFilePrefix = 'none'
	 -> jws_set(min_pos_on_tune_set, 0) 
	 ; % Allow there to be NO tune set.  (to do: redesign calling convention)
	  (TuneFilePrefix = not_given_by_caller___ 
	    -> concat([Task,      '_tune.f'], Tune_F_file),
	       concat([Task,      '_tune.n'], Tune_N_file)
	    ;  concat([TuneFilePrefix, '.f'], Tune_F_file),
	       concat([TuneFilePrefix, '.n'], Tune_N_file)),
	  set(tune_pos, Tune_F_file),
	  set(tune_neg, Tune_N_file),
          jws_read_tune_set_examples(Tune_F_file, pos), % These are robust in that they will report failure, but not crash/hang.
          jws_read_tune_set_examples(Tune_N_file, neg),
	  true),
	(TestFilePrefix = not_given_by_caller___
	  -> concat([Task,      '_test.f'], Test_F_file),
	     concat([Task,      '_test.n'], Test_N_file)
	  ;  concat([TestFilePrefix, '.f'], Test_F_file),
	     concat([TestFilePrefix, '.n'], Test_N_file)),
	set(test_pos, Test_F_file),
	set(test_neg, Test_N_file).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Evaluating learning rules.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

uw_create_confusion_matrices_for_rule_set(Task, RuleSet) :- % Just pass in file name, not full path.
	ensure_loaded('../aleph'),
	once(read_all(Task)),
	uw_concat(['learned/', RuleSet], RuleFile),
	set(rulefile, RuleFile),
	consult(RuleSet),
	jws_set(have_read_all_once, true),
	concat([Task, '_test.f'], Test_F_file),
	concat([Task, '_test.n'], Test_N_file),
	set(test_pos, Test_F_file),
	set(test_neg, Test_N_file),
	% Create the file that will collect the confusion matrix.
	uw_concat(['learned/CONFUSION_',  RuleSet], ConfusionFile),
	aleph_open(ConfusionFile, write, Stream),
	set_output(Stream),
	get_performance,
	close(Stream),
	set_output(user_output).

% To score a single clause on a test set, load the relevant .b file as well
% as the testset positives as the .f file and the testset negatives
% as the .n file.  Then type the command "rdhyp." (read hypothesis),
% followed by carriage return, and then type (or paste) the clause,
% ending with a period and carriage return.  To see how many positives
% are covered, type "covers."  To see how many negatives are covered,
% type "coversn."

% Sample usage:
%   ['../uw_experiment'], ['../uw_go'], ['../aleph'], uw_test_clause('easy_linked_10x', trainset).
% Provided clause must be formatted something like:
%   (linked(B,A) :-   (startingdates_within(A,B,hours4), common_actor(A,B,_), contains_person_with_capability(A,capability_8)))

uw_test_clause(Task, trainset) :- % NOT YET TESTED (would be nice to get clause from a file ...)
	read_all(Task),
	nl, write('Type in the hypothesis to test on the train set: '),
	rdhyp, nl,
	write('POSITIVES COVERED'), nl,
	covers, nl,
	write('NEGATIVES COVERED'), nl,
	coversn, nl.

uw_test_clause(Task, testset) :- %
	uw_concat([Task, '_test'], TaskTestsetExamplesFile),
	read_all(Task,             TaskTestsetExamplesFile),
	nl, write('Type in the hypothesis to test on the test set: '),
	rdhyp, nl,
	write('POSITIVES COVERED'), nl,
	covers, nl,
	write('NEGATIVES COVERED'), nl,
	coversn, nl.

