:- gc.  % Added by JWS (enables garbage collection).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Manage JWS-specific globals.

jws_default_setting(rule_file_prefix_rules,      '').     % Used to preface the RULES files.
jws_default_setting(rule_file_prefix_singletons, '').     % Used to preface the SINGLETONS files.
jws_default_setting(show_bottom,      false).  % Can have aleph print out the bottom clause.
jws_default_setting(report_details_level,  0). % Control amount of printout produced by JWS additions (higher # -> more printout).
jws_default_setting(typevars,         true).   % When printing learned rules, add the variable's type (eg, A becomes WordA).
jws_default_setting(show_all_goods,   false).  % Report GOOD clauses found (but not NEW BEST's)?
jws_default_setting(min_clause_length_for_using_prooftime, 5). % Only use prooftime 'vitor' counter for clauses at least this long (counting the head).

jws_default_setting(prune_open_counter,                0). % Used to prune OPEN every now and then if the following flag is set.
jws_default_setting(prune_long_opens,               true). % Look at prune_open in aleph to see how this is used.
jws_default_setting(prune_long_opens_excess_buffer, 1000). % Since this pruning is not safe, leave some excess as a buffer.

jws_default_setting(inverse_sampling_rate_of_neg_examples, 1).  % 1 out of this many negs are kept in the train set.
jws_default_setting(positive_examples_filtered_out,        0).  % Correct for # of pos examples filtered out before alpeh called.

jws_default_setting(use_builtin_rrr_discontinue_search, false). % Stop at minimally acceptable clauses?
jws_default_setting(generate_uniform_clauselength_dist, true).
jws_default_setting(randomly_generate_seeds,            true).  % If false, always start with pos1.
jws_default_setting(fraction_clauses_kept,              1.0).   % Only keep this fraction of children clauses generated.
jws_default_setting(fraction_clauses_kept_adds,         1.0).   % For use in rls.
jws_default_setting(fraction_clauses_kept_deletes,      1.0).   % Ditto.

% This is set to 0 if no tuning sets are loaded.
jws_default_setting(min_pos_on_tune_set, 2). % Discard (ie, prune) rules that dont satisfy at least this many pos tune-set examples.
jws_default_setting(tune_set_counter_pos, 0).
jws_default_setting(tune_set_counter_neg, 0).

jws_default_setting(seeds_to_process, 1000000000). % Process at most this many seeds (use "infinity" here).

% These are specific to information extraction, but leave here so grouped with others.
jws_default_setting(use_target_stuff_trainset, true). % Turn on the background knowledge related to the specific args in the target concept?
jws_default_setting(use_target_stuff_tuneset,  false).
jws_default_setting(use_target_stuff_testset,  false).

jws_default_setting(use_stemmed_words, true).

% ***** THE FOLLOWING IS CURRENTLY NOT USED IN ALEPH *****
% The number of covered POS is scaled to PERCENTAGE of all |POS|.
% The number of covered NEG is scaled to PERCENTAGE of all |POS| (note, NOT scaled to |NEG|).
% The number of literals (if used in score function, eg 'heuristic') is scaled by the following method.
% A setting of 1 means that one literal counts the same (in magnitude, but opposite in sign) as covering
% 1% of the pos examples, a setting of 0.1 means same as 0.1%, etc.  Thus, a setting of 0.1
% means that adding one literal is only "worth it" if this literal at least
% covers 0.1% of ALL the POS training ex's.  Hence, a rule with 10 literals needs to cover at least 1% of
% the POS's in order to get a non-negative heuristic score.
jws_default_setting(one_literal_counts_as_this_percentage_of_pos_examples, 0.1). % This is reset in jws_run_induce().


jws_check_setting(clause_search_time, X):-
	!, ((nonvar(X), (integer(X), X > 0)) -> true ; err_message(jws_set(clause_search_time,     X)), fail).
jws_check_setting(clause_evaluation_time, X):-
	!, ((nonvar(X), (integer(X), X > 0)) -> true ; err_message(jws_set(clause_evaluation_time, X)), fail).
jws_check_setting(jws_prooftime, X) :-
	!, ((nonvar(X), (integer(X), X > 0)) -> true ; err_message(jws_set(jws_prooftime,          X)), fail).
jws_check_setting(one_literal_counts_as_this_percentage_of_pos_examples, X):-
	!, (uw_isa_percentage(X)             -> true ; err_message(jws_set(jws_check_setting,      X)), fail).
jws_check_setting(_, _).

jws_set_special_consideration(_, _).

jws_rm_special_consideration(_, _).

:- jws_set_all_defaults.       % Note: can't call until 'jws_utils.yap' loaded.

%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


jws_run_induce(InduceType, LearnedRulesFilePrefix, SingletonsFilePrefix) :-
	atom(InduceType),
	atom(LearnedRulesFilePrefix),
	atom(SingletonsFilePrefix), !,
	(jws_setting(have_run_induce_already, true, true)
	  -> told,
	     nl, write(' Have already run some variant of induce.'),
	     nl, write(' Exit and reload; otherwise may get strange results.'), nl, nl, abort
	  ;  true),  % To do: figure out how to ful
	jws_create_file_signature(InduceType, ExperimentSpec, PrettyExperimentSpec),
	uw_concat([LearnedRulesFilePrefix, ExperimentSpec, '_rules.yap'],      LearnedRulesFile),
	uw_concat([SingletonsFilePrefix,   ExperimentSpec, '_singletons.yap'], SingletonsFile),
	jws_set(current_induce_type,        InduceType),
	jws_set(current_learned_rules_file, LearnedRulesFile),
	jws_set(current_singletons_file,    SingletonsFile),
	jws_set(current_rule_number,	      1),
	jws_set(current_singleton_number,     1),
	jws_set(total_rule_coverage,	      0),
	jws_set(total_rule_time,	      0),
	jws_set(count_of_pruned_search_paths, 0), % Count paths pruned on a per-clause basis.
	jws_set(count_of_clause_search_counter_thrown,      0),
	jws_set(count_of_clause_evaluation_counter_thrown,  0), % To do: reset these on a per-clause basis?
	jws_set(count_of_example_evaluation_counter_thrown, 0),
	set_default(prooftime), % Need to make sure this wasn't set in aleph.
	call_count_reset,
	(jws_setting(clause_search_time, ClauseSearchTime,   if_not_set_just_fail)
	  -> uw_concat(['% jws_set(clause_search_time,     ', ClauseSearchTime,').'],  ClauseSearchMsg) ; ClauseSearchMsg = ''),
	(jws_setting(clause_evaluation_time, ClauseEvalTime, if_not_set_just_fail)
	  -> uw_concat(['% jws_set(clause_evaluation_time, ', ClauseEvalTime,').'],    ClauseEvalMsg)   ; ClauseEvalMsg   = ''),
	((jws_setting(jws_prooftime, ExampleProofTime,   if_not_set_just_fail), ExampleProofTime > 0, ExampleProofTime < inf)
	  -> uw_concat(['% jws_set(jws_prooftime,          ', ExampleProofTime, ').'], ExampleProofMsg),
	     % Need to actually set prooftime, since it wont be set for "long" clauses during learning by jws_prove_examples().
	     uw_concat([':-    set(prooftime,              ', ExampleProofTime, ').'], ExampleProofMsgActual) ; ExampleProofMsg = '', ExampleProofMsgActual = ''),
	uw_format("[Running with~n~p]~n~p~n~p~n~p~n~p~n", [PrettyExperimentSpec, ClauseSearchMsg, ClauseEvalMsg, ExampleProofMsg, ExampleProofMsgActual]),
	uw_concat(['% ', InduceType, ' running with: '], InduceTypeMsg),
	uw_concat([PrettyExperimentSpec, '\n', ClauseSearchMsg, '\n', ClauseEvalMsg, '\n', ExampleProofMsg, '\n', ExampleProofMsgActual, '\n'], PrettyExperimentSpec2),
	(jws_setting(starting_from_scratch, true, ok_if_not_set)
	   -> jws_init_progress_reporting_files(LearnedRulesFile,
						'% This file contains the current set of the rules being learned by',
						InduceTypeMsg, PrettyExperimentSpec2),
	      jws_init_progress_reporting_files(SingletonsFile,
						'% This file contains the current pos examples for which no rule was learned by',
						InduceTypeMsg, PrettyExperimentSpec2)
	   ;  true),
	setting(evalfn, EvalFn),
	(uw_member(EvalFn, [jws_compression, jws_coverage])
	  -> jws_get_aleph_example_counts(PosCountRaw, _),
	     % jws_get_aleph_example_counts(PosCountRaw, NumbNeg),
	     jws_setting(positive_examples_filtered_out, PosFiltered),
	     NumbPos is PosCountRaw + PosFiltered,
	     PosScaleFactor is 100 / NumbPos, % Want P and N counts to be normalized as PERCENTILES (of the number of positive examples).
	     % NegScaleFactor is 100 / NumbNeg, % Allow playing around with this separately.
	     NegScaleFactor = PosScaleFactor,
	     jws_set(pos_example_count_scaleFactor, PosScaleFactor),
	     jws_set(neg_example_count_scaleFactor, NegScaleFactor),
	     (jws_setting(one_literal_counts_as_this_percentage_of_pos_examples, LiteralsScaleFactor, just_fail_if_not_set)
	       -> (NumbPos =< 100
	            -> LiteralsScaleFactor is 10 / NumbPos % Since so few pos ex's, scale proportional to number of pos.  Eg, if NumbPos=10, then a literal counts the same as 1 pos.
	            ;  LiteralsScaleFactor is 0.1),	   % Here, a literal counts the same as 0.1% of the POS.
		  jws_set(one_literal_counts_as_this_percentage_of_pos_examples, LiteralsScaleFactor)
	       ; true), % Already set.
	     uw_format("~n% The scale factor on the counts of covered pos examples (P) is ~2f and neg examples (N) is ~2f.~n",
		       [PosScaleFactor, NegScaleFactor]),
	    (EvalFn = jws_compression
	       -> uw_format("The scale factor on the length of the body (L) is ~2f.~2n", [LiteralsScaleFactor])
	       ;  true)
	   ; true),

	jws_reset_for_next_rule,
	call(InduceType), !,
	jws_set(have_run_induce_already, true),
	open(LearnedRulesFile, append, LearnedRulesStream), % Write some final info.
	set_output(LearnedRulesStream),
	nl,
	write('% ********************************     DONE!    ********************************'), nl, nl,
	jws_setting(current_rule_number,      RulesLearnedPlusOne), % Recall these are incremented for the NEXT rule.
	jws_setting(current_singleton_number, SingletonsPlusOne),
	RulesLearned is RulesLearnedPlusOne - 1, Singletons is SingletonsPlusOne - 1,
	uw_format("% Learning via ~p produced ~D rules~n% (there were ~D seed examples for which no rule was learned).~2n",
		   [InduceType, RulesLearned, Singletons]),
	jws_setting(total_rule_coverage, TotalRuleCoverage),
	jws_setting(total_rule_time,     TotalRuleTime),
	(RulesLearned < 1 -> RulesLearnedDenom is 1 ; RulesLearnedDenom is RulesLearned),
	AveRuleCoverage is TotalRuleCoverage / RulesLearnedDenom,
	AveRuleTime     is TotalRuleTime     / RulesLearnedDenom,
	(AveRuleTime < 100
	  ->      AveRuleTimeScaled is AveRuleTime,	TimeUnits = 'sec.'
	  ;  (AveRuleTime < 6000
		-> AveRuleTimeScaled is AveRuleTime /   60, TimeUnits = 'min.'
		;  AveRuleTimeScaled is AveRuleTime / 3600, TimeUnits = 'hr.')),
	uw_format("% The average rule covers ~2f examples and took ~2f ~a to learn.~3n",
		   [AveRuleCoverage, AveRuleTimeScaled, TimeUnits]),
	get_performance, % Report the confusion matrix at the end.
	told,
	jws_clear_hold_for_write_rule_to_temp_file.
jws_run_induce(_, _, _) :-
	told, nl, write(' usage: jws_run_induce(+InduceType, +LearnedRulesFilePrefix, +SingletonsFilePrefix).'), nl, nl, abort.

jws_append_rule_to_temp_file :-
	jws_report_best_ins(done_with_seed),
	jws_read_then_unset(current_clause,	 Clause), % Need to know that this has been cleared (in case called w/o another rule being learned).
	jws_set(count_of_pruned_search_paths, 0),
	nonvar(Clause), % Can get called without a rule being written.  Just do nothing.
	jws_setting(current_learned_rules_file, File), % Do NOT unset this one since it is only saved once.
	atom(File), % Make sure a place to write has been specified.
	jws_setting(current_induce_type, InduceType), % Do NOT unset this one since it is only saved once.
	atom(InduceType),
	jws_read_then_unset(current_nodes,	   Nodes),
	jws_read_then_unset(current_cputime_spent, CPUtime),
	jws_read_then_unset(current_label,	   LabelRaw),	
	safe_label(LabelRaw, Label),
	% format("*****~n nodes = ~p~n cpu  =  ~p~n label = ~p~n*****~n", [Nodes, CPUtime, Label]),
	once( (jws_read_then_unset(current_label_when_produced_minus2,	LabelWhenProducedMinus2, if_not_set_just_fail) ; LabelWhenProducedMinus2 = -1) ),
	once( (jws_read_then_unset(current_label_when_produced_minus1,	LabelWhenProducedMinus1, if_not_set_just_fail) ; LabelWhenProducedMinus1 = -1) ),
	once( (jws_read_then_unset(current_label_when_produced,		LabelWhenProduced,       if_not_set_just_fail) ; LabelWhenProduced       = -1) ),
	once( (jws_read_then_unset(current_nodes_when_produced_minus2,	NodesWhenProducedMinus2, if_not_set_just_fail) ; NodesWhenProducedMinus2 = -1) ),
	once( (jws_read_then_unset(current_nodes_when_produced_minus1,	NodesWhenProducedMinus1, if_not_set_just_fail) ; NodesWhenProducedMinus1 = -1) ),
	once( (jws_read_then_unset(current_nodes_when_produced,		NodesWhenProduced,       if_not_set_just_fail) ; NodesWhenProduced       = -1) ),
	once( (jws_read_then_unset(current_literals_in_bottom,		TotalLiterals,		 if_not_set_just_fail) ; TotalLiterals	         = -1) ),
	
	open(File, append, Stream),
	current_output(OldStream),
	set_output(Stream),
	write('% ----------  '),
	once((  (Label = [PosCovered, NegCovered, RuleLengthPlusOne, HeuristicValue | _],
		 RuleLength is RuleLengthPlusOne - 1)
	      ; (PosCovered = -1, NegCovered = -1, RuleLengthPlusOne = 1, HeuristicValue = -1) )), % Don't fail if label isn't what is expected.
	jws_get_aleph_example_counts(PosCountRaw, NegCount),
	once((jws_setting(positive_examples_filtered_out, PosFiltered) ; PosFiltered = 0)),
	once((jws_setting(inverse_sampling_rate_of_neg_examples, InverseSamplingRate, ok_if_not_set) ; InverseSamplingRate = 1)),
	PosCount is PosCountRaw + PosFiltered,
	PosCoveredFraction is 100 * (PosCovered / PosCount),
	NegCoveredFraction is 100 * (NegCovered / NegCount),
	( (number(PosCovered), PosCovered > 1) -> ISA_rule = true ; ISA_rule = false), % Ie, is a singleton.
	(ISA_rule
	  ->	jws_setting(current_rule_number, CurrentRuleNumber),
		format("   Rule #~D  ", [CurrentRuleNumber]),
		(CurrentRuleNumber <  10 -> write(' ') ; true),
		(CurrentRuleNumber < 100 -> write(' ') ; true),
		jws_increment(current_rule_number)
	  ;	jws_setting(current_singleton_number, CurrentSingletonNumber),
		format("Singleton #~D", [CurrentSingletonNumber]),
		(CurrentSingletonNumber <  10 -> write(' ') ; true),
		(CurrentSingletonNumber < 100 -> write(' ') ; true),
		jws_increment(current_singleton_number)),
	once( ((jws_setting(current_seed_example, [SeedType, SeedExampleNum], ok) ; SeedType = 'pos', SeedExampleNum = '?')) ),
	write(' ---------------------------------------------------'), nl,
	% Report the following so we can watch to see what happens before crashes, to see if there are memory leaks (eg, constantly grow?), etc.
	% HeapSpace is heapused // 1000000, % Doesnt work: LocalSpace is local // 1000, GlobalSpace is global // 1000,
	% format("% HEAPSPACE = ~D MB, LOCAL = ~D KB, GLOBAL = ~D KB~n", [HeapSpace, LocalSpace, GlobalSpace]),
	example(SeedExampleNum, SeedType, SeedExampleHead),
	format("% seed example chosen: ~p~p  \"~p\"~n", [SeedType, SeedExampleNum, SeedExampleHead]),
	setting(search, SearchStrategy),
	(ISA_rule
	 -> ((jws_setting(lastValueOfPosUncovered, OldPosLeftCount, ok_if_not_set), integer(OldPosLeftCount), OldPosLeftCount =\= PosCount)
	      -> OldPosLeftCountFraction is 100 * (PosCovered / OldPosLeftCount), % Report fraction of REMAINING examples covered as well as fraction of ALL examples.
	         format("% positives covered:   ~D (of ~D[~D + ~D filtered] total [~2f%] / of ~D left [~2f%])~n",   [PosCovered, PosCount, PosCountRaw, PosFiltered, PosCoveredFraction, OldPosLeftCount, OldPosLeftCountFraction])
	      ;  format("% positives covered:   ~D (of ~D[~D + ~D filtered] = ~2f%)~n",                             [PosCovered, PosCount, PosCountRaw, PosFiltered, PosCoveredFraction])),
	    format(     "% negatives covered:   ~D (of ~D[x~3f] = ~2f%)~n",                                         [NegCovered, NegCount, InverseSamplingRate, NegCoveredFraction]),
	    M_for_M_estimate is 1,
	    Accuracy   is 100 *  PosCovered                     / (PosCovered + NegCovered),
	    M_Accuracy is 100 * (PosCovered + M_for_M_estimate) / (PosCovered + NegCovered + 2 * M_for_M_estimate), % Assume prior is 0.5.
	    (SearchStrategy = induce
	      -> format("%   WARNING: the above accuracies do not include the pos examples covered earlier (since search=~p).~n", [SearchStrategy])
	      ;  true),
	    format("% train-set accuracy:  ~2f%~n", [Accuracy]),
	    format("%     m-est accuracy:  ~2f% (m=~p)~n", [M_Accuracy, M_for_M_estimate]),
	    Ncorrected is InverseSamplingRate * NegCovered,
	    TotalCalledPos is PosCovered + Ncorrected,
	    (TotalCalledPos > 0, TotalCalledPos < inf -> Precision is PosCovered / TotalCalledPos ; Precision = 0),
	    (PosCount       > 0                       -> Recall    is PosCovered / PosCount       ; Recall    = 0),
	    (Precision      > 0, Recall > 0           -> F1        is (2 * Precision * Recall) / (Precision + Recall) ; F1 = 0.0), 
	    format("% recall/precision/F1: ~4f/~4f/~4f [corrected for sampling rate of 1 in ~2f]~n", [Recall, Precision, F1, InverseSamplingRate]),
            jws_count_matches_on_tune_set(pos, Clause, PosTuneMatches, PosTuneCount),
	    jws_count_matches_on_tune_set(neg, Clause, NegTuneMatches, NegTuneCount),
	    (PosTuneCount < 1 -> true
		;    TuneAccuracy  is 100 * (PosTuneMatches / (PosTuneMatches + NegTuneMatches)),
		     TuneRecall    is        PosTuneMatches / PosTuneCount ,
		     format("% tune-set accuracy:   ~2f% (recall = ~4f;  ~D/~D pos  ~D/~D neg)~n",
			    [TuneAccuracy, TuneRecall, PosTuneMatches, PosTuneCount, NegTuneMatches, NegTuneCount]))
	 ;  true),
	recorded(aleph, atoms_left(pos, PLeft), _), % Copied from aleph.
	interval_count(PLeft, PosLeftCount),
	(InduceType = induce_max
	  -> true % Doesn't seem this info is recorded for this case.
	  ;  (jws_setting(lastValueOfPosUncovered, OldPosLeftCount, ok_if_not_set)
	       -> NewlyCoveredPos is OldPosLeftCount - PosLeftCount,
		  (ISA_rule
		    -> format("% pos newly covered:   ~D~n", [NewlyCoveredPos])
		    ;  (NewlyCoveredPos = 1 -> true ; format("% ***** Odd that ~D positives covered by a singleton!~n", [NewlyCoveredPos]))),
		  setting(minpos,     MinPos),
		  ((PosLeftCount > 0, PosLeftCount < MinPos, OldPosLeftCount >= MinPos, InduceType = induce)
		    -> format("% ***** Note: PosLeft < MinPos [~D vs. ~D] so no more clauses will be learned.~n",
			      [PosLeftCount, MinPos]) % Only report first time.  SHOULD ONLY REPORT FOR induce?  Does, say, induce_cover stop early as well?
		    ;  true),
		  (NewlyCoveredPos > PosCovered % PROBABLY CAN DELETE; BUG THAT THIS TRAPPED HAS BEEN FIXED.
		    -> format("% ^^^ ^^^^^ Weird that more NEWLY covered than covered ...~n", [])
		    ;  true)
	       ; true), % First time through, all are new [poetry!].
	     format("% pos not covered yet: ~D~n", [PosLeftCount])),
	(ISA_rule
	   -> format("% body length:         ~D~n", [RuleLength]),
	      format("% heuristic value:     ~4f~n", [HeuristicValue]),
	      (NodesWhenProduced       =< 0 -> true ; format("% learned @ node:      ~D  Label=~p~n", [NodesWhenProduced,       LabelWhenProduced])),
	      (NodesWhenProducedMinus1 =< 0 -> true ; format("% prev learned @ node: ~D  Label=~p~n", [NodesWhenProducedMinus1, LabelWhenProducedMinus1])),
	      (NodesWhenProducedMinus2 =< 0 -> true ; format("% two previous @ node: ~D  Label=~p~n", [NodesWhenProducedMinus2, LabelWhenProducedMinus2]))
	   ;  true),
	format("% literals in bottom:  ~D~n", [TotalLiterals]),
	format("% clauses constructed: ~D~n", [Nodes]),
	jws_set(lastValueOfPosUncovered, PosLeftCount),
	(CPUtime < 100
	   ->  format("% cpu time:            ~3f sec.~n", [CPUtime])
	   ;   CPUtimeInMin is CPUtime / 60,
	       format("% cpu time:            ~3f min.~n", [CPUtimeInMin])),
	TotalCPUtimeInHours is cputime / 3600,
	format("% cpu time so far:     ~3f hr.~n", [TotalCPUtimeInHours]),
	jws_setting(count_of_clause_search_counter_thrown,      TotalThrownClauseSearches),
	jws_setting(count_of_clause_evaluation_counter_thrown,  ThrownClauseEvals),
	jws_setting(count_of_example_evaluation_counter_thrown, ThrownExampleEvals),
	uw_format("% TotalThrownClauseSearches = ~D  ThrownClauseEvals = ~D  ThrownExampleEvals = ~D~n",
		  [  TotalThrownClauseSearches,      ThrownClauseEvals,      ThrownExampleEvals]),
	jws_set(count_of_clause_evaluation_counter_thrown,  0),
	jws_set(count_of_example_evaluation_counter_thrown, 0),
	(PosCovered > 1 % Keep these stats on the rules.
	   -> jws_increment(total_rule_coverage, PosCovered), jws_increment(total_rule_time, CPUtime)
	   ;  % Keep these stats on the singletons (none currently).
	      once( (jws_read_then_unset(bestScoreWithMinPos,      BestScoreWithMinPos,      if_not_set_just_fail) ; BestScoreWithMinPos      = -1) ),
	      once( (jws_read_then_unset(bestScoreWithMinPosLabel, BestScoreWithMinPosLabel, if_not_set_just_fail) ; BestScoreWithMinPosLabel = -1) ),
	      (BestScoreWithMinPos >= 0.0
	       -> format("% best node found:     ~p~n", [BestScoreWithMinPosLabel]),
	          once( (jws_read_then_unset(bestScoreWithMinPosClause, BestScoreWithMinPosClause, if_not_set_just_fail)
		         ; BestScoreWithMinPosClause = (dummy :- idiot) )),
		  format("/*~n", []),
		  pp_dclause(BestScoreWithMinPosClause),
		  format("*/~n", [])
	       ;  format("% no node within minimal coverage of pos examples found~n", []))),
%	(PosCovered > 1 -> nl ; format("/*~n     ", [])), % Comment out the singletons.
	(PosCovered > 1 -> nl ; true),
	% Save in OPTIMIZED form so future use is faster (eg, visualizing, creating the confusion matrix, etc).
	setting(optimise_clauses, OptimizeClauses),
	setting(proof_strategy,   Proof),
	rewrite_clause(Proof, OptimizeClauses, Clause, Clause1),
	jws_setting(typevars, TypeVars),
	((TypeVars, ISA_rule)
	   -> format("/*~n", []),
	      jws_pp_dclause_with_typed_vars(Clause),
	      (Clause = Clause1 -> true % Print both if not equivalent.
	        ; format("~n%%%%%  Optimized Version  %%%%%~2n", []), jws_pp_dclause_with_typed_vars(Clause1)),
	      format("*/~n", []) % Put in quotes since "vars" will be quoted constants.
	   ;  true), % Dont print the seed here anymore since printed above.  pp_dclause(Clause)),
	(PosCovered > 1 % Write some other facts about this rule for possible use during testing.
	  -> copy_term(Clause, ClauseCopied),  vsc_numbervars_nosingletons(ClauseCopied, 0, _),
	     (TypeVars
	       -> format("~n~p.", [ClauseCopied]) % Need a version that can be read in later.  CANNOT USE OPTIMIZED VERSION SINCE CANT USE CUTS WHEN ALL RULES BEING PROCESSED SIMULTANEOUSLY.
	       ;  true),
	     % Store both the optimized and the "normal" versions (if different) for future use.
	     (Clause = Clause1
	       -> format("~nlearned_rule(~d,~n  (~q),~n  (~q)).~2n", [CurrentRuleNumber, ClauseCopied, ClauseCopied])
	       ;  copy_term(Clause1, Clause1Copied), vsc_numbervars_nosingletons(Clause1Copied, 0, _),
	          format("~nlearned_rule(~d,~n  (~q),~n  (~q)).~2n", [CurrentRuleNumber, ClauseCopied, Clause1Copied])),
	     (InduceType = induce
	       -> true   % NOTE: these fractions are NOT accurate for induce since previously covered pos are removed.
	       ;  format("trainset_results(~d, [~d, ~d, ~d, ~d]).~2n",
			 [CurrentRuleNumber, PosCovered, PosCount, NegCovered, NegCount])
		  % To do: compute rule's TUNING SET ACCURACY AT THIS POINT (write function that walks through list of tuning examples, if any).  Or better to do off line after learning?  Probably ...
	     )
	  ;  nl), % no longer needed for singletons  format("*/~n", [])),
	close(Stream),	
	((number(PosCovered), PosCovered > 1)
	   -> true
	   ;  jws_setting(current_singletons_file, SingletonsFile),
	      jws_write_singleton_to_file(SingletonsFile, Clause)),
	set_output(OldStream),
	jws_reset_for_next_rule, !.
jws_append_rule_to_temp_file :-
	told, format("Something went wrong in jws_append_rule_to_temp_file", []), break.

jws_create_file_signature(InduceType, ExperimentSpec, PrettyExpSpec) :- % Fill last two args with strings that characterize the experimental setup.
	(setting(nodes,	       MaxNodes)     -> true ; MaxNodes     = 5000), % Use default values.  Probably no longer necessary since latest aleph sets all to their defaults, but might as well leave this hear.
	(setting(noise,	       Noise)        -> true ; Noise	    = 0),
	(setting(minacc,       MinAcc)       -> true ; MinAcc       = 0.0),
	(setting(minscore,     MinScore)     -> true ; MinScore     = -inf),
	(setting(minpos,       MinPos)       -> true ; MinPos       = 1),
%	(setting(minposfrac,   MinPosFrac)   -> true ; MinPosFrac   = 0.0),
	(setting(clauselength, ClauseLength) -> true ; ClauseLength = 4),
	(setting(i,	       I)	     -> true ; I	    = 2),
	(setting(depth,	       Depth)	     -> true ; Depth	    = 5),
	(setting(search,       SearchLong)   -> true ; SearchLong   = bf),
	(setting(evalfn,       EvalFnLong)   -> true ; EvalFnLong   = coverage),
	MaxNodesInK is MaxNodes // 1000,
	MinAccPercentage  is integer(10000 * MinAcc),     % Show four digits of accuracy.
%	MinFracPercentage is integer(  100 * MinPosFrac), % Show two digits of accuracy.
	jws_short_search_name(SearchLong, Search),
	jws_short_evalfn_name(EvalFnLong, EvalFn),
	%uw_concat([InduceType, ' [', MinAccPercentage, '% ', MinPos, 'pos ', Noise, 'neg cl', ClauseLength, ' i', I, ' d', Depth,
	%	      ' ', MaxNodesInK, 'k ',  Search, ' ', EvalFn, '] '],
	%	     ExperimentSpec), % Need to have no spaces in file names in order to use Condor.
	uw_concat([InduceType, '_', MinAccPercentage, 'acc_', MinPos, 'pos_', Noise, 'neg_cl', ClauseLength, '_i', I, '_d', Depth,
		      '_', MaxNodesInK, 'k_',  Search, '_', EvalFn],
		     ExperimentSpec),
	jws_collect_parameter_settings_into_atom([minacc, minscore, minpos, noise, clauselength, i, depth, nodes, search, evalfn], PrettyExpSpec), !.
	

jws_reset_for_next_rule :- % Reset any stats that are stored on a per-rule basis.
	jws_set(current_nodes_when_produced,        -1),
	jws_set(current_nodes_when_produced_minus1, -1),
	jws_set(current_nodes_when_produced_minus2, -1),
	jws_set(bestScoreWithMinPos,                -1),
	jws_set(bestScoreWithMinPosLabel,           -1), !.
jws_reset_for_next_rule :-
	told, format("Something went wrong in jws_reset_for_next_rule", []), break.


% Need to hold this until ready to print (need to wait until rm_seeds called, so stats are correct).
jws_hold_for_write_rule_to_temp_file(Clause, Nodes, CPUtime, Label) :-
	jws_set(current_clause,	       Clause), % Will erase if old value exists.
	jws_set(current_nodes,	       Nodes),
	jws_set(current_cputime_spent, CPUtime),
	jws_set(current_label,	       Label), !.
jws_hold_for_write_rule_to_temp_file(Clause, Nodes, CPUtime, Label) :-
	told, format("Something went wrong in jws_hold_for_write_rule_to_temp_file(~p,~p,~p,~p)", [Clause, Nodes, CPUtime, Label]), break.

jws_clear_hold_for_write_rule_to_temp_file :-
	jws_noset(current_seed_example),
	jws_noset(current_induce_type),
	jws_noset(current_learned_rules_file),
	jws_noset(current_singletons_file),
	jws_noset(lastValueOfPosUncovered),
	jws_noset(current_clause),
	jws_noset(current_nodes),
	jws_noset(current_cputime_spent),
	jws_noset(current_nodes_when_produced_minus2),
	jws_noset(current_nodes_when_produced_minus1),
	jws_noset(current_nodes_when_produced),
	jws_noset(current_label),
	!.
jws_clear_hold_for_write_rule_to_temp_file :-
	told, format("Something went wrong in jws_clear_hold_for_write_rule_to_temp_file", []), break.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Some aleph wrappers and extensions

% This first group contains "wrappers" for accessing induce variants.
jws_induce :-
	jws_setting(rule_file_prefix_rules,      Prefix_rules),
	jws_setting(rule_file_prefix_singletons, Prefix_singletons), !,
	jws_induce(Prefix_rules, Prefix_singletons).
jws_induce(Prefix) :- jws_induce(Prefix, Prefix).
jws_induce(LearnedRulesFilePrefix, SingletonsFilePrefix) :-
	atom(LearnedRulesFilePrefix),
	atom(SingletonsFilePrefix), !,
	once(jws_run_induce(induce, LearnedRulesFilePrefix, SingletonsFilePrefix)).
jws_induce(_, _) :-
	told, nl, write(' usage: jws_induce(+LearnedRulesFilePrefix, +SingletonsFilePrefix).'), nl, nl, abort.
jws_induce :- jws_induce(_, _).

jws_induce_cover :- 
	jws_setting(rule_file_prefix_rules,      Prefix_rules),
	jws_setting(rule_file_prefix_singletons, Prefix_singletons), !,
	jws_induce_cover(Prefix_rules, Prefix_singletons).
jws_induce_cover(Prefix) :- jws_induce_cover(Prefix, Prefix).
jws_induce_cover(LearnedRulesFilePrefix, SingletonsFilePrefix) :-
	atom(LearnedRulesFilePrefix),
	atom(SingletonsFilePrefix), !,
	once(jws_run_induce(induce_cover, LearnedRulesFilePrefix, SingletonsFilePrefix)).
jws_induce_cover(_, _) :-
	told, nl, write(' usage: jws_induce_cover(+LearnedRulesFilePrefix, +SingletonsFilePrefix).'), nl, nl, abort.
jws_induce_cover :- jws_induce_cover(_, _).

jws_induce_max :- 
	jws_setting(rule_file_prefix_rules,      Prefix_rules),
	jws_setting(rule_file_prefix_singletons, Prefix_singletons), !,
	jws_induce_max(Prefix_rules, Prefix_singletons).
jws_induce_max(Prefix) :- jws_induce_max(Prefix, Prefix).
jws_induce_max(LearnedRulesFilePrefix, SingletonsFilePrefix) :-
	atom(LearnedRulesFilePrefix),
	atom(SingletonsFilePrefix), !,
	once(jws_run_induce(induce_max, LearnedRulesFilePrefix, SingletonsFilePrefix)).
jws_induce_max(_, _) :-
	told, nl, write(' usage: jws_induce_max(+LearnedRulesFilePrefix, +SingletonsFilePrefix).'), nl, nl, abort.
jws_induce_max :- jws_induce_max(_, _).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
% JWS-extended version.  Keeps list of last three best clauses and when they were learned.
jws_help_show_clause(LabelRaw, Clause, Nodes) :-
	safe_label(LabelRaw, Label),
	jws_setting(current_seed_example, [SeedType, SeedExampleNum]), % Added by JWS.
	once((fpd_setting(rulesConsidered, RulesConsidered, ok) ; RulesConsidered = 0)), 
	(Nodes = RulesConsidered % Can get a difference in, say, iterative deepening.
	   -> uw_format("~n[found clause for seed ~p~p at #nodes=~D, label=~p]~n",              [SeedType, SeedExampleNum, Nodes, Label])
	   ;  uw_format("~n[found clause for seed ~p~p at #nodes=~D (total = ~D), label=~p]~n", [SeedType, SeedExampleNum, Nodes, RulesConsidered, Label])),
	NodesToReport is max(Nodes, RulesConsidered),
	uw_pp_dclause_to_stream(Clause),

	(jws_setting(current_label_when_produced_minus1,  OldLabelMinus1,  if_not_set_just_fail)
	   ; OldLabelMinus1  = -1),
	(jws_setting(current_label_when_produced,	  OldLabel,	   if_not_set_just_fail)
	   ; OldLabel	     = -1),
	jws_set(current_label_when_produced_minus2,       OldLabelMinus1),
	jws_set(current_label_when_produced_minus1,       OldLabel),
	jws_set(current_label_when_produced,	          Label),

	(jws_setting(current_clause_when_produced_minus1, OldClauseMinus1, if_not_set_just_fail)
	   ; OldClauseMinus1 = -1),
	(jws_setting(current_clause_when_produced,	  OldClause,       if_not_set_just_fail)
	   ; OldClause       = -1),
	jws_set(current_clause_when_produced_minus2,      OldClauseMinus1),
	jws_set(current_clause_when_produced_minus1,      OldClause),
	jws_set(current_clause_when_produced,	          Clause),

	(jws_setting(current_nodes_when_produced_minus1,  OldNodesMinus1,  if_not_set_just_fail)
	   ; OldNodesMinus1  = -1),
	(jws_setting(current_nodes_when_produced,	  OldNodes,	   if_not_set_just_fail)
	   ; OldNodes	     = -1),
	jws_set(current_nodes_when_produced_minus2,       OldNodesMinus1),
	jws_set(current_nodes_when_produced_minus1,       OldNodes),
	jws_set(current_nodes_when_produced,	          NodesToReport), !.
jws_help_show_clause(Label, Clause, Nodes) :-
	told, format("Something went wrong in jws_help_show_clause(~p, ~p, ~p)", [Label, Clause, Nodes]), break.


% Collect some aleph parameters into one atom (for naming files, etc).
jws_collect_parameter_settings_into_atom(A, B) :- jws_collect_parameter_settings_into_atom(A, B, true).
jws_collect_parameter_settings_into_atom([], '', _).
jws_collect_parameter_settings_into_atom([Parameter | Rest], Atom, FirstOne) :-
	(once(setting(Parameter, Value)) -> true; Value = ''),
	uw_concat('=', Value, Value1), % Avoid yap bug (see uw_concat).
	(Rest = []  % Assumes there are at least TWO parameters.
	   -> uw_concat(['\n% and ', Parameter, Value1], Temp1)
	   ;  (FirstOne
		 -> uw_concat([   '%     ', Parameter, Value1], Temp1)
		 ;  uw_concat([',\n%     ', Parameter, Value1], Temp1))),
	jws_collect_parameter_settings_into_atom(Rest, Temp2, false),
	uw_concat(Temp1, Temp2, Atom).
jws_get_aleph_example_counts(Pos, Neg) :-
	recorded(aleph, size(pos, Pos), _),
	recorded(aleph, size(neg, Neg), _).

jws_set_minpos_percentage(Percentage) :-
	recorded(aleph, size(pos, NumbPos), _), !,
	MinPos is integer((Percentage / 100) * NumbPos),
	(MinPos > 2 -> MinPos2 is MinPos ; MinPos2 is 3), % Must be at least 2 to prevent singletons.
	set(minpos, MinPos2), !.
jws_set_minpos_percentage(Percentage) :-
	told, uw_format("~n jws_set_minpos_percentage(~p) failed.  Have examples been read in?~2n", Percentage), break.

%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Writing to files

jws_init_progress_reporting_files(File, Msg1, Msg2, Msg3) :-
	(exists(File)
	  -> format("[Reinitializing file: ~p]~n", [File])
	  ;  format("[Initializing file: ~p]~n",   [File])),
	open(File, write, Stream),
	format(Stream, "~p~n~p~n~p~2n", [Msg1, Msg2, Msg3]),
	(setting(train_pos, TrainSetPosFile)
	  -> format(Stream, "% Trainset pos examples read from:  ~p~n", TrainSetPosFile) ; true),
	(setting(train_neg, TrainSetNegFile)
	  -> format(Stream, "% Trainset neg examples read from:  ~p~n", TrainSetNegFile) ; true),
	(setting(test_pos,  TestSetPosFile)
	  -> format(Stream, "% Testset  pos examples read from:  ~p~n", TestSetPosFile)  ; true),
	(setting(test_neg,  TestSetNegFile)
	  -> format(Stream, "% Testset  neg examples read from:  ~p~n", TestSetNegFile)  ; true),
	format(Stream, "~n", []),
	close(Stream), !.
jws_init_progress_reporting_files(File, _, _, _ ) :-
	told, format("Something went wrong in jws_init_progress_reporting_files for~n  ~p ", [File]), break.

jws_write_singleton_to_file(File, Clause) :-
	open(File, append, Stream),
	current_output(OldStream),
	set_output(Stream),	
	pp_dclause(Clause),
	close(Stream),
	set_output(OldStream), !.
jws_write_singleton_to_file(_, _).

%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% See if this clause is acceptable as a "new best" (not based on accuracy, etc,
%% but based on any other considerations, such as head vars being bound).

jws_acceptable_clause(Clause) :-
	clause(acceptable_clause(_),_), !, % If acceptable_clause is defined, then call it.
	acceptable_clause(Clause).
jws_acceptable_clause(_). % Otherwise assume acceptable.

%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Learn a clause, but with a "Vitor timer" if requested.
%%    (Overrides 'searchtime' in aleph).

jws_search(S, Nodes) :-
	%debug uw_format("~n**** Calling jws_search *****~n", []), read(_), % debug
	jws_setting(clause_search_time, ClauseSearchTime, if_not_set_just_fail),
	ClauseSearchTime > 0, ClauseSearchTime < inf, !,
	IntegerClauseSearchTime is integer(ClauseSearchTime),
	/* jws jws might want to do this at "reduce" so timing is for, say, all of iterative deepening - to do */
	jws_catch_vitor_counter(jws_clause_search_counter,
			        IntegerClauseSearchTime,
				(graphsearch(S,_),
				 % nl, jws_report_vitor_counters('Vitor calls for SUCCESSFUL jws_search:', true),
				 true)),
	recorded(search, current(_, Nodes, _), _).  % Need to reach here, even if "throw" occurs.

jws_process_thrown_counter(jws_clause_search_counter) :-
	jws_setting(clause_search_time, ClauseSearchTime), !,
	jws_increment(count_of_clause_search_counter_thrown),
	jws_setting(  count_of_clause_search_counter_thrown, NewTotal),
	ClauseSearchTimeInM is ClauseSearchTime / 1000000,
	uw_format("~n[***** jws_clause_search_counter limit of ~3fM reached (#~p). *****]~n", [ClauseSearchTimeInM, NewTotal]),
	nl, jws_report_vitor_counters('Vitor calls for THROWN jws_search:', true).

jws_report_vitor_counters(Msg) :- jws_report_vitor_counters(Msg, false).
jws_report_vitor_counters(Msg, UseUWformat) :-
	call_count_data(VitorCallsA, VitorCallsB, VitorCallsC),
	VitorCallsAinM is VitorCallsA / 1000000,
	VitorCallsBinM is VitorCallsB / 1000000,
	VitorCallsCinM is VitorCallsC / 1000000,
	(UseUWformat
	  -> uw_format("[***** ~p [~3fM, ~3fM, ~3fM] *****]~2n",
	   	       [Msg, VitorCallsAinM, VitorCallsBinM, VitorCallsCinM])
	  ;     format("[***** ~p [~3fM, ~3fM, ~3fM] *****]~2n",
	   	       [Msg, VitorCallsAinM, VitorCallsBinM, VitorCallsCinM])).
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Prove examples but with a "Vitor timer" if requested.
%%    If counter expires, then simply fail (within cut).

jws_prove_examples(S,Flag,Contradiction,Entry,Best,CL,L2,Clause,Pos,Neg,PCover,NCover,Label) :-
	%debug uw_format("~n***** jws_prove_examples *****~n       ~p~n", [Clause]), % debug
	jws_setting(clause_evaluation_time, ClauseEvalTime, if_not_set_just_fail),
	ClauseEvalTime > 0, ClauseEvalTime < inf,  !,
	IntegerClauseEvalTime is integer(ClauseEvalTime),
	% Need to return what prove_examples returns unless timeout occurs.
	jws_set(last_clause_evaluated, Clause),	
	jws_setting(min_clause_length_for_using_prooftime, MinClauseLength),
	(CL < MinClauseLength -> true  % Only use prooftime if not too many examples (else too much overhead).  Maybe count examples [interval_count] instead?  Is that a fast calc?
	  ; (jws_setting(jws_prooftime, ProofTime, if_not_set_just_fail) -> set(prooftime, ProofTime) ; true)),
	jws_catch_vitor_counter(jws_clause_evaluation_counter,
				IntegerClauseEvalTime,
				prove_examples(S,Flag,Contradiction,Entry,Best,CL,L2,Clause,Pos,Neg,PCover,NCover,Label)),
	set_default(prooftime). % To do: redesign to RESTORE to prooftime setting coming in ...
jws_prove_examples(S,Flag,Contradiction,Entry,Best,CL,L2,Clause,Pos,Neg,PCover,NCover,Label) :-
	prove_examples(S,Flag,Contradiction,Entry,Best,CL,L2,Clause,Pos,Neg,PCover,NCover,Label).


jws_process_thrown_counter(jws_clause_evaluation_counter) :-
	jws_setting(clause_evaluation_time, ClauseEvalTime), !,
	jws_increment(count_of_clause_evaluation_counter_thrown),
	jws_setting(  count_of_clause_evaluation_counter_thrown, NewTotal),
	ClauseEvalTimeInM is ClauseEvalTime / 1000000,
	uw_format("~n[***** jws_clause_evaluation_counter limit of ~3fM reached (#~p). *****]~n", [ClauseEvalTimeInM, NewTotal]),
	jws_setting(last_clause_evaluated, Clause),
	uw_pp_dclause(Clause),
	set_default(prooftime),
	fail. % Return fail in this case.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Try to prove ONE example.
%%    (Replaces 'prooftime' in aleph).
%%

jws_counter_bound_call(IntegerExampleProofTime, Depth, Goals) :-
	jws_catch_vitor_counter(jws_prove_example_counter,
				IntegerExampleProofTime,
				depth_bound_call(Goals, Depth)).

jws_process_thrown_counter(jws_prove_example_counter) :-
	jws_increment(count_of_example_evaluation_counter_thrown), !,
	fail. % Return fail in this case.

%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Managing multiple counters within the one "vitor counter."
%%
%%      jws_counter_list(example_proof_time,[clause_evaluation_time/2000,clause_search_time/198000]).
%%
%%	  means the current counter is example_proof. After example_proof completes you have 2000
%%	  ticks to do clause_evaluation, and after eval2 completes you have 198000 to do clause_search
%%        (plus any unused time on the current counter).

jws_catch_vitor_counter(CounterName, CounterTicks, Body) :-
	(jws_setting(jws_counter_list_active,  _, just_fail_if_not_set) % If something else is to expire before this one, no need to catch.
	  -> call_count_data(_, _, TicksStillToGo),
	     (TicksStillToGo =< CounterTicks -> CatchNeeded = false ; CatchNeeded = true)
	  ;  CatchNeeded = true),
	(CatchNeeded
	  -> %debug uw_format("~njws_catch_vitor_counter(~p,~p,_) [ticks still to go = ~p]~n", [CounterName, CounterTicks, TicksStillToGo]), %debug
	     catch(( once(jws_add_counter(CounterName, CounterTicks)),
	             (call(Body) -> Result = true ; Result = false),
	             once(jws_activate_next_counter_in_list),
		     Result ), % Return result of the Body call.
	           call_and_retry_counter,
	           ( %debug jws_setting(jws_counter_list_active, CurrentCounter),
		     %debug uw_format("~n***** Throwing counter <~p> *****~2n", [CurrentCounter]), % debug
		     once(jws_activate_next_counter_in_list), % Do some bookkeeping.
	             jws_process_thrown_counter(CounterName)))
	   ; call(Body)). % Simply call body without using a catch since something else will expire first.

jws_process_thrown_counter(X) :-
	told,nl,nl,write(' unknown counter thrown: '), write(X), write(' '), read(_).

jws_activate_next_counter_in_list :-
	jws_setting(jws_counter_list_pending, ListOfCounters, just_fail_if_not_set), !,
	jws_help_activate_next_counter_in_list(ListOfCounters).
jws_activate_next_counter_in_list :-  % Ok if no counters to activate.  Might have removed last one.
	told,nl,nl,write('Can we reach here: jws_activate_next_counter_in_list, clause 2?'), nl, read(_),
	call_count_reset. % Get rid of any remaining time.

jws_help_activate_next_counter_in_list([]) :-
	!,
	jws_noset(jws_counter_list_active),
	jws_noset(jws_counter_list_pending),
	call_count_reset. % Get rid of any remaining time.
% we have active counters to start.
jws_help_activate_next_counter_in_list([Counter/TicksLeft | ExtraCounters]) :-
	call_count_data(_, _, TicksStillToGo),
	%debug uw_format("Activating ~p (~p ticks left)~n", [Counter/TicksLeft, TicksStillToGo]), % debug
	(TicksStillToGo > 0 -> TotalTicksLeft is TicksStillToGo + TicksLeft ; TotalTicksLeft is TicksLeft),
	jws_set(jws_counter_list_active,  Counter),
	jws_set(jws_counter_list_pending, ExtraCounters),
	call_count(_, _, TotalTicksLeft).

jws_add_counter(Name, TicksToDo) :-
	jws_setting(jws_counter_list_active,  _, just_fail_if_not_set), 
	call_count_data(_, _, TicksStillToGo), !,
	(TicksToDo < TicksStillToGo
	   -> jws_add_as_current_counter(TicksStillToGo, Name/TicksToDo)
	   ;  % insert counter in list
	      told, uw_format("Should not happen [TicksToDo < TicksStillToGo] in jws_add_counter(~p, TicksToDo=~p) [~p ticks left]~n", [Name, TicksToDo, TicksStillToGo]), read(_),
	      jws_add_as_delayed_counter(Name/TicksToDo)),
	%debug uw_format("  Adding counter of ~p ticks for ~p~n", [TicksToDo, Name]), % debug
	true.

jws_add_counter(Name, TicksToDo) :- % If here, no current counters.
	%debug uw_format("Adding INITIAL counter of ~p ticks for ~p~n", [TicksToDo, Name]), % debug
	jws_set(jws_counter_list_active,  Name),
	jws_set(jws_counter_list_pending, []),
	call_count(_, _, TicksToDo).

% Add to FRONT of list since this counter will expire soonest.
jws_add_as_current_counter(TicksStillToGoOnCurrentCounter, NewPrimeCounter/TicksToDoOnNewCounter) :-
	jws_setting(jws_counter_list_active,  CurrentCounter,               just_fail_if_not_set),
	jws_setting(jws_counter_list_pending, CurrentListOfPendingCounters, just_fail_if_not_set), !,
	TimeStillLeftOnCurrentWhenNewDone is TicksStillToGoOnCurrentCounter - TicksToDoOnNewCounter,
	TimeStillLeftOnCurrentWhenNewDone >= 0, % Check for an error in this code.
	%debug uw_format("jws_add_as_current_counter(TicksOnCurrent=~p, New=~p)~n", [TicksStillToGoOnCurrentCounter, NewPrimeCounter/TicksToDoOnNewCounter]), % debug
	%debug uw_format("  TimeStillLeftOnCurrentWhenNewDone = ~p~n", [TimeStillLeftOnCurrentWhenNewDone]), % debug
	jws_set(jws_counter_list_active,  NewPrimeCounter),
	jws_set(jws_counter_list_pending, [CurrentCounter/TimeStillLeftOnCurrentWhenNewDone | CurrentListOfPendingCounters]),
	call_count(_, _, TicksToDoOnNewCounter),
	%debug jws_report_time_on_each_counter, % debug
	true. % debug
jws_add_as_current_counter(TicksStillToGoOnCurrentCounter, NewPrimeCounter/TicksToDoOnNewCounter) :- 
	told, format("~2n***** Should not get here in~n       jws_add_as_current_counter(~p, ~p])~n",
	             [TicksStillToGoOnCurrentCounter, NewPrimeCounter/TicksToDoOnNewCounter]), read(_).

% Insert in list so list is sorted by expiration time.
jws_add_as_delayed_counter(NewCounter/TicksToDoOnNewCounter) :-
	jws_setting(jws_counter_list_active,  CurrentCounter),
	jws_setting(jws_counter_list_pending, CurrentListOfPendingCounters),
	call_count_data(_, _, TicksStillToGoOnCurrentCounter),
	TicksStillToDo is TicksToDoOnNewCounter - TicksStillToGoOnCurrentCounter,
	jws_insert_into_pending_counters_list(NewCounter/TicksStillToDo, CurrentListOfPendingCounters, NewListOfPendingCounters),
	jws_set(jws_counter_list_active,  CurrentCounter),
	jws_set(jws_counter_list_pending, NewListOfPendingCounters).

/*
jws_add_to_all_timers(TicksStillToGo, [], []).
jws_add_to_all_timers(TicksStillToGo, [Counter/Ticks | RestPending], [Counter/AddedTicks | AddedRestPending]) :-
	AddedTicks is Ticks + TicksStillToGo,
	jws_add_to_all_timers(TicksStillToGo, RestPending, AddedRestPending).

jws_subtract_from_all_timers(TicksToDoOnNewCounter, [], []).
jws_subtract_from_all_timers(TicksToDoOnNewCounter, [Counter/Ticks | RestPending], [Counter/SubtractedTicks | SubtractedRestPending]) :-
	SubtractedTicks is Ticks - TicksToDoOnNewCounter,
	jws_subtract_from_all_timers(TicksToDoOnNewCounter, RestPending, SubtractedRestPending).
*/

jws_insert_into_pending_counters_list(NewCounter/TicksToDoOnNewCounter, [], [NewCounter/TicksToDoOnNewCounter]).
jws_insert_into_pending_counters_list(NewCounter/TicksToDoOnNewCounter, [Counter/Ticks | RestPending], [NewCounter/TicksToDoOnNewCounter, Counter/NewTicksOnFollower | RestPending]) :-
	Ticks > TicksToDoOnNewCounter, !, % Stick in here.  Need to subtract from FOLLOWING item since using some of its time.
	NewTicksOnFollower is Ticks - TicksToDoOnNewCounter.
jws_insert_into_pending_counters_list(NewCounter/TicksToDoOnNewCounter, [Counter/Ticks | RestPending], [Counter/Ticks | NewListOfRestPendingCounters]) :-
	TicksStillToDo is TicksToDoOnNewCounter - Ticks,
	jws_insert_into_pending_counters_list(NewCounter/TicksStillToDo, RestPending, NewListOfRestPendingCounters).



jws_report_time_on_each_counter :-	
	jws_setting(jws_counter_list_active,  CurrentCounter,               just_fail_if_not_set),
	jws_setting(jws_counter_list_pending, CurrentListOfPendingCounters, just_fail_if_not_set),
	call_count_data(_, _, TicksStillToGoOnCurrentCounter),
	uw_format("   Active  counter ~p has ~p ticks left.~n", [CurrentCounter, TicksStillToGoOnCurrentCounter]),
	jws_report_time_on_each_pending_counter(CurrentListOfPendingCounters, TicksStillToGoOnCurrentCounter).

jws_report_time_on_each_pending_counter([], _) :- nl.
jws_report_time_on_each_pending_counter([FirstCounter/Ticks | Rest], TicksAhead) :-
	Total is TicksAhead + Ticks,
	uw_format("   Pending counter ~p has ~p ticks left.~n", [FirstCounter, Total]),
	jws_report_time_on_each_pending_counter(Rest, Total).

jws_test_counters :-
	jws_add_counter(a1, 111100),
	jws_setting(jws_counter_list_active,  CurrentCounter_a1), 
	jws_setting(jws_counter_list_pending, CurrentListOfPendingCounters_a1),
	format("After ~p,~n Current=~p~n Pending=~p~n", [jws_add_counter(a1, 111100), CurrentCounter_a1, CurrentListOfPendingCounters_a1]),
	jws_report_time_on_each_counter,

	jws_add_counter(a2, 222200),
	jws_setting(jws_counter_list_active,  CurrentCounter_a2), 
	jws_setting(jws_counter_list_pending, CurrentListOfPendingCounters_a2),
	format("After ~p,~n Current=~p~n Pending=~p~n", [jws_add_counter(a2, 222200), CurrentCounter_a2, CurrentListOfPendingCounters_a2]),
	jws_report_time_on_each_counter,

	jws_add_counter(a3, 333300),
	jws_setting(jws_counter_list_active,  CurrentCounter_a3), 
	jws_setting(jws_counter_list_pending, CurrentListOfPendingCounters_a3),
	format("After ~p,~n Current=~p~n Pending=~p~n", [jws_add_counter(a3, 333300), CurrentCounter_a3, CurrentListOfPendingCounters_a3]),
	jws_report_time_on_each_counter,

	jws_add_counter(b1,  99900),
	jws_setting(jws_counter_list_active,  CurrentCounter_b1), 
	jws_setting(jws_counter_list_pending, CurrentListOfPendingCounters_b1),
	format("After ~p,~n Current=~p~n Pending=~p~n", [jws_add_counter(b1,  99900), CurrentCounter_b1, CurrentListOfPendingCounters_b1]),
	jws_report_time_on_each_counter,

	jws_add_counter(b3, 300000),
	jws_setting(jws_counter_list_active,  CurrentCounter_b3), 
	jws_setting(jws_counter_list_pending, CurrentListOfPendingCounters_b3),
	format("After ~p,~n Current=~p~n Pending=~p~n", [jws_add_counter(b3, 300000), CurrentCounter_b3, CurrentListOfPendingCounters_b3]),
	jws_report_time_on_each_counter,

	jws_add_counter(a4, 444400),
	jws_setting(jws_counter_list_active,  CurrentCounter_a4), 
	jws_setting(jws_counter_list_pending, CurrentListOfPendingCounters_a4),
	format("After ~p,~n Current=~p~n Pending=~p~n", [jws_add_counter(a4, 444400), CurrentCounter_a4, CurrentListOfPendingCounters_a4]),
	jws_report_time_on_each_counter,

	once(jws_activate_next_counter_in_list),
	jws_setting(jws_counter_list_active,  CurrentCounter_pop), 
	jws_setting(jws_counter_list_pending, CurrentListOfPendingCounters_pop),
	format("After POPing active counter,~n Current=~p~n Pending=~p~n", [jws_add_counter(a4, 444400), CurrentCounter_pop, CurrentListOfPendingCounters_pop]),
	jjws_report_time_on_each_counter,
	true.

% ['../aleph'], jws_test_counters.

%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Use PROPORTIONAL P & N for "compression", etc scoring.  Plus F1 and Precions * Recall eval functions.
%%

% I'm not sure why I wrote this ... (to catch "typos"?).  Similar items are in aleph, so might as well copy.
special_consideration(search, X):-
	uw_member(X, [jws_compression, jws_coverage, jws_laplace, jws_f1, jws_pr]),
	set(search, heuristic), set(evalfn, jws_compression), !.

evalfn_name(jws_compression,'jws_compression').
evalfn_name(jws_coverage,   'jws_pos-jws_neg').
evalfn_name(jws_laplace,    'jws_laplace').
evalfn_name(jws_f1,         'jws_f1').
evalfn_name(jws_pr,         'jws_pr').  % Use Precision * Recall.

built_in_prune(jws_coverage).
built_in_prune(jws_compression).
built_in_prune(jws_laplace).
built_in_prune(jws_f1).
built_in_prune(jws_pr).

compression_ok(jws_compression, [P, _, L | _]):-
	!,
	jws_setting(pos_example_count_scaleFactor,                         PosExamplesScaleFactor),
	jws_setting(one_literal_counts_as_this_percentage_of_pos_examples, LiteralsScaleFactor),
	PosExamplesScaleFactor * P - LiteralsScaleFactor * (L - 1) > PosExamplesScaleFactor. % The score of a singleton is "PosExamplesScaleFactor" (and this function is checking if the compression of a rule is better than that of a singleton).

evalfn(jws_coverage,     [P, N, L | _], Val):-
	(P = -inf -> Val = -inf;
		setting_jws(pos_example_count_scaleFactor,                         PosExamplesScaleFactor),
		setting_jws(neg_example_count_scaleFactor,                         NegExamplesScaleFactor),
		setting_jws(one_literal_counts_as_this_percentage_of_pos_examples, LiteralsScaleFactor),
        	Val is PosExamplesScaleFactor * P - NegExamplesScaleFactor * N - LiteralsScaleFactor * (L - 1)),
	!.
evalfn(jws_compression, [P, N, _ | _], Val):-
	(P = -inf -> Val = -inf;
		setting_jws(pos_example_count_scaleFactor,                         PosExamplesScaleFactor),
		setting_jws(neg_example_count_scaleFactor,                         NegExamplesScaleFactor),
		Val is PosExamplesScaleFactor * P - NegExamplesScaleFactor * N),
	!.


evalfn(jws_laplace, [P, N | _], Val):- % No need to use unless PosExamplesScaleFactor =\= NegExamplesScaleFactor.
	(P = -inf -> Val is 0.5;
		setting_jws(pos_example_count_scaleFactor,                         PosExamplesScaleFactor),
		setting_jws(neg_example_count_scaleFactor,                         NegExamplesScaleFactor),
		Val is (PosExamplesScaleFactor * (P + 1)) / (PosExamplesScaleFactor * (P + 1) + NegExamplesScaleFactor * (N + 1))),
	!.


evalfn(jws_f1, [P, N | _], F1) :-
	recorded(aleph, size(pos, PosCount), _),
	once((jws_setting(inverse_sampling_rate_of_neg_examples, InverseSamplingRate, ok_if_not_set) ; InverseSamplingRate = 1)), !, % to do: correction factor for POS? (Add when/if needed.)
	Ncorrected is InverseSamplingRate * N,
	TotalCalledPos is P + Ncorrected,
	(TotalCalledPos > 0, TotalCalledPos < inf -> Precision is P / TotalCalledPos ; Precision = 0),
	(PosCount       > 0                       -> Recall    is P / PosCount       ; Recall    = 0),
	(Precision      > 0, Recall > 0           -> F1        is (2 * Precision * Recall) / (Precision + Recall) ; F1 = 0.0).
evalfn(jws_f1, _, _) :-
	told, uw_format("jws_f1 not working since size(pos, PosCount) or inverse_sampling_rate_of_neg_examples not set.~n"), break.

evalfn(jws_pr, [P, N | _], PR) :-
	recorded(aleph, size(pos, PosCount), _),
	once((jws_setting(inverse_sampling_rate_of_neg_examples, InverseSamplingRate, ok_if_not_set) ; InverseSamplingRate = 1)), !,
	Ncorrected is InverseSamplingRate * N,
	TotalCalledPos is P + Ncorrected,
	(TotalCalledPos > 0, TotalCalledPos < inf -> Precision is P / TotalCalledPos ; Precision = 0),
	(PosCount       > 0                       -> Recall    is P / PosCount       ; Recall    = 0),
	PR is Precision * Recall.
evalfn(jws_pr, _, _) :-
	told, uw_format("jws_pr not working since size(pos, PosCount) or inverse_sampling_rate_of_neg_examples not set.~n"), break.

best_value(jws_coverage,_,      [P, _, L| _], Best) :-
	evalfn(jws_coverage,    [P, 0, L| _], Best).
best_value(jws_compression,_,   [P, _, L| _], Best) :-
	evalfn(jws_compression, [P, 0, L| _], Best).
best_value(jws_laplace,_,       [P, _, L| _], Best) :-
	evalfn(jws_laplace,     [P, 0, L| _], Best).
best_value(jws_f1,_,            [P, _, L| _], Best) :-
	evalfn(jws_f1,          [P, 0, L| _], Best).
best_value(jws_pr,_,            [P, _, L| _], Best) :-
	evalfn(jws_pr,          [P, 0, L| _], Best).


%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

jws_check_node_for_best(Label, Clause) :-
	setting(search, SearchType), uw_isa_randomized_search(SearchType), %%%%% ONLY DO THIS FOR RANDOMIZED SEARCHES (to keep full-theory file sizes small)
	Label = [P, N | _],
	setting(minpos, MinPosToCover),
	setting(noise,  MaxNegToCover),
	number(P), P >= MinPosToCover, number(N), N =< MaxNegToCover,
%	uw_format("Calling jws_check_node_for_best(P=~p,N=~p)~n", [P, N]),
	jws_get_aleph_example_counts(PosCountRaw, _),
	once((jws_setting(positive_examples_filtered_out, PosFiltered, of_if_not_set) ; PosFiltered = 0)),
	PosCount is PosCountRaw + PosFiltered,
	once((jws_setting(inverse_sampling_rate_of_neg_examples, InverseSamplingRate, ok_if_not_set) ; InverseSamplingRate = 1)),
	Pcorrected is P + 1, % Do an m=1 Laplace correction here to deal with small values.
	Ncorrected is InverseSamplingRate * N + 1,  % Could add the 1 BEFORE multiplying ...
	TotalCalledPos is Pcorrected + Ncorrected,
	(TotalCalledPos > 0 -> Precision is Pcorrected / TotalCalledPos ; Precision = 0.0),
	(PosCount       > 0 -> Recall    is Pcorrected / PosCount       ; Recall    = 0.0),
%	setting(minacc, MinAccuracy), Precision >= MinAccuracy,  % Use the "corrected" accuracy here.
	jws_keep_best_in_column(Precision, Recall, Label, Clause),
	jws_keep_best_in_row(   Precision, Recall, Label, Clause),
	fail.
jws_check_node_for_best(_, _).


% To save space, only keep a "new best" in the interval in which it occurred
% (ie, other intervals should inherit this info from earlier intervals).
isInThisInterval(RulesConsidered, first1K)           :-                             RulesConsidered =<    1000.
isInThisInterval(RulesConsidered, interval1K_10K)    :- RulesConsidered >    1000,  RulesConsidered =<   10000.
isInThisInterval(RulesConsidered, interval10K_25K)   :- RulesConsidered >   10000,  RulesConsidered =<   25000.
isInThisInterval(RulesConsidered, interval25K_50K)   :- RulesConsidered >   10000,  RulesConsidered =<   50000.
isInThisInterval(RulesConsidered, interval50K_100K)  :- RulesConsidered >   50000,  RulesConsidered =<  100000.
isInThisInterval(RulesConsidered, interval100K_250K) :- RulesConsidered >  100000,  RulesConsidered =<  250000.
isInThisInterval(RulesConsidered, interval250K_500K) :- RulesConsidered >  250000,  RulesConsidered =<  500000.
isInThisInterval(RulesConsidered, interval500K_750K) :- RulesConsidered >  500000,  RulesConsidered =<  750000.
isInThisInterval(RulesConsidered, interval750K_1M)   :- RulesConsidered >  750000,  RulesConsidered =< 1000000.
isInThisInterval(RulesConsidered, afterFirst1M)      :- RulesConsidered > 1000000.

intervalPredecessor(interval1K_10K,    first1K).
intervalPredecessor(interval10K_25K,   interval1K_10K).
intervalPredecessor(interval25K_50K,   interval10K_25K).
intervalPredecessor(interval50K_100K,  interval25K_50K).
intervalPredecessor(interval100K_250K, interval50K_100K).
intervalPredecessor(interval250K_500K, interval100K_250K).
intervalPredecessor(interval500K_750K, interval250K_500K).
intervalPredecessor(interval750K_1M,   interval500K_750K).
intervalPredecessor(afterFirst1M,      interval750K_1M).

beatsBestInEarlierColumn(first1K, _, _, _, _).  % This one is first, so no need to check earlier ones.

beatsBestInEarlierColumn(ThisInterval, Lower, Signature, SeedType/SeedExampleNum, PR) :-
	intervalPredecessor(ThisInterval, PredecessorInterval),
	recorded(jws_best_in_column, best(PredecessorInterval, Lower, Signature, SeedType/SeedExampleNum, OldRecall, OldPrecision, _, _, _, _), _),
	!, PR > (OldPrecision * OldRecall). % Stop once an entry found.
beatsBestInEarlierColumn(ThisInterval, Lower, Signature, SeedType/SeedExampleNum, PR) :-
	intervalPredecessor(ThisInterval, PredecessorInterval),
	beatsBestInEarlierColumn(PredecessorInterval, Lower, Signature, SeedType/SeedExampleNum, PR).

beatsBestInEarlierRow(first1K, _, _, _ , _).  % This one is first, so need to check earlier ones.

beatsBestInEarlierRow(ThisInterval, Lower, Signature, SeedType/SeedExampleNum, PR) :-
	intervalPredecessor(ThisInterval, PredecessorInterval),
	recorded(jws_best_in_row, best(PredecessorInterval, Lower, Signature, SeedType/SeedExampleNum, OldRecall, OldPrecision, _, _, _, _), _),
	!, PR > (OldPrecision * OldRecall). % Stop once an entry found.
beatsBestInEarlierRow(ThisInterval, Lower, Signature, SeedType/SeedExampleNum, PR) :-
	intervalPredecessor(ThisInterval, PredecessorInterval),
	beatsBestInEarlierRow(PredecessorInterval, Lower, Signature, SeedType/SeedExampleNum, PR).


jws_keep_best_in_column(Precision, Recall, LabelRaw, Clause) :-	
	safe_label(LabelRaw, Label),
	number(Precision), number(Recall),
	Lower is integer(20 * Recall) * 5,
	(jws_setting(            current_seed_example,      [SeedType, SeedExampleNum], ok)
	  -> true ;  jws_setting(first_seed_example,        [SeedType, SeedExampleNum], ok)),
	fpd_setting(rulesConsidered, RulesConsidered, ok),
	isInThisInterval(RulesConsidered, IntervalOfRulesConsidered),
	(recorded(jws_best_in_column, best(IntervalOfRulesConsidered, Lower, Signature, SeedType/SeedExampleNum, OldRecall, OldPrecision, _, _, _, _), NodeRef)
	   -> EraseNodeRef = true ; jws_get_signature(Signature), OldRecall = 0.0, OldPrecision = 0.0, EraseNodeRef = false), % First time seen.
	number(OldPrecision), number(OldRecall),
	PR     is    Precision *    Recall,
	BestPR is OldPrecision * OldRecall,
	beatsBestInEarlierColumn(IntervalOfRulesConsidered, Lower, Signature, SeedType/SeedExampleNum, PR),
	(PR > BestPR
	   -> (EraseNodeRef -> erase(NodeRef) ; true),
	      jws_count_matches_on_tune_set(pos, Clause, PosTuneMatches, PosTuneCount),
	      jws_count_matches_on_tune_set(neg, Clause, NegTuneMatches, NegTuneCount),
	      uw_format("    New best ~p prec x recall [recall bin ~d] = ~p (was ~p)   Tune: ~p/~p pos  ~p/~p neg~n",
		        [IntervalOfRulesConsidered, Lower, PR, BestPR, PosTuneMatches, PosTuneCount, NegTuneMatches, NegTuneCount]),
	      jws_set(need_to_report_best_ins, true),
	      recorda(jws_best_in_column, best(IntervalOfRulesConsidered, Lower, Signature, SeedType/SeedExampleNum, Recall, Precision, Label, RulesConsidered, [PosTuneMatches/PosTuneCount, NegTuneMatches/NegTuneCount], Clause), _)
	   ;  true).

jws_keep_best_in_row(Precision, Recall, LabelRaw, Clause) :-
	safe_label(LabelRaw, Label),
	number(Precision), number(Recall),
	Lower is integer(20 * Precision) * 5,
	(jws_setting(            current_seed_example,      [SeedType, SeedExampleNum], ok)
	  -> true ;  jws_setting(first_seed_example,        [SeedType, SeedExampleNum], ok)),
	fpd_setting(rulesConsidered, RulesConsidered, ok),
	isInThisInterval(RulesConsidered, IntervalOfRulesConsidered),
	(recorded(jws_best_in_row, best(IntervalOfRulesConsidered, Lower, Signature, SeedType/SeedExampleNum, OldRecall, OldPrecision, _, _, _, _), NodeRef)
	  -> EraseNodeRef = true ; jws_get_signature(Signature), OldRecall = 0.0, OldPrecision = 0.0, EraseNodeRef = false), % First time seen.
	number(OldPrecision), number(OldRecall),
	PR     is    Precision *    Recall,
	BestPR is OldPrecision * OldRecall,
	beatsBestInEarlierRow(IntervalOfRulesConsidered, Lower, Signature, SeedType/SeedExampleNum, PR),
	(PR > BestPR
	   -> (EraseNodeRef -> erase(NodeRef) ; true),
	      jws_count_matches_on_tune_set(pos, Clause, PosTuneMatches, PosTuneCount),
	      jws_count_matches_on_tune_set(neg, Clause, NegTuneMatches, NegTuneCount),
	      uw_format("    New best ~p prec x recall [prec bin ~d] = ~p (was ~p)   Tune: ~p/~p pos  ~p/~p neg~n",
		        [IntervalOfRulesConsidered, Lower, PR, BestPR, PosTuneMatches, PosTuneCount, NegTuneMatches, NegTuneCount]),
	      jws_set(need_to_report_best_ins, true),	      
	      recorda(jws_best_in_row, best(IntervalOfRulesConsidered, Lower, Signature, SeedType/SeedExampleNum, Recall, Precision, Label, RulesConsidered, [PosTuneMatches/PosTuneCount, NegTuneMatches/NegTuneCount], Clause), _)
	   ;  true).

jws_help_checkpoint_prec_recall_grid(Stream) :-
	% Need to be robust of this being called before the saved values are unsaved.  And also handle the initial case.
	((setting(search, SearchType), uw_isa_randomized_search(SearchType))
	  -> (fpd_setting(            rulesConsidered,            RulesConsidered,      ok)
	       -> true ; (jws_setting(rulesConsidered_saved,      RulesConsidered,      ok) -> true ; RulesConsidered      = 0)),
	     (fpd_setting(            annEvaluatedRules,          AnnEvaluatedRules,    ok)
	       -> true ; (jws_setting(annEvaluatedRules_saved,    AnnEvaluatedRules,    ok) -> true ; AnnEvaluatedRules    = 0)),
	     (fpd_setting(            acceptableRules,            AcceptableRules,      ok)
	       -> true ; (jws_setting(acceptableRules_saved,      AcceptableRules,      ok) -> true ; AcceptableRules      = 0))
	  ;  RulesConsidered = 0, AnnEvaluatedRules = 0, AcceptableRules = 0), % If a REGULAR search, want to start at ZERO for these.
	(jws_setting(            totalRulesConsidered,       TotalRulesConsidered, ok)
	  -> true ; (jws_setting(totalRulesConsidered_saved, TotalRulesConsidered, ok) -> true ; TotalRulesConsidered = 0)),
	(jws_setting(            seeds_processed,            SeedsSoFar,           ok)
	  -> true ; (jws_setting(seeds_processed_saved,      SeedsSoFar,           ok) -> true ; SeedsSoFar           = 0)),
	(jws_setting(            current_seed_example,      [SeedType, SeedExampleNum], ok)
	  -> true ; (jws_setting(first_seed_example,        [SeedType, SeedExampleNum], ok) -> true ; SeedType = unk, SeedExampleNum = -1)),
	jws_setting(checkpoint_reloads,   CheckpointReloadsCounter), % Should be already set.
	format(Stream, ":- jws_set(checkpoint_reloads,         ~p).~n",    [CheckpointReloadsCounter]),
	format(Stream, ":- jws_set(rulesConsidered_saved,      ~p).~n",    [RulesConsidered]),
	format(Stream, ":- jws_set(annEvaluatedRules_saved,    ~p).~n",    [AnnEvaluatedRules]),
	format(Stream, ":- jws_set(acceptableRules_saved,      ~p).~n",    [AcceptableRules]),
	format(Stream, ":- jws_set(totalRulesConsidered_saved, ~p).~n",    [TotalRulesConsidered]),
	format(Stream, ":- jws_set(seeds_processed_saved,      ~p).~n",    [SeedsSoFar]),
	patch_type_number(SeedType, SeedExampleNum),
	(SeedExampleNum < 0 -> true  % If not set yet, then dont save anything.  This will tell aleph which seed to use when restarting.
	                ;  format(Stream, ":- jws_set(first_seed_example,        [~p,~p]).~n", [SeedType, SeedExampleNum])),
	(recorded(search, current(_,Nodes,_), _)
	  -> true ; (jws_setting(nodes_saved,   Nodes,   ok) -> true ; Nodes = 0)),
	format(Stream, ":- jws_set(nodes_saved,                ~p).~n",    [Nodes]),
	(recorded(rls, restart(Restart), _)
	  -> true ; (jws_setting(restart_saved, Restart, ok) -> true ; Restart = 0)),
	format(Stream, ":- jws_set(restart_saved,              ~p).~n",    [Restart]),
	(recorded(rls, move(   Move),    _)
	  -> true ; (jws_setting(move_saved,    Move,    ok) -> true ; Move    = 0)),
	format(Stream, ":- jws_set(move_saved,                 ~p).~n",    [Move]),
	fail.
jws_help_checkpoint_prec_recall_grid(Stream) :-
	format(Stream, "~n% Rules Learned Previously:~n", []), % First collect the old saved rules.
	recorded(jws_theory_saved, rule(Label/Seed, Signature, Clause),_),
	format(Stream, ":- recordz(jws_theory_saved, rule(~p/~p, ~p, (~q)), _).~n", [Label, Seed, Signature, Clause]),
	fail.
jws_help_checkpoint_prec_recall_grid(Stream) :-
	format(Stream, "~n% Rules Learned This Run:~n", []),
	jws_get_signature(Signature),
	recorded(aleph, theory(_, LabelRaw/Seed, Clause,_,_), _),
	safe_label(LabelRaw, Label),
	format(Stream, ":- recordz(jws_theory_saved, rule(~p/~p, ~p, (~q)), _).~n", [Label, Seed, Signature, Clause]),
	fail.
jws_help_checkpoint_prec_recall_grid(Stream) :-
	setting(search, SearchType), uw_isa_randomized_search(SearchType), %%%%% ONLY DO THIS FOR RANDOMIZED SEARCHES (to keep full-theory file sizes small)
	format(Stream, "~n% Best in Row:~n", []),
	recorded(jws_best_in_row,    best(IntervalOfRulesConsidered, Lower, Signature, SeedType/SeedExampleNum, Recall, Precision, Label, RulesConsidered, [PosTuneMatches/PosTuneCount, NegTuneMatches/NegTuneCount], Clause), _),
	format(Stream, ":- recordz(jws_best_in_row, best(~p, ~p, ~p, ~p/~p, ~p, ~p, ~p, ~p, [~p/~p, ~p/~p], (~q)), _).~n",
		[IntervalOfRulesConsidered, Lower, Signature, SeedType, SeedExampleNum, Recall, Precision, Label, RulesConsidered,
		 PosTuneMatches, PosTuneCount, NegTuneMatches, NegTuneCount, Clause]),
	fail.
jws_help_checkpoint_prec_recall_grid(Stream) :-
	setting(search, SearchType), uw_isa_randomized_search(SearchType), %%%%% ONLY DO THIS FOR RANDOMIZED SEARCHES (to keep full-theory file sizes small)
	format(Stream, "~n% Best in Column:~n", []),
	recorded(jws_best_in_column, best(IntervalOfRulesConsidered, Lower, Signature, SeedType/SeedExampleNum, Recall, Precision, Label, RulesConsidered, [PosTuneMatches/PosTuneCount, NegTuneMatches/NegTuneCount], Clause), _),
	format(Stream, ":- recordz(jws_best_in_column, best(~p, ~p, ~p, ~p/~p, ~p, ~p, ~p, ~p, [~p/~p, ~p/~p], (~q)), _).~n",
		[IntervalOfRulesConsidered, Lower, Signature, SeedType, SeedExampleNum, Recall, Precision, Label, RulesConsidered,
		 PosTuneMatches, PosTuneCount, NegTuneMatches, NegTuneCount, Clause]),
	fail.
jws_help_checkpoint_prec_recall_grid(Stream) :- % Save all the "done-with seeds"
	format(Stream, "~n", []),
	format(Stream, ":- retract_all(aleph, atoms_left(_,_)).~n", []),
	recorded(aleph,atoms_left(Type, Intervals),_),
	% This will get loaded AFTER the read_all and so will overwrite old values.
	format(Stream, ":- recordz(aleph, atoms_left(~p, ~p), _).~n",
		[Type, Intervals]),
	fail.
jws_help_checkpoint_prec_recall_grid(_).
jws_checkpoint_prec_recall_grid(Status) :-
	once((Status = first_time ; jws_setting(current_seed_example, _))), % When run is done, still get a call.  Trap here before file is opened and nothing is saved. to do - figure out why this final call occurs.
	jws_setting(precision_recall_checkpoint_file, File),
	uw_format("% ***** Writing out the precision-recall checkpoint file~n%     ~p~n", [File]),
	aleph_open(File, write, Stream),
	jws_help_checkpoint_prec_recall_grid(Stream),
	close(Stream),
	uw_format("%  Done writing out the precision-recall checkpoint file.~n"),
	fail.
jws_checkpoint_prec_recall_grid(_).  % Catch any unanticipated fails in the above.

jws_initialize_total_rules_considered :-  % Dont need to do it this way (since only set once, but might as well for consistency).
	(jws_setting(  totalRulesConsidered_saved, SavedValue, ok_if_not_set)
	  -> jws_noset(totalRulesConsidered_saved),
	     jws_set(totalRulesConsidered, SavedValue)
	  ;  jws_set(totalRulesConsidered, 0)).
jws_initialize_seeds_processed :-
	(jws_setting(  seeds_processed_saved,      SavedValue, ok_if_not_set)
	  -> jws_noset(seeds_processed_saved),
	     jws_set(seeds_processed, SavedValue)
	  ;  jws_set(seeds_processed, 0)).
jws_initialize_nodes(Strategy) :-
	(jws_setting(  nodes_saved, SavedValue, ok_if_not_set)
	  -> jws_noset(nodes_saved),
	     recorda(Strategy, nodes(SavedValue), _)
	  ;  recorda(Strategy, nodes(0),          _)).
jws_initialize_restart(Strategy) :-
	(jws_setting(  restart_saved, SavedValue, ok_if_not_set)
	  -> jws_noset(restart_saved),
	     recorda(Strategy, restart(SavedValue), _)
	  ;  recorda(Strategy, restart(1),          _)).
jws_initialize_move(Strategy) :-
	(jws_setting(  move_saved, SavedValue, ok_if_not_set)
	  -> jws_noset(move_saved),
	     recorda(Strategy, move(SavedValue), _)
	  ;  recorda(Strategy, move(1),          _)).

jws_get_lower(Lower) :-
	uw_member(Lower, [0, 5, 10, 15, 20, 25, 30, 35, 40, 45,  50,
                            55, 60, 65, 70, 75, 80, 85, 90, 95, 100]).

patch_type_number(SeedType, SeedExampleNum) :-
	var(SeedType), var(SeedExampleNum), !,
	(jws_setting(            current_seed_example,      [SeedType, SeedExampleNum], ok)
	  -> true ; (jws_setting(first_seed_example,        [SeedType, SeedExampleNum], ok) -> true ; SeedType = unk, SeedExampleNum = -1)).
patch_type_number(_, _).	

jws_help_report_best_ins(Stream) :-
	jws_get_lower(Lower100),
	format(Stream, "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%~n", []),
	recorded(jws_best_in_column, best(IntervalOfRulesConsidered, Lower100, Signature, ExType/ExNumber, Recall, Precision, Label, LearnedAt, [PosTuneMatches/PosTuneCount, NegTuneMatches/NegTuneCount], Clause), _),
%	patch_type_number(ExType, ExNumber),
	number(Precision), number(Recall),
	Lower is Lower100 / 100,
	Upper is Lower + 0.05,
	format(Stream, "% rp, ~4f, ~4f~n", [Recall, Precision]), % This line is included so can grep the file and make a comma-separated file for Excel.
	format(Stream, "% Best Adjusted Precision for Recall in [~2f,~2f) is ~3f, Label=~p~n%   Learned@~p for ~p~p~n",
		       [Lower, Upper, Precision, Label, LearnedAt, ExType, ExNumber]),
	format(Stream, "%   Tuneset:  ~p/~p pos  ~p/~p neg~n", [PosTuneMatches, PosTuneCount, NegTuneMatches, NegTuneCount]),
	format(Stream, "best_precision_rule(~p, ~p, ~p, ~p(~p), recall(~p), precision(~p), ~p, tuneset(pos(~p/~p), neg(~p/~p)), (~q)).~n", 
		       [IntervalOfRulesConsidered, Lower100, Signature, ExType, ExNumber, Recall, Precision, Label, PosTuneMatches, PosTuneCount, NegTuneMatches, NegTuneCount, Clause]),
	format(Stream, "/*~n", []),
	uw_pp_dclause_to_stream(Stream, Clause),
%	jws_pp_dclause_with_typed_vars(Clause), % to do: need to write to a stream ...
	format(Stream, "*/~n", []),
	fail.
jws_help_report_best_ins(Stream) :-
	jws_get_lower(Lower100),
	format(Stream, "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%~n", []),
	recorded(jws_best_in_row, best(IntervalOfRulesConsidered, Lower100, Signature, ExType/ExNumber, Recall, Precision, Label, LearnedAt, [PosTuneMatches/PosTuneCount, NegTuneMatches/NegTuneCount], Clause), _),
%	patch_type_number(ExType, ExNumber),
	number(Precision), number(Recall),
	Lower is Lower100 / 100,
	Upper is Lower + 0.05,
	format(Stream, "% rp, ~4f, ~4f~n", [Recall, Precision]), % This line is included so can grep the file and make a comma-separated file for Excel.
	format(Stream, "% Best Recall for Adjusted Precision in [~2f,~2f) is ~3f, Label=~p~n%   Learned@~p for ~p~p~n",
		       [Lower, Upper, Recall, Label, LearnedAt, ExType, ExNumber]),
	format(Stream, "%   Tuneset:  ~p/~p pos  ~p/~p neg~n", [PosTuneMatches, PosTuneCount, NegTuneMatches, NegTuneCount]),
	format(Stream, "best_recall_rule(~p, ~p, ~p, ~p(~p), recall(~p), precision(~p), ~p, tuneset(pos(~p/~p), neg(~p/~p)), (~q)).~n", 
		       [IntervalOfRulesConsidered, Lower100, Signature, ExType, ExNumber, Recall, Precision, Label, PosTuneMatches, PosTuneCount, NegTuneMatches, NegTuneCount, Clause]),
	format(Stream, "/*~n", []),
	uw_pp_dclause_to_stream(Stream, Clause),
	format(Stream, "*/~n", []),
	fail.
jws_help_report_best_ins(Stream) :-
	format(Stream, "~n% Rules Learned Previously:~n", []), % First collect the old saved rules.
	recorded(jws_theory_saved, rule(Label/Seed, Signature, Clause),_),
	format(Stream, "learned_rule_in_theory(~p/~p, ~p, (~q)).~n", [Label, Seed, Signature, Clause]),
	fail.
jws_help_report_best_ins(Stream) :-
	format(Stream, "~n% Rules Learned This Run:~n", []),
	jws_get_signature(Signature),
	recorded(aleph, theory(_, LabelRaw/Seed, Clause, _, _), _),
	safe_label(LabelRaw, Label),
	format(Stream, "learned_rule_in_theory(~p/~p, ~p, (~q)).~n", [Label, Seed, Signature, Clause]),
	fail.
jws_help_report_best_ins(_).
jws_report_best_ins(Status) :-
	setting(search, Search),
	once((Status = first_time ; Status = done_with_seed ; Status = final_wrapup ; uw_isa_randomized_search(Search))), % Dont save anything on a normal search unless done with a seed (in case need to restart).
	once((Status = first_time ; Status = done_with_seed ; Status = final_wrapup ; 
	      (Search \= scs, Rsave_anyway is random, Rsave_anyway < 0.100) ; % Periodically write even if not needed so that the counters are reasonably up to date in the file stored on disk.
	      (Search  = scs, Rsave_anyway is random, Rsave_anyway < 0.001) ; % SCS ends after one random-draw of a clause, so save less often.
	      jws_setting(need_to_report_best_ins, true, ok_if_not_set))),
	jws_setting(precision_recall_file, File),
	uw_format("~n% ***** Writing out the grid of best rules~n%     ~p~n", [File]),
	setting(minpos, MinPosToCover),
	setting(noise,  MaxNegToCover),
	setting(minacc, MinAccuracy),
	setting(evalfn, EvalFn),
	jws_get_aleph_example_counts(PosCount, NegCount),
	jws_setting(positive_examples_filtered_out, PosFiltered),
	jws_setting(inverse_sampling_rate_of_neg_examples, SamplingRate),
	(jws_setting(            seeds_processed,            SeedsSoFar,           ok)
	  -> true ; (jws_setting(seeds_processed_saved,      SeedsSoFar,           ok) -> true ; SeedsSoFar           = 0)),
	(fpd_setting(            rulesConsidered,            RulesConsidered,      ok)
	  -> true ; (jws_setting(rulesConsidered_saved,      RulesConsidered,      ok) -> true ; RulesConsidered      = 0)),
	(jws_setting(            totalRulesConsidered,       TotalRulesConsidered, ok)
	  -> true ; (jws_setting(totalRulesConsidered_saved, TotalRulesConsidered, ok) -> true ; TotalRulesConsidered = 0)),
	aleph_open(File, write, Stream),
	format(Stream, "% Best rules (out of ~D [~D current seed] considered from ~D seeds) in the precision-recall stripes:~n",
	       [TotalRulesConsidered, RulesConsidered, SeedsSoFar]),
	format(Stream, "%   MinPos = ~D,  MaxNeg = ~D,  MinAccuracy = ~2f~n",
	       [MinPosToCover, MaxNegToCover, MinAccuracy]),
	((uw_isa_randomized_search(Search),	  
	  setting(tries,    RandomStarts), % Be robust in case these arent set yet, though they should be.
	  setting(moves,    LocalMoves))
	  -> 	setting(rls_type, RLStype),
		format(Stream, "%   Pos = ~D[+~D],  Neg = ~D(x~3f),  Search = ~p/~p,  Eval = ~p,  RandomStarts = ~p,  LocalMoves=~p~2n",
	               [PosCount, PosFiltered, NegCount, SamplingRate, Search, RLStype, EvalFn, RandomStarts, LocalMoves])
	  ;     format(Stream, "%   Pos = ~D[+~D],  Neg = ~D(x~3f),  Search = ~p,  Eval = ~p~2n",
                       [PosCount, PosFiltered, NegCount, SamplingRate, Search, EvalFn])),
	jws_help_report_best_ins(Stream),
	close(Stream),
	uw_format("%  Done writing out the grid of best rules.~n"),
	jws_set(need_to_report_best_ins, false),
	jws_checkpoint_prec_recall_grid(Status), !.
jws_report_best_ins(Status) :-
	setting(search, Search),
	once((Status = first_time ; Status = done_with_seed ; Status = final_wrapup ; 
	      (uw_isa_randomized_search(Search), jws_setting(need_to_report_best_ins, true, ok_if_not_set)))),
	told,
	uw_format("Failed jws_report_best_ins(~p).~n", [Status]),
	break.  % Catch any unanticipated fails in the above.
jws_report_best_ins(_).  % Ok to fail other times.

jws_get_signature([Search, EvalFn, condor(N), minAcc(MinAccuracy), maxNodes(MaxNodes), minPos(MinPosToCover)]) :-
	setting(search, SearchA),
	(SearchA = rls -> setting(rls_type, Search) ; Search = SearchA),
	setting(minpos, MinPosToCover), % setting(noise,  MaxNegToCover),
	setting(minacc, MinAccuracy),
	setting(evalfn, EvalFn),
	setting(nodes,  MaxNodes),
	recorded(uw_condor_run_number, condor(N), _), !.
jws_get_signature(_) :-
	told, uw_format("Something went wrong with jws_get_signature!"), break.
	

jws_help_generate_uniform_clauselength_dist(Current, Last, _, []) :- Current > Last, !.
jws_help_generate_uniform_clauselength_dist(Current, Last, Fraction, [Fraction-Current | RestDistribution]) :-
	CurrentPlus1 is Current + 1,
	jws_help_generate_uniform_clauselength_dist(CurrentPlus1, Last, Fraction, RestDistribution).
jws_generate_uniform_clauselength_dist :-
	setting(clauselength, ClauseLength),
	Fraction is 1.0 / (ClauseLength - 1), % Give no probablity to the head-only (ie, length=1) clause.
	jws_help_generate_uniform_clauselength_dist(2, ClauseLength, Fraction, Distribution),
	set(clauselength_distribution, Distribution),
	uw_format("% clauselength_distribution = ~p~n", [Distribution]),
	fail.
jws_generate_uniform_clauselength_dist. % Design so always succeeds (and only once).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Some minor utilities.
	

jws_short_search_name(heuristic,   heur)    :- !. % Everything else has a short name already!
jws_short_search_name(rls,         RlsType) :- % Except for the stochastic searches, where the specific name is stored elsewhere.
	!, setting(rls_type,RlsType).
jws_short_search_name(X, X).

jws_short_evalfn_name(compression,     cmprss)  :- !.
jws_short_evalfn_name(jws_compression, jcmprss) :- !.
jws_short_evalfn_name(foil_gain,       foil)    :- !.
jws_short_evalfn_name(coverage,        cov)     :- !.
jws_short_evalfn_name(jws_coverage,    jcov)    :- !.
jws_short_evalfn_name(laplace,         lap)     :- !.
jws_short_evalfn_name(jws_laplace,     jlap)    :- !.
jws_short_evalfn_name(posonly,         pos)     :- !.
jws_short_evalfn_name(mestimate,       m_est)   :- !.
jws_short_evalfn_name(pbayes,          pbay)    :- !.
jws_short_evalfn_name(jws_f1,          f1)      :- !.
jws_short_evalfn_name(jws_pr,          pr)      :- !.
jws_short_evalfn_name(X, X).

jws_open_size(_) :-
%	uw_format("Computing the size of OPEN.~n"),
	jws_set(size_of_open, 0),
	recorded(openlist, Gains,_),
	get_node(Gains, _, _, _),
	jws_increment(size_of_open),
	fail.
jws_open_size(N) :- jws_setting(size_of_open, N).



jws_help2_read_tune_set_examples(Type, Stream) :-
	repeat,
	read(Stream, Fact),
	(Fact = end_of_file
	  -> !
	  ;  (Type = pos 
		   -> recordz(jws_tune_set_pos, Fact, _),
		      jws_increment(tune_set_counter_pos)
		   ;  recordz(jws_tune_set_neg, Fact, _),
		      jws_increment(tune_set_counter_neg)),
	     fail).

jws_help_read_tune_set_examples(Stream, Type, Count) :-
	(Type = pos 
	   -> jws_set(    tune_set_counter_pos, 0), retract_all(jws_tune_set_pos)
	   ;  jws_set(    tune_set_counter_neg, 0), retract_all(jws_tune_set_neg)),
	jws_help2_read_tune_set_examples(Type, Stream),
        (Type = pos 
	   -> jws_setting(tune_set_counter_pos, Count)
	   ;  jws_setting(tune_set_counter_neg, Count)).

jws_read_tune_set_examples(File, Type) :-
	uw_format("~n% Opening tune set ~p.~n", [File]),
	aleph_open(File,read,Stream), !,
	jws_help_read_tune_set_examples(Stream, Type, Count),
	uw_format("% Have read ~D ~p tuning-set examples.~n", [Count, Type]).
jws_read_tune_set_examples(File, _):-
	p1_message('% cannot open tune set'), p_message(File).  % Want to fail w/o hanging since there might not be a tuning set.

% To do: clean up so tune/test share code.
jws_help2_read_test_set_examples(Type, Stream) :-
	repeat,
	read(Stream, Fact),
	(Fact = end_of_file
	  -> !
	  ;  (Type = pos 
		   -> recordz(jws_test_set_pos, Fact, _),
		      jws_increment(test_set_counter_pos)
		   ;  recordz(jws_test_set_neg, Fact, _),
		      jws_increment(test_set_counter_neg)),
	     fail).

jws_help_read_test_set_examples(Stream, Type, Count) :-
	(Type = pos 
	   -> jws_set(    test_set_counter_pos, 0), retract_all(jws_test_set_pos)
	   ;  jws_set(    test_set_counter_neg, 0), retract_all(jws_test_set_neg)),
	jws_help2_read_test_set_examples(Type, Stream),
        (Type = pos 
	   -> jws_setting(test_set_counter_pos, Count)
	   ;  jws_setting(test_set_counter_neg, Count)).

jws_read_test_set_examples(File, Type) :-
	uw_format("~n% Opening test set ~p.~n", [File]),
	aleph_open(File,read,Stream), !,
	jws_help_read_test_set_examples(Stream, Type, Count),
	uw_format("% Have read ~D ~p test-set examples.~n", [Count, Type]).
jws_read_test_set_examples(File, _):-
	p1_message('% cannot open test set'), p_message(File).  % Want to fail w/o hanging since there might not be a tuning set.

% Does this clause match at least "ProofsRequested" examples of this Type (pos or neg)?
jws_eval_on_tune_set(_, _, 0) :- !.
jws_eval_on_tune_set(Type, (Head :- Body), ProofsRequested) :-
	integer(ProofsRequested), ProofsRequested > 0, !,
	jws_set(temp_tune_set_counter, 0),
	jws_get_tune_set_example(Type, Head),
	(call(Body) 
	   -> jws_increment(temp_tune_set_counter),
	      jws_setting(  temp_tune_set_counter, Count),
	      Count >= ProofsRequested
	   ; fail).
jws_eval_on_tune_set(_, _, ProofsRequested) :-
	told, uw_format("Bad jws_eval_on_tune_set request: ProofsRequested = ~p~n", [ProofsRequested]), break.

jws_get_tune_set_example(pos, Example) :- recorded(jws_tune_set_pos, Example, _).
jws_get_tune_set_example(neg, Example) :- recorded(jws_tune_set_neg, Example, _).
jws_get_test_set_example(pos, Example) :- recorded(jws_test_set_pos, Example, _).
jws_get_test_set_example(neg, Example) :- recorded(jws_test_set_neg, Example, _).

jws_count_matches_on_tune_set(Type, (Head :- Body), _, _):-
	jws_set(temp_tune_set_counter, 0),
	jws_get_tune_set_example(Type, Head),
	(call(Body) -> jws_increment(temp_tune_set_counter), fail
		    ;                                        fail).
jws_count_matches_on_tune_set(Type, _, MatchesCount, TotalTuneSetSize):-
	(Type = pos 
	   -> jws_setting(tune_set_counter_pos, TotalTuneSetSize)
	   ;  jws_setting(tune_set_counter_neg, TotalTuneSetSize)),
	jws_setting(temp_tune_set_counter, MatchesCount).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


jws_count_node_items_at_this_index(Index) :-
	recorded(Index, node(_,_,_,_,_,_,_,_), _),
	jws_increment(jws_count_node_items),	
	fail.
jws_count_node_items_at_this_index(_).

help_jws_count_node_items(Index):- Index < 0, !.
help_jws_count_node_items(Index):-
	jws_count_node_items_at_this_index(Index),
	NextIndex is Index - 1,
	help_jws_count_node_items(NextIndex).

jws_count_node_items :-
	setting(nodes,MaxNodes),
	LastIndex is MaxNodes // 10000, % Changed from 1000 by JWS.
	jws_set(jws_count_node_items, 0),
	help_jws_count_node_items(LastIndex),
	jws_setting(jws_count_node_items, Count),
	uw_format("  *** Count of recorded(NodeIndex, node(...), _) = ~p~n", [Count]).

help_jws_count_records(Type/Pred) :-
	recorded(Type, Pred, Ref), 
	not uw_erased(Ref),
%	uw_format("   ~p~n", [Pred]),
	jws_increment(record_counter),
	fail.
help_jws_count_records(_).
jws_count_records(Type/Pred) :-
	uw_format("  *** Count of recorded(~p, ~p, _) = ", [Type, Pred]),
	jws_set(record_counter, 0),
	help_jws_count_records(Type/Pred),
	jws_setting(record_counter, Counter),
	uw_format("~D~n", [Counter]).
jws_count_all_records :-
	current_key(Name, Key),
	key_statistics(Key, Entries, SizeClauses, SizeMemory),
	SizeMemory > 10000, % Only report LARGE items.
	uw_format("YAP Key ~p [~p]: entries=~p size_clauses=~p size_mem=~p~n", [Name, Key, Entries, SizeClauses, SizeMemory]),
	fail.
jws_count_all_records.

jws_count_all_records_old :-
	% Report the following so we can watch to see what happens before crashes, to see if there are memory leaks (eg, constantly grow?), etc.
	HeapSpace is heapused // 1000000, % Doesnt work: LocalSpace is local // 1000, GlobalSpace is global // 1000,
%	uw_format("% HEAPSPACE = ~D MB, LOCAL = ~D KB, GLOBAL = ~D KB~n", [HeapSpace, LocalSpace, GlobalSpace]),
	uw_format("% HEAPSPACE = ~D MB~n", [HeapSpace]),

	jws_count_records(jws_intervals_pos/_),
	jws_count_records(jws_intervals_neg/_),
	jws_count_records(jws_counters_pos/_),
	jws_count_records(jws_counters_neg/_),
/*	
	jws_count_node_items,
	jws_count_records(aleph/_),
	jws_count_records(covers/_),
	jws_count_records(jws_repeats/_),
%	jws_count_records(pclause/_),
	jws_count_records(openlist/_),
	jws_count_records(nodes/_),
	jws_count_records(gains/_),
%	jws_count_records(aleph_dyn/_),
%	jws_count_records(clauseprior/_),
	jws_count_records(rls/_),
	jws_count_records(rls/seen(_,_)),
	jws_count_records(rls/restart(_)),
	jws_count_records(rls/move(_)),
	jws_count_records(rls/nodes(_)),
	jws_count_records(rls/selected(_,_,_,_)),
	jws_count_records(rls/parent_stats(_,_,_)),
%	jws_count_records(search/_),
%	jws_count_records(search/selected(_,_,_,_)),
%	jws_count_records(search/current(_,_,_)),
%	jws_count_records(search/nextnode(_)),
%	jws_count_records(search/best(_)),
%	jws_count_records(search/best_label(_)),
%	jws_count_records(search/expansion(_,_,_,_)), 
	uw_format("~n"),
*/
	true.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Create Precision-Recall Curves



create_prec_recall(Label, FirstKrules, Fold, DataSet, CondorSeedNumbersInUse, MaxTheories, Search, EvalFn, MaxLen, MinAcc, MaxNodes, MinPos) :-
	eraseall(times_covered),
	(number(MinAcc) -> MinAccFraction is integer(100 * MinAcc) ; MinAccFraction = MinAcc),	
	jws_short_search_name(Search, SearchShort),
	jws_short_evalfn_name(EvalFn, EvalFnShort),
	uw_set(pr_curve_info, [MaxTheories, FirstKrules, Fold, DataSet]),
	MaxNodesInK is MaxNodes // 1000,
	uw_concat(['prec_recall/curve_', Label, 'first', FirstKrules, 'rules_', Fold, '_', DataSet, '_',
		    SearchShort,  '_', EvalFnShort,    '_cl', MaxLen, '_', 
		    MaxNodesInK, 'n_', MinAccFraction, 'minacc.csv'], File),
	uw_set(prec_recall_curve_file, File),
	uw_set(examples_considered, 0),
	eval_on_examples(DataSet, pos, CondorSeedNumbersInUse, Search, EvalFn, MaxLen, MinAcc, MaxNodes, MinPos),
	uw_set(examples_considered, 0),
	eval_on_examples(DataSet, neg, CondorSeedNumbersInUse, Search, EvalFn, MaxLen, MinAcc, MaxNodes, MinPos),
	report_prec_recall_curve.
report_prec_recall_curve :-
	uw_setting(prec_recall_curve_file, File),
	uw_setting(pr_curve_info,          [MaxTheories, FirstKrules, Fold, DataSet]),
	uw_format("~n% Opening CSV file containing the recall-precision curve:~n%   ~p.~2n", [File]),
	aleph_open(File, write, Stream),
	count_at_least_this_many_matches(DataSet, pos, 0, TotalPos),
	format(Stream, "K, Recall, Precision, Pos Covered, Neg Covered, TotalPos=~d, ~p~n",
	       [TotalPos, File]),
	uw_set(writing_first_line_in_pr_curve, true),
	report_pr_results(DataSet, MaxTheories, TotalPos, Stream),
	uw_setting(auc, AUC),
	(recorded(jws_cost_per_clause, CostPerClause, _) -> true ; CostPerClause = 1),  % Units are "K nodes."
	Work is MaxTheories * FirstKrules * CostPerClause,
	format(Stream, "~n , Work, Area, Max Theories, FirstKrules, CostPerClause (in K), Fold, File~n", []),
	format(Stream, "AUC, ~p, ~4f, ~p, ~p, ~p, ~p, ~p~n",
	       [Work, AUC, MaxTheories, FirstKrules, CostPerClause, Fold, File]),
	close(Stream),
	!.
create_prec_recall(Label, FirstKrules, Fold, DataSet, Search, EvalFn, MaxLen, MinAcc, MaxNodes, MinPos) :-
	told,
	uw_format("Something went wrong with create_prec_recall(~p, ~p, ~p, ~p, ~p, ~p, ~p, ~p, ~p, ~p)!~n",
		  [Label, FirstKrules, Fold, DataSet, Search, EvalFn, MaxLen, MinAcc, MaxNodes, MinPos]),
	break.

short_report_pr_results :-
	recorded(times_covered, stats(DataSet, ExampleType, TimesCovered, CountInThisBin), _),
	uw_format("~p ~p ~d matches occurred ~d times.~n", [DataSet, ExampleType, TimesCovered, CountInThisBin]),
	fail.
short_report_pr_results.

report_pr_results(DataSet, TimesCovered, TotalPos, Stream) :-
	TimesCovered >= 0, !,
	(recorded(times_covered, stats(DataSet, pos, TimesCovered, CountInThisBinPos), _)
		-> true ; CountInThisBinPos = 0),
	uw_format("~n"),
	uw_format("%   ~p ~p ~p examples matched exactly ~p theories.~n",
		  [CountInThisBinPos, DataSet, pos, TimesCovered]),
	count_at_least_this_many_matches(DataSet, pos, TimesCovered, AtLeastCountPos),
	uw_format("%      ~p ~p ~p examples matched at least ~p theories.~n",
		  [AtLeastCountPos, DataSet, pos, TimesCovered]),
	(recorded(times_covered, stats(DataSet, neg, TimesCovered, CountInThisBinNeg), _)
		-> true ; CountInThisBinNeg = 0),
	uw_format("%   ~p ~p ~p examples matched exactly ~p theories.~n",
		  [CountInThisBinNeg, DataSet, neg, TimesCovered]),
	count_at_least_this_many_matches(DataSet, neg, TimesCovered, AtLeastCountNeg),
	uw_format("%      ~p ~p ~p examples matched at least ~p theories.~n",
		  [AtLeastCountNeg, DataSet, neg, TimesCovered]),
	TotalMatched is AtLeastCountPos + AtLeastCountNeg,
	(TotalMatched > 0 % Should this be something like 3 to get rid of small-number effects?
	  -> Precision is 100 * (AtLeastCountPos / TotalMatched),
	     Recall is    100 * (AtLeastCountPos / TotalPos),
	     uw_setting(writing_first_line_in_pr_curve,     FirstLine),
	     (FirstLine
		-> format(Stream, "extrapolate, 0.000, ~3f~n", [Precision]),  % Extrapolate to recall=0.
		   uw_set(auc,           0),
		   uw_set(lastRecall,    0.0),
		   uw_set(lastPrecision, Precision),
		   uw_set(writing_first_line_in_pr_curve, false)
		;  true),
	     uw_setting(lastRecall,    OldRecall),
	     uw_setting(lastPrecision, OldPrecision),
	     Span is abs(Recall - OldRecall),
	     (Recall >= OldRecall -> true ; uw_format("% OldRecall > Recall !!!! ~2f vs. ~2f~n", [OldRecall, Recall]), break),
	     RectangleAUC is min(OldPrecision, Precision) * Span,
	     TriangleAUC  is (max(OldPrecision, Precision) - min(OldPrecision, Precision)) * Span / 2,
	     DeltaAUC is (RectangleAUC + TriangleAUC) / 10000,  % Correct for fact that numbers are 0-100% rather than 0-1.
	     uw_increment(auc,     DeltaAUC),
	     uw_set(lastRecall,    Recall),
	     uw_set(lastPrecision, Precision),
	     uw_format("% RECALL    = ~3f% = ~d/~d~n",
		       [Recall,    AtLeastCountPos, TotalPos]),  
	     uw_format("% PRECISION = ~3f% = ~d/(~d+~d)~n",
		       [Precision, AtLeastCountPos, AtLeastCountPos, AtLeastCountNeg]),
	     uw_setting(auc, AUC),
	     uw_format("% AUC (so far) is ~3f~n", [AUC]),
	     format(Stream, "~d, ~3f, ~3f, ~d, ~d, ~d~n",
	           [TimesCovered, Recall, Precision, AtLeastCountPos, AtLeastCountNeg, TotalPos])
	  ;  true),
	TimesCoveredMinusOne is TimesCovered - 1,
	report_pr_results(DataSet, TimesCoveredMinusOne, TotalPos, Stream).
report_pr_results(_, _, _, _).

count_at_least_this_many_matches(DataSet, ExampleType, MinTimesCovered, _) :-
	uw_set(at_least_counter, 0),
	recorded(times_covered, stats(DataSet, ExampleType, ThisTimesCovered, CountInThisBin), _),
	ThisTimesCovered >= MinTimesCovered,
	uw_increment(at_least_counter, CountInThisBin),
	fail.
count_at_least_this_many_matches(_, _, _, AtLeastCount) :-
	uw_setting(at_least_counter, AtLeastCount).	

load_rules_files(_,           _,    _,       _,                      _,      _,      _,      _,     _,        _) :-
	recorded(max_theories_to_use, MaxToLoad, _),
	uw_setting(rules_files_loaded, Count, ok_if_not_set),
	Count >= MaxToLoad, !.  % Stop if enough files loaded.

load_rules_files(_,           _,    _,       [],                     _,      _,      _,      _,     _,        _).
load_rules_files(FirstKrules, Fold, DataSet, [CondorSeedNumber | _], Search, 'any',  MaxLen, 'any', MaxNodes, MinPos) :-
	eval_fn_to_use('any', EvalFnToUse),
	min_acc_to_use('any', MinAccToUse),
	load_rules_files(FirstKrules, Fold, DataSet, [CondorSeedNumber], Search, EvalFnToUse, MaxLen, MinAccToUse, MaxNodes, MinPos),
	fail.
load_rules_files(FirstKrules, Fold, DataSet, [CondorSeedNumber | _], Search, 'any',  MaxLen, MinAcc, MaxNodes, MinPos) :-
	not (MinAcc = 'any'),
	eval_fn_to_use( 'any', EvalFnToUse),
	load_rules_files(FirstKrules, Fold, DataSet, [CondorSeedNumber], Search, EvalFnToUse, MaxLen, MinAcc, MaxNodes, MinPos),
	fail.
load_rules_files(FirstKrules, Fold, DataSet, [CondorSeedNumber | _], Search, EvalFn,  MaxLen, 'any', MaxNodes, MinPos) :-
	not (EvalFn = 'any'),
	min_acc_to_use('any', MinAccToUse),
	load_rules_files(FirstKrules, Fold, DataSet, [CondorSeedNumber], Search, EvalFn, MaxLen, MinAccToUse, MaxNodes, MinPos),
	fail.
load_rules_files(FirstKrules, Fold, DataSet, [_ | Tail],   Search, 'any', MaxLen, MinAcc, MaxNodes, MinPos) :-
	uw_setting(rules_files_loaded, Count),
	uw_format("% Have loaded ~d files so far.~n", [Count]),
	load_rules_files(FirstKrules, Fold, DataSet, Tail, Search, 'any', MaxLen, MinAcc, MaxNodes, MinPos).
load_rules_files(FirstKrules, Fold, DataSet, [CondorSeedNumber | Tail], Search, EvalFn, MaxLen, MinAcc, MaxNodes, MinPos) :-
	not (EvalFn = 'any'),
	uw_set(rules_file_loaded, false),
	load_rules_file( FirstKrules, Fold, DataSet, CondorSeedNumber, Search, EvalFn, MaxLen, MinAcc, MaxNodes, MinPos, false),
	load_rules_files(FirstKrules, Fold, DataSet,            Tail,  Search, EvalFn, MaxLen, MinAcc, MaxNodes, MinPos).

load_rules_file(_, Fold, _, CondorSeedNumber, Search, EvalFn, MaxLen, MinAcc, MaxNodes, MinPos, _) :-
	% Don't load more than once.
	recorded(jws_theories_loaded, loaded(Fold, CondorSeedNumber, Search, EvalFn, MaxLen, MinAcc, MaxNodes, MinPos), _),
	!, uw_set(rules_file_loaded, true).

load_rules_file(FirstKrules, Fold, DataSet, CondorSeedNumber, Search, EvalFn, MaxLen, MinAcc, MaxNodes, MinPos, _) :-
	uw_setting(precomputing, false, dont_complain_if_not_found),
	create_theory_matches_fileName(FirstKrules, Fold, DataSet, pos, CondorSeedNumber, Search, EvalFn, MaxLen, MinAcc, MaxNodes, _, FileNamePos),
%	uw_format("FileNamePos=~p~n", [FileNamePos]),
	create_theory_matches_fileName(FirstKrules, Fold, DataSet, neg, CondorSeedNumber, Search, EvalFn, MaxLen, MinAcc, MaxNodes, _, FileNameNeg),
	exists(FileNamePos), % Load the "cached" results rather than the rules themselves. (WARNING: this should only be used if NOT precomputing).
	(exists(FileNameNeg) -> true ; uw_format("POS file exists by not NEG! ~n  ~p~n", [FileNameNeg]), break, fail),
	!,  % Need to have both (to do: report error if only ONE present)
	(dontload_precomputed(Search, EvalFn, MinAcc)  % Note: this is AFTER the CUT, so dont try to load the rules.
	  -> uw_format("~nDont load this precomputed file: ~p/~p/~p~p~n", [Search, EvalFn, MinAcc, FileNamePos]), fail
	  ; true),
	uw_format("Loading the POS cached file:~n"), consult(FileNamePos),
	uw_format("Loading the NEG cached file:~n"), consult(FileNameNeg),
	uw_increment(rules_files_loaded),
	recorda(jws_theories_loaded, loaded(Fold, CondorSeedNumber, Search, EvalFn, MaxLen, MinAcc, MaxNodes, MinPos), _).

load_rules_file(_, Fold, _, CondorSeedNumber, Search, EvalFn, MaxLen, MinAcc, MaxNodes, MinPos, ComplainIfNotFound) :-
	MinAccFraction is integer(100 * MinAcc),	
	jws_short_search_name(Search, SearchShort),
	jws_short_evalfn_name(EvalFn, EvalFnShort),
	(dontload_rules(Search, EvalFn, MinAcc)
	  -> uw_format("~nDont load this 'raw' rules file: ~p/~p/~p(seed=~d)~n", [Search, EvalFn, MinAcc, CondorSeedNumber]), fail
	  ; true),
	(recorded(jws_use_old_file_names, true, _)
	   -> uw_concat(['prec_recall/datasets/NPonly_train', Fold, '_protein_location_condor', CondorSeedNumber,
		         '_', SearchShort, '_', EvalFnShort, '_cl', MaxLen, '_', MinAccFraction, 'minacc.yap'],
			  FileName1)
	   ;  FileName1 = 'ignore_this'), % Dont use the shorthand name any longer by default since different MaxNodes being used.
	MaxNodesInK is MaxNodes // 1000,
	uw_concat(['prec_recall/datasets/NPonly_train', Fold, '_protein_location_condor', CondorSeedNumber,
		   '_', SearchShort, '_', EvalFnShort, '_cl', MaxLen, '_', MaxNodesInK, 'n_', MinAccFraction, 'minacc.yap'],
		  FileName2),
	uw_format("~n~p~n~p~n", [FileName1, FileName2]),
	((exists(FileName1), consult(FileName1)) -> uw_increment(rules_files_loaded) % Handle old and new formats.
		 ; ((exists(FileName2), consult(FileName2)) -> uw_increment(rules_files_loaded)
				       ;  (ComplainIfNotFound -> uw_format("~nCould not load~n   ~p nor~n   ~p!~n", [FileName1, FileName2]), break ; true),
				          fail)),
	!,
	check_for_duplicate_seeds(Search, EvalFn, CondorSeedNumber, MinAcc),
	uw_set(rules_file_loaded, true),
	uw_set(have_loaded_rules, true), % This is a global that says SOME rules file loaded.
	recorda(jws_theories_loaded, loaded(Fold, CondorSeedNumber, Search, EvalFn, MaxLen, MinAcc, MaxNodes, MinPos), _).
load_rules_file(_, _, _, _, _, _, _, _, _, _, _). % Success even if no file loaded. (Can check rules_file_loaded to see if succeeded.)

check_for_duplicate_seeds(Search, EvalFn, CondorSeedNumber, MinAcc) :-
	learned_rule_in_theory(_/SeedPosEx, [Search, EvalFn, condor(CondorSeedNumber) | _], _),
	count_occurrences_of_this_seed_pos_example(SeedPosEx, Search, EvalFn, CondorSeedNumber, Count),
	(Count > 1 -> uw_format("*** Duplicates (~d copies in condor~d/~p/~p/~p) of pos #~p used as a seed~n", [Count, CondorSeedNumber, Search, EvalFn, MinAcc, SeedPosEx]), uw_set(found_duplicate_seed_examples, true)),
	fail.
check_for_duplicate_seeds(_, _, _, _).
count_occurrences_of_this_seed_pos_example(SeedPosEx, Search, EvalFn, CondorSeedNumber, Count) :-
	findall(SeedPosEx,
	        learned_rule_in_theory(_/SeedPosEx, [Search, EvalFn, condor(CondorSeedNumber) | _], _),
	        Collection),
	length(Collection, Count). 

eval_on_examples(DataSet, ExampleType, CondorSeedNumbersInUse, Search, EvalFn, MaxLen, MinAcc, MaxNodes, MinPos) :-
	% For each example of ExampleType in set DataSet, count how many theories cover it.
	(DataSet = tune 
	  -> jws_get_tune_set_example(ExampleType, Example)
	  ;  jws_get_test_set_example(ExampleType, Example)),
	once(consider_this_example(DataSet, ExampleType, Example, CondorSeedNumbersInUse, Search, EvalFn, MaxLen, MinAcc, MaxNodes, MinPos)),
	uw_increment(examples_considered),
	uw_setting(  examples_considered, Count),
	CountMod100 is (Count mod 100),
	(CountMod100 = 0 -> uw_format(" Have processed ~d ~p examples in ~p.~n",
					   [Count, ExampleType, DataSet])
			 ;  true),
	CountMod1000 is (Count mod 10000),
	(CountMod1000 = 0 -> report_prec_recall_curve ; true),
	fail.
eval_on_examples(_, _, _, _, _, _, _, _, _).

consider_this_example(DataSet, ExampleType, Example, CondorSeedNumbersInUse, Search, EvalFn, MaxLen, MinAcc, MaxNodes, MinPos) :-
	uw_set(current_example_covered, 0),
%	uw_format("Counting matches to ~p~n", [Example]),
	count_theories_covering_example(Example, CondorSeedNumbersInUse, Search, EvalFn, MaxLen, MinAcc, MaxNodes, MinPos, TimesCovered),
%	uw_format("  this ~p ~p example covered ~p times.~n", [DataSet, ExampleType, TimesCovered]),
	(recorded(times_covered, stats(DataSet, ExampleType, TimesCovered, CountInThisBin), DBref)
	  -> erase(DBref), CountInThisBinPlusOne is CountInThisBin + 1
	  ;                CountInThisBinPlusOne is 1),
	recorda(  times_covered, stats(DataSet, ExampleType, TimesCovered, CountInThisBinPlusOne), _).

count_theories_covering_example(Example, CondorSeedNumbersInUse, Search, EvalFn,      MaxLen, MinAcc, MaxNodes, MinPos, _) :-
	uw_member(CondorSeedNumber, CondorSeedNumbersInUse),
	eval_fn_to_use(EvalFn, EvalFnToUse),
	min_acc_to_use(MinAcc, MinAccToUse),
	(once(cached_theory_covers_example(Example, CondorSeedNumber,       Search, EvalFnToUse, MaxLen, MinAccToUse, MaxNodes, MinPos))
		-> uw_increment(current_example_covered)),
	fail.
count_theories_covering_example(      _,       _,                  _,      _,      _,      _,      _,        _, TimesCovered) :-
	uw_setting(current_example_covered, TimesCovered).


% Use only on first K rules in theories (fortunately rules saved in proper order).
collect_examples_matching_this_theory(FirstKrules, FoldsToUse, DatasetsInUse, ExampleTypesInUse, CondorSeedNumbersInUse, 
				      Search, EvalFn, MaxLen, MinAcc, MaxNodes, MinPos) :-
	uw_member(Fold,             FoldsToUse),
	uw_member(CondorSeedNumber, CondorSeedNumbersInUse),	
	eval_fn_to_use(EvalFn,      EvalFnToUse),
	min_acc_to_use(MinAcc,      MinAccToUse),
	uw_member(DataSet,          DatasetsInUse),
	uw_member(ExampleType,      ExampleTypesInUse),
	collect_examples_matching_this_theory(FirstKrules, Fold, DataSet, ExampleType,
					      [CondorSeedNumber, Search, EvalFnToUse, MaxLen, MinAccToUse, MaxNodes, MinPos]),
	% Now remove all the loaded rules, etc.
	(retractall(best_precision_rule)    -> true ; true),
	(retractall(best_recall_rule)       -> true ; true),
	(retractall(learned_rule_in_theory) -> true ; true),
	fail.
collect_examples_matching_this_theory(_, _, _, _, _, _, _, _, _, _, _).

collect_examples_matching_this_theory(FirstKrules, Fold, DataSet, ExampleType, FileSpec) :-
	FileSpec = [    CondorSeedNumber, Search, EvalFn, MaxLen, MinAcc, MaxNodes, MinPos],
	uw_format("File Spec (on ~p): ~p~n", [Fold, FileSpec]),
	uw_set(    rules_file_loaded, false),
	uw_set(found_duplicate_seed_examples, false),
	load_rules_file(FirstKrules, Fold, DataSet, CondorSeedNumber, Search, EvalFn, MaxLen, MinAcc, MaxNodes, MinPos, true),
	uw_setting(found_duplicate_seed_examples, FoundDuplicates),
	(FoundDuplicates -> break, fail ; true),
	uw_setting(rules_file_loaded, true, just_fail_if_not_set), % Make sure file containing theory was loaded.
	create_theory_matches_fileName(FirstKrules, Fold, DataSet, ExampleType, CondorSeedNumber, Search, EvalFn, MaxLen, MinAcc, MaxNodes, MinPos, FileName),
	format("~n", []),
	uw_set(global_file, FileName),
	uw_format("covered(~p, ~p, ~p, ~p,~n  [", [Fold, DataSet, ExampleType, FileSpec]),
	uw_set(count_examples_covered,    0),
	uw_set(count_examples_considered, 0),
	check_all_examples(FirstKrules, Fold, DataSet, ExampleType, FileSpec),
	uw_setting(count_examples_covered,    CountCovered),
	uw_setting(count_examples_considered, CountConsidered),
	count_theory_size(CondorSeedNumber, Search, EvalFn, MaxLen, MinAcc, MaxNodes, MinPos, TheorySize),
	(TheorySize >= FirstKrules
	 -> true
	 ;  uw_format("~2n% Not enough rules! ~d vs. ~d~2n", [TheorySize, FirstKrules]),
	    ((ExampleType = pos, FirstKrules =< 100) -> break ; true),  % No need to break on both pos and neg examples.
	    true),
	uw_format("~n -1]). % ~D of ~D ~p/~p ~p examples covered by theory containing first ~d of ~d rules.~2n",
		  [CountCovered, CountConsidered, Fold, DataSet, ExampleType, FirstKrules, TheorySize]), % Stick in this -1 because of the last comma.
	uw_noset(global_file).

create_theory_matches_fileName(FirstKrules, Fold, DataSet, ExampleType, CondorSeedNumber, Search, EvalFn, MaxLen, MinAcc, MaxNodes, _, FileName) :-
	MinAccFraction is integer(100 * MinAcc),	
	jws_short_search_name(Search, SearchShort),
	jws_short_evalfn_name(EvalFn, EvalFnShort),
	MaxNodesInK is MaxNodes // 1000,
	uw_concat(['theory_matches/NPonly_train_first', FirstKrules, '_', Fold, % Probably should add minPos to these ...
		   '_protein_location_', DataSet, '_', ExampleType, '_',
	           'condor', CondorSeedNumber,
		   '_', SearchShort, '_',  EvalFnShort, '_cl', MaxLen,
		   '_', MaxNodesInK, 'n_', MinAccFraction, 'minacc.yap'],
		  FileName).

check_all_examples(FirstKrules, Fold, DataSet, ExampleType, [CondorSeedNumber, Search, EvalFn, MaxLen, MinAcc, MaxNodes, MinPos]) :-
	(DataSet = tune 
	  -> jws_get_tune_set_example(ExampleType, Example)
	  ;  jws_get_test_set_example(ExampleType, Example)),
%	format("Example = ~p~n", [Example]),
	uw_increment(count_examples_considered),
	(once(theory_covers_example(NumberTried, FirstKrules, Example, CondorSeedNumber, Search, EvalFn, MaxLen, MinAcc, MaxNodes, MinPos))
		-> uw_increment(count_examples_covered), 
		   uw_setting(count_examples_covered, Count),
		   NumberInRow is Count mod 20,
		   (NumberInRow = 0 -> uw_format("~n   ") ; true),
		   % Will need to generalize this at some point ...
		   ExampleSpec =.. [ExampleType, ExampleNumber],
		   (Example = protein_location(_, _, _, ExampleSpec, DataSet, Fold)
			-> true ; uw_format("Didnt match ~p!~n", [Example])),
%		   uw_format("~p [~p of ~p],", [ExampleNumber, NumberTried, FirstKrules]),
		   uw_format("~p,",            [ExampleNumber]),
		   (NumberTried > FirstKrules -> format("~n*** ODDITY = #~p of ~p~n", [NumberTried, FirstKrules]), break),
		   fail).
check_all_examples(_, _, _, _, _).
	

eval_fn_to_use('any', 'laplace').
eval_fn_to_use('any', 'coverage').
eval_fn_to_use('any', 'jws_pr').
eval_fn_to_use('any', 'jws_f1').
eval_fn_to_use(X, X) :- not (X = 'any').

min_acc_to_use('any', 0.75).
min_acc_to_use('any', 0.90).
min_acc_to_use(X, X) :- not (X = 'any').

cached_theory_covers_example(Example, CondorSeedNumber, Search, EvalFn, MaxLen, MinAcc, MaxNodes, MinPos) :-
	uw_setting(precomputing, false, simply_fail_if_not_set),
%	format("Example=~p~n", [Example]),
	Example = protein_location(_, _, _, ExampleSpec, DataSet, Fold), % to do: make general-purpose
	ExampleSpec =.. [ExampleType, ExampleNumber],
%	format("Fold=~p DataSet=~p ExampleType=~p CondorSeedNumber=~p~n", [Fold, DataSet, ExampleType, CondorSeedNumber]),
	% See if we have cached the answers for this ruleset on this dataset.
	covered(Fold, DataSet, ExampleType,
	        [CondorSeedNumber, Search, EvalFn, MaxLen, MinAcc, MaxNodes, MinPos],
		ListOfMatchingExamples),
%	format("Is #~p in: ~p~2n", [ExampleNumber, ListOfMatchingExamples]),
	!, uw_member(ExampleNumber, ListOfMatchingExamples).

theory_covers_example(NumberTried, FirstKrules, Example, CondorSeedNumber, Search, EvalFn, _, MinAcc, MaxNodes, MinPos) :-
	once(uw_set(count_of_rules_tried, 0)), % I think these ONCE's are unnecessary, but use them for safety.
	once(uw_setting(have_loaded_rules, true, just_fail_if_unbound)),
	learned_rule_in_theory(
	 _, % [Pos, Neg, Length, Score]/SeedNumber,
	 [Search, EvalFn, condor(CondorSeedNumber), minAcc(MinAcc), maxNodes(MaxNodes), minPos(MinPos)], 
	 (Example :- Body)), % Skip over the singletons.
%	format("Body=~p~n", [Body]),
	once(uw_increment(count_of_rules_tried)),
	once(uw_setting(count_of_rules_tried, NumberTried)),
	NumberTried =< FirstKrules,
	call(Body).

count_theory_size(CondorSeedNumber, Search, EvalFn, _, MinAcc, MaxNodes, MinPos, _) :-
	once(uw_set(theory_size, 0)),
%	uw_setting(theory_size, Size),
%	format("  Theory size initial = ~d for ~p~n",
%	       [Size, [CondorSeedNumber, Search, EvalFn,MinAcc, MaxNodes, MinPos]]),
	learned_rule_in_theory(
	 _, % [Pos, Neg, Length, Score]/SeedNumber,
	 [Search, EvalFn, condor(CondorSeedNumber), minAcc(MinAcc), maxNodes(MaxNodes), minPos(MinPos)], 
	 (_ :- _)), % Skip over the singletons (ie, require a rule).
	uw_increment(theory_size),
	fail.
count_theory_size(_, _, _, _, _, _, _, Size) :-
%	uw_setting(theory_size, Size), format("  Theory size final = ~d~n", [Size]),
	uw_setting(theory_size, Size).

% Need to do this so YAP doesnt print this vector as a string.
safe_label([P, N, L, Score], [P, N, L, NewScore]) :- integer(Score), NewScore is float(Score - 0.01), !.
safe_label(X, X).


%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% need to stop normal aleph if restarted?
jws_consider_halting(WriteResults) :- % See if a enough nodes have been explored for a random search.
	setting(search, SearchType),
	((uw_isa_randomized_search(SearchType),
	  jws_setting(totalRulesConsidered,    TotalRulesConsidered, ok_if_not_set), 
	  jws_setting(maxTotalRulesToConsider, MaxNodesToConsider,   ok_if_not_set),
	  TotalRulesConsidered >= MaxNodesToConsider)
	   -> uw_format("Halting because total rules considered (~D) exceeds MaxNodesToConsider (~D).~n", [TotalRulesConsidered, MaxNodesToConsider]),
	      (WriteResults = true       -> jws_report_best_ins(final_wrapup) ; true),
%	      (WriteResults = first_time -> jws_report_best_ins(first_time)   ; true),  % TEMP TEMP
	      halt
	   ;  ((%fail, % Turn this off.  Ie, run to normal completion.
		not uw_isa_randomized_search(SearchType),
		(jws_setting(seeds_processed_saved, SeedsSoFarSaved, ok_if_not_set) -> true ; SeedsSoFarSaved = 0),
		(jws_setting(seeds_processed,       SeedsSoFar,      ok_if_not_set) -> true ; SeedsSoFar      = 0),
%		StopHere = 125,
		StopHere = 1000000,
		TotalSeedsProcessed is max(SeedsSoFar, SeedsSoFarSaved),
		TotalSeedsProcessed > StopHere) % THIS IS HARDWIRED IN, SO BE CAREFUL!
		-> uw_format("Halting because total seeds considered (~D) exceeds ~p (hard-wired-in constant).~n",
			     [TotalSeedsProcessed, StopHere]),
	           halt
		; (WriteResults = first_time -> jws_report_best_ins(first_time)   ; true),  % Need to record that a reload occurred, so next start will be different than this one.
		   true)).

