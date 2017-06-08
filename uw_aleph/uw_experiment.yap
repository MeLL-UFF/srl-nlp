%%%%%
%% neural network parameters (all searches)
%%%%%
uw_experiment_using(Method, RandomStarts, LocalMoves, true) :-
  fpd_set(use_neural_net, true),
  fpd_set(neural_net_initial_training, 100),
  fpd_set(prob_bypass_neural_net, 0.1),
  %
  fpd_set(ann_hidden_units, 10),
  fpd_set(ann_learning_rate, 0.1),  % initial!
  fpd_set(ann_prob_remove_iopair, 0.01),
  fpd_set(ann_learning_function, compression),
  fpd_set(ann_record, true),
  fpd_set(ann_eval_if_best_so_far, false), !,
  uw_experiment_using(Method, RandomStarts, LocalMoves),
  !.
uw_experiment_using(Method, RandomStarts, LocalMoves, _) :-
  uw_experiment_using(Method, RandomStarts, LocalMoves),
  !.


%%%%%
%% SCS - Stochastic Clause Selection
%%%%%
uw_experiment_using(scs, Tries, _) :-
 set(search, scs),
 set(scs_sample, Tries).



%%%%%
%% Other Randomised Methods (GSAT, WSAT, RRR, ANNEAL)
%%%%%
uw_experiment_using(gsat, RandomStarts, LocalMoves) :-
 set(search,   rls),
 set(rls_type, gsat),
 set(tries,    RandomStarts),
 set(moves,    LocalMoves), uw_set(expand_clauses_both_directions, true).
uw_experiment_using(wsat, RandomStarts, LocalMoves) :-
 set(search,   rls),
 set(rls_type, wsat),
 set(tries,    RandomStarts),
 set(moves,    LocalMoves), uw_set(expand_clauses_both_directions, true).
uw_experiment_using(rrr, RandomStarts, LocalMoves) :-
 set(search,   rls),
 set(rls_type, rrr),
 set(tries,    RandomStarts),
 set(moves,    LocalMoves), uw_set(expand_clauses_both_directions, true).
uw_experiment_using(anneal, RandomStarts, LocalMoves) :-
 set(search,   rls),
 set(rls_type, anneal),
 set(tries,    RandomStarts),
 set(moves,    LocalMoves), uw_set(expand_clauses_both_directions, true).


uw_isa_randomized_search(wsat).
uw_isa_randomized_search(rrr).
uw_isa_randomized_search(gsat).
uw_isa_randomized_search(scs).
uw_isa_randomized_search(anneal).
uw_isa_randomized_search(rls).

%%%%%
%% ANN
%%%%%
uw_experiment_using(ann, RandomStarts, _) :-
 set(search, ann),
 fpd_set(ann_sample, RandomStarts),
 fpd_set(ann_hidden_units, 10),
 fpd_set(ann_learning_rate, 0.1),  % initial!
 fpd_set(ann_prob_remove_iopair, 0.05),
 fpd_set(ann_learning_function, compression),
 fpd_set(ann_record, true).

% Test NN performance using a tune set?
%
%:- fpd_set(use_tuning_set,true).
%:- fpd_set(tuning_set_examples,100).
%:- fpd_set(evaluate_tuneset_every,10).

% Initial "burn-in" period of NN
%
%:- fpd_set(burnin, true).
%:- fpd_set(burnin_exs, 51).



%%%%%
%% standard aleph searches
%%%%%
uw_experiment_using(bf,        _, _) :-  set(search, bf),        uw_set(expand_clauses_both_directions, false).
uw_experiment_using(df,        _, _) :-  set(search, df),        uw_set(expand_clauses_both_directions, false).
uw_experiment_using(id,        _, _) :-  set(search, id),        uw_set(expand_clauses_both_directions, false).
uw_experiment_using(ibs,       _, _) :-  set(search, ibs),       uw_set(expand_clauses_both_directions, false).
uw_experiment_using(ils,       _, _) :-  set(search, ils),       uw_set(expand_clauses_both_directions, false).
uw_experiment_using(ar,        _, _) :-  set(search, ar),        uw_set(expand_clauses_both_directions, false).
uw_experiment_using(ic,        _, _) :-  set(search, ic),        uw_set(expand_clauses_both_directions, false).
uw_experiment_using(heuristic, _, _) :-  set(search, heuristic), uw_set(expand_clauses_both_directions, false). % vsc: made false here.

uw_experiment_using(X, _, _) :-
	told, format("~2nUnknown search type: ~p~n", [X]), read(_).