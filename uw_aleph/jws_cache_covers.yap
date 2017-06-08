
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% N E W   V E R S I O N   O F   S T O R I N G   I N T E R V A L S  F O R  A L E P H  (by JWS)
%

jws_record_intervals_list(ExampleIntervals, Length/Ref, Type) :-
	compact_intervals(ExampleIntervals, CompactedExampleIntervals, Length),
	jws_record_intervals_list2(CompactedExampleIntervals, Length/Ref, Type).

% Store this example-intervals list.  Index by length and type (pos or neg).
jws_record_intervals_list2(ExampleIntervals, Length/Ref, Type) :-
	jws_set_intervals_indices(Type, IndexRef, IndexCounter),
%	length(ExampleIntervals, Length),
	recorded(IndexRef,     Length/ExampleIntervals, Ref), % See if already in cache.
	recorded(IndexCounter, Ref/Counter,             OldCounterRef),
	CounterPlus1 is Counter + 1, % Increment counter (can this be done 'in place'?).
	recorda(IndexCounter,  Ref/CounterPlus1, _),
	%uw_format("Calling jws_record_intervals_list(~p/*, ~p, ~p), counter=~D~n",
	%           [Length, Ref, Type, CounterPlus1]),
	erase(OldCounterRef), !. % Get rid of old counter.
% Handle the case of first-time for this specific example interval.
jws_record_intervals_list2(ExampleIntervals, Length/Ref, Type) :-
	jws_set_intervals_indices(Type, IndexRef, IndexCounter),
%	length(ExampleIntervals, Length),
	recorda(IndexRef,     Length/ExampleIntervals, Ref),
	recorda(IndexCounter, Ref/1,                   _),
	%uw_format("Calling jws_record_intervals_list(~p/*, ~p, ~p), counter=~D~n",
	%           [Length, Ref, Type, 1]),
	!.
% Catch errors.
jws_record_intervals_list2(_, Length/Ref, Type) :-
	told, format("Error: bad call to jws_record_intervals_list(~p/*, ~p, ~p)~n",
	             [Length, Ref, Type]),
	break.

	
% Read this stored example-intervals list. Reduce count after reading.
jws_get_intervals_list(Length/Ref, ExampleIntervals, Type) :-
	%uw_format("Calling jws_get_intervals_list(~p, ~d/*, ~p)", [Ref, Length, Type]),
	var(ExampleIntervals),
	jws_set_intervals_indices(Type, IndexRef, IndexCounter),
	recorded(IndexRef,     Length/CompactedExampleIntervals, Ref),
	uncompact_intervals(CompactedExampleIntervals,ExampleIntervals),
	recorded(IndexCounter, Ref/Counter,             OldCounterRef),
	%uw_format(", counter=~D~n", [Counter]),
	CounterMinus1 is Counter - 1,
	erase(OldCounterRef),
	(CounterMinus1 > 0
	    -> recorda(IndexCounter, Ref/CounterMinus1, _)
	    ;  erase(Ref)), % No need to store these ExampleIntervals any longer.
	!.
jws_get_intervals_list(Length/Ref, _, Type) :-
	told, uw_format("Error: bad call to jws_get_intervals_list(~p, ~p/*, ~p)~n", [Ref, Length, Type]), break.

% Like jws_get_intervals_list, but
% do NOT reduce count after reading.  Also indicate how many occurrences of these there are.
jws_peek_intervals_list(Length/Ref, ExampleIntervals, Type, Counter) :-
	var(ExampleIntervals),
	jws_set_intervals_indices(Type, IndexRef, IndexCounter),
	recorded(IndexRef,     Length/CompactedExampleIntervals, Ref), 
	uncompact_intervals(CompactedExampleIntervals,ExampleIntervals),
	recorded(IndexCounter, Ref/Counter,             _), !.
jws_peek_intervals_list(Length/Ref, _,  Type, Counter) :-
	told, uw_format("Error: bad call to jws_peek_intervals_list(~p, ~p/*, ~p, ~p)~n",
	                [Ref, Length, Type, Counter]),
	break.

jws_retract_all_intervals_counters :-
	retract_all(jws_intervals_pos),
	retract_all(jws_intervals_neg),
	retract_all(jws_counters_pos),
	retract_all(jws_counters_neg).


% Args should be +Type, -IndexRef, -IndexCounter
jws_set_intervals_indices(pos, jws_intervals_pos, jws_counters_pos) :- !.
jws_set_intervals_indices(neg, jws_intervals_neg, jws_counters_neg) :- !.
jws_set_intervals_indices(Type, IndexRef, IndexCounter) :-
	told, uw_format("Error: bad call to jws_set_intervals_indices(~p, ~p, ~p)~n",
	                [Type, IndexRef, IndexCounter]),
	break.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Report on the current size of the example intervals.
jws_report_openlist_properties :- 
	jws_set(open_size_counter_total,       0),
	jws_set(open_size_counter_savings_pos, 0),
	jws_set(open_size_counter_savings_neg, 0),
	jws_set(open_size_counter_tiny_p,   0),
	jws_set(open_size_counter_small_p,  0),
	jws_set(open_size_counter_medium_p, 0),
	jws_set(open_size_counter_large_p,  0),
	jws_set(open_size_counter_huge_p,   0),
	jws_set(open_size_counter_sum_p,    0),
	jws_set(open_size_counter_tiny_n,   0),
	jws_set(open_size_counter_small_n,  0),
	jws_set(open_size_counter_medium_n, 0),
	jws_set(open_size_counter_large_n,  0),
	jws_set(open_size_counter_huge_n,   0),
	jws_set(open_size_counter_total_n,  0),
	jws_set(open_size_counter_sum_n,    0),
	recorded(openlist, Gains, _),
	get_node(Gains, _, _, NodeRef), % Walk through all the nodes.
	once((instance(NodeRef, node(_, _, _, _, PosCoverRef, NegCoverRef, _, _)),
	      jws_peek_intervals_list(PosCoverRef, PosCover, pos, OccurrencesPos),
	      jws_peek_intervals_list(NegCoverRef, NegCover, neg, OccurrencesNeg),
	      ((PosCover = [], NegCover = []) -> true ; jws_increment(open_size_counter_total)),
	      (PosCover = [] -> true
	         ; length(PosCover, PosLength),
		   PosLengthProRated is PosLength / OccurrencesPos,
		   SavingsPos is ((OccurrencesPos - 1) / OccurrencesPos) * PosLength,
	           jws_increment(open_size_counter_sum_p, PosLengthProRated),
		   jws_increment(open_size_counter_savings_pos, SavingsPos),
	           ((PosLength >=  50, PosLength =<  100)
	 	      -> jws_increment(open_size_counter_tiny_p)   ; true),
	           ((PosLength >= 101, PosLength =<  250)
	 	      -> jws_increment(open_size_counter_small_p)  ; true),
	           ((PosLength >  250, PosLength =<  500)
		      -> jws_increment(open_size_counter_medium_p) ; true),
	           ((PosLength >  500, PosLength =< 1000)
		      -> jws_increment(open_size_counter_large_p)  ; true),
	           ( PosLength > 1000
		      -> jws_increment(open_size_counter_huge_p)   ; true)),
	      (NegCover = [] -> true
	         ; length(NegCover, NegLength),
		   NegLengthProRated is NegLength / OccurrencesNeg,
		   SavingsNeg is ((OccurrencesNeg - 1) / OccurrencesNeg) * NegLength,
	           jws_increment(open_size_counter_sum_n, NegLengthProRated),
		   jws_increment(open_size_counter_savings_neg, SavingsNeg),
	           ((NegLength >=  50, NegLength =<  100)
	 	      -> jws_increment(open_size_counter_tiny_n)   ; true),
	           ((NegLength >= 101, NegLength =<  250)
	 	      -> jws_increment(open_size_counter_small_n)  ; true),
	           ((NegLength >  250, NegLength =<  500)
		      -> jws_increment(open_size_counter_medium_n) ; true),
	           ((NegLength >  500, NegLength =< 1000)
		      -> jws_increment(open_size_counter_large_n)  ; true),
	           ( NegLength > 1000
		      -> jws_increment(open_size_counter_huge_n)   ; true)))),
	fail.
jws_report_openlist_properties :-
	jws_setting(open_size_counter_total, Total, ok_if_not_set),
	Total > 0, % Only report if Total>0.
	jws_setting(open_size_counter_savings_pos, SavingsPos),
	jws_setting(open_size_counter_savings_neg, SavingsNeg),
	jws_setting(open_size_counter_tiny_p,   Tiny_p),
	jws_setting(open_size_counter_small_p,  Small_p),
	jws_setting(open_size_counter_medium_p, Medium_p),
	jws_setting(open_size_counter_large_p,  Large_p),
	jws_setting(open_size_counter_huge_p,   Huge_p),
	jws_setting(open_size_counter_sum_p,    Sum_p),
	jws_setting(open_size_counter_tiny_n,   Tiny_n),
	jws_setting(open_size_counter_small_n,  Small_n),
	jws_setting(open_size_counter_medium_n, Medium_n),
	jws_setting(open_size_counter_large_n,  Large_n),
	jws_setting(open_size_counter_huge_n,   Huge_n),
	jws_setting(open_size_counter_sum_n,    Sum_n),
	TinyP_p   is (100 * Tiny_p)   / Total,
	SmallP_p  is (100 * Small_p)  / Total,
	MediumP_p is (100 * Medium_p) / Total,
	LargeP_p  is (100 * Large_p)  / Total,
	HugeP_p   is (100 * Huge_p)   / Total,
	Ave_p     is Sum_p            / Total,
	TinyP_n   is (100 * Tiny_n)   / Total,
	SmallP_n  is (100 * Small_n)  / Total,
	MediumP_n is (100 * Medium_n) / Total,
	LargeP_n  is (100 * Large_n)  / Total,
	HugeP_n   is (100 * Huge_n)   / Total,
	Ave_n     is Sum_n            / Total,
	SavingsPosPercentage is (100 * SavingsPos) / (Sum_p + SavingsPos),
	SavingsNegPercentage is (100 * SavingsNeg) / (Sum_n + SavingsNeg),
        uw_format("    OPEN sizes pos:~n"),
        uw_format("                    [ 50- 100]=~D/~1f% [101-250]=~D/~1f% [251-500]=~D/~1f%~n",
                  [Tiny_p, TinyP_p, Small_p, SmallP_p, Medium_p, MediumP_p]),
        uw_format("                    [501-1000]=~D/~1f% [1000+]=~D/~1f%~n                   ",
                  [Large_p, LargeP_p, Huge_p, HugeP_p]),
        uw_format(" (Sum=~1f)/(Total=~D) = ~1f ave;  Saved = ~1f (~1f%)~n",
                  [Sum_p, Total, Ave_p, SavingsPos, SavingsPosPercentage]),
        uw_format("    OPEN sizes neg:~n"),
        uw_format("                    [ 50- 100]=~D/~1f% [101-250]=~D/~1f% [251-500]=~D/~1f%~n",
                  [Tiny_n, TinyP_n, Small_n, SmallP_n, Medium_n, MediumP_n]),
        uw_format("                    [501-1000]=~D/~1f% [1000+]=~D/~1f%~n                   ",
                  [Large_n, LargeP_n, Huge_n, HugeP_n]),
        uw_format(" (Sum=~1f)/(Total=~D) = ~1f ave;  Saved = ~1f (~1f%)~n",
                  [Sum_n, Total, Ave_n, SavingsNeg, SavingsNegPercentage]),
	!.
jws_report_openlist_properties. % Catch any failures above.


% vsc: code to compact list
compact_intervals(L0, T, Len) :- 
	compact_intervals(L0,L1,0,Len),
	T =.. [cvr|L1].

compact_intervals([], [], Len, Len).
compact_intervals([A-A|L], [A|NL], Len0, LenF) :- !,
	Leni is Len0+A,
	compact_intervals(L, NL, Leni, LenF).
compact_intervals([A-B|L], [NA,B|NL],Len0,LenF) :-
	NA is 0-A,
	LenI is A+B+Len0,
	compact_intervals(L, NL, LenI, LenF).

uncompact_intervals(T, L) :- 
	T =.. [_|L0],
	uncompact_intervals2(L0,L).

uncompact_intervals2([], []).
uncompact_intervals2([A,B|L], [NA-B|NL]) :- A < 0, !,
	NA is 0-A,
	uncompact_intervals2(L,NL).
uncompact_intervals2([A|L], [A-A|NL]) :-
	uncompact_intervals2(L,NL).

