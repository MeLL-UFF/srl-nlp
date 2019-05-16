%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Trap unexpected conditions.
uw_confirm(Test, _) :-
	call(Test), !.
uw_confirm(Test, Reason) :-
	uw_format("***** '~q' failed~n*****    Explanation: ~p~n***** Waiting until a Prolog atom entered: ",
		  [Test, Reason]),
	read(_).
uw_confirm(Test) :- uw_confirm(Test, "no reason given").

uw_erased(R) :- db_reference(R), erased(R).


% Every N steps, hold until the user says ready to continue.
uw_check_wait_counter :-
	uw_decrement(wait_counter),
	uw_setting(wait_counter, Count),
	(Count < 0
	   -> nl, write('Specify number of reports to wait until stopping again ... '), read(N), nl,
	      (integer(N) -> uw_set(wait_counter, N) ; uw_set(wait_counter, 0))
	   ;  true).


%%%%%%%%%%%%%%%%%%%%%%%
%% Make findall have what to me (JWS) are the natural semantics (ie, return [] if none, rather than FAIL).
%%

uw_findall(A, B, C) :-
	findall(A, B, C), !.
uw_findall(_, _, []).

uw_all(A, B, C) :-
	all(A, B, C), !.
uw_all(_, _, []).

%%%%%%%%%%%%%%%%%%%%%%%
%% Provide a way to print both to the screen and to a file.
%%

uw_tab(N):-
	once((uw_setting(global_stream, Stream, ok_if_not_set) ; Stream = fail)),
	((Stream == false ; Stream == fail; var(Stream))
	  -> tab(N)
	  ;  tab(Stream, N), tab(N)).

uw_format(String) :- uw_format(String, []).
uw_format(String, Args) :-
	once((uw_setting(global_stream, Stream, ok_if_not_set) ; Stream = fail)),
	((Stream == false ; Stream == fail; var(Stream))
	  -> format(String, Args)
	  ;  uw_format(Stream, String, Args)), !.
uw_format(Stream, String, Args) :- % Print to this stream as well as to the user.
	(uw_setting(dont_print_to_screen_if_printing_to_file, true, ok_if_not_set)
	  ->  true ; format(String, Args)), 
	(once((Stream == false ; var(Stream))) % Dont print if Stream does not look like a stream.
	  -> true
	  ;  format(Stream, String, Args),
	     flush_output(Stream)), !.

uw_pp_dclause(Clause) :- % Print to the SCREEN and to GLOBAL_STREAM (if defined).
	pp_dclause(Clause),
	uw_pp_dclause_to_stream(Clause).
uw_pp_dclause_to_stream(Clause) :- % Pull out so this can be called separately as well.
	once((uw_setting(global_stream, Stream, ok_if_not_set) ; Stream = fail)),
	uw_pp_dclause_to_stream(Stream, Clause).
uw_pp_dclause_to_stream(Stream, Clause) :- % Pull out so this can be called separately as well.
	(once((Stream == false ; Stream == fail; var(Stream)))
	  -> true
	  ;  current_output(OldStream),
	     set_output(Stream), % Now print to the global_stream file.
	     pp_dclause(Clause),
	     flush_output(Stream),
	     set_output(OldStream)), !.

uw_pp_dlist(Clause) :- % Print to the SCREEN and to GLOBAL_STREAM (if defined).
	pp_dlist(Clause),
	once((uw_setting(global_stream, Stream, ok_if_not_set) ; Stream = fail)),
	((Stream == false ; Stream == fail; var(Stream))
	  -> true
	  ;  current_output(OldStream),
	     set_output(Stream), % Now print to the global_stream file.
	     pp_dlist(Clause),
	     flush_output(Stream),
	     set_output(OldStream)), !.


%%%%%%%%%%%%%%%%%%%%%%%
%% Use these to set globals that are in use UW-wide (as opposed to those of one specific local developer).

uw_set(      Variable, Value)           :- uw_indexed_set(      uw, Variable, Value).
uw_setting(  Variable, Value)           :- uw_indexed_setting(  uw, Variable, Value, false). % Complain if not set.
uw_setting(  Variable, Value, OkNotSet) :- uw_indexed_setting(  uw, Variable, Value, OkNotSet).
uw_noset(    Variable)                  :- uw_indexed_noset(    uw, Variable).
 
uw_increment(Variable)                  :- uw_indexed_increment(uw, Variable).
uw_increment(Variable, Delta)           :- uw_indexed_increment(uw, Variable, Delta).

uw_decrement(Variable)                  :- uw_indexed_decrement(uw, Variable).
uw_decrement(Variable, Delta)           :- uw_indexed_decrement(uw, Variable, Delta).

uw_set_all_defaults                     :- uw_indexed_set_all_defaults(uw).

uw_check_setting(_, _). 

uw_default_setting(global_file, false).
uw_default_setting(wait_counter, 5).
uw_default_setting(expand_clauses_both_directions, true).  % Also needs search=heuristic.

% uw_default_setting(dummy, false).   % Only need this if NO defaults used; better to not use a variable ...

uw_set_special_consideration(global_file, File) :-
	uw_noset(global_stream), 
	(File \= false % File=false means NO file.
		-> open(File, write, Stream),
		   uw_set(global_stream, Stream)
		;  true), !.
uw_set_special_consideration(_, _).

uw_rm_special_consideration(global_file, _) :-
	(uw_setting(global_stream, S, ok_if_not_set) -> close(S); true), !.
uw_rm_special_consideration(_, _).

:- multifile uw_indexed_check_setting/3.
:- multifile uw_indexed_default_setting/3.
:- multifile uw_indexed_set_special_consideration/3.
:- multifile uw_indexed_rm_special_consideration/3.

% Need to grab the "call backs" from the uw-generic set/noset functions.
uw_indexed_check_setting(            uw, Variable, Value) :- uw_check_setting(            Variable, Value).
uw_indexed_default_setting(          uw, Variable, Value) :- uw_default_setting(          Variable, Value).
uw_indexed_set_special_consideration(uw, Variable, Value) :- uw_set_special_consideration(Variable, Value).
uw_indexed_rm_special_consideration( uw, Variable, Value) :- uw_rm_special_consideration( Variable, Value).
	


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Some generic utils
%% 

uw_isa_percentage( X) :- nonvar(X), number(X), X >= 0.0, X =< 100.0.
uw_isa_probability(X) :- nonvar(X), number(X), X >= 0.0, X =<   1.0.

uw_assertion(Bool, Msg)        :-uw_assertion(Bool, Msg, Msg).
uw_assertion(Bool, Msg1, Msg2) :-
	(Bool -> true ;  told, format("uw_assertion '~q' failed: ~p ~p~n", [Bool, Msg1, Msg2]), break).

	

uw_report_keysorted_list([]).
uw_report_keysorted_list([Key-Value | Rest]) :-
	uw_format("~3f: ~p~n", [Key, Value]),  % Assume Key is a float (to make a prettier printout).
	uw_report_keysorted_list(Rest).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Some standard Prolog functions
%%

uw_append([], L,  L). % Give (hopefully) unique name in case append is loaded.
uw_append([H|T], L, [H|T1]) :- uw_append(T, L, T1).

uw_reverse([], []).
uw_reverse(X, RevX) :- help_uw_reverse(X, RevX, []).  % uw: I sense that there is a simpler way to do reverse() in Prolog ...  But it is the same as that on http://www.cs.may.ie/~jpower/Courses/PROLOG/list-acc.html
help_uw_reverse([], ResultSoFar, ResultSoFar).
help_uw_reverse([H | Tail], Reversed, ResultSoFar) :-
	help_uw_reverse(Tail, Reversed, [H | ResultSoFar]).

uw_member(Item, [Item | _   ]). % :- !.  Want to allow all possibilities in backtracking.  Create find_first_member if want to only get one answer (or use "once").
uw_member(Item, [_    | Tail]) :- uw_member(Item, Tail).

% Remove ALL occurrences of ITEM in LIST and put in RESULT.
uw_remove(_, [], []).
uw_remove(Item, [Item  | Tail], Result) :-
	uw_remove(Item, Tail, Result).
uw_remove(Item, [Other | Tail], [Other | Result]) :-
	Item \== Other, uw_remove(Item, Tail, Result).

% Combo = Prefix+Postfix (all three are atoms).  Handles case where items are numbers (regular concat doesnt).
uw_concat(Prefix, Postfix, Combo) :-
	var(Prefix), !, % Get in an infinite loop if these are vars, so force to constants.
	format("Have a VARIABLE in call to uw_concat(~p, ~p, RESULT).~n", [Prefix, Postfix]), break,
	uw_concat('wasa_VAR', Postfix, Combo).
uw_concat(Prefix, Postfix, Combo) :-
	var(Postfix), !,
	format("Have a VARIABLE in call to uw_concat(~p, ~p, RESULT).~n", [Prefix, Postfix]), break,
	uw_concat(Prefix, 'wasa_VAR', Combo).
uw_concat(Prefix, Postfix, Combo) :- % Unlike YAP's atom_concat, can use numbers as well as atoms. Ie, this could be called atomic_concat.
	atomic(Prefix), atomic(Postfix),  !,
	(float(Prefix)
	  -> PrefixTimes10 is integer(round(Prefix * 10)), % Due to a yap bug in name(), need to do this extra processing.
	     uw_concat(PrefixTimes10, '/10', Prefix1)
	  ;  Prefix1 = Prefix),
	name(Prefix1, PreString),
	name(Postfix, PostString),
	uw_append(PreString, PostString, ComboString),
	name(Combo, ComboString).
uw_concat([], ''). % Concatenate a LIST of items.
uw_concat([H | T], Result) :- uw_concat(T, Temp), uw_concat(H, Temp, Result).


% I (JWS) am always forgetting the brackets!  So track a few common mistakes.
uw_concat(X, Y, Z, _)          :- uw_error_concat(X, Y, Z).
uw_concat(X, Y, Z, _, _)       :- uw_error_concat(X, Y, Z).
uw_concat(X, Y, Z, _, _, _)    :- uw_error_concat(X, Y, Z).
uw_concat(X, Y, Z, _, _, _, _) :- uw_error_concat(X, Y, Z).
uw_error_concat(X, Y, Z) :-
	told, format("Remember the []'s in uw_concat([~p, ~p, ~p, ...], Result)!~2n", [X, Y, Z]), break.



% Do NOT include the breaker comment (or add a flag that controls this - if so, change last arg to [Breaker] in the base case).
uw_collect_chars_up_to(Breaker, [Breaker | _], []) :- !.
uw_collect_chars_up_to(Breaker, [A | Rest], [A | CollectedRest]) :-
	uw_collect_chars_up_to(Breaker, Rest, CollectedRest).


% Do NOT include the breaker comment (or add a flag that controls this - if so, change last arg to [Breaker] in the base case).
uw_collect_chars_after(_, [], []) :- !.
uw_collect_chars_after(Breaker, [Breaker | Rest], Rest) :- !.
uw_collect_chars_after(Breaker, [_       | Rest], CollectedRest) :-
	uw_collect_chars_after(Breaker, Rest, CollectedRest).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Allows for, in effect, global variables.
%% Taken from aleph.pl and slightly modified.  Also avoids name conflicts that might arise if aleph is updated.
%% 

uw_indexed_set(Initials, Variable, Value) :-
	nonvar(Initials),
	nonvar(Variable),
	nonvar(Value), !,
	once((uw_indexed_check_setting(Initials, Variable, Value) ; uw_assertion(false, 'Failed uw_indexed_check_setting', Initials))),
	(recorded(Initials, set(Variable, _),DbRef) -> erase(DbRef) ; true), % Only save ONE value to this constant.
	% format("Setting (for ~p) ~p to ~p~n", [Initials, Variable, Value]), % Useful if need to debug this set/noset stuff.
	recordz(Initials, set(Variable, Value), _),
	once((uw_indexed_set_special_consideration(Initials, Variable, Value) ; uw_assertion(false, 'Failed uw_indexed_set_special_consideration', Initials))).
uw_indexed_set(Initials, Variable, Value) :- % Catch typo's when setTING was meant (if want to set to vars, do so with a flag?).
	told, !, % Always switch back to the main console.
	format("~n*****(~p) Trying to set ~p to ~p.   Cannot set to a variable. *****~2n",
	       [Initials, Variable, Value]), break.

uw_indexed_setting(Initials, Variable, Value, _) :- % Third arg means "ok if not set; simply fail in this case."
	nonvar(Initials),
	nonvar(Variable),
	recorded(Initials, set(Variable, Value), _), 
	% format("Reading (for ~p) the current setting of ~p which is ~p~n", [Initials, Variable, Value]),
	!.
uw_indexed_setting(Initials, Variable, Value, false) :- % Catch (most likely buggy) attempts to read unset values (use an explicit flag if ok to be unset).
	nonvar(Initials),
	nonvar(Variable),
	% vsc: just send the mess if there is no setting at all!
	\+ recorded(Initials, set(Variable, _), _),
	told, !, % Always switch back to the main console.
	format("~n% *****(~p) There currently is no setting for ~p = ~p *****~2n",
	       [Initials, Variable, Value]), break.
uw_indexed_setting(Initials, Variable, Value, _) :- % Catch typo's.
	nonvar(Initials),
	var(Variable), !,
	told, % Always switch back to the main console.
	format("~n% *****(~p) Trying to place setting of ~p into ~p.~n% ***** Must get non-var field.~2n",
	       [Initials, Variable, Value]), break.

uw_indexed_noset(Initials, Variable) :-
	% format("Unsetting (for ~p) ~p~n", [Initials, Variable]),
	nonvar(Initials),
	nonvar(Variable),
	once((uw_indexed_rm_special_consideration(Initials, Variable, Value) ; uw_assertion(false, 'Failed uw_indexed_rm_special_consideration', Initials))),
        recorded(Initials, set(Variable, Value), DbRef),
	erase(DbRef),
	uw_indexed_set_default(Initials, Variable).
uw_indexed_noset(_, _).

uw_indexed_set_default(Initials, A):-
	nonvar(Initials),
	uw_indexed_default_setting(Initials, A, B),
	% format("Setting (for ~p) ~p to its default value of ~p.~n", [Initials, A, B]), % Leave this uncommented for awhile to see which globals are set at UW.
	uw_indexed_set(Initials, A, B),
	fail.
uw_indexed_set_default(_, _).


uw_indexed_set_all_defaults(Initials) :- uw_indexed_set_default(Initials, _).


uw_indexed_get_all_settings(Initials, Variable, Value) :- % Use this during debugging if ALL settings wanted.
	recorded(Initials, set(Variable, Value), _).



uw_indexed_increment(Initials, Variable) :- uw_indexed_increment(Initials, Variable, 1).
uw_indexed_increment(Initials, Variable, Delta) :-
	atom(Variable),
	once(uw_indexed_setting(Initials, Variable, Value, ok_if_not_set)), % If multiple values, then increment the FIRST one found.
	!,
	NewValue is Value + Delta,
	once(uw_indexed_set(Initials, Variable, NewValue)).  % I don't think a once() is needed here, but play it safe since an odd bug found.
uw_indexed_increment(Initials, Variable, Delta) :- % If variable doesn't exist, then initialize to this value.
	atom(Variable), !, uw_indexed_set(Initials, Variable, Delta).
uw_indexed_increment(Initials, Variable, _) :-  % Maybe should just succeed, but a warning might help debugging.
	format("*****~p Trying to increment %p.  Can only increment atoms! *****~n", [Initials, Variable]).
uw_indexed_decrement(Initials, Variable) :- uw_indexed_decrement(Initials, Variable, 1).
uw_indexed_decrement(Initials, Variable, Delta) :-
	atom(Variable), !,
	once(uw_indexed_setting(Initials, Variable, Value, ok_if_not_set)), % If multiple values, then increment the FIRST one found.
	NewValue is Value - Delta,
	once(uw_indexed_set(Initials, Variable, NewValue)).
uw_indexed_decrement(Initials, Variable, Delta) :- % If variable doesn't exist, then initialize to this value.
	atom(Variable), !, uw_indexed_set(Initials, Variable, Delta).
uw_indexed_decrement(Initials, Variable, _) :-
	format("*****~p Trying to decrement %p.  Can only decrement atoms! *****~n", [Initials, Variable]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Call this like uw_create_modeb_and_determination(action/5, *, keeper(+player)),
uw_create_modeb_and_determination(HeadPred, NumberOfOccurrencesAllowed, BodyPredicate) :-
 modeb(NumberOfOccurrencesAllowed, BodyPredicate),
 BodyPredicate =.. [PredName | Args],
 length(Args, NumberArgs),
 determination(HeadPred, PredName/NumberArgs).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



:- uw_set_all_defaults.

