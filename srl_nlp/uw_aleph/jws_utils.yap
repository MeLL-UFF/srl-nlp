%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Some generic utils.
%% 

jws_is_in_between(N, Lower, Upper) :- N >= Lower, N < Upper.

jws_sandwich(Lo, X, Hi, Result)  :- Temp is min(X, Hi), Result is max(Lo, Temp).

jws_file_must_exist(FileName) :-
	exists(FileName), !.
jws_file_must_exist(FileName) :-
	told, !,
	format("~n***** Cannot find the file:~n*****     ~p~2n", FileName),
	break.

jws_report_one_item_per_line([]).
jws_report_one_item_per_line([FirstItem | RestItems]) :-
	format_jws("  ~p~n", [FirstItem]),
	jws_report_one_item_per_line(RestItems).

jws_capitalize_char(Char, Cap) :- % See if [a-z].
	number(Char), Char >= 97, Char =< 122, Cap is Char - 32, !. % Note: there are some other chars between [A-Z] and [a-z]
jws_capitalize_char(Char, Char).



jws_nth0(N, List, Item) :- jws_nth0_helper(N, 0, List, Item).
jws_nth0_helper(N, N, [First | _],    First) :- !.
jws_nth0_helper(N, M, [_     | Rest], Item)  :- M < N, Mplus1 is M + 1, jws_nth0_helper(N, Mplus1, Rest, Item).


jws_nth(N, List, Item) :- M is N - 1, jws_nth0(M, List, Item).

% Need to do "['../aleph'], cd('condor/jobs'), jws_mkdir_condor(25)." before doing this.
jws_mkdir_condor(N) :- N < 0, !.
jws_mkdir_condor(N) :- % At times, this will only do 28 recursive calls??????  Maybe due to the # of streams that can be opened (even if later closed?)
	N >= 0,
	uw_concat(['mkdir run', N], X),
	system(X),
	uw_concat(['run', N], Y),
	cd(Y),
	getcwd(Dir), format("~nDir = ~p~n", [Dir]),
	uw_concat([Dir, '/do.yap'], File), format("File = ~p~n", [File]),
	uw_set(global_file, File),
	open(File, write, Stream),
	format(Stream, "cd('../..'), consult(do_condor), do_condor(~p).~n", [N]),
	close(Stream),
	cd('..'),
	N1 is N - 1,
	jws_mkdir_condor(N1).

% Doesnt work - would need to transfer ALL the files to be loaded ...
jws_mkdir_condor_flock(N) :- N < 0, !.
jws_mkdir_condor_flock(N) :- % At times, this will only do 28 recursive calls??????  Maybe due to the # of streams that can be opened (even if later closed?)
	N >= 0,
	uw_concat(['run', N], Y),
	cd(Y),
	getcwd(Dir), format("~nDir = ~p~n", [Dir]),
	uw_concat([Dir, '/flock.yap'], File), format("File = ~p~n", [File]),
	uw_set(global_file, File),
	open(File, write, Stream),
	format(Stream, "consult(do_condor), do_condor(~p).~n", [N]),
	close(Stream),
	cd('..'),
	N1 is N - 1,
	jws_mkdir_condor_flock(N1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Add the type of vars to their names, eg SentenceA, PersonB, etc.

jws_add_typenames_to_VSC_vars((Head :- Body), (TypedHead :- TypedBody)) :-	
	jws_add_typenames_to_head_VSC_vars(Head, TypedHead),
	jws_add_typenames_to_body_VSC_vars(Body, TypedBody).

jws_add_typenames_to_head_VSC_vars(Head, TypedHead) :-
	jws_add_typenames_to_VSC_literal(Head, TypedHead, modeh).

jws_add_typenames_to_body_VSC_vars((Lit, Lits), (TypedLit, TypedLits)) :-
	!,
	jws_add_typenames_to_VSC_literal(Lit, TypedLit, modeb),
	jws_add_typenames_to_body_VSC_vars(Lits, TypedLits).
jws_add_typenames_to_body_VSC_vars((Lit), (TypedLit)) :-
	jws_add_typenames_to_VSC_literal(Lit,  TypedLit, modeb).

jws_add_typenames_to_VSC_literal('!', '!', _) :- !.   % Not really needed, though saves a little time.
jws_add_typenames_to_VSC_literal(Lit, TypedLit, Mode) :-
	Lit    =.. [Pred | Args],
	functor(Lit, Pred, NumbArgs),
	jws_create_mode_grabbing_lit(Pred, NumbArgs, LitNew),
	(Mode = modeh
	  -> recorded(aleph, modeh(_, LitNew), _) % This will bind all the args to their types.
	  ;  recorded(aleph, modeb(_, LitNew), _)),
	LitNew    =.. [Pred | ArgsNew],
	jws_combineArgLists(Args, ArgsNew, CombinedArgs),
	TypedLit =.. [Pred | CombinedArgs], !.
jws_add_typenames_to_VSC_literal(Lit, Lit, _). % No change if no mode recorded.

% Sample: jws_create_mode_grabbing_lit(Pred, 7, LitNew) should return LitNew =.. [Pred, _, _, _, _, _, _, _].
jws_create_mode_grabbing_lit(Pred, Numb, LitNew) :-
	jws_create_mode_grabbing_lit_helper(Numb, NumbArgs),
	uw_append([Pred], NumbArgs, NumbArgsPlusPred),
	LitNew =.. NumbArgsPlusPred.
jws_create_mode_grabbing_lit_helper(0, []).
jws_create_mode_grabbing_lit_helper(Numb, NumbArgs) :-
	number(Numb), Numb > 0, NumbMinus1 is Numb - 1,
	jws_create_mode_grabbing_lit_helper(NumbMinus1, NumbArgsMinus1),
	uw_append([_], NumbArgsMinus1, NumbArgs).

jws_combineArgLists([], [], []).
jws_combineArgLists(['$VAR'(-1) | RestArgVSC], [_ | RestArgType], ['$VAR'(-1) | RestArgOut]) :-
	!, % Maybe should make something like _AnyWord ?
	jws_combineArgLists(RestArgVSC, RestArgType, RestArgOut).
jws_combineArgLists(['$VAR'(VarNumb) | RestArgVSC], [FirstArgType | RestArgType], [FirstArgOut | RestArgOut]) :-
	!,
	jws_drop_mode_indicator(FirstArgType_fixed, FirstArgType), !, % Drop leading +/- then capitalize first letter.
	VarNumbPlus65 is VarNumb + 65, % Assume no more than 26 vars ...
	name(FirstArgType_fixed, ArgString),
	ArgString = [FirstLetter | RestLetters],
	jws_capitalize_char(FirstLetter, Cap),
	ArgStringCapped = [Cap | RestLetters],
	uw_append(ArgStringCapped, [VarNumbPlus65], FirstArgOutString),
	name(FirstArgOut, FirstArgOutString),
	jws_combineArgLists(RestArgVSC, RestArgType, RestArgOut).
jws_combineArgLists([FirstArgOut | RestArgVSC], [_ | RestArgType], [FirstArgOut | RestArgOut]) :-
	jws_combineArgLists(RestArgVSC, RestArgType, RestArgOut).

jws_drop_mode_indicator(I, +I).
jws_drop_mode_indicator(I, -I).
%jws_drop_mode_indicator(I, #I). % No need to check this since wont get a var if a constant is needed.

/*

%  ['..\\aleph'], jws_test.

jws_test :-
	modeh(1, protein_location(+phrase, +phrase, +sentence)),
	modeb(*, sentence_child(+sentence, -phrase)),
	modeb(*, phrase_contains_specific_word(+phrase, -word, +string)), % Cant use #string here for some reason ...
	(HeadVSC :- BodyVSC) = (protein_location(A, B, C) :- (sentence_child(C, A), phrase_contains_specific_word(B, D, house), sentence_child(C, B), phrase_contains_specific_word(B, D, house))),
	vsc_numbervars_nosingletons((HeadVSC :- BodyVSC), 0, _),
	jws_add_typenames_to_VSC_vars((HeadVSC :- BodyVSC), (X :- Y)),
	format("X = ~q~nY = ~q~n", [X, Y]).
*/



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Convert A, B, C to WordA, PhraseB, SentenceC, etc 
%% (bug: will lose [single] quotes on capitalized constants, so if those are needed then turn this off).
%%

% Unquoting the new TypedVars can mess up other quoted constants.  So might not to use this.

jws_pp_dclause_with_typed_vars((H :- true)):-
	!, jws_pp_dclause_with_typed_vars(H).

jws_pp_dclause_with_typed_vars((H :- B)):-
	!,
	copy_term((H :- B),(Head1 :- Body1)),
	vsc_numbervars_nosingletons((Head1 :- Body1),0,_),
	jws_add_typenames_to_VSC_vars((Head1 :- Body1), (Head :- Body)),
	write(Head), % Do NOT use writeq here (but what about things that SHOULD be quoted? bug)
	write(' :-'), nl,
	setting(print, N),
	jws_print_lits_with_typed_vars(Body, 1, N).

jws_pp_dclause_with_typed_vars((Lit)):-
	copy_term(Lit, Lit1),
	vsc_numbervars_nosingletons(Lit1, 0, _),
	jws_add_typenames_to_head_VSC_vars(Lit1, TypedLit1),
	write(TypedLit1),
	write('.'), nl.

jws_print_lits_with_typed_vars((Lit, Lits), LitNum, LastLit):-
	!,
	jws_print_lit_with_typed_vars(Lit, LitNum, LastLit,', ', NextLit),
	jws_print_lits_with_typed_vars(Lits, NextLit, LastLit).
jws_print_lits_with_typed_vars((Lit), LitNum, _):-
	jws_print_lit_with_typed_vars(Lit, LitNum, LitNum, '.', _).

jws_print_lit_with_typed_vars(Lit,LitNum,LastLit,Sep,NextLit):-
	(LitNum = 1 -> tab(3); true),
	write(Lit), write(Sep),
	(LitNum = LastLit-> nl, NextLit = 1; NextLit is LitNum + 1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Allows for, in effect, global variables.
%% Taken from aleph.pl and slightly modified. 
%% Also avoids name conflicts that might arise if aleph is updated.
%% 

jws_set(    Variable, Value)          :- uw_indexed_set(             jws, Variable, Value).
jws_set_all_defaults                  :- uw_indexed_set_all_defaults(jws).

jws_setting(Variable, Value)          :- uw_indexed_setting(         jws, Variable, Value, false).
jws_setting(Variable, Value, OkNotSet):- uw_indexed_setting(         jws, Variable, Value, OkNotSet).

jws_noset(  Variable)                 :- uw_indexed_noset(           jws, Variable).

jws_get_all_settings(Variable, Value) :- uw_indexed_get_all_settings(jws, Variable, Value).

jws_increment(Variable)               :- uw_indexed_increment(       jws, Variable).
jws_increment(Variable, Delta)        :- uw_indexed_increment(       jws, Variable, Delta).

jws_decrement(Variable)               :- uw_indexed_decrement(       jws, Variable).
jws_decrement(Variable, Delta)        :- uw_indexed_decrement(       jws, Variable, Delta).

% Need to grab the "call backs" from the uw-generic set/noset functions.
uw_indexed_check_setting(            jws, Variable, Value) :- jws_check_setting(            Variable, Value).
uw_indexed_default_setting(          jws, Variable, Value) :- jws_default_setting(          Variable, Value).
uw_indexed_set_special_consideration(jws, Variable, Value) :- jws_set_special_consideration(Variable, Value).
uw_indexed_rm_special_consideration( jws, Variable, Value) :- jws_rm_special_consideration( Variable, Value).

% Need these "terminators" but they are in jws_aleph_extension.yap instead
% jws_check_setting(_, _).
% jws_set_special_consideration(_, _). 
% jws_rm_special_consideration(_, _).

%
%  jws_read_then_unset(+Variable, ?Value) - put this in the "uw" stuff if others ever need it
jws_read_then_unset(Variable, Value) :- jws_read_then_unset(Variable, Value, false).
jws_read_then_unset(Variable, Value, OkIfNotSet) :-
	jws_setting(Variable, Value, OkIfNotSet), !,
	jws_noset(Variable).
jws_read_then_unset(Variable, _, _) :- % Even if this Variable does not equal Value, erase it.
	jws_noset(Variable).

%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Profiling
%%  (taken from http://www.ncc.up.pt/~vsc/Yap/yap.html)

% Need to do:
%
% yap_flag(profiling,on).
% profile_reset.
% jws_list_profile.

jws_list_profile :-
        % get number of calls for each profiled procedure
        setof(D-[M:P|D1],(current_module(M),profile_data(M:P,calls,D),profile_data(M:P,retries,D1)),LP),
        % output so that the most often called
        % predicates will come last:
        jws_write_profile_data(LP).

jws_list_profile(Module) :-
        % get number of calls for each profiled procedure
        setof(D-[Module:P|D1],(profile_data(Module:P,calls,D),profile_data(Module:P,retries,D1)),LP),
        % output so that the most often called
        % predicates will come last:
        jws_write_profile_data(LP).

jws_write_profile_data([]).
jws_write_profile_data([D-[M:P|R]|SLP]) :-
        % swap the two calls if you want the most often
        %  called predicates first.
	write_profile_data(SLP), % Get MOST called preds first.
        format('~a:~w: ~32+~t~d~12+~t~d~12+~n', [M,P,D,R]),
        % write_profile_data(SLP), % Get LEAST first.
	true.



%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Fix bug in rewrite_clause - instead of (P, Q, R, S) a body might be (P, (Q, R), S).
%%

% Args are: UnflattedClauseBody, CurrentStackOfFlattenedClauses, NewStackOfFlattenedClauses
help_flatten_clause_body( ((LitA, LitsA), Lits), OldStack, NewStack) :-
	!, % format("1: LitA=~p LitsA=~p Lits=~p~n", [LitA, LitsA, Lits]),
	help_flatten_clause_body(LitA,  OldStack,  NewStack1), % Stack the first "embedded" clause.
	help_flatten_clause_body(LitsA, NewStack1, NewStack2), % Stack the rest of the embedded clause
	help_flatten_clause_body(Lits,  NewStack2, NewStack).  % Stack the rest of the clause.
help_flatten_clause_body( (Lit, Lits), OldStack, NewStack) :-
	!, % format("2: Lit=~p Lits=~p~n", [Lit, Lits]), % First arg is not "embedded" so simply stack it.
	addin_lit_to_flattened(Lit, OldStack, NewStack1),
	help_flatten_clause_body(Lits, NewStack1, NewStack).
help_flatten_clause_body( ((LitA, LitsA)), OldStack, NewStack) :-
	!, % format("3: LitA=~p LitsA=~p~n", [LitA, LitsA]), % Handle case where LAST item is "embedded."
	help_flatten_clause_body(LitA,  OldStack,  NewStack1),
	help_flatten_clause_body(LitsA, NewStack1, NewStack).
help_flatten_clause_body(Lit, OldStack, NewStack) :- % Handle case where LAST item is normal.
	addin_lit_to_flattened(Lit, OldStack, NewStack).

addin_lit_to_flattened(Lit, true,    Lit) :- !. % This is the first lit being added.
addin_lit_to_flattened(Lit, OldStack, (Lit, OldStack)).

% Args are: OriginalListLeftToReverse, FinalResult, PartiallyReversedList
help_reverse_clause_body((Lit, Lits), Reversed, ResultSoFar) :- % Pop from original to temp stack.
	!, help_reverse_clause_body(Lits, Reversed, (Lit, ResultSoFar)).
help_reverse_clause_body(Lit, (Lit, ResultSoFar), ResultSoFar). % Done when at last Literal in original.

reverse_clause_body((Lit, Lits), Reversed) :- !, help_reverse_clause_body(Lits, Reversed, Lit).
reverse_clause_body(Lit, Lit).

flatten_clause_body(Old, New) :-
	% format("~nPatch ~p~2n", [Old]),
	help_flatten_clause_body(Old, true, Flattened),
	reverse_clause_body(Flattened, New).

%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Aids for debugging.
here0 :- write(here0), nl.
here1 :- write(here1), nl.
here2 :- write(here2), nl.
here3 :- write(here3), nl.
here4 :- write(here4), nl.
here5 :- write(here5), nl.
here6 :- write(here6), nl.
here7 :- write(here7), nl.
here8 :- write(here8), nl.
here9 :- write(here9), nl.