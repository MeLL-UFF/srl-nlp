

:- multifile vsc_set_hook/1.
:- dynamic vsc_set_hook/1.

%
% ok, everyone else does it, why not me?
%
vsc_set(G) :-
	var(G), !,
	throw(error(instantiation_error)).
vsc_set(G) :-
	( vsc_set_hook(G) -> true ; true),
	functor(G,Na,Ar),
	functor(NG,Na,Ar),
	retractall(NG),
	assert(G).


%
% profiling memory usage
%

vsc_check(Last) :-
	vsc_check_mem(on), !,
	vsc_do_check(Last).
vsc_check(_).

vsc_do_check(Last) :- 
	Last mod 10000 =:= 0,
	vsc_do_tests(Last).
vsc_do_check(_).

vsc_do_tests(Last) :-
	statistics(runtime,T), 
	statistics(heap,[H,_]), 
	format(user_error,'vsc: At node %d, took ~w sec, used up ~w.~n',[Last,H,T]),
	fail.
vsc_do_tests(_) :-
	current_key(_,K),
	key_statistics(K,S1,S2,S3),
	S1 \= 0,
	format(user_error,"~w: ~w clauses, ~w/~w.~n",[K,S1,S2,S3]),
	fail.
%vsc_do_tests(_) :-
%	recorded(0,N,_), write(user_error,N), nl(user_error),
%	fail.
%vsc_do_tests(Last) :-
%	findall(R,recorded(gains,_,R),LR),
%	length(LR,N),
%	write(user_error,vsc:(gains,N)), nl(user_error),
%	fail.


