%%%%%%%%%%
%%
%%  ARTIFICIAL NEURAL NETWORK CLAUSE SELECTION
%%
%%%%%%%%%%

%%
%% Interface from aleph to ANN
%%
train_ANN_on_all_IO_pairs :-
	set_value(errorSum, 0.0),
	get_value(numWeightsHiddenLayer, NumWeightsHiddenLayer),
	get_value(numWeightsOutputLayer, NumWeightsOutputLayer),
	static_array(del_w_hidden,NumWeightsHiddenLayer,float), 
	static_array(del_w_output,NumWeightsOutputLayer,float), 
	findall(L,(recorded(ann,iopair(L,_),_)),LList),
	length(LList,Terms),
	set_value(terms,Terms),
	jws_setting(report_details_level, JWS_LEVEL),
	(JWS_LEVEL > 1 ->
		uw_format("[~p I/O Pairs recorded]~n", [Terms]);
		true),
	recorded(ann,iopair(Lits,Acc),_),
	%
	train_example_list(del_w_hidden, del_w_output, Lits,Acc),
	false.
train_ANN_on_all_IO_pairs :- 
	get_value(errorSum, OldErrorSum),
	set_value(errorSum, 0.0),
	get_value(terms,Terms),
	commit_changes(del_w_hidden, del_w_output, w_hidden, w_output, Terms),
	run_ANN_on_all_IO_pairs,
	get_value(errorSum, NewErrorSum),
	jws_setting(report_details_level, JWS_LEVEL),
	(JWS_LEVEL > 1 ->
		format("[NN error: ~p -> ~p]~n",[OldErrorSum, NewErrorSum]);
		true),
	%  adaptive learning rate ... 
	nu(Nu),
	(NewErrorSum < OldErrorSum ->
		NewNu is min(Nu*1.111, 1.0) ;  % cap learning rate
		NewNu is Nu*0.9),
	nu(NewNu),      %  :-)
	trim_list,
	close_static_array(del_w_hidden),
	close_static_array(del_w_output),
	!.

trim_list :-
	recorded(ann,iopair(_,_),DbRef),
	fpd_setting(ann_prob_remove_iopair, P, ok),
	Rand is random,
	(Rand < P ->
		erase(DbRef);
		true),
	false.
trim_list :- !.


run_ANN_on_all_IO_pairs :-
	recorded(ann,iopair(Lits,Acc),_),
	run_example_list(Lits,Out),
	AbsError is abs(Acc - Out),
	get_value(errorSum, Sum),
	NewSum is Sum+AbsError,
	set_value(errorSum, NewSum),
	false.
run_ANN_on_all_IO_pairs :-
	!.


%%
%% commit_changes(...)
%%
commit_changes(DelWHidden, DelWOutput, WHidden, WOutput,Terms) :-
	get_value(numWeightsHiddenLayer, NumWeightsHiddenLayer),
	get_value(numWeightsOutputLayer, NumWeightsOutputLayer),
	% ... HIDDEN LAYER
	set_value(i,0),	repeat,	get_value(i,I),
	%
	array_element(WHidden, I, W_H),
	array_element(DelWHidden, I, Del_W_H),
	New_W_H is W_H+(Del_W_H/Terms),
	update_array(WHidden, I, New_W_H),
	%
	I1 is I+1,	set_value(i,I1),
	I1 >= NumWeightsHiddenLayer, !,
	%
	% ... OUTPUT LAYER
	set_value(j,0),	repeat,	get_value(j,J),
	%
	array_element(WOutput, J, W_O),
	array_element(DelWOutput, J, Del_W_O),
	New_W_O is W_O+(Del_W_O/Terms),
	update_array(WOutput, J, New_W_O),
	%
	J1 is J+1,	set_value(j,J1),
	J1 >= NumWeightsOutputLayer, !,
	eraseall(i),
	eraseall(j).



%%
%%  Convert lists to static arrays for NN training & testing
%%      i.e. convert [1 4 5] to [1 0 0 1 1 0 0 ...]
%%
run_example_list(Lits, Output) :-
	lits_to_static_array(Lits,lits_arry),
	run_example(lits_arry, Output),
	close_static_array(lits_arry),!.

train_example_list(WeightsH, WeightsO, Lits, Output) :-
	lits_to_static_array(Lits,lits_arry),
	train_example(WeightsH, WeightsO, lits_arry, Output),
	close_static_array(lits_arry),!.

lits_to_static_array(List, ArrayName) :- 
	get_value(inUnits, InUnits),
	(close_static_array(ArrayName) ; true),
	static_array(ArrayName, InUnits, float),
	set_elts_in_array(List,ArrayName), !.
	%format("[ ~p -> (", [List]),
	%print_array(ArrayName),
	%write(')]'), nl.

set_elts_in_array([], _) :- !.
set_elts_in_array([H|T], ArrayName) :-
	H1 is H-1,
	update_array(ArrayName, H1, 1.0),
	set_elts_in_array(T, ArrayName).

%%
%% copy an array ... assume type float
%%
array_copy(In,Out) :-
	array_length(In,Len),
	(close_static_array(Out) ; true),
	static_array(Out, Len, float),
	Len > 0,   % make sure >= 1 elt in array
	set_value(j,0),
	repeat,
	get_value(j,J),
	array_element(In,J,Val),
	update_array(Out,J,Val),
	J1 is J+1,
	set_value(j,J1),
	J1 >= Len.

array_length(Array,J1) :-
	set_value(j,0),
	repeat,
	get_value(j,J),
	J1 is J+1,
	set_value(j,J1),
	not(array_element(Array, J1, _)).

%%
%%  print_array(+Array)
%%
print_array(Arry) :-
	set_value(j,0),
	format("~p = [", [Arry]),
	array_element(Arry, 0, _),   % make sure >= 1 elt in array
	repeat,
	get_value(j,J),
	array_element(Arry, J, Out),
	format(" ~p ", [Out]),
	J1 is J+1,
	set_value(j,J1),
	not(array_element(Arry, J1, _)),  % repeat until no elts. remain
	format("]~n", []),
	!.


%%
%%  print_weights
%%
print_weights :-
	%get_value(inUnits, InUnits),
	get_value(hiddenUnits, HiddenUnits),
	get_value(numWeightsHiddenLayer, NumWeightsHiddenLayer),
	get_value(numWeightsOutputLayer, NumWeightsOutputLayer),
	% ... HIDDEN LAYER
	set_value(i,0),
	format("Input -> Hidden Layer~n---------------------~n",[]),
	repeat,
	get_value(i,I),
	FromInputUnit is I // (HiddenUnits),
	ToHiddenUnit is I mod (HiddenUnits),
	(ToHiddenUnit = 0 -> format("~n~nFrom Input ~d:~n",[FromInputUnit]) ; true),
	array_element(w_hidden, I, WeightHidden),
	format("   ~f", [WeightHidden]),
	I1 is I+1,
	set_value(i,I1),
	I1 >= NumWeightsHiddenLayer, !,
	%
	% ... OUTPUT LAYER
	set_value(j,0),
	format("~n~nHidden -> Output Layer~n---------------------~n",[]),
	repeat,
	get_value(j,J),
	array_element(w_output, J, WeightOut),
	format("   ~f", [WeightOut]),
	J1 is J+1,
	set_value(j,J1),
	J1 >= NumWeightsOutputLayer, !,
	eraseall(i),
	eraseall(j).

%%
%%  initialize_ANN(+InUnits, +HiddenUnits)
%%
initialize_ANN(InUnits, HiddenUnits) :-
	set_value(inUnits, InUnits),
	set_value(hiddenUnits, HiddenUnits),
	NumWeightsHiddenLayer is (InUnits+1)*HiddenUnits,
	NumWeightsOutputLayer is HiddenUnits+1,
	set_value(numWeightsHiddenLayer, NumWeightsHiddenLayer),
	set_value(numWeightsOutputLayer, NumWeightsOutputLayer),
	static_array(w_hidden,NumWeightsHiddenLayer,float),  % TO DO: make
	static_array(w_output,NumWeightsOutputLayer,float),  % these user-def.
	%
	% initialize to random weights in [-0.3,0.3] ...
	% ... HIDDEN LAYER
	set_value(i,0),
	repeat,
	get_value(i,I),
	RandWeightH is (random*0.6)-0.3,
	update_array(w_hidden, I, RandWeightH),
	I1 is I+1,
	set_value(i,I1),
	I1 >= NumWeightsHiddenLayer, !,
	%
	% ... OUTPUT LAYER
	set_value(j,0),
	repeat,
	get_value(j,J),
	RandWeightO is (random*0.6)-0.3,
	update_array(w_output, J, RandWeightO),
	J1 is J+1,
	set_value(j,J1),
	J1 >= NumWeightsOutputLayer, !,
	eraseall(i),
	eraseall(j),
	format(
	     "[Initialized Neural Network with ~d Inputs and ~d Hidden Units]~n",
	     [InUnits, HiddenUnits]).


%%
%%  run_example(+ArrayIn, -Out)
%%
run_example(ArrayIn, Out) :-
	get_value(inUnits, InUnits),
	get_value(hiddenUnits, HiddenUnits),
	get_value(numWeightsHiddenLayer, NumWeightsHiddenLayer),
	get_value(numWeightsOutputLayer, NumWeightsOutputLayer),
	(close_static_array(a_hidden) ; true),
	static_array(a_hidden, NumWeightsOutputLayer, float),
	update_array(a_hidden, HiddenUnits, 1.0),   % Hardcoded bias node
	set_value(a_out, 0.0),
	%%%%%%%%%%%%%%%
	%
	%  forward propagate through hidden layer
	set_value(i,0),
	repeat,
	get_value(i,I),
	FromInputUnit is I // (HiddenUnits),
	ToHiddenUnit is I mod (HiddenUnits),
	(FromInputUnit = InUnits ->
		InH is 1;     % hardcoded bias node
		array_element(ArrayIn, FromInputUnit, InH)),
	array_element(w_hidden, I, WeightH),
	array_element(a_hidden, ToHiddenUnit, ACurr_H),
	ASub_H is WeightH*InH,
	ANew_H is ACurr_H + ASub_H,
	update_array(a_hidden, ToHiddenUnit, ANew_H),
	I1 is I+1,
	set_value(i,I1),
	I1 >= NumWeightsHiddenLayer, 
	!,
	%%%%%%%%%%%%%%%
	%
	%  apply activation function at hidden layer
	set_value(j,0),
	repeat,
	get_value(j,J),
	array_element(a_hidden, J, HiddenIn),
	activation(HiddenIn, HiddenOut),
	update_array(a_hidden, J, HiddenOut),
	J1 is J+1,
	set_value(j,J1),
	J1 >= HiddenUnits, 
	!,
	%%%%%%%%%%%%%%%
	%
	%  forward propagate through output layer
	set_value(k,0),
	repeat,
	get_value(k,K),
	array_element(a_hidden, K, InO),
	array_element(w_output, K, WeightO),
	get_value(a_out, ACurr_O),
	ASub_O is WeightO*InO,
	ANew_O is ACurr_O + ASub_O,
	set_value(a_out, ANew_O),
	K1 is K+1,
	set_value(k,K1),
	K1 >= NumWeightsOutputLayer, 
	!,
	%%%%%%%%%%%%%%%
	%
	%  apply activation function at output layer
	%         ... use a linear output unit
	%
	get_value(a_out, Out),
	eraseall(i),eraseall(j),eraseall(k).

%%
%%  train_example(+WeightsH, +WeightsO, +ArrayIn, +DesiredOut)
%%
train_example(WeightsH, WeightsO, ArrayIn, DesiredOut) :-
	nu(Nu),
	get_value(inUnits, InUnits),
	get_value(hiddenUnits, HiddenUnits),
	get_value(numWeightsHiddenLayer, NumWeightsHiddenLayer),
	get_value(numWeightsOutputLayer, NumWeightsOutputLayer),
	run_example(ArrayIn, Out),
	Error is DesiredOut - Out,
	%
	% linear output unit
	%
	Del_Output is Error,
	%
	(close_static_array(del_hidden) ; true),
	static_array(del_hidden, NumWeightsOutputLayer, float),
	%%%%%%%%%%%%%%%
	%
	%  back propagate error to hidden layer
	set_value(i,0),	repeat,	get_value(i,I),
	%
	array_element(a_hidden, I, X_i),
	array_element(w_output, I, Weight_i),
	activation_prime(X_i,ActPrime_X_i),
	Del_Hidden_i is ActPrime_X_i*Del_Output*Weight_i,
	update_array(del_hidden, I, Del_Hidden_i),
	%
	I1 is I+1, set_value(i,I1), I1 >= NumWeightsOutputLayer, 
	!,
	%%%%%%%%%%%%%%%
	%
	%  back propagate - update weights through hidden layer
	set_value(j,0),	repeat,	get_value(j,J),
	%
	array_element(WeightsO, J, Weight_j),
	array_element(a_hidden, J, X_j),
	Del_W_j is Del_Output*X_j*Nu,
	WeightNew_j is Weight_j + Del_W_j,
	update_array(WeightsO, J, WeightNew_j),
	%
	J1 is J+1, set_value(j,J1), J1 >= NumWeightsOutputLayer, 
	!,
	%%%%%%%%%%%%%%%
	%
	%  back propagate - update weights through input layer
	set_value(k,0), repeat, get_value(k,K),
	%
	FromInputUnit is K // (HiddenUnits),
	ToHiddenUnit is K mod (HiddenUnits),
	(FromInputUnit = InUnits ->
		X_k is 1;     % hardcoded bias node
		array_element(ArrayIn, FromInputUnit, X_k)),
	array_element(WeightsH, K, Weight_k),
	array_element(del_hidden, ToHiddenUnit, Del_k),
	Del_W_k is Del_k*X_k*Nu,
	WeightNew_k is Weight_k + Del_W_k,
	update_array(WeightsH, K, WeightNew_k),
	%
	K1 is K+1, set_value(k,K1), K1 >= NumWeightsHiddenLayer, 
	%
	AbsError is abs(Error),
	get_value(errorSum, Sum),
	NewSum is Sum+AbsError,
	set_value(errorSum, NewSum),
	!.

%%
%% sum all the pos elts. in an array
%%
sum_pos_elts(Array,Sum1) :-
	set_value(sum,0.0),
	set_value(j,0),
	array_element(Array, 0, _),   % make sure >= 1 elt in array
	repeat,
	get_value(j,J),	J1 is J+1, set_value(j,J1),
	array_element(Array, J, Out),
	get_value(sum,Sum),
	(Out > 0 ->
		Sum1 is Sum+Out;
		Sum1 is Sum),
	set_value(sum,Sum1),
	not(array_element(Array, J1, _)).  % repeat until no elts. remain



%%
%%  compute_gains(+GainsArray, +Input) ...
%%    - computes, for each input, a gain value which shows how much flipping
%%      this particular input from 'Input' increases 
%%      (or decreases for gain <= 0) the output.
%%    - the complete list of gains are stored in the array specified in
%%      parameter 'GainsArray'.  
%%
compute_gains(Gains,Input) :-
	get_value(inUnits, InUnits),
	get_value(hiddenUnits, HiddenUnits),
	get_value(numWeightsHiddenLayer, NumWeightsHiddenLayer),
	get_value(numWeightsOutputLayer, NumWeightsOutputLayer),
	(close_static_array(Gains) ; true),
	static_array(Gains, InUnits, float),
	run_example(Input, _),
	%
	% linear output unit
	Del_Output is 1,
	(close_static_array(del_hidden) ; true),
	static_array(del_hidden, NumWeightsOutputLayer, float),
	%%%%%%%%%%%%%%%
	%
	%  back propagate to hidden layer
	set_value(i,0),	repeat,	get_value(i,I),
	%
	array_element(a_hidden, I, X_i),
	array_element(w_output, I, Weight_i),
	activation_prime(X_i,ActPrime_X_i),
	Del_Hidden_i is ActPrime_X_i*Weight_i*Del_Output,
	update_array(del_hidden, I, Del_Hidden_i),
	%
	I1 is I+1, set_value(i,I1), I1 >= NumWeightsOutputLayer, 
	!,
	%%%%%%%%%%%%%%%
	%
	%  back propagate - update inputs
	set_value(j,0), repeat, 
	get_value(j,J),
	%
	FromInputUnit is J // (HiddenUnits),
	ToHiddenUnit is J mod (HiddenUnits),
	(FromInputUnit = InUnits ->
		true     % can't change bias node input
	;
		array_element(Gains, FromInputUnit, Gain_j),
		array_element(w_hidden, J, Weight_j),
		array_element(del_hidden, ToHiddenUnit, Del_j),
		Del_Gain_j is Del_j*Weight_j, 
		GainNew_j is Gain_j + Del_Gain_j,
		update_array(Gains, FromInputUnit, GainNew_j)),
	%
	J1 is J+1, set_value(j,J1), J1 >= NumWeightsHiddenLayer,!.


%%
%%  activation(+X, -X_out)
%%
activation(X, X_out) :-
	X_out is 1/(1+exp(-X)).


%%
%%  activation_prime(+X, -X_out)
%%
activation_prime(X, X_out) :-
	X_out is X*(1-X).

%%
%%  nu(-Nu)
%%
nu(Nu) :- 
	fpd_setting(ann_learning_rate, NuBig, ok), 
	Nu is NuBig,
	!.
nu(Nu) :- 
	jws_setting(report_details_level, JWS_LEVEL),
	(JWS_LEVEL > 1 ->
		uw_format("[ANN learning rate now ~p]~n", [Nu]);
		true),
	fpd_set(ann_learning_rate, Nu), !.    % override the old setting
omega(Omega) :- Omega = 0.1, !.

sandwich(Lo, X, Hi, Result)  :- Temp is min(X, Hi), Result is max(Lo, Temp).

%%
%%  delete_ANN
%%
delete_ANN :-
	close_static_array(w_hidden),
	close_static_array(w_output).
