:- modeh(1,answer(+id,+box_term)).
:- modeh(1,answer(+id,+dt_term)).

%%%%%%%%%
% Mode  %
% Boxer %
%%%%%%%%%
:- modeb(*,     topic(-source, +id, -box_term, +box_term)).
:- modeb(*,     topic(-source, +id, +box_term, +box_term)).
:- modeb(*,      noum(-source, +id, -box_term, +box_term)).
:- modeb(*,      noum(-source, +id, +box_term, +box_term)).
:- modeb(*,    number(-source, +id, -box_term, +box_term)).
:- modeb(*,    number(-source, +id, +box_term, +box_term)).
:- modeb(*, adjective(-source, +id, -box_term, +box_term)).
:- modeb(*, adjective(-source, +id, +box_term, +box_term)).
:- modeb(*,      verb(-source, +id, -box_term, +box_term)).
:- modeb(*,      verb(-source, +id, +box_term, +box_term, +box_term)).
:- modeb(*,  relation(-source, +id, +box_term, -box_term, +box_term)).
:- modeb(*,  relation(-source, +id, -box_term, +box_term, +box_term)).
:- modeb(*,  relation(-source, +id, -box_term, -box_term, +box_term)).
:- modeb(*,        eq(-source, +id, -box_term, +box_term)).
:- modeb(*,        eq(-source, +id, +box_term, +box_term)).
:- modeb(*,      card(-source, +id, -box_term, +box_term)).
:- modeb(*,      card(-source, +id, +box_term, +box_term)).



%%%%%%%%%%%
%  Mode   %
% DepTree %
%%%%%%%%%%%
:- modeb(*,         ccomp(-source, +id, -dt_term, +dt_term)).
:- modeb(*,         ccomp(-source, +id, +dt_term, +dt_term)).
:- modeb(*,            cc(-source, +id, -dt_term, +dt_term)).
:- modeb(*,            cc(-source, +id, +dt_term, +dt_term)).
:- modeb(*,           det(-source, +id, -dt_term, +dt_term)).
:- modeb(*,           det(-source, +id, +dt_term, +dt_term)).
:- modeb(*,         agent(-source, +id, -dt_term, +dt_term)).
:- modeb(*,         agent(-source, +id, +dt_term, +dt_term)).
:- modeb(*,        entity(-source, +id, -dt_term)).
:- modeb(*,        entity(-source, +id, +dt_term)).
:- modeb(*,           prt(-source, +id, -dt_term, +dt_term)).
:- modeb(*,           prt(-source, +id, +dt_term, +dt_term)).
:- modeb(*,           num(-source, +id, -dt_term, +dt_term)).
:- modeb(*,           num(-source, +id, +dt_term, +dt_term)).
:- modeb(*,     nsubjpass(-source, +id, -dt_term, +dt_term)).
:- modeb(*,     nsubjpass(-source, +id, +dt_term, +dt_term)).
:- modeb(*,          conj(-source, +id, -dt_term, +dt_term)).
:- modeb(*,          conj(-source, +id, +dt_term, +dt_term)).
:- modeb(*,          dobj(-source, +id, -dt_term, +dt_term)).
:- modeb(*,          dobj(-source, +id, +dt_term, +dt_term)).
:- modeb(*,           adv(-source, +id, -dt_term, +dt_term)).
:- modeb(*,           adv(-source, +id, +dt_term, +dt_term)).
:- modeb(*,           adp(-source, +id, -dt_term, +dt_term)).
:- modeb(*,           adp(-source, +id, +dt_term, +dt_term)).
:- modeb(*,           neg(-source, +id, -dt_term, +dt_term)).
:- modeb(*,           neg(-source, +id, +dt_term, +dt_term)).
:- modeb(*,          poss(-source, +id, -dt_term, +dt_term)).
:- modeb(*,          poss(-source, +id, +dt_term, +dt_term)).
:- modeb(*,          pron(-source, +id, -dt_term, +dt_term)).
:- modeb(*,          pron(-source, +id, +dt_term, +dt_term)).
:- modeb(*,       auxpass(-source, +id, -dt_term, +dt_term)).
:- modeb(*,       auxpass(-source, +id, +dt_term, +dt_term)).
:- modeb(*,         advcl(-source, +id, -dt_term, +dt_term)).
:- modeb(*,         advcl(-source, +id, +dt_term, +dt_term)).
:- modeb(*,           aux(-source, +id, -dt_term, +dt_term)).
:- modeb(*,           aux(-source, +id, +dt_term, +dt_term)).
:- modeb(*,           adj(-source, +id, -dt_term, +dt_term)).
:- modeb(*,           adj(-source, +id, +dt_term, +dt_term)).
:- modeb(*,          prep(-source, +id, -dt_term, +dt_term)).
:- modeb(*,          prep(-source, +id, +dt_term, +dt_term)).
:- modeb(*,     punct_sym(-source, +id, -dt_term, +dt_term)).
:- modeb(*,     punct_sym(-source, +id, +dt_term, +dt_term)).
:- modeb(*,          verb(-source, +id, -dt_term, +dt_term)).
:- modeb(*,          verb(-source, +id, +dt_term, +dt_term)).
:- modeb(*,        advmod(-source, +id, -dt_term, +dt_term)).
:- modeb(*,        advmod(-source, +id, +dt_term, +dt_term)).
:- modeb(*,         nsubj(-source, +id, -dt_term, +dt_term)).
:- modeb(*,         nsubj(-source, +id, +dt_term, +dt_term)).
:- modeb(*,        nummod(-source, +id, -dt_term, +dt_term)).
:- modeb(*,        nummod(-source, +id, +dt_term, +dt_term)).
:- modeb(*, sentence_root(-source, +id, +dt_term)).
:- modeb(*,           sym(-source, +id, -dt_term, +dt_term)).
:- modeb(*,           sym(-source, +id, +dt_term, +dt_term)).
:- modeb(*,         punct(-source, +id, -dt_term, +dt_term)).
:- modeb(*,         punct(-source, +id, +dt_term, +dt_term)).
:- modeb(*,          part(-source, +id, -dt_term, +dt_term)).
:- modeb(*,          part(-source, +id, +dt_term, +dt_term)).
:- modeb(*,      compound(-source, +id, -dt_term, +dt_term)).
:- modeb(*,      compound(-source, +id, +dt_term, +dt_term)).
:- modeb(*,         pcomp(-source, +id, -dt_term, +dt_term)).
:- modeb(*,         pcomp(-source, +id, +dt_term, +dt_term)).
:- modeb(*,          intj(-source, +id, -dt_term, +dt_term)).
:- modeb(*,          intj(-source, +id, +dt_term, +dt_term)).
:- modeb(*,         relcl(-source, +id, -dt_term, +dt_term)).
:- modeb(*,         relcl(-source, +id, +dt_term, +dt_term)).
:- modeb(*,      npadvmod(-source, +id, -dt_term, +dt_term)).
:- modeb(*,      npadvmod(-source, +id, +dt_term, +dt_term)).
:- modeb(*,          case(-source, +id, -dt_term, +dt_term)).
:- modeb(*,          case(-source, +id, +dt_term, +dt_term)).
:- modeb(*,    propername(-source, +id, -dt_term, +dt_term)).
:- modeb(*,    propername(-source, +id, +dt_term, +dt_term)).
:- modeb(*,          noun(-source, +id, -dt_term, +dt_term)).
:- modeb(*,          noun(-source, +id, +dt_term, +dt_term)).
:- modeb(*,          attr(-source, +id, -dt_term, +dt_term)).
:- modeb(*,          attr(-source, +id, +dt_term, +dt_term)).
:- modeb(*,           dep(-source, +id, -dt_term, +dt_term)).
:- modeb(*,           dep(-source, +id, +dt_term, +dt_term)).
:- modeb(*,         appos(-source, +id, -dt_term, +dt_term)).
:- modeb(*,         appos(-source, +id, +dt_term, +dt_term)).
:- modeb(*,         cconj(-source, +id, -dt_term, +dt_term)).
:- modeb(*,         cconj(-source, +id, +dt_term, +dt_term)).
:- modeb(*,         xcomp(-source, +id, -dt_term, +dt_term)).
:- modeb(*,         xcomp(-source, +id, +dt_term, +dt_term)).
:- modeb(*,          nmod(-source, +id, -dt_term, +dt_term)).
:- modeb(*,          nmod(-source, +id, +dt_term, +dt_term)).
:- modeb(*,          amod(-source, +id, -dt_term, +dt_term)).
:- modeb(*,          amod(-source, +id, +dt_term, +dt_term)).
:- modeb(*,          mark(-source, +id, -dt_term, +dt_term)).
:- modeb(*,          mark(-source, +id, +dt_term, +dt_term)).
:- modeb(*,          pobj(-source, +id, -dt_term, +dt_term)).
:- modeb(*,          pobj(-source, +id, +dt_term, +dt_term)).
:- modeb(*,             x(-source, +id, -dt_term, +dt_term)).
:- modeb(*,             x(-source, +id, +dt_term, +dt_term)).
:- modeb(*,          oprd(-source, +id, -dt_term, +dt_term)).
:- modeb(*,          oprd(-source, +id, +dt_term, +dt_term)).
:- modeb(*,      quantmod(-source, +id, -dt_term, +dt_term)).
:- modeb(*,      quantmod(-source, +id, +dt_term, +dt_term)).
:- modeb(*,           acl(-source, +id, -dt_term, +dt_term)).
:- modeb(*,           acl(-source, +id, +dt_term, +dt_term)).



%%%%%%%%%%%%%%%%%
% Determination %
%     Boxer     %
%%%%%%%%%%%%%%%%%
:- determination(answer/2, topic/4).
:- determination(answer/2, noum/4).
:- determination(answer/2, number/4).
:- determination(answer/2, adjective/4).
:- determination(answer/2, verb/4).
:- determination(answer/2, relation/5).
:- determination(answer/2, eq/4).
:- determination(answer/2, card/4).




%%%%%%%%%%%%%%%%%
% Determination %
%    DepTree    %
%%%%%%%%%%%%%%%%%
:- determination(answer/2, ccomp/4).
:- determination(answer/2, cc/4).
:- determination(answer/2, det/4).
:- determination(answer/2, agent/4).
:- determination(answer/2, entity/3).
:- determination(answer/2, prt/4).
:- determination(answer/2, num/4).
:- determination(answer/2, nsubjpass/4).
:- determination(answer/2, conj/4).
:- determination(answer/2, dobj/4).
:- determination(answer/2, adv/4).
:- determination(answer/2, adp/4).
:- determination(answer/2, neg/4).
:- determination(answer/2, poss/4).
:- determination(answer/2, pron/4).
:- determination(answer/2, auxpass/4).
:- determination(answer/2, advcl/4).
:- determination(answer/2, aux/4).
:- determination(answer/2, adj/4).
:- determination(answer/2, prep/4).
:- determination(answer/2, punct_sym/4).
:- determination(answer/2, verb/4).
:- determination(answer/2, advmod/4).
:- determination(answer/2, nsubj/4).
:- determination(answer/2, nummod/4).
:- determination(answer/2, sentence_root/3).
:- determination(answer/2, sym/4).
:- determination(answer/2, punct/4).
:- determination(answer/2, part/4).
:- determination(answer/2, compound/4).
:- determination(answer/2, pcomp/4).
:- determination(answer/2, intj/4).
:- determination(answer/2, relcl/4).
:- determination(answer/2, npadvmod/4).
:- determination(answer/2, case/4).
:- determination(answer/2, propername/4).
:- determination(answer/2, noun/4).
:- determination(answer/2, attr/4).
:- determination(answer/2, dep/4).
:- determination(answer/2, appos/4).
:- determination(answer/2, cconj/4).
:- determination(answer/2, xcomp/4).
:- determination(answer/2, nmod/4).
:- determination(answer/2, amod/4).
:- determination(answer/2, mark/4).
:- determination(answer/2, pobj/4).
:- determination(answer/2, x/4).
:- determination(answer/2, oprd/4).
:- determination(answer/2, quantmod/4).
:- determination(answer/2, acl/4).

%:- consult(kb).

%%%%%%%%%%%%%%%%%
% Eval settings %
%%%%%%%%%%%%%%%%%

%:- set(evalfn,auto_m).
:- set(evalfn,f).
:- set(noise,  5).
:- set(minpos, 2).
%:- set(nodes, 10000).
:- set(clauselength, 10).