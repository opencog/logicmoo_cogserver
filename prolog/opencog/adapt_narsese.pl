/*
 * LOGICMOO CogServer API Impl
 *
 * Copyright (c) 2022 Logicmoo Co <support@logicmoo.org>
 *
 * LICENSE:
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License v3 as
 * published by the Free Software Foundation and including the exceptions
 * at http://opencog.org/wiki/Licenses
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program; if not, write to:
 * Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

use_nars_config(File):-  (\+ atom(File); \+ is_absolute_file_name(File)),
   absolute_file_name(File, Absolute), !, use_nars_config(Absolute).
use_nars_config(Absolute):-  open(Absolute, read, In),
    load_sgml(In, Dom,
                   [  dialect(html5),
                      attribute_value(string),
                      cdata(string),
                     nars_system_entities(true),
                     nars_space(remove),
                     nars_syntax_errors(quiet),
                      case_preserving_attributes(false),
                      case_sensitive_attributes(false),
                   max_errors(-1)]), !,
    close(In),
    use_nars_config_info(Dom), !.

 parse_config:-
   use_nars_config(library('../config/mvpConfig.xml')).



% This program covers the inferencerules of upto NAL-6 in
% "Non-Axiomatic Logic: A Model of Intelligent Reasoning"
% For the details of syntax, see the "User's Guide of NAL"

%%% individual inferencerules

% There are three types of inferencerules in NAL:
% (1) "revision" merges its two premises into a conclusion;
% (2) "choice"nars_selects one of its two premises as a conclusion;
% (3) "inference" generates a conclusion from one or two premises.

nars_ctx(default).

%revision/3
revision([S, T1], [S, T2], [S, T]):- nars_revision([S, T1], [S, T2], [S, T]).

nars_revision([S, T1], [S, T2], [S, T]):-
 	narz_f_rev(T1, T2, T).

%NARS choice/3
choice(X, Y, Z):- nars_choice(X, Y, Z).

nars_choice([S, [F1, C1]], [S, [_F2, C2]], [S, [F1, C1]]):-
 	C1 >= C2, !.
nars_choice([S, [_F1, C1]], [S, [F2, C2]], [S, [F2, C2]]):-
 	C1 < C2, !.
nars_choice([S1, T1], [S2, T2], [S1, T1]):-
 	S1 \= S2, narz_f_exp(T1, E1), narz_f_exp(T2, E2), E1 >= E2, !.
nars_choice([S1, T1], [S2, T2], [S2, T2]):-
 	S1 \= S2, narz_f_exp(T1, E1), narz_f_exp(T2, E2), E1 < E2, !.


%NARS infer-ence/2 (simplified version)
infer(T1, T):- nars_infer(T1, T).

nars_infer(T1, T):-  nars_ctx(Ctx), nars_inference(Ctx, [T1, [1, 0.9]], T).

nars_infer(
          inheritance(W1, ext_image(ext_image(represent, [nil, inheritance(product([X, T2]), R)]), [nil, W2, W3])), 
          inheritance(W1, ext_image(represent, [nil, X])), 
             [inheritance(ext_image(represent, [nil, Y]), 
                          ext_image(ext_image(represent, [nil, inheritance(product([Y, T2]), R)]), [nil, W2, W3])),
           V]):-
   narz_f_ind([1, 0.9], [1, 0.9], V), !.

nars_infer(inheritance(W3, ext_image(ext_image(represent, [nil, inheritance(product([T1, X]), R)]), [W1, W2, nil])), inheritance(W3, ext_image(represent, [nil, X])), [inheritance(ext_image(represent, [nil, Y]), ext_image(ext_image(represent, [nil, inheritance(product([T1, Y]), R)]), [W1, W2, nil])), V]):-
 narz_f_ind([1, 0.9], [1, 0.9], V), !.

nars_infer(T1, T2, T):-  nars_ctx(Ctx), nars_inference(Ctx, [T1, [1, 0.9]], [T2, [1, 0.9]], T).


%NARS inference/2
inference(T1, T):- nars_ctx(Ctx), nars_inference(Ctx, T1, T).

%% immediate inference

nars_inference(_Ctx, [inheritance(S, P), T1], [inheritance(P, S), T]):-
 	narz_f_cnv(T1, T).
nars_inference(_Ctx, [implication(S, P), T1], [implication(P, S), T]):-
 	narz_f_cnv(T1, T).
nars_inference(_Ctx, [implication(negation(S), P), T1], [implication(negation(P), S), T]):-
 	narz_f_cnt(T1, T).

nars_inference(_Ctx, [negation(S), T1], [S, T]):-
 	narz_f_neg(T1, T).
nars_inference(_Ctx, [S, [F1, C1]], [negation(S), T]):-
 	F1 < 0.5, narz_f_neg([F1, C1], T).

%% structural inference

nars_inference(_Ctx, [S1, T], [S, T]):-
 	narz_reduce(S1, S), S1 \== S, !.
nars_inference(Ctx, [S1, T], [S, T]):-
 	nars_equivalence(Ctx, S1, S);nars_equivalence(Ctx, S, S1).

nars_inference(Ctx, P, C):-
 	nars_inference(Ctx, P, [S, [1, 1]], C), call(S).
nars_inference(Ctx, P, C):-
 	nars_inference(Ctx, [S, [1, 1]], P, C), call(S).


% inference/3
inference(X, Y, Z):- nars_ctx(Ctx), nars_inference(Ctx, X, Y, Z).


%% inheritance-based syllogism

nars_inference(_Ctx, [inheritance(M, P), T1], [inheritance(S, M), T2], [inheritance(S, P), T]):-
 	S \= P, narz_f_ded(T1, T2, T).
nars_inference(_Ctx, [inheritance(P, M), T1], [inheritance(S, M), T2], [inheritance(S, P), T]):-
 	S \= P, narz_f_abd(T1, T2, T).
nars_inference(_Ctx, [inheritance(M, P), T1], [inheritance(M, S), T2], [inheritance(S, P), T]):-
 	S \= P, narz_f_ind(T1, T2, T).
nars_inference(_Ctx, [inheritance(P, M), T1], [inheritance(M, S), T2], [inheritance(S, P), T]):-
 	S \= P, narz_f_exe(T1, T2, T).

%% similarity from inheritance

nars_inference(_Ctx, [inheritance(S, P), T1], [inheritance(P, S), T2], [similarity(S, P), T]):-
 	narz_f_int(T1, T2, T).

%% similarity-based syllogism

nars_inference(_Ctx, [inheritance(P, M), T1], [inheritance(S, M), T2], [similarity(S, P), T]):-
 	S \= P, narz_f_com(T1, T2, T).
nars_inference(_Ctx, [inheritance(M, P), T1], [inheritance(M, S), T2], [similarity(S, P), T]):-
 	S \= P, narz_f_com(T1, T2, T).
nars_inference(_Ctx, [inheritance(M, P), T1], [similarity(S, M), T2], [inheritance(S, P), T]):-
 	S \= P, narz_f_ana(T1, T2, T).
nars_inference(_Ctx, [inheritance(P, M), T1], [similarity(S, M), T2], [inheritance(P, S), T]):-
 	S \= P, narz_f_ana(T1, T2, T).
nars_inference(_Ctx, [similarity(M, P), T1], [similarity(S, M), T2], [similarity(S, P), T]):-
 	S \= P, narz_f_res(T1, T2, T).

%% inheritance-based composition

nars_inference(_Ctx, [inheritance(P, M), T1], [inheritance(S, M), T2], [inheritance(N, M), T]):-
 	S \= P, narz_reduce(int_intersection([P, S]), N), narz_f_int(T1, T2, T).
nars_inference(_Ctx, [inheritance(P, M), T1], [inheritance(S, M), T2], [inheritance(N, M), T]):-
 	S \= P, narz_reduce(ext_intersection([P, S]), N), narz_f_uni(T1, T2, T).
nars_inference(_Ctx, [inheritance(P, M), T1], [inheritance(S, M), T2], [inheritance(N, M), T]):-
 	S \= P, narz_reduce(int_difference(P, S), N), narz_f_dif(T1, T2, T).
nars_inference(_Ctx, [inheritance(M, P), T1], [inheritance(M, S), T2], [inheritance(M, N), T]):-
 	S \= P, narz_reduce(ext_intersection([P, S]), N), narz_f_int(T1, T2, T).
nars_inference(_Ctx, [inheritance(M, P), T1], [inheritance(M, S), T2], [inheritance(M, N), T]):-
 	S \= P, narz_reduce(int_intersection([P, S]), N), narz_f_uni(T1, T2, T).
nars_inference(_Ctx, [inheritance(M, P), T1], [inheritance(M, S), T2], [inheritance(M, N), T]):-
 	S \= P, narz_reduce(ext_difference(P, S), N), narz_f_dif(T1, T2, T).

%% inheirance-based decomposition

nars_inference(_Ctx, [inheritance(S, M), T1], [inheritance(int_intersection(L), M), T2], [inheritance(P, M), T]):-
 	narz_ground(S), narz_ground(L), member(S, L), delete(L, S, N), narz_reduce(int_intersection(N), P), narz_f_pnn(T1, T2, T).
nars_inference(_Ctx, [inheritance(S, M), T1], [inheritance(ext_intersection(L), M), T2], [inheritance(P, M), T]):-
 	narz_ground(S), narz_ground(L), member(S, L), delete(L, S, N), narz_reduce(ext_intersection(N), P), narz_f_npp(T1, T2, T).
nars_inference(_Ctx, [inheritance(S, M), T1], [inheritance(int_difference(S, P), M), T2], [inheritance(P, M), T]):-
 	atom(S), atom(P), narz_f_pnp(T1, T2, T).
nars_inference(_Ctx, [inheritance(S, M), T1], [inheritance(int_difference(P, S), M), T2], [inheritance(P, M), T]):-
 	atom(S), atom(P), narz_f_nnn(T1, T2, T).
nars_inference(_Ctx, [inheritance(M, S), T1], [inheritance(M, ext_intersection(L)), T2], [inheritance(M, P), T]):-
 	narz_ground(S), narz_ground(L), member(S, L), delete(L, S, N), narz_reduce(ext_intersection(N), P), narz_f_pnn(T1, T2, T).
nars_inference(_Ctx, [inheritance(M, S), T1], [inheritance(M, int_intersection(L)), T2], [inheritance(M, P), T]):-
 	narz_ground(S), narz_ground(L), member(S, L), delete(L, S, N), narz_reduce(int_intersection(N), P), narz_f_npp(T1, T2, T).
nars_inference(_Ctx, [inheritance(M, S), T1], [inheritance(M, ext_difference(S, P)), T2], [inheritance(M, P), T]):-
 	atom(S), atom(P), narz_f_pnp(T1, T2, T).
nars_inference(_Ctx, [inheritance(M, S), T1], [inheritance(M, ext_difference(P, S)), T2], [inheritance(M, P), T]):-
 	atom(S), atom(P), narz_f_nnn(T1, T2, T).

%% implication-based syllogism

nars_inference(_Ctx, [implication(M, P), T1], [implication(S, M), T2], [implication(S, P), T]):-
 	S \= P, narz_f_ded(T1, T2, T).
nars_inference(_Ctx, [implication(P, M), T1], [implication(S, M), T2], [implication(S, P), T]):-
 	S \= P, narz_f_abd(T1, T2, T).
nars_inference(_Ctx, [implication(M, P), T1], [implication(M, S), T2], [implication(S, P), T]):-
 	S \= P, narz_f_ind(T1, T2, T).
nars_inference(_Ctx, [implication(P, M), T1], [implication(M, S), T2], [implication(S, P), T]):-
 	S \= P, narz_f_exe(T1, T2, T).

%% implication to equivalence

nars_inference(_Ctx, [implication(S, P), T1], [implication(P, S), T2], [equivalence(S, P), T]):-
 	narz_f_int(T1, T2, T).

%% equivalence-based syllogism

nars_inference(_Ctx, [implication(P, M), T1], [implication(S, M), T2], [equivalence(S, P), T]):-
 	S \= P, narz_f_com(T1, T2, T).
nars_inference(_Ctx, [implication(M, P), T1], [implication(M, S), T2], [equivalence(S, P), T]):-
 	S \= P, narz_f_com(T1, T2, T).
nars_inference(_Ctx, [implication(M, P), T1], [equivalence(S, M), T2], [implication(S, P), T]):-
 	S \= P, narz_f_ana(T1, T2, T).
nars_inference(_Ctx, [implication(P, M), T1], [equivalence(S, M), T2], [implication(P, S), T]):-
 	S \= P, narz_f_ana(T1, T2, T).
nars_inference(_Ctx, [equivalence(M, P), T1], [equivalence(S, M), T2], [equivalence(S, P), T]):-
 	S \= P, narz_f_res(T1, T2, T).

%% implication-based composition

nars_inference(_Ctx, [implication(P, M), T1], [implication(S, M), T2], [implication(N, M), T]):-
 	S \= P, narz_reduce(disjunction([P, S]), N), narz_f_int(T1, T2, T).
nars_inference(_Ctx, [implication(P, M), T1], [implication(S, M), T2], [implication(N, M), T]):-
 	S \= P, narz_reduce(conjunction([P, S]), N), narz_f_uni(T1, T2, T).
nars_inference(_Ctx, [implication(M, P), T1], [implication(M, S), T2], [implication(M, N), T]):-
 	S \= P, narz_reduce(conjunction([P, S]), N), narz_f_int(T1, T2, T).
nars_inference(_Ctx, [implication(M, P), T1], [implication(M, S), T2], [implication(M, N), T]):-
 	S \= P, narz_reduce(disjunction([P, S]), N), narz_f_uni(T1, T2, T).

%% implication-based decomposition

nars_inference(_Ctx, [implication(S, M), T1], [implication(disjunction(L), M), T2], [implication(P, M), T]):-
 	narz_ground(S), narz_ground(L), member(S, L), delete(L, S, N), narz_reduce(disjunction(N), P), narz_f_pnn(T1, T2, T).
nars_inference(_Ctx, [implication(S, M), T1], [implication(conjunction(L), M), T2], [implication(P, M), T]):-
 	narz_ground(S), narz_ground(L), member(S, L), delete(L, S, N), narz_reduce(conjunction(N), P), narz_f_npp(T1, T2, T).
nars_inference(_Ctx, [implication(M, S), T1], [implication(M, conjunction(L)), T2], [implication(M, P), T]):-
 	narz_ground(S), narz_ground(L), member(S, L), delete(L, S, N), narz_reduce(conjunction(N), P), narz_f_pnn(T1, T2, T).
nars_inference(_Ctx, [implication(M, S), T1], [implication(M, disjunction(L)), T2], [implication(M, P), T]):-
 	narz_ground(S), narz_ground(L), member(S, L), delete(L, S, N), narz_reduce(disjunction(N), P), narz_f_npp(T1, T2, T).

%% conditional syllogism

nars_inference(_Ctx, [implication(M, P), T1], [M, T2], [P, T]):-
 	narz_ground(P), narz_f_ded(T1, T2, T).
nars_inference(_Ctx, [implication(P, M), T1], [M, T2], [P, T]):-
 	narz_ground(P), narz_f_abd(T1, T2, T).
nars_inference(_Ctx, [M, T1], [equivalence(S, M), T2], [S, T]):-
 	narz_ground(S), narz_f_ana(T1, T2, T).

%% conditional composition

nars_inference(_Ctx, [P, T1], [S, T2], [C, T]):-
 	C == implication(S, P), narz_f_ind(T1, T2, T).
nars_inference(_Ctx, [P, T1], [S, T2], [C, T]):-
 	C == equivalence(S, P), narz_f_com(T1, T2, T).
nars_inference(_Ctx, [P, T1], [S, T2], [C, T]):-
 	narz_reduce(conjunction([P, S]), N), N == C, narz_f_int(T1, T2, T).
nars_inference(_Ctx, [P, T1], [S, T2], [C, T]):-
 	narz_reduce(disjunction([P, S]), N), N == C, narz_f_uni(T1, T2, T).

%% propositional decomposition

nars_inference(_Ctx, [S, T1], [conjunction(L), T2], [P, T]):-
 	narz_ground(S), narz_ground(L), member(S, L), delete(L, S, N), narz_reduce(conjunction(N), P), narz_f_pnn(T1, T2, T).
nars_inference(_Ctx, [S, T1], [disjunction(L), T2], [P, T]):-
 	narz_ground(S), narz_ground(L), member(S, L), delete(L, S, N), narz_reduce(disjunction(N), P), narz_f_npp(T1, T2, T).

%% multi-conditional syllogism

nars_inference(_Ctx, [implication(conjunction(L), C), T1], [M, T2], [implication(P, C), T]):-
 	nonvar(L), member(M, L), nars_subtract(L, [M], A), A \= [], narz_reduce(conjunction(A), P), narz_f_ded(T1, T2, T).
nars_inference(_Ctx, [implication(conjunction(L), C), T1], [implication(P, C), T2], [M, T]):-
 	narz_ground(L), member(M, L), nars_subtract(L, [M], A), A \= [], narz_reduce(conjunction(A), P), narz_f_abd(T1, T2, T).
nars_inference(_Ctx, [implication(conjunction(L), C), T1], [M, T2], [S, T]):-
 	S == implication(conjunction([M|L]), C), narz_f_ind(T1, T2, T).

nars_inference(_Ctx, [implication(conjunction(Lm), C), T1], [implication(A, M), T2], [implication(P, C), T]):-
 	nonvar(Lm), narz_replace(Lm, M, La, A), narz_reduce(conjunction(La), P), narz_f_ded(T1, T2, T).
nars_inference(_Ctx, [implication(conjunction(Lm), C), T1], [implication(conjunction(La), C), T2], [implication(A, M), T]):-
 	nonvar(Lm), narz_replace(Lm, M, La, A), narz_f_abd(T1, T2, T).
nars_inference(_Ctx, [implication(conjunction(La), C), T1], [implication(A, M), T2], [implication(P, C), T]):-
 	nonvar(La), narz_replace(Lm, M, La, A), narz_reduce(conjunction(Lm), P), narz_f_ind(T1, T2, T).

%% variable introduction

nars_inference(_Ctx, [inheritance(M, P), T1], [inheritance(M, S), T2], [implication(inheritance(X, S), inheritance(X, P)), T]):-
 	S \= P, narz_f_ind(T1, T2, T).
nars_inference(_Ctx, [inheritance(P, M), T1], [inheritance(S, M), T2], [implication(inheritance(P, X), inheritance(S, X)), T]):-
 	S \= P, narz_f_abd(T1, T2, T).
nars_inference(_Ctx, [inheritance(M, P), T1], [inheritance(M, S), T2], [equivalence(inheritance(X, S), inheritance(X, P)), T]):-
 	S \= P, narz_f_com(T1, T2, T).
nars_inference(_Ctx, [inheritance(P, M), T1], [inheritance(S, M), T2], [equivalence(inheritance(P, X), inheritance(S, X)), T]):-
 	S \= P, narz_f_com(T1, T2, T).
nars_inference(_Ctx, [inheritance(M, P), T1], [inheritance(M, S), T2], [conjunction([inheritance(var(Y, []), S), inheritance(var(Y, []), P)]), T]):-
 	S \= P, narz_f_int(T1, T2, T).
nars_inference(_Ctx, [inheritance(P, M), T1], [inheritance(S, M), T2], [conjunction([inheritance(S, var(Y, [])), inheritance(P, var(Y, []))]), T]):-
 	S \= P, narz_f_int(T1, T2, T).

%% 2nd variable introduction

nars_inference(_Ctx, [implication(A, inheritance(M1, P)), T1], [inheritance(M2, S), T2], [implication(conjunction([A, inheritance(X, S)]), inheritance(X, P)), T]):-
 	S \= P, M1 == M2, A \= inheritance(M2, S), narz_f_ind(T1, T2, T).
nars_inference(_Ctx, [implication(A, inheritance(M1, P)), T1], [inheritance(M2, S), T2], [conjunction([implication(A, inheritance(var(Y, []), P)), inheritance(var(Y, []), S)]), T]):-
 	S \= P, M1 == M2, A \= inheritance(M2, S), narz_f_int(T1, T2, T).
nars_inference(_Ctx, [conjunction(L1), T1], [inheritance(M, S), T2], [implication(inheritance(Y, S), conjunction([inheritance(Y, P2)|L3])), T]):-
 	nars_subtract(L1, [inheritance(M, P)], L2), L1 \= L2, S \= P, narz_dependant(P, Y, P2), narz_dependant(L2, Y, L3), narz_f_ind(T1, T2, T).
nars_inference(_Ctx, [conjunction(L1), T1], [inheritance(M, S), T2], [conjunction([inheritance(var(Y, []), S), inheritance(var(Y, []), P)|L2]), T]):-
 	nars_subtract(L1, [inheritance(M, P)], L2), L1 \= L2, S \= P, narz_f_int(T1, T2, T).

nars_inference(_Ctx, [implication(A, inheritance(P, M1)), T1], [inheritance(S, M2), T2], [implication(conjunction([A, inheritance(P, X)]), inheritance(S, X)), T]):-
 	S \= P, M1 == M2, A \= inheritance(S, M2), narz_f_abd(T1, T2, T).
nars_inference(_Ctx, [implication(A, inheritance(P, M1)), T1], [inheritance(S, M2), T2], [conjunction([implication(A, inheritance(P, var(Y, []))), inheritance(S, var(Y, []))]), T]):-
 	S \= P, M1 == M2, A \= inheritance(S, M2), narz_f_int(T1, T2, T).
nars_inference(_Ctx, [conjunction(L1), T1], [inheritance(S, M), T2], [implication(inheritance(S, Y), conjunction([inheritance(P2, Y)|L3])), T]):-
 	nars_subtract(L1, [inheritance(P, M)], L2), L1 \= L2, S \= P, narz_dependant(P, Y, P2), narz_dependant(L2, Y, L3), narz_f_abd(T1, T2, T).
nars_inference(_Ctx, [conjunction(L1), T1], [inheritance(S, M), T2], [conjunction([inheritance(S, var(Y, [])), inheritance(P, var(Y, []))|L2]), T]):-
 	nars_subtract(L1, [inheritance(P, M)], L2), L1 \= L2, S \= P, narz_f_int(T1, T2, T).

%% dependant variable elimination


nars_inference(_Ctx, [conjunction(L1), T1], [inheritance(M, S), T2], [C, T]):-
 	nars_subtract(L1, [inheritance(var(N, D), S)], L2), L1 \= L2,
 	replace_var(L2, var(N, D), L3, M), narz_reduce(conjunction(L3), C), narz_f_cnv(T2, T0), narz_f_ana(T1, T0, T).
nars_inference(_Ctx, [conjunction(L1), T1], [inheritance(S, M), T2], [C, T]):-
 	nars_subtract(L1, [inheritance(S, var(N, D))], L2), L1 \= L2,
 	replace_var(L2, var(N, D), L3, M), narz_reduce(conjunction(L3), C), narz_f_cnv(T2, T0), narz_f_ana(T1, T0, T).

replace_var([], _, [], _).
replace_var([inheritance(S1, P)|T1], S1, [inheritance(S2, P)|T2], S2):-
 	replace_var(T1, S1, T2, S2).
replace_var([inheritance(S, P1)|T1], P1, [inheritance(S, P2)|T2], P2):-
 	replace_var(T1, P1, T2, P2).
replace_all([H|T1], H1, [H|T2], H2):-
 	replace_var(T1, H1, T2, H2).



%%% Theorems in IL:

%NARS inheritance/2
inheritance(X, Y):- nars_ctx(Ctx), nars_inheritance(Ctx, X, Y).

nars_inheritance(_Ctx, ext_intersection(Ls), P):-
 	narz_include([P], Ls).
nars_inheritance(_Ctx, S, int_intersection(Lp)):-
 	narz_include([S], Lp).
nars_inheritance(_Ctx, ext_intersection(S), ext_intersection(P)):-
 	narz_include(P, S), P \= [_].
nars_inheritance(_Ctx, int_intersection(S), int_intersection(P)):-
 	narz_include(S, P), S \= [_].
nars_inheritance(_Ctx, ext_set(S), ext_set(P)):-
 	narz_include(S, P).
nars_inheritance(_Ctx, int_set(S), int_set(P)):-
 	narz_include(P, S).

nars_inheritance(_Ctx, ext_difference(S, P), S):-
 	narz_ground(S), narz_ground(P).
nars_inheritance(_Ctx, S, int_difference(S, P)):-
 	narz_ground(S), narz_ground(P).

nars_inheritance(_Ctx, product(L1), R):-
 	narz_ground(L1), member(ext_image(R, L2), L1), narz_replace(L1, ext_image(R, L2), L2).
nars_inheritance(_Ctx, R, product(L1)):-
 	narz_ground(L1), member(int_image(R, L2), L1), narz_replace(L1, int_image(R, L2), L2).

%NARS similarity/2
similarity(X, Y):- nars_ctx(Ctx), nars_similarity(Ctx, X, Y).

nars_similarity(_Ctx, X, Y):-
 	narz_ground(X), narz_reduce(X, Y), X \== Y, !.

nars_similarity(_Ctx, ext_intersection(L1), ext_intersection(L2)):-
 	narz_same_set(L1, L2).
nars_similarity(_Ctx, int_intersection(L1), int_intersection(L2)):-
 	narz_same_set(L1, L2).
nars_similarity(_Ctx, ext_set(L1), ext_set(L2)):-
 	narz_same_set(L1, L2).
nars_similarity(_Ctx, int_set(L1), int_set(L2)):-
 	narz_same_set(L1, L2).

%NARS implication/2
implication(X, Y):- nars_ctx(Ctx), nars_implication(Ctx, X, Y).

nars_implication(_Ctx, similarity(S, P), inheritance(S, P)).
nars_implication(_Ctx, equivalence(S, P), implication(S, P)).

nars_implication(_Ctx, conjunction(L), M):-
 	narz_ground(L), member(M, L).
nars_implication(_Ctx, M, disjunction(L)):-
 	narz_ground(L), member(M, L).

nars_implication(_Ctx, conjunction(L1), conjunction(L2)):-
 	narz_ground(L1), narz_ground(L2), subset(L2, L1).
nars_implication(_Ctx, disjunction(L1), disjunction(L2)):-
 	narz_ground(L1), narz_ground(L2), subset(L1, L2).

nars_implication(_Ctx, inheritance(S, P), inheritance(ext_intersection(Ls), ext_intersection(Lp))):-
 	narz_ground(Ls), narz_ground(Lp), narz_replace(Ls, S, L, P), narz_same(L, Lp).
nars_implication(_Ctx, inheritance(S, P), inheritance(int_intersection(Ls), int_intersection(Lp))):-
 	narz_ground(Ls), narz_ground(Lp), narz_replace(Ls, S, L, P), narz_same(L, Lp).
nars_implication(_Ctx, similarity(S, P), similarity(ext_intersection(Ls), ext_intersection(Lp))):-
 	narz_ground(Ls), narz_ground(Lp), narz_replace(Ls, S, L, P), narz_same(L, Lp).
nars_implication(_Ctx, similarity(S, P), similarity(int_intersection(Ls), int_intersection(Lp))):-
 	narz_ground(Ls), narz_ground(Lp), narz_replace(Ls, S, L, P), narz_same(L, Lp).

nars_implication(_Ctx, inheritance(S, P), inheritance(ext_difference(S, M), ext_difference(P, M))):-
 	narz_ground(M).
nars_implication(_Ctx, inheritance(S, P), inheritance(int_difference(S, M), int_difference(P, M))):-
 	narz_ground(M).
nars_implication(_Ctx, similarity(S, P), similarity(ext_difference(S, M), ext_difference(P, M))):-
 	narz_ground(M).
nars_implication(_Ctx, similarity(S, P), similarity(int_difference(S, M), int_difference(P, M))):-
 	narz_ground(M).
nars_implication(_Ctx, inheritance(S, P), inheritance(ext_difference(M, P), ext_difference(M, S))):-
 	narz_ground(M).
nars_implication(_Ctx, inheritance(S, P), inheritance(int_difference(M, P), int_difference(M, S))):-
 	narz_ground(M).
nars_implication(_Ctx, similarity(S, P), similarity(ext_difference(M, P), ext_difference(M, S))):-
 	narz_ground(M).
nars_implication(_Ctx, similarity(S, P), similarity(int_difference(M, P), int_difference(M, S))):-
 	narz_ground(M).

nars_implication(_Ctx, inheritance(S, P), negation(inheritance(S, ext_difference(M, P)))):-
 	narz_ground(M).
nars_implication(_Ctx, inheritance(S, ext_difference(M, P)), negation(inheritance(S, P))):-
 	narz_ground(M).
nars_implication(_Ctx, inheritance(S, P), negation(inheritance(int_difference(M, S), P))):-
 	narz_ground(M).
nars_implication(_Ctx, inheritance(int_difference(M, S), P), negation(inheritance(S, P))):-
 	narz_ground(M).

nars_implication(_Ctx, inheritance(S, P), inheritance(ext_image(S, M), ext_image(P, M))):-
 	narz_ground(M).
nars_implication(_Ctx, inheritance(S, P), inheritance(int_image(S, M), int_image(P, M))):-
 	narz_ground(M).
nars_implication(_Ctx, inheritance(S, P), inheritance(ext_image(M, Lp), ext_image(M, Ls))):-
 	narz_ground(Ls), narz_ground(Lp), append(L1, [S|L2], Ls), append(L1, [P|L2], Lp).
nars_implication(_Ctx, inheritance(S, P), inheritance(int_image(M, Lp), int_image(M, Ls))):-
 	narz_ground(Ls), narz_ground(Lp), append(L1, [S|L2], Ls), append(L1, [P|L2], Lp).

nars_implication(_Ctx, negation(M), negation(conjunction(L))):-
 	narz_include([M], L).
nars_implication(_Ctx, negation(disjunction(L)), negation(M)):-
 	narz_include([M], L).

nars_implication(_Ctx, implication(S, P), implication(conjunction(Ls), conjunction(Lp))):-
 	narz_ground(Ls), narz_ground(Lp), narz_replace(Ls, S, L, P), narz_same(L, Lp).
nars_implication(_Ctx, implication(S, P), implication(disjunction(Ls), disjunction(Lp))):-
 	narz_ground(Ls), narz_ground(Lp), narz_replace(Ls, S, L, P), narz_same(L, Lp).
nars_implication(_Ctx, equivalence(S, P), equivalence(conjunction(Ls), conjunction(Lp))):-
 	narz_ground(Ls), narz_ground(Lp), narz_replace(Ls, S, L, P), narz_same(L, Lp).
nars_implication(_Ctx, equivalence(S, P), equivalence(disjunction(Ls), disjunction(Lp))):-
 	narz_ground(Ls), narz_ground(Lp), narz_replace(Ls, S, L, P), narz_same(L, Lp).


%NARS equivalence/2
equivalence(X, Y):- nars_ctx(Ctx), nars_equivalence(Ctx, X, Y).

nars_equivalence(_Ctx, X, Y):-
 	narz_ground(X), narz_reduce(X, Y), X \== Y, !.

nars_equivalence(_Ctx, similarity(S, P), similarity(P, S)).

nars_equivalence(_Ctx, inheritance(S, ext_set([P])), similarity(S, ext_set([P]))).
nars_equivalence(_Ctx, inheritance(int_set([S]), P), similarity(int_set([S]), P)).

nars_equivalence(Ctx, inheritance(S, ext_intersection(Lp)), conjunction(L)):-
 	findall(nars_inheritance(Ctx, S, P), member(P, Lp), L).
nars_equivalence(Ctx, inheritance(int_intersection(Ls), P), conjunction(L)):-
 	findall(nars_inheritance(Ctx, S, P), member(S, Ls), L).

nars_equivalence(_Ctx, inheritance(S, ext_difference(P1, P2)),
 	    conjunction([inheritance(S, P1), negation(inheritance(S, P2))])).
nars_equivalence(_Ctx, inheritance(int_difference(S1, S2), P),
 	    conjunction([inheritance(S1, P), negation(inheritance(S2, P))])).

nars_equivalence(_Ctx, inheritance(product(Ls), product(Lp)), conjunction(L)):-
 	equ_product(Ls, Lp, L).

nars_equivalence(_Ctx, inheritance(product([S|L]), product([P|L])), inheritance(S, P)):-
 	narz_ground(L).
nars_equivalence(Ctx, inheritance(S, P), inheritance(product([H|Ls]), product([H|Lp]))):-
 	narz_ground(H), nars_equivalence(Ctx, inheritance(product(Ls), product(Lp)), inheritance(S, P)).

nars_equivalence(_Ctx, inheritance(product(L), R), inheritance(T, ext_image(R, L1))):-
 	narz_replace(L, T, L1).
nars_equivalence(_Ctx, inheritance(R, product(L)), inheritance(int_image(R, L1), T)):-
 	narz_replace(L, T, L1).

nars_equivalence(_Ctx, equivalence(S, P), equivalence(P, S)).

nars_equivalence(_Ctx, equivalence(negation(S), P), equivalence(negation(P), S)).

nars_equivalence(_Ctx, conjunction(L1), conjunction(L2)):-
 	narz_same_set(L1, L2).
nars_equivalence(_Ctx, disjunction(L1), disjunction(L2)):-
 	narz_same_set(L1, L2).

nars_equivalence(Ctx, implication(S, conjunction(Lp)), conjunction(L)):-
 	findall(nars_implication(Ctx, S, P), member(P, Lp), L).
nars_equivalence(Ctx, implication(disjunction(Ls), P), conjunction(L)):-
 	findall(nars_implication(Ctx, S, P), member(S, Ls), L).

nars_equivalence(Ctx, T1, T2):-
 	not(atom(T1)), not(atom(T2)), narz_ground(T1), narz_ground(T2),
 	T1 =.. L1, T2 =.. L2, nars_equivalence_list(Ctx, L1, L2).

nars_equivalence_list(_Ctx, L, L).
nars_equivalence_list(Ctx, [H|L1], [H|L2]):-
 	nars_equivalence_list(Ctx, L1, L2).
nars_equivalence_list(Ctx, [H1|L1], [H2|L2]):-
 	nars_similarity(Ctx, H1, H2), nars_equivalence_list(Ctx, L1, L2).
nars_equivalence_list(Ctx, [H1|L1], [H2|L2]):-
 	nars_equivalence(Ctx, H1, H2), nars_equivalence_list(Ctx, L1, L2).

% compound termnars_structurereduction

narz_reduce(similarity(ext_set([S]), ext_set([P])), similarity(S, P)):-
 	!.
narz_reduce(similarity(int_set([S]), int_set([P])), similarity(S, P)):-
 	!.

narz_reduce(instance(S, P), inheritance(ext_set([S]), P)):-
 	!.
narz_reduce(property(S, P), inheritance(S, int_set([P]))):-
 	!.
narz_reduce(inst_prop(S, P), inheritance(ext_set([S]), int_set([P]))):-
 	!.

narz_reduce(ext_intersection([T]), T):-
 	!.
narz_reduce(int_intersection([T]), T):-
 	!.

narz_reduce(ext_intersection([ext_intersection(L1), ext_intersection(L2)]), ext_intersection(L)):-
 	nars_union(L1, L2, L), !.
narz_reduce(ext_intersection([ext_intersection(L1), L2]), ext_intersection(L)):-
 	nars_union(L1, [L2], L), !.
narz_reduce(ext_intersection([L1, ext_intersection(L2)]), ext_intersection(L)):-
 	nars_union([L1], L2, L), !.
narz_reduce(ext_intersection([ext_set(L1), ext_set(L2)]), ext_set(L)):-
 	intersection(L1, L2, L), !.
narz_reduce(ext_intersection([int_set(L1), int_set(L2)]), int_set(L)):-
 	nars_union(L1, L2, L), !.

narz_reduce(int_intersection([int_intersection(L1), int_intersection(L2)]), int_intersection(L)):-
 	nars_union(L1, L2, L), !.
narz_reduce(int_intersection([int_intersection(L1), L2]), int_intersection(L)):-
 	nars_union(L1, [L2], L), !.
narz_reduce(int_intersection([L1, int_intersection(L2)]), int_intersection(L)):-
 	nars_union([L1], L2, L), !.
narz_reduce(int_intersection([int_set(L1), int_set(L2)]), int_set(L)):-
 	intersection(L1, L2, L), !.
narz_reduce(int_intersection([ext_set(L1), ext_set(L2)]), ext_set(L)):-
 	nars_union(L1, L2, L), !.

narz_reduce(ext_difference(ext_set(L1), ext_set(L2)), ext_set(L)):-
 	nars_subtract(L1, L2, L), !.
narz_reduce(int_difference(int_set(L1), int_set(L2)), int_set(L)):-
 	nars_subtract(L1, L2, L), !.

narz_reduce(product(product(L), T), product(L1)):-
 	append(L, [T], L1), !.

narz_reduce(ext_image(product(L1), L2), T1):-
 	member(T1, L1), narz_replace(L1, T1, L2), !.
narz_reduce(int_image(product(L1), L2), T1):-
 	member(T1, L1), narz_replace(L1, T1, L2), !.

narz_reduce(negation(negation(S)), S):-
 	!.

narz_reduce(conjunction([T]), T):-
 	!.
narz_reduce(disjunction([T]), T):-
 	!.

narz_reduce(conjunction([conjunction(L1), conjunction(L2)]), conjunction(L)):-
 	nars_union(L1, L2, L), !.
narz_reduce(conjunction([conjunction(L1), L2]), conjunction(L)):-
 	nars_union(L1, [L2], L), !.
narz_reduce(conjunction([L1, conjunction(L2)]), conjunction(L)):-
 	nars_union([L1], L2, L), !.

narz_reduce(disjunction(disjunction(L1), disjunction(L2)), disjunction(L)):-
 	nars_union(L1, L2, L), !.
narz_reduce(disjunction(disjunction(L1), L2), disjunction(L)):-
 	nars_union(L1, [L2], L), !.
narz_reduce(disjunction(L1, disjunction(L2)), disjunction(L)):-
 	nars_union([L1], L2, L), !.

narz_reduce(X, X).


%nars_union(X,Y,Z):- (nonvar(X);nonvar(Z)), catch(union(X,Y,Z),_,fail).
nars_union(X,Y,Z):- catch(union(X,Y,Z),_,fail).
%nars_subtract(X,Y,Z):- (nonvar(X);nonvar(Z)), catch(subtract(X,Y,Z),_,fail).
nars_subtract(X,Y,Z):- catch(subtract(X,Y,Z),_,fail).

%%% Argument processing

equ_product([], [], []).
equ_product([T|Ls], [T|Lp], L):-
 	equ_product(Ls, Lp, L), !.
equ_product([S|Ls], [P|Lp], [inheritance(S, P)|L]):-
 	equ_product(Ls, Lp, L).

narz_same_set(L1, L2):-
 	L1 \== [], L1 \== [_], narz_same(L1, L2), L1 \== L2.

narz_same([], []).
narz_same(L, [H|T]):-
 	member(H, L), nars_subtract(L, [H], L1), narz_same(L1, T).

narz_include(L1, L2):-
 	narz_ground(L2), include1(L1, L2), L1 \== [], L1 \== L2.

 include1([], _).
 include1([H|T1], [H|T2]):-
 	include1(T1, T2).
 include1([H1|T1], [H2|T2]):-
 	H2 \== H1, include1([H1|T1], T2).

narz_not_member(_, []).
narz_not_member(C, [C|_]):-  !, fail.
narz_not_member([S, T], [[S1, T]|_]):- nars_equivalence(_Ctx, S, S1), !, fail.
narz_not_member(C, [_|L]):- narz_not_member(C, L).

narz_replace([T|L], T, [nil|L]).
narz_replace([H|L], T, [H|L1]):-
 	narz_replace(L, T, L1).

narz_replace([H1|T], H1, [H2|T], H2).
narz_replace([H|T1], H1, [H|T2], H2):-
 	narz_replace(T1, H1, T2, H2).

narz_dependant(var(V, L), Y, var(V, [Y|L])):-
 	!.
narz_dependant([H|T], Y, [H1|T1]):-
 	narz_dependant(H, Y, H1), narz_dependant(T, Y, T1), !.
narz_dependant(inheritance(S, P), Y, inheritance(S1, P1)):-
 	narz_dependant(S, Y, S1), narz_dependant(P, Y, P1), !.
narz_dependant(ext_image(R, A), Y, ext_image(R, A1)):-
 	narz_dependant(A, Y, A1), !.
narz_dependant(int_image(R, A), Y, int_image(R, A1)):-
 	narz_dependant(A, Y, A1), !.
narz_dependant(X, _, X).


%%% Truth-value functions

narz_f_rev([F1, C1], [F2, C2], [F, C]):-
      C1 < 1,
      C2 < 1,
 	M1 is C1 / (1 - C1),
 	M2 is C2 / (1 - C2),
 	F is (M1 * F1 + M2 * F2) / (M1 + M2),
 	C is (M1 + M2) / (M1 + M2 + 1).

narz_f_exp([F, C], E):-
 	E is C * (F - 0.5) + 0.5.

narz_f_neg([F1, C1], [F, C1]):-
 	u_not(F1, F).

narz_f_cnv([F1, C1], [1, C]):-
     u_and([F1, C1], W),
 	u_w2c(W, C).

narz_f_cnt([F1, C1], [0, C]):-
 	u_not(F1, F0),
     u_and([F0, C1], W),
 	u_w2c(W, C).

narz_f_ded([F1, C1], [F2, C2], [F, C]):-
 	u_and([F1, F2], F),
 	u_and([C1, C2, F], C).

narz_f_ana([F1, C1], [F2, C2], [F, C]):-
 	u_and([F1, F2], F),
 	u_and([C1, C2, F2], C).

narz_f_res([F1, C1], [F2, C2], [F, C]):-
 	u_and([F1, F2], F),
 	u_or([F1, F2], F0),
 	u_and([C1, C2, F0], C).

narz_f_abd([F1, C1], [F2, C2], [F2, C]):-
 	u_and([F1, C1, C2], W),
 	u_w2c(W, C).

narz_f_ind(T1, T2, T):-
 	narz_f_abd(T2, T1, T).

narz_f_exe([F1, C1], [F2, C2], [1, C]):-
 	u_and([F1, C1, F2, C2], W),
 	u_w2c(W, C).

narz_f_com([0, _C1], [0, _C2], [0, 0]).
narz_f_com([F1, C1], [F2, C2], [F, C]):-
 	u_or([F1, F2], F0),
 	F0 > 0,
 	F is F1 * F2 / F0,
 	u_and([F0, C1, C2], W),
 	u_w2c(W, C).

narz_f_int([F1, C1], [F2, C2], [F, C]):-
 	u_and([F1, F2], F),
 	u_and([C1, C2], C).

narz_f_uni([F1, C1], [F2, C2], [F, C]):-
 	u_or([F1, F2], F),
 	u_and([C1, C2], C).

narz_f_dif([F1, C1], [F2, C2], [F, C]):-
 	u_not(F2, F0),
 	u_and([F1, F0], F),
 	u_and([C1, C2], C).

narz_f_pnn([F1, C1], [F2, C2], [F, C]):-
 	u_not(F2, F2n),
 	u_and([F1, F2n], Fn),
 	u_not(Fn, F),
 	u_and([Fn, C1, C2], C).

narz_f_npp([F1, C1], [F2, C2], [F, C]):-
 	u_not(F1, F1n),
 	u_and([F1n, F2], F),
 	u_and([F, C1, C2], C).

narz_f_pnp([F1, C1], [F2, C2], [F, C]):-
 	u_not(F2, F2n),
 	u_and([F1, F2n], F),
 	u_and([F, C1, C2], C).

narz_f_nnn([F1, C1], [F2, C2], [F, C]):-
 	u_not(F1, F1n),
 	u_not(F2, F2n),
 	u_and([F1n, F2n], Fn),
 	u_not(Fn, F),
 	u_and([Fn, C1, C2], C).

% Utility functions

u_not(N0, N):-
 	N is (1 - N0), !.

u_and([N], N).
u_and([N0 | Nt], N):-
 	u_and(Nt, N1), N is N0 * N1, !.

u_or([N], N).
u_or([N0 | Nt], N):-
 	u_or(Nt, N1), N is (N0 + N1 - N0 * N1), !.

u_w2c(W, C):-
 	K = 1, C is (W / (W + K)), !.





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%memory.pl

%11.1.2 State accumulation using engines - https://www.swi-prolog.org/pldoc/man?section=engine-state
:- use_module(library(heaps)).

create_heap(E) :- empty_heap(H), engine_create(_, update_heap(H), E).
update_heap(H) :- engine_fetch(Command), ( update_heap(Command, Reply, H, H1) ->  true; H1 = H, Reply = false ), engine_yield(Reply), update_heap(H1).

update_heap(add(Priority, Key), true, H0, H) :- add_to_heap(H0, Priority, Key, H).
update_heap(get(Priority, Key), Priority-Key, H0, H) :- get_from_heap(H0, Priority, Key, H).

heap_add(Priority, Key, E) :- engine_post(E, add(Priority, Key), true).

heap_get(Priority, Key, E) :- engine_post(E, get(Priority, Key), Priority-Key).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%control.pl

priority([_, [F,C]], P) :- narz_f_exp([F, C], E), P is E.

input_event(Event) :- heap_add(1.0, Event, belief_events_queue).

derive_event(Event) :- priority(Event, P), heap_add(P, Event, belief_events_queue).

inference_step(_) :- (heap_get(_Priority, Event, belief_events_queue),
                      heap_get(Priority2, Event2, belief_events_queue),
                      heap_add(Priority2, Event2, belief_events_queue), %undo removal of the second premise (TODO)
                      inference(Event,Event2,Conclusion), 
                      derive_event(Conclusion),
                      write(Conclusion), nl
                     ; true ).

nars_main :- create_heap(belief_events_queue), nars_main(1).
nars_main(T) :- read_nal(X),
     (X = 1, write("performing 1 inference steps:"), nl, 
     inference_step(T), write("done with 1 additional inference steps."), nl,  
       nars_main(T+1) ; 
       X \= 1, write("Input: "), write(X), nl, input_event(X), nars_main(T+1)).

% read_nal(X):- read(X).
read_nal(X):- nal_read_clause(current_input, X).

:- if( prolog_load_context(reload,false)).
:- create_heap(belief_events_queue).
:- endif.

%test:
%nars_main.
%[inheritance(cat,animal), [1.0, 0.9]].
%[inheritance(animal,being), [1.0, 0.9]].
%1.
%output:
%performing 1 inference steps:
%[inheritance(cat,being),[1.0,0.81]]
%done with 1 additional inference steps.


:-  fixup_exports.

