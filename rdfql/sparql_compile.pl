/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2005-2012, University of Amsterdam
            VU University Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(
  sparql_compile,
  [
	  sparql_compile/3 % +Query, -Compiled, +Options
  ]
).

/** <module> ClioPatria: SPARQL Query & Update compiler

@author Jan Wielemaker
@author Wouter Beek
@version 2016/10
*/

:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(assoc)).
:- use_module(library(error)).
:- use_module(library(option)).
:- use_module(library(semweb/rdf_optimise)).
:- use_module(library(settings)).

:- use_module(cp(rdfql/sparql_grammar)).
:- use_module(cp(rdfql/sparql_runtime)).





%! sparql_compile(+Query, -Compiled, +Options)
%
% Compiles SPARQL Query and SPARQL Update requests.
%
% Splitting serves two purposes???
%
% The result of the compilation can be cached.
%
% Options provides information about the parsed query.

sparql_compile(
  Query,
  sparql_query(Optimised,ReplyTemplate,Entailment),
  Options
) :-
  (   option(backend(M), Options)
  ->  true
  ;   setting(sparql:backend, M)
  ),
  
  sparql_parse(Query, Parsed, Options),
  debug(sparql(parse), "PARSED:~n~w~n", [Parsed]),
  
  q_rewrite(M, Parsed, Rewritten),
  debug(sparql(rewrite), "REWRITE:~n~w~n", [Rewritten]),

  optimise(Rewritten, Optimised, Options),
  debug(sparql(optimise), "OPTIMISED:~n~w~n", [Optimised]),
  
  (   option(entailment(Entailment), Options)
  ->  true
  ;   setting(sparql:entailment, Entailment)
  ),
  option(type(Type), Options, _),
  option(ordered(Order), Options, _),
  option(distinct(Distinct), Options, _),
  prepare(Parsed, Type, Order, Distinct, ReplyTemplate).


q_rewrite(
  M,
  ask(Projections,Query0,Solutions),
  ask(Projections,Query,Solutions)
) :- !,
  q_rewrite_query(M, Query0, Query).
q_rewrite(
  M,
  select(Projections,Datasets,Query0,Solutions),
  select(Projections,Datasets,Query,Solutions)
) :-
  q_rewrite_query(M, Query0, Query).

q_rewrite_query(_, VAR, VAR) :-
  var(VAR), !.
q_rewrite_query(_, A, A) :-
  atomic(A), !.
q_rewrite_query(M, rdf(S,P,O), q(M,S,P,O)) :- !.
q_rewrite_query(M, rdf(S,P,O,G), q(M,S,P,O,G)) :- !.
q_rewrite_query(M, Comp0, Comp) :-
  Comp0 =.. [Pred|Args0],
  maplist(q_rewrite_query(M), Args0, Args),
  Comp =.. [Pred|Args].


prepare(select(Vars,_,_,S), select(Names), O, D, Reply) :- !,
  select_result(Vars, Reply, Names),
  solutions(S, O, D).
prepare(construct(_,_,_,S), construct, O, D, _) :- !,
  solutions(S, O, D).
prepare(ask(_,_,S), ask, O, D, _) :- !,
  solutions(S, O, D).
prepare(describe(_,_,_,S), describe, O, D, _) :- !,
  solutions(S, O, D).
prepare(update(_), update, false, false, _) :- !.
prepare(Query, Type, _, _, _) :-
  nonvar(Type),
  functor(Type, Expected, _),
  functor(Query, Found, _),
  type_error(query_type(Expected), Found).


%! select_result(+Bindings, -Row, -Names) is det.
%
% Transform the list Bindings of the form Name=Var into a Row term of
% the form row(Col1, Col2, ...)  and a term names(Name1, ...).
%
% For example:
%
% ```prolog
% ?- select_result([x=1,y=2], Row, Names).
% Row = row(1,2), Names = names(x,y)
% ```

select_result(Bindings, Row, Names) :-
  vars_in_bindings(Bindings, Vars, VarNames),
  Names =.. [names|VarNames],
  Row =.. [row|Vars].


vars_in_bindings([], [], []).
vars_in_bindings([Name=Var|T0], [Var|T], [Name|NT]) :-
  vars_in_bindings(T0, T, NT).


solutions(solutions(_Group,_Having,_Aggregate,unsorted,_,_), O) :- !,
  O = false.
solutions(_, true).

solutions(distinct(S), O, true) :- !,
  solutions(S, O).
solutions(S, O, false) :-
  solutions(S, O).


%! optimise(+Parsed, -Optimised, +Options) is det.
%
% Perform sparql query optimization using rdf_optimise/2.
%
% @tbd The UPDATE modify requests involve a query and must be
%      optimized.

optimise(update(Updates), update(Updates), _) :- !.
optimise(Parsed, Optimised, Options) :-
  (   option(optimise(Optimise), Options)
  ->  Optimise == true
  ;   setting(cliopatria:optimise_query, true)
  ),
  prolog_goal(Parsed, Goal0),
  simplify_group(Goal0, Goal1),
  optimise_eval(Goal1, Goal2),
  rdf_optimise(Goal2, Goal3), !,
  bind_null(Goal3, Goal, Options),
  set_prolog_goal(Parsed, Goal, Optimised).
optimise(Parsed, Optimised, Options) :-
  prolog_goal(Parsed, Goal0),
  simplify_group(Goal0, Goal1),
  bind_null(Goal1, Goal, Options),
  set_prolog_goal(Parsed, Goal, Optimised).

%! simplify_group(+In, -Out) is det.
%
% Remove the outer SPARQL group, which has no meaning and reduces
% readability.

simplify_group(sparql_group(G), G) :- !.
simplify_group(sparql_group(G,VIn,VOut), G) :-
  VIn = VOut, !.
simplify_group(Goal, Goal).


bind_null(Goal0, Goal, Options) :-
  option(bind_null(true), Options), !,
  sparql_select_bind_null(Goal0, Goal).
bind_null(Goal, Goal, _).


prolog_goal(select(_Proj, _DataSets, Goal, _Solutions), Goal).
prolog_goal(construct(_Templ, _DataSets, Goal, _Solutions), Goal).
prolog_goal(ask(_DataSets, Goal, _Solutions), Goal).
prolog_goal(describe(_Proj, _DataSets, Goal, _Solutions), Goal).
prolog_goal(sparql_group(Goal), Goal).
prolog_goal(sparql_group(Goal,_VA,_VZ), Goal).


set_prolog_goal(select(Proj, DataSets, _Goal, Solutions), Goal,
  select(Proj, DataSets, Goal, Solutions)).
set_prolog_goal(construct(Templ, DataSets, _Goal, Solutions), Goal,
  construct(Templ, DataSets, Goal, Solutions)).
set_prolog_goal(ask(DataSets, _Goal, Solutions), Goal,
  ask(DataSets, Goal, Solutions)).
set_prolog_goal(describe(Proj, DataSets, _Goal, Solutions), Goal,
  describe(Proj, DataSets, Goal, Solutions)).
set_prolog_goal(sparql_group(_Goal), Goal, Goal).
set_prolog_goal(sparql_group(_Goal,VA,VZ), Goal, (Goal,VA=VZ)).


%! optimise_eval(+Goal0, -Goal) is det.
%
% Perform partial evaluation on sparql_true/1 and sparql_eval/2 goals.

optimise_eval(GoalIn, GoalOut) :-
  annotate_variables(GoalIn, Vars),
  optimise_annotated(GoalIn, GoalOut),
  unbind_variables(Vars).


%! annotate_variables(+Goal, -Vars) is det.
%
% Annotate variables that appear in Goal.  The annotation is a
% variable attribute named `annotations` and the value of this
% attribute is a list of annotations.

annotate_variables(Goal, Vars) :-
  empty_assoc(Vars0),
  annotate_vars(Goal, Vars0, Vars).


annotate_vars(Var, _, _) :-
  var(Var), !,
  instantiation_error(Var).
annotate_vars((A,B), Vars0, Vars) :- !,
  annotate_vars(A, Vars0, Vars1),
  annotate_vars(B, Vars1, Vars).
annotate_vars((A;B), Vars0, Vars) :- !,
  annotate_vars(A, Vars0, Vars1),
  annotate_vars(B, Vars1, Vars).
annotate_vars((A*->B), Vars0, Vars) :- !,
  annotate_vars(A, Vars0, Vars1),
  annotate_vars(B, Vars1, Vars).
annotate_vars(sparql_group(G), Vars0, Vars) :- !,
  annotate_vars(G, Vars0, Vars).
annotate_vars(sparql_group(G, _, _), Vars0, Vars) :- !,
  annotate_vars(G, Vars0, Vars).
annotate_vars(rdf(S,P,_), Vars0, Vars) :- !,
  annotate_var(S, resource, Vars0, Vars1),
  annotate_var(P, resource, Vars1, Vars).
annotate_vars(rdf(S,P,_,G), Vars0, Vars) :- !,
  annotate_var(S, resource, Vars0, Vars1),
  annotate_var(P, resource, Vars1, Vars2),
  annotate_var(G, resource, Vars2, Vars).
annotate_vars(_, Vars, Vars).


annotate_var(V, Type, Vars0, Vars) :-
  var(V),
  (   get_attr(V, annotations, A0)
  ->  \+ memberchk(Type, A0)
  ;   A0 = []
  ), !,
  put_attr(V, annotations, [Type|A0]),
  put_assoc(V, Vars0, true, Vars).
annotate_var(_, _, Vars, Vars).


unbind_variables(VarAssoc) :-
  assoc_to_keys(VarAssoc, VarList),
  maplist(unbind_var, VarList).


unbind_var(V) :-
  del_attr(V, annotations).


%! optimise_eval(+GoalIn, -GoalOut)

optimise_annotated((A0,B0), (A,B)) :- !,
  optimise_annotated(A0, A),
  optimise_annotated(B0, B).
optimise_annotated((A0;B0), (A;B)) :- !,
  optimise_annotated(A0, A),
  optimise_annotated(B0, B).
optimise_annotated((A0*->B0), (A*->B)) :- !,
  optimise_annotated(A0, A),
  optimise_annotated(B0, B).
optimise_annotated(sparql_group(G0), sparql_group(G)) :- !,
  optimise_annotated(G0, G).
optimise_annotated(sparql_group(G0,OV,IV), sparql_group(G,OV,IV)) :- !,
  optimise_annotated(G0, G).
optimise_annotated(sparql_true(E), G) :- !,
  sparql_simplify(sparql_true(E), G).
optimise_annotated(sparql_eval(E,V), G) :- !,
  sparql_simplify(sparql_eval(E,V), G).
optimise_annotated(G, G).
