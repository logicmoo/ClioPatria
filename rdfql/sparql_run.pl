:- module(
  sparql_run,
  [
    sparql_run/2 % +CompiledQuery, -Reply
  ]
).

/** <module> ClioPatria: Running a compiled SPARQL Query or Update request

@author Jan Wielemaker
@author Wouter Beek
@version 2016/10
*/

:- use_module(library(assoc)).
:- use_module(library(q/q_rdf)).
:- use_module(library(q/q_term)).

:- use_module(cp(rdfql/rdfql_runtime)).
:- use_module(cp(rdfql/rdfql_util)).
:- use_module(cp(rdfql/sparql_runtime)).





%! sparql_run(+Compiled, -Reply) is nondet.
%
% Runs a compiled SPARQL query, returning the result incrementally on
% backtracking.  Provided there are no errors in the SPARQL
% implementation the only errors this can produce are resource-related
% errors.

sparql_run(sparql_query(Parsed,Reply,Entailment), Reply) :-
  sparql_reset_bnodes,
  sparql_run(Parsed, Reply, Entailment).


sparql_run(select(_Vars,_DataSets,Query,Solutions), Reply, _Entailment) :-
  select_results(Solutions, Reply, Query).
sparql_run(construct(Triples,_DataSets,Query,Solutions), Reply, _Entailment) :-
  select_results(
    Solutions,
    Reply,
    (Query, rdfql_triple_in(Reply, Triples))
  ).
sparql_run(ask(_DataSets,Query,_Solutions), Result, _Entailment) :-
  (Query -> Result = true ; Result = false).
sparql_run(describe(IRIs, _DataSets, Query, Solutions), Reply, _Entailment) :-
  select_results(Solutions, Reply, (Query, member(IRI, IRIs))),
  setting(sparql:backend, M),
  sparql_describe(IRI, M, Reply).
sparql_run(update(Updates), Result, _Entailment) :-
  (sparql_update(Updates) -> Result = true ; Result = false).


%! select_results(+Spec, -Reply, :Goal)
%
% Apply ordering and limits on result-set.
%
% @tbd Handle =reduced=

:- meta_predicate select_results(+,+,0).
:- public select_results/3.    % used on sparql_subquery/4

select_results(
  distinct(solutions(Group,Having,Agg,Order,Limit,Offset)),
  Reply,
  Goal
) :- !,
  select_results(
    distinct,
    Group,
    Having,
    Agg,
    Offset,
    Limit,
    Order,
    Reply,
    Goal
  ).
select_results(reduced(Solutions), Reply, Goal) :- !,
  select_results(Solutions, Reply, Goal).
select_results(solutions(Group,Having,Agg,Order,Limit,Offset), Reply, Goal) :-
  select_results(all, Group, Having, Agg, Offset, Limit, Order, Reply, Goal).


%! sparql_describe(+IRI, -Triple)
%
% Return -on backtracking- triples that describe IRI.  The
% documentation does not specify which triples must be returned for a
% description.  As a way to get started we simply return all direct
% properties.

sparql_describe(_Var=IRI, M, Triple) :- !,
  sparql_describe(IRI, M, Triple).
sparql_describe(IRI, M, Triple) :-
  empty_assoc(Seen),
  sparql_describe(IRI, M, Triple, Seen).


sparql_describe(IRI, M, Triple, Seen) :-
  q(M, IRI, P, O),
  (   q_is_bnode(O),
      \+ get_assoc(O, Seen, true)
  ->  (   Triple = rdf(IRI,P,O)
      ;   put_assoc(O, Seen, true, Seen2),
          sparql_describe(O, M, Triple, Seen2)
      )
  ;   Triple = rdf(IRI,P,O)
  ).
