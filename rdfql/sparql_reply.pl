:- module(
  sparql_reply,
  [
    sparql_media_type/5 % +MT, +Query, +Entailment, +DefaultGs, +NamedGs
  ]
).

/** <module> ClioPatria: SPARQL Query & Update reply

@author Jan Wielemaker
@author Wouter Beek
@version 2016/10
*/

:- use_module(library(http/http11)).
:- use_module(library(lists)).

:- use_module(cp(rdfql/sparql_compile)).
:- use_module(cp(rdfql/sparql_results)).
:- use_module(cp(rdfql/sparql_run)).
:- use_module(cp(user/user_db)).





%! sparql_media_type(+MT, +Query, +Entailment, +DefaultGs, +NamedGs) is det.

sparql_media_type(MT, Query, Entailment, DefaultGs, NamedGs) :-
  append(DefaultGs, NamedGs, Gs),
  statistics(cputime, Cpu1),
  sparql_compile(
    Query,
    Compiled,
    [
      distinct(Distinct),
      entailment(Entailment),
      ordered(Ordered),
      type(Type)
    ]
  ),
  (   Compiled = sparql_query(update(_), _, _)
  ->  authorized(write(Gs, sparql))
  ;   authorized(read(Gs, sparql))
  ),
  findall(Row, sparql_run(Compiled, Row), Rows),
  statistics(cputime, Cpu2),
  DeltaCpu is Cpu2 - Cpu1,
  sparql_write_result(
    MT,
    Type,
    Rows,
    [cputime(DeltaCpu),distinct(Distinct),ordered(Ordered)]
  ).
