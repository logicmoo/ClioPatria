:- module(
  cp_cli,
  [
    cp_map/2,          % +Point, -Result
    cp_sparql_query/2, % +Query, -Reply
    cp_sparql_query/3, % +Query, -Reply, +Options
    sp2b/1             % +Name
  ]
).

/** <module> ClioPatria: Command-Line Interface (CLI)

@author Wouter Beek
@version 2016/07, 2016/10
*/

:- use_module(library(debug)).
:- use_module(library(http/http_io)).
:- use_module(library(http/json)).
:- use_module(library(os/file_ext)).
:- use_module(library(os/io)).
:- use_module(library(iri/iri_ext)).
:- use_module(library(lists)).

:- use_module(cp(rdfql/sparql_compile)).
:- use_module(cp(rdfql/sparql_run)).





sp2b(Name) :-
  absolute_file_name(
    cpack(sp2b/queries),
    Dir,
    [access(read),file_type(directory)]
  ),
  directory_path(Dir, Path),
  file_name_extension(Name, sparql, Path),
  call_on_stream(Path, sp2b).

sp2b(In, L, L) :-
  read_stream_to_atom(In, Query),
  debug(sparql(query), "QUERY:~n~w", [Query]),
  forall(
    cp_sparql_query(Query, _Reply),
    format(user_output, ".", [])
  ).



cp_map(point(Lng,Lat), Result) :-
  between(1, inf, Page),
  iri_here([map], [lat(Lat),lng(Lng),page(Page)], Iri),
  http_get(
    Iri,
    cp_map0(Result),
    [request_header('Accept'='application/json')]
  ).


cp_map0(Result, In, Meta, Meta) :-
  json_read_dict(In, Result0),
  member(Result, Result0.results).



%! cp_sparql_query(+Query, -Reply) is det.
%! cp_sparql_query(+Query, -Reply, +Options) is det.
%
% Where Query is either a SPARQL query text or a parsed query.  Reply
% depends on the type of query:
%
%    |SELECT    | row(Col1, Col2, ....) |
%    |CONSTRUCT  | rdf(S,P,O) |
%    |DESCRIBE  | rdf(S,P,O) |
%    |ASK    | Reply == true or failure of pred |
%
% The following Options are supported:
%
%    * entailment(Entailment)
%
%      Specify the entailment module used.  The default is controlled
%      by the setting =|sparql:entailment|=.
%
%    * base_uri(Base)
%
%      Specify the base IRI to use for parsing the query
%
%    * type(-Type)
%
%      Returns one of select(-VarNames), construct, describe or ask.
%
%    * ordered(-Bool)
%
%      True if query contains an ORDER BY clause
%
%    * distinct(-Bool)
%
%      True if query contains a DISTINCT clause

cp_sparql_query(Query, Reply) :-
  cp_sparql_query(Query, Reply, []).


cp_sparql_query(Query, Reply, Options) :-
  sparql_compile(Query, Compiled, Options),
  sparql_run(Compiled, Reply).
