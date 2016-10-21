:- module(
  sparql_csv_result,
  [
    sparql_write_csv_result/3  % +Out, +Result, +Opts
  ]
).

/** <module> Write SPARQL results as CSV

@see  http://www.w3.org/TR/2013/REC-sparql11-results-csv-tsv-20130321/
*/

:- use_module(library(assoc)).
:- use_module(library(csv)).
:- use_module(library(option)).
:- use_module(library(q/q_term)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(sgml_write)).

sparql_csv_mime_type('text/tab-separated-values; charset=UTF-8').

%! sparql_write_csv_result(+Out:stream, +Result, +Opts) is det.
%
% Emit results from a SPARQL SELECT query as CSV.  Opts:
%
%   - bnode_state(State0-State)
%
%     Maintain blank node mapping accross multiple calls.  State0 is
%     either a variable or a state returned by a previous call.
%
%    - http_header(+Boolean)
%
%      If `true` (default), emit an HTTP =Content-type= header.
%
%    - header_row(+Boolean)
%
%      If `true` (default), emit header row with binding names.
%
% @see http://www.w3.org/TR/rdf-sparql-json-res/

sparql_write_csv_result(Out, select(VarTerm, Rows), Opts) :- !,
  option(bnode_state(BNodes0-BNodes), Opts, _),
  (   var(BNodes0)
  ->  empty_assoc(BNodeDict),
      BNodes0 = bnode(1, BNodeDict)
  ;   true
  ),
  rows_to_csv(Rows, CSVRows, BNodes0, BNodes),
  (   option(http_header(true), Opts, true)
  ->  sparql_csv_mime_type(ContentType),
      format('Content-type: ~w~n~n', [ContentType])
  ;   true
  ),
  (   option(header_row(true), Opts, true)
  ->  csv_write_stream(Out, [VarTerm|CSVRows], [])
  ;   csv_write_stream(Out, CSVRows, [])
  ).
sparql_write_csv_result(_Out, Result, _Opts) :- !,
  domain_error(csv_sparql_result, Result).

rows_to_csv([], [], BNodeDict, BNodeDict).
rows_to_csv([H0|T0], [H|T], BNodeDict0, BNodeDict) :-
  row_to_csv(H0, H, BNodeDict0, BNodeDict1),
  rows_to_csv(T0, T, BNodeDict1, BNodeDict).

row_to_csv(RDF, CSV, BNodeDict0, BNodeDict) :-
  RDF =.. [_|RDFFields],
  fields_to_csv(RDFFields, CSVFields, BNodeDict0, BNodeDict),
  CSV =.. [row|CSVFields].

fields_to_csv([], [], BNodeDict, BNodeDict).
fields_to_csv([H0|T0], [H|T], BNodeDict0, BNodeDict) :-
  field_to_csv(H0, H, BNodeDict0, BNodeDict1),
  fields_to_csv(T0, T, BNodeDict1, BNodeDict).

field_to_csv(Var, '', BNodeDict, BNodeDict) :-
  (   var(Var)
  ->  true
  ;   Var == '$null$'
  ), !.
field_to_csv(literal(Literal), Text, BNodeDict, BNodeDict) :-
  q_literal_lex(Literal, Text), !.
field_to_csv(@(LangString,Lang), Text, BNodeDict, BNodeDict) :-
  q_literal_lex(@(LangString,Lang), Text), !.
field_to_csv(^^(Lexical,Type), Text, BNodeDict, BNodeDict) :-
  q_literal_lex(^^(Lexical,Type), Text), !.
field_to_csv(Resource, BNode, BNodeDict0, BNodeDict) :-
  rdf_is_bnode(Resource), !,
  BNodeDict0 = bnode(N0, Dict0),
  (   get_assoc(Resource, Dict0, BNode)
  ->  BNodeDict = BNodeDict0
  ;   succ(N0, N),
      atomic_list_concat(['_:node', N], BNode),
      put_assoc(Resource, Dict0, BNode, Dict),
      BNodeDict = bnode(N, Dict)
  ).
field_to_csv(Atomic, Atomic, BNodeDict, BNodeDict) :-
  atomic(Atomic), !.
field_to_csv(Term, String, BNodeDict, BNodeDict) :-
  term_string(Term, String).



     /*******************************
     *   INTERACTIVE QUERY RESULT  *
     *******************************/

:- multifile
    rdf_io:write_table/4.

rdf_io:write_table(csv, _, Rows, Opts) :-
  memberchk(variables(Vars), Opts), !,
  (   is_list(Vars)
  ->  VarTerm =.. [vars|Vars]
  ;   VarTerm = Vars
  ),
  sparql_write_csv_result(
    current_output,
    select(VarTerm, Rows),
    [content_type(text/plain),Opts]
  ).
