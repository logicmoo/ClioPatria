:- module(
  sparql_results,
  [
    sparql_write_result/4 % +MT, +Type, +Rows, +Opts
  ]
).

/** <module> SPARQL results

@author Jan Wielemaker
@author Wouter Beek
@version 2016/09
*/

:- use_module(library(http/http_cors)).
:- use_module(library(http/http_ext)).

:- use_module(cp(rdfql/sparql_csv_result)).
:- use_module(cp(rdfql/sparql_json_result)).
:- use_module(cp(rdfql/sparql_xml_result)).





sparql_write_result(MT, Type, Rows, Opts) :-
  cors_enable,
  sparql_write_result_cors_enabled(MT, Type, Rows, Opts).

sparql_write_result_cors_enabled(
  application/'sparql-results+xml',
  ask,
  [True],
  Opts
) :-
  http_content_type(application/'sparql-results+xml'),
  http_end_of_header,
  sparql_write_xml_result(current_output, ask(True), Opts).
sparql_write_result_cors_enabled(
  application/'sparql-results+xml',
  update,
  [True],
  Opts
) :-
  http_content_type(application/'sparql-results+xml'),
  http_end_of_header,
  sparql_write_xml_result(current_output, update(True), Opts).
sparql_write_result_cors_enabled(
  application/'sparql-results+xml',
  select(VarNames),
  Rows,
  Opts
) :-
  format("Transfer-encoding: chunked~n"),
  http_content_type(application/'sparql-results+xml'),
  http_end_of_header,
  sparql_write_xml_result(current_output, select(VarNames, Rows), Opts).
sparql_write_result_cors_enabled(
  application/'sparql-results+xml',
  _,
  RDF,
  _
) :-
  http_content_type(application/'rdf+xml'),
  http_end_of_header,
  rdf_write_xml(current_output, RDF).
sparql_write_result_cors_enabled(
  application/'sparql-results+json',
  ask,
  [True],
  Opts
) :-
  sparql_write_json_result(current_output, ask(True), Opts).
sparql_write_result_cors_enabled(
  application/'sparql-results+json',
  select(VarNames),
  Rows,
  Opts
) :-
  format("Transfer-encoding: chunked~n"),
  sparql_write_json_result(current_output, select(VarNames, Rows), Opts).
sparql_write_result_cors_enabled(
  text/'tab-separated-values',
  select(VarNames),
  Rows,
  Opts
) :-
  format("Transfer-encoding: chunked~n"),
  sparql_write_csv_result(current_output, select(VarNames, Rows), Opts).
