:- module(sparql_query, []).

/** <module> Quine: SPARQL Query endpoint

@author Jan Wielemaker
@author Wouter Beek
@version 2016/09-2016/10
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(html/html_ext)).
:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http11)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_cors)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_ext)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_server_files)).
:- use_module(library(http/js_write)).
:- use_module(library(http/rest)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(rdf_write)).
:- use_module(library(settings)).
:- use_module(library(uri)).

:- use_module(cp(applications/sparql_editor)).
:- use_module(cp(http_parms)).
:- use_module(cp(rdfql/sparql_reply)).
:- use_module(cp(rdfql/sparql_results)).
:- use_module(cp(skin/cliopatria)).
:- use_module(cp(user/user_db)).

:- multifile
    http_param/1,
    media_type/1.

http_param('default-graph-uri').
http_param(entailment).
http_param('named-graph-uri').
http_param(query).

:- http_handler(
     cliopatria(sparql),
     sparql_query_handler,
     [spawn(sparql_query_handler)]
   ).

html:menu_item(query, 1, sparql_query_handler, "SPARQL Query").





%! sparql_query_handler(+Req) is det.
%
% HTTP handler for SPARQL requests.
%
% As part of a SPARQL request the user may specify the following things:
%
%   * `default-graph`
%
%     The default graph as specified by the SPARQL dataset structure,
%     against which the query is evaluated.  Zero or more default
%     graphs may be specified.
%
%   * `named-graph'
%
%     The named graphs as specified by the SPARQL dataset structure,
%     against which the query is evaluated.  Zero or more named graphs
%     may be specified.
%
%   * `query`
%
%     The contents of the SPARQL query.  Exactly one must occur in
%     every SPARQL query request.
%
% There are three ways of posing a SPARQL query:
%
%  1. An HTTP GET request where `query`, `default-graph` and
%     `named-graph` appear in the IRI's search string and are all
%     subject to IRI-encoding.  Example: `curl
%     http://localhost:3020/sparql/?query=select%20*%20where%20%7B%20%3Fs%20%3Fp%20%3Fo%20%7D`.
%     No Content-Type needs to be specified.
%
%  2. An HTTP POST request where `query`, `default-graph` and
%     `named-graph` appear in the POST body using IRI search string
%     syntax and subject to IRI-encoding.  Example: `curl --data
%     "query=select * where { ?s ?p ?o }"
%     http://localhost:3020/sparql/`.  The Content-Type must be
%     `application/x-www-form-urlencoded`.
%
%  3. An HTTP POST request where `default-graph` and `named-graph`
%     appear in the IRI's search string and are subject to
%     IRI-encoding and where the query appears as-is in the POST body.
%     Example: `curl -X POST -H "Content-Type:
%     application/sparql-query" -d @query.sparql
%     http://localhost:3020/sparql/`.  The Content-Type must be
%     `application/sparql-query`.

sparql_query_handler(Req) :-
  rest_method(Req, [get,post], sparql_query_method).


sparql_query_method(Req, Method, MTs) :-
  (   memberchk(content_type(A), Req)
  ->  http_parse_header('content-type', A, ContentType)
  ;   true
  ),
  rest_media_type(
    Req,
    Method,
    MTs,
    sparql_query_media_type(Req, ContentType)
  ).


sparql_query_media_type(Req, _, get, text/html) :-
  sparql_editor(Req).
% Perform a SPARQL query via GET.
% @compat SPARQL 1.1 Protocol recommendation, section 2.1.1.
sparql_query_media_type(Req, _, get, MT) :-
  sparql_query_media_type_params(MT, Req).
% Perform a SPARQL query via POST with unencoded body.
% @compat SPARQL 1.1 Protocol recommendation, section 2.1.3.
sparql_query_media_type(
  Req,
  media_type("application","sparql-query",_),
  post,
  MT
) :-
  http_parameters(
    Req,
    [
      'default-graph-uri'(DefaultGs),
      entailment(Entailment),
      'named-graph-uri'(NamedGs)
    ],
    [attribute_declarations(http_param(sparql_query))]
  ),
  http_read_data(Req, Query, []),
  sparql_media_type(MT, Query, Entailment, DefaultGs, NamedGs).
% Perform a SPARQL query via POST with encoded parameters in body.
% @compat SPARQL 1.1 Protocol recommendation, section 2.1.2.
sparql_query_media_type(
  Req,
  media_type("application","x-www-form-urlencoded",_),
  post,
  MT
) :-
  sparql_query_media_type_params(MT, Req).


%! sparql_query_media_type_params(+MT, +Req) is det.
%
% Both GET and POST requests can contain HTTP parameters in the
% request URI.

sparql_query_media_type_params(MT, Req) :-
  http_parameters(
    Req,
    [
      'default-graph-uri'(DefaultGs),
      entailment(Entailment),
      'named-graph-uri'(NamedGs),
      query(Query)
    ],
    [attribute_declarations(http_param(sparql_query))]
  ),
  sparql_media_type(MT, Query, Entailment, DefaultGs, NamedGs).
