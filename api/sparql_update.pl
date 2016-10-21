:- module(sparql_update, []).

/** <module> Quine: SPARQL Update endpoint

@author Jan Wielemaker
@author Wouter Beek
@version 2016/09-2016/10
*/

:- use_module(library(http/http_client)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_ext)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http11)).
:- use_module(library(lists)).

:- use_module(cp(applications/sparql_editor)).
:- use_module(cp(http_parms)).
:- use_module(cp(rdfql/sparql_reply)).

:- http_handler(
     cliopatria(sparql/update),
     sparql_update_handler,
     [spawn(sparql_update_handler)]
   ).

html:menu_item(query, 3, sparql_update_handler, "SPARQL Update").

:- multifile
    http_param/1,
    media_type/1.

http_param(entailment).
http_param(update).
http_param('using-graph-uri').
http_param('using-named-graph-uri').





%! sparql_update(+Req) is det.
%
% HTTP handler for SPARQL update requests.  This is the same as query
% requests, but the takes the query in the `update` field rather than
% in the `query` field.

sparql_update(Req) :-
  rest_method(Req, [get,post], sparql_update_method).


sparql_update_method(Req, Method, MTs) :-
  memberchk(content_type(A), Req),
  http_parse_header('content-type', A, ContentType),
  rest_media_type(
    Req,
    Method,
    MTs,
    sparql_update_media_type(Req, ContentType)
  ).


sparql_update_media_type(Req, _, get, text/html) :-
  sparql_editor(Req).
% Perform a SPARQL update via POST directly.
% @compat SPARQL 1.1 Protocol recommendation, section 2.2.2.
sparql_update_media_type(
  Req,
  media_type("application","sparql-update",_),
  post,
  MT
) :-
  http_parameters(
    Req,
    [
      entailment(Entailment),
      'using-graph-uri'(DefaultGs),
      'using-named-graph-uri'(NamedGs)
    ],
    [attribute_declarations(http_param(sparql_update_endpoint))]
  ),
  http_read_data(Req, Query, []),
  sparql_media_type(MT, Query, Entailment, DefaultGs, NamedGs).
% Perform a SPARQL update via POST with URL-encoded parameters.
% @compat SPARQL 1.1 Protocol recommendation, section 2.2.1.
sparql_update_media_type(
  Req,
  media_type("application","x-www-form-urlencoded",_),
  post,
  MT
) :-
  http_parameters(
    Req,
    [
      entailment(Entailment),
      update(Query),
      'using-graph-uri'(DefaultGs),
      'using-named-graph-uri'(NamedGs)
    ],
    [attribute_declarations(http_param(sparql_update_endpoint))]
  ),
  sparql_media_type(MT, Query, Entailment, DefaultGs, NamedGs).
