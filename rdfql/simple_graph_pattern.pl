:- module(simple_graph_pattern, []).

/** <module> Quine Graph: Simple Graph Pattern (SGP) endpoint

@author Wouter Beek
@version 2016/06-2016/10
*/

:- use_module(library(dict_ext)).
:- use_module(library(gen/gen_ntuples)).
:- use_module(library(hdt/hdt_ext)).
:- use_module(library(html/html_doc)).
:- use_module(library(html/html_ext)).
:- use_module(library(html/qh)).
:- use_module(library(html/qh_ui)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_ext)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_path)).
:- use_module(library(http/rest)).
:- use_module(library(jsonld/jsonld_build)).
:- use_module(library(jsonld/jsonld_generics)).
:- use_module(library(pagination)).
:- use_module(library(pair_ext)).
:- use_module(library(q/q_rdf)).
:- use_module(library(q/q_shape)).
:- use_module(library(q/q_term)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(settings)).

:- use_module(cp(http_parms)).
:- use_module(cp(skin/cliopatria)).

:- http_handler(cp(sgp), sgp_handler, [prefix]).

:- multifile
    html_doc:section/1,
    http_param/1,
    media_type/1.

http_param(dataset).
http_param(graph).
http_param(object).
http_param(page).
http_param(page_size).
http_param(predicate).
http_param(subject).

media_type(application/'ld+json').
media_type(application/'n-quads').
media_type(application/'n-triples').
media_type(text/html).

:- setting(
     backend,
     oneof([any,hdt,trp]),
     any,
     "The backend that is used to populate the graph browser."
   ).
:- setting(
     default_page_size,
     positive_integer,
     20,
     "The default number of tuples that is retreived in one request."
   ).
:- setting(
     max_page_size,
     positive_integer,
     100,
     "The maximum number of tuples that can be retrieved in one request."
   ).





sgp_handler(Req) :-
  rest_method(Req, [get], sgp_method).


sgp_method(Req, get, MTs) :-
  setting(backend, M),
  http_parameters(
    Req,
    [
      dataset(D0),
      graph(G0),
      object(O0),
      page(Page),
      page_size(PageSize),
      predicate(P0),
      subject(S0)
    ],
    [attribute_declarations(http_param(simple_graph_pattern))]
  ),
  maplist(q_term_expansion, [D0,G0,O0,P0,S0], [D,G,O,P,S]),
  http_location_iri(Req, Iri),
  include(
    ground,
    [dataset(D),graph(G),object(O),predicate(P),subject(S)],
    Query0
  ),
  maplist(q_query_term, Query0, Query),
  PageOpts = _{iri: Iri, page: Page, page_size: PageSize, query: Query},
  pagination(rdf(S,P,O,G), q(M, S, P, O, G, D), PageOpts, Pagination),
  rest_media_type(Req, get, MTs, sgp_media_type(Pagination)).


sgp_media_type(Result, get, application/'ld+json') :-
  setting(backend, M),
  quads_to_jsonld(M, Result.results, Dicts),
  http_pagination_links(Result),
  reply_jsonld(Dicts).
sgp_media_type(Result, get, MT) :-
  ntuples_media_type(MT, Format),
  http_content_type(MT),
  http_pagination_links(Result),
  http_end_of_header,
  Opts = [compression(false),rdf_format(Format)],
  call_to_ntuples(current_output, gen_ntuples(Result.results), Opts).
sgp_media_type(Result, get, text/html) :-
  %ignore(memberchk(dataset(D), Result.query)),
  %ignore(memberchk(graph(G), Result.query)),
  reply_html_page(
    cliopatria([]),
    [
      \pagination_links(Result),
      \cp_title(["Simple Graph Pattern"])
    ],
    html([
      %\qh_dataset_graph_menu(sgp_handler, M, D, G),
      \pagination_result(Result, qh_ui:qh_quad_table)
    ])
  ).





% DOC %

html_doc:section(sgp_doc).

html_doc:sgp_doc -->
  {
    http_link_to_id(sgp_handler, GraphLoc),
    http_absolute_uri(GraphLoc, GraphIri)
  },
  html([
    header(h2(["Graph API (",\endpoint_link(sgp_handler),")"])),
    section([
      \row_1(
        p("The Graph API allows all data to be queried at the triple
           level.  The API is consistent with the Linked Data Fragments
           (LDF) specification.")
      ),
      h3("Parameters"),
      \row_1(\param_table(sgp)),
      \row_1([
        p("Notice that all parameters are optional, so the following is
           a valid request:"),
        \code_link(["curl ",GraphIri])
      ]),
      \row_1([
        p("The following request enumerates all statements about a
           specific municipality (‘gemeente’):"),
        \code_link([
          "curl ",
          GraphIri,
          "?subject=http%3A//geonovum.triply.cc/id/gemeente/0003"
        ])
      ]),
      h3("Formats"),
      \row_1(\media_type_table(sgp_media_type))
    ])
  ]).
