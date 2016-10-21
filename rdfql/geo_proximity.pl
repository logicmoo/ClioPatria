:- module(geo_proximity, []).

/** <module> ClioPatria Geo: Proximity queries

@author Wouter Beek
@version 2016/06-2016/10
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(atom_ext)).
:- use_module(library(date_time/date_time)).
:- use_module(library(default)).
:- use_module(library(dict_ext)).
:- use_module(library(gen/gen_ntuples)).
:- use_module(library(gis/gis)).
:- use_module(library(html/html_doc)).
:- use_module(library(html/html_ext)).
:- use_module(library(html/html_map)).
:- use_module(library(html/qh_ui)).
:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_ext)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_path)).
:- use_module(library(http/http_write)).
:- use_module(library(http/json)).
:- use_module(library(http/rest)).
:- use_module(library(json_ext)).
:- use_module(library(jsonld/geold)).
:- use_module(library(jsonld/jsonld_build)).
:- use_module(library(jsonld/jsonld_generics)).
:- use_module(library(lists)).
:- use_module(library(pagination)).
:- use_module(library(pair_ext)).
:- use_module(library(q/q_rdf)).
:- use_module(library(q/q_shape)).
:- use_module(library(q/q_term)).
:- use_module(library(q/qb)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(settings)).
:- use_module(library(yall)).

:- use_module(cp(http_parms)).
:- use_module(cp(skin/cliopatria)).

:- http_handler(cp(geo/proximity), geo_proximity_handler, [prefix]).

:- multifile
    gis:subject_to_geometry/5,
    http_param/1,
    media_type/1.

http_param(graph).
http_param(page).
http_param(page_size).
http_param(properties).

media_type(application/'ld+json').
media_type(application/'vnd.geo+json').
media_type(application/'n-quads').
media_type(application/'n-triples').
media_type(text/html).

:- setting(
     backend,
     oneof([hdt,trp]),
     hdt,
     "The backend that is used to populate the Geo API with."
   ).
:- setting(
     default_graph,
     atom,
     '',
     "The default RDF graph from which geometric entities are retrieved."
   ).
:- setting(
     default_page_size,
     positive_integer,
     20,
     "The default number of geographic entities that is retreived in one request."
   ).
:- setting(
     max_page_size,
     positive_integer,
     10,
     "The maximum number of objects that can be retrieved in one request."
   ).





geo_proximity_handler(Req) :-
  rest_method(Req, [get], geo_proximity_method).


geo_proximity_method(Req, get, MTs) :- !,
  setting(backend, M),
  http_parameters(
    Req,
    [
      graph(G),
      lat(Lat),
      lng(Lng),
      page(Page),
      page_size(PageSize),
      properties(Properties),
      zoom(Zoom)
    ],
    [attribute_declarations(geo_proximity_param)]
  ),
  http_location_iri(Req, Iri),
  PageOpts = _{iri: Iri, page: Page, page_size: PageSize},
  Point = point(Lng,Lat),
  (   ground(Point),
      gis_exists
  ->  once(pagination(S, geo_proximity_item(Point, G, S), PageOpts, Result))
  ;   pagination_empty(PageOpts, Result)
  ),
  rest_media_type(
    Req,
    get,
    MTs,
    geo_proximity_result(M, G, Properties, Point, Zoom, Result)
  ).


geo_proximity_param(
  properties,
  [
    boolean,
    default(false),
    description("Whether properties of the retrieved entities are included or not.")
  ]
).
geo_proximity_param(Key, Spec) :-
  http_param(Key),
  http:http_param(geo_proximity_endpoint, Key, Spec).
geo_proximity_param(Key, Spec) :-
  map_param(Key, Spec).



%! geo_proximity_item(+Point, ?G, -S) is nondet.

geo_proximity_item(Point, G, S) :-
  distinct(S, gis_nearest(Point, S, G)).



%! geo_proximity_result(+M, +G, +Properties, +Point, +Zoom, +Result, +MT) is det.

geo_proximity_result(
  M, G, Properties, _, _, Result, get, application/'ld+json'
) :-
  geo_proximity_result_jsonld_properties(M, G, Properties, Result, Triples),
  triples_to_jsonld(M, Triples, Dict),
  http_pagination_links(Result),
  reply_jsonld(Dict).
geo_proximity_result(
  M, G, Properties, _, _, Result, get, application/'vnd.geo+json'
) :-
  maplist(subject_to_geojson(M, Properties, G), Result.results, Features),
  geojson_feature_collection(Features, FeatureCollection),
  http_pagination_links(Result),
  reply_geojson(FeatureCollection).
geo_proximity_result(M, G, _, _, _, Result, get, MT) :-
  ntuples_media_type(MT, Format),
  findall(
    Tuple,
    (
      member(S, Result.results),
      q_quad(M, S, G, Tuple)
    ),
    Tuples
  ),
  http_content_type(MT),
  http_pagination_links(Result),
  http_end_of_header,
  Opts = [compression(false),rdf_format(Format)],
  call_to_ntuples(current_output, gen_ntuples(Tuples), Opts).
geo_proximity_result(_, _, _, CenterPoint, Zoom, Result, get, text/html) :-
  http_link_to_id(geo_proximity_handler, Endpoint),
  geojson_feature_collection(Result.results, FeatureCollection),
  reply_html_page(
    cliopatria([class='container-with-map']),
    [
      \pagination_links(Result),
      \cp_title(["Geo","Proximity"])
    ],
    [
      \html_post(q_navbar_right, \qh_graph_menu(gis)),
      \map(Endpoint, CenterPoint, Zoom, FeatureCollection),
      \html_requires(map)
    ]
  ).


geo_proximity_result_jsonld_properties(M, G, false, Result, Triples) :- !,
  findall(
    Triple,
    (
      member(S, Result.results),
      q_triple(M, S, geold:geometry, _, G, Triple)
    ),
    Triples
  ).
geo_proximity_result_jsonld_properties(M, G, true, Result, Triples) :-
  findall(
    Triple,
    (
      member(S, Result.results),
      q_cbd_triple(M, S, G, Triple)
    ),
    Triples
  ).





% DOC %

html_doc:section(geo_doc).

html_doc:geo_doc -->
  {
    http_link_to_id(geo_handler, GeoLoc),
    http_absolute_uri(GeoLoc, GeoIri)
  },
  html([
    header(h2(["Geo API (",\endpoint_link(geo_handler),")"])),
    section([
      \row_1(
        p("The Geo API allows all geographic data to be queried at the
        entry level.  Queries return collections of entities that have
        certain geographic properties.  All geographic entities also
        occur in the Graph API (see above).  Once en entity IRI is
        obtained through the Geo API, the Graph API can be used to
        retrieve more information about that entity.")
      ),
      h3("Parameters"),
      \row_1(\param_table(geo)),
      \row_1([
        p("Notice that all parameters are optional, so the following
        is a valid request:"),
        \code_link(["curl ",GeoIri])
      ]),
      \row_1([
        p("The following request enumerates all statements about a
        specific municipality (‘gemeente’):"),
        \code_link([
          "curl ",
          GeoIri,
          "?term=http%3A//geonovum.triply.cc/id/gemeente/GM0003"
        ])
      ]),
      h3("Formats"),
      \row_1(\media_type_table(geo_proximity_endpoint))
    ])
  ]).





% MESSAGE %

:- multifile
    prolog:message//1.

prolog:message(error(existence_error(key,Key,_),_)) -->
  ["Key ‘~a’ is missing from your request."-[Key]].
