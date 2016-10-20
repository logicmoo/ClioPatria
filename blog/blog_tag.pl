:- module(
  blog_tag,
  [
  % API
    assert_tag/4,         % +M, +Tag, +Lbl, +G
    resource_tags/4,      % +M, +G, +Resource, -Tags
    tag_resources/5,      % +M, +G, +Tag, +C, -Resources
    tags_by_popularity/3, % +M, +G, -Tags
    tags_by_popularity/4, % +M, +G, +MaxNumTags, -Tags
  % HTML
    resource_tags//3, % +M, +Resource, +G
    tag//3,           % +M, +G, +Tag
    tag_list//3,      % +M, +G, +Tags
    tag_menu//4,      % +M, +G, +MaxNumTags, +Context
    tag_menu_items//4 % +M, +G, +MaxNumTags, +Context
  ]
).

/** <module> ClioPatria blog: Tags

@author Wouter Beek
@version 2016/10
*/

:- use_module(library(aggregate)).
:- use_module(library(html/html_ext)).
:- use_module(library(html/qh)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/rest)).
:- use_module(library(iri/iri_ext)).
:- use_module(library(jsonld/jsonld_build)).
:- use_module(library(list_ext)).
:- use_module(library(nlp/nlp_lang)).
:- use_module(library(pair_ext)).
:- use_module(library(q/q_iri)).
:- use_module(library(q/q_rdf)).
:- use_module(library(q/q_rdfs)).
:- use_module(library(q/q_stat)).
:- use_module(library(q/q_term)).
:- use_module(library(q/qb)).
:- use_module(library(rdfa/rdfa_ext)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(settings)).
:- use_module(library(solution_sequences)).

:- use_module(cp(blog/blog_generic)).
:- use_module(cp(blog/blog_tag)).

:- http_handler(cp(tag), tag_handler, [prefix]).

:- rdf_meta
   assert_tag(+, r, +, r),
   resource_tags(+, r, r, -),
   tag_resources(+, r, r, r, -),
   tags_by_popularity(+, r, -),
   tags_by_popularity(+, r, +, -).





% ENDPOINT %

%! tag_handler(+Req) is det.

tag_handler(Req) :-
  setting(blog:backend, M),
  q_graph_iri(blog, G),
  rest_method(
    Req,
    [get],
    tag_plural_method(M, G),
    tag_handler,
    tag_singular_method(M, G)
  ).


tag_plural_method(M, G, Req, Method, MTs) :-
  rest_media_type(Req, Method, MTs, tag_plural_media_type(M, G)).


tag_plural_media_type(M, G, get, application/'ld+json') :-
  q_triples(M, _, rdf:type, sioc:'Tag', G, Triples),
  triples_to_jsonld(M, Triples, G, Dict),
  reply_json_dict(Dict).
tag_plural_media_type(M, G, get, text/html) :-
  tags_by_popularity(M, G, Tags),
  maplist(lstring, [tag,overview], Strs),
  reply_html_page(
    tag(_),
    \blog_title(Strs),
    \tag_list(M, G, Tags)
  ).


tag_singular_method(M, G, Tag, Req, get, MTs) :-
  \+ q_instance(M, Tag, sioc:'Tag', G), !,
  rest_exception(Req, MTs, 404).
tag_singular_method(M, G, Tag, Req, Method, MTs) :-
  rest_media_type(Req, Method, MTs, tag_singular_media_type(M, G, Tag)).


tag_singular_media_type(M, G, Tag, get, application/'ld+json') :-
  subject_to_jsonld(M, Tag, G, Dict),
  reply_json_dict(Dict).
tag_singular_media_type(M, G, Tag, get, text/html) :-
  once(q_pref_label(M, Tag, Lbl, G)),
  lstring(tag, Str1),
  q_literal_string(Lbl, Str2),
  reply_html_page(
    tag(Tag),
    \blog_title([Str1,Str2]),
    article([
      header(
        h1([
          \lstring(articles_tagged_with),
          " ",
          \quote(\qh_literal(Lbl)),
          ":"
        ])
      ),
      \tag(M, _, Tag)
    ])
  ).





% API %

%! assert_tag(+M, +Tag, +Lbl, +G) is det.

assert_tag(M, Tag, Lbl, G) :-
  qb_instance(M, Tag, sioc:'Tag', G),
  qb(M, Tag, rdfs:label, Lbl, G).



%! resource_tags(+M, +G, +Resource, -Tags) is det.
%
% Returns the Tags associates with Resource.

resource_tags(M, G, Resource, Tags) :-
  aggregate_all(
    set(Tag),
    (
      dc_subject(M, Resource, Tag, G),
      % Filter for tags that we have minted ourselves.
      q_abox_iri(tag, Tag)
    ),
    Tags
  ).



%! tag_resources(+M, +G, +Tag, +C, -Resources) is det.
%
% Returns the Resources associated with the given Tag.

tag_resources(M, G, Tag, C, Resources) :-
  findall(
    DT-Resource,
    (
      distinct(Resource, dc_subject(M, Resource, Tag, G)),
      q_instance(M, Resource, C, G),
      once(dc_created(M, Resource, DT, G))
    ),
    Pairs
  ),
  desc_pairs_values(Pairs, Resources).



%! tags_by_popularity(+M, +G, -Tags) is det.
%! tags_by_popularity(+M, +G, +MaxNumTags, -Tags) is det.
%
% Returns all tags, sorted by the number of resources with which they
% are associates, descending.

tags_by_popularity(M, G, Tags) :-
  tags_by_popularity(M, G, inf, Tags).


tags_by_popularity(M, G, MaxNumTags, TopTags) :-
  findall(
    NumResources-Tag,
    (
      q_instance(M, Tag, sioc:'Tag', G),
      q_number_of_subjects(M, dc:subject, Tag, G, NumResources)
    ),
    Pairs
  ),
  desc_pairs_values(Pairs, Tags),
  list_truncate(Tags, MaxNumTags, TopTags).





% HTML %

%! resource_tags(+M, +Resource, +G)// is det.

resource_tags(M, Resource, G) -->
  {resource_tags(M, G, Resource, Tags)},
  html([
    \icon(tag),
    \space,
    ul(rel='blog:hasTag', \html_maplist(tag_item(M, G), Tags))
  ]).



%! tag(+M, +G, +Tag)// is det.

tag(M, G, Tag) -->
  {tag_resources(M, G, Tag, blog:'Article', Articles)},
  deck(
    [about=Tag,rev='blog:hasTag',typeof='sioc:Tag'],
    blog_article:article_card(M, G),
    Articles
  ).



%! tag_item(+M, +G, +Tag)// is det.
%! tag_item(+M, +G, +Selected, +Tag)// is det.

tag_item(M, G, Tag) -->
  tag_item(M, G, false, Tag).


tag_item(M, G, Selected, Tag) -->
  {
    once(q_pref_label(M, Tag, Lbl, G)),
    (Selected == true -> Attrs = [class=active] ; Attrs = []),
    iri_to_location(Tag, Loc)
  }, !,
  html(
    li(Attrs,
      \internal_link(Loc, [typeof='sioc:Tag'], \qh_literal(Lbl))
    )
  ).
tag_item(_, _, _, _) --> [].



%! tag_list(+M, +G, +Tags)// is det.

tag_list(M, G, Tags) -->
  row_1(h2(\lstring(tags))),
  html(ul(\html_maplist(tag_item(M, G), Tags))).



%! tag_menu(+M, +G, +MaxNumTags, +Context)// is det.

tag_menu(M, G, MaxNumTags, Context) -->
  html(ul(\tag_menu_items(M, G, MaxNumTags, Context))).



%! tag_menu_item(+M, +G, +Context, +Tag)// is det.

tag_menu_item(M, G, Context, Tag) -->
  {(Context = tag(Tag) -> Selected = true ; Selected = false)},
  tag_item(M, G, Selected, Tag).



%! tag_menu_items(+M, +G, +MaxNumTags, +Context)// is det.

tag_menu_items(M, G, MaxNumTags, Context) -->
  {tags_by_popularity(M, G, MaxNumTags, Tags)},
  html_maplist(tag_menu_item(M, G, Context), Tags).
