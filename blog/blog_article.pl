:- module(blog_article, []).

/** <module> ClioPatria blog: Articles

@author Wouter Beek
@version 2016/10
*/

:- use_module(library(html/html_ext)).
:- use_module(library(html/qh)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/rest)).
:- use_module(library(iri/iri_ext)).
:- use_module(library(jsonld/jsonld_build)).
:- use_module(library(nlp/nlp_lang)).
:- use_module(library(q/q_iri)).
:- use_module(library(q/q_rdf)).
:- use_module(library(q/q_rdfs)).
:- use_module(library(q/q_term)).
:- use_module(library(rdfa/rdfa_ext)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(settings)).

:- use_module(cp(blog/blog_article)).
:- use_module(cp(blog/blog_author)).
:- use_module(cp(blog/blog_comment)).
:- use_module(cp(blog/blog_generic)).
:- use_module(cp(blog/blog_tag)).

:- http_handler(cp(article), article_handler, [prefix]).

:- multifile
    blog:article_hook//1.

:- rdf_meta
   article(+, r, r, ?, ?).





% ENDPOINT %

%! article_handler(+Req) is det.

article_handler(Req) :-
  setting(blog:backend, M),
  q_graph_iri(blog, G),
  rest_method(
    Req,
    [get],
    article_plural_method(M, G),
    article_handler,
    article_singular_method(M, G)
  ).


article_plural_method(M, G, Req, Method, MTs) :-
  rest_media_type(Req, Method, MTs, article_plural_media_type(M, G)).


article_plural_media_type(M, G, get, application/'ld+json') :-
  q_triples(M, _, rdf:type, blog:'Article', G, Triples),
  triples_to_jsonld(M, Triples, G, Dict),
  reply_json_dict(Dict).
article_plural_media_type(M, G, get, text/html) :-
  resources_by_date_created(M, G, blog:'Article', Articles),
  maplist(lstring, [article,overview], Strs),
  reply_html_page(
    article(_),
    \blog_title(Strs),
    \article_deck(M, G, Articles)
  ).


article_singular_method(M, G, Article, Req, get, MTs) :-
  \+ q_instance(M, Article, blog:'Article', G), !,
  rest_exception(Req, MTs, 404).
article_singular_method(M, G, Article, Req, Method, MTs) :-
  rest_media_type(
    Req,
    Method,
    MTs,
    article_singular_media_type(M, G, Article)
  ).


article_singular_media_type(M, G, Article, get, application/'ld+json') :-
  findall(Triple, article_meta_triple(M, Article, Triple, G), Triples),
  triples_to_jsonld(M, Triples, G, Dict),
  reply_json_dict(Dict).
article_singular_media_type(M, G, Article, get, text/html) :-
  once(dc_title(M, Article, Title, G)),
  lstring(article, Str1),
  q_literal_string(Title, Str2),
  reply_html_page(
    article(Article),
    [
      \article_metas(M, G, Article),
      \blog_title([Str1,Str2])
    ],
    \article(M, G, Article)
  ).


article_metas(M, G, Article) -->
  {findall(Triple, article_meta_triple(M, Article, Triple, G), Triples)},
  html_maplist(article_meta, Triples).


article_meta_triple(M, S, rdf(S,'og:description',Abstract), G) :-
  once(dc_abstract(M, S, Abstract0, G)),
  q_literal_string(Abstract0, Abstract).
article_meta_triple(M, S, rdf(S,'og:image',Img), G) :-
  once(foaf_depiction(M, S, Img, G)).
article_meta_triple(_, S, rdf(S,'og:site_name',SiteName), _) :-
  setting(cliopatria:title, SiteName).
article_meta_triple(M, G, S, rdf(S,'og:title',Title)) :-
  once(dc_title(M, S, Title0, G)),
  q_literal_string(Title0, Title).
article_meta_triple(_, S, rdf(S,'og:type',"article"), _).
article_meta_triple(_, S, rdf(S,'og:url',S), _).


article_meta(rdf(_,P,O)) -->
  html(meta([property=P,content=O], [])).





% HTML %

%! article(+M, +G, +Article)// is det.

article(M, G, Article) -->
  {
    rdfa_prefixed_iri(Article, Article0),
    iri_to_location(Article, Loc)
  },
  html(
    article(about=Article0, [
      header([
        \internal_link(Loc, \dc_title(M, Article, G)),
        div(class=metadata, [
          \internal_link(Loc, \ignore(bf_subtitle(M, Article, G))),
          \resource_creators_sep(M, Article, G),
          \resource_created_sep(M, Article, G),
          \resource_tags(M, Article, G)
        ]),
        ul(class='social-btns',
          \social_buttons(M, G, article(Article))
        )
      ]),
      \row_3(
        [2,1,0,0], [],
        [8,10,12,12], [
          \article_content(Article),
          \article_comments(M, G, Article)
        ],
        [2,1,0,0], []
      )
    ])
  ).



%! article_card(+M, +G, +Article)// is det.

article_card(M, G, Article) -->
  {
    iri_to_location(Article, Loc),
    rdfa_prefixed_iri(Article, Article0)
  },
  card(
    [about=Article0,typeof='blog:Article'],
    \internal_link(Loc, \foaf_depiction(M, Article, G)),
    [
      \internal_link(Loc, \dc_title(M, Article, G)),
      \dc_abstract(M, Article, G),
      footer([
        \resource_creators_sep(M, Article, G),
        \resource_created_sep(M, Article, G),
        \resource_tags(M, Article, G)
      ])
    ]
  ).



%! article_comments(+M, +G, +Article)// is det.

article_comments(M, G, Article) -->
  {resource_comments(M, G, Article, Comments)},
  html([
    h2(\lstring(comments)),
    \deck(rel='sioc:has_reply', comment(M, G), Comments),
    \add_comment(Article)
  ]).
article_comments(_, _, _) --> [].



%! article_content(+Article)// is det.

article_content(Article) -->
  {q_abox_iri(article, Local, Article)},
  html(div(property='sioc:content', \(blog:article_hook(Local)))).



%! article_deck(+M, +G, +Articles)// is det.

article_deck(M, G, Articles) -->
  deck(blog_article:article_card(M, G), Articles).



%! article_sidebar(+M, +G, +Article)// is det.

article_sidebar(M, G, Article) -->
  {once(dc_creator(M, Article, Author, G))},
  grid(100, 100, author(M, G), [Author]).



%! article_tag(+M, +G, +Tag)// is det.

article_tag(M, G, Tag) -->
  {
    lstring(view_all_posts_tagged_with, Str1),
    once(q_pref_label(M, Tag, Lbl, G)),
    q_literal_string(Lbl, Str2),
    atomics_to_string([Str1," ‘",Str2,"’"], Title),
    iri_to_location(Tag, Loc)
  },
  internal_link(Loc, [rel=tag,title=Title], \qh_literal(Lbl)).
