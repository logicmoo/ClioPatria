:- module(
  blog_author,
  [
  % API
    create_author/6,       % +M, +Author, +Img, +GivenName, +FamilyName, +G
  % HTML
    author//3,                 % +M, +G, +Author
    author_grid//3             % +M, +G, +Authors
  ]
).

/** <module> ClioPatria blog: Authors

@author Wouter Beek
@version 2016/10
*/

:- use_module(library(html/html_ext)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_json)).
:- use_module(library(http/rest)).
:- use_module(library(iri/iri_ext)).
:- use_module(library(jsonld/jsonld_build)).
:- use_module(library(nlp/nlp_lang)).
:- use_module(library(pair_ext)).
:- use_module(library(q/q_rdf)).
:- use_module(library(q/q_term)).
:- use_module(library(q/qb)).
:- use_module(library(rdfa/rdfa_ext)).
:- use_module(library(semweb/rdf11)).

:- use_module(cp(blog/blog_article)).
:- use_module(cp(blog/blog_comment)).
:- use_module(cp(blog/blog_generic)).

:- http_handler(cp(author), author_handler, [prefix]).

:- rdf_meta
   create_author(+, r, +, +, +, r).





% ENDPOINT %

%! author_handler(+Req) is det.

author_handler(Req) :-
  setting(blog:backend, M),
  q_graph_iri(blog, G),
  rest_method(
    Req,
    [get],
    author_plural_method(M, G),
    author_handler,
    author_singular_method(M, G)
  ).


author_plural_method(M, G, Req, Method, MTs) :-
  rest_media_type(Req, Method, MTs, author_plural_media_type(M, G)).


author_plural_media_type(M, G, get, application/'ld+json') :-
  q_triples(M, _, rdf:type, blog:'Author', G, Triples),
  triples_to_jsonld(M, Triples, G, Dict),
  reply_json_dict(Dict).
author_plural_media_type(M, G, get, text/html) :-
  findall(
    NumArticles-Author,
    (
      distinct(Author, creator(M, _, Author, G)),
      q_aggregate_all(count, q(M, _, dc:creator, Author, G), NumArticles)
    ),
    Pairs
  ),
  desc_pairs_values(Pairs, Authors),
  maplist(lstring, [author,overview], Strs),
  reply_html_page(
    author(_),
    \blog_title(Strs),
    \author_grid(M, G, Authors)
  ).


author_singular_method(M, G, Author, Req, _, MTs) :-
  \+ q_instance(M, Author, blog:'Author', G), !,
  rest_exception(Req, MTs, 404).
author_singular_method(M, G, Author, Req, Method, MTs) :-
  rest_media_type(Req, Method, MTs, author_singular_media_type(M, G, Author)).


author_singular_media_type(M, G, Author, get, application/'ld+json') :-
  subject_to_jsonld(M, Author, G, Dict),
  reply_json_dict(Dict).
author_singular_media_type(M, G, Author, get, text/html) :-
  once(creator(M, _, Author, G)),
  lstring(author, Str),
  agent_name(M, Author, Name, G),
  reply_html_page(
    author(Author),
    \blog_title([Str,Name]),
    \author(M, G, Author)
  ).





% API %

%! create_author(+M, +Author, +Img, +GivenName, +FamilyName, +G) is det.

create_author(M, Author, Img, GivenName, FamilyName, G) :-
  qb_user(M, Author, blog:'Author', Img, GivenName, FamilyName, G).





% HTML %

%! author(+M, +G, +Author)// is det.

author(M, G, Author) -->
  html(
    article([about=Author,typeof='blog:Author'], [
      header([
        h1(\agent_name(M, Author, G)),
        \agent_image(M, Author, G),
        \ignore(foaf_mbox(M, Author, G)),
        \ignore(foaf_homepage(M, Author, G))
      ]),
      \author_article_deck(M, G, Author),
      \user_comment_deck(M, G, Author)
    ])
  ).



%! author_article_deck(+M, +G, +Author)// is det.

author_article_deck(M, G, Author) -->
  {user_posts(M, G, Author, blog:'Article', Articles)},
  author_article_deck0(M, G, Articles).


author_article_deck0(_, _, []) --> !, html([]).
author_article_deck0(M, G, L) -->
  row_1(h2(\lstring(articles))),
  deck([rel='dc:creator'], blog_article:article_card(M, G), L).



%! author_grid(+M, +G, +Authors)// is det.

author_grid(M, G, Authors) -->
  grid(800, 150, author_tile(M, G), Authors).



%! author_tile(+M, +G, +Author)// is det.

author_tile(M, G, Author) -->
  {iri_to_location(Author, Loc)},
  html([
    \internal_link(Loc, \agent_image(M, Author, G)),
    h1(\agent_name(M, Author, G))
  ]).
