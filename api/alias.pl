:- module(alias, []).

/** <module> ClioPatria: Alias API

@author Jan Wielemaker
@author Wouter Beek
@version 2016/09-2016/10
*/

:- use_module(library(apply)).
:- use_module(library(dict_ext)).
:- use_module(library(html/html_ext)).
:- use_module(library(html/qh)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_ext)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/rest)).
:- use_module(library(lists)).
:- use_module(library(pagination)).
:- use_module(library(q/q_term)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(settings)).

:- use_module(cp(http_parms)).
:- use_module(cp(skin/cliopatria)).

:- http_handler(cliopatria(alias), alias_handler, [prefix]).

:- multifile
    http_param/1.

http_param(page).
http_param(page_size).

:- setting(
     default_page_size,
     positive_integer,
     100,
     "The default number of aliases that is retrieved in one request."
   ).
:- setting(
     max_page_size,
     positive_integer,
     1000,
     "The maximum number of aliases that can be retrieved in one request."
   ).





alias_handler(Req) :-
  rest_method(Req, [get], alias_method).


alias_method(Req, get, MTs) :-
  http_parameters(
    Req,
    [page(Page),page_size(PageSize)],
    [attribute_declarations(http_param(alias_endpoint))]
  ),
  http_location_iri(Req, Iri),
  PageOpts = _{iri: Iri, page: Page, page_size: PageSize},
  pagination(
    Alias-Prefix,
    q_alias_prefix(Alias, Prefix),
    PageOpts,
    Result
  ),
  rest_media_type(Req, get, MTs, alias_media_type(Result)).


alias_media_type(Result, get, application/json) :-
  dict_pairs(Dict, Result.results),
  reply_json_dict(Dict).
alias_media_type(Result, get, application/trig) :-
  alias_get_trig_or_turtle(Result, trig).
alias_media_type(Result, get, application/turtle) :-
  alias_get_trig_or_turtle(Result, turtle).
alias_media_type(Result, get, text/html) :-
  reply_html_page(
    cliopatria([]),
    \cp_title(["Alias","Overview"]),
    \pagination_result(Result, alias_table)
  ).


alias_get_trig_or_turtle(Result, Subtype) :-
  longest_alias(Result.results, MaxLen),
  Col is MaxLen + 10,
  http_content_type(application/Subtype),
  http_pagination_links(Result),
  http_end_of_header,
  maplist(turtle_alias(Col), Result.results).


%! longest_alias(+Pairs, -Len) is det.

longest_alias(Pairs, Len) :-
  pairs_keys(Pairs, Aliases),
  maplist(atom_length, Aliases, Lens),
  max_list(Lens, Len).


turtle_alias(Col, Alias-Prefix) :-
  format("@prefix ~t~w: ~*|<~w> .~n", [Alias,Col,Prefix]).





% HTML %

alias_table([]) --> !, [].
alias_table(Pairs) -->
  table(
    \table_header_row(["Alias","Prefix"]),
    \html_maplist(alias_table_data_row, Pairs)
  ).


alias_table_data_row(Alias-Prefix) -->
  html(tr([td(\qh_alias(Alias)),td(\external_link(Prefix))])).
