:- module(graphs, []).

/** <module> ClioPatria: Graphs overview

@author Wouter Beek
@version 2016/06-2016/10
*/

:- use_module(library(html/html_ext)).
:- use_module(library(html/qh_ui)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/rest)).
:- use_module(library(settings)).

:- use_module(cp(skin/cliopatria)).

:- http_handler(cliopatria(graphs), graphs_handler, [prefix]).

html:menu_item(places, 4, graphs_handler, "Graphs").

:- setting(
     backend,
     oneof([hdt,trp]),
     hdt,
     "Backend that powers the graphs overview."
   ).





graphs_handler(Req) :-
  rest_method(Req, [get], graphs_method).


graphs_method(Req, Method, MTs) :-
  rest_media_type(Req, Method, MTs, graphs_media_type).


graphs_media_type(get, text/html) :-
  reply_html_page(
    cliopatria([]),
    \cp_title(["Graphs","Overview"]),
    \row_1(9, \qh_graph_table)
  ).
