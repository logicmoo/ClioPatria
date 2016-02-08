:- module(bs, []).

/** <module> Bootstrap

Defines the HTML resources for including Bootstrap into Web pages.

@author Wouter Beek
@version 2015/08, 2015/12, 2016/02
*/

:- use_module(library(debug)).
:- use_module(library(html/html_resource)).
:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).
:- use_module(library(http/jquery)).
:- use_module(library(settings)).

:- set_setting(jquery:version, '2.2.0.min').

:- if(debugging(css(bootstrap))).
  :- html_resource(
    css(bootstrap),
    [requires([css('bootstrap-3.3.6.css')]),virtual(true)]
  ).
:- else.
  :- html_resource(
    css(bootstrap),
    [requires([css('bootstrap-3.3.6.min.css')]),virtual(true)]
  ).
:- endif.

:- if(debugging(css('bootstrap-theme'))).
  :- html_resource(
    css('bootstrap-theme'),
    [ordered(true),requires([css(bootstrap),css('bootstrap-theme-3.3.6.css')]),virtual(true)]
  ).
:- else.
  :- html_resource(
    css('bootstrap-theme'),
    [ordered(true),requires([css(bootstrap),css('bootstrap-theme-3.3.6.min.css')]),virtual(true)]
  ).
:- endif.

:- if(debugging(js(bootstrap))).
  :- html_resource(
    js(bootstrap),
    [ordered(true),requires([jquery,js('bootstrap-3.3.6.js')]),virtual(true)]
  ).
:- else.
  :- html_resource(
    js(bootstrap),
    [ordered(true),requires([jquery,js('bootstrap-3.3.6.min.js')]),virtual(true)]
  ).
:- endif.
