:- module(user_page, []).

/** <module> ClioPatria User: User page

@author Jan Wielemaker
@author Wouter Beek
@version 2016/10
*/

:- use_module(library(html/html_ext)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(http/rest)).

:- use_module(cp(skin/cliopatria)).
:- use_module(cp(user/user_db)).

:- http_handler(cliopatria(user), user_handler, [prefix]).

html:menu_item(user, 2, user_handler, "User page") :-
  someone_logged_on.





%! user_handler(+Req) is det.
%
%  OpenID server user page for a registered user.

user_handler(Req) :-
  rest_method(Req, [get], user_method).

user_method(Req, get, MTs) :-
  current_user(User), !,
  findall(Prop, user_property(User, Prop), Props),
  rest_media_type(Req, get, MTs, user_media_type(User, Props)).
user_method(Req, _, MTs) :-
  rest_exception(Req, MTs, 401).

user_media_type(User, Props, get, text/html) :-
  reply_html_page(
    cliopatria([]),
    [
      \link('openid.server'-link_to_id(openid_server)),
      \cp_title(["OpenID",User])
    ],
    [
      h1(["OpenID page for user ",\quote(User)]),
      \html_maplist(user_property, Props)
    ]
  ).

user_property(realname(Name)) --> !,
  html(div(["Real name: ",Name])).
user_property(connection(Login, IdleF)) --> !,
  {
    format_time(string(S), "%+", Login),
    Idle is round(IdleF),
    Hours is Idle // 3600,
    Min is Idle mod 3600 // 60,
    Sec is Idle mod 60
  },
  html(div(["Logged in since ~s, idle for ~d:~d:~d"-[S,Hours,Min,Sec]])).
user_property(_) -->
  [].
