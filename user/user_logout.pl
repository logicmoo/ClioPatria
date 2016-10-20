:- module(logout_endpoint, []).

/** <module> Quine: Logout endpoint

@author Jan Wielemaker
@author Wouter Beek
@version 2016/10
*/

:- use_module(library(html/html_ext)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/rest)).

:- use_module(cp(skin/cliopatria)).
:- use_module(cp(user/user_db)).

:- http_handler(cp(user/logout), user_logout, []).

html:menu_item(user, 5, user_logout, "Logout") :-
	someone_logged_on.





%! user_logout(+Req) is det.
%
% Logout the current user.

user_logout(Req) :-
  rest_method(Req, [get], user_logout_method).


user_logout_method(Req, Method, MTs) :-
  logged_on(User), !,
  logout(User),
  rest_media_type(Req, Method, MTs, succeed_media_type(User)).
user_logout_method(Req, Method, MTs) :-
  rest_media_type(Req, Method, MTs, failed_media_type).


failed_media_type(get, text/html) :-
  reply_html_page(
    cliopatria([]),
    \cp_title(["Logout"]),
    [
      h1("Not logged on"),
      p("Possibly you are logged out because the session has timed out.")
    ]
  ).


succeed_media_type(User, get, text/html) :-
  reply_html_page(
    cliopatria([]),
    \cp_title(["Logout"]),
    h1(["Logged out ",\quote(User)])
  ).
