:- module(users, [users_handler/1]).

/** <module> ClioPatria: Users page

@author Jan Wielemaker
@author Wouter Beek
@version 2016/10
*/

:- use_module(library(html/html_ext)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/rest)).
:- use_module(library(option)).

:- use_module(cp(components/cp_auth)).
:- use_module(cp(skin/cliopatria)).
:- use_module(cp(user/user_db)).
:- use_module(cp(user/user_page)).

:- http_handler(cliopatria(users), users_handler, [prefix]).

html:menu_item(admin, 1, users_handler, "Users").





%! users_handler(+Req) is det.
%
% HTTP Handler listing registered users.

users_handler(Req) :-
  rest_method(Req, [get], users_method).


users_method(Req, get, MTs) :-
  authorized(admin(users_handler)),
  if_allowed(admin(user(edit)), [edit(true)], UserOpts),
  if_allowed(admin(openid(edit)), [edit(true)], OpenIdOpts),
  rest_media_type(Req, get, MTs, users_media_type(UserOpts, OpenIdOpts)).


users_media_type(UserOpts, OpenIdOpts, get, text/html) :-
  reply_html_page(
    cliopatria([]),
    \cp_title(["Users"]),
    [
      h1("Users"),
      \user_table(UserOpts),
      p(\endpoint_link(add_user_form, "Add user")),
      h1("OpenID servers"),
      \openid_server_table(OpenIdOpts),
      p(\endpoint_link(add_openid_server_form, "Add OpenID server"))
    ]
  ).


%! user_table(+Opts)//
%
% HTML component generating a table of registered users.

user_table(Opts) -->
  {setof(User, current_user(User), Users)},
  table(
    \table_header_row(["UserID","Real name","On since","Idle","Edit"]),
    \html_maplist(user_row(Opts), Users)
  ).


user_row(Opts, User) -->
  {
    user_property(User, realname(Name)),
    findall(
      Idle-Login,
      user_property(User, connection(Login, Idle)),
      Pairs0
    ),
    keysort(Pairs0, Pairs),
    (   Pairs == []
    ->  OnLine = (-)
    ;   length(Pairs, NumPairs),
        Pairs = [Idle-Login|_],
        OnLine = online(Login, Idle, NumPairs)
    )
  },
  html(
    tr([
      td(User),
      td(Name),
      td(\on_since(OnLine)),
      td(\idle(OnLine)),
      td(\edit_user_button(User, Opts))
    ])
  ).


edit_user_button(User, Opts) -->
  {option(edit(true), Opts)}, !,
  internal_link(link_to_id(edit_user_form, [user=User]), "Edit").
edit_user_button(_, _) --> [].


on_since(online(Login, _Idle, _Connections)) --> !,
  {format_time(string(Date), '%+', Login)},
  html(Date).
on_since(_) -->
  html(-).


idle(online(_,Idle,_)) -->
  {mmss_duration(Idle, String)},
  html(String).
idle(_) -->
  html(-).


mmss_duration(Time, String) :-    % Time in seconds
  Secs is round(Time),
  Hour is Secs // 3600,
  Min  is (Secs // 60) mod 60,
  Sec  is Secs mod 60,
  format(string(String), '~`0t~d~2|:~`0t~d~5|:~`0t~d~8|', [Hour,Min,Sec]).



%! openid_server_table(+Opts)// is det.
%
% Lists registered OpenID servers.

openid_server_table(Opts) -->
  {setof(S, openid_current_server(S), Servers)}, !,
  table(
    \table_header_row(["Server","Description"]),
    \html_maplist(openid_server_row(Opts), Servers)
  ).
openid_server_table(_) --> [].


openid_server_row(Opts, Server) -->
  html(
    tr([
      td(\openid_server(Server)),
      td(\openid_field(Server, description)),
      tr(\edit_openid_button(Server, Opts))
    ])
  ).


openid_server(*) --> !,
  html(*).
openid_server(Server) -->
  html(a(href(Server), Server)).


openid_field(Server, Field) -->
  {
    Term =.. [Field, Value],
    openid_server_property(Server, Term)
  }, !,
  html(Value).
openid_field(_, _) --> [].


edit_openid_button(Server, Opts) -->
  {option(edit(true), Opts)}, !,
  internal_link(
    link_to_id(edit_openid_server_form, [openid_server=Server]),
    "Edit"
  ).
edit_openid_button(_, _) --> [].
