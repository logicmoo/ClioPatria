:- module(user_page, []).

/** <module> ClioPatria: User page

@author Jan Wielemaker
@author Wouter Beek
@tbd 
@version 2016/10
*/

:- use_module(library(html/html_ext)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(http/rest)).

:- use_module(cp(applications/admin)).
:- use_module(cp(http_parms)).
:- use_module(cp(skin/cliopatria)).
:- use_module(cp(user/user_db)).
:- use_module(cp(user/users)).

:- http_handler(cliopatria(user), user_handler, [prefix]).

html:menu_item(user, 2, user_handler, "User page") :-
  someone_logged_on.

:- multifile
    http_param/1,
    media_type/1.

http_param(admin).
http_param(pwd1).
http_param(pwd2).
http_param(read).
http_param(realname).
http_param(user).
http_param(write).

media_type(text/html).





%! user_handler(+Req) is det.
%
% OpenID server user page for a registered user.

user_handler(Req) :-
  rest_method(Req, [get,post], user_method).

user_method(Req, get, MTs) :-
  current_user(User), !,
  findall(Prop, user_property(User, Prop), Props),
  rest_media_type(Req, get, MTs, user_media_type(User, Props)).
user_method(Req, get, MTs) :-
  rest_exception(Req, MTs, 401).
user_method(Req, post, MTs) :-
  (has_current_user -> authorized(post-user_handler) ; FirstUser = true),
  http_parameters(
    Req,
    [
      admin(Admin),
      pwd1(Password),
      pwd2(Retype),
      read(Read),
      realname(RealName),
      user(User),
      write(Write)
    ],
    [attribute_declarations(http_param(user_page))]
  ),
  % @tbd Return the correct HTTP status code.
  (   current_user(User)
  ->  throw(
        error(
          permission_error(create, user, User),
          context(_, "Already present")
        )
      )
  ;   true
  ),
  % @tbd Return the correct HTTP status code.
  (Password == Retype -> true ; throw(password_mismatch)),
  password_hash(Password, Hash),
  phrase(allow(Read, Write, Admin), Allow),
  user_add(User, [allow(Allow),password(Hash),realname(RealName)]),
  rest_media_type(Req, post, MTs, user_media_type(FirstUser, User, Password)).

  
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
user_media_type(true, User, Password, post, MT) :- !,
  user_add(
    anonymous,
    [allow([read(_,_)]),realname("Define rights for not-logged in users")]
  ),
  reply_login([user(User),password(Password)], [MT]).
user_media_type(true, _, _, post, _) :-
  users_handler(_).


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
