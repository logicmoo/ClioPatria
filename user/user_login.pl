:- module(user_login, []).

/** <module> ClioPatria User: Login

@author Jan Wielemaker
@author Wouter Beek
@version 2016/10
*/

:- use_module(library(apply)).
:- use_module(library(html/html_ext)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/rest)).

:- use_module(cp(applications/admin)).
:- use_module(cp(http_parms)).
:- use_module(cp(skin/cliopatria)).
:- use_module(cp(user/user_db)).

:- http_handler(cliopatria(user/login), user_login, []).

html:menu_item(user, 1, login_form, "Login") :-
  \+ someone_logged_on.

http_param('openid.return_to').
http_param(password).
http_param(return_to).
http_param(user).





%! user_login(+Request) is det.
%
% HTTP handler that presents a form to login.
%
% Handle =user= and =password=.  If there is a parameter =return_to=
% or =|openid.return_to|=, reply using a redirect to the given
% URL. Otherwise display a welcome page.

user_login(Req) :-
  rest_method(Req, [get,post], login_method).


login_method(Req, post, MTs) :-
  http_parameters(
    Req,
    [
      'openid.return_to'(ReturnTo),
      password(Password),
      return_to(ReturnTo),
      user(User)
    ],
    [attribute_declarations(http_param(user_login))]
  ),
  maplist(ground, [Password,User]), !,
  (var(ReturnTo) -> Extra = [] ;  Extra = [return_to(ReturnTo)]),
  reply_login([user(User),password(Password)|Extra], MTs).
login_method(Req, Method, MTs) :-
  memberchk(Method, [get,post]),
  rest_media_type(Req, get, MTs, login_media_type).


login_media_type(get, text/html) :-
  reply_html_page(
    cliopatria(default),
    \cp_title(["Login"]),
    [
      h1("Login"),
      \form(
        link_to_id(user_login),
        [method=post],
        [
          \input_text(user, [size(20)]),
          \input_password(password, [size(20)]),
          \submit_button("Login")
        ]
      )
    ]
  ).


login_success(User, Iri, post, text/html) :-
  reply_html_page(
    cliopatria(default),
    \cp_title(["User",User,"Logged in"]),
    [
      h1(["User ",\quote(User)," logged in successfully"]),
      p(["You're logged in with OpenID ",\link(Iri)])
    ]
  ).
