:- module(change_password, []).

/** <module> Quine admin: Change password

@author Jan Wielemaker
@author Wouter Beek
@version 2016/10
*/

:- use_module(library(error)).
:- use_module(library(html/html_ext)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/rest)).

:- use_module(cp(http_parms)).
:- use_module(cp(skin/cliopatria)).
:- use_module(cp(user/user_db)).

:- http_handler(cp(admin/changePassword), change_password_handler, []).

html:menu_item(user, 3, change_password_handler, "Change password") :-
  local_user_logged_on.

:- multifile
    http_param/1.

http_param(pwd0).
http_param(pwd1).
http_param(pwd2).
http_param(user).





% ENDPOINT %

%! change_password_handler(+Req) is det.

change_password_handler(Req) :-
  rest_method(Req, [get,put], change_password_method).


change_password_method(Req, get, MTs) :-
  logged_on(User), !,
  user_property(User, realname(RealName)),
  rest_media_type(Req, get, MTs, change_password_media_type(User, RealName)).
change_password_method(Req, put, MTs) :-
  logged_on(Login), !,
  http_parameters(
    Req,
    [pwd0(Password),pwd1(New),pwd2(Retype),user(User)],
    [attribute_declarations(http_param(change_password))]
  ),
  (   Login == admin
  ->  (current_user(User) -> true ; existence_error(user, User))
  ;   Login = User,
      validate_password(User, Password)
  ),
  (   New == Retype
  ->  true
  ;   throw(password_mismatch)
  ),
  password_hash(New, Hash),
  set_user_property(User, password(Hash)),
  rest_media_type(Req, post, MTs, change_password_result).
change_password_method(Req, _, MTs) :-
  rest_exception(Req, MTs, 401).


change_password_media_type(User, RealName, get, application/json) :-
  atomics_to_string(
    [
      "Send a POST request to change the password of ",
      User,
      " (",
      RealName,
      ")."
    ],
    Msg
  ),
  reply_json_dict(_{msg: Msg}).
change_password_media_type(User, RealName, get, text/html) :-
  reply_html_page(
    cliopatria([]),
    \cp_title(["Admin","Change password"]),
    [
      h1(["Change password for ",\quote(User)," (",RealName,")"]),
      \row_1(6, \change_password_form(User))
    ]
  ).


change_password_result(post, application/json) :-
  reply_json_dict(_{msg: "Password was changed successfully."}).
change_password_result(post, text/html) :-
  reply_html_page(
    cliopatria([]),
    \cp_title(["Admin","Password changed"]),
    [
      h1("Password changed"),
      p("Password was changed successfully.")
    ]
  ).





% HTML %

%! change_password_form(+User)// is det.
%
% HTML form for changing user passwords.  Admin can change the
% password of any user.

change_password_form(User) -->
  html(
    \form(
      link_to_id(change_password_handler),
      [method=post],
      [
        \user_or_old(User),
        \input_password(pwd1, [], [label("New Password")]),
        \input_password(pwd2, [], [label("Retype")]),
        \submit_button("Change password")
      ]
    )
  ).

user_or_old(admin) --> !,
  input_text(user, [], [label("User")]).
user_or_old(_) -->
  input_password(pwd0, [], [label("Old password")]).
