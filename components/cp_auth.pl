:- module(
  cp_auth,
  [
    if_allowed/3 % +Token, +Opts1, -Opts2
  ]
).

/** <module> ClioPatria authentication components

@author Jan Wielemaker
@author Wouter Beek
@version 2016/10
*/

:- use_module(cp(user/user_db)).





%! if_allowed(+Token, +Opts1, -Opts2) is det.

if_allowed(Token, Opts, Opts) :-
  logged_on(User, anonymous),
  catch(check_permission(User, Token), _, fail), !.
if_allowed(_, _, []).
