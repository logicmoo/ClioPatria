:- module(
  http_parms,
  [
    http_param/3 % +Mod, +Key, +Spec
  ]
).

/** <module> ClioPatria: HTTP parameters

@author Jan Wielemaker
@author Wouter Beek
@version 2016/09-2016/10
*/

:- use_module(library(gis/gis)).
:- use_module(library(settings)).

:- multifile
    http:http_param/3.

http:http_param(
  _,
  admin,
  [
    boolean,
    default(false),
    description("Provide administrative rights.")
  ]
).
http:http_param(
  Mod,
  dataset,
  [
    description("Use a specific dataset as a filter on results."),
    q_iri
  | T]
) :-
  (   Mod == simple_graph_pattern
  ->  T = [optional(true)]
  ;   T = []
  ).
http:http_param(
  Mod,
  graph,
  [
    description("Use a specific graph as a filter on results."),
    q_iri
  | T]
) :-
  (   Mod == upload_endpoint
  ->  T = []
  ;   Mod == geo,
      once(gis_graph(G))
  ->  T = [default(G)]
  ;   T = [optional(true)]
  ).
http:http_param(
  _,
  object,
  [
    description("Use a specific object term as a filter on results."),
    optional(true),
    q_term
  ]
).
http:http_param(
  _,
  page,
  [
    default(1),
    description("The results page number."),
    positive_integer
  ]
).
http:http_param(
  Mod,
  page_size,
  [
    between(1, MaxPageSize),
    default(DefPageSize),
    description("The number of results per page.")
  ]
) :-
  setting(Mod:max_page_size, MaxPageSize),
  setting(Mod:default_page_size, DefPageSize).
http:http_param(
  Mod,
  password,
  [
    description("The current password."),
    length > 5
  | T]
) :-
  (   memberchk(Mod, [change_password,user_login])
  ->  T = [optional(true)]
  ;   T = []
  ).
http:http_param(
  _,
  pwd1,
  [
    length > 5,
    description("The first entry of a new password.")
  ]
).
http:http_param(
  _,
  pwd2,
  [
    description("The second entry of a new password."),
    length > 5
  ]
).
http:http_param(
  _,
  predicate,
  [
    description("Use a specific predicate term as a filter on results."),
    optional(true),
    q_iri
  ]
).
http:http_param(
  _,
  'openid.return_to',
  [optional(true)]
).
http:http_param(
  _,
  read,
  [
    boolean,
    default(false),
    description("Provide read-only access to the RDF store.")
  ]
).
http:http_param(
  _,
  realname,
  [
    atom,
    description("Comment on user identifier-name.")
  ]
).
http:http_param(
  _,
  return_to,
  [
    atom,
    description("URI to return to"),
    optional(true)
  ]
).
http:http_param(
  _,
  subject,
  [
    description("Use a specific subject term as a filter on results."),
    optional(true),
    q_iri
  ]
).
http:http_param(
  Mod,
  user,
  [
    description("User identifier-name."),
    length > 1
  | T]
) :-
  (   memberchk(Mod, [change_password,user_login])
  ->  T = [optional(true)]
  ;   T = []
  ).
http:http_param(
  _,
  write,
  [
    boolean,
    default(false),
    description("Obtain write access to the RDF store.")
  ]
).



%! http_param(+Mod, +Key, +Spec) is det.

http_param(Mod, Key, Spec) :-
  Mod:http_param(Key),
  http:http_param(Mod, Key, Spec).
