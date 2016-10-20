:- module(
  blog_comment,
  [
  % API
    create_comment/5,    % +M, +G, +User, +Resource, +Content:string
    resource_comments/4, % +M, +G, +Resource, -Comments
    user_posts/5,        % +M, +G, +User, +Class, -Posts
  % HTML
    add_comment//1,      % +Resource
    comment//3,          % +M, +G, +Comment
    user_comment_deck//3 % +M, +G, +User
  ]
).

/** <module> Quine blog: Comments

http_link_to_id(vote, VoteLink)

```prolog
%! add_comment_money// is det.

add_comment_money -->
  html([
    '$',
    input([
      id='add-comment-money',
      max=99,
      min=1,
      name=currency,
      size=2,
      title='Monetary voting',
      type=number
    ])
  ]).
```

@author Wouter Beek
@version 2016/10
*/

:- use_module(library(error)).
:- use_module(library(html/html_ext)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_ext)).
:- use_module(library(http/http_json)).
:- use_module(library(http/rest)).
:- use_module(library(jsonld/jsonld_build)).
:- use_module(library(pair_ext)).
:- use_module(library(q/q_iri)).
:- use_module(library(q/q_list)).
:- use_module(library(q/q_rdf)).
:- use_module(library(q/q_term)).
:- use_module(library(q/qb)).
:- use_module(library(rdfa/rdfa_ext)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(settings)).
:- use_module(library(solution_sequences)).

:- use_module(cp(blog/blog_comment)).
:- use_module(cp(blog/blog_generic)).
:- use_module(cp(blog/blog_vote)).
:- use_module(cp(user/user_db)).

:- http_handler(cp(comment), comment_handler, [prefix]).

:- rdf_meta
   create_comment(+, r, r, r, +),
   resource_comments(+, r, r, -),
   user_posts(+, r, r, r, -).





% ENDPOINT %

%! create_comment(+M, +G, +User, +Resource, +Content:string) is det.
%
% Asserts a comment by User about Resource consisting of Content.

create_comment(M, G, User, Resource, Content) :-
  q_abox_iri(comment, Comment),
  qb_instance(M, Comment, sioc:'Comment', G),
  create_post(M, G, User, Resource, Content, Comment).



%! create_post(+M, +G, +User, +Resource, -Post) is det.

create_post(M, G, User, Resource, Content, Post) :-
  qb(M, Post, sioc:reply_of, Resource, G),
  qb_now(M, Post, dc:created, G),
  qb(M, Post, dc:creator, User, G),
  qb(M, Post, blog:downVotes, 0^^xsd:nonNegativeInteger, G),
  qb(M, Post, blog:upVotes, 0^^xsd:nonNegativeInteger, G),
  qb(M, Post, sioc:content, Content, G).



%! resource_comments(+M, +G, +Resource, -Comments) is det.

resource_comments(M, G, Resource, Comments) :-
  findall(
    DT-Comment,
    (
      distinct(Comment, q(M, Comment, sioc:reply_of, Resource, G)),
      \+ q(M, Comment, blog:wasDeletedBy, _, G),
      once(dc_created(M, Comment, DT, G))
    ),
    Pairs
  ),
  % Do not display the comments section if (i) there are no comments and
  % (ii) the user is not logged in.
  (Pairs == [] -> has_current_user ; true), !,
  desc_pairs_values(Pairs, Comments).





% ENDPOINT %

%! comment_handler(+Req) is det.

comment_handler(Req) :-
  setting(blog:backend, M),
  q_graph_iri(blog, G),
  rest_method(
    Req,
    [delete,get],
    comment_plural_method(M, G),
    comment_handler,
    comment_singular_method(M, G)
  ).


comment_plural_method(_, _, Req, post, MTs) :-
  \+ has_current_user, !,
  rest_exception(Req, MTs, 401).
comment_plural_method(M, G, Req, Method, MTs) :-
  rest_media_type(Req, Method, MTs, comment_plural_media_type(Req, M, G)).


comment_plural_media_type(_, M, G, get, application/'ld+json') :-
  q_triples(M, _, rdf:type, sioc:'Comment', Triples, G),
  triples_to_jsonld(M, Triples, G, Jsonld),
  reply_json_dict(Jsonld).
comment_plural_media_type(Req, M, G, post, application/json) :-
  catch(
    (
      http_read_json_dict(Req, Data),
      Content = Data.content,
      must_be(string, Content),
      atom_string(Comment, Data.about),
      must_be(iri, Comment)
    ),
    E,
    rest_exception([application/json], bad_request(E))
  ),
  current_user(User),
  create_comment(M, G, User, Comment, Content),
  reply_json(_{}, [status(201)]).


comment_singular_method(M, G, Comm, Req, _, MTs) :-
  \+ q_instance(M, Comm, sioc:'Comment', G), !,
  rest_exception(Req, MTs, 404).
comment_singular_method(M, G, Comm, Req, delete, MTs) :-
  \+ ((
    once(creator(M, Comm, User, G)),
    current_user(User)
  )), !,
  rest_exception(Req, MTs, 401).
comment_singular_method(M, G, Comm, Req, Method, MTs) :-
  rest_media_type(Req, Method, MTs, comment_singular_media_type(M, G, Comm)).


comment_singular_media_type(M, G, Comm, delete, application/json) :-
  current_user(User),
  q_transaction((
    qb_action(M, blog:'Deletion', User, Action, G),
    qb(M, Comm, blog:wasDeletedBy, Action, G)
  )),
  reply_json(_{}, [status(201)]).
comment_singular_media_type(M, G, Comm, get, application/'ld+json') :-
  subject_to_jsonld(M, Comm, G, Dict),
  reply_json_dict(Dict).





% HTML %

%! add_comment(+Resource)// is det.
%
% Generates HTML UI for adding a post of the given kind.

add_comment(Resource) -->
  {
    has_current_user, !,
    http_link_to_id(comment_handler, CommentLink),
    format(
      atom(CreateFunc),
      'createComment("~a", "~a", $("#create-comment"));',
      [CommentLink,Resource]
    )
  },
  html([
    div(id='add-a-new-comment', 'Add a new comment:'),
    form(id='create-comment', [
      div(id='create-comment-content', textarea("")),
      div(id='create-comment-toolbar', [
        \icon_button(create, CreateFunc),
        \icon_button(cancel, 'clearTextField();')
      ])
    ])
  ]).
add_comment(_) --> [].



%! comment(+M, +G, +Comment)// is det.

comment(M, G, Comment) -->
  {once(creator(M, Comment, Author, G))},
  html(
    article([about=Comment,typeof='sioc:Comment'], [
      header([
        \lstring(written_by),
        " ",
        \agent_name(M, Author, G),
        " ",
        \lstring(on),
        " ",
        \dc_created(M, Comment, G),
        \votes(M, G, Comment, Author)
      ]),
      div([
        div(class='comment-inner', [
          \sioc_content(M, Comment, G),
          \edit_comment(Comment, Author)
        ]),
        \agent_image(M, Author, G)
      ])
    ])
  ).



%! comment_item(+M, +G, +Class, +Comment)// is det.

comment_item(M, G, C, Comment) -->
  html(
    li([about=Comment,typeof=C], [
      \lstring(commented_on),
      " ",
      \sioc_reply_of(M, Comment, G)
    ])
  ).



%! edit_comment(+Resource, +User)// is det.
%
% Generates HTML UI for editing the given comment.
%
% The UI is more comprehensive for the logged-in user that created the
% comment.

edit_comment(Resource, Author) -->
  {
    current_user(Author), !,
    format(atom(DeleteFunc), 'deleteComment("~a");', [Resource])
  },
  html(
    div(class='edit-comment-toolbar', [
      \icon_button(update),
      \icon_button(delete, DeleteFunc)
    ])
  ).
edit_comment(_, _) --> [].



%! user_comment_deck(+M, +G, +User)// is det.

user_comment_deck(M, G, User) -->
  {user_posts(M, G, User, sioc:'Comment', Comments)},
  user_comment_deck0(M, G, Comments).


user_comment_deck0(_, _, []) --> !, html([]).
user_comment_deck0(M, G, L) -->
  row_1(h2(\lstring(comments))),
  deck([rel='dc:creator'], comment(M, G), L).



%! user_posts(+M, +G, +User, +Class, -Posts) is det.
%
% Filters posts of the give class that are made by the given user
% and generates HTML for each such post based on Goal_1.

user_posts(M, G, User, Class, Posts) :-
  findall(
    DT-Post,
    (
      distinct(Post, q_list_member(M, Post, dc:creator, User, G)),
      q_instance(M, Post, Class, G),
      once(dc_created(M, Post, DT, G))
    ),
    Pairs
  ),
  desc_pairs_values(Pairs, Posts).
