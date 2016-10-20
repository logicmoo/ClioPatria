:- module(
  blog_vote,
  [
  % API
    resource_votes/4,     % +M, +G,        +Resource, -Vote:integer
    user_tries_to_vote/5, % +M, +G, +User, +Resource, +Vote:integer
    user_vote/5,          % +M, +G, +User, +Resource, -Vote:integer
  % HTML
    votes//4              % +M, +G, +Author, +G
  ]
).

/** <module> ClioPatria blog: Votes

@author Wouter Beek
@version 2016/10
*/

:- use_module(library(html/html_ext)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_ext)).
:- use_module(library(http/http_json)).
:- use_module(library(http/rest)).
:- use_module(library(jsonld/jsonld_build)).
:- use_module(library(q/qb)).
:- use_module(library(q/qu)).
:- use_module(library(q/q_iri)).
:- use_module(library(q/q_prefix), []).
:- use_module(library(q/q_rdf)).
:- use_module(library(q/q_term)).
:- use_module(library(rdfs/rdfs_ext)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(settings)).

:- use_module(library(q/q_prefix), []).
:- use_module(library(q/q_rdf)).
:- use_module(library(q/q_term)).
:- use_module(library(q/qb)).
:- use_module(library(q/qu)).

:- use_module(cp(blog/blog_author)).
:- use_module(cp(user/user_db)).

:- http_handler(cp(vote), vote_handler, []).

:- rdf_meta
   user_tries_to_vote(r, r, r, r).





vote_handler(Req) :-
  setting(blog:backend, M),
  q_graph_iri(blog, G),
  rest_method(
    Req,
    [get],
    vote_plural_method(M, G),
    vote_handler,
    vote_singular_method(M, G)
  ).


vote_plural_method(_, _, Req, _, _) :-
  \+ has_current_user, !,
  http_status_reply(Req, authorise(basic,'')).
vote_plural_method(M, G, Req, Method, MTs) :-
  rest_media_type(Req, Method, MTs, vote_plural_media_type(M, G)).


vote_plural_media_type(M, G, get, application/'json+ld') :-
  q_triples(M, _, rdf:type, blog:'Vote', G, Trips),
  triples_to_jsonld(M, Trips, G, Dict),
  reply_json_dict(Dict).
vote_plural_media_type(M, G, post, application/json) :-
  current_user(User),
  catch(
    (
      http_read_json_dict(Data),
      atom_string(S, Data.id),
      user_tries_to_vote(M, G, User, S, Data.vote)
    ),
    E,
    throw(http_reply(bad_request(E)))
  ),
  reply_json(_{}, [status(201)]).


vote_singular_method(M, G, Vote, Req, get, MTs) :-
  \+ q_instance(M, Vote, blog:'Vote', G), !,
  rest_exception(Req, MTs, 404).
vote_singular_method(M, G, Vote, Req, Method, MTs) :-
  rest_media_type(Req, Method, MTs, vote_singular_media_type(M, G, Vote)).


vote_singular_media_type(M, G, Vote, get, application/'json+ld') :-
  subject_to_jsonld(M, Vote, G, Dict),
  reply_json_dict(Dict).





% HTML %

%! resource_votes(+M, +G, +Resource, -Vote:integer) is det.
%
% Returns the cumulative vote for Resource.

resource_votes(M, G, Resource, Vote) :-
  once(q(M, Resource, blog:downVotes, Down^^xsd:nonNegativeInteger, G)),
  once(q(M, Resource, blog:upVotes, Up^^xsd:nonNegativeInteger, G)),
  Vote is Up - Down.



%! user_tries_to_vote(+M, +G, +User, +Resource, +Vote:integer) is det.

% The user has already voted for this resource.
user_tries_to_vote(M, G, User, Resource, _) :-
  q(M, User, blog:votedFor, Resource, G), !.
% The user has not yet voted for this resource.
user_tries_to_vote(M, G, User, Resource, Vote) :-
  q_transaction(assert_user_vote(M, G, User, Resource, Vote)).


assert_user_vote(_, G, User, Resource, Vote) :-
  (   Vote < 0
  ->  qu_inc(Resource, blog:downVotes, Vote, G),
      Local = 'DownVote'
  ;   Vote > 0
  ->  qu_inc(Resource, blog:upVotes, Vote, G),
      Local = 'UpVote'
  ),
  rdf_global_id(blog:Local, C),
  qb_action(M, C, User, Action, G),
  qb(M, Action, prov:wasGeneratedBy, User, G),
  qb(M, Action, rdf:value, Vote^^xsd:decimal, G),
  qb(M, User, blog:votedFor, Resource, G),
  qb(M, Resource, blog:receivedVote, Action, G).



%! user_vote(+M, +G, +User, +Resource, -Vote:integer) is det.

% The user has already voted for this resource.
user_vote(M, G, User, Resource, Vote) :-
  q(M, User, blog:votedFor, Resource, G), !,
  q(M, Act, prov:wasGeneratedBy, User, G),
  q(M, Resource, blog:hasVote, Act, G),
  (   q_instance(M, Act, blog:'UpVote', G)
  ->  Vote = 1
  ;   q_instance(M, Act, blog:'DownVote', G)
  ->  Vote = -1
  ).
% The user has not yet voted for this resource.
user_vote(_, _, _, _, 0).





% HTML %

%! votes(+M, +G, +Resource, +Author)// is det.

votes(M, G, Resource, Author) -->
  {current_user(User)},
  votes(M, G, Resource, Author, User).


votes(M, G, Resource, User, User) --> !,
  html([
    \number_of_votes(M, G, Resource),
    " ",
    \lstring(votes)
  ]).
votes(M, G, Resource, _, User) -->
  {user_vote(M, G, User, Resource, Vote)},
  html([
    \vote_down(Vote),
    " ",
    \number_of_votes(M, G, Resource),
    " ",
    \vote_up(Vote)
  ]).



%! number_of_votes(+M, +G, +Resource)// is det.

number_of_votes(M, G, Resource) -->
  {resource_votes(M, G, Resource, Votes)},
  html(span([datatype='xsd:integer',property='blog:votes'], Votes)).
