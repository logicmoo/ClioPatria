:- module(
  blog_generic,
  [
  % API
    resources_by_date_created/4, % +M, +G, +C, -Ress
  % HTML
    blog_context/1,              % +Context
    blog_title//1,               % +Comps
    idle//1,
    ip//1,
    resource_created//3,         % +M, +Resource, +G
    resource_created_sep//3,     % +M, +Resource, +G
    resource_creators//3,        % +M, +Resource, +G
    resource_creators_sep//3,    % +M, +Resource, +G
    social_buttons//3            % +M, +G, +Context
  ]
).

/** <module> ClioPatria blog: Generics

@author Wouter Beek
@version 2016/10
*/

:- use_module(library(html/html_ext)).
:- use_module(library(http/html_write)).
:- use_module(library(pair_ext)).
:- use_module(library(q/q_rdf)).
:- use_module(library(q/q_rdfs)).
:- use_module(library(q/q_term)).
:- use_module(library(rdfa/rdfa_ext)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(settings)).

:- use_module(cp(blog/blog_tag)).
:- use_module(cp(skin/cliopatria)).

:- rdf_meta
   resources_by_date_created(+, r, r, -).

:- setting(
     blog:backend,
     oneof([hdt,trp]),
     trp,
     "The backend from which Quine blog content is loaded."
   ).





% API %

%! resources_by_date_created(+M, +G, +C, -Res) is det.

resources_by_date_created(M, G, C, Ress) :-
  findall(
    DT-Res,
    (
      q_instance(M, Res, C, G),
      dc_created(M, Res, DT, G)
    ),
    Pairs
  ),
  desc_pairs_values(Pairs, Ress).





% HTML %

%! blog_context(+Context) is semidet.

blog_context(article(_)).
blog_context(author(_)).
blog_context(blog(_)).
blog_context(tag(_)).



%! blog_title(+Comps)// is det.

blog_title(T) -->
  cp_title(["Blog"|T]).



%! idle(+Time)// is det.

idle(Time) -->
  {
    Secs is round(Time),
    Min is Secs // 60,
    Sec is Secs mod 60
  },
  html("~`0t~d~2|:~`0t~d~5|"-[Min, Sec]).



%! ip(+Ip)// is det.

ip(ip(A,B,C,D)) --> !,
  html("~d.~d.~d.~d"-[A,B,C,D]).
ip(IP) -->
  html("~w"-[IP]).



%! resource_created(+M, +Resource, +G)// is det.
%! resource_created_sep(+M, +Resource, +G)// is det.

resource_created(M, Resource, G) -->
  icon(time),
  space,
  dc_created(M, Resource, G).


resource_created_sep(M, Resource, G) -->
  resource_created(M, Resource, G), !,
  pipe.
resource_created_sep(_, _, _) --> [].



%! resource_creators(+M, +Resource, +G)// is det.
%! resource_creators_sep(+M, +Resource, +G)// is det.

resource_creators(M, Resource, G) -->
  icon(pen),
  space,
  creators(M, Resource, G).


resource_creators_sep(M, Resource, G) -->
  resource_creators(M, Resource, G), !,
  pipe.
resource_creators_sep(_, _, _) --> [].



%! social_buttons(+M, +G, +Context)// is det.

social_buttons(M, G, Context) -->
  {
    Context = article(Article),
    ground(Article),
    context_link(M, G, Context, Iri, Str), !,
    format(string(Str1), "Deel “~s” op Facebook.", [Str]),
    format(string(Str2), "Deel “~s” op Twitter.", [Str])
  },
  html([
    li(class='social-btn',
      \tooltip(Str1, \fb_share(Iri, Str))
    ),
    li(class='social-btn',
      \tooltip(Str2, \twitter_share(Iri, Str))
    )
  ]).
social_buttons(_, _, _) --> [].


context_link(M, G, article(Article), Article, Str) :-
  dc_title(M, Article, Title, G),
  q_literal_string(Title, Str).
context_link(M, G, author(Author), Author, Str) :-
  q_pref_label(M, Author, Lit, G),
  q_literal_string(Lit, Str).
context_link(M, G, tag(Tag), Tag, Str) :-
  q_pref_label(M, Tag, Lit, G),
  q_literal_string(Lit, Str).
