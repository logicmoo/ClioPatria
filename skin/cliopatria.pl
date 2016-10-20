/*  Part of ClioPatria SPARQL server

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2010, University of Amsterdam,
		   VU University Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(cp_skin,
	  [ server_address//1,		% +Component
      cp_head_generics//0,
      cp_logo/1,           % -Local
      cp_navbar_brand//0,
      cp_page/4,           % +Context, +Spec, +Title, :Content_0
      cp_title//1,         % +Comps
	    current_page_doc_link//0
	  ]).
:- use_module(library(debug)).
:- use_module(library(doc_http)). % Load plDoc.
:- use_module(library(http/html_write)).
:- use_module(library(http/html_head)).
:- use_module(library(http/http_wrapper)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_path)).
:- use_module(library(http/jquery)).
:- use_module(library(pldoc/doc_index)). % plDoc search menu.
:- use_module(library(settings)).
:- use_module(library(version)).

:- use_module(library(default)).
:- use_module(library(html/html_ext)).
:- use_module(library(http/http_ext)).
:- use_module(library(pair_ext)).

:- use_module(components(simple_search)).
:- use_module(applications(help/version)).
:- use_module(user/user_db).
:- include(library(pldoc/hooks)).

/** <module> ClioPatria skin

This page defines the overall layout of  ClioPatria pages. All pages are
returned using reply_html_page/3, using the   page class cliopatria(Id),
where Id is currently  always  =default=.   Pages  can  be  redefined by
providing a rule for user:body//2, where   the first argument must unify
with the page class.

The default skin provides the overall menu,   a  simple search form, the
content and the `server-address'. Because the   search-form uses the YUI
autocomplete widgets, the body must include class =|yui-skin-sam|=.  The
default body has the classes =|yui-skin-sam|= and =cliopatria=.

The default skin provided by this can be overruled using two hooks:

	$ cliopatria:page_body//1 :
	Emit a page from the given content.  This hook can be used to modify
	the overall page layout beyond what can be achieved with CSS.
	$ cliopatria:server_address//0 :
	Write the address of the server.

This   library   also   provides   building     blocks,    notably   for
server_address//0:

	$ server_address//1 :
	Presents the version info and a link to a GIT module.
	$ current_page_doc_link//0 :
	Presents a link to the documentation of a page if the
	self-documentation facilities are loaded.  See run.pl.in.

The CSS file css('cliopatria.css') contains the ClioPatria style that is
makes ClioPatria look pretty to our  eyes,   but  is  not essential. The
plugin config-available/fix_menu.pl contains example code  to extend the
ClioPatria skin.
*/


%%	user:body(+Style, :Body)// is det.
%
%	The multi-file implementation defines the overall layout of HTML
%	pages with the Style cliopatria(_).

:- html_meta
   cp_page(+, +, +, html).

:- multifile
	cp:head//2,
	cp:body//2,
	user:head//2,
	user:body//2.

user:head(Context, Content) -->
  cp:head(Context, Content), !.
user:head(cliopatria(_), Content) --> !,
  html(head([\cp_head_generics|Content])).

user:body(Context, Content) -->
  cp:body(Context, Content), !.
% Overrule default style with cliopatria:page_body//1.
user:body(cliopatria(_), Content) -->
	cliopatria:page_body(Content), !.
% Overrule default style with cliopatria:page_body//2.
user:body(cliopatria(Attrs), Content) -->
	cliopatria:page_body(cliopatria(Attrs), Content), !.
% Default style.
user:body(cliopatria(Attrs), Content) -->
  html(
    body(Attrs, [
      \cp_navbar,
      \row_1([class=main], 12, Content),
      \cp_footer
    ])
  ).
% Wiki style.
user:body(pldoc(wiki), Content) -->
	{absolute_file_name(cliopatria(.), Dir, [access(read),file_type(directory)])},
  user:body(cliopatria([]), [\doc_links(Dir, [])|Content]).
% Documentation style.
user:body(pldoc(_), Content) -->
	user:body(cliopatria([]), Content).



/* HEAD */

%! cp_head_generics// is det.

cp_head_generics -->
  {
    cp_logo(Local),
    setting(cliopatria:title, Title),
    setting(cliopatria:subtitle, Subtitle)
  },
  meta_charset,
  meta_ie_latest,
  meta_viewport,
  meta_authors,
  favicon(img(Local)),
  html_requires(html_ext),
  meta_description([Title,": ",Subtitle]).



/* FOOTER */

%! cp_footer// is det.

cp_footer -->
  html(
    footer(class=main,
      \row_3(
        [3,3,3,6], \cp_footer_contact,
        [3,3,3,6], \cp_footer_links,
        [6,6,6,12], \cp_footer_panel
      )
    )
  ).


cp_footer_contact -->
  html([
    h1("Contact"),
    ul([
      li(\pl_link),
      li(
        \external_link(
          'http://mailman.few.vu.nl/mailman/listinfo/cliopatria-list',
          "ClioPatria mailinglist"
        )
      ),
      li(
        \external_link(
          'http://www.swi-prolog.org/Mailinglist.html',
          "SWI-Prolog mailinglist"
        )
      )
    ])
  ]).


cp_footer_links -->
  html([
    h1("Apps"),
    ul([
      li(\external_link('http://lodlaundromat.org', "LOD Laundromat")),
      li(\external_link('http://lodsearch.org', "LOD Search")),
      li(\external_link('http://semblog.org', "SemBlog"))
    ])
  ]).


cp_footer_panel -->
  {
    cp_logo(Local),
    setting(cliopatria:title, Title)
  },
  footer_panel(img(Local), Title, \cp_license).


cp_license -->
  html(
    div(class=license,
      p(["Licensed under the ",\license(cc_by),"."])
    )
  ).



/* LOGO */

%! cp_logo(-Local) is det.
%
% The local file name of the web site logo.  This is either the
% ClioPatria logo or a custom brand logo set with hook logo/1.

cp_logo(Local) :-
  (cliopatria:logo(Base) -> true ; Base = logo),
  file_name_extension(Base, svg, Local).



/* NAVIGATION BAR */

cp_navbar -->
  navbar(
    \cp_navbar_brand,
    [
      \menu,
      \simple_search_form([value(p(q))]),
      \html_receive(cp_navbar_right)
    ],
    \cp_user_menu
  ).


cp_navbar_brand -->
  {
    cp_logo(Local),
    setting(cliopatria:title, Title),
    format(string(Alt), "~s logo", [Title])
  },
  navbar_brand_img(img(Local), [alt=Alt]).



/* PAGE */

%! cp_page(+Context, +Spec, +Title, :Content_0) is det.
%
% Generates a simple Quine HTML page.

cp_page(Context, Spec, Title, Content_0) :-
  reply_html_page(
    Context,
    \cp_title([Title]),
    article([
      header(\internal_link(Spec, h1(Title))),
      \row_3(
        [2,1,0,0], [],
        [8,10,12,12], div(property='sioc:content', Content_0),
        [2,1,0,0], []
      )
    ])
  ).



/* TITLE */

%! cp_title(+Comps)// is det.

cp_title(Comps) -->
  {setting(cliopatria:title, Title)},
  title([Title|Comps]).



/* USER MENU */

%! cp_user_menu// is det.
%
% The HTML menu that represents information about the currently
% authenticated user.  It also provides access to user settings and
% preferences.

cp_user_menu -->
  {logged_on(User)}, !,
  {
    findall(
      Minor-menu_item(Handle,Lbl),
      html:menu_item(user, Minor, Handle, Lbl),
      Pairs
    ),
    asc_pairs_values(Pairs, MinorNodes)
  },
  html(
    ul(class=[nav,'navbar-nav','navbar-right'],
      \dropdown_menu(
        [id='user-menu'],
        \cp_user_top_item(User),
        cp_user_item,
        MinorNodes
      )
    )
  ).
cp_user_menu -->
  form(
    link_to_id(user_login),
    [class=['navbar-form','navbar-right'],id='user-menu'],
    \submit_button(\internal_link(link_to_id(user_login), "Login"))
  ).


cp_user_item(menu_item(Handle,Lbl)) -->
  internal_link(link_to_id(Handle), Lbl).


cp_user_top_item(User) -->
  {user_name(User, Name)},
  html([
    \image(
      'http://1.bp.blogspot.com/-OMvShGh7yUk/Ufayj2SStWI/AAAAAAAAIPQ/zOuF-vstmGk/s1600/quine.jpeg',
      [alt=Name]
    ),
    Name
  ]).


user_name(User, Name) :-
  user_property(User, realname(Name)), !.
user_name(_, "My account").



/* ADDRESS */

%%	address//
%
%	Emit an element =address= with   class  =cliopatria=. This first
%	class  the  hook  cliopatria:server_address//0.  If  this  hooks
%	fails, it calls server_address('ClioPatria').
%
%	@see version.pl

address -->
	cliopatria:server_address, !.
address -->
	server_address('ClioPatria').


%%	server_address(+Component)//
%
%	HTML component that emits the   default ClioPatria address link.
%	This provides a link to the ClioPatria   home page and the (GIT)
%	version information. ClioPatria  is  registered   with  the  GIT
%	module =|ClioPatria|= and the default server address is provided
%	by calling:
%
%	    ==
%		...,
%		server_address('ClioPatria'),
%		...
%	    ==
%
%	@see register_git_module/2 for registering a GIT module.

server_address(Component) -->
	html([ address(class(footer),
		       [ \component_address(Component),
			 \current_page_doc_link
		       ])
	     ]).

%%	component_address(+Name)//
%
%	The label ClioPatria as a link to its home-page on the web.

component_address(Component) -->
	(   { git_module_property(Component, home_url(Home)) }
	->  html(a([ class(home), href(Home),
		     title(Component+' home')
		   ], Component))
	;   html(span(class(home), Component))
	),
	html(' (version '),
	component_version(Component),
	html(')').


%%	component_version(+Name)//
%
%	Give verion information and link to detailed version info

component_version(Component) -->
	{ (   git_module_property(Component, version(Version))
	  ->  true
	  ;   Version = 'no GIT?'
	  ),
	  http_link_to_id(version_info, [], VREF)
	},
	html(a([title('About versions'),
		class(version),
		href(VREF)],
	       Version)).



/* DOCUMENTATION */

%%	current_page_doc_link//
%
%	Create a link to  the  documentation   (and  from  there  to the
%	implementation) of this page. This link   is created only if the
%	library applications(help/http_help) is loaded.

:- if(current_predicate(http_help:page_documentation_link//1)).
current_page_doc_link -->
	{ http_current_request(Request) },
	http_help:page_documentation_link(Request).
:- else.
current_page_doc_link --> [].
:- endif.
