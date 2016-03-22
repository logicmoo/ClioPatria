/*  Part of ClioPatria SeRQL and SPARQL server

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2010 VU University Amsterdam

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

:- module(html_basics,
	  [ cp_table//1,		% :DataRows_2
	    cp_table//2,		% :HeaderRow_2, :DataRows_2
	    cp_table//3,		% :Caption_2, :HeaderRow_2, :DataRows_2
	    cp_table_header//1,		% +Headers
	    cp_table_row//1,		% +Row
	    hidden//2,			% +Name, +Value
	    form_input//2,		% +Label, +Input
	    form_submit//1,		% +Label
	    n//2,			% +Format, +Value
	    nc//2,			% +Format, +Value
	    nc//3,			% +Format, +Value, +Options
	    sort_th//3,			% +Field, +ByNow, :Content
	    insert_html_file//1		% +FileSpec
	  ]).
:- use_module(library(http/html_write)).
:- use_module(library(sgml)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(occurs)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_wrapper)).

:- html_meta((
	cp_table(html, ?, ?),
	cp_table(html, html, ?, ?),
	cp_table(html, html, html, ?, ?),
	form_input(html, html, ?, ?),
	html_maplist(3, +, ?, ?),
	sort_th(+, +, html, ?, ?))).

/** <module> Simple Small HTML components
*/

		 /*******************************
		 *	       FORMS		*
		 *******************************/

%%	hidden(+Name, +Value)// is det.
%
%	Create a hidden input field with given name and value

hidden(Name, Value) -->
	html(input([ type(hidden),
		     name(Name),
		     value(Value)
		   ])).


%%	form_input(+Label, +Input)// is det.
%%	form_submit(+Label)// is det.
%
%	Building blocks for HTML forms. The  form itself is a two-column
%	table of class =form= with labels at  the left and inputs at the
%	right. These rules create rows for input and submit.

form_input(Label, Input) -->
	html(tr([ th(class(label), Label),
		  td(Input)
		])).


form_submit(Label) -->
	html(tr(class(buttons),
		[ th([align(right), colspan(2)],
		     input([ type(submit),
			     value(Label)
			   ]))
		])).


		 /*******************************
		 *	       TABLES		*
		 *******************************/

%! cp_table(:DataRows_2)// is det.

cp_table(DataRows_2) -->
  cp_table(_, DataRows_2).


%! cp_table(:HeaderRow_2, :DataRows_2)// is det.

cp_table(HeaderRow_2, DataRows_2) -->
  cp_table(_, HeaderRow_2, DataRows_2).


%! cp_table(:Caption_2, :HeaerRow_2, :DataRows_2)// is det.

cp_table(Caption_2, HeaderRow_2, DataRows_2) -->
  html(
    table(class=[block,table,'table-condensed','table-striped'], [
      \cp_table_caption(Caption_2),
      \cp_table_header_row(HeaderRow_2),
      tbody(DataRows_2)
    ])
  ).

cp_table_caption(Content_2) --> {var_goal(Content_2)}, !, [].
cp_table_caption(Content_2) --> html(Content_2).

cp_table_header_row(HeaderRow_2) --> {var_goal(HeaderRow_2)}, !, [].
cp_table_header_row(HeaderRow_2) --> html(thead(HeaderRow_2)).

cp_table_row(L) --> html(tr(\html_maplist(cp_table_cell, L))).

cp_table_cell(X) --> html(td(X)).

cp_table_header(L) --> html(tr(\html_maplist(cp_table_header_cell, L))).

cp_table_header_cell(X) --> html(th(X)).


%%	nc(+Format, +Value)// is det.
%%	nc(+Format, +Value, +Options)// is det.
%
%	Numeric  cell.  The  value  is    formatted   using  Format  and
%	right-aligned in a table cell (td).
%
%	@param	Format is a (numeric) format as described by format/2 or
%		the constant =human=.  _Human_ formatting applies to
%		integers and prints then in abreviated (K,M,T) form,
%		e.g., 4.5M for 4.5 million.
%	@param	Options is passed as attributed to the =td= element.
%		Default alignment is =right=.

nc(Fmt, Value) -->
	nc(Fmt, Value, []).

nc(Fmt, Value, Options) -->
	{ class(Value, Class),
	  merge_options(Options,
			[ align(right),
			  class(Class)
			], Opts),
	  number_html(Fmt, Value, HTML)
	},
	html(td(Opts, HTML)).

class(Value, Class) :-
	(   integer(Value)
	->  Class = int
	;   float(Value)
	->  Class = float
	;   Class = value
	).


%%	sort_th(+Field, +ByNow, :Label)
%
%	Provide a column-header for a table   that can be resorted. This
%	call creates a =th= element holding an   =a=. The =a= has either
%	CSS class =sorted= if the column is   sorted  or =resort= if the
%	column can be sorted on this column.   The use of this component
%	demands that the handler processes the parameter =sort_by= using
%	the field-name as argument.
%
%	@param	Field is the field-name that describes the sort on this
%		column.
%	@param	ByNow is the field on which the table is currently
%		sorted.
%	@param  Label is the label of the =a= element

sort_th(Name, Name, Label) -->
	html(th(a([class(sorted)], Label))).
sort_th(Name, _By, Label) -->
	{ http_current_request(Request),
	  http_reload_with_parameters(Request, [sort_by(Name)], HREF)
	},
	html(th(a([href(HREF), class(resort)], Label))).


		 /*******************************
		 *	       META		*
		 *******************************/

%! html_maplist(:Goal_3, +Args1) .

html_maplist(_, []) --> [].
html_maplist(Goal_3, [H|T]) -->
  call(Goal_3, H),
  html_maplist(Goal_3, T).


var_goal(X):- var(X), !.
var_goal(_:X):- var(X).


		 /*******************************
		 *	       NUMBERS		*
		 *******************************/

%%	n(+Format, +Value)//
%
%	HTML component to emit a number.
%
%	@see nc//2 for details.

n(Fmt, Value) -->
	{ number_html(Fmt, Value, HTML) },
	html(HTML).

number_html(human, Value, HTML) :-
	integer(Value), !,
	human_count(Value, HTML).
number_html(Fmt, Value, HTML) :-
	number(Value), !,
	HTML = Fmt-[Value].
number_html(_, Value, '~p'-[Value]).


human_count(Number, HTML) :-
	Number < 1024, !,
	HTML = '~d'-[Number].
human_count(Number, HTML) :-
	Number < 1024*1024, !,
	KB is Number/1024,
	digits(KB, N),
	HTML = '~*fK'-[N, KB].
human_count(Number, HTML) :-
	Number < 1024*1024*1024, !,
	MB is Number/(1024*1024),
	digits(MB, N),
	HTML = '~*fM'-[N, MB].
human_count(Number, HTML) :-
	TB is Number/(1024*1024*1024),
	digits(TB, N),
	HTML = '~*fG'-[N, TB].

digits(Count, N) :-
	(   Count < 100
	->  N = 1
	;   N = 0
	).


		 /*******************************
		 *	   INCLUDE FILES	*
		 *******************************/

%%	insert_html_file(+Specification)//
%
%	Insert the content of an HTML   file  into the current document.
%	Only the content of the =body= element is included.

insert_html_file(Alias) -->
	{ absolute_file_name(Alias, Page, [access(read)]),
	  load_html_file(Page, DOM),
	  contains_term(element(body, _, Body), DOM),
	  Style = element(style, _, _),
	  findall(Style, sub_term(Style, DOM), Styles),
	  append(Styles, Body, Content)
	},
	html(Content).
