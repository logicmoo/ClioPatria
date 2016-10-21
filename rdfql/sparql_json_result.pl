/*  Part of ClioPatria SPARQL server

Author:        Jan Wielemaker
E-mail:        J.Wielemaker@uva.nl
WWW:           http://www.swi-prolog.org
Copyright (C): 2009, University of Amsterdam

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this library; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
USA

As a special exception, if you link this library with other files,
compiled with a Free Software compiler, to produce an executable, this
library does not by itself cause the resulting executable to be
covered by the GNU General Public License. This exception does not
however invalidate any other reasons why the executable file might be
covered by the GNU General Public License.
*/

:- module(
  sparql_json_result,
  [
    sparql_write_json_result/3  % +Out, +Result, +Optts
  ]
).

/** <module> Write SPARQL results as JSON

@tbd  Support other SPARQL request results
@author Jan Wielemaker
@author Michiel Hildebrand
@author Wouter Beek
@version 2016/09
*/

:- use_module(library(apply)).
:- use_module(library(dict_ext)).
:- use_module(library(http/http_json)).
:- use_module(library(option)).
:- use_module(library(q/q_term)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(sgml_write)).





sparql_json_mime_type(application/'sparql-results+json; charset=UTF-8').



%! sparql_write_json_result(+Out, +Result, +Opts) is det.
%
% Emit results from a SPARQL query as JSON.
%
% @see http://www.w3.org/TR/rdf-sparql-json-res/

sparql_write_json_result(Out, select(VarTerm, Rows), Opts1) :-
  VarTerm =.. [_|VarNames],
  Dict = _{head: _{vars: VarNames}, results: _{bindings: Bindings}},
  maplist(row_to_json(VarNames), Rows, Bindings),
  (   option(content_type(_), Opts1)
  ->  Opts2 = Opts1
  ;   sparql_json_mime_type(Mime),
      merge_options(Opts1, [content_type(Mime)], Opts2)
  ),
  with_output_to(Out, reply_json_dict(Dict, Opts2)).
sparql_write_json_result(Out, ask(True), Opts1) :-
  Dict = _{head: _{}, boolean: True},
  (   option(content_type(_), Opts1)
  ->  Opts2 = Opts1
  ;   sparql_json_mime_type(Mime),
      merge_options(Opts1, [content_type(Mime)], Opts2)
  ),
  with_output_to(Out, reply_json(Dict, Opts2)).


row_to_json(Vars, Row, Dict) :-
  var_col_bindings(Vars, 1, Row, Pairs),
  dict_pairs(Dict, Pairs).


var_col_bindings([], _, _, []).
var_col_bindings([V0|T0], I1, Row, Bindings) :-
  arg(I1, Row, Val),
  I2 is I1 + 1,
  (   Val = '$null$'    % also catches variables
  ->  var_col_bindings(T0, I2, Row, Bindings)
  ;   rdf_term_to_json(Val, Dict),
      Bindings = [V0-Dict|T],
      var_col_bindings(T0, I2, Row, T)
  ).


rdf_term_to_json(Lit, Dict2) :-
  q_is_literal(Lit), !,
  q_literal_lex(Lit, Lex),
  rdf_literal_to_json(Lit, Dict1),
  Dict2 = Dict1.put(_{type: literal, value: Lex}).
rdf_term_to_json(BNode, _{type: bnode, value: BNode}) :-
  q_is_bnode(BNode), !.
rdf_term_to_json(Iri1, _{type: uri, value: Iri2}) :-
  rdf_global_id(Iri1, Iri2).


rdf_literal_to_json(_@LTag, _{'xml:lang': LTag}) :- !.
rdf_literal_to_json(_^^Type, _{datatype: Type}).



     /*******************************
     *   INTERACTIVE QUERY RESULT  *
     *******************************/

:- multifile
  rdf_io:write_table/4.

rdf_io:write_table(json, _, Rows, Opts) :-
  memberchk(variables(Vars), Opts), !,
  (is_list(Vars) -> VarTerm =.. [vars|Vars] ; VarTerm = Vars),
  sparql_write_json_result(
    current_output,
    select(VarTerm, Rows),
    [content_type(text/plain),Opts]
  ).
