:- module(
  sparql_editor,
  [
    sparql_editor/1, % +Req
    sparql_editor//0
  ]
).

/** <module> Quine SPARQL editor

@author Jan Wielemaker
@author Wouter Beek
@version 2016/09-2016/10
*/

:- use_module(library(html/html_ext)).
:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_ext)).
:- use_module(library(http/http_server_files)).
:- use_module(library(http/js_write)).
:- use_module(library(option)).

:- use_module(cp(api/alias)).
:- use_module(cp(api/sparql_query)).
:- use_module(cp(skin/cliopatria)).

:- html_resource(
     css(yasqe),
     [requires([yasqe('yasqe.min.css')]),virtual(true)]
   ).
:- html_resource(
     js(yasqe),
     [requires([yasqe('yasqe.bundled.min.js')]),virtual(true)]
   ).
:- html_resource(
     yasqe,
     [requires([css(yasqe),js(yasqe)]),virtual(true)]
   ).

:- html_resource(
     css(yasr),
     [requires([yasr('yasr.min.css')]),virtual(true)]
   ).
:- html_resource(
     js(yasr),
     [requires([yasr('yasr.bundled.min.js')]),virtual(true)]
   ).
:- html_resource(
     yasr,
     [requires([css(yasr),js(yasr)]),virtual(true)]
   ).

:- http_handler(yasqe(.), serve_files_in_directory(yasqe), [prefix]).
:- http_handler(yasr(.), serve_files_in_directory(yasr), [prefix]).

:- setting(
     handle_id,
     atom,
     '',
     "The HTTP handler for terms in the SPARQL result set."
   ).





sparql_editor(_) :-
  reply_html_page(
    cliopatria([]),
    \cp_title(["SPARQL","Editor"]),
    \sparql_editor
  ).


sparql_editor -->
  {
    setting(handle_id, HandleId),
    http_link_to_id(sparql_query_handler, SparqlEndpoint),
    http_link_to_id(alias_handler, AliasesEndpoint),
    http_link_to_id(HandleId, TermEndpoint)
  },
  html_requires(yasqe),
  html_requires(yasr),
  html([
    div(id=yasqe, []),
    div(id=yasr, [])
  ]),
  js_script({|javascript(SparqlEndpoint,AliasesEndpoint,TermEndpoint)||
window.onload=function(){

  var yasqe = YASQE(document.getElementById("yasqe"), {
    sparql: {endpoint: SparqlEndpoint, showQueryButton: true}
  });

  var serverPrefixes; // TBD: re-fetch if out-of-date?
  
  function usedPrefixes() {
    var prefixmap = yasqe.getPrefixesFromQuery();
    if ( serverPrefixes ) {
      for(var key in serverPrefixes) {
        var yasrKey = key+":";
        if ( !prefixmap[yasrKey] )
          prefixmap[yasrKey] = serverPrefixes[key];
      }
    }
    return prefixmap;
  }

  YASR.plugins.table.defaults.callbacks.onCellClick = function(td, event) {
    var href = YASR.$(td).find("a").attr("href");
    if (href) {
      window.location = TermEndpoint + "?subject=" + encodeURIComponent(href);
      event.preventDefault();
    }
  };

  var yasr = {};

  YASQE.$.ajax({
    url: AliasesEndpoint,
    dataType: "json",
    contentType: 'application/json',
    success: function(data, status) {
      serverPrefixes = data;
    },
    complete: function() {
      yasr = YASR(document.getElementById("yasr"), {
        getUsedPrefixes: usedPrefixes
      });
    }
   });

  // Set some of the hooks to link YASR and YASQE
  yasqe.options.sparql.callbacks.success = function(data, textStatus, xhr) {
    yasr.setResponse({response: data, contentType: xhr.getResponseHeader("Content-Type")});
  };

  yasqe.options.sparql.callbacks.error = function(xhr, textStatus, errorThrown) {
    var exceptionMsg = textStatus + " (response status code " + xhr.status + ")";
    if (errorThrown && errorThrown.length)
      exceptionMsg += ": " + errorThrown;
    yasr.setResponse({exception: exceptionMsg});
  };
};
  |}).
