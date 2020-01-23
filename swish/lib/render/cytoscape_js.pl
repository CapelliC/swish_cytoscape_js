/*  Part of SWISH

    Author:        Carlo Capelli
    E-mail:        cc.carlo.cap@gmail.com

    Copyright (c)  2019, Carlo Capelli
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(swish_render_cytoscape_js,
    [term_rendering//3 % +Term, +Vars, +Options
    ]).

:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(library(error)).
:- use_module(library(http/html_write)).
:- use_module(library(http/js_write)).
:- use_module(library(ugraphs)).
:- use_module('../render').
:- use_module(library(option)).

:- use_module(library(debug)).

%/* debug paths
:- findall(X=Y,file_search_path(X,Y),L),sort(L,S),maplist(writeln,S).
%:- use_module(swish('config-enabled/web/plugin_cytoscape_js')).
%*/
:- use_module(config(plugin_cytoscape_js)).
:- register_renderer(cytoscape_js, "Render graph with CytoscapeJS").
/*
:- setting(swish:cytoscapejs_url, string,
           "https://cdnjs.cloudflare.com/ajax/libs/cytoscape/3.8.2/cytoscape.min.js",
           "Location from where to download CytoscapeJS").
*/

/** <module> SWISH CytoscapeJS based graphs renderer

Render data as an interactive graph.
*/

%%  term_rendering(+Term, +Vars, +Options)//
%
%   Renders Term as a CytoscapeJS graph.
%   I'm pleased to see how easy is to pass JSON to
%   js_script//1, using quasi-quotation
%
term_rendering(Term,_Vars,Options) -->
  {
    %parse_options(Options,OP),
  select_option(width(Width),Options,Options1,'400px'),
  select_option(height(Height),Options1,Options2,'400px'),
  select_option(backgroundColor(BackgroundColor),Options2,Options3,ivory),
  select_option(render_pairs(Render_pairs),Options3,_Options3,true),

    parse_term(Term,Render_pairs,Elements,Style,Layout),
/*    Width=Parsed.width,
    Height=Parsed.height,
    BackgroundColor=Parsed.backgroundColor,
    */
    /*
    select_option(width(Width),Options,Options1,'400px'),
    select_option(height(Height),Options1,Options2,'400px'),
    select_option(backgroundColor(BackgroundColor),Options2,_Options2,ivory),
    debug(swish_render_cytoscape_js, 'options:~w,~w,~w,~w', [Options,Width,Height,BackgroundColor]),
    */
    debug(swish_render_cytoscape_js, 'elements:~w~nstyle:~w~nlayout:~w', [Elements,Style,Layout]),

    CyUrl='/plugin/cytoscape_js/cytoscape.min.js'
    %absolute_file_name(plugin('cytoscape_js/cytoscape.min.js'),CyUrl,[])
    %setting(swish:cytoscapejs_url,CyUrl)
  },
  html(div([ class(['render-cy']),
        'data-render'('As CytoscapeJS graph')
      ],
      [ \js_script({|javascript(CyUrl,Elements,Style,Layout,Width,Height,BackgroundColor)||
(function() {
  var trace = console.log
  if ($.ajaxScript) {

    var cycont = $.ajaxScript.parent()

    // cytoscape needs to know div size
    $(cycont).css('height', Height)
    $(cycont).css('width', Width)

    // just to inspect the render area
    $(cycont).css('background-color', BackgroundColor)

    require([CyUrl], function(cytoscape) {
      trace('running cytoscape', Elements,Style,Layout)
      try {
        var cy = cytoscape({
          container: cycont,
          elements: Elements,
          style: Style,
          layout: Layout,
        })
      }
      catch(e) {
        $(cycont).html(e)
      }
    })
  }
})();
    |})
  ])).

%%  parse_term(+G,+Options,-Elements,-Style,-Layout) is det
%
%   accepts either
%   - a dict with optional
%     -- elements, graph or edges
%     -- style specification
%     -- layout specification
%   - a list of pairs, where pairs are either
%     -- SourceId-TargetId
%     -- SourceId-[TargetId1,TargetId2...]
%
parse_term(G,_Render_pairs,Elements,Style,Layout) :-
  is_dict(G),
  (   G1 = G.get(graph)
  ->  pairs_nodes_edges_to_elements(G1,Elements)
  ;   G1 = G.get(edges)
  ->  pairs_nodes_edges_to_elements(G1,Elements)
  ;   Elements = G.get(elements)
  ),
  (   Style = G.get(style)
  ->  true
  ;   style_default(Style)
  ),
  (   Layout = G.get(layout)
  ->  true
  ;   layout_default(Layout)
  ).

parse_term(G,true,Elements,Style,Layout) :-
  %debug(swish_render_cytoscape_js, 'parse_term ~w ~w', [OP,OP.render_pairs]),
  pairs_nodes_edges_to_elements(G,Elements),
  style_default(Style),
  layout_default(Layout)
  .
/*
parse_options(Options,
              _{width:Width,
                height:Height,
                backgroundColor:BackgroundColor,
                render_pairs:Render_pairs
               }) :-
  select_option(width(Width),Options,Options1,'400px'),
  select_option(height(Height),Options1,Options2,'400px'),
  select_option(backgroundColor(BackgroundColor),Options2,Options3,ivory),
  select_option(render_pairs(Render_pairs),Options3,_Options3,true),
  debug(swish_render_cytoscape_js,
        'options:~w,~w,~w,~w,~w',
        [Options,Width,Height,BackgroundColor,Render_pairs]).



*/
