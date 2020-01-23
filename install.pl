/*  File:    install.pl
    Author:  Carlo,,,
    Created: Jan 22 2020
    Purpose: copy files and folders to swish target directory
*/

:- module(install, []).

:- initialization copy_resources.

copy_resources :-

    % adjust this path to your SWISH installation directory
    SwishRoot = '~/swish',

    expand_file_name(SwishRoot, [SwishDir]),
    assertion(exists_directory(SwishDir)),

    module_property(install, file(Install)),
    file_directory_name(Install, InstallDir),

    maplist(copy_resource(SwishDir, InstallDir), [
                'config-enabled/web/plugin/cytoscape_js/cytoscape.min.js',
                'config-enabled/plugin_cytoscape_js.pl',
                'lib/render/cytoscape_js.pl',
                'examples/test_cytoscape.swinb',
                'examples/CytoscapeJS_renderer.swinb',
                'examples/map_coloring_with_CytoscapeJS.swinb'
            ]),

    path(SwishDir, 'swish.pl', SwishPl),
    read_file_to_string(SwishPl, S, []),
    path(InstallDir, 'swish/swish_append.pl', Swish_append),
    read_file_to_string(Swish_append, T, []),
    (   sub_string(S, _,_,_, T)
    ->  writeln('cytoscape_js already installed?')
    ;   open(SwishPl, write, SP),
        format(SP, '~s~n~s', [S, T]),
        close(SP)
    ),

    path(SwishDir, 'examples/prolog_tutorials.swinb', ExProTutPath),
    
    open(ExProTutPath, append, S_ExProTutPath),
    format(S_ExProTutPath, '~n~s~n', [`
<div class="notebook">
<div class="nb-cell markdown">
---
## CytoscapeJS renderer

These notebooks show the basics of CytoscapeJS

  - [Test installation](example/test_cytoscape.swinb)
  - [Options and style](example/CytoscapeJS_renderer.swinb)
  - [Map coloring](example/map_coloring_with_CytoscapeJS.swinb)
  
</div>
</div>
`]),

    writeln('install done.').

copy_resource(SwishDir, InstallDir, Relative) :-
    path(InstallDir, swish/Relative, SrcPath),
    path(SwishDir, Relative, DstPath),
    file_directory_name(DstPath, DstDir),
    make_directory_path(DstDir),
    copy_file(SrcPath, DstPath).

path(D, F, P) :-
    format(atom(P), '~w/~w', [D, F]).
