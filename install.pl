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
                'lib/render/cytoscape_js.pl',
                'examples/test_cytoscape.swinb'
            ]),

    %format(atom(SwishPl), '~w/~w', [SwishDir, 'swish.pl']),
    path(SwishDir, 'swish.pl', SwishPl),
    read_file_to_string(SwishPl, S, []),
    path(InstallDir, 'swish/swish_append.pl', Swish_append),
    read_file_to_string(Swish_append, T, []),
    (   sub_string(S, _,_,_, T)
    ->  writeln('cytoscape_js already installed?')
    ;   open(SwishPl, write, SP),
        format(SP, '~s~n~s', [S, T]),
        close(SP)
    ).

copy_resource(SwishDir, InstallDir, Relative) :-
    %format(atom(SrcPath), '~w/swish/~w', [InstallDir, Relative]),
    %format(atom(DstPath), '~w/~w', [SwishDir, Relative]),
    path(InstallDir, swish/Relative, SrcPath),
    path(SwishDir, Relative, DstPath),
    file_directory_name(DstPath, DstDir),
    make_directory_path(DstDir),
    copy_file(SrcPath, DstPath).

path(D, F, P) :-
    format(atom(P), '~w/~w', [D, F]).
