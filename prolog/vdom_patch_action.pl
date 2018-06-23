:- module(vdom_patch_action, [
    vdom_replace_at/4,   % +Path, +VDom, -Diff, -VOut
    vdom_set_attrs_at/3, % +Path, +Changes -Diff
    vdom_roc_at/4        % +Path, +Rebuild, -Diff, -VOut
]).

:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(library(lists)).
:- use_module(vdom_build).
:- use_module(vdom_build_attrs).

% Generates attributes update action at the given path.
% Empty set of attributes generates no action.

vdom_set_attrs_at(_, [], []).

vdom_set_attrs_at(Path, Changes, Diff):-
    must_be(nonvar, Path),
    must_be(nonvar, Changes),
    path_term(Path, PathTerm),
    vdom_build_attrs(Changes, AttrsTerm),
    Diff = [set_attrs(PathTerm, AttrsTerm)].

% Generates replace action at the given path.
% As this can render components through vdom_build,
% it will give back the final current vdom.

vdom_replace_at(Path, VDom, Diff, VOut):-
    must_be(nonvar, Path),
    must_be(nonvar, VDom),
    vdom_build(VDom, NodeTerm, VOut),
    path_term(Path, PathTerm),
    Diff = [replace(PathTerm, NodeTerm)].

% Generates reorder-or-create (ROC) action.
% ROC action specifies for each child element
% whether to use a previous child or create a new one.
% Output VDOM contains reuse(Index) at the
% place where an old child is being reused.

vdom_roc_at(Path, Rebuild, Diff, VOut):-
    must_be(nonvar, Path),
    must_be(nonvar, Rebuild),
    path_term(Path, PathTerm),
    maplist(roc_action, Rebuild, Actions, VOut),
    ActionsTerm =.. [actions|Actions],
    Diff = [roc(PathTerm, ActionsTerm)].

% Reuse maps to a special term reuse(Index).

roc_action(reuse(Index), reuse(Index), reuse(Index)):- !.

% New node creation maps to term create(Term).

roc_action(create(Node), create(Term), VDom):- !,
    vdom_build(Node, Term, VDom).

roc_action(Action, _, _):-
    throw(error(vdom_invalid_roc_action(Action), _)).

path_term(Path, Term):-
    must_be(list, Path),
    reverse(Path, Reversed),
    Term =.. [path|Reversed].
