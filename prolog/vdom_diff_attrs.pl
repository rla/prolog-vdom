:- module(vdom_diff_attrs, [
    vdom_diff_attrs/4 % +Path, +Attrs, +AttrsPrev, -Diff
]).

:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(vdom_patch_action).

% Calculates attribute change diff.

vdom_diff_attrs(_, Attrs, AttrsPrev, []):-
    Attrs == AttrsPrev, !.

vdom_diff_attrs(Path, Attrs, AttrsPrev, Diff):-
    include(attrs_set(AttrsPrev), Attrs, AttrsSet),
    include(attrs_unset(Attrs), AttrsPrev, AttrsUnset),
    maplist(attrs_set_null, AttrsUnset, AttrsNull),
    append(AttrsNull, AttrsSet, Changes),
    vdom_set_attrs_at(Path, Changes, Diff).

attrs_set_null(Name=_, Name=null).

attrs_unset(Attrs, Name=_):-
    \+ memberchk(Name=_, Attrs).

attrs_set(AttrsPrev, Name=Value):-
    Name \= key,
    Name \= data,
    Name \= component,    
    \+ memberchk(Name=Value, AttrsPrev).
