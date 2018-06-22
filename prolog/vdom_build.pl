:- module(vdom_build, [
    vdom_build/3 % +VDom, -Term, -VDomOut
]).

:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(library(lists)).
:- use_module(vdom_node).
:- use_module(vdom_component).
:- use_module(vdom_build_attrs).

% Generates term that is used by
% patch.js to build an actual DOM node.
% Components might have to be rendered
% so that this predicate also gives out
% the final current VDOM tree.

vdom_build(VDom, Term, VOut):-
    must_be(nonvar, VDom),
    vdom_node_type(VDom, Type),
    vdom_build(Type, VDom, Term, VOut).

vdom_build(text, VDom, Term, VDom):- !,
    atom_string(VDom, Term).

vdom_build(tag, VDom, Term, VOut):- !,
    vdom_node_tag(VDom, Name, Attrs, Body),
    vdom_build_attrs(Attrs, AttrsTerm),
    maplist(vdom_build, Body, BodyBuild, VBodyOut),
    vdom_node_tag(VOut, Name, Attrs, VBodyOut),
    BodyTerm =.. [body|BodyBuild],
    Term =.. [Name, AttrsTerm, BodyTerm].

vdom_build(component, VDom, Term, VOut):- !,
    vdom_component_render(VDom, VRendered),    
    vdom_build(VRendered, Term, VOut).

vdom_build(tag_component, VDom, Term, VOut):- !,
    vdom_build(tag, VDom, Term, VOut).

vdom_build(_, VDom, _, _):-
    throw(error(vdom_build_failed(VDom), _)).
