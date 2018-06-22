:- module(vdom_component, [
    vdom_component_render/2,   % +VDom, -VRendered
    vdom_component_exists/1,   % ?Name
    vdom_component_each/4,     % +Name, +DataList, +KeyField, -List
    vdom_component_register/2, % +Name, +Closure
    vdom_component_clear/0
]).

:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(library(lists)).
:- use_module(vdom_node).

:- dynamic(registry/2).

% Registers a new component.

:- meta_predicate(vdom_component_register(+, 3)).

vdom_component_register(Name, Closure):-    
    must_be(atom, Name),
    must_be(nonvar, Closure),
    retractall(registry(Name, _)),
    asserta(registry(Name, Closure)).

% Removes all registered components.

vdom_component_clear:-
    retractall(registry(_, _)).

% Checks whether a component with
% the given name exists.

vdom_component_exists(Name):-
    registry(Name, _).

% Checks whether the component exists,
% throws error if it does not.

check_component_exists(Name):-
    registry(Name, _), !.

check_component_exists(Name):-
    throw(error(vdom_no_component(Name), _)).

% Renders the component. It calls the render
% predicate for the component and then post-
% processes the rendered VDOM.

vdom_component_render(VDom, VOut):-
    vdom_node_name(VDom, Name),
    vdom_node_data(VDom, Data),
    vdom_node_body(VDom, Children),
    check_component_exists(Name),
    registry(Name, Closure),
    (   call(Closure, Data, Children, VRendered)
    ->  component_post(VDom, VRendered, VOut)
    ;   throw(error(vdom_component_failed(VDom), _))
    ).

% Postprocesses the component. The rendered
% VDOM must have a single root and the root
% must be a tag or a component node.
% The root is annoted using the attributes:
% component, data and key. The key attribute is
% missing if the original component VDOM
% node had no key.

component_post(_, VRendered, _):-
    vdom_node_type(VRendered, Type),
    Type = text, !,
    throw(error(vdom_invalid_component_root(VRendered), _)).

component_post(VDom, VRendered, VOut):-
    vdom_node_name(VDom, Name),
    vdom_node_data(VDom, Data),
    (   vdom_node_has_key(VDom)
    ->  vdom_node_key(VDom, Key),
        AttrsUpdate = [component=Name, data=Data, key=Key]
    ;   AttrsUpdate = [component=Name, data=Data]
    ),
    vdom_node_attrs_update(VRendered, AttrsUpdate, VOut).

% Helper predicate to create a component call for
% each item in DataList. It does not render the
% components.

vdom_component_each(Name, DataList, KeyField, List):-
    must_be(atom, Name),
    must_be(list, DataList),
    must_be(atom, KeyField),
    check_component_exists(Name),
    maplist(each_create(Name, KeyField), DataList, List).

each_create(Name, KeyField, Data, VDom):-
    (   get_dict(KeyField, Data, Key)
    ->  VDom =.. [Name, [key=Key,data=Data], []]
    ;   throw(error(vdom_each_no_key_field(KeyField), _))
    ).
