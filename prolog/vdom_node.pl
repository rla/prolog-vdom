:- module(vdom_node, [
    vdom_node_type/2,         % +VDom, -Type
    vdom_node_tag/4,          % ?VDom, ?Name, ?Attrs, ?Body
    vdom_node_component/4,    % ?VDom, ?Name, ?Attrs, ?Body
    vdom_node_name/2,         % +VDom, -Name
    vdom_node_key/2,          % +VDom, -Key
    vdom_node_data/2,         % +VDom, -Data
    vdom_node_component/2,    % +VDom, -Name
    vdom_node_attrs/2,        % +VDom, -Attrs
    vdom_node_attr/3,         % +VDom, +Name, -Value
    vdom_node_attrs_update/3, % +VDom, +Update, -VOut
    vdom_node_body/2,         % +VDom, -Body
    vdom_node_has_key/1,      % +VDom
    vdom_node_is_keyed/1      % +VDom
]).

:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(error)).
:- use_module(vdom_component).

% Extracts the key from the Virtual DOM
% node. The node must be a tag, component
% or tag_component.

vdom_node_key(VDom, Key):-
    vdom_node_attr(VDom, key, Key).

% Extracts the data attribute from the Virtual DOM
% node. The node must be a tag_component node.

vdom_node_data(VDom, Data):-
    vdom_node_attr(VDom, data, Data).

% Extracts the component attribute from the Virtual DOM
% node. The node must be a tag_component node.

vdom_node_component(VDom, Data):-
    vdom_node_attr(VDom, component, Data).

% Extracts the attribute value from the Virtual DOM
% node. The node must be a tag, component
% or tag_component.

vdom_node_attr(VDom, Name, ValueOut):-
    vdom_node_attrs(VDom, Attrs),
    (   memberchk(Name=Value, Attrs)
    ->  Value = ValueOut
    ;   throw(error(vdom_node_no_attr(Name), _))
    ).

% Updates the node's attributes with the
% given values.

vdom_node_attrs_update(VDom, Update, VOut):-
    vdom_node_attrs(VDom, Attrs),
    maplist(updated_attr(Update), Attrs, AttrsUpdated),
    include(added_attr(AttrsUpdated), Update, AttrsAdded),
    append(AttrsUpdated, AttrsAdded, AttrsOut),
    vdom_node_name(VDom, Name),
    vdom_node_body(VDom, Body),
    vdom_node_tag(VOut, Name, AttrsOut, Body).

added_attr(Existing, Name=_):-
    \+ memberchk(Name=_, Existing).

updated_attr(Update, Name=_, Name=NewValue):-
    memberchk(Name=NewValue, Update), !.

updated_attr(_, Name=Value, Name=Value).

% Tests whether the Virtual DOM node has the
% key attribute.

vdom_node_has_key(VDom):-
    VDom =.. [_, Attrs, _],
    memberchk(key=_, Attrs).

% Checks whether every Virtual DOM node child has
% the key attribute.

vdom_node_is_keyed(VDom):-
    vdom_node_body(VDom, Body),
    Body \= [],
    forall(member(Child, Body),
        vdom_node_has_key(Child)).

% Extracts the attributes from the VDOM
% node. The node must be a tag, component
% or tag_component.

vdom_node_attrs(VDom, Attrs):-
    VDom =.. [_, Attrs, _], !.

vdom_node_attrs(VDom, _):-
    throw(error(vdom_node_no_attrs(VDom), _)).

% Extracts the body from the VDOM
% node. The node must be a tag, component
% or tag_component.

vdom_node_body(VDom, Body):-
    VDom =.. [_, _, Body], !.

vdom_node_body(VDom, _):-
    throw(error(vdom_node_no_body(VDom), _)).

% Extracts the name from the VDOM
% node. The node must be a tag, component
% or tag_component.

vdom_node_name(VDom, Name):-
    must_be(nonvar, VDom),
    VDom =.. [Name, _, _], !.

vdom_node_name(VDom, _):-
    throw(error(vdom_node_no_name(VDom), _)).

% Virtual-DOM node type, it can be either text, component,
% tag_component or tag. Throws error on invalid node.

vdom_node_type(Atomic, text):-
    atomic(Atomic), !.

% Component looks like a tag, except that
% its compound name is associated with a
% component rendering predicate.

vdom_node_type(Component, component):-
    Component =.. [Name, _, _],
    vdom_component_exists(Name), !.

% Annotated tag referring to a component.
% This is used for remembering that a component
% produced a Virtual-DOM tree with this node
% as a root.

vdom_node_type(Tag, tag_component):-
    Tag =.. [_, Attrs, _],
    memberchk(component=_, Attrs), !.

vdom_node_type(Tag, tag):-
    Tag =.. [_, _, _], !.

vdom_node_type(VDom, _):-
    throw(error(vdom_invalid_node(VDom), _)).

% Bi-directional mapping between tag node and
% its name, attributes and body.

vdom_node_tag(VDom, Name, Attrs, Body):-
    VDom =.. [Name, Attrs, Body].

% Bi-directional mapping between component node and
% its name, attributes and body.

vdom_node_component(VDom, Name, Attrs, Body):-
    VDom =.. [Name, Attrs, Body].
