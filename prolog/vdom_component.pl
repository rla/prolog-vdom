:- module(vdom_component, [
    vdom_component_render/4,  % +Name, +Data, +Children, -VDom
    vdom_component_render/5,  % +Name, +Key, +Data, +Children, -VDom
    vdom_component_exists/1,  % ?Name
    vdom_component_each/4,    % +Name, +DataList, +KeyField, -List
    vdom_component_register/2 % +Name, +Closure
]).

:- use_module(library(error)).
:- use_module(library(debug)).

:- dynamic(registry/2).

% Registers a new component.

:- meta_predicate(vdom_component_register(+, 3)).

vdom_component_register(Name, Closure):-    
    must_be(atom, Name),
    must_be(nonvar, Closure),
    debug(vdom, 'Registering component "~w".', [Name]),
    retractall(registry(Name, _)),
    asserta(registry(Name, Closure)).

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

% Renders component. No key is given.

vdom_component_render(Name, Data, Children, VDom):-
    must_be(atom, Name),
    must_be(nonvar, Data),
    check_component_exists(Name),
    debug(vdom, 'Rendering component "~w"', [Name]),    
    registry(Name, Closure),
    (   call(Closure, Data, Children, VDomComponent)
    ->  component_adjust_root(VDomComponent, Name, Data, VDom)
    ;   throw(error(vdom_component_failed(Name, Data), _))
    ).

% Renders component. Key is given.

vdom_component_render(Name, Key, Data, Children, VDom):-
    must_be(atom, Name),
    must_be(nonvar, Data),
    check_component_exists(Name),
    debug(vdom, 'Rendering keyed component "~w"', [Name]),
    registry(Name, Closure),
    (   call(Closure, Data, Children, VDomComponent)
    ->  component_adjust_root(VDomComponent, Name, Key, Data, VDom)
    ;   throw(error(vdom_component_failed(Name, Data), _))
    ).

sandbox:safe_meta(vdom_component:vdom_component_render(_,_,_,_), [true]).
sandbox:safe_meta(vdom_component:vdom_component_render(_,_,_,_,_), [true]).

% Attaches component name and data onto its
% root tag.

component_adjust_root(VDom, Name, Data, VDomOut):-
    VDom =.. [TagName, Attrs, Body],
    VDomOut =.. [TagName,
        [component=Name, data=Data|Attrs], Body].

% Same as above but also adds key.

component_adjust_root(VDom, Name, Key, Data, VDomOut):-
    VDom =.. [TagName, Attrs, Body],
    VDomOut =.. [TagName,
        [component=Name, key=Key, data=Data|Attrs], Body].

% Helper predicate to create a component call for
% each item in DataList. It does not render the
% components.

vdom_component_each(Name, DataList, KeyField, List):-
    must_be(atom, Name),
    must_be(list, DataList),
    must_be(atom, KeyField),
    check_component_exists(Name),
    maplist(vdom_component_each_create(Name, KeyField), DataList, List).
    
vdom_component_each_create(Name, KeyField, Data, VDom):-
    (   get_dict(KeyField, Data, Key)
    ->  VDom =.. [Name, [key=Key,data=Data], []]
    ;   throw(error(vdom_each_no_key_field(KeyField), _))
    ).
