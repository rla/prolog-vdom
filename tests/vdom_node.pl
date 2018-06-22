:- begin_tests(vdom_node).
:- use_module(prolog/vdom_node).
:- use_module(prolog/vdom_component).
:- use_module(tests/tests_util).

test(text_node):-
    vdom_node_type(hello, Type),
    assertion(Type = text).

test(text_node_number):-
    vdom_node_type(42, Type),
    assertion(Type = text).

test(text_node_string):-
    vdom_node_type("hello", Type),
    assertion(Type = text).

test(component_node, [setup(register_dummy),
    cleanup(unregister_all)]):-
    vdom_node_type(dummy([data='Hello'], []), Type),
    assertion(Type = component).

test(tag_node):-
    vdom_node_type(div([className=test], []), Type),
    assertion(Type = tag).

test(tag_node):-
    vdom_node_type(div([className=test], []), Type),
    assertion(Type = tag).

% Tag annotated with a component.
test(tag_component):-
    vdom_node_type(div([component=dummy], []), Type),
    assertion(Type = tag_component).

% Shorthands are not allowed at the moment.
test(invalid_node, [throws(error(vdom_invalid_node(_), _))]):-
        vdom_node_type(div([className=test]), _).

:- end_tests(vdom_node).
