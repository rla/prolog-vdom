:- begin_tests(vdom_component).
:- use_module(prolog/vdom_component).
:- use_module(tests/tests_util).

test(component_simple, [setup(register_dummy),
    cleanup(unregister_all)]):-
    vdom_component_render(dummy([data=hello], []), VOut),
    assertion(VOut = div([className=hello, component=dummy, data=hello], [hello])).

% Key must be passed on to the root.

test(component_keyed, [setup(register_dummy),
    cleanup(unregister_all)]):-
    vdom_component_render(dummy([data=hello, key=k1], []), VOut),
    assertion(VOut = div([className=hello, component=dummy, data=hello, key=k1], [hello])).

% Children passed into the body.

test(component_children, [setup(register_children),
    cleanup(unregister_all)]):-
    vdom_component_render(children([data=[]], [br([], [])]), VOut),
    assertion(VOut = div([className=wrap, component=children, data=[]], [br([], [])])).

% Data-less components are not allowed.

test(component_no_data, [setup(register_dummy),
    cleanup(unregister_all),
    throws(error(vdom_node_no_attr(data), _))]):-
    vdom_component_render(dummy([], []), _).

% Text components are not allowed.

test(component_keyed, [setup(register_text),
    cleanup(unregister_all),
    throws(error(vdom_invalid_component_root(_), _))]):-
    vdom_component_render(text([data=hello], []), _).

:- end_tests(vdom_component).
