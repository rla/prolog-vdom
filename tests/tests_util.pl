:- module(tests_util, [
    register_dummy/0,
    register_text/0,
    register_children/0,
    unregister_all/0
]).

:- use_module(prolog/vdom_component).

register_dummy:-
    vdom_component_register(dummy, dummy_component).

register_text:-
    vdom_component_register(text, text_component).

register_children:-
    vdom_component_register(children, children_component).

unregister_all:-
    vdom_component_clear.

dummy_component(Text, _, div([className=hello], [Text])).

% Text components are not allowed.

text_component(_, _, hello).

% Component that wraps its children

children_component(_, Children, div([className=wrap], Children)).
