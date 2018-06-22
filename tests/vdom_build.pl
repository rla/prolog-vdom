:- begin_tests(vdom_build).
:- use_module(prolog/vdom_build).
:- use_module(prolog/vdom_component).
:- use_module(tests/tests_util).

test(build_text):-
    vdom_build(hello, Term, VOut),
    assertion(Term = "hello"),
    assertion(VOut = hello).

test(build_text_number):-
    vdom_build(42, Term, VOut),
    assertion(Term = "42"),
    assertion(VOut = 42).

test(build_tag):-
    vdom_build(div([className=hello], []), Term, VOut),
    assertion(Term = div(attrs(className("hello")), body)),
    assertion(VOut = div([className=hello], [])).

test(build_component, [setup(register_dummy),
    cleanup(unregister_all)]):-
    vdom_build(dummy([data=hello], []), Term, VOut),
    assertion(Term = div(attrs(className("hello")), body("hello"))),
    assertion(VOut = div([className=hello, component=dummy, data=hello], [hello])).

test(build_tag_component):-
    vdom_build(div([component=test, data=[1], className=hello], []), Term, VOut),
    assertion(Term = div(attrs(className("hello")), body)),
    assertion(VOut = div([component=test, data=[1], className=hello], [])).

:- end_tests(vdom_build).
