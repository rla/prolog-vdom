:- begin_tests(vdom_build_attrs).
:- use_module(prolog/vdom_build_attrs).

test(build_simple):-
    vdom_build_attrs([className=test], Term),
    assertion(Term = attrs(className("test"))).

test(build_multiple):-
    vdom_build_attrs([className=test, 'data-prolog'=42], Term),
    assertion(Term = attrs(className("test"), 'data-prolog'("42"))).

test(build_empty):-
    vdom_build_attrs([], Term),
    assertion(Term = attrs).

test(build_skipped_attrs):-
    vdom_build_attrs([key=k1, className=test, component=test, data=[1]], Term),
    assertion(Term = attrs(className("test"))).

:- end_tests(vdom_build_attrs).
