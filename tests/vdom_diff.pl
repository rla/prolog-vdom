% TODO test keyed node attrs diff.
:- begin_tests(vdom_diff).
:- use_module(prolog/vdom_diff).
:- use_module(prolog/vdom_component).
:- use_module(tests/tests_util).

test(diff_text):-
    vdom_diff(hello, hello, Diff, VOut),
    assertion(Diff = []),
    assertion(VOut = hello).

test(diff_text_replace):-
    vdom_diff(hello, world, Diff, VOut),
    assertion(Diff = [replace(path, "hello")]),
    assertion(VOut = hello).

test(diff_text_replace_deep):-
    vdom_diff(
        div([], [hello]),
        div([], [world]),
        Diff, VOut),
    assertion(Diff = [replace(path(0), "hello")]),
    assertion(VOut = div([], [hello])).

test(diff_tag):-
    vdom_diff(
        div([], []),
        div([], []),
        Diff, VOut),
    assertion(Diff = []),
    assertion(VOut = div([], [])).

test(diff_tag_replace_text):-
    vdom_diff(
        div([], []),
        hello,
        Diff, VOut),
    assertion(Diff = [replace(path, div(attrs, body))]),
    assertion(VOut = div([], [])).

test(diff_tag_replace_tag):-
    vdom_diff(
        div([], []),
        span([], []),
        Diff, VOut),
    assertion(Diff = [replace(path, div(attrs, body))]),
    assertion(VOut = div([], [])).

test(diff_keyed_same):-
    vdom_diff(
        div([key=k1], []),
        div([key=k1], []),
        Diff, VOut),
    assertion(Diff = []),
    assertion(VOut = div([key=k1], [])).

test(diff_keyed_reorder):-
    vdom_diff(
        span([], [span([key=k1], []), span([key=k2], [])]),
        span([], [span([key=k2], []), span([key=k1], [])]),
        Diff, VOut),
    assertion(Diff = [roc(path, actions(reuse(1), reuse(0)))]),
    assertion(VOut = span([], [span([key=k1], []), span([key=k2], [])])).

test(diff_keyed_reorder_and_attrs):-
    vdom_diff(
        span([className=world], [
            span([key=k1], []), span([key=k2], [])
        ]),
        span([className=hello],
            [span([key=k2], []), span([key=k1], [])
        ]),
        Diff, VOut),
    assertion(Diff = [
        set_attrs(path, attrs(className("world"))),
        roc(path, actions(reuse(1), reuse(0)))
    ]),
    assertion(VOut = span([className=world], [
        span([key=k1], []), span([key=k2], [])
    ])).

test(diff_attrs):-
    vdom_diff(
        div([className=class2], []),
        div([className=class1], []),
        Diff, VOut),
    assertion(Diff = [set_attrs(path, attrs(className("class2")))]),
    assertion(VOut = div([className=class2], [])).

:- end_tests(vdom_diff).
