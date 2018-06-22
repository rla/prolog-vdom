:- begin_tests(vdom_patch_action).
:- use_module(prolog/vdom_patch_action).

test(patch_text_root):-
    vdom_replace_at([], hello, Term, VOut),
    assertion(Term = [replace(path, "hello")]),
    assertion(VOut = hello).

test(patch_text_deep):-
    vdom_replace_at([0, 2], hello, Term, VOut),
    assertion(Term = [replace(path(2, 0), "hello")]),
    assertion(VOut = hello).

test(patch_attrs):-
    vdom_set_attrs_at([0, 2], [className=hello], Term),
    assertion(Term = [set_attrs(path(2, 0), attrs(className("hello")))]).

test(patch_roc_reuse):-
    vdom_roc_at([0, 2], [reuse(1), reuse(0)], Term, VOut),
    assertion(Term = [roc(path(2, 0), actions(reuse(1), reuse(0)))]),
    assertion(VOut = [reuse(1), reuse(0)]).

test(patch_roc_create):-
    vdom_roc_at([0, 2], [
        reuse(1), create(div([], []))], Term, VOut),
    assertion(Term = [roc(path(2, 0), actions(
        reuse(1), create(div(attrs, body))))]),
    assertion(VOut = [reuse(1), div([], [])]).

:- end_tests(vdom_patch_action).
