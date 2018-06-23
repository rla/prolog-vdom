:- use_module(vdom_diff).

render(VDom):-
    nb_getval(clicks, Clicks),
    VDom = span([className=hello], [
        'Hello World', br([], []),
        'You have clicked ', Clicks, ' times.'
    ]).

diff(Diff):-
    render(VDom),
    nb_getval(vdom, VPrev),
    vdom_diff(VDom, VPrev, Diff, VOut),
    write_canonical(Diff), nl,
    nb_linkval(vdom, VOut).

initial_render(Diff):-
    nb_linkval(vdom, ''),
    nb_linkval(clicks, 0),
    diff(Diff).

click(Id, Diff):-
    writeln(Id),
    nb_getval(clicks, Clicks),
    ClicksNext is Clicks + 1,
    nb_setval(clicks, ClicksNext),
    diff(Diff).
