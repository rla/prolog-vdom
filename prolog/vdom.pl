:- module(vdom, [
    vdom_diff/4 % +VDom, +VDomPrev, -Diff, -VDomOut
]).

:- use_module(library(error)).
:- use_module(vdom_component).

vdom_diff(VDom, VDomPrev, Diff, VDomOut):-
    must_be(var, Diff),
    must_be(var, VDomOut),
    debug(vdom, 'Calculating diff', []),
    vdom_diff(VDom, VDomPrev, [], Diff, VDomOut),
    debug(vdom, 'Finished calculating diff', []).

vdom_diff(VDom, VDomPrev, Path, Diff, VDomOut):-    
    vdom_node_type(VDom, Type),
    vdom_node_type(VDomPrev, TypePrev),
    debug(vdom, 'Calculating diff at ~w, vdom type is ~w and previous type was ~w',
        [Path, Type, TypePrev]),
    vdom_diff(Type, TypePrev, VDom, VDomPrev, Path, Diff, VDomOut),
    debug(vdom, 'Calculating diff at ~w is finished', [Path]).

vdom_diff(text, text, VDom, VDom, _, [], VDom):- !.

vdom_diff(text, _, VDom, _, Path, Diff, VDomOut):- !,
    diff_replace_at(Path, VDom, Diff, VDomOut).

vdom_diff(component, tag_component, VDom, VDomPrev, Path, Diff, VDomOut):- !,
    vdom_node_tag_name(VDom, ComponentName),
    vdom_node_tag_component(VDomPrev, ComponentNamePrev),
    vdom_diff_component(ComponentName, ComponentNamePrev, VDom, VDomPrev, Path, Diff, VDomOut).

vdom_diff(component, _, VDom, _, Path, Diff, VDomOut):- !,
    diff_replace_at(Path, VDom, Diff, VDomOut).

vdom_diff(tag_component, tag, VDom, VDomPrev, Path, Diff, VDomOut):- !,
    vdom_diff(tag, tag, VDom, VDomPrev, Path, Diff, VDomOut).

% This is reached only when new vdom was a component?
% Rendering changes component -> tag_component.

vdom_diff(tag_component, tag_component, VDom, VDomPrev, Path, Diff, VDomOut):- !,
    vdom_diff(tag, tag, VDom, VDomPrev, Path, Diff, VDomOut).

vdom_diff(tag, tag_component, VDom, VDomPrev, Path, Diff, VDomOut):- !,
    vdom_diff(tag, tag, VDom, VDomPrev, Path, Diff, VDomOut).

vdom_diff(tag, tag, VDom, VDomPrev, Path, Diff, VDomOut):- !,
    vdom_node_tag_name(VDom, TagName),
    vdom_node_tag_name(VDomPrev, TagNamePrev),
    vdom_diff_tag(TagName, TagNamePrev, VDom, VDomPrev, Path, Diff, VDomOut).

vdom_diff(tag, _, VDom, _, Path, Diff, VDomOut):- !,
    diff_replace_at(Path, VDom, Diff, VDomOut).

vdom_diff(Type, TypePrev, _, _, _, _, _):-
    throw(error(vdom_unhandled_diff_case(Type, TypePrev, _), _)).

% Component-tag correspondence.
% Both refer to same-named component.

vdom_diff_component(Name, Name, VDom, VDomPrev, Path, Diff, VDomOut):- !,
    VDom =.. [_, Attrs, Body],
    vdom_node_data(VDom, Data),
    vdom_node_data(VDomPrev, DataPrev),
    (   Data == DataPrev
    ->  Diff = [],
        VDomOut = VDomPrev,
        debug(vdom, 'Skipping rendering component "~w" at ~w', [Name, Path])
    ;   (   memberchk(key=Key, Attrs)
        ->  writeln(key(Key)),
            vdom_component_render(Name, Key, Data, Body, VDomRendered)
        ;   vdom_component_render(Name, Data, Body, VDomRendered)
        ),
        writeln(vdom_rendered(VDomRendered)),
        vdom_diff(VDomRendered, VDomPrev, Path, Diff, VDomOut),
        writeln(component_in(VDom)),
        writeln(component_out(VDomOut))
    ).

% Component-tag correspondence.
% Both refer to different-named component.

vdom_diff_component(_, _, VDom, _, Path, Diff, VDomOut):- !,
    diff_replace_at(Path, VDom, Diff, VDomOut).

% Same tag name, body is inspected.
% If both bodies are keyed then swap logic
% is attempted.

vdom_diff_tag(Name, Name, VDom, VDomPrev, Path, Diff, VDomOut):- !,
    (   vdom_body_is_keyed(VDom),
        vdom_body_is_keyed(VDomPrev)
    ->  vdom_diff_tag_keyed(VDom, VDomPrev, Path, Diff, VDomOut)
    ;   vdom_diff_tag_general(VDom, VDomPrev, Path, Diff, VDomOut)
    ).

% Different tag name, whole node is replaced.

vdom_diff_tag(_, _, VDom, _, Path, Diff, VDomOut):-
    diff_replace_at(Path, VDom, Diff, VDomOut).

% Keyed children replacement. First we reorder
% the body while adding new elements, then we
% perform modifications on the old child elements.
% Each direct child node has guaranteed to have
% key at this point.

vdom_diff_tag_keyed(VDom, VDomPrev, Path, Diff, VDomOut):-
    debug(vdom, 'Using keyed diff at ~w', [Path]),
    VDom =.. [Name, Attrs, Body],
    vdom_node_tag_body(VDomPrev, BodyPrev),
    length(Body, Length),
    length(BodyPrev, LengthPrev),
    (   Length = LengthPrev
    ->  (   vdom_list_has_same_keys(Body, BodyPrev) % no reordering will be performed
        ->  vdom_diff_tag_general_body(Body, BodyPrev, 0, Path, Diff, VDomBodyOut), % handle as a general tag body
            VDomOut =.. [Name, Attrs, VDomBodyOut]
        ;   vdom_diff_tag_keyed_rebuild(VDom, VDomPrev, Path, Diff, VDomOut) % reordering and/or modifications
        )
    ;   debug(vdom, 'Keyed diff at ~w reorders and modifies children (diff length)', [Path]),
        vdom_diff_tag_keyed_rebuild(VDom, VDomPrev, Path, Diff, VDomOut) % reordering and/or modifications
    ).
    

% Reorders child nodes, adds or replaces new
% nodes where needed.

vdom_diff_tag_keyed_rebuild(VDom, VDomPrev, Path, Diff, VDomOut):-
    VDom =.. [Name, Attrs, Body],
    VDomOut =.. [Name, Attrs, VDomOutBody],
    vdom_keyed_assoc(VDomPrev, Assoc),    
    vdom_node_tag_body(VDomPrev, BodyPrev),
    tag_keyed_rebuild(Body, Assoc, Rebuild),
    diff_reorder_or_create_at(Path, Rebuild, DiffRebuild, VDomRebuild),
    diff_after_reorder(Body, BodyPrev, 0, Path, Rebuild, VDomRebuild, DiffAfter, VDomOutBody),
    append(DiffRebuild, DiffAfter, Diff).

% Finds diff after tag children have been reordered.
% TODO: write into tail-recursive form.

diff_after_reorder([Head|Tail], BodyPrev, Index, Path, Rebuild, VDomRebuild, Diff, VDomOut):-
    IndexNext is Index + 1,
    diff_after_reorder(Tail, BodyPrev, IndexNext, Path, Rebuild, VDomRebuild, DiffNext, VDomOutTail),
    (   nth0(Index, Rebuild, reuse(IndexInPrev)) % tag was reused
    ->  nth0(IndexInPrev, BodyPrev, HeadPrev),
        vdom_diff(Head, HeadPrev, [Index|Path], DiffHead, VDomOutHead), % diff with reused one
        append(DiffHead, DiffNext, Diff)        
    ;   Diff = DiffNext,
        nth0(Index, VDomRebuild, VDomOutHead) % freshly build vdom
    ),
    VDomOut = [VDomOutHead|VDomOutTail].

diff_after_reorder([], _, _, _, _, _, [], []).

% Builds new body for the tag. Either reuses
% old body elements or adds new ones.

tag_keyed_rebuild([Node|Nodes], Assoc, DiffRebuild):-
    vdom_node_tag_key(Node, Key),
    (   get_assoc(Key, Assoc, Index)
    ->  debug(vdom, 'Keyed diff reuses child ~w for key "~w"', [Index, Key]),
        DiffRebuild = [reuse(Index)|DiffNext] % reuse old child
    ;   debug(vdom, 'Keyed diff creates new element for key "~w"', [Key]),
        DiffRebuild = [create(Node)|DiffNext] % create new element
    ),
    tag_keyed_rebuild(Nodes, Assoc, DiffNext).

tag_keyed_rebuild([], _, []).

% Tests whether the list of nodes have
% the same keys.

vdom_list_has_same_keys([Head|Tail], [HeadPrev|TailPrev]):-
    vdom_node_tag_key(Head, Key),
    vdom_node_tag_key(HeadPrev, Key),
    vdom_list_has_same_keys(Tail, TailPrev).

vdom_list_has_same_keys([], []).

% Builds up an assoc structure that maps
% keys to node indices. Used for looking
% up previous nodes.

vdom_keyed_assoc(VDom, Assoc):-
    empty_assoc(Empty),
    vdom_node_tag_body(VDom, Body),
    vdom_keyed_assoc(Body, 0, Empty, Assoc).

vdom_keyed_assoc([Node|Nodes], Index, In, Out):-
    vdom_node_tag_key(Node, Key),
    put_assoc(Key, In, Index, Tmp),
    IndexNext is Index + 1,
    vdom_keyed_assoc(Nodes, IndexNext, Tmp, Out).

vdom_keyed_assoc([], _, Assoc, Assoc).

% General case for tag diff. Replaces
% whole tag when body length does not
% match.

vdom_diff_tag_general(VDom, VDomPrev, Path, Diff, VDomOut):-
    VDom =.. [Name, Attrs, Body],
    VDomPrev =.. [_, AttrsPrev, BodyPrev],
    (   length(Body, Length),
        length(BodyPrev, Length)
    ->  attrs_diff(Path, Attrs, AttrsPrev, DiffAttrs),
        vdom_diff_tag_general_body(Body, BodyPrev, 0, Path, DiffBody, VDomOutBody),        
        VDomOut =.. [Name, Attrs, VDomOutBody],
        append(DiffAttrs, DiffBody, Diff)
    ;   diff_replace_at(Path, VDom, Diff, VDomOut),
        writeln(tag_in(VDom)),
        writeln(tag_out(VDomOut))
    ).

% Calculates attribute change diff.

attrs_diff(_, Attrs, AttrsPrev, []):-
    Attrs == AttrsPrev, !.

attrs_diff(Path, Attrs, AttrsPrev, Diff):-
    include(attrs_diff_set(AttrsPrev), Attrs, AttrsSet),
    include(attrs_diff_unset(Attrs), AttrsPrev, AttrsUnset),
    maplist(attrs_diff_set_null, AttrsUnset, AttrsNull),
    append(AttrsNull, AttrsSet, Changes),
    diff_set_attrs_at(Path, Changes, Diff).

attrs_diff_set_null(Name=_, Name=null).

attrs_diff_unset(Attrs, Name=_):-
    \+ memberchk(Name=_, Attrs).

% Avoid component annotations on tags.

attrs_diff_set(_, component=_):- !, fail.
attrs_diff_set(_, data=_):- !, fail.
attrs_diff_set(AttrsPrev, Name=Value):-
    memberchk(Name=Value, AttrsPrev), !, fail.
attrs_diff_set(_, _).

% Tag body diff. Assumes that both bodies have
% same number of elements.
% TODO: write as tail-recursive.

vdom_diff_tag_general_body([Head|Tail], [HeadPrev|TailPrev], Index, Path, Diff, VDomOut):-
    VDomOut = [VDomOutHead|VDomOutTail],
    vdom_diff(Head, HeadPrev, [Index|Path], DiffHead, VDomOutHead),
    IndexNext is Index + 1,
    vdom_diff_tag_general_body(Tail, TailPrev, IndexNext, Path, DiffNext, VDomOutTail),
    append(DiffHead, DiffNext, Diff).

vdom_diff_tag_general_body([], [], _, _, [], []).

% Checks whether every VDom node child has
% the key attribute.

vdom_body_is_keyed(VDom):-
    vdom_node_tag_body(VDom, Body),
    length(Body, Length),
    Length > 0,
    forall(member(Child, Body), vdom_node_has_key(Child)).

% Tests whether the vdom node has key.

vdom_node_has_key(VDom):-
    vdom_node_tag_attrs(VDom, Attrs),
    memberchk(key=_, Attrs).

% VDom node type, right now text or tag.

vdom_node_type(Atomic, text):-
    atomic(Atomic), !.

vdom_node_type(Component, component):-
    Component =.. [Name, _, _],
    vdom_component_exists(Name), !.

% Tag referring to a component.

vdom_node_type(Tag, tag_component):-
    Tag =.. [_, Attrs, _],
    memberchk(component=_, Attrs), !.

vdom_node_type(Tag, tag):-
    Tag =.. [_, _, _], !.

vdom_node_type(VDom, _):-
    throw(error(invalid_vdom_node(VDom), _)).

% VDom tag node's tag name.

vdom_node_tag_name(Tag, Name):-
    Tag =.. [Name, _, _].

% VDom tag node's attributes.

vdom_node_tag_attrs(Tag, Attrs):-
    Tag =.. [_, Attrs, _].

% VDom tag node's body.

vdom_node_tag_body(Tag, Body):-
    Tag =.. [_, _, Body].

% VDom tag node's key.

vdom_node_tag_key(Tag, Key):-
    vdom_node_tag_attrs(Tag, Attrs),
    memberchk(key=Key, Attrs).

% VDom component's name.

vdom_node_tag_component(Tag, ComponentName):-
    vdom_node_tag_attrs(Tag, Attrs),
    memberchk(component=ComponentName, Attrs).

% Tag node's body length.

vdom_node_tag_body_length(Tag, Length):-
    vdom_node_tag_body(Tag, Body),
    length(Body, Length).

% Tag/component node's associated data.

vdom_node_data(Component, Data):-
    vdom_node_tag_attrs(Component, Attrs),
    memberchk(data=Data, Attrs).

% Generates replace action at the given path.

diff_replace_at(Path, VDom, Diff, VDomOut):-
    reverse(Path, Reversed),
    debug(vdom, 'Rebuilding node at ~w', [Path]),
    vdom_build(VDom, NodeBuild, VDomOut),
    debug(vdom, 'Rebuilt node at ~w', [Path]),
    Diff = [diff{ type: replace, path: Reversed, vdom: NodeBuild }].

% Generates attribute update action at the given path.

diff_set_attrs_at(_, [], []):- !.

diff_set_attrs_at(Path, Changes, Diff):-
    reverse(Path, Reversed),
    dict_create(Attrs, attrs, Changes),
    Diff = [diff{ type: attrs, path: Reversed, attrs: Attrs }].

diff_reorder_or_create_at(Path, Rebuild, Diff, VDomOut):- % contains reuse(index) parts!!!
    reverse(Path, Reversed),
    maplist(rebuild_action, Rebuild, Actions, VDomOut),
    Diff = [diff{ type: reorder_or_create, path: Reversed, actions: Actions }].

rebuild_action(reuse(Index), Action, reuse(Index)):- !,
    Action = action{ type: reuse, index: Index }.

rebuild_action(create(Node), Action, VDomNode):- !,
    vdom_build(Node, NodeBuild, VDomNode),
    Action = action{ type: build, vdom: NodeBuild }.

% TODO handle fail case.

vdom_build(VDom, Dict, VDomOut):-
    vdom_node_type(VDom, Type),
    vdom_build(Type, VDom, Dict, VDomOut).

vdom_build(text, VDom, VDom, VDom).

vdom_build(tag, VDom, Dict, VDomOut):-
    VDom =.. [ Name, Attrs, Body ],
    debug(vdom, 'Building tag "~w"', [Name]),
    dict_create(AttrsDict, attrs, Attrs),
    maplist(vdom_build, Body, BodyBuild, VDomBodyOut),
    VDomOut =.. [ Name, Attrs, VDomBodyOut ],
    Dict = node{ name: Name, attrs: AttrsDict, body: BodyBuild },
    debug(vdom, 'Built tag "~w"', [Name]).

vdom_build(component, VDom, Dict, VDomOut):-
    VDom =.. [Name, Attrs, Body],
    debug(vdom, 'Building component "~w"', [Name]),
    memberchk(data=Data, Attrs),
    (   memberchk(key=Key, Attrs)
    ->  vdom_component_render(Name, Key, Data, Body, VDomRendered)
    ;   vdom_component_render(Name, Data, Body, VDomRendered)
    ),
    vdom_build(VDomRendered, Dict, VDomOut).

vdom_build(tag_component, VDom, Dict, VDomOut):-
    vdom_build(tag, VDom, Dict, VDomOut).
