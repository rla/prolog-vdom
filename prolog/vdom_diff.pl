:- module(vdom_diff, [
    vdom_diff/4 % +VIn, +VPrev, -Diff, -VOut
]).

:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(library(assoc)).
:- use_module(vdom_component).
:- use_module(vdom_node).
:- use_module(vdom_build).
:- use_module(vdom_keyed).
:- use_module(vdom_diff_attrs).
:- use_module(vdom_patch_action).

% Topevel predicate that takes the current
% partial VDOM tree (VIn), previous tree (VPrev)
% and calculates the VDOM difference (Diff) and
% the full output VDOM tree (VOut). The output
% tree must be used for calculating the diff
% next time.

vdom_diff(VIn, VPrev, Diff, VOut):-
    must_be(nonvar, VIn),
    must_be(nonvar, VPrev),
    must_be(var, Diff),
    must_be(var, VOut),
    vdom_diff(VIn, VPrev, [], List, VOut),
    Diff =.. [diff|List].

vdom_diff(VIn, VPrev, Path, Diff, VOut):-
    vdom_node_type(VIn, TIn),
    vdom_node_type(VPrev, TPrev),
    vdom_diff(TIn, TPrev, VIn, VPrev, Path, Diff, VOut).

vdom_diff(text, text, VIn, VIn, _, [], VIn):- !.

% Text node will replace node of any other type
% or a different text node.

vdom_diff(text, _, VIn, _, Path, Diff, VOut):- !,
    vdom_replace_at(Path, VIn, Diff, VOut).

% Component node will only appear in the current
% VDOM. After rendering the component node
% becomes a tag_component node which is the
% rendered root element annotated with the
% component data.

vdom_diff(component, tag_component, VIn, VPrev, Path, Diff, VOut):- !,
    vdom_node_name(VIn, NIn),
    vdom_node_component(VPrev, NPrev),
    vdom_diff_component(NIn, NPrev, VIn, VPrev, Path, Diff, VOut).

% Component is fully rendered and turned into
% a VDOM tree when it replaces a previously non-
% component node.

vdom_diff(component, _, VIn, _, Path, Diff, VOut):- !,
    vdom_replace_at(Path, VIn, Diff, VOut).

% After rendering the component we compare it
% with the tag it replaces.

vdom_diff(tag_component, tag, VIn, VPrev, Path, Diff, VOut):- !,
    vdom_diff(tag, tag, VIn, VPrev, Path, Diff, VOut).

% After rendering the component we compare it
% with the component it replaces.

vdom_diff(tag_component, tag_component, VIn, VPrev, Path, Diff, VOut):- !,
    vdom_diff(tag, tag, VIn, VPrev, Path, Diff, VOut).

% A tag replacing a previous component node.

vdom_diff(tag, tag_component, VIn, VPrev, Path, Diff, VOut):- !,
    vdom_diff(tag, tag, VIn, VPrev, Path, Diff, VOut).

vdom_diff(tag, tag, VIn, VPrev, Path, Diff, VOut):- !,
    vdom_node_name(VIn, NIn),
    vdom_node_name(VPrev, NPrev),
    vdom_diff_tag(NIn, NPrev, VIn, VPrev, Path, Diff, VOut).

% Tag replacing a text node.

vdom_diff(tag, _, VIn, _, Path, Diff, VOut):- !,
    vdom_replace_at(Path, VIn, Diff, VOut).

vdom_diff(TIn, TPrev, _, _, _, _, _):-
    throw(error(vdom_unhandled_diff_case(TIn, TPrev, _), _)).

% Both refer to same-named component.
% Component input data has not been changed.
% The last VDOM subtree is reused.

vdom_diff_component(Name, Name, VIn, VPrev, _, [], VPrev):-
    vdom_node_data(VIn, DIn),
    vdom_node_data(VPrev, DPrev),
    DIn == DPrev, !.

% Component input data has been changed.
% Render component and diff its output tree
% with the previous VDOM tree.

vdom_diff_component(Name, Name, VIn, VPrev, Path, Diff, VOut):- !,
    vdom_component_render(VIn, VRendered),
    vdom_diff(VRendered, VPrev, Path, Diff, VOut).

% Component-tag correspondence.
% Both refer to different-named component.

vdom_diff_component(_, _, VIn, _, Path, Diff, VOut):- !,
    vdom_replace_at(Path, VIn, Diff, VOut).

% Same tag name, body is inspected.
% If both bodies are keyed then ROC
% is attempted.

vdom_diff_tag(Name, Name, VIn, VPrev, Path, Diff, VOut):-
    vdom_node_is_keyed(VIn),
    vdom_node_is_keyed(VPrev), !,
    vdom_diff_tag_keyed(VIn, VPrev, Path, Diff, VOut).

vdom_diff_tag(Name, Name, VIn, VPrev, Path, Diff, VOut):- !,
    vdom_diff_tag_general(VIn, VPrev, Path, Diff, VOut).

% Different tag name, whole node is replaced.

vdom_diff_tag(_, _, VIn, _, Path, Diff, VOut):-
    vdom_replace_at(Path, VIn, Diff, VOut).

% Keyed children replacement. First we reorder
% the body while adding new elements, then we
% perform modifications on the old child elements.
% Each direct child node has guaranteed to have
% key at this point.

% Special case for handling the keyed body.
% When bodies have same keys we use the general
% tag body diff calculation method. This will
% avoid ROC action when no reordering is performed.

vdom_diff_tag_keyed(VIn, VPrev, Path, Diff, VOut):-
    vdom_node_body(VIn, BIn),
    vdom_node_body(VPrev, BPrev),
    vdom_list_has_same_keys(BIn, BPrev), !,
    vdom_diff_tag_general(VIn, VPrev, Path, Diff, VOut).

vdom_diff_tag_keyed(VIn, VPrev, Path, Diff, VOut):-
    vdom_diff_tag_keyed_rebuild(VIn, VPrev, Path, Diff, VOut).

% Reorders child nodes, adds or replaces new
% nodes where needed.

vdom_diff_tag_keyed_rebuild(VIn, VPrev, Path, Diff, VOut):-
    vdom_node_tag(VIn, Name, AIn, BIn),
    vdom_node_tag(VPrev, Name, APrev, BPrev),
    vdom_node_tag(VOut, Name, AIn, BOut),
    vdom_keyed_assoc(VPrev, Assoc),
    tag_keyed_rebuild(BIn, Assoc, Rebuild),
    vdom_roc_at(Path, Rebuild, DiffRebuild, VRebuild),
    diff_after_reorder(BIn, BPrev, 0, Path,
        Rebuild, VRebuild, DiffAfter, BOut),
    vdom_diff_attrs(Path, AIn, APrev, DiffAttrs),
    append(DiffRebuild, DiffAfter, DiffChildren),
    append(DiffAttrs, DiffChildren, Diff).

% Finds VDOM node children diff after the children
% have been reordered.

diff_after_reorder(BIn, BPrev, Index, Path, Rebuild, VRebuild, Diff, VOut):-
    BIn = [Head|Tail],
    IndexNext is Index + 1,
    diff_after_reorder(Tail, BPrev, IndexNext, Path,
        Rebuild, VRebuild, DiffNext, VOutTail),
    (   nth0(Index, Rebuild, reuse(IndexInPrev))
    ->  nth0(IndexInPrev, BPrev, HeadPrev),
        vdom_diff(Head, HeadPrev, [Index|Path], DiffHead, VOutHead),
        append(DiffHead, DiffNext, Diff)
    ;   Diff = DiffNext,
        nth0(Index, VRebuild, VOutHead)
    ),
    VOut = [VOutHead|VOutTail].

diff_after_reorder([], _, _, _, _, _, [], []).

% Builds new body for the tag. Either reuses
% old body elements or adds new ones.

tag_keyed_rebuild([Node|Nodes], Assoc, DiffRebuild):-
    vdom_node_key(Node, Key),
    get_assoc(Key, Assoc, Index), !,
    DiffRebuild = [reuse(Index)|DiffNext],
    tag_keyed_rebuild(Nodes, Assoc, DiffNext).

tag_keyed_rebuild([Node|Nodes], Assoc, DiffRebuild):-
    DiffRebuild = [create(Node)|DiffNext],
    tag_keyed_rebuild(Nodes, Assoc, DiffNext).

tag_keyed_rebuild([], _, []).

% Tests whether the list of nodes have
% the same keys.

vdom_list_has_same_keys([HIn|TIn], [HPrev|TPrev]):-
    vdom_node_key(HIn, Key),
    vdom_node_key(HPrev, Key),
    vdom_list_has_same_keys(TIn, TPrev).

vdom_list_has_same_keys([], []).

% General case for tag diff. Replaces
% whole tag when body length does not
% match.

vdom_diff_tag_general(VIn, VPrev, Path, Diff, VOut):-
    vdom_node_tag(VIn, Name, AIn, BIn),
    vdom_node_tag(VPrev, Name, APrev, BPrev),
    length(BIn, Length),
    length(BPrev, Length), !,
    vdom_diff_attrs(Path, AIn, APrev, DiffAttrs),    
    vdom_diff_tag_general_body(BIn, BPrev, 0, Path, DiffBody, VBody),
    append(DiffAttrs, DiffBody, Diff),
    vdom_node_tag(VOut, Name, AIn, VBody).

vdom_diff_tag_general(VIn, _, Path, Diff, VOut):-
    vdom_replace_at(Path, VIn, Diff, VOut).

% Tag body diff. Assumes that both bodies have
% same number of elements.

vdom_diff_tag_general_body(VIn, VPrev, Index, Path, Diff, VOut):-
    VIn = [VHead|VTail],
    VPrev = [VHeadPrev|VTailPrev],
    VOut = [VOutHead|VOutTail],
    vdom_diff(VHead, VHeadPrev, [Index|Path], DiffHead, VOutHead),
    IndexNext is Index + 1,
    vdom_diff_tag_general_body(VTail, VTailPrev,
        IndexNext, Path, DiffNext, VOutTail),
    append(DiffHead, DiffNext, Diff).

vdom_diff_tag_general_body([], [], _, _, [], []).
