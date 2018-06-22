:- module(vdom_keyed, [
    vdom_keyed_assoc/2 % +VDom, -Assoc
]).

:- use_module(library(assoc)).
:- use_module(library(lists)).
:- use_module(library(error)).
:- use_module(vdom_node).

% Builds up an assoc structure that maps
% keys to node indices. Used for looking
% up previous nodes.

vdom_keyed_assoc(VDom, Assoc):-
    empty_assoc(Empty),
    vdom_node_body(VDom, Body),
    vdom_keyed_assoc(Body, 0, Empty, Assoc).

vdom_keyed_assoc([Node|Nodes], Index, In, Out):-
    vdom_node_key(Node, Key),
    put_assoc(Key, In, Index, Tmp),
    IndexNext is Index + 1,
    vdom_keyed_assoc(Nodes, IndexNext, Tmp, Out).

vdom_keyed_assoc([], _, Assoc, Assoc).
