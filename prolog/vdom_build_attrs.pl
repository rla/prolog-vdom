:- module(vdom_build_attrs, [
    vdom_build_attrs/2 % +Attrs, -Term
]).

:- use_module(library(lists)).
:- use_module(library(error)).

% Builds term for attributes. It uses
% name(Value) pairs.

% TODO: handle style attribute.

vdom_build_attrs(Attrs, Term):-
    must_be(list, Attrs),
    include(is_exported, Attrs, Exported),
    maplist(attribute_term, Exported, Terms),
    Term =.. [attrs|Terms].

is_exported(Name=_):-
    Name \= component,
    Name \= data,
    Name \= key.

attribute_term(Name=Value, Term):-
    must_be(atomic, Value),
    Term =.. [Name, String],
    atom_string(Value, String).
