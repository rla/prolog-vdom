# Prolog Virtual DOM

This is a Virtual DOM implementation for Prolog. It is
designed to be used with the WebAssembly build of SWI-Prolog.

The implementation keeps VDOM tree in the Prolog side
and serializes the initial tree and modifications through
the foreign language interface.

## Prolog API

See example:

```prolog
VIn = div([], [hello]),
VPrev = div([], [world]),
vdom_diff(VIn, VPrev, Diff, VOut).
```

 * VIn - current VDOM tree.
 * VPrev - previous VDOM tree.
 * Diff - serialized updates (`[replace(path(0), "hello")]`).
 * VOut - current full VDOM tree.

In this example the first child of the root node is being
replaced by the text node containing "hello".

## DOM serialization

Atomic terms are serialized as strings.

Attribute terms:

```prolog
attrs(name1(Value1), name2(Value2))
```

where Value1 and Value2 are strings.

Tags:

```prolog
name(Attrs, body(Child1, Child2)).
```

where Attrs is an attribute term and Child1 and
Child2 are serialized DOM subtrees.

## DOM patch serialization

Node rebuild:

```prolog
replace(Path, Dom)
```

where Path is a path term and Dom is a serialized DOM
tree as described in "DOM serialization".

Set/unset attributes:

```
set_attrs(Path, Attrs)
```

where Path is a path term and Attrs an attributes term.

Reorder-or-create:

```prolog
roc(Path, actions(Action1, Action2))
```

where Path is a path term and Action1 and Action2 are
terms in the form:

Reuse term (reuses DOM node from the old index):

```prolog
reuse(Index)
```

Create term (creates a new DOM node):

```prolog
create(Dom)
```

where Dom is a serialized DOM tree for the node as
described in "DOM serialization".

Path term:

```prolog
path(0, 2)
```

Path term represents path from the root element down to
the element that is being modified. For example, `path(0, 2)`
refers to 1st child of the root, and 3rd subchild of the child.

## TODO

 * Style updates.
 * Special `each` node.

## License

The MIT license. See the LICENSE file.
