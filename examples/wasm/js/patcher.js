class Patcher {

    constructor(prolog, root) {
        this.prolog = prolog;
        const aux = document.createTextNode('');
        root.appendChild(aux);
        this.root = aux;
    }

    initial() {
        const prolog = this.prolog;
        const ref = prolog.new_term_ref();
        prolog.put_functor(ref,
            prolog.new_functor(
                prolog.new_atom('initial_render'), 1));
        if (prolog.call(ref)) {
            const arg = prolog.new_term_ref();
            prolog.get_arg(1, ref, arg);
            this._apply(arg);
        } else {
            throw new Error('Failed to get initial DOM.');
        }
    }

    // Parses diff term and applies it to the current DOM.
    // Term: diff(...).
    _apply(ref) {
        const prolog = this.prolog;
        const arity = prolog.functor_arity(
            prolog.get_functor(ref));
        for (let i = 0; i < arity; i++) {
            const arg = prolog.new_term_ref();
            prolog.get_arg(i + 1, ref, arg);
            this._applyPatch(arg);
        }
    }

    // Applies single patch term.
    _applyPatch(ref) {
        const prolog = this.prolog;
        const name = prolog.atom_chars(
            prolog.functor_name(
                prolog.get_functor(ref)));
        if (name === 'replace') {
            this._applyReplace(ref);
        }
    }

    // Replace term.
    // Term: replace(Path, Dom).
    _applyReplace(ref) {
        const prolog = this.prolog;
        const pathArg = prolog.new_term_ref();
        const domArg = prolog.new_term_ref();
        prolog.get_arg(1, ref, pathArg);
        prolog.get_arg(2, ref, domArg);
        const path = this._extractPath(pathArg);
        const dom = this._buildNode(domArg);
        const element = this._query(this.root, path, 0);
        element.parentNode.replaceChild(dom, element);
        if (element === this.root) {
            // Root replacement.
            this.root = dom;
        }
    }

    // Parses the path term into an array.
    // Term: path(...).
    _extractPath(ref) {
        const prolog = this.prolog;
        const arity = prolog.functor_arity(
            prolog.get_functor(ref));
        const path = [];
        for (let i = 0; i < arity; i++) {
            const arg = prolog.new_term_ref();
            prolog.get_arg(i + 1, ref, arg);
            path.push(prolog.get_integer(arg));
        }
        return path;
    }

    // Builds real DOM object.
    // Term: name(attrs, body) or string.
    _buildNode(ref) {
        const prolog = this.prolog;
        if (prolog.is_string(ref)) {
            const text = prolog.get_chars(ref);
            return document.createTextNode(text);
        } else {
            const attrs = prolog.new_term_ref();
            const body = prolog.new_term_ref();
            prolog.get_arg(1, ref, attrs);
            prolog.get_arg(2, ref, body);
            const name = prolog.atom_chars(
                prolog.functor_name(
                    prolog.get_functor(ref)));
            const dom = document.createElement(name);
            this._setAttrs(dom, attrs);
            this._buildBody(dom, body);
            return dom;
        }
    }

    // Sets DOM node attributes.
    // Term ref is attrs(...).
    _setAttrs(dom, ref) {
        const prolog = this.prolog;
        const arity = prolog.functor_arity(
            prolog.get_functor(ref));
        for (let i = 0; i < arity; i++) {
            const arg = prolog.new_term_ref();
            prolog.get_arg(i + 1, ref, arg);
            const name = prolog.atom_chars(
                prolog.functor_name(
                    prolog.get_functor(arg)));
            const valueArg = prolog.new_term_ref();
            prolog.get_arg(1, arg, valueArg);
            const valueText = prolog.get_chars(valueArg);
            if (name.match(/^data-.+/)) {
                dom.setAttribute(name, valueText);
            } else {
                dom[name] = valueText;
            }
        }
    }

    // Builds DOM node body.
    // Term ref is body(...).
    _buildBody(dom, ref) {
        const prolog = this.prolog;
        const arity = prolog.functor_arity(
            prolog.get_functor(ref));
        for (let i = 0; i < arity; i++) {
            const arg = prolog.new_term_ref();
            prolog.get_arg(i + 1, ref, arg);
            dom.appendChild(this._buildNode(arg));
        }
    }

    // Finds element based on the path array.
    _query(root, path, index) {
        if (index === path.length) {
            return root; // last path element
        } else if (index < path.length) {
            return this._query(root.childNodes[path[index]], path, index + 1);
        } else {
            throw new Error('Invalid path: ' + path);
        }
    }

    // Adds delegate event handlers to the document.
    attachEvents() {
        document.addEventListener('click', (e) => {
            const prolog = this.prolog;
            const ref = prolog.new_term_ref();
            prolog.put_functor(ref,
                prolog.new_functor(
                    prolog.new_atom('click'), 2));
            const id = e.target.id;
            const arg1 = prolog.new_term_ref();
            const realArg1 = prolog.new_term_ref();
            prolog.get_arg(1, ref, realArg1);
            prolog.put_chars_string(arg1, "aaa");
            if (!prolog.unify(arg1, realArg1)) {
                throw new Error('Unify failed.');
            }
            if (prolog.call(ref)) {
                const arg2 = prolog.new_term_ref();
                prolog.get_arg(2, ref, arg2);
                this._apply(arg2);
            } else {
                throw new Error('Failed to handle event.');
            }
            e.stopPropagation();
            e.preventDefault();
        }, false);
    }
}
