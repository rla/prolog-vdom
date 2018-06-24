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
        } else if (name === 'set_attrs') {
            this._applySetAttrs(ref);
        } else if (name === 'roc') {
            this._applyRoc(ref);
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

    // Sets attributes.
    // Term: set_attrs(path, attrs)
    _applySetAttrs(ref) {
        const prolog = this.prolog;
        const pathArg = prolog.new_term_ref();
        const attrsArg = prolog.new_term_ref();
        prolog.get_arg(1, ref, pathArg);
        prolog.get_arg(2, ref, attrsArg);
        const path = this._extractPath(pathArg);
        const element = this._query(this.root, path, 0);
        this._setAttrs(element, attrsArg);
    }

    // Applies reorder-or-create patch.
    _applyRoc(ref) {
        const prolog = this.prolog;
        const pathArg = prolog.new_term_ref();
        const actionsArg = prolog.new_term_ref();
        prolog.get_arg(1, ref, pathArg);
        prolog.get_arg(2, ref, actionsArg);
        const path = this._extractPath(pathArg);
        const element = this._query(this.root, path, 0);
        this._applyRocActions(element, actionsArg);
    }

    _applyRocActions(dom, actions) {
        const prolog = this.prolog;
        const arity = prolog.functor_arity(
            prolog.get_functor(actions));
        const body = document.createDocumentFragment();
        const children = Array.from(dom.childNodes);
        for (let i = 0; i < arity; i++) {
            const arg = prolog.new_term_ref();
            prolog.get_arg(i + 1, actions, arg);
            this._applyRocAction(body, children, arg);
        }
        while (dom.firstChild) {
            dom.removeChild(dom.firstChild);
        }
        dom.appendChild(body);
    }

    // Term ref is reuse(...) or create(...).
    _applyRocAction(body, children, ref) {
        const prolog = this.prolog;
        const name = prolog.atom_chars(
            prolog.functor_name(
                prolog.get_functor(ref)));
        if (name === 'reuse') {
            this._applyRocActionReuse(body, children, ref);
        } else if (name === 'create') {
            this._applyRocActionCreate(body, children, ref);
        }
    }

    // Term ref is reuse(Integer).
    _applyRocActionReuse(body, children, ref) {
        const prolog = this.prolog;
        const indexArg = prolog.new_term_ref();
        prolog.get_arg(1, ref, indexArg);
        const index = prolog.get_integer(indexArg);
        body.appendChild(children[index]);
    }

    // Term ref is build(Dom).
    _applyRocActionCreate(body, dom, ref) {
        const prolog = this.prolog;
        const build = prolog.new_term_ref();
        prolog.get_arg(1, ref, build);
        body.appendChild(this._buildNode(build));
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
            const id = e.target.id || '';
            this._handleEvent('click', e, id);
        }, false);
        document.addEventListener('input', (e) => {
            const id = e.target.id || '';
            const value = e.target.value || '';
            this._handleEvent('input', e, id, value);
        }, false);
        document.addEventListener('submit', (e) => {
            const id = e.target.id || '';
            this._handleEvent('submit', e, id);
        }, false);
    }

    // Calls the Prolog-side event handler.
    _handleEvent(name, e, ...args) {
        const prolog = this.prolog;
        const arity = args.length + 2; // adds Diff and Handled
        const ref = prolog.new_term_ref();
        prolog.put_functor(ref,
            prolog.new_functor(
                prolog.new_atom(`event_${name}`), arity));
        args.forEach((arg, i) => {
            this._setPrimitiveArg(ref, i + 1, arg);
        });
        if (prolog.call(ref)) {
            const diff = prolog.new_term_ref();
            const handled = prolog.new_term_ref();
            prolog.get_arg(arity - 1, ref, diff);
            prolog.get_arg(arity, ref, handled);
            this._apply(diff);
            if (prolog.get_chars(handled) === 'true') {
                e.stopPropagation();
                e.preventDefault();
            }
        } else {            
            e.stopPropagation();
            e.preventDefault();
            console.log(`Error while handling event "${name}" with` +
                ` arguments "${args.join(',')}".`);
        }
    }

    // Unifies primitive JS value with argument in
    // the called predicate (ref). Index is 1-based.
    _setPrimitiveArg(ref, index, value) {
        const prolog = this.prolog;
        const ref1 = prolog.new_term_ref();
        const ref2 = prolog.new_term_ref();
        prolog.get_arg(index, ref, ref1);
        if (typeof value === 'string') {
            prolog.put_chars_string(ref2, value);            
        } else {
            throw new Error('Call argument must be a primitive value.');
        }
        if (!prolog.unify(ref1, ref2)) {
            throw new Error('Failed to unify the call argument.');
        }
    }
}
