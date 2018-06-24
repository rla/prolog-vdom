class Prolog {

    constructor(module, args) {
        this.module = module;
        this.args = args;
        this.bindings = {};
        this._bind();
        this._initialise();     
    }

    // Creates bindings to the SWI foreign API.
    _bind() {
        this.bindings.PL_atom_chars = this.module.cwrap(
            'PL_atom_chars', 'number', ['number']);
        this.bindings.PL_functor_arity = this.module.cwrap(
            'PL_functor_arity', 'number', ['number']);
        this.bindings.PL_functor_name = this.module.cwrap(
            'PL_functor_name', 'number', ['number']);
        this.bindings.PL_get_functor = this.module.cwrap(
            'PL_get_functor', 'number', ['number', 'number']);
        this.bindings.PL_get_chars = this.module.cwrap(
            'PL_get_chars', 'number', ['number', 'number', 'number']);
        this.bindings.PL_get_arg = this.module.cwrap(
            'PL_get_arg', 'number', ['number', 'number', 'number']);
        this.bindings.PL_get_integer = this.module.cwrap(
            'PL_get_integer', 'number', ['number', 'number']);
        this.bindings.PL_put_chars = this.module.cwrap(
            'PL_put_chars', 'number', ['number', 'number', 'number', 'number']);
        this.bindings.PL_unify = this.module.cwrap(
            'PL_unify', 'number', ['number', 'number']);
        this.bindings.PL_is_string = this.module.cwrap(
            'PL_is_string', 'number', ['number']);
        this.bindings.PL_initialise = this.module.cwrap(
            'PL_initialise', 'number', ['number', 'number']);
        this.bindings.PL_new_atom = this.module.cwrap(
            'PL_new_atom', 'number', ['string']);
        this.bindings.PL_new_functor = this.module.cwrap(
            'PL_new_functor', 'number', ['number', 'number']);
        this.bindings.PL_new_term_ref = this.module.cwrap(
            'PL_new_term_ref', 'number', []);
        this.bindings.PL_put_functor = this.module.cwrap(
            'PL_put_functor', 'number', ['number', 'number']);
        this.bindings.PL_chars_to_term = this.module.cwrap(
            'PL_chars_to_term', 'number', ['string', 'number']);
        this.bindings.PL_call = this.module.cwrap(
            'PL_call', 'number', ['number', 'number']);
        this.bindings.PL_unify_arg = this.module.cwrap(
            'PL_unify_arg', 'number', ['number', 'number', 'number']);
    }

    // See http://www.swi-prolog.org/pldoc/doc_for?object=c(%27PL_initialise%27)
    _initialise() {
        const argv = this.args.map((arg) =>
            this.module.allocate(
                this.module.intArrayFromString(arg),
                'i8', this.module.ALLOC_NORMAL));
        const ptr = this.module._malloc(argv.length * 4);
        argv.forEach((arg, i) => {
            this.module.setValue(ptr + i * 4, arg, '*');
        });
        if (!this.bindings.PL_initialise(4, ptr)) {
            throw new Error('SWI-Prolog initialisation failed.');
        }
        this.call_string("assert(user:file_search_path(library, 'wasm-preload/library')).");
    }

    // Helper function to parse a JavaScript
    // string into a Prolog term and call is as a query.
    call_string(query) {
        const ref = this.new_term_ref();
        if (!this.chars_to_term(query, ref)) {
            throw new Error(`Query has a syntax error: ${query}.`);
        }
        return !!this.call(ref, 0);
    }

    // Return the arity of the given functor.
    functor_arity(functor) {
        return this.bindings.PL_functor_arity(functor);
    }

    // Return an atom representing the name of the given functor.
    functor_name(functor) {
        return this.bindings.PL_functor_name(functor);
    }

    // Returns functor of the given term.
    // Returns null when the term is not a compound.
    get_functor(term) {
        const ptr = this.module._malloc(4);
        if (this.bindings.PL_get_functor(term, ptr)) {
            const functor = this.module.getValue(ptr, 'i32');
            this.module._free(ptr);
            return functor;
        } else {
            this.module._free(ptr);
            return null;
        }
    }

    // Returns integer number for the given term.
    // Returns null when the term is not an integer.
    get_integer(term) {
        const ptr = this.module._malloc(4);
        if (this.bindings.PL_get_integer(term, ptr)) {
            const number = this.module.getValue(ptr, 'i32');
            this.module._free(ptr);
            return number;
        } else {
            this.module._free(ptr);
            return null;
        }
    }

    // Implements PL_put_chars for string case.
    put_chars_string(term, string) {
        const len = this.module.lengthBytesUTF8(string) + 1;
        const ptr = this.module._malloc(len);
        this.module.stringToUTF8(string, ptr, len);
        const ret = !!this.bindings.PL_put_chars(term, 5, len - 1, ptr);
        this.module._free(ptr);
        return ret;
    }

    // Unifies the terms. Returns false if the terms
    // do not unify.
    unify(term1, term2) {
        return !!this.bindings.PL_unify(term1, term2);
    }

    // Returns whether the term is a string.
    is_string(term) {
        return !!this.bindings.PL_is_string(term);
    }

    // Return a C-string for the text represented by the given atom.
    atom_chars(atom) {
        const ptr = this.bindings.PL_atom_chars(atom);
        if (ptr === 0) {
            return null;
        } else {
            return this.module.Pointer_stringify(ptr);
        }
    }

    // Call term t just like the Prolog predicate once/1.
    call(term, module) {
        return this.bindings.PL_call(term, module);
    }

    // Parse the string chars and put the resulting
    // Prolog term into the term t.
    chars_to_term(query, t) {
        return this.bindings.PL_chars_to_term(query, t);
    }

    // Converts the argument term to a string.
    get_chars(term) {
        const ptr = this.module._malloc(4);
        const flags = 0x0001 | 0x0002 | 0x0004 | 0x0008 | 0x0010 | 0x0020 | 0x0080 | 0x1000 | 0x0200;
        if (this.bindings.PL_get_chars(term, ptr, flags)) {
            // TODO properly free.
            return this.module.UTF8ToString(this.module.getValue(ptr, 'i32'));
        } else {
            return null;
        }
    }

    // If t is compound and index is between 1 and arity (inclusive),
    // assign a with a term reference to the argument.
    get_arg(index, term, arg) {
        return this.bindings.PL_get_arg(index, term, arg);
    }

    // Return an atom handle for the given C-string.
    new_atom(string) {
        return this.bindings.PL_new_atom(string);
    }

    // Returns a functor identifier, a handle for the name/arity pair.
    new_functor(atom, arity) {
        return this.bindings.PL_new_functor(atom, arity);
    }

    // Return a fresh reference to a term.
    new_term_ref() {
        return this.bindings.PL_new_term_ref();
    }

    // Create a new compound term from functor and bind t to this term.
    put_functor(term, functor) {        
        return this.bindings.PL_put_functor(term, functor);
    }

    // Unifies the index-th argument (1-based) of term with arg.
    unify_arg(index, term, arg) {
        return this.bindings.PL_unify_arg(index, term, arg);
    }
}
