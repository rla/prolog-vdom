(function() {
    function apply(root, patch, hilight) {
        var element = query(root, patch.path, 0);
        if (!element) {
            throw new Error('Element at ' + patch.path.join(', ') + ' does not exist.');
        }
        if (patch.type === 'replace') {
            var built = build(patch.vdom);
            element.parentNode.replaceChild(built, element);
            if (hilight) {
                hilightReplace(built);
            }
        } else if (patch.type === 'reorder_or_create') {
            // Check: cannot be root.
            var body = document.createDocumentFragment();
            var children = copyArrayLike(element.childNodes);
            for (var a = 0; a < patch.actions.length; a++) {
                var action = patch.actions[a];
                if (action.type === 'reuse') {
                    body.appendChild(children[action.index]);
                } else if (action.type === 'build') {
                    body.appendChild(build(action.vdom));
                } else {
                    throw new Error('Unknown action type: ' + action.type);
                }
            }
            while (element.firstChild) {
                element.removeChild(element.firstChild);
            }
            element.appendChild(body);
            if (hilight) {
                hilightKeyed(element);
            }
        } else if (patch.type === 'attrs') {
            setAttributes(element, patch.attrs);
            if (hilight) {
                hilightAttrs(element);
            }
        } else {
            throw new Error('Unknown patch type: ' + patch.type);
        }
    }
    // Finds element based on the path array.
    function query(root, path, index) {
        if (index === path.length) {
            return root; // last path element
        } else if (index < path.length) {
            return query(root.childNodes[path[index]], path, index + 1);
        } else {
            throw new Error('Invalid path: ' + path);
        }
    }
    // Builds real DOM element from vdom.
    function build(vdom) {
        if (typeof vdom === 'object') {
            var element = document.createElement(vdom.name);
            setAttributes(element, vdom.attrs);
            for (var b = 0; b < vdom.body.length; b++) {
                element.appendChild(build(vdom.body[b]));
            }
            return element;
        } else {
            return document.createTextNode(vdom);
        }
    }
    // Sets or updates attributes on the given element.
    function setAttributes(element, attrs) {
        for (var key in attrs) {
            if (key.match(/^data-.+/)) {
                element.setAttribute(key, attrs[key]);
            } else {
                element[key] = attrs[key];
            }
        }
    }
    // Copies array-like object into an actual array.
    function copyArrayLike(arrayLike) {
        var ret = [];
        for (var i = 0; i < arrayLike.length; i++) {
            ret.push(arrayLike[i]);
        }
        return ret;
    }
    // Debug helper to highlight changed/replaced element.
    function hilightReplace(element) {
        var prev = element.style.outline;
        element.style.outline = '2px solid red';
        setTimeout(function() {
            element.style.outline = prev;
        }, 500);
    }
    // Debug helper to highlight element with changed attributes.
    function hilightAttrs(element) {
        if (!element.getAttribute('data-hilighted')) {
            var prev = element.style.outline;
            element.style.outline = '2px solid blue';
            element.setAttribute('data-hilighted', 'yes');
            setTimeout(function() {
                element.style.outline = prev;
                element.removeAttribute('data-hilighted');
            }, 500);
        }
    }
    // Debug helper to highlight element with keyed children.
    function hilightKeyed(element) {
        var prev = element.style.outline;
        element.style.outline = '2px solid green';
        setTimeout(function() {
            element.style.outline = prev;
        }, 500);
    }
    function applyAll(root, patches, hilight) {
        if (root.children.length === 0) {
            // Initial dummy node.
            root.appendChild(document.createTextNode(''));
        }
        var appRoot = root.childNodes[0];
        for (var i = 0; i < patches.length; i++) {
            apply(appRoot, patches[i], hilight);
        }
    }

    window.applyPatches = applyAll;
})();
