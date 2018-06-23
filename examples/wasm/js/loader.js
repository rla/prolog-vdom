// Loads files over fetch and stores them
// in the virtual filesystem.

const loadPrologFiles = async () => {
    const files = [
        '/prolog/vdom_build_attrs.pl',
        '/prolog/vdom_build.pl',
        '/prolog/vdom_component.pl',
        '/prolog/vdom_diff_attrs.pl',
        '/prolog/vdom_diff.pl',
        '/prolog/vdom_keyed.pl',
        '/prolog/vdom_node.pl',
        '/prolog/vdom_patch_action.pl',
        '/examples/wasm/app.pl'
    ];

    FS.mkdir('/app');

    return Promise.all(files.map(async (file) => {
        const name = file.match(/\/([^\/]+.pl)$/)[1];
        const response = await fetch(file);
        const content = await response.text();
        const localName = `/app/${name}`;
        FS.writeFile(localName, content);
    }));
};
