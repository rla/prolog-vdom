// Generic event handlers using event delegation.

document.addEventListener('click', function(event) {
    console.log('Handling click event.');
    var target = event.target;
    var id = target.id;
    askPengine('handle_click(' + Pengine.stringify(id) + ', Diff)');
}, false);

document.addEventListener('submit', function(event) {
    console.log('Handling submit event.');
    event.stopPropagation();
    event.preventDefault();
    var id = event.target.id;
    askPengine('handle_submit(' + Pengine.stringify(id) + ', Diff)');
}, false);

document.addEventListener('change', function(event) {
    console.log('Handling change event.');
    event.stopPropagation();
    event.preventDefault();
    var id = event.target.id;
    askPengine('handle_change(' +
        Pengine.stringify(id) + ', ' +
        Pengine.stringify(event.target.value) +
        ', Diff)');
}, false);

function askPengine(query) {
    if (window.pengine) {
        window.pengine.ask(query);
    }    
}
