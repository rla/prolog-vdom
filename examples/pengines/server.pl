:- use_module(prolog/vdom_component).
:- use_module(prolog/vdom).

% Register components.

:- vdom_component_register(task, task).

task(Task, _, li([className=Class,id=Id], [Task.text])):-
    atom_concat('task-', Task.id, Id),
    task_class(Task.done, Class).

task_class(yes, 'task task-done').
task_class(no, 'task').

:- vdom_component_register(tasks, tasks).

tasks([], _, Dom):-
    Dom = div([], ['You have no tasks, add them using the form below.']).

tasks(Tasks, _, Dom):-
    length(Tasks, Count),
    vdom_component_each(task, Tasks, id, Body),
    Dom = div([], [
        ul([], Body),
        div([], [
            'You can click on a task to mark it done or not done.', br([], []),
            'You have ', Count, ' tasks in total.'
        ])
    ]).

:- vdom_component_register(task_form, task_form).

task_form(Data, _, Body):-
    (   Data.error = ''
    ->  Error = []
    ;   Error = [div([className=error], [Data.error])]
    ),
    Body = form([id=form], [
        input([type=text, value=Data.value, id=task_name], []),
        ' ',
        button([type=submit], ['Add'])
        | Error
    ]).

:- vdom_component_register(app, component_app).

component_app(State, _,
    div([], [
        tasks([data=State.tasks], []),
        task_form([data=d{
            value: State.task_name,
            error: State.task_name_error
        }], [])
    ])).

% Pengines server stuff.

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_path)).
:- use_module(library(http/http_authenticate)).
:- use_module(library(http/http_server_files)).
:- use_module(library(http/http_error)).
:- use_module(library(pengines)).

:- http_handler(root(.), http_reply_file('examples/pengines/index.html', []), []).
:- http_handler(root('jquery.min.js'), http_reply_file('examples/pengines/jquery.min.js', []), []).
:- http_handler(root('patch.js'), http_reply_file('patch.js', []), []).
:- http_handler(root('events.js'), http_reply_file('examples/pengines/events.js', []), []).
:- http_handler(root('client.pl'), http_reply_file('examples/pengines/client.pl', []), []).
:- http_server(http_dispatch, [ port(8010), workers(16) ]).
