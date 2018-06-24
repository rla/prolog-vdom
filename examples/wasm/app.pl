:- use_module(library(gensym)).
:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(vdom).

% Initial state.
% Key task_name is the name in the form.
% Key task_error is error message when name is unsuitable.
% Key tasks is the list of tasks.

initial_state(d{
    task_name: "",
    task_error: "",
    tasks: []
}).

% Helper predicate to update the current
% state. State management is completely
% application-dependent.

:- meta_predicate(update_state(2)).

update_state(Updater):-
    nb_getval(state, StateOld),
    call(Updater, StateOld, StateNew),
    nb_linkval(state, StateNew).

set_key(Key, Value, StateOld, StateNew):-
    atom_string(KeyAtom, Key),
    put_dict(KeyAtom, StateOld, Value, StateNew).

% Adds new task.

add_task(StateOld, StateNew):-
    gensym('task-', TaskId),
    Name = StateOld.task_name,
    (   Name = ""
    ->  StateNew = StateOld.put(d{
            task_error: "The task name must be entered."
        })
    ;   Task = task{
            id: TaskId,
            name: Name,
            done: no
        },
        append(StateOld.tasks, [Task], Tasks),
        StateNew = StateOld.put(d{
            task_name: "",
            task_error: "",
            tasks: Tasks
        })
    ).

toggle_task_done(Id, StateOld, StateNew):-
    maplist(task_toggle_done(Id), StateOld.tasks, Tasks),
    StateNew = StateOld.put(tasks, Tasks).

task_toggle_done(Id, TaskIn, TaskOut):-
    TaskIn.id = Id, !,
    done_toggle(TaskIn.done, Done),
    TaskOut = TaskIn.put(done, Done).

task_toggle_done(_, Task, Task).

done_toggle(yes, no).
done_toggle(no, yes).

% Application components.

% The component task_form renders the form to enter
% task's name. When error has been set, it will
% display the error next to the input element.

:- vdom_component_register(task_form, render_task_form).

render_task_form(Data, _, Body):-
    task_form_error(Data.error, Error),
    Body = form([id=task_form], [
        'Task name: ', input([type=text, value=Data.value, id=task_name], []), ' ',
        button([type=submit], ['Add']) | Error        
    ]).

% This is not a component but used by a component
% as a normal Prolog predicate. It can retrieve
% arbitrary elements.

task_form_error("", []):- !.
task_form_error(Message, [br([], []), span([className=error], [Message])]).

% The description component just renders a helpful
% text about this application.

:- vdom_component_register(description, render_description).

render_description(_, _, div([className=description], [
    'This application implements ToDo list.', br([], []),
    'Use the task form below to enter a new task.'
])).

% The task_list component renders the list of tasks
% currently entered into the application.

:- vdom_component_register(task_list, render_task_list).

render_task_list([], _, div([], [
    'You have no tasks entered so far.'
])):- !.

render_task_list(Tasks, _, Dom):-
    length(Tasks, Count),
    vdom_component_each(task, Tasks, id, Body),
    Dom = div([], [
        ul([], Body),
        div([], [
            'You can click on a task to mark it done or not done.', br([], []),
            'You have ', strong([], [Count]), ' tasks in total.'
        ])
    ]).

% The task component renders a single task.

:- vdom_component_register(task, render_task).

render_task(Task, _, Dom):-
    Dom = li([className=Class,id=Task.id], [Task.name]),
    task_class(Task.done, Class).

task_class(yes, 'task task-done').
task_class(no, 'task').

% This renders partial VDOM according
% to the current application state.

render(VDom):-
    nb_getval(state, State),
    VDom = div([className=app], [
        h1([], ['ToDo list']),
        description([data=[]], []),
        h2([], ['Tasks']),
        task_list([data=State.tasks], []),
        h2([], ['New task form']),
        task_form([data=d{error: State.task_error, value: State.task_name}], [])
    ]).

% This calls render/1, calculates the VDOM diff
% with the previous VDOM tree and stores the
% current tree for the next diff calculation.

diff(Diff):-
    render(VDom),
    nb_getval(vdom, VPrev),
    vdom_diff(VDom, VPrev, Diff, VOut),
    write('Diff: '), write_canonical(Diff), nl,
    nb_linkval(vdom, VOut).

initial_render(Diff):-
    initial_state(State),
    nb_linkval(vdom, ''),
    nb_linkval(state, State),
    diff(Diff).

% Event handling.

% Setting 3rd argument of event handler
% to true will mark the event handled.

event_click(Id, Diff, true):-
    atom_concat('task-', _, Id), !,
    atom_string(IdAtom, Id),
    update_state(toggle_task_done(IdAtom)),
    diff(Diff).

event_click(_, diff, false).

% Predicate event_input is called whenever an input
% element's value is changed.

event_input("task_name", Value, Diff, true):-
    update_state(set_key(task_name, Value)),
    diff(Diff).

event_input(_, _, diff, false).

% Predicate event_submit is called whenever a
% form element is submitted.

event_submit("task_form", Diff, true):- !,
    update_state(add_task),
    diff(Diff).

event_submit(_, diff, false).
