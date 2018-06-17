:- module(client, [
    current_diff/1,  % -Diff
    handle_click/2,  % +ElementId, -Diff
    handle_change/3, % +ElementId, +Value, -Diff
    handle_submit/2  % +ElementId, -Diff
]).

% Client-side code.

:- use_module(prolog/vdom).

:- dynamic(vdom/1).
:- asserta(vdom('')).

% State management is application-specific.
% Here we just use a predicate to store the
% state term.

:- dynamic(state/1).
:- asserta(state(d{
    tasks:[],
    task_name: '',
    task_name_error: ''
})).

update_state(Closure):-
    state(Old),
    call(Closure, Old, New),
    retractall(state(_)),
    asserta(state(New)).

update_field(Key, Value, Old, New):-
    atom_string(KeyAtom, Key),
    put_dict(KeyAtom, Old, Value, New).

mark_task_done(Id, Done):-
    state(State),
    updated_done_task_list(State.tasks, Id, Done, List),
    update_state(update_field(tasks, List)).

updated_done_task_list([Task|Tasks], Id, Done, [TaskOut|TasksOut]):-
    (   Task.id = Id
    ->  put_dict(done, Task, Done, TaskOut),
        TasksOut = Tasks
    ;   TaskOut = Task,
        updated_done_task_list(Tasks, Id, Done, TasksOut)
    ).

updated_done_task_list([], _, _, []).

task_find(List, Id, Task):-
    member(Task, List),
    Task.id = Id, !.

done_toggle(yes, no).
done_toggle(no, yes).            

% Renders the application's VDOM.
% Here we just render the top-level "app" component.

render_vdom(app([data=Data], [])):-
    state(Data).

% Renders new virtual dom tree and calculates the
% difference with the previous rendering result.

current_diff(Diff):-
    vdom(VDomPrev),
    render_vdom(VDom),
    vdom:vdom_diff(VDom, VDomPrev, Diff, VDomOut),
    retractall(vdom(_)),
    asserta(vdom(VDomOut)).

% Generic handler for clicks.

handle_click(TaskId, Diff):-
    state(State),
    atom_concat('task-', Id, TaskId), !,
    task_find(State.tasks, Id, Task),
    done_toggle(Task.done, Done),
    mark_task_done(Id, Done),
    current_diff(Diff).

handle_click(_, []).

% Generic handler for change events.

handle_change("null", _, []):- !.

handle_change(Id, Value, Diff):- !,
    update_state(update_field(Id, Value)),
    current_diff(Diff).

% Generic handler for submit events.

handle_submit("null", []):- !.

handle_submit("form", Diff):- !,
    state(State),
    (   State.task_name = ''
    ->  update_state(update_field(task_name_error,
            'You need to enter task description.'))
    ;   gensym(todo, Id),
        append(State.tasks, [task{
            text: State.task_name,
            id: Id,
            done: no
        }], Tasks),
        update_state(update_field(tasks, Tasks)),
        update_state(update_field(task_name, '')),
        update_state(update_field(task_name_error, ''))
    ),
    current_diff(Diff).
