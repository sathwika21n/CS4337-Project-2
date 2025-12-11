:- module(findExit, [findExit/2]).
:- use_module(library(apply), [foldl/4]).
:- use_module(library(lists), [append/3, nth0/3, reverse/2]).
% CS4337 Project 2 – Maze solver
% This project provides findExit/2 that will validate a maze and either check a 
% given action list or generate one that leads from the start to an exit.

:- module(findExit, [findExit/2]).

% Public predicate
findExit(Maze, Actions) :-
    validMaze(Maze),
    findStart(Maze, Start),
    (   nonvar(Actions)
    ->  pathValid(Maze, Start, Actions)
    ;   bfsPath(Maze, Start, Actions)
    ).

% This is the maze validation part

validMaze(Maze) :-
    is_list(Maze),
    Maze \= [],
    maplist(non_empty_row, Maze),
    rectangular(Maze, Cols),
    allowed_cells(Maze, StartCount, ExitCount),
    StartCount =:= 1,
    ExitCount  >= 1,
    Cols > 0.

non_empty_row(Row) :- is_list(Row), Row \= [].

rectangular([Row|Rows], Len) :-
    length(Row, Len),
    Len > 0,
    maplist({Len}/[R]>>length(R, Len), Rows).

allowed_cells(Maze, StartCount, ExitCount) :-
    allowed_cells(Maze, 0, 0, StartCount, ExitCount).

allowed_cells([], S, E, S, E).
allowed_cells([Row|Rest], S0, E0, S, E) :-
    allowed_row(Row, S0, E0, S1, E1),
    allowed_cells(Rest, S1, E1, S, E).

allowed_row([], S, E, S, E).
allowed_row([Cell|Cells], S0, E0, S, E) :-
    (   Cell = f -> S1=S0, E1=E0
    ;   Cell = w -> S1=S0, E1=E0
    ;   Cell = s -> S1 is S0+1, E1=E0
    ;   Cell = e -> S1=S0, E1 is E0+1
    ),
    allowed_row(Cells, S1, E1, S, E).

% ---------- Helpers for maze access ----------

findStart(Maze, (R,C)) :-
    nth0(R, Maze, Row),
    nth0(C, Row, s).

cellAt(Maze, (R,C), Cell) :-
    nth0(R, Maze, Row),
    nth0(C, Row, Cell).

inBounds(Maze, (R,C)) :-
    R >= 0, C >= 0,
    length(Maze, Rows),
    nth0(0, Maze, Row0), length(Row0, Cols),
    R < Rows, C < Cols.

% ---------- Action simulation ----------

dir_delta(left,  0, -1).
dir_delta(right, 0,  1).
dir_delta(up,   -1,  0).
dir_delta(down,  1,  0).

stepPos((R,C), Act, (R2,C2)) :-
    dir_delta(Act, DR, DC),
    R2 is R + DR,
    C2 is C + DC.

pathValid(Maze, Start, Actions) :-
    foldl(follow(Maze), Actions, Start, End),
    cellAt(Maze, End, e).

follow(Maze, Action, Pos0, Pos) :-
    dir_delta(Action, _, _),
    stepPos(Pos0, Action, Pos),
    inBounds(Maze, Pos),
    cellAt(Maze, Pos, Cell),
    Cell \= w.

% ---------- Path generation (BFS) ----------

bfsPath(Maze, Start, Actions) :-
    bfs([(Start, [])], [Start], Maze, ActionsRev),
    reverse(ActionsRev, Actions).

bfs([(Pos, Path)|_], _, Maze, Path) :-
    cellAt(Maze, Pos, e), !.
bfs([(Pos, Path)|Rest], Visited, Maze, Actions) :-
    findall((Next, [A|Path]),
            ( move(Pos, Maze, Visited, A, Next) ),
            NextStates),
    extract_positions(NextStates, NewPositions),
    append(Visited, NewPositions, Visited1),
    append(Rest, NextStates, Queue1),
    bfs(Queue1, Visited1, Maze, Actions).

extract_positions([], []).
extract_positions([(P,_)|Rest], [P|Ps]) :-
    extract_positions(Rest, Ps).

move(Pos, Maze, Visited, Action, Next) :-
    dir_delta(Action, _, _),
    stepPos(Pos, Action, Next),
    inBounds(Maze, Next),
    \+ member(Next, Visited),
    cellAt(Maze, Next, Cell),
    Cell \= w,
    Cell \= s.  % avoid looping back to start


