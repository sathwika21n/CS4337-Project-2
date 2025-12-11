:- module(findExit, [findExit/2]).
:- use_module(library(apply), [foldl/4]).
:- use_module(library(lists), [append/3, nth0/3, reverse/2]).
% CS4337 Project 2 – Maze solver
% This project provides findExit/2 that will validate a maze and either check a 
% given action list or generate one that leads from the start to an exit.

:- module(findExit, [findExit/2]).

% This is public predicate: it will either validate an action peace
% or generate a valid path from the start position.
findExit(Maze, Actions) :-
    % makes sure the maze is well formed
    validMaze(Maze),
    % finds the row/column of the start cell
    findStart(Maze, Start),
    % if the action is already provided
    (   nonvar(Actions)
    % validates it 
    ->  pathValid(Maze, Start, Actions)
    % if not searches for a path
    ;   bfsPath(Maze, Start, Actions)
    ).

% This will check if the maze is a list of non-empty 
% equal length rows 
validMaze(Maze) :-
    is_list(Maze),
    Maze \= [],
    maplist(non_empty_row, Maze),
    rectangular(Maze, Cols),
    allowed_cells(Maze, StartCount, ExitCount),
    StartCount =:= 1,
    ExitCount  >= 1,
    Cols > 0.
% the row should be non-empty list
non_empty_row(Row) :- is_list(Row), Row \= [].

% makes sure all rows have equal lengths
rectangular([Row|Rows], Len) :-
    length(Row, Len),
    Len > 0,
    maplist({Len}/[R]>>length(R, Len), Rows).

% counts the occurances of the allowed cells
% Tracks the start count and end count
allowed_cells(Maze, StartCount, ExitCount) :-
    allowed_cells(Maze, 0, 0, StartCount, ExitCount).

allowed_cells([], S, E, S, E).
allowed_cells([Row|Rest], S0, E0, S, E) :-
    allowed_row(Row, S0, E0, S1, E1),
    allowed_cells(Rest, S1, E1, S, E).

% processes one row of the maze
allowed_row([], S, E, S, E).
allowed_row([Cell|Cells], S0, E0, S, E) :-
    % increments the counter based on the cell type
    % free cell
    (   Cell = f -> S1=S0, E1=E0
    % wall cell
    ;   Cell = w -> S1=S0, E1=E0
    % start cell
    ;   Cell = s -> S1 is S0+1, E1=E0
    % end cell
    ;   Cell = e -> S1=S0, E1 is E0+1
    ),
    allowed_row(Cells, S1, E1, S, E).

% Maze access helpers:

% locates the start cell 's'
findStart(Maze, (R,C)) :-
    nth0(R, Maze, Row),
    nth0(C, Row, s).

% gets the cell value at (R,C)
cellAt(Maze, (R,C), Cell) :-
    nth0(R, Maze, Row),
    nth0(C, Row, Cell).

% true if the (R,C) is within the maze bounds
inBounds(Maze, (R,C)) :-
    R >= 0, C >= 0,
    length(Maze, Rows),
    nth0(0, Maze, Row0), length(Row0, Cols),
    R < Rows, C < Cols.

% Action simulations

% coordinate deltas for the four actions
dir_delta(left,  0, -1).
dir_delta(right, 0,  1).
dir_delta(up,   -1,  0).
dir_delta(down,  1,  0).

% computing new positions after performing actions act
% from (R,C)
stepPos((R,C), Act, (R2,C2)) :-
    dir_delta(Act, DR, DC),
    R2 is R + DR,
    C2 is C + DC.

% validates the entire path
pathValid(Maze, Start, Actions) :-
    foldl(follow(Maze), Actions, Start, End),
    cellAt(Maze, End, e).

% checks valid steps 
follow(Maze, Action, Pos0, Pos) :-
    dir_delta(Action, _, _),
    stepPos(Pos0, Action, Pos),
    inBounds(Maze, Pos),
    cellAt(Maze, Pos, Cell),
    Cell \= w.

% BFS path generation

% this produces actions that lead from start to exit
bfsPath(Maze, Start, Actions) :-
    bfs([(Start, [])], [Start], Maze, ActionsRev),
    reverse(ActionsRev, Actions).

% if the current position is exit then return to its path
bfs([(Pos, Path)|_], _, Maze, Path) :-
    cellAt(Maze, Pos, e), !.

% BFS expansion step
bfs([(Pos, Path)|Rest], Visited, Maze, Actions) :-
    findall((Next, [A|Path]),
            ( move(Pos, Maze, Visited, A, Next) ),
            NextStates),
    extract_positions(NextStates, NewPositions),
    % mark visited
    append(Visited, NewPositions, Visited1),
    % queue new states
    append(Rest, NextStates, Queue1),
    bfs(Queue1, Visited1, Maze, Actions).

% extracted visited positions from the state pairs
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
    % avoids looping back to start cell
    Cell \= s.  


