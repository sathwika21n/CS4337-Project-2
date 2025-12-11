# CS4337-Project-2

This project implements a maze solver in prolog by using both path 
validation and BFS (Breadth-first Search). The main exported predicate is:

findExit(Maze, Action).

This will validate the maze and find the start location and either checks 
the provided action list or generates a valid list of actions.

Features:

Maze Validation:
    Before solving, the maze will be checked to see if its 
        non-empty list of characters
        rectangular
        one or more exit cells
        exactly one start cell
        only allowed cells are
            f - free
            w - wall
            s - start
            e - exit

How to use:
Import the Module:
    :- use_module(findExit).
Example Maze:
Maze = [
  [w, w, w, w, w],
  [w, s, f, f, w],
  [w, w, f, e, w],
  [w, w, w, w, w]
].

Validate the action list:
?- findExit(Maze, [right, right, down]).
true.

Automatically generate a path:
?- findExit(Maze, Actions).
Actions = [right, right, down] .

SESSION HISTORY:

Session #1 - Date: 12/08/25

Start time: 6:00pm 
End time: 7:15pm 

In this session, my goals were to understand the project requirements and 
to start writing the maze validator.

Works completed: 
I've read the project description and clarified what it should do and also 
implemented basic maze validation. Some challenges were that I forgot how to 
accumulate values through multiple rows. In this session, I learned how to structure 
validation predicates cleanly and the importance of seperating row-level and maze-level 
logic. 

Session #2 - Date: 12/09/25

Start time: 2:30pm
End time: 4:00pm 

In this session, my goal was to implement path simulation. Some challenges I faced was that 
I forgot how to initially forgot that foldl passes the accumulator as the position. In this 
session I learned the importance of checking bounds before reading the cell contents. 

Session #3 - Date: 12/10/25-12/11/25

Start time: 8:00pm 
End time: 4:00am 

The goal of this session is to implement the BFS pathfinding. I had a lot of difficulties in this 
session because the BFS kept revisting the start position which caused loops. Also the queue expansion 
produced reversed paths.