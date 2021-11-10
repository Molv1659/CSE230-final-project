# GoOL - an online Go game client program written in Haskell
CSE230-final-project

## Members
Danling Wang

Pengfei Rao

Sicheng Wang

Tianyang Zhou

## Overview
We are going to develop a Go game online client program in Haskell. Using our client program, two players can be connected by network to play Go together in a gaming room, where the board and other related information will be shown in a command-line interface built by the Brick library. The software may allow others to join the gaming room and watch the live of the ongoing game, and in order to provide a better experience, onlookers who join halfway can review the game history step by step.

As the oldest board game continuously played to the present day, the Go game has many classic problems that people study to learn good strategies for playing. Our software provides the function for users to choose from a list of classic problems and use them as good practices for improving their Go game skills.

Considering the difficulty of the Go game, a standard 19 * 19 checkerboard may be too advanced for beginners. To help novices get started, we support changing the boards to different sizes.

## Program Structure
The program is based on 3 main modules:
1. Network communication
- Supports point-to-point communication between two players;
- Supports broadcast communication to watchers;
2. Logic judgement
- Stores and retrieves the current and historic state of the board (where each piece is placed);
- Supervises user actions according to the Go game rules;
3. UI system
- Visualizes the board and the pieces according to the outputs from the logic judgement module;
- Accepts mouse/keyboard input from players/watchers and forwards them to the judgement module;
- Visualizes username/ip address of players and watchers;

## Major Workflow
1. A user lays down a new piece by clicking on the visualized board.
2. The UI system receives the coordinates of the user's input and translates it into a position on the logic board.
3. Then the logic judgement module checks whether the user’s action is valid, and then sends its response to the UI module (if true, visualizes the piece and results; if false, displays the reason to the player) and the network module;
4. The network module sends the piece coordinate to the opponent. Then it waits for the input from the opponent;
5. The network module of the opponent receives the location input, and posts it to the logic module.
6. Then the logic judgement of the opponent also validates the move. Then, the UI module displays the result on the opponent’s screen, and then waits for input.
7. The software repeats steps 1 to 6 between the two players until the logic judgement module indicates that there is a winner or a tie.

