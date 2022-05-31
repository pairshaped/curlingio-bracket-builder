# Bracket Builder for [Curling I/O](https://curling.io)


![Bracket Builder for Curling I/O](curlingio-bracket-builder.gif?raw=true "Bracket Builder for Curling I/O")

## Features

* You can create games.
* You can drag and drop games.
* You can assign teams to each game.
* You can connect winners or losers from one game to another game.
* You can add multiple groups (A Event, B Event, etc.).
* You can connect games between groups.
* You can rename games and groups.

![Bracket Builder Cheatsheet for Curling I/O](curlingio-bracket-builder-cheatsheet.png?raw=true "Bracket Builder Cheatsheet for Curling I/O")

[Documentation](https://curling.io/docs/event-management/playoff-brackets)

## Installing Dependencies

```
yarn
```

## Running It

```
yarn start
```

## Flags

* demoMode: bool (whether or not to run in demo mode)
* baseUrl: string (this is the url for the event, under which teams and brackets are nested)
* id: int (this is the bracket id, if we're editing an existing bracket)

## Data Structures

We have two roote data structures, the teams (read only), and the bracket. The bracket contains all of the games.

### Teams

The list of teams participating in the event that can be assigned to games.

* id: int
* name: string

### Bracket

* id: int
* name: string

#### Games

* id: int (a negative integer is used for new games, created via the bracket builder)
* name: string
* coords: a record of coordinates, not sure this needs to be a complex type.
* game_positions: array of game positions

##### Coords

* group_id: int (refers to the parent group in the bracket)
* col: int (the column / x-axis)
* row: int (the row / y-axis)

##### Game Position

* position: int (0 == top, 1 == bottom)
* lsfe: bool (whether the team at this position has the last shot in the first end, a.k.a first hammer)
* assignment: complex type, can refer to a team or a result from a previous game.
* team_id: ID of the team if a team is assigned.
* winner_id: ID of the game if a game winner is assigned.
* loser_id: ID of the game if a game loser is assigned.


## Source
<https://github.com/pairshaped/curlingio-bracket-builder>

## Copyright and License

Curling I/O Bracket Builder
Copyright (C) 2022 Curling I/O

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
