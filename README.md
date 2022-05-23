# Bracket Builder

## Running

1. Install node (nvm is a good option)
2. Install elm (via node or some other way)
3. In project directory run `npm install`
4. Install the json-server if you want to use it. `npm install json-server -g`
5. `./run.sh` to run the client.
6. `./server.sh` to run the server.

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

### Games

We need a list of games.
TODO: Maybe break these out of the bracket data structure?

* id: int (a negative integer is used for new games, created via the bracket builder)
* name: string
* coords: a record of coordinates, not sure this needs to be a complex type.
* game_positions: array of game positions

#### Coords

TODO: Maybe we just flatten these onto the game, instead of having their own type.

* group: int (refers to the parent group in the bracket)
* col: int (the column / x-axis)
* row: int (the row / y-axis)

#### Game Position

* position: int (0 == top, 1 == bottom)
* lsfe: bool (whether the team at this position has the last shot in the first end, a.k.a first hammer)
* assignment: complex type, can refer to a team or a result from a previous game.

##### Assignment

TODO: Look into other options here. Not sure we need a complex type. Could just have team_id, from_game_id, from_result on the game position instead.
Neither option feels particularly clean, but flatter is usually better if it's not obvious it should be a complex type.

* assignment_type: string (enum that must be "team" or "game")
* result: string (enum that must be either "winner" or "loser", or not present for a team assignment)
* id: int (the id of the team or the game, depending on assignment type)

### Bracket

Right now the bracket holds all of the games and teams, but that may need to change, at least for teams.

* id: int
* name: string
