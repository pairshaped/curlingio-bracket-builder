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

## For Contributors

### Installing Dependencies

We use elm and elm-live for development. You can install these via npm.

```
npm install
```

### Running It

Edit dev.html and configure the application's parameters for your environment. Then run it:

```
npm start
```

### Production Deployment

Make sure you have uglify-js installed to compress the production js.
```
npm install -g uglify-js
```

Compile and optimize for production using:

```
./prod.sh
```

## Flags

* baseUrl: string (this is the url for the event, under which teams and brackets are nested)
* id: int (this is the bracket id, if we're editing an existing bracket)


## Source
<https://github.com/pairshaped/curlingio-bracket-builder>

## Copyright and License

[See LICENSE.md](LICENSE.md)
