port module Main exposing (..)

-- import Debug exposing (log)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onDoubleClick, onInput)
import Html5.DragDrop as DragDrop
import Json.Decode exposing (Value)
import Json.Encode as Json
import List.Extra
import Set
import String.Extra
import Svg exposing (line, svg)
import Svg.Attributes exposing (stroke, strokeDasharray, strokeOpacity, x1, x2, y1, y2)



---- PORTS ----


port dragstart : Value -> Cmd msg



---- MODEL ----


gridSize =
    50


type alias Model =
    { dragDrop : DragDrop.Model DraggableId DroppableId
    , groups : List Group
    , cols : Int
    , teams : List Team
    , games : List Game
    , overlay : Maybe Overlay
    , newGameCount : Int
    }


type Overlay
    = EditingGame Game
    | EditingGroup Group
    | ViewingHelp


type DraggableId
    = DraggableGame Int
    | DraggableResult ( Int, Result )


type DroppableId
    = DroppableCell Coords
    | DroppableGamePosition ( Int, Int )


type alias Group =
    { position : Int
    , name : String
    , rows : Int
    , visible : Bool
    }


type alias Coords =
    { group : Int
    , col : Int
    , row : Int
    }


type alias Game =
    { id : Int
    , name : Maybe String
    , gamePositions : List GamePosition
    , gameWinner : Maybe Int
    , coords : Coords
    }


type alias Team =
    { id : Int
    , name : String
    }


type alias GamePosition =
    { position : Int
    , firstHammer : Bool
    , assignment : Maybe Assignment
    }


type Result
    = Winner
    | Loser


type Assignment
    = TeamAssignment Int
    | GameAssignment Result Int


type alias LineConnector =
    { result : Result
    , fromCoords : ( Int, Int )
    , toCoords : ( Int, Int )
    }


initTeams : List Team
initTeams =
    [ Team 1 "Homer"
    , Team 2 "Marge"
    , Team 3 "Bart"
    , Team 4 "Lisa"
    , Team 5 "Maggie"
    , Team 6 "Krusty"
    , Team 7 "Snowball"
    , Team 8 "Apu"
    , Team 9 "Barney"
    , Team 10 "Ralph"
    , Team 11 "Itchy"
    , Team 12 "Scratchy"
    , Team 13 "Millhouse"
    , Team 14 "Moe"
    , Team 15 "Monty"
    , Team 16 "Ned"
    ]


init : ( Model, Cmd Msg )
init =
    ( { dragDrop = DragDrop.init
      , groups = [ Group 0 "A Event" 16 True, Group 1 "B Event" 8 True ]
      , cols = 25
      , teams = initTeams
      , games =
            [ Game 1
                (Just "1 vs 2")
                [ GamePosition 0 False (Just (TeamAssignment 1))
                , GamePosition 1 True (Just (TeamAssignment 2))
                ]
                Nothing
                (Coords 0 0 0)
            , Game 2
                (Just "3 vs 4")
                [ GamePosition 0 False (Just (TeamAssignment 3))
                , GamePosition 1 True (Just (TeamAssignment 4))
                ]
                Nothing
                (Coords 0 0 2)
            , Game 3
                (Just "5 vs 6")
                [ GamePosition 0 False (Just (TeamAssignment 5))
                , GamePosition 1 True (Just (TeamAssignment 6))
                ]
                Nothing
                (Coords 0 0 4)
            , Game 4
                (Just "7 vs 8")
                [ GamePosition 0 False (Just (TeamAssignment 7))
                , GamePosition 1 True (Just (TeamAssignment 8))
                ]
                Nothing
                (Coords 0 0 6)
            , Game 5
                (Just "9 vs 10")
                [ GamePosition 0 False (Just (TeamAssignment 9))
                , GamePosition 1 True (Just (TeamAssignment 10))
                ]
                Nothing
                (Coords 0 0 8)
            , Game 6
                (Just "11 vs 12")
                [ GamePosition 0 False (Just (TeamAssignment 11))
                , GamePosition 1 True (Just (TeamAssignment 12))
                ]
                Nothing
                (Coords 0 0 10)
            , Game 7
                (Just "13 vs 14")
                [ GamePosition 0 False (Just (TeamAssignment 13))
                , GamePosition 1 True (Just (TeamAssignment 14))
                ]
                Nothing
                (Coords 0 0 12)
            , Game 8
                (Just "15 vs 16")
                [ GamePosition 0 False (Just (TeamAssignment 15))
                , GamePosition 1 True (Just (TeamAssignment 16))
                ]
                Nothing
                (Coords 0 0 14)

            -- Group A Round 2
            , Game 9
                (Just "Quarterfinal 1")
                [ GamePosition 0 False (Just (GameAssignment Winner 1))
                , GamePosition 1 True (Just (GameAssignment Winner 2))
                ]
                Nothing
                (Coords 0 4 1)
            , Game 10
                (Just "Quarterfinal 2")
                [ GamePosition 0 False (Just (GameAssignment Winner 3))
                , GamePosition 1 True (Just (GameAssignment Winner 4))
                ]
                Nothing
                (Coords 0 4 5)
            , Game 11
                (Just "Quarterfinal 3")
                [ GamePosition 0 False (Just (GameAssignment Winner 5))
                , GamePosition 1 True (Just (GameAssignment Winner 6))
                ]
                Nothing
                (Coords 0 4 9)
            , Game 12
                (Just "Quarterfinal 4")
                [ GamePosition 0 False (Just (GameAssignment Winner 7))
                , GamePosition 1 True (Just (GameAssignment Winner 8))
                ]
                Nothing
                (Coords 0 4 13)

            -- Group A Semifinal
            , Game 13
                (Just "Semifinal 1")
                [ GamePosition 0 False (Just (GameAssignment Winner 9))
                , GamePosition 1 True (Just (GameAssignment Winner 10))
                ]
                Nothing
                (Coords 0 8 3)
            , Game 14
                (Just "Semifinal 2")
                [ GamePosition 0 False (Just (GameAssignment Winner 11))
                , GamePosition 1 True (Just (GameAssignment Winner 12))
                ]
                Nothing
                (Coords 0 8 11)

            -- Group A Final
            , Game 15
                (Just "Final")
                [ GamePosition 0 False (Just (GameAssignment Winner 13))
                , GamePosition 1 True (Just (GameAssignment Winner 14))
                ]
                Nothing
                (Coords 0 12 7)

            -- Group B
            , Game 16
                (Just "B 1")
                [ GamePosition 0 False (Just (GameAssignment Loser 1))
                , GamePosition 1 True (Just (GameAssignment Loser 2))
                ]
                Nothing
                (Coords 1 0 0)
            , Game 17
                (Just "B 2")
                [ GamePosition 0 False (Just (GameAssignment Loser 3))
                , GamePosition 1 True (Just (GameAssignment Loser 4))
                ]
                Nothing
                (Coords 1 0 2)
            , Game 18
                (Just "B 3")
                [ GamePosition 0 False (Just (GameAssignment Loser 5))
                , GamePosition 1 True (Just (GameAssignment Loser 6))
                ]
                Nothing
                (Coords 1 0 4)
            , Game 19
                (Just "B 4")
                [ GamePosition 0 False (Just (GameAssignment Loser 7))
                , GamePosition 1 True (Just (GameAssignment Loser 8))
                ]
                Nothing
                (Coords 1 0 6)

            -- Group B Round 2
            , Game 20
                (Just "B Semifinal 1")
                [ GamePosition 0 False (Just (GameAssignment Winner 16))
                , GamePosition 1 True (Just (GameAssignment Winner 17))
                ]
                Nothing
                (Coords 1 4 1)
            , Game 21
                (Just "B Semifinal 2")
                [ GamePosition 0 False (Just (GameAssignment Winner 18))
                , GamePosition 1 True (Just (GameAssignment Winner 19))
                ]
                Nothing
                (Coords 1 4 5)

            -- Group B Semifinal
            , Game 22
                (Just "B Final")
                [ GamePosition 0 False (Just (GameAssignment Winner 20))
                , GamePosition 1 True (Just (GameAssignment Winner 21))
                ]
                Nothing
                (Coords 1 8 3)
            ]
      , overlay = Nothing
      , newGameCount = -1
      }
    , Cmd.none
    )



--- HELPERS ---


{-| Find a game by it's coordinates
-}
findGameByCoords : List Game -> Coords -> Maybe Game
findGameByCoords games coords =
    List.Extra.find (\game -> game.coords == coords) games


{-| Move a game on the grid from one set of coords to another.
-}
moveGame : List Game -> Int -> Coords -> List Game
moveGame games gameId coords =
    List.Extra.updateIf (\game -> game.id == gameId) (\game -> { game | coords = coords }) games


connectGameResult : List Game -> ( Int, Result ) -> ( Int, Int ) -> List Game
connectGameResult games ( fromGameId, result ) ( toGameId, toPosition ) =
    let
        unassignedGamePosition gamePosition =
            -- Unassign the old connection if it exists
            case gamePosition.assignment of
                Just (GameAssignment existingResult existingGameId) ->
                    if existingResult == result && existingGameId == fromGameId then
                        { gamePosition | assignment = Nothing }

                    else
                        gamePosition

                _ ->
                    gamePosition

        unassignedGame game =
            { game | gamePositions = List.map unassignedGamePosition game.gamePositions }

        assignedGamePosition gamePosition =
            if gamePosition.position == toPosition then
                -- Assign the new connection
                { gamePosition | assignment = Just (GameAssignment result fromGameId) }

            else
                gamePosition

        assignedGame game =
            if game.id == toGameId then
                { game | gamePositions = List.map assignedGamePosition game.gamePositions }

            else
                game
    in
    List.map unassignedGame games
        |> List.map assignedGame


updatedGroups : List Group -> Group -> List Group
updatedGroups groups group =
    List.Extra.updateIf (\g -> g.position == group.position) (\g -> group) groups


{-| Add a game to the grid, with the assigned ID, but only if there's room.
-}
addGame : Int -> List Game -> Coords -> List Game
addGame id games coords =
    -- Don't add games on top of each other. Only add a game if there's room.
    case findGameByCoords games coords of
        Nothing ->
            games
                ++ [ Game id
                        Nothing
                        [ GamePosition 0 False Nothing
                        , GamePosition 1 True Nothing
                        ]
                        Nothing
                        coords
                   ]

        _ ->
            games


{-| Find the minimum required cols based on the placement of games, making sure there are enough cols for all games to be shown.
-}
minCols : List Game -> Int
minCols games =
    games
        |> List.map (\g -> g.coords.col + 3)
        |> List.maximum
        |> Maybe.withDefault 25


{-| Find the minimum required rows for a group based on the placement of games within it, making sure there are enough rows for all of a groups games to be shown.
-}
minRows : Group -> List Game -> Int
minRows group games =
    games
        |> List.filter (\g -> g.coords.group == group.position)
        |> List.map (\g -> g.coords.row + 3)
        |> List.maximum
        |> Maybe.withDefault 10


{-| Return a list of teams that haven't been assigned to a game yet.
-}
unassignedTeams : List Team -> List Game -> Maybe Game -> List Team
unassignedTeams teams games excludeGame =
    let
        gamesNotExcluded =
            case excludeGame of
                Just game ->
                    games
                        |> List.filter (\g -> g.id /= game.id)

                Nothing ->
                    games

        assignedTo team g =
            let
                assignedToPosition p =
                    case p.assignment of
                        Just (TeamAssignment id) ->
                            id == team.id

                        _ ->
                            False
            in
            List.any assignedToPosition g.gamePositions

        unassigned team =
            gamesNotExcluded
                |> List.filter (assignedTo team)
                |> List.isEmpty
    in
    List.filter unassigned teams


assignGameName : List Team -> Game -> Game
assignGameName teams game =
    case game.name of
        Nothing ->
            let
                teamName gamePosition =
                    case gamePosition.assignment of
                        Just (TeamAssignment id) ->
                            List.Extra.find (\t -> t.id == id) teams
                                |> Maybe.map (\t -> t.name)
                                |> Maybe.withDefault "TBD"

                        _ ->
                            "TBD"
            in
            { game
                | name =
                    List.map teamName game.gamePositions
                        |> String.join " vs "
                        |> Just
            }

        _ ->
            game


assignGameNamesWhenPossible : List Team -> List Game -> List Game
assignGameNamesWhenPossible teams games =
    List.map (assignGameName teams) games


trimMaybe : Maybe String -> Maybe String
trimMaybe str =
    case str of
        Just s ->
            case String.trim s of
                "" ->
                    Nothing

                s_ ->
                    Just s_

        Nothing ->
            Nothing


{-| Return a list of game results that haven't been assigned to another game yet.
We can pass in a game that should be excluded from the matching check, like the game we are currently editing, so that winner from that game is excluded.
We can also pass in a game assignment that should be included, regardless of whether or not it's been assigned. For example, the current selected assignment in the dropdown.
-}
unassignedGameResults : List Game -> Maybe Int -> Maybe Assignment -> List Assignment
unassignedGameResults games currentGameId includeAssignment =
    -- TODO: exclude and include params need to be implemented
    let
        allAssignments =
            let
                winners =
                    List.map (\game -> Just (GameAssignment Winner game.id)) games

                losers =
                    List.map (\game -> Just (GameAssignment Loser game.id)) games
            in
            winners ++ losers

        notCurrentGame assignment =
            case assignment of
                Just (GameAssignment result gameId) ->
                    not (Just gameId == currentGameId)

                _ ->
                    True

        notAssigned assignment =
            case assignment of
                Just (GameAssignment result gameId) ->
                    let
                        assignedToGamePosition gamePosition =
                            assignment == gamePosition.assignment

                        assignedToGame game =
                            List.filter assignedToGamePosition game.gamePositions
                                |> List.isEmpty
                                |> not
                    in
                    -- Check to see if this assignment has already been made against any game, except the current game.
                    List.filter assignedToGame games
                        |> List.isEmpty

                _ ->
                    True
    in
    allAssignments
        |> List.filter notCurrentGame
        |> List.filter notAssigned
        |> (::) includeAssignment
        |> List.filterMap identity


lineConnectors : List Game -> List LineConnector
lineConnectors games =
    let
        connectors : Game -> List (Maybe LineConnector)
        connectors toGame =
            let
                connectorForPosition : Int -> GamePosition -> Maybe LineConnector
                connectorForPosition toPosition gamePosition =
                    case gamePosition.assignment of
                        Just (GameAssignment result fromGameId) ->
                            let
                                fromCoords =
                                    case List.Extra.find (\g -> g.id == fromGameId) games of
                                        Just fromGame ->
                                            Just
                                                ( fromGame.coords.col * gridSize + 175
                                                , fromGame.coords.row
                                                    * gridSize
                                                    + (if result == Winner then
                                                        30

                                                       else
                                                        50
                                                      )
                                                )

                                        _ ->
                                            Nothing

                                toCoords =
                                    Just
                                        ( toGame.coords.col * gridSize + 1
                                        , toGame.coords.row
                                            * gridSize
                                            + (if toPosition == 0 then
                                                30

                                               else
                                                50
                                              )
                                        )
                            in
                            case ( fromCoords, toCoords ) of
                                ( Just from, Just to ) ->
                                    Just (LineConnector result from to)

                                _ ->
                                    Nothing

                        _ ->
                            Nothing
            in
            List.indexedMap connectorForPosition toGame.gamePositions
    in
    List.map connectors games
        |> List.concat
        |> List.filterMap identity



---- UPDATE ----


type Msg
    = DragDropMsg (DragDrop.Msg DraggableId DroppableId)
    | ToggleHelp
    | AddCol
    | RemoveCol
    | AddGroup
    | EditGroup Group
    | ToggleGroup Group
    | UpdateGroupName Group String
    | CloseEditGroup Group
    | RemoveGroup Group
    | AddGame Coords
    | RemoveGame Game
    | EditGame Game
    | UpdateGameName Game String
    | UpdateGamePosition Game Int String
    | CloseEditGame Game
    | Save
    | Revert


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DragDropMsg msg_ ->
            let
                ( model_, result ) =
                    DragDrop.update msg_ model.dragDrop
            in
            ( case result of
                Just ( DraggableGame gameId, DroppableCell coords, _ ) ->
                    { model
                        | dragDrop = model_
                        , games = moveGame model.games gameId coords
                        , groups =
                            let
                                updatedRowsForGroups : List Group -> List Game -> List Group
                                updatedRowsForGroups groups games =
                                    let
                                        updatedRows : Group -> Group
                                        updatedRows group =
                                            { group | rows = Basics.max 10 (minRows group games) }
                                    in
                                    groups
                                        |> List.map updatedRows
                            in
                            updatedRowsForGroups model.groups (moveGame model.games gameId coords)
                        , cols =
                            moveGame model.games gameId coords
                                |> minCols
                                |> Basics.max 22
                                |> (+) 3
                    }

                Just ( DraggableResult from, DroppableGamePosition to, _ ) ->
                    -- TODO: Team dragging.
                    { model
                        | dragDrop = model_
                        , games = connectGameResult model.games from to
                    }

                _ ->
                    { model | dragDrop = model_ }
            , DragDrop.getDragstartEvent msg_
                |> Maybe.map (.event >> dragstart)
                |> Maybe.withDefault Cmd.none
            )

        ToggleHelp ->
            ( { model
                | overlay =
                    case model.overlay of
                        Just ViewingHelp ->
                            Nothing

                        _ ->
                            Just ViewingHelp
              }
            , Cmd.none
            )

        AddCol ->
            -- NOT DONE: Add columns automatically when a game is dropped on the last column
            ( { model | cols = model.cols + 5 }
            , Cmd.none
            )

        RemoveCol ->
            ( { model | cols = model.cols - 5 }
            , Cmd.none
            )

        AddGroup ->
            let
                nextGroupId =
                    List.length model.groups
            in
            ( { model
                | groups = model.groups ++ [ Group nextGroupId ("Group " ++ String.fromInt (nextGroupId + 1)) 8 True ]
              }
            , Cmd.none
            )

        EditGroup group ->
            ( { model | overlay = Just (EditingGroup group) }
            , Cmd.none
            )

        ToggleGroup group ->
            let
                updatedGroup : Group
                updatedGroup =
                    { group | visible = not group.visible }
            in
            ( { model | groups = updatedGroups model.groups updatedGroup }
            , Cmd.none
            )

        UpdateGroupName group name ->
            let
                updatedGroup : Group
                updatedGroup =
                    { group | name = name }
            in
            ( { model | overlay = Just (EditingGroup updatedGroup) }
            , Cmd.none
            )

        CloseEditGroup group ->
            ( { model
                | overlay = Nothing
                , groups = updatedGroups model.groups group
              }
            , Cmd.none
            )

        RemoveGroup group ->
            ( { model
                | overlay = Nothing
                , groups = List.Extra.remove group model.groups
              }
            , Cmd.none
            )

        AddGame coords ->
            ( { model
                | games = addGame model.newGameCount model.games coords
                , newGameCount = model.newGameCount - 1
              }
            , Cmd.none
            )

        RemoveGame game ->
            ( { model
                | overlay = Nothing
                , games = List.Extra.remove game model.games
              }
            , Cmd.none
            )

        EditGame game ->
            ( { model | overlay = Just (EditingGame game) }, Cmd.none )

        UpdateGameName game name ->
            let
                maybeName : Maybe String
                maybeName =
                    case name of
                        "" ->
                            Nothing

                        _ ->
                            Just name

                updatedGame : Game
                updatedGame =
                    { game | name = maybeName }
            in
            ( { model
                | overlay = Just (EditingGame updatedGame)
              }
            , Cmd.none
            )

        UpdateGamePosition game position assignment ->
            -- TODO
            let
                typedAssignment : Maybe Assignment
                typedAssignment =
                    let
                        parsedAssignment =
                            String.split "_" assignment
                    in
                    case parsedAssignment of
                        x :: xs ->
                            case x of
                                "" ->
                                    Nothing

                                "team" ->
                                    -- A team was selected
                                    case List.head xs of
                                        Just teamIdStr ->
                                            case String.toInt teamIdStr of
                                                Just teamId ->
                                                    case List.Extra.find (\t -> t.id == teamId) model.teams of
                                                        Just team ->
                                                            Just (TeamAssignment team.id)

                                                        Nothing ->
                                                            Nothing

                                                Nothing ->
                                                    Nothing

                                        _ ->
                                            Nothing

                                gameResult ->
                                    -- A winner / loser from game was selected
                                    case List.head xs of
                                        Just gameIdStr ->
                                            case String.toInt gameIdStr of
                                                Just gameId ->
                                                    case List.Extra.find (\g -> g.id == gameId) model.games of
                                                        Just g ->
                                                            -- Pattern match on position to assign to and whether it was a winner or loser from game
                                                            -- TODO Do we want to deselect from other game's game positions, like in connectGameResult?
                                                            -- fromGameId == g.id
                                                            -- result == typedGameResult
                                                            -- toGameId == game.id
                                                            -- toPosition == position
                                                            let
                                                                typedGameResult =
                                                                    case gameResult of
                                                                        "winner" ->
                                                                            Winner

                                                                        _ ->
                                                                            Loser
                                                            in
                                                            Just (GameAssignment typedGameResult g.id)

                                                        Nothing ->
                                                            Nothing

                                                Nothing ->
                                                    Nothing

                                        _ ->
                                            Nothing

                        _ ->
                            Nothing

                updatedGames =
                    case typedAssignment of
                        Just (GameAssignment typedGameResult fromGameId) ->
                            connectGameResult model.games ( fromGameId, typedGameResult ) ( game.id, position )

                        Just (TeamAssignment teamId) ->
                            let
                                updatedGamePosition gamePosition =
                                    if gamePosition.position == position then
                                        { gamePosition | assignment = Just (TeamAssignment teamId) }

                                    else
                                        gamePosition

                                updatedGame g =
                                    { g | gamePositions = List.map updatedGamePosition g.gamePositions }
                            in
                            List.map updatedGame model.games

                        _ ->
                            model.games

                editingGame =
                    List.Extra.find (\g -> g.id == game.id) updatedGames
                        |> Maybe.map EditingGame
            in
            ( { model
                | overlay = editingGame
                , games = updatedGames
              }
            , Cmd.none
            )

        CloseEditGame game ->
            let
                -- Assign a name based on teams, if we have teams but no user specified name.
                updatedGame : Game
                updatedGame =
                    case trimMaybe game.name of
                        Nothing ->
                            assignGameName model.teams game

                        _ ->
                            game
            in
            ( { model
                | overlay = Nothing
                , games = List.Extra.updateIf (\g -> g.coords == game.coords) (\g -> game) model.games
              }
            , Cmd.none
            )

        Save ->
            ( model, Cmd.none )

        Revert ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        dragId =
            DragDrop.getDragId model.dragDrop

        dropId =
            DragDrop.getDropId model.dragDrop

        modalOpen =
            model.overlay /= Nothing
    in
    div [ classList [ ( "modal-open", modalOpen ) ] ]
        [ div [ class "p-3" ]
            [ div [ class "d-flex justify-content-between" ]
                [ h2 [] [ text "Curling I/O Bracket Builder Demo" ]
                , div [ style "min-width" "100px", class "text-right" ]
                    [ button [ class "btn btn-info btn-sm", onClick ToggleHelp ]
                        [ text "Help" ]
                    ]
                ]
            , viewGroups model dragId dropId
            , button [ class "btn btn-primary", onClick AddGroup ] [ text "Add Group" ]
            , if modalOpen then
                viewOverlay model

              else
                text ""
            ]
        , div [ classList [ ( "modal-backdrop", modalOpen ), ( "show", modalOpen ) ] ] []
        ]


viewOverlay : Model -> Html Msg
viewOverlay model =
    div [ class "modal", style "display" "block" ]
        [ div [ class "modal-dialog" ]
            [ case model.overlay of
                Just ViewingHelp ->
                    viewHelp

                Just (EditingGame game) ->
                    viewEditGame model game

                Just (EditingGroup group) ->
                    viewEditGroup model group

                Nothing ->
                    text ""
            ]
        ]


viewHelp : Html Msg
viewHelp =
    div [ class "modal-content" ]
        [ div [ class "modal-header" ]
            [ h2 [ class "modal-title" ] [ text "Help" ] ]
        , div [ class "modal-body" ]
            [ h3 [] [ text "Games" ]
            , ul []
                [ li [] [ text "Drag and drop games anywhere you like." ]
                , li [] [ text "Double click an empty area in a group to add a new game." ]
                , li [] [ text "Double click on a game to edit it." ]
                , li [] [ text "Click the ✘ in the top right corner of a game to remove it." ]
                , li [] [ text "The grid will automatically grow or shrink as you move games near it's edges." ]
                , li [] [ text "The green circles represent the potential winners of a game. Click and drag it to another game to assign who the winner plays next." ]
                , li [] [ text "The red circles represent the potential losers of a game. Click and drag it to another game to assign who the loser plays next." ]
                ]
            , h3 [] [ text "Groups" ]
            , ul []
                [ li [] [ text "Click on a group name to temporarily hide it so you can better see other groups." ]
                , li [] [ text "Click the ✎ icon next to a group name to edit its name." ]
                ]
            , h3 [] [ text "Saving" ]
            , ul []
                [ li [] [ text "Saving changes is currently disabled for this demo. Nothing you do will be saved / persisted when you reload the page." ]
                ]
            ]
        , div [ class "modal-footer" ]
            [ button [ class "btn btn-primary", onClick ToggleHelp ] [ text "Close" ]
            ]
        ]


viewEditGroup : Model -> Group -> Html Msg
viewEditGroup model group =
    let
        hasNoGames : Bool
        hasNoGames =
            model.games
                |> List.filter (\g -> g.coords.group == group.position)
                |> List.isEmpty
                |> not

        hasNoName : Bool
        hasNoName =
            String.trim group.name == ""
    in
    div [ class "modal-content" ]
        [ div [ class "modal-header" ]
            [ h5 [ class "modal-title" ] [ text "Edit Group" ] ]
        , div [ class "modal-body" ]
            [ div
                [ class "form-group" ]
                [ label [ for "editing-group-name" ] [ text "Group Name" ]
                , input
                    [ class "form-control"
                    , id "editing-group-name"
                    , value group.name
                    , onInput (UpdateGroupName group)
                    ]
                    []
                ]
            ]
        , div [ class "modal-footer d-flex justify-content-between" ]
            [ button [ onClick (RemoveGroup group), class "btn btn-danger mr-2", disabled hasNoGames ] [ text "Remove" ]
            , button [ onClick (CloseEditGroup group), class "btn btn-primary", disabled hasNoName ] [ text "Close" ]
            ]
        ]


viewEditGame : Model -> Game -> Html Msg
viewEditGame model game =
    let
        teamOption : Int -> GamePosition -> Team -> Html Msg
        teamOption index selectedGamePosition team =
            let
                isSelected =
                    case selectedGamePosition.assignment of
                        Just (TeamAssignment id) ->
                            id == team.id

                        _ ->
                            False
            in
            option [ value ("team_" ++ String.fromInt team.id), selected isSelected ] [ text team.name ]

        gameOption : Int -> GamePosition -> Assignment -> Html Msg
        gameOption index selectedGamePosition assignment =
            let
                isSelected =
                    selectedGamePosition.assignment == Just assignment

                gameId =
                    case assignment of
                        GameAssignment Winner id ->
                            Just ("winner_" ++ String.fromInt id)

                        GameAssignment Loser id ->
                            Just ("winner_" ++ String.fromInt id)

                        _ ->
                            Nothing

                gameLabel =
                    let
                        gameName id =
                            List.Extra.find (\g -> g.id == id) model.games
                                |> Maybe.map (\g -> Maybe.withDefault "TBD" g.name)
                    in
                    case assignment of
                        GameAssignment Winner id ->
                            Just ("Winner of " ++ Maybe.withDefault "TBD" (gameName id))

                        GameAssignment Loser id ->
                            Just ("Loser of " ++ Maybe.withDefault "TBD" (gameName id))

                        _ ->
                            Nothing
            in
            case ( gameId, gameLabel ) of
                ( Just id, Just label ) ->
                    option [ value id, selected isSelected ] [ text label ]

                _ ->
                    text ""

        assignmentOptions : Int -> GamePosition -> List (Html Msg)
        assignmentOptions index selectedGamePosition =
            [ option [] [] ]
                ++ (unassignedTeams model.teams model.games (Just game)
                        |> List.map (teamOption index selectedGamePosition)
                   )
                ++ (unassignedGameResults model.games (Just game.id) selectedGamePosition.assignment
                        |> List.map (gameOption index selectedGamePosition)
                   )

        viewGamePositionField index gamePosition =
            div
                [ class "form-group" ]
                [ label [ for "editing-game" ] [ text "Team" ]
                , select
                    [ class "form-control"
                    , id "editing-game"
                    , onInput (UpdateGamePosition game index)
                    ]
                    (assignmentOptions index gamePosition)
                ]
    in
    div [ class "modal-content" ]
        [ div [ class "modal-header" ]
            [ h5 [ class "modal-title" ] [ text "Edit Game" ] ]
        , div [ class "modal-body" ]
            ([ div
                [ class "form-group" ]
                [ label [ for "editing-game-name" ] [ text "Game Name" ]
                , input
                    [ class "form-control"
                    , id "editing-game-name"
                    , value (Maybe.withDefault "" game.name)
                    , onInput (UpdateGameName game)
                    ]
                    []
                ]
             ]
                ++ List.indexedMap viewGamePositionField game.gamePositions
            )
        , div [ class "modal-footer" ]
            [ button [ onClick (CloseEditGame game), class "btn btn-primary mr-2" ] [ text "Close" ]
            ]
        ]


viewGroups : Model -> Maybe DraggableId -> Maybe DroppableId -> Html Msg
viewGroups model dragId dropId =
    div []
        (List.map (viewGroup model dragId dropId) model.groups)


viewGroup : Model -> Maybe DraggableId -> Maybe DroppableId -> Group -> Html Msg
viewGroup model dragId dropId group =
    let
        groupGames =
            List.filter (\g -> g.coords.group == group.position) model.games
    in
    div
        [ class "group-container" ]
        [ div
            [ class "d-flex" ]
            [ div
                [ class "group-name btn btn-default"
                , onClick (ToggleGroup group)
                ]
                [ text ("☷ " ++ group.name) ]
            , div
                [ class "btn btn-default px-0"
                , onClick (EditGroup group)
                ]
                [ text "✎" ]
            ]
        , div [ class "group" ]
            (if group.visible then
                [ viewSvgLines dragId group groupGames
                , table
                    []
                    (List.map (viewRow model dragId dropId group) (List.range 0 (group.rows - 1)))
                , viewGames dragId dropId model.teams model.games groupGames
                ]

             else
                [ div [ class "text-muted group-hide" ] [ text "..." ] ]
            )
        ]


viewRow : Model -> Maybe DraggableId -> Maybe DroppableId -> Group -> Int -> Html Msg
viewRow model dragId dropId group row =
    tr
        []
        (List.map (viewCell model dragId dropId group row) (List.range 0 (model.cols - 1)))


viewCell : Model -> Maybe DraggableId -> Maybe DroppableId -> Group -> Int -> Int -> Html Msg
viewCell model dragId dropId group row col =
    let
        onCoords =
            Coords group.position col row

        onGame =
            findGameByCoords model.games onCoords

        highlighted =
            case ( dragId, dropId ) of
                ( Just (DraggableGame _), Just (DroppableCell coords) ) ->
                    if coords == onCoords then
                        True

                    else
                        False

                _ ->
                    False
    in
    td
        ([ style "width" (String.fromInt gridSize ++ "px")
         , style "height" (String.fromInt gridSize ++ "px")
         , classList [ ( "drop-target", highlighted ) ]
         ]
            ++ DragDrop.droppable DragDropMsg (DroppableCell onCoords)
            ++ [ onDoubleClick (AddGame onCoords) ]
        )
        []


viewGames : Maybe DraggableId -> Maybe DroppableId -> List Team -> List Game -> List Game -> Html Msg
viewGames dragId dropId teams games groupGames =
    div [ class "games" ] (List.map (viewGame dragId dropId teams games) groupGames)


viewGame : Maybe DraggableId -> Maybe DroppableId -> List Team -> List Game -> Game -> Html Msg
viewGame dragId dropId teams games game =
    let
        dragging =
            case dragId of
                Just (DraggableGame gameId) ->
                    gameId == game.id

                _ ->
                    False
    in
    div
        ([ classList [ ( "game", True ), ( "dragging-game", dragging ) ]
         , style "left" (String.fromInt (game.coords.col * gridSize) ++ "px")
         , style "top" (String.fromInt (game.coords.row * gridSize) ++ "px")
         , onDoubleClick (EditGame game)
         ]
            ++ DragDrop.draggable DragDropMsg (DraggableGame game.id)
        )
        [ div
            [ class "d-flex game-header" ]
            [ div
                [ class "game-name flex-fill" ]
                [ text (Maybe.withDefault "TDB" game.name) ]
            , div
                [ class "game-delete align-self-end"
                , onClick (RemoveGame game)
                ]
                [ text "✘" ]
            ]
        , div [ class "game-body d-flex" ]
            [ div
                [ class "game-positions flex-fill" ]
                (List.indexedMap (\index gamePosition -> viewGamePosition dragId dropId teams games game.id index gamePosition) game.gamePositions)
            , div [ class "align-self-end ml-1" ] (viewResultConnectors dragId game.id)
            ]
        ]


viewGamePosition : Maybe DraggableId -> Maybe DroppableId -> List Team -> List Game -> Int -> Int -> GamePosition -> Html Msg
viewGamePosition dragId dropId teams games gameId position gamePosition =
    let
        positionClass =
            if position == 0 then
                ( "game-top", True )

            else
                ( "game-bottom", True )

        dropTarget =
            case ( dragId, dropId ) of
                ( Just (DraggableResult _), Just (DroppableGamePosition gameIdAndPosition) ) ->
                    if gameIdAndPosition == ( gameId, position ) then
                        True

                    else
                        False

                _ ->
                    False

        label =
            case gamePosition.assignment of
                Just (TeamAssignment id) ->
                    case List.Extra.find (\t -> t.id == id) teams of
                        Just team ->
                            team.name

                        Nothing ->
                            "TBD"

                Just (GameAssignment Winner id) ->
                    case List.Extra.find (\g -> g.id == id) games of
                        Just g ->
                            "W: " ++ Maybe.withDefault "TDB" g.name

                        Nothing ->
                            "TBD"

                Just (GameAssignment Loser id) ->
                    case List.Extra.find (\g -> g.id == id) games of
                        Just g ->
                            "L: " ++ Maybe.withDefault "TDB" g.name

                        Nothing ->
                            "TBD"

                Nothing ->
                    "TBD"
    in
    div
        ([ classList [ positionClass, ( "drop-target", dropTarget ) ] ] ++ DragDrop.droppable DragDropMsg (DroppableGamePosition ( gameId, position )))
        [ text label ]


viewResultConnectors : Maybe DraggableId -> Int -> List (Html Msg)
viewResultConnectors dragId gameId =
    let
        dragging result =
            case dragId of
                Just (DraggableResult ( id, result_ )) ->
                    id == gameId && result_ == result

                _ ->
                    False
    in
    [ div
        ([ classList
            [ ( "game-result-connector", True )
            , ( "dragging-connector", dragging Winner )
            ]
         ]
            ++ DragDrop.draggable DragDropMsg (DraggableResult ( gameId, Winner ))
        )
        [ div [ class "game-result-connector-icon game-result-connector-winner" ] [ text "W" ]
        ]
    , div
        ([ classList
            [ ( "game-result-connector", True )
            , ( "dragging-connector", dragging Loser )
            ]
         ]
            ++ DragDrop.draggable DragDropMsg (DraggableResult ( gameId, Loser ))
        )
        [ div [ class "game-result-connector-icon game-result-connector-loser" ] [ text "L" ]
        ]
    ]


viewSvgLines : Maybe DraggableId -> Group -> List Game -> Html Msg
viewSvgLines dragId group games =
    let
        dragging =
            not (dragId == Nothing)

        viewSvgLine l =
            line
                [ x1 (String.fromInt (Tuple.first l.fromCoords))
                , y1 (String.fromInt (Tuple.second l.fromCoords))
                , x2 (String.fromInt (Tuple.first l.toCoords))
                , y2 (String.fromInt (Tuple.second l.toCoords))
                , strokeOpacity "0.6"
                , strokeDasharray "3"
                , stroke
                    (case l.result of
                        Winner ->
                            "green"

                        Loser ->
                            "red"
                    )
                ]
                []
    in
    div [ classList [ ( "group-lines", True ), ( "group-lines-while-dragging", dragging ) ] ]
        [ svg
            [ Svg.Attributes.width (String.fromInt ((minCols games + 1) * gridSize)), Svg.Attributes.height (String.fromInt ((minRows group games - 1) * gridSize)) ]
            (List.map viewSvgLine (lineConnectors games))
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
