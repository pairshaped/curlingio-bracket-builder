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



---- PORTS ----


port dragstart : Value -> Cmd msg



---- MODEL ----


type alias Model =
    { dragDrop : DragDrop.Model Coords Coords
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


type alias Coords =
    { group : Int
    , row : Int
    , col : Int
    }


type alias Group =
    { position : Int
    , name : String
    , rows : Int
    , visible : Bool
    }


type alias Game =
    { id : Int
    , name : Maybe String
    , top : Maybe GamePosition
    , bottom : Maybe GamePosition
    , gameWinner : Maybe GameWinner
    , coords : Coords
    }


type alias Team =
    { id : Int
    , name : String
    }


type GamePosition
    = TeamAssignment Int
    | GameAssignment GameResult Int


type GameResult
    = Winner
    | Loser


type GameWinner
    = Top
    | Bottom


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
      , cols = 14
      , teams = initTeams
      , games =
            [ Game 1 (Just "1 vs 2") (Just (TeamAssignment 1)) (Just (TeamAssignment 2)) Nothing (Coords 0 0 0)
            , Game 2 (Just "3 vs 4") (Just (TeamAssignment 3)) (Just (TeamAssignment 4)) Nothing (Coords 0 2 0)
            , Game 3 (Just "5 vs 6") (Just (TeamAssignment 5)) (Just (TeamAssignment 6)) Nothing (Coords 0 4 0)
            , Game 4 (Just "7 vs 8") (Just (TeamAssignment 7)) (Just (TeamAssignment 8)) Nothing (Coords 0 6 0)
            , Game 5 (Just "9 vs 10") (Just (TeamAssignment 9)) (Just (TeamAssignment 10)) Nothing (Coords 0 8 0)
            , Game 6 (Just "11 vs 12") (Just (TeamAssignment 11)) (Just (TeamAssignment 12)) Nothing (Coords 0 10 0)
            , Game 7 (Just "13 vs 14") (Just (TeamAssignment 13)) (Just (TeamAssignment 14)) Nothing (Coords 0 12 0)
            , Game 8 (Just "15 vs 16") (Just (TeamAssignment 15)) (Just (TeamAssignment 16)) Nothing (Coords 0 14 0)

            -- Group A Round 2
            , Game 9 (Just "Quarterfinal 1") (Just (GameAssignment Winner 1)) (Just (GameAssignment Winner 2)) Nothing (Coords 0 1 2)
            , Game 10 (Just "Quarterfinal 2") (Just (GameAssignment Winner 3)) (Just (GameAssignment Winner 4)) Nothing (Coords 0 5 2)
            , Game 11 (Just "Quarterfinal 3") (Just (GameAssignment Winner 5)) (Just (GameAssignment Winner 6)) Nothing (Coords 0 9 2)
            , Game 12 (Just "Quarterfinal 4") (Just (GameAssignment Winner 7)) (Just (GameAssignment Winner 8)) Nothing (Coords 0 13 2)

            -- Group A Semifinal
            , Game 13 (Just "Semifinal 1") (Just (GameAssignment Winner 9)) (Just (GameAssignment Winner 10)) Nothing (Coords 0 3 4)
            , Game 14 (Just "Semifinal 2") (Just (GameAssignment Winner 11)) (Just (GameAssignment Winner 12)) Nothing (Coords 0 11 4)

            -- Group A Final
            , Game 15 (Just "Final") (Just (GameAssignment Winner 13)) (Just (GameAssignment Winner 14)) Nothing (Coords 0 7 6)

            -- Group B
            , Game 16 (Just "B 1") (Just (GameAssignment Loser 1)) (Just (GameAssignment Loser 2)) Nothing (Coords 1 0 0)
            , Game 17 (Just "B 2") (Just (GameAssignment Loser 3)) (Just (GameAssignment Loser 4)) Nothing (Coords 1 2 0)
            , Game 18 (Just "B 3") (Just (GameAssignment Loser 5)) (Just (GameAssignment Loser 6)) Nothing (Coords 1 4 0)
            , Game 19 (Just "B 4") (Just (GameAssignment Loser 7)) (Just (GameAssignment Loser 8)) Nothing (Coords 1 6 0)

            -- Group B Round 2
            , Game 20 (Just "B Semifinal 1") (Just (GameAssignment Winner 16)) (Just (GameAssignment Winner 17)) Nothing (Coords 1 1 2)
            , Game 21 (Just "B Semifinal 2") (Just (GameAssignment Winner 18)) (Just (GameAssignment Winner 19)) Nothing (Coords 1 5 2)

            -- Group B Semifinal
            , Game 22 (Just "B Final") (Just (GameAssignment Winner 20)) (Just (GameAssignment Winner 21)) Nothing (Coords 1 3 4)
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
moveGame : List Game -> Coords -> Coords -> List Game
moveGame games fromCoords toCoords =
    List.Extra.updateIf (\game -> game.coords == fromCoords) (\game -> { game | coords = toCoords }) games


updatedGames : List Game -> Game -> List Game
updatedGames games game =
    List.Extra.updateIf (\g -> g.coords == game.coords) (\g -> game) games


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
            games ++ [ Game id Nothing Nothing Nothing Nothing coords ]

        _ ->
            games


{-| Find the minimum required rows for a group based on the placement of games within it, making sure there are enough rows for all of a groups games to be shown.
-}
minRowsForGroup : Group -> List Game -> Int
minRowsForGroup group games =
    games
        |> List.filter (\g -> g.coords.group == group.position)
        |> List.map (\g -> g.coords.row + 2)
        |> List.maximum
        |> Maybe.withDefault 8


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
                assignedTop =
                    case g.top of
                        Just (TeamAssignment id) ->
                            id == team.id

                        _ ->
                            False

                assignedBottom =
                    case g.bottom of
                        Just (TeamAssignment id) ->
                            id == team.id

                        _ ->
                            False
            in
            assignedTop || assignedBottom

        unassigned team =
            gamesNotExcluded
                |> List.filter (assignedTo team)
                |> List.isEmpty
    in
    List.filter unassigned teams


assignGameNamesWhenPossible : List Team -> List Game -> List Game
assignGameNamesWhenPossible teams games =
    let
        assignName game =
            case game.name of
                Nothing ->
                    -- Check if we have 2 teams assigned and auto generate the name if we do
                    case ( game.top, game.bottom ) of
                        ( Just (TeamAssignment topId), Just (TeamAssignment bottomId) ) ->
                            let
                                team1Maybe =
                                    List.Extra.find (\t -> t.id == topId) teams

                                team2Maybe =
                                    List.Extra.find (\t -> t.id == bottomId) teams
                            in
                            case ( team1Maybe, team2Maybe ) of
                                ( Just team1, Just team2 ) ->
                                    { game | name = Just (team1.name ++ " vs " ++ team2.name) }

                                _ ->
                                    game

                        _ ->
                            game

                _ ->
                    game
    in
    List.map assignName games


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


{-| Return a list of games who's winner or loser hasn't been assigned to another game yet.
We can pass in a game that should be excluded from the matching check, like the game we are currently editing, so that winner from that game is excluded.
We can also pass in a game assignment that should be included, regardless of whether or not it's been assigned. For example, the current selected assignment in the dropdown.
-}
unassignedGames : GameResult -> Maybe Int -> Maybe Int -> List Game -> List Game
unassignedGames gameResult excludeGameId includeGameId games =
    let
        isAssignedToGame : Game -> Game -> Bool
        isAssignedToGame game gameWithAssignment =
            if Just game.id == includeGameId then
                False

            else if game.name == Nothing then
                True

            else
                case gameResult of
                    Winner ->
                        case ( gameWithAssignment.top, gameWithAssignment.bottom ) of
                            ( Just (GameAssignment Winner topId), Just (GameAssignment Winner bottomId) ) ->
                                topId == game.id || bottomId == game.id

                            ( Just (GameAssignment Winner id), _ ) ->
                                id == game.id

                            ( _, Just (GameAssignment Winner id) ) ->
                                id == game.id

                            _ ->
                                False

                    Loser ->
                        case ( gameWithAssignment.top, gameWithAssignment.bottom ) of
                            ( Just (GameAssignment Loser topId), Just (GameAssignment Loser bottomId) ) ->
                                topId == game.id || bottomId == game.id

                            ( Just (GameAssignment Loser id), _ ) ->
                                id == game.id

                            ( _, Just (GameAssignment Loser id) ) ->
                                id == game.id

                            _ ->
                                False

        hasNotBeenAssigned : Game -> Bool
        hasNotBeenAssigned game =
            games
                |> List.filter (isAssignedToGame game)
                |> List.isEmpty
    in
    List.filter hasNotBeenAssigned games



---- UPDATE ----


type Msg
    = DragDropMsg (DragDrop.Msg Coords Coords)
    | ToggleHelp
    | AddCol
    | RemoveCol
    | AddGroup
    | EditGroup Group
    | ToggleGroup Group
    | UpdateGroupName Group String
    | UpdateGroupRows Group String
    | CloseEditGroup Group
    | RemoveGroup Group
    | AddGame Coords
    | RemoveGame Game
    | EditGame Game
    | UpdateGameName Game String
    | UpdateGamePosition Game String String
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
            ( { model
                | dragDrop = model_
                , games =
                    case result of
                        Nothing ->
                            model.games

                        Just ( fromCoords, toCoords, { x, y } ) ->
                            moveGame model.games fromCoords toCoords
                , groups =
                    case result of
                        Nothing ->
                            model.groups

                        Just ( fromCoords, toCoords, { x, y } ) ->
                            let
                                updatedRowsForGroups : List Group -> List Game -> List Group
                                updatedRowsForGroups groups games =
                                    let
                                        updatedRows : Group -> Group
                                        updatedRows group =
                                            { group | rows = Basics.max group.rows (minRowsForGroup group games + 1) }
                                    in
                                    groups
                                        |> List.map updatedRows
                            in
                            updatedRowsForGroups model.groups (moveGame model.games fromCoords toCoords)
              }
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
            ( { model | cols = model.cols + 1 }
            , Cmd.none
            )

        RemoveCol ->
            ( { model | cols = model.cols - 1 }
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

        UpdateGroupRows group rows ->
            let
                updatedGroup : Group
                updatedGroup =
                    case String.toInt rows of
                        Just i ->
                            { group | rows = i }

                        Nothing ->
                            group
            in
            ( { model
                | overlay = Just (EditingGroup updatedGroup)
                , groups =
                    updatedGroups model.groups updatedGroup
              }
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
            let
                updatedGame : Game
                updatedGame =
                    let
                        parsedAssignment =
                            String.split "_" assignment
                    in
                    case parsedAssignment of
                        x :: xs ->
                            case x of
                                "" ->
                                    -- Blank was selected
                                    case position of
                                        "top" ->
                                            { game | top = Nothing }

                                        "bottom" ->
                                            { game | bottom = Nothing }

                                        _ ->
                                            game

                                "team" ->
                                    -- A team was selected
                                    case List.head xs of
                                        Just teamIdStr ->
                                            case String.toInt teamIdStr of
                                                Just teamId ->
                                                    case List.Extra.find (\t -> t.id == teamId) model.teams of
                                                        Just team ->
                                                            case position of
                                                                "top" ->
                                                                    { game | top = Just (TeamAssignment team.id) }

                                                                "bottom" ->
                                                                    { game | bottom = Just (TeamAssignment team.id) }

                                                                _ ->
                                                                    game

                                                        Nothing ->
                                                            game

                                                Nothing ->
                                                    game

                                        _ ->
                                            game

                                gameResult ->
                                    -- A winner / loser from game was selected
                                    case List.head xs of
                                        Just gameIdStr ->
                                            case String.toInt gameIdStr of
                                                Just gameId ->
                                                    case List.Extra.find (\g -> g.id == gameId) model.games of
                                                        Just g ->
                                                            -- Pattern match on position to assign to and whether it was a winner or loser from game
                                                            case ( position, gameResult ) of
                                                                ( "top", "winner" ) ->
                                                                    { game | top = Just (GameAssignment Winner g.id) }

                                                                ( "top", "loser" ) ->
                                                                    { game | top = Just (GameAssignment Loser g.id) }

                                                                ( "bottom", "winner" ) ->
                                                                    { game | bottom = Just (GameAssignment Winner g.id) }

                                                                ( "bottom", "loser" ) ->
                                                                    { game | bottom = Just (GameAssignment Loser g.id) }

                                                                _ ->
                                                                    game

                                                        Nothing ->
                                                            game

                                                Nothing ->
                                                    game

                                        _ ->
                                            game

                        _ ->
                            game
            in
            ( { model
                | overlay = Just (EditingGame updatedGame)
                , games = updatedGames model.games updatedGame
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
                            case ( game.top, game.bottom ) of
                                ( Just (TeamAssignment topId), Just (TeamAssignment bottomId) ) ->
                                    let
                                        team1Maybe =
                                            List.Extra.find (\t -> t.id == topId) model.teams

                                        team2Maybe =
                                            List.Extra.find (\t -> t.id == bottomId) model.teams
                                    in
                                    case ( team1Maybe, team2Maybe ) of
                                        ( Just team1, Just team2 ) ->
                                            { game | name = Just (team1.name ++ " vs " ++ team2.name) }

                                        _ ->
                                            { game | name = Nothing }

                                _ ->
                                    { game | name = Nothing }

                        _ ->
                            game
            in
            ( { model
                | overlay = Nothing
                , games = updatedGames model.games updatedGame
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
        fromCoords =
            DragDrop.getDropId model.dragDrop

        toCoords =
            DragDrop.getDroppablePosition model.dragDrop

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
            , viewGroups model fromCoords toCoords
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
                ]
            , h3 [] [ text "Groups" ]
            , ul []
                [ li [] [ text "Click on a group name to temporarily hide it so you can better see other groups." ]
                , li [] [ text "Click the ✎ icon next to a group name to edit its name and number of rows (height)." ]
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
            , div
                [ class "form-group" ]
                [ label [ for "editing-group-rows" ] [ text ("Group Rows: " ++ String.fromInt group.rows) ]
                , input
                    [ class "form-control"
                    , id "editing-group-rows"
                    , type_ "range"
                    , Html.Attributes.min (String.fromInt (minRowsForGroup group model.games))
                    , Html.Attributes.max "60"
                    , value (String.fromInt group.rows)
                    , onInput (UpdateGroupRows group)
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
        teamOption : Maybe GamePosition -> Team -> Html Msg
        teamOption selectedGamePosition team =
            let
                isSelected =
                    case selectedGamePosition of
                        Just (TeamAssignment id) ->
                            id == team.id

                        _ ->
                            False
            in
            option [ value ("team_" ++ String.fromInt team.id), selected isSelected ] [ text team.name ]

        teamOptions : Maybe GamePosition -> List (Html Msg)
        teamOptions selectedGamePosition =
            unassignedTeams model.teams model.games (Just game)
                |> List.map (teamOption selectedGamePosition)
                |> (::) (option [] [])

        gameOption : String -> Maybe Int -> Game -> Html Msg
        gameOption fromType selectedId forGame =
            let
                isSelected : Bool
                isSelected =
                    case selectedId of
                        Just id ->
                            id == forGame.id

                        _ ->
                            False
            in
            option [ value (fromType ++ "_" ++ String.fromInt forGame.id), selected isSelected ] [ text (String.Extra.toTitleCase fromType ++ " of " ++ Maybe.withDefault "TDB" forGame.name) ]

        winnerGameOptions : Maybe GamePosition -> List (Html Msg)
        winnerGameOptions selectedGamePosition =
            let
                selectedId =
                    case selectedGamePosition of
                        Just (GameAssignment Winner id) ->
                            Just id

                        _ ->
                            Nothing
            in
            model.games
                |> unassignedGames Winner (Just game.id) selectedId
                |> List.map (gameOption "winner" selectedId)

        loserGameOptions : Maybe GamePosition -> List (Html Msg)
        loserGameOptions selectedGamePosition =
            let
                selectedId =
                    case selectedGamePosition of
                        Just (GameAssignment Loser id) ->
                            Just id

                        _ ->
                            Nothing
            in
            model.games
                |> unassignedGames Loser (Just game.id) selectedId
                |> List.map (gameOption "loser" selectedId)
    in
    div [ class "modal-content" ]
        [ div [ class "modal-header" ]
            [ h5 [ class "modal-title" ] [ text "Edit Game" ] ]
        , div [ class "modal-body" ]
            [ div
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
            , div
                [ class "form-group" ]
                [ label [ for "editing-game-top" ] [ text "Top Team" ]
                , select
                    [ class "form-control"
                    , id "editing-game-top"
                    , onInput (UpdateGamePosition game "top")
                    ]
                    (teamOptions game.top ++ winnerGameOptions game.top ++ loserGameOptions game.top)
                ]
            , div
                [ class "form-group" ]
                [ label [ for "editing-game-bottom" ] [ text "Bottom Team" ]
                , select
                    [ class "form-control"
                    , id "editing-game-bottom"
                    , onInput (UpdateGamePosition game "bottom")
                    ]
                    (teamOptions game.bottom ++ winnerGameOptions game.bottom ++ loserGameOptions game.bottom)
                ]
            ]
        , div [ class "modal-footer" ]
            [ button [ onClick (CloseEditGame game), class "btn btn-primary mr-2" ] [ text "Close" ]
            ]
        ]


viewGroups : Model -> Maybe Coords -> Maybe DragDrop.Position -> Html Msg
viewGroups model fromCoords toCoords =
    div []
        (List.map (viewGroup model fromCoords toCoords) model.groups)


viewGroup : Model -> Maybe Coords -> Maybe DragDrop.Position -> Group -> Html Msg
viewGroup model fromCoords toCoords group =
    div
        [ class "group" ]
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
        , if group.visible then
            table
                []
                (List.map (viewRow model fromCoords toCoords group) (List.range 0 (group.rows - 1)))

          else
            div [ class "text-muted group-hide" ] [ text "..." ]
        ]


viewRow : Model -> Maybe Coords -> Maybe DragDrop.Position -> Group -> Int -> Html Msg
viewRow model fromCoords toCoords group row =
    let
        cellStart =
            row * model.cols
    in
    tr
        []
        (List.map (viewCell model fromCoords toCoords group row) (List.range 0 (model.cols - 1)))


viewCell : Model -> Maybe Coords -> Maybe DragDrop.Position -> Group -> Int -> Int -> Html Msg
viewCell model fromCoords toCoords group row col =
    let
        onCoords =
            Coords group.position row col

        onGame =
            findGameByCoords model.games onCoords

        highlight =
            if fromCoords |> Maybe.map ((==) onCoords) |> Maybe.withDefault False then
                case toCoords of
                    Nothing ->
                        []

                    Just pos ->
                        [ class "drop-target" ]

            else
                []

        positionText position =
            case position of
                Just (TeamAssignment id) ->
                    case List.Extra.find (\t -> t.id == id) model.teams of
                        Just team ->
                            team.name

                        Nothing ->
                            "TBD"

                Just (GameAssignment Winner id) ->
                    case List.Extra.find (\g -> g.id == id) model.games of
                        Just game ->
                            "W: " ++ Maybe.withDefault "TDB" game.name

                        Nothing ->
                            "TBD"

                Just (GameAssignment Loser id) ->
                    case List.Extra.find (\g -> g.id == id) model.games of
                        Just game ->
                            "L: " ++ Maybe.withDefault "TDB" game.name

                        Nothing ->
                            "TBD"

                Nothing ->
                    "TBD"
    in
    td
        (highlight
            ++ (if onGame == Nothing then
                    DragDrop.droppable DragDropMsg onCoords

                else
                    []
               )
            ++ [ onDoubleClick (AddGame onCoords) ]
        )
        (case onGame of
            Just game ->
                [ div
                    ([ class "game", onDoubleClick (EditGame game) ] ++ DragDrop.draggable DragDropMsg onCoords)
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
                    , div
                        [ class "game-top" ]
                        [ text (positionText game.top) ]
                    , div
                        [ class "game-bottom" ]
                        [ text (positionText game.bottom) ]
                    ]
                ]

            Nothing ->
                []
        )



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
