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



---- PORTS ----


port dragstart : Value -> Cmd msg



---- MODEL ----


type alias Model =
    { dragDrop : DragDrop.Model Coords Coords
    , showGrid : Bool
    , groups : List Group
    , cols : Int
    , teams : List Team
    , games : List Game
    , editingGame : Maybe Game
    , editingGroup : Maybe Group
    , newGameCount : Int
    }


type alias Coords =
    { group : Int
    , row : Int
    , col : Int
    }


type alias Group =
    { position : Int
    , name : String
    , rows : Int
    }


type alias Game =
    { id : Maybe Int
    , tempId : Maybe Int
    , name : String
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
    = TeamAssignment Team
    | WinnerFrom Game
    | LoserFrom Game


type GameWinner
    = Top
    | Bottom


initTeams : List Team
initTeams =
    [ Team 1 "Team 1"
    , Team 2 "Team 2"
    , Team 3 "Team 3"
    , Team 4 "Team 4"
    , Team 5 "Team 5"
    , Team 6 "Team 6"
    , Team 7 "Team 7"
    , Team 8 "Team 8"
    , Team 9 "Team 9"
    , Team 10 "Team 10"
    , Team 11 "Team 11"
    , Team 12 "Team 12"
    , Team 13 "Team 13"
    , Team 14 "Team 14"
    , Team 15 "Team 15"
    , Team 16 "Team 16"
    ]


init : ( Model, Cmd Msg )
init =
    ( { dragDrop = DragDrop.init
      , showGrid = True
      , groups = [ Group 0 "A Event" 16, Group 1 "B Event" 16 ]
      , cols = 14
      , teams = initTeams
      , games =
            [ Game (Just 1) Nothing "1 vs 2" (Just (TeamAssignment (Team 1 "Team 1"))) (Just (TeamAssignment (Team 2 "Team 2"))) Nothing (Coords 0 0 0)
            , Game (Just 2) Nothing "3 vs 4" (Just (TeamAssignment (Team 3 "Team 3"))) (Just (TeamAssignment (Team 4 "Team 4"))) Nothing (Coords 0 2 0)
            , Game (Just 3) Nothing "5 vs 6" Nothing Nothing Nothing (Coords 0 4 0)
            , Game (Just 4) Nothing "7 vs 8" Nothing Nothing Nothing (Coords 0 6 0)
            , Game (Just 5) Nothing "9 vs 10" Nothing Nothing Nothing (Coords 0 8 0)
            , Game (Just 6) Nothing "11 vs 12" Nothing Nothing Nothing (Coords 0 10 0)
            , Game (Just 7) Nothing "13 vs 14" Nothing Nothing Nothing (Coords 0 12 0)
            , Game (Just 8) Nothing "15 vs 16" Nothing Nothing Nothing (Coords 0 14 0)

            -- Group A Round 2
            , Game (Just 9) Nothing "Quarterfinal 1" Nothing Nothing Nothing (Coords 0 1 2)
            , Game (Just 10) Nothing "Quarterfinal 2" Nothing Nothing Nothing (Coords 0 5 2)
            , Game (Just 11) Nothing "Quarterfinal 3" Nothing Nothing Nothing (Coords 0 9 2)
            , Game (Just 12) Nothing "Quarterfinal 4" Nothing Nothing Nothing (Coords 0 13 2)

            -- Group A Semifinal
            , Game (Just 13) Nothing "Semifinal 1" Nothing Nothing Nothing (Coords 0 3 4)
            , Game (Just 14) Nothing "Semifinal 2" Nothing Nothing Nothing (Coords 0 11 4)

            -- Group A Final
            , Game (Just 15) Nothing "Final" Nothing Nothing Nothing (Coords 0 7 6)

            -- Group B
            , Game (Just 16) Nothing "TBD" Nothing Nothing Nothing (Coords 1 0 0)
            , Game (Just 17) Nothing "TBD" Nothing Nothing Nothing (Coords 1 2 0)
            , Game (Just 18) Nothing "TBD" Nothing Nothing Nothing (Coords 1 4 0)
            , Game (Just 19) Nothing "TBD" Nothing Nothing Nothing (Coords 1 6 0)

            -- Group B Round 2
            , Game (Just 20) Nothing "TBD" Nothing Nothing Nothing (Coords 1 1 2)
            , Game (Just 21) Nothing "TBD" Nothing Nothing Nothing (Coords 1 5 2)
            , Game (Just 22) Nothing "TBD" Nothing Nothing Nothing (Coords 1 1 10)
            , Game (Just 23) Nothing "TBD" Nothing Nothing Nothing (Coords 1 5 10)
            , Game (Just 24) Nothing "TBD" Nothing Nothing Nothing (Coords 1 0 12)
            , Game (Just 25) Nothing "TBD" Nothing Nothing Nothing (Coords 1 2 12)
            , Game (Just 26) Nothing "TBD" Nothing Nothing Nothing (Coords 1 4 12)
            , Game (Just 27) Nothing "TBD" Nothing Nothing Nothing (Coords 1 6 12)

            -- Group B Semifinal
            , Game (Just 28) Nothing "Semifinal" Nothing Nothing Nothing (Coords 1 3 4)
            , Game (Just 29) Nothing "Semifinal" Nothing Nothing Nothing (Coords 1 3 8)

            -- Group B Final
            , Game (Just 30) Nothing "Final" Nothing Nothing Nothing (Coords 1 3 6)
            ]
      , editingGame = Nothing
      , editingGroup = Nothing
      , newGameCount = 0
      }
    , Cmd.none
    )



--- HELPERS ---


findGameByCoords : List Game -> Coords -> Maybe Game
findGameByCoords games coords =
    List.Extra.find (\game -> game.coords == coords) games


moveGame : List Game -> Coords -> Coords -> List Game
moveGame games fromCoords toCoords =
    List.Extra.updateIf (\game -> game.coords == fromCoords) (\game -> { game | coords = toCoords }) games


updateGame : List Game -> Game -> List Game
updateGame games game =
    List.Extra.updateIf (\g -> g.coords == game.coords) (\g -> game) games


updateGroup : List Group -> Group -> List Group
updateGroup groups group =
    List.Extra.updateIf (\g -> g.position == group.position) (\g -> group) groups


addGame : Int -> List Game -> Coords -> List Game
addGame tempId games coords =
    -- Don't add games on top of each other. Only add a game if there's room.
    case findGameByCoords games coords of
        Nothing ->
            games ++ [ Game Nothing (Just tempId) "New" Nothing Nothing Nothing coords ]

        _ ->
            games


unassignedTeams : List Team -> List Game -> Maybe Game -> List Team
unassignedTeams teams games excludeGame =
    let
        gamesNotExcluded =
            case excludeGame of
                Just game ->
                    case game.id of
                        Just _ ->
                            games
                                |> List.filter (\g -> g.id /= game.id)

                        Nothing ->
                            case game.tempId of
                                Just _ ->
                                    games
                                        |> List.filter (\g -> g.tempId /= game.tempId)

                                Nothing ->
                                    games

                Nothing ->
                    games

        assignedTo team g =
            let
                assignedTop =
                    case g.top of
                        Just (TeamAssignment t) ->
                            t.id == team.id

                        _ ->
                            False

                assignedBottom =
                    case g.bottom of
                        Just (TeamAssignment t) ->
                            t.id == team.id

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



---- UPDATE ----


type Msg
    = DragDropMsg (DragDrop.Msg Coords Coords)
    | ToggleGrid
    | AddRow Group
    | RemoveRow Group
    | AddCol
    | RemoveCol
    | AddGroup
    | EditGroup Group
    | UpdateGroupName String
    | CancelEditGroup
    | SaveGroup Group
    | RemoveGroup Group
    | AddGame Coords
    | RemoveGame Game
    | EditGame Game
    | SaveGame Game
    | UpdateGameName String
    | CancelEditGame
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
                            -- let
                            --     _ =
                            --         log "result" result
                            -- in
                            moveGame model.games fromCoords toCoords
              }
            , DragDrop.getDragstartEvent msg_
                |> Maybe.map (.event >> dragstart)
                |> Maybe.withDefault Cmd.none
            )

        ToggleGrid ->
            ( { model | showGrid = not model.showGrid }
            , Cmd.none
            )

        AddRow group ->
            let
                updatedGroup g =
                    if group.position == g.position then
                        { g | rows = g.rows + 1 }

                    else
                        g
            in
            ( { model | groups = List.map updatedGroup model.groups }
            , Cmd.none
            )

        RemoveRow group ->
            let
                updatedGroup g =
                    if group.position == g.position then
                        { g | rows = g.rows - 1 }

                    else
                        g
            in
            ( { model | groups = List.map updatedGroup model.groups }
            , Cmd.none
            )

        AddCol ->
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
                | groups = model.groups ++ [ Group nextGroupId ("Group " ++ String.fromInt (nextGroupId + 1)) 16 ]
              }
            , Cmd.none
            )

        EditGroup group ->
            ( { model | editingGroup = Just group }
            , Cmd.none
            )

        UpdateGroupName name ->
            let
                updatedGroup =
                    case model.editingGroup of
                        Just group ->
                            Just { group | name = name }

                        Nothing ->
                            model.editingGroup
            in
            ( { model
                | editingGroup = updatedGroup
              }
            , Cmd.none
            )

        CancelEditGroup ->
            ( { model | editingGroup = Nothing }
            , Cmd.none
            )

        SaveGroup group ->
            ( { model
                | editingGroup = Nothing
                , groups = updateGroup model.groups group
              }
            , Cmd.none
            )

        RemoveGroup group ->
            ( { model
                | editingGroup = Nothing
                , groups = List.Extra.remove group model.groups
              }
            , Cmd.none
            )

        AddGame coords ->
            ( { model
                | games = addGame model.newGameCount model.games coords
                , newGameCount = model.newGameCount + 1
              }
            , Cmd.none
            )

        RemoveGame game ->
            ( { model
                | editingGame = Nothing
                , games = List.Extra.remove game model.games
              }
            , Cmd.none
            )

        EditGame game ->
            ( { model | editingGame = Just game }, Cmd.none )

        UpdateGameName name ->
            let
                updatedGame =
                    case model.editingGame of
                        Just game ->
                            Just { game | name = name }

                        Nothing ->
                            model.editingGame
            in
            ( { model
                | editingGame = updatedGame
              }
            , Cmd.none
            )

        SaveGame game ->
            ( { model
                | editingGame = Nothing
                , games = updateGame model.games game
              }
            , Cmd.none
            )

        CancelEditGame ->
            ( { model | editingGame = Nothing }
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
            model.editingGroup /= Nothing || model.editingGame /= Nothing
    in
    div [ classList [ ( "modal-open", modalOpen ) ] ]
        [ div [ class "p-3" ]
            [ div [ class "d-flex justify-content-between" ]
                [ p [ class "alert alert-info" ] [ text "Drag and drop games anywhere you like. Double click anywhere to add a new game. Double click a game to change or remove it. Double click a group name to change or remove it." ]
                , div [ style "min-width" "100px", class "text-right" ]
                    [ button [ class "btn btn-info btn-sm", onClick ToggleGrid ]
                        [ text
                            (if model.showGrid then
                                "Hide Grid"

                             else
                                "Show Grid"
                            )
                        ]
                    ]
                ]
            , viewGroups model fromCoords toCoords
            , button [ class "btn btn-primary", onClick AddGroup ] [ text "Add Group" ]
            , if modalOpen then
                viewModal model

              else
                text ""
            ]
        , div [ classList [ ( "modal-backdrop", modalOpen ), ( "show", modalOpen ) ] ] []
        ]


viewModal : Model -> Html Msg
viewModal model =
    div [ class "modal", style "display" "block" ]
        [ div [ class "modal-dialog" ]
            [ case model.editingGame of
                Just game ->
                    viewEditGame model game

                Nothing ->
                    case model.editingGroup of
                        Just group ->
                            viewEditGroup model group

                        Nothing ->
                            text ""
            ]
        ]


viewEditGroup : Model -> Group -> Html Msg
viewEditGroup model group =
    div [ class "modal-content" ]
        [ div [ class "modal-header" ]
            [ h5 [ class "modal-title" ] [ text "Edit Game" ] ]
        , div [ class "modal-body" ]
            [ div
                [ class "form-group" ]
                [ label [ for "editing-group-name" ] [ text "Group Name" ]
                , input
                    [ class "form-control"
                    , id "editing-group-name"
                    , value group.name
                    , onInput UpdateGroupName
                    ]
                    []
                ]
            ]
        , div [ class "modal-footer" ]
            [ button [ onClick CancelEditGroup, class "btn btn-secondary mr-2" ] [ text "Cancel" ]
            , button [ onClick (RemoveGroup group), class "btn btn-danger mr-2" ] [ text "Remove" ]
            , button [ onClick (SaveGroup group), class "btn btn-primary" ] [ text "Update" ]
            ]
        ]


viewEditGame : Model -> Game -> Html Msg
viewEditGame model game =
    let
        teamOptions =
            unassignedTeams model.teams model.games (Just game)
                |> List.map (\t -> option [ value (String.fromInt t.id) ] [ text t.name ])
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
                    , value game.name
                    , onInput UpdateGameName
                    ]
                    []
                ]
            , div
                [ class "form-group" ]
                [ label [ for "editing-game-top" ] [ text "Team" ]
                , select
                    [ class "form-control"
                    , id "editing-game-top"
                    ]
                    teamOptions
                ]
            ]
        , div [ class "modal-footer" ]
            [ button [ onClick CancelEditGame, class "btn btn-secondary mr-2" ] [ text "Cancel" ]
            , button [ onClick (RemoveGame game), class "btn btn-danger mr-2" ] [ text "Remove" ]
            , button [ onClick (SaveGame game), class "btn btn-primary" ] [ text "Update" ]
            ]
        ]


viewGroups : Model -> Maybe Coords -> Maybe DragDrop.Position -> Html Msg
viewGroups model fromCoords toCoords =
    div []
        (List.map (viewGroup model fromCoords toCoords) model.groups)


viewGroup : Model -> Maybe Coords -> Maybe DragDrop.Position -> Group -> Html Msg
viewGroup model fromCoords toCoords group =
    let
        grid =
            if model.showGrid then
                [ class "show-grid" ]

            else
                []
    in
    div
        [ class "group" ]
        [ div
            [ class "group-name btn btn-default", onDoubleClick (EditGroup group) ]
            [ text ("☷ " ++ group.name) ]
        , table
            grid
            (List.map (viewRow model fromCoords toCoords group) (List.range 0 (group.rows - 1)))
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
                Just (TeamAssignment team) ->
                    team.name

                Just (WinnerFrom game) ->
                    "Winner from"

                Just (LoserFrom game) ->
                    "Loser from"

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
                        [ class "game-name" ]
                        [ text game.name ]
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
