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
    , rows : Int
    , teams : List Team
    , games : List Game
    , editingGame : Maybe Game
    , editingGroup : Maybe Group
    }


type alias Coords =
    { group : Int
    , row : Int
    , col : Int
    }


type alias Group =
    { position : Int
    , name : String
    }


type alias Game =
    { id : Maybe Int
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
    = TeamAssignment (Maybe Team)
    | WinnerFrom Game
    | LoserFrom Game


type GameWinner
    = Top
    | Bottom


teams : List Team
teams =
    [ Team 1 "Team A", Team 2 "Team B" ]


init : ( Model, Cmd Msg )
init =
    ( { dragDrop = DragDrop.init
      , showGrid = True
      , groups = [ Group 0 "A Event", Group 1 "B Event" ]
      , cols = 14
      , rows = 16
      , teams = teams
      , games =
            [ Game Nothing "A1" (Just (TeamAssignment (List.Extra.getAt 0 teams))) (Just (TeamAssignment (List.Extra.getAt 1 teams))) (Just Top) (Coords 0 0 0)
            , Game Nothing "C vs D" Nothing Nothing Nothing (Coords 0 2 0)
            , Game Nothing "E vs F" Nothing Nothing Nothing (Coords 0 4 0)
            , Game Nothing "G vs H" Nothing Nothing Nothing (Coords 0 6 0)
            , Game Nothing "I vs J" Nothing Nothing Nothing (Coords 0 8 0)
            , Game Nothing "K vs L" Nothing Nothing Nothing (Coords 0 10 0)
            , Game Nothing "M vs N" Nothing Nothing Nothing (Coords 0 12 0)
            , Game Nothing "O vs P" Nothing Nothing Nothing (Coords 0 14 0)

            -- Group A Round 2
            , Game Nothing "TBD" Nothing Nothing Nothing (Coords 0 1 2)
            , Game Nothing "TBD" Nothing Nothing Nothing (Coords 0 5 2)
            , Game Nothing "TBD" Nothing Nothing Nothing (Coords 0 9 2)
            , Game Nothing "TBD" Nothing Nothing Nothing (Coords 0 13 2)

            -- Group A Semifinal
            , Game Nothing "Semifinal" Nothing Nothing Nothing (Coords 0 3 4)
            , Game Nothing "Semifinal" Nothing Nothing Nothing (Coords 0 11 4)

            -- Group A Final
            , Game Nothing "Final" Nothing Nothing Nothing (Coords 0 7 6)

            -- Group B
            , Game Nothing "A vs B" Nothing Nothing Nothing (Coords 1 0 0)
            , Game Nothing "C vs D" Nothing Nothing Nothing (Coords 1 2 0)
            , Game Nothing "E vs F" Nothing Nothing Nothing (Coords 1 4 0)
            , Game Nothing "G vs H" Nothing Nothing Nothing (Coords 1 6 0)

            -- Group B Round 2
            , Game Nothing "TBD" Nothing Nothing Nothing (Coords 1 1 2)
            , Game Nothing "TBD" Nothing Nothing Nothing (Coords 1 5 2)
            , Game Nothing "TBD" Nothing Nothing Nothing (Coords 1 1 10)
            , Game Nothing "TBD" Nothing Nothing Nothing (Coords 1 5 10)
            , Game Nothing "TBD" Nothing Nothing Nothing (Coords 1 0 12)
            , Game Nothing "TBD" Nothing Nothing Nothing (Coords 1 2 12)
            , Game Nothing "TBD" Nothing Nothing Nothing (Coords 1 4 12)
            , Game Nothing "TBD" Nothing Nothing Nothing (Coords 1 6 12)

            -- Group B Semifinal
            , Game Nothing "Semifinal" Nothing Nothing Nothing (Coords 1 3 4)
            , Game Nothing "Semifinal" Nothing Nothing Nothing (Coords 1 3 8)

            -- Group B Final
            , Game Nothing "Final" Nothing Nothing Nothing (Coords 1 3 6)
            ]
      , editingGame = Nothing
      , editingGroup = Nothing
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


addGame : List Game -> Coords -> List Game
addGame games coords =
    -- Don't add games on top of each other. Only add a game if there's room.
    case findGameByCoords games coords of
        Nothing ->
            games ++ [ Game Nothing "New" Nothing Nothing Nothing coords ]

        _ ->
            games



---- UPDATE ----


type Msg
    = DragDropMsg (DragDrop.Msg Coords Coords)
    | ToggleGrid
    | AddRow
    | RemoveRow
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

        AddRow ->
            ( { model | rows = model.rows + 1 }
            , Cmd.none
            )

        RemoveRow ->
            ( { model | rows = model.rows - 1 }
            , Cmd.none
            )

        AddCol ->
            ( { model | rows = model.cols + 1 }
            , Cmd.none
            )

        RemoveCol ->
            ( { model | rows = model.cols - 1 }
            , Cmd.none
            )

        AddGroup ->
            let
                nextGroupId =
                    List.length model.groups
            in
            ( { model
                | groups = model.groups ++ [ Group nextGroupId ("Group " ++ String.fromInt (nextGroupId + 1)) ]
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
                | games = addGame model.games coords
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
    div [ class "p-3" ]
        [ case model.editingGame of
            Just game ->
                viewEditGame model game

            Nothing ->
                case model.editingGroup of
                    Just group ->
                        viewEditGroup model group

                    Nothing ->
                        viewBracket model
        ]


viewEditGroup : Model -> Group -> Html Msg
viewEditGroup model group =
    div [ class "d-flex justify-content-center" ]
        [ div [ class "edit-group" ]
            [ div [ class "border p-3" ]
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
                , div
                    [ class "d-flex justify-content-end" ]
                    [ button [ onClick CancelEditGroup, class "btn btn-secondary mr-2" ] [ text "Cancel" ]
                    , button [ onClick (RemoveGroup group), class "btn btn-danger mr-2" ] [ text "Remove" ]
                    , button [ onClick (SaveGroup group), class "btn btn-primary" ] [ text "Update" ]
                    ]
                ]
            ]
        ]


viewEditGame : Model -> Game -> Html Msg
viewEditGame model game =
    div [ class "d-flex justify-content-center" ]
        [ div [ class "edit-game" ]
            [ div [ class "border p-3" ]
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
                    [ label [ for "editing-top-position" ] [ text "Top Position" ] ]
                , div
                    [ class "d-flex justify-content-end" ]
                    [ button [ onClick CancelEditGame, class "btn btn-secondary mr-2" ] [ text "Cancel" ]
                    , button [ onClick (RemoveGame game), class "btn btn-danger mr-2" ] [ text "Remove" ]
                    , button [ onClick (SaveGame game), class "btn btn-primary" ] [ text "Update" ]
                    ]
                ]
            ]
        ]


viewBracket : Model -> Html Msg
viewBracket model =
    let
        fromCoords =
            DragDrop.getDropId model.dragDrop

        toCoords =
            DragDrop.getDroppablePosition model.dragDrop
    in
    div []
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
            (List.map (viewRow model fromCoords toCoords group) (List.range 0 (model.rows - 1)))
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
                    case team of
                        Just team_ ->
                            team_.name

                        Nothing ->
                            "TBD"

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
