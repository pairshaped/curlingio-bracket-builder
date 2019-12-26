port module Main exposing (..)

import Browser
import Debug exposing (log)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onDoubleClick)
import Html5.DragDrop as DragDrop
import Json.Decode exposing (Value)
import List.Extra



---- PORTS ----


port dragstart : Value -> Cmd msg



---- MODEL ----


type alias Model =
    { dragDrop : DragDrop.Model Coords Coords
    , groups : List Group
    , cols : Int
    , rows : Int
    , games : List Game
    , editingGame : Maybe Game
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
    , coords : Coords
    }


init : ( Model, Cmd Msg )
init =
    ( { dragDrop = DragDrop.init
      , groups = [ Group 0 "Group A", Group 1 "Group B" ]
      , cols = 14
      , rows = 16
      , games =
            [ Game Nothing "A vs B" (Coords 0 0 0)
            , Game Nothing "C vs D" (Coords 0 2 0)
            , Game Nothing "E vs F" (Coords 0 4 0)
            , Game Nothing "G vs H" (Coords 0 6 0)
            , Game Nothing "I vs J" (Coords 0 8 0)
            , Game Nothing "K vs L" (Coords 0 10 0)
            , Game Nothing "M vs N" (Coords 0 12 0)
            , Game Nothing "O vs P" (Coords 0 14 0)

            -- Group A Round 2
            , Game Nothing "TBD" (Coords 0 1 2)
            , Game Nothing "TBD" (Coords 0 5 2)
            , Game Nothing "TBD" (Coords 0 9 2)
            , Game Nothing "TBD" (Coords 0 13 2)

            -- Group A Semifinal
            , Game Nothing "Semifinal" (Coords 0 3 4)
            , Game Nothing "Semifinal" (Coords 0 11 4)

            -- Group A Final
            , Game Nothing "Final" (Coords 0 7 6)

            -- Group B
            , Game Nothing "A vs B" (Coords 1 0 0)
            , Game Nothing "C vs D" (Coords 1 2 0)
            , Game Nothing "E vs F" (Coords 1 4 0)
            , Game Nothing "G vs H" (Coords 1 6 0)

            -- Group B Round 2
            , Game Nothing "TBD" (Coords 1 1 2)
            , Game Nothing "TBD" (Coords 1 5 2)
            , Game Nothing "TBD" (Coords 1 1 10)
            , Game Nothing "TBD" (Coords 1 5 10)
            , Game Nothing "TBD" (Coords 1 0 12)
            , Game Nothing "TBD" (Coords 1 2 12)
            , Game Nothing "TBD" (Coords 1 4 12)
            , Game Nothing "TBD" (Coords 1 6 12)

            -- Group B Semifinal
            , Game Nothing "Semifinal" (Coords 1 3 4)
            , Game Nothing "Semifinal" (Coords 1 3 8)

            -- Group B Final
            , Game Nothing "Final" (Coords 1 3 6)
            ]
      , editingGame = Nothing
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


addGame : List Game -> List Game
addGame games =
    let
        coords =
            Coords 0 0 6
    in
    -- Don't add games on top of each other. Only add a game if there's room.
    case findGameByCoords games coords of
        Nothing ->
            games ++ [ Game Nothing "New Game" coords ]

        _ ->
            games



---- UPDATE ----


type Msg
    = DragDropMsg (DragDrop.Msg Coords Coords)
    | AddRow
    | RemoveRow
    | AddCol
    | RemoveCol
    | AddGroup
    | RemoveGroup Group
    | AddGame
    | RemoveGame Game
    | EditGame Game
    | UpdateGame Game
    | CancelEdit
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
                            let
                                _ =
                                    log "result" result

                                --{ from = fromCoords, to = toCoords }
                            in
                            moveGame model.games fromCoords toCoords
              }
            , DragDrop.getDragstartEvent msg_
                |> Maybe.map (.event >> dragstart)
                |> Maybe.withDefault Cmd.none
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
            ( { model
                | groups = model.groups ++ [ Group 2 "Group C" ]
              }
            , Cmd.none
            )

        RemoveGroup group ->
            ( { model | groups = List.Extra.remove group model.groups }
            , Cmd.none
            )

        AddGame ->
            ( { model
                | games = addGame model.games
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
            let
                _ =
                    log "Edit" game
            in
            ( { model | editingGame = Just game }, Cmd.none )

        UpdateGame game ->
            let
                _ =
                    log "Update" game
            in
            ( { model
                | editingGame = Nothing
                , games = updateGame model.games game
              }
            , Cmd.none
            )

        CancelEdit ->
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
                viewBracket model
        ]


viewEditGame : Model -> Game -> Html Msg
viewEditGame model game =
    div [ class "row justify-content-center" ]
        [ div [ class "col-xs-12 col-md-8 col-lg-6 col-xl-5" ]
            [ div [ class "border p-3" ]
                [ h5 [] [ text game.name ]
                , div
                    []
                    [ p [] [ text "Body" ] ]
                , div
                    [ class "d-flex justify-content-end" ]
                    [ button [ onClick CancelEdit, class "btn btn-secondary mr-2" ] [ text "Cancel" ]
                    , button [ onClick (UpdateGame game), class "btn btn-primary" ] [ text "Update" ]
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
        [ div [ class "text-right" ]
            [ button [ class "btn btn-primary btn-sm mr-1", onClick AddGroup ] [ text "Add Group" ]
            , button [ class "btn btn-success btn-sm", onClick AddGame ] [ text "Add Game" ]
            ]
        , viewGroups model fromCoords toCoords
        ]


viewGroups : Model -> Maybe Coords -> Maybe DragDrop.Position -> Html Msg
viewGroups model fromCoords toCoords =
    div []
        (List.map (viewGroup model fromCoords toCoords) model.groups)


viewGroup : Model -> Maybe Coords -> Maybe DragDrop.Position -> Group -> Html Msg
viewGroup model fromCoords toCoords group =
    div
        [ class "ml-3 mr-3 mb-5"
        ]
        [ h3
            []
            [ text group.name ]
        , table
            []
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
    in
    td
        (highlight
            ++ (if onGame == Nothing then
                    DragDrop.droppable DragDropMsg onCoords

                else
                    []
               )
        )
        (case onGame of
            Just game ->
                [ div
                    ([ onDoubleClick (EditGame game) ] ++ DragDrop.draggable DragDropMsg onCoords)
                    [ div [ class "actions" ]
                        [ div [ class "delete", onClick (RemoveGame game) ]
                            [ text "X" ]
                        ]
                    , text game.name
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
