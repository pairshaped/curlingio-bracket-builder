port module Main exposing (..)

-- import Debug exposing (log)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onDoubleClick, onInput)
import Html5.DragDrop as DragDrop
import Http exposing (expectJson)
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra
import RemoteData exposing (RemoteData(..), WebData)
import Set
import String.Extra
import Svg exposing (line, polyline, svg)
import Svg.Attributes exposing (fill, points, stroke, strokeDasharray, strokeOpacity, x1, x2, y1, y2)



---- PORTS ----


port dragstart : Decode.Value -> Cmd msg


port sendBracketToLocalStorage : Encode.Value -> Cmd msg


port requestBracketFromLocalStorage : String -> Cmd msg


port receiveBracketFromLocalStorage : (Decode.Value -> msg) -> Sub msg



---- MODEL ----


gridSize =
    50


type alias Model =
    { demoMode : Bool
    , url : String
    , dragDrop : DragDrop.Model DraggableId DroppableId
    , overlay : Maybe Overlay
    , changed : Bool
    , bracket : WebData Bracket
    }


type Overlay
    = EditingGame Game
    | EditingGroup Group
    | RevertConfirmation
    | ClearConfirmation


type DraggableId
    = DraggableGame Int
    | DraggableResult ( Int, GameResult )


type DroppableId
    = DroppableCell Coords
    | DroppableGamePosition ( Int, Int )


type alias Bracket =
    { id : Int
    , teams : List Team
    , groups : List Group
    , games : List Game
    }


type alias Group =
    { position : Int
    , name : String
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
    , coords : Coords
    , gamePositions : List GamePosition
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


type GameResult
    = Winner
    | Loser


type Assignment
    = TeamAssignment Int
    | GameAssignment GameResult Int


type alias LineConnector =
    { result : GameResult
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
    ]


emptyBracket : Bracket
emptyBracket =
    { id = 1
    , teams = initTeams
    , groups =
        [ Group 0 "Group 1" True ]
    , games = []
    }


demoBracket : Bracket
demoBracket =
    { id = 1
    , teams = initTeams
    , groups =
        [ Group 0 "A Event" True, Group 1 "B Event" True ]
    , games =
        [ Game 1
            (Just "A1")
            (Coords 0 0 0)
            [ GamePosition 0 False (Just (TeamAssignment 1))
            , GamePosition 1 True (Just (TeamAssignment 2))
            ]
        , Game 2
            (Just "A2")
            (Coords 0 0 2)
            [ GamePosition 0 False (Just (TeamAssignment 3))
            , GamePosition 1 True (Just (TeamAssignment 4))
            ]
        , Game 3
            (Just "A3")
            (Coords 0 0 4)
            [ GamePosition 0 False (Just (TeamAssignment 5))
            , GamePosition 1 True (Just (TeamAssignment 6))
            ]
        , Game 4
            (Just "A4")
            (Coords 0 0 6)
            [ GamePosition 0 False (Just (TeamAssignment 7))
            , GamePosition 1 True (Just (TeamAssignment 8))
            ]

        -- Group A Round 2
        , Game 5
            (Just "A Semi-Final 1")
            (Coords 0 5 1)
            [ GamePosition 0 False (Just (GameAssignment Winner 1))
            , GamePosition 1 True (Just (GameAssignment Winner 2))
            ]
        , Game 6
            (Just "A Semi-Final 2")
            (Coords 0 5 5)
            [ GamePosition 0 False (Just (GameAssignment Winner 3))
            , GamePosition 1 True (Just (GameAssignment Winner 4))
            ]

        -- Group A Round 3
        , Game 7
            (Just "A Final")
            (Coords 0 10 3)
            [ GamePosition 0 False (Just (GameAssignment Winner 5))
            , GamePosition 1 True (Just (GameAssignment Winner 6))
            ]

        -- Group B Round 1
        , Game 8
            (Just "B1")
            (Coords 1 0 0)
            [ GamePosition 0 False (Just (GameAssignment Loser 1))
            , GamePosition 1 True (Just (GameAssignment Loser 2))
            ]
        , Game 9
            (Just "B2")
            (Coords 1 0 2)
            [ GamePosition 0 False (Just (GameAssignment Loser 3))
            , GamePosition 1 True (Just (GameAssignment Loser 4))
            ]

        -- Group B Round 2
        , Game 10
            (Just "B Final")
            (Coords 1 5 1)
            [ GamePosition 0 False (Just (GameAssignment Winner 8))
            , GamePosition 1 True (Just (GameAssignment Winner 9))
            ]
        ]
    }


init : { demoMode : Bool, url : String } -> ( Model, Cmd Msg )
init { demoMode, url } =
    ( { demoMode = demoMode
      , url = url
      , dragDrop = DragDrop.init
      , overlay = Nothing
      , changed = False
      , bracket = Loading
      }
    , loadBracket demoMode url
    )



--- HELPERS ---


{-| Find a game by it's coordinates
-}
findGameByCoords : Coords -> List Game -> Maybe Game
findGameByCoords coords games =
    List.Extra.find (\game -> game.coords == coords) games


connectGameResult : List Game -> ( Int, GameResult ) -> ( Int, Int ) -> List Game
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


{-| Find the minimum required cols based on the placement of games, making sure there are enough cols for all games to be shown.
-}
colsForGames : List Game -> Int
colsForGames games =
    games
        |> List.map (\g -> g.coords.col + 5)
        |> List.maximum
        |> Maybe.withDefault 10


{-| Find the minimum required rows for a group based on the placement of games within it, making sure there are enough rows for all of a groups games to be shown.
-}
rowsForGroup : Group -> List Game -> Int
rowsForGroup group games =
    games
        |> List.filter (\g -> g.coords.group == group.position)
        |> List.map (\g -> g.coords.row + 3)
        |> List.maximum
        |> Maybe.withDefault 4


{-| Trim a string, if we have a string
-}
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


saveBracket : Bool -> String -> WebData Bracket -> Cmd Msg
saveBracket demoMode url bracketResult =
    case bracketResult of
        Success bracket ->
            bracketEncoder bracket
                |> (if demoMode then
                        sendBracketToLocalStorage

                    else
                        sendBracketToServer url (Just bracket.id)
                   )

        _ ->
            Cmd.none


sendBracketToServer : String -> Maybe Int -> Encode.Value -> Cmd Msg
sendBracketToServer url maybeId bracketJson =
    Http.request
        { method = "PATCH"
        , headers = []
        , url = url
        , body = Http.jsonBody bracketJson
        , expect = Http.expectJson Saved bracketDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


loadBracket : Bool -> String -> Cmd Msg
loadBracket demoMode url =
    if demoMode then
        requestBracketFromLocalStorage ""

    else
        requestBracketFromServer url


requestBracketFromServer : String -> Cmd Msg
requestBracketFromServer url =
    Http.get
        { url = url
        , expect = expectJson (RemoteData.fromResult >> ReceivedBracketFromServer) bracketDecoder
        }



---- ENCODERS ----


bracketEncoder : Bracket -> Encode.Value
bracketEncoder bracket =
    Encode.object
        [ ( "id", Encode.int bracket.id )
        , ( "teams", teamsEncoder bracket.teams )
        , ( "groups", groupsEncoder bracket.groups )
        , ( "games", gamesEncoder bracket.games )
        ]


teamsEncoder : List Team -> Encode.Value
teamsEncoder teams =
    let
        teamEncoder : Team -> Encode.Value
        teamEncoder team =
            Encode.object
                [ ( "id", Encode.int team.id )
                , ( "name", Encode.string team.name )
                ]
    in
    Encode.list teamEncoder teams


groupsEncoder : List Group -> Encode.Value
groupsEncoder groups =
    let
        groupEncoder : Group -> Encode.Value
        groupEncoder group =
            Encode.object
                [ ( "position", Encode.int group.position )
                , ( "name", Encode.string group.name )
                ]
    in
    Encode.list groupEncoder groups


gamesEncoder : List Game -> Encode.Value
gamesEncoder games =
    let
        gameEncoder : Game -> Encode.Value
        gameEncoder game =
            let
                gamePositionEncoder : GamePosition -> Encode.Value
                gamePositionEncoder gamePosition =
                    let
                        encodeAssignment : Maybe Assignment -> Encode.Value
                        encodeAssignment assignment =
                            case assignment of
                                Just (TeamAssignment id) ->
                                    Encode.object
                                        [ ( "assignment_type", Encode.string "team" )
                                        , ( "id", Encode.int id )
                                        ]

                                Just (GameAssignment result id) ->
                                    let
                                        resultEncoder : Encode.Value
                                        resultEncoder =
                                            Encode.string
                                                (case result of
                                                    Winner ->
                                                        "winner"

                                                    Loser ->
                                                        "loser"
                                                )
                                    in
                                    Encode.object
                                        [ ( "assignment_type", Encode.string "game" )
                                        , ( "result", resultEncoder )
                                        , ( "id", Encode.int id )
                                        ]

                                Nothing ->
                                    Encode.null
                    in
                    Encode.object
                        [ ( "position", Encode.int gamePosition.position )
                        , ( "first_hammer", Encode.bool gamePosition.firstHammer )
                        , ( "assignment", encodeAssignment gamePosition.assignment )
                        ]

                coordsEncoder : Coords -> Encode.Value
                coordsEncoder coords =
                    Encode.object
                        [ ( "group", Encode.int coords.group )
                        , ( "col", Encode.int coords.col )
                        , ( "row", Encode.int coords.row )
                        ]
            in
            Encode.object
                [ ( "id", Encode.int game.id )
                , ( "name"
                  , case game.name of
                        Just name ->
                            Encode.string name

                        Nothing ->
                            Encode.null
                  )
                , ( "game_positions", Encode.list gamePositionEncoder game.gamePositions )
                , ( "coords", coordsEncoder game.coords )
                ]
    in
    Encode.list gameEncoder games


buildErrorMessage : Http.Error -> String
buildErrorMessage httpError =
    case httpError of
        Http.BadUrl message ->
            message

        Http.Timeout ->
            "Server is taking too long to respond. Please try again later."

        Http.NetworkError ->
            "Unable to reach server."

        Http.BadStatus statusCode ->
            "Request failed with status code: " ++ String.fromInt statusCode

        Http.BadBody message ->
            message



---- DECODERS ----


bracketDecoder : Decode.Decoder Bracket
bracketDecoder =
    Decode.map4
        Bracket
        (Decode.field "id" Decode.int)
        (Decode.field "teams" (Decode.list teamDecoder))
        (Decode.field "groups" (Decode.list groupDecoder))
        (Decode.field "games" (Decode.list gameDecoder))


teamDecoder : Decode.Decoder Team
teamDecoder =
    Decode.map2
        Team
        (Decode.field "id" Decode.int)
        (Decode.field "name" Decode.string)


groupDecoder : Decode.Decoder Group
groupDecoder =
    Decode.map3
        Group
        (Decode.field "position" Decode.int)
        (Decode.field "name" Decode.string)
        (Decode.succeed True)


gameDecoder : Decode.Decoder Game
gameDecoder =
    let
        gamePositionDecoder : Decode.Decoder GamePosition
        gamePositionDecoder =
            let
                assignmentDecoder : Decode.Decoder Assignment
                assignmentDecoder =
                    let
                        assignmentFromType : String -> Decode.Decoder Assignment
                        assignmentFromType str =
                            case str of
                                "game" ->
                                    gameAssignmentDecoder

                                "team" ->
                                    teamAssignmentDecoder

                                _ ->
                                    Decode.fail ("Invalid assignment type: " ++ str)

                        teamAssignmentDecoder : Decode.Decoder Assignment
                        teamAssignmentDecoder =
                            Decode.map
                                TeamAssignment
                                (Decode.field "id" Decode.int)

                        gameAssignmentDecoder : Decode.Decoder Assignment
                        gameAssignmentDecoder =
                            let
                                resultDecoder : Decode.Decoder GameResult
                                resultDecoder =
                                    let
                                        resultFromString : String -> Decode.Decoder GameResult
                                        resultFromString str =
                                            case str of
                                                "winner" ->
                                                    Decode.succeed Winner

                                                "loser" ->
                                                    Decode.succeed Loser

                                                _ ->
                                                    Decode.fail ("Invalid result: " ++ str)
                                    in
                                    Decode.string |> Decode.andThen resultFromString
                            in
                            Decode.map2
                                GameAssignment
                                (Decode.field "result" resultDecoder)
                                (Decode.field "id" Decode.int)
                    in
                    Decode.field "assignment_type" Decode.string
                        |> Decode.andThen assignmentFromType
            in
            Decode.map3
                GamePosition
                (Decode.field "position" Decode.int)
                (Decode.field "first_hammer" Decode.bool)
                (Decode.maybe (Decode.field "assignment" assignmentDecoder))

        coordsDecoder : Decode.Decoder Coords
        coordsDecoder =
            Decode.map3
                Coords
                (Decode.field "group" Decode.int)
                (Decode.field "col" Decode.int)
                (Decode.field "row" Decode.int)
    in
    Decode.map4
        Game
        (Decode.field "id" Decode.int)
        (Decode.maybe (Decode.field "name" Decode.string))
        (Decode.field "coords" coordsDecoder)
        (Decode.field "game_positions" (Decode.list gamePositionDecoder))



---- UPDATE ----


type Msg
    = DragDropMsg (DragDrop.Msg DraggableId DroppableId)
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
    | Saved (Result Http.Error Bracket)
    | ConfirmRevert
    | Revert
    | ReceivedBracketFromServer (WebData Bracket)
    | ReceivedBracketFromLocalStorage Decode.Value
    | ConfirmClear
    | Clear
    | CancelConfirmation


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
                    let
                        updatedBracket bracket =
                            { bracket
                                | games =
                                    List.Extra.updateIf (\game -> game.id == gameId) (\game -> { game | coords = coords }) bracket.games
                            }
                    in
                    { model
                        | dragDrop = model_
                        , changed = True
                        , bracket = RemoteData.map updatedBracket model.bracket
                    }

                Just ( DraggableResult from, DroppableGamePosition to, _ ) ->
                    let
                        updatedBracket bracket =
                            { bracket | games = connectGameResult bracket.games from to }
                    in
                    { model
                        | dragDrop = model_
                        , changed = True
                        , bracket = RemoteData.map updatedBracket model.bracket
                    }

                _ ->
                    { model | dragDrop = model_ }
            , DragDrop.getDragstartEvent msg_
                |> Maybe.map (.event >> dragstart)
                |> Maybe.withDefault Cmd.none
            )

        AddGroup ->
            let
                updatedBracket bracket =
                    let
                        nextGroupId =
                            List.length bracket.groups

                        newGroup =
                            Group nextGroupId ("Group " ++ String.fromInt (nextGroupId + 1)) True
                    in
                    { bracket | groups = bracket.groups ++ [ newGroup ] }
            in
            ( { model
                | changed = True
                , bracket = RemoteData.map updatedBracket model.bracket
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

                updatedGroups : List Group -> List Group
                updatedGroups groups =
                    List.Extra.updateIf (\g -> g.position == updatedGroup.position) (\g -> updatedGroup) groups

                updatedBracket bracket =
                    { bracket | groups = updatedGroups bracket.groups }
            in
            ( { model
                | changed = True
                , bracket = RemoteData.map updatedBracket model.bracket
              }
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
            let
                updatedGroups : List Group -> List Group
                updatedGroups groups =
                    List.Extra.updateIf (\g -> g.position == group.position) (\g -> group) groups

                updatedBracket bracket =
                    { bracket | groups = updatedGroups bracket.groups }
            in
            ( { model
                | overlay = Nothing
                , changed = True
                , bracket = RemoteData.map updatedBracket model.bracket
              }
            , Cmd.none
            )

        RemoveGroup group ->
            let
                updatedBracket bracket =
                    { bracket | groups = List.Extra.remove group bracket.groups }
            in
            ( { model
                | overlay = Nothing
                , changed = True
                , bracket = RemoteData.map updatedBracket model.bracket
              }
            , Cmd.none
            )

        AddGame coords ->
            let
                -- Add a game to the grid, with the assigned ID, but only if there's room.
                addGame : List Game -> List Game
                addGame games =
                    -- Don't add games on top of each other. Only add a game if there's room.
                    case findGameByCoords coords games of
                        Nothing ->
                            let
                                -- Assign a negative number. It doesn't matter what it is, as long as it never conflicts with any existing game ids.
                                newGameId =
                                    List.length games * -1
                            in
                            games
                                ++ [ Game newGameId
                                        Nothing
                                        coords
                                        [ GamePosition 0 False Nothing
                                        , GamePosition 1 True Nothing
                                        ]
                                   ]

                        _ ->
                            games

                updatedBracket bracket =
                    { bracket | games = addGame bracket.games }
            in
            ( { model
                | changed = True
                , bracket = RemoteData.map updatedBracket model.bracket
              }
            , Cmd.none
            )

        RemoveGame game ->
            let
                updatedBracket bracket =
                    { bracket | games = List.Extra.remove game bracket.games }
            in
            ( { model
                | overlay = Nothing
                , changed = True
                , bracket = RemoteData.map updatedBracket model.bracket
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
                , changed = True
              }
            , Cmd.none
            )

        UpdateGamePosition game position assignment ->
            let
                updatedBracket bracket =
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
                                                            case List.Extra.find (\t -> t.id == teamId) bracket.teams of
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
                                                            case List.Extra.find (\g -> g.id == gameId) bracket.games of
                                                                Just g ->
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
                                    connectGameResult bracket.games ( fromGameId, typedGameResult ) ( game.id, position )

                                Just (TeamAssignment teamId) ->
                                    let
                                        updatedGamePosition gamePosition =
                                            if gamePosition.position == position then
                                                { gamePosition | assignment = Just (TeamAssignment teamId) }

                                            else
                                                gamePosition

                                        updatedGame g =
                                            if g.id == game.id then
                                                { g | gamePositions = List.map updatedGamePosition g.gamePositions }

                                            else
                                                g
                                    in
                                    List.map updatedGame bracket.games

                                _ ->
                                    bracket.games
                    in
                    { bracket | games = updatedGames }
            in
            ( { model
                | changed = True
                , bracket = RemoteData.map updatedBracket model.bracket
              }
            , Cmd.none
            )

        CloseEditGame game ->
            let
                updatedBracket bracket =
                    { bracket | games = List.Extra.updateIf (\g -> g.coords == game.coords) (\g -> game) bracket.games }
            in
            ( { model
                | overlay = Nothing
                , changed = True
                , bracket = RemoteData.map updatedBracket model.bracket
              }
            , Cmd.none
            )

        Save ->
            ( { model | changed = False }
            , saveBracket model.demoMode model.url model.bracket
            )

        Saved (Ok bracketJson) ->
            ( { model | bracket = RemoteData.succeed bracketJson }, Cmd.none )

        Saved (Err error) ->
            -- TODO: Error message that save failed
            ( model, Cmd.none )

        ConfirmRevert ->
            ( { model | overlay = Just RevertConfirmation }, Cmd.none )

        Revert ->
            ( { model
                | overlay = Nothing
                , changed = False
                , bracket = Loading
              }
            , loadBracket model.demoMode model.url
            )

        ReceivedBracketFromServer result ->
            ( { model | bracket = result }
            , Cmd.none
            )

        ReceivedBracketFromLocalStorage data ->
            ( case Decode.decodeValue bracketDecoder data of
                Ok bracket ->
                    { model | bracket = Success bracket }

                Err error ->
                    { model
                        | bracket =
                            if model.demoMode then
                                Success demoBracket

                            else
                                Success emptyBracket
                    }
            , Cmd.none
            )

        ConfirmClear ->
            ( { model | overlay = Just ClearConfirmation }, Cmd.none )

        Clear ->
            ( { model
                | overlay = Nothing
                , changed = True
                , bracket = Success emptyBracket
              }
            , Cmd.none
            )

        CancelConfirmation ->
            ( { model | overlay = Nothing }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    receiveBracketFromLocalStorage ReceivedBracketFromLocalStorage



---- VIEW ----


view : Model -> Html Msg
view model =
    case model.bracket of
        Success bracket ->
            viewOnceLoaded model bracket

        NotAsked ->
            h3 [ class "mt-5 text-center" ] [ text "Initializing..." ]

        Loading ->
            h3 [ class "mt-5 text-center" ] [ text "Loading..." ]

        Failure e ->
            p [ class "mt-5 text-center text-danger" ] [ text (buildErrorMessage e) ]


viewOnceLoaded : Model -> Bracket -> Html Msg
viewOnceLoaded { overlay, dragDrop, changed } bracket =
    let
        dragId =
            DragDrop.getDragId dragDrop

        dropId =
            DragDrop.getDropId dragDrop

        modalOpen =
            overlay /= Nothing
    in
    div [ classList [ ( "modal-open", modalOpen ) ] ]
        [ div [ class "p-3" ]
            [ viewGroups bracket dragId dropId
            , button [ class "btn btn-primary", onClick AddGroup ] [ text "Add Group" ]
            , if modalOpen then
                viewOverlay overlay bracket

              else
                div [ class "save-buttons" ]
                    [ button
                        [ class "btn btn-primary mr-1"
                        , disabled (not changed)
                        , onClick Save
                        ]
                        [ text "Save" ]
                    , button
                        [ class "btn btn-secondary mr-1"
                        , onClick ConfirmRevert
                        ]
                        [ text "Revert" ]
                    , button
                        [ class "btn btn-danger mr-1"
                        , onClick ConfirmClear
                        ]
                        [ text "Clear" ]
                    , a
                        [ class "btn btn-info"
                        , href "https://curling.io/docs/event-management/playoff-brackets"
                        , target "_blank"
                        ]
                        [ text "Help" ]
                    ]
            ]
        , div [ classList [ ( "modal-backdrop", modalOpen ), ( "show", modalOpen ) ] ] []
        ]


viewOverlay : Maybe Overlay -> Bracket -> Html Msg
viewOverlay overlay bracket =
    div [ class "modal", style "display" "block" ]
        [ div [ class "modal-dialog" ]
            [ case overlay of
                Just (EditingGame game) ->
                    viewEditGame bracket game

                Just (EditingGroup group) ->
                    viewEditGroup bracket group

                Just RevertConfirmation ->
                    viewRevertConfirmation

                Just ClearConfirmation ->
                    viewClearConfirmation

                Nothing ->
                    text ""
            ]
        ]


viewEditGroup : Bracket -> Group -> Html Msg
viewEditGroup bracket group =
    let
        hasNoGames : Bool
        hasNoGames =
            bracket.games
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
            [ button
                [ onClick (RemoveGroup group)
                , class "btn btn-danger mr-2"
                , disabled hasNoGames
                , title
                    (if hasNoGames then
                        "Remove games from this group first"

                     else
                        ""
                    )
                ]
                [ text "Remove" ]
            , button
                [ onClick (CloseEditGroup group)
                , class "btn btn-primary"
                , disabled hasNoName
                , title
                    (if hasNoName then
                        "Name is required"

                     else
                        ""
                    )
                ]
                [ text "Close" ]
            ]
        ]


viewEditGame : Bracket -> Game -> Html Msg
viewEditGame bracket game =
    let
        -- Return a list of teams that haven't been assigned to a game yet.
        unassignedTeams : List Team -> List Game -> List Team
        unassignedTeams teams games =
            let
                gamesNotExcluded =
                    games
                        |> List.filter (\g -> g.id /= game.id)

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

        -- Return a list of game results that haven't been assigned to another game yet.
        -- Pass in the currently selected assigninment to be included.
        unassignedGameResults : List Game -> List Assignment
        unassignedGameResults games =
            -- TODO: exclude and include params need to be implemented
            let
                allAssignments =
                    let
                        winners =
                            List.map (\g -> Just (GameAssignment Winner g.id)) games

                        losers =
                            List.map (\g -> Just (GameAssignment Loser g.id)) games
                    in
                    winners ++ losers

                notCurrentGame assignment =
                    case assignment of
                        Just (GameAssignment result gameId) ->
                            not (gameId == game.id)

                        _ ->
                            True

                notAlreadyAssigned assignment =
                    case assignment of
                        Just (GameAssignment result gameId) ->
                            let
                                assignedToGamePosition gamePosition =
                                    assignment == gamePosition.assignment

                                assignedToGame g =
                                    if g.id == game.id then
                                        False

                                    else
                                        List.filter assignedToGamePosition g.gamePositions
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
                |> List.filter notAlreadyAssigned
                |> List.filterMap identity

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
                            List.Extra.find (\g -> g.id == id) bracket.games
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
                ++ (unassignedTeams bracket.teams bracket.games
                        |> List.map (teamOption index selectedGamePosition)
                   )
                ++ (unassignedGameResults bracket.games
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


viewRevertConfirmation : Html Msg
viewRevertConfirmation =
    div [ class "modal-content" ]
        [ div [ class "modal-header" ]
            [ h5 [ class "modal-title" ] [ text "Confirm Revert" ] ]
        , div [ class "modal-body" ]
            [ p []
                [ text "DANGER: This will revert all of your changes since the last save. Are you sure you want to continue?" ]
            ]
        , div [ class "modal-footer" ]
            [ button [ onClick Revert, class "btn btn-danger mr-2" ] [ text "Continue" ]
            , button [ onClick CancelConfirmation, class "btn btn-secondary mr-2" ] [ text "Cancel" ]
            ]
        ]


viewClearConfirmation : Html Msg
viewClearConfirmation =
    div [ class "modal-content" ]
        [ div [ class "modal-header" ]
            [ h5 [ class "modal-title" ] [ text "Confirm Clear" ] ]
        , div [ class "modal-body" ]
            [ p []
                [ text "DANGER: This will completely wipe the bracket so you can start over. Nothing will be saved until you click the Save button, but you will lose any changes made since the last save. Are you sure you want to continue?" ]
            ]
        , div [ class "modal-footer" ]
            [ button [ onClick Clear, class "btn btn-danger mr-2" ] [ text "Continue" ]
            , button [ onClick CancelConfirmation, class "btn btn-secondary mr-2" ] [ text "Cancel" ]
            ]
        ]


viewGroups : Bracket -> Maybe DraggableId -> Maybe DroppableId -> Html Msg
viewGroups bracket dragId dropId =
    div []
        (List.map (viewGroup bracket dragId dropId) bracket.groups)


viewGroup : Bracket -> Maybe DraggableId -> Maybe DroppableId -> Group -> Html Msg
viewGroup bracket dragId dropId group =
    let
        groupGames =
            List.filter (\g -> g.coords.group == group.position) bracket.games
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
                [ viewSvgLines group groupGames
                , table
                    []
                    (List.map (viewRow bracket dragId dropId group) (List.range 0 (rowsForGroup group bracket.games - 1)))
                , viewGames dragId dropId bracket.teams bracket.games groupGames
                ]

             else
                [ div [ class "text-muted group-hide" ] [ text "..." ] ]
            )
        ]


viewRow : Bracket -> Maybe DraggableId -> Maybe DroppableId -> Group -> Int -> Html Msg
viewRow bracket dragId dropId group row =
    tr
        []
        (List.map (viewCell bracket dragId dropId group row) (List.range 0 (colsForGames bracket.games - 1)))


viewCell : Bracket -> Maybe DraggableId -> Maybe DroppableId -> Group -> Int -> Int -> Html Msg
viewCell bracket dragId dropId group row col =
    let
        onCoords =
            Coords group.position col row

        onGame =
            findGameByCoords onCoords bracket.games

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
        [ text "" ]


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


viewSvgLines : Group -> List Game -> Html Msg
viewSvgLines group games =
    let
        lineConnectors : List LineConnector
        lineConnectors =
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
                                                                32

                                                               else
                                                                57
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
                                                        32

                                                       else
                                                        57
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

        strPoint coords =
            String.fromInt (Tuple.first coords) ++ "," ++ String.fromInt (Tuple.second coords) ++ " "

        viewSvgLine l =
            polyline
                [ fill "none"
                , strokeOpacity "0.5"
                , strokeDasharray "3"
                , stroke
                    (case l.result of
                        Winner ->
                            "green"

                        Loser ->
                            "red"
                    )
                , points
                    (strPoint ( Tuple.first l.fromCoords + 6, Tuple.second l.fromCoords )
                        ++ strPoint ( Tuple.first l.fromCoords + 11, Tuple.second l.fromCoords )
                        ++ strPoint ( Tuple.first l.toCoords - 8, Tuple.second l.toCoords )
                        ++ strPoint ( Tuple.first l.toCoords - 3, Tuple.second l.toCoords )
                    )
                ]
                []
    in
    div [ class "group-lines" ]
        [ svg
            [ Svg.Attributes.width (String.fromInt ((colsForGames games + 1) * gridSize)), Svg.Attributes.height (String.fromInt ((rowsForGroup group games - 1) * gridSize)) ]
            (List.map viewSvgLine lineConnectors)
        ]



---- PROGRAM ----


main : Program { demoMode : Bool, url : String } Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
