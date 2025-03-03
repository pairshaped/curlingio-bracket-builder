port module BracketBuilder exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onDoubleClick, onInput)
import Html5.DragDrop as DragDrop
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra
import Random
import RemoteData exposing (RemoteData(..), WebData)
import RemoteData.Http
import Set
import String.Extra
import Svg exposing (line, polyline, svg)
import Svg.Attributes exposing (fill, points, stroke, strokeDasharray, strokeOpacity, x1, x2, y1, y2)
import UUID exposing (UUID)



---- PORTS ----


port dragstart : Decode.Value -> Cmd msg



---- MODEL ----


gridSize : Int
gridSize =
    50


type alias Model =
    { flags : Flags
    , nextGameId : Maybe String
    , dragDrop : DragDrop.Model DraggableId DroppableId
    , overlay : Maybe Overlay
    , changed : Bool
    , teams : WebData (List Team)
    , bracket : WebData Bracket
    , lastError : Maybe String
    }


type alias Flags =
    { id : Maybe Int
    , bracketsUrl : String
    , teamsUrl : String
    , backUrl : String
    }


type Overlay
    = EditingBracketName
    | EditingGroup Group
    | EditingGame Game
    | RevertConfirmation
    | ClearConfirmation


type DraggableId
    = DraggableGame String
    | DraggableResult Assignment


type DroppableId
    = DroppableCell Coords
    | DroppableSide ( String, Int )


type alias Team =
    { id : Int
    , name : String
    }


type alias Bracket =
    { id : Maybe Int
    , name : String
    , groups : List Group
    , games : List Game
    }


type alias Group =
    { id : Int
    , name : String
    , visible : Bool
    }


type alias Game =
    { errorMessage : Maybe String
    , id : String
    , name : Maybe String
    , markedForDeletion : Bool
    , coords : Coords
    , state : GameState
    , sides : List Side
    }


type GameState
    = GamePending
    | GameActive
    | GameComplete


type alias Side =
    { position : Int
    , assignment : Maybe Assignment
    , teamId : Maybe Int
    , result : Maybe SideResult
    }


type SideResult
    = SideResultWon
    | SideResultLost


type alias Coords =
    { groupId : Int
    , col : Int
    , row : Int
    }


type Assignment
    = TeamAssignment Int
    | WinnerAssignment String
    | LoserAssignment String


type GameResult
    = Winner
    | Loser


type alias LineConnector =
    { gameResult : GameResult
    , fromCoords : ( Int, Int )
    , toCoords : ( Int, Int )
    }


emptyBracket : Maybe Int -> String -> Bracket
emptyBracket id name =
    { id = id
    , name = name
    , groups =
        [ Group 1 "Group 1" True ]
    , games = []
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { flags = flags
      , nextGameId = Nothing
      , dragDrop = DragDrop.init
      , overlay = Nothing
      , changed = False
      , teams = Loading
      , bracket =
            case flags.id of
                Just _ ->
                    Loading

                Nothing ->
                    Success (emptyBracket Nothing "Playoffs")
      , lastError = Nothing
      }
    , Cmd.batch
        [ loadTeams flags
        , case flags.id of
            Just _ ->
                loadBracket flags

            Nothing ->
                Cmd.none
        , generateNextGameId
        ]
    )



--- HELPERS ---


generateNextGameId : Cmd Msg
generateNextGameId =
    Random.generate GenerateNextGameId Random.independentSeed


{-| Find a game by it's coordinates
-}
findGameByCoords : Coords -> List Game -> Maybe Game
findGameByCoords coords games =
    List.Extra.find (\game -> game.coords == coords) games


clearAssignmentFromAllGames : List Game -> Assignment -> List Game
clearAssignmentFromAllGames games assignment =
    let
        unassignedSide side =
            -- Unassign the old connection if it exists
            if side.assignment == Just assignment then
                { side | assignment = Nothing }

            else
                side

        unassignedGame game =
            { game | sides = List.map unassignedSide game.sides }
    in
    List.map unassignedGame games


connectGameResult : List Game -> Assignment -> ( String, Int ) -> List Game
connectGameResult games assignment ( toGameId, toPosition ) =
    let
        assignedSide side =
            if side.position == toPosition then
                -- Assign the new connection
                { side | assignment = Just assignment }

            else
                side

        assignedGame game =
            if game.id == toGameId then
                { game | sides = List.map assignedSide game.sides }

            else
                game
    in
    clearAssignmentFromAllGames games assignment
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
        |> List.filter (\g -> g.coords.groupId == group.id)
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


saveBracket : Flags -> WebData Bracket -> Cmd Msg
saveBracket { bracketsUrl } bracketResult =
    let
        sendCmd action url bracket =
            action url ReceivedBracketFromServer bracketDecoder (bracketEncoder bracket)
    in
    case bracketResult of
        Success bracket ->
            case bracket.id of
                Just id ->
                    sendCmd RemoteData.Http.patch (bracketsUrl ++ "/" ++ String.fromInt id) bracket

                Nothing ->
                    sendCmd RemoteData.Http.post bracketsUrl bracket

        _ ->
            Cmd.none


loadBracket : Flags -> Cmd Msg
loadBracket { bracketsUrl, id } =
    let
        url =
            case id of
                Just id_ ->
                    bracketsUrl ++ "/" ++ String.fromInt id_

                Nothing ->
                    bracketsUrl ++ "/new"
    in
    RemoteData.Http.get url ReceivedBracketFromServer bracketDecoder


loadTeams : Flags -> Cmd Msg
loadTeams { teamsUrl } =
    RemoteData.Http.get teamsUrl ReceivedTeamsFromServer teamsDecoder


assignTeamsForCompletedGames : List Game -> List Game
assignTeamsForCompletedGames games =
    -- If the winner or loser assignment changes, it's possible that the game that's assigned has actually completed,
    -- and if that's the case, we also want to assign the team that won / lost.
    let
        updatedGame game =
            let
                updatedSide side =
                    let
                        updatedTeamId =
                            let
                                sidesForGameId gameId =
                                    -- Only if the game has completed
                                    List.Extra.find (\g -> g.id == gameId && g.state == GameComplete) games
                                        |> Maybe.map .sides
                                        |> Maybe.withDefault []
                            in
                            case side.assignment of
                                Just (WinnerAssignment gameId) ->
                                    -- Check if the game has completed, and assign the winning team if it has.
                                    List.Extra.find (\s -> s.result == Just SideResultWon) (sidesForGameId gameId)
                                        |> Maybe.map .teamId
                                        |> Maybe.withDefault Nothing

                                Just (LoserAssignment gameId) ->
                                    -- Check if the game has completed, and assign the losing team if it has.
                                    List.Extra.find (\s -> s.result == Just SideResultLost) (sidesForGameId gameId)
                                        |> Maybe.map .teamId
                                        |> Maybe.withDefault Nothing

                                Nothing ->
                                    -- Make sure a team isn't still assigned from earlier.
                                    Nothing

                                Just (TeamAssignment _) ->
                                    -- Do nothing. A direct team assignment is fine as is.
                                    side.teamId
                    in
                    { side | teamId = updatedTeamId }
            in
            { game | sides = List.map updatedSide game.sides }
    in
    List.map updatedGame games


visibleGames : List Game -> List Game
visibleGames games =
    List.filter (\g -> g.markedForDeletion == False) games



---- ENCODERS ----


wrapperEncoder : Bracket -> Encode.Value
wrapperEncoder bracket =
    Encode.object
        [ ( "bracket", bracketEncoder bracket )
        ]


bracketEncoder : Bracket -> Encode.Value
bracketEncoder bracket =
    Encode.object
        [ ( "id"
          , case bracket.id of
                Just id ->
                    Encode.int id

                _ ->
                    Encode.null
          )
        , ( "name_en", Encode.string bracket.name )
        , ( "groups", groupsEncoder bracket.groups )
        , ( "games", gamesEncoder bracket.games )
        ]


groupsEncoder : List Group -> Encode.Value
groupsEncoder groups =
    let
        groupEncoder : Group -> Encode.Value
        groupEncoder group =
            Encode.object
                [ ( "id", Encode.int group.id )
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
                sideEncoder : Side -> Encode.Value
                sideEncoder side =
                    Encode.object
                        [ ( "position", Encode.int side.position )
                        , ( "team_id"
                          , case side.teamId of
                                Just teamId ->
                                    Encode.int teamId

                                Nothing ->
                                    Encode.null
                          )
                        , ( "winner_id"
                          , case side.assignment of
                                Just (WinnerAssignment id) ->
                                    Encode.string id

                                _ ->
                                    Encode.null
                          )
                        , ( "loser_id"
                          , case side.assignment of
                                Just (LoserAssignment id) ->
                                    Encode.string id

                                _ ->
                                    Encode.null
                          )
                        ]

                coordsEncoder : Coords -> Encode.Value
                coordsEncoder coords =
                    Encode.object
                        [ ( "group_id", Encode.int coords.groupId )
                        , ( "col", Encode.int coords.col )
                        , ( "row", Encode.int coords.row )
                        ]
            in
            Encode.object
                [ ( "id", Encode.string game.id )
                , ( "name"
                  , case game.name of
                        Just name ->
                            Encode.string name

                        Nothing ->
                            Encode.null
                  )
                , ( "_delete", Encode.bool game.markedForDeletion )
                , ( "game_positions", Encode.list sideEncoder game.sides )
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


errorMessageDecoder : Decode.Decoder String
errorMessageDecoder =
    Decode.field "error" Decode.string


bracketDecoder : Decode.Decoder Bracket
bracketDecoder =
    Decode.map4
        Bracket
        (Decode.field "id" Decode.int |> Decode.maybe)
        (Decode.field "name" Decode.string)
        (Decode.field "groups" (Decode.list groupDecoder))
        (Decode.field "games" (Decode.list gameDecoder))


teamsDecoder : Decode.Decoder (List Team)
teamsDecoder =
    Decode.list teamDecoder


teamDecoder : Decode.Decoder Team
teamDecoder =
    Decode.map2
        Team
        (Decode.field "id" Decode.int)
        (Decode.field "short_name" Decode.string)


groupDecoder : Decode.Decoder Group
groupDecoder =
    Decode.map3
        Group
        (Decode.field "id" Decode.int)
        (Decode.field "name" Decode.string)
        (Decode.succeed True)


gameDecoder : Decode.Decoder Game
gameDecoder =
    let
        sideDecoder : Decode.Decoder Side
        sideDecoder =
            let
                assignmentDecoder : Decode.Decoder Assignment
                assignmentDecoder =
                    Decode.oneOf
                        [ Decode.map WinnerAssignment (Decode.field "winner_id" Decode.string)
                        , Decode.map LoserAssignment (Decode.field "loser_id" Decode.string)
                        , Decode.map TeamAssignment (Decode.field "team_id" Decode.int)
                        ]

                decodeSideResult : Decode.Decoder SideResult
                decodeSideResult =
                    Decode.string
                        |> Decode.andThen
                            (\str ->
                                case str of
                                    "won" ->
                                        Decode.succeed SideResultWon

                                    _ ->
                                        Decode.succeed SideResultLost
                            )
            in
            Decode.map4
                Side
                (Decode.field "position" Decode.int)
                (assignmentDecoder |> Decode.maybe)
                (Decode.field "team_id" Decode.int |> Decode.maybe)
                (Decode.field "result" decodeSideResult |> Decode.maybe)

        coordsDecoder : Decode.Decoder Coords
        coordsDecoder =
            Decode.map3
                Coords
                (Decode.field "group_id" Decode.int)
                (Decode.field "col" Decode.int)
                (Decode.field "row" Decode.int)

        decodeGameState : Decode.Decoder GameState
        decodeGameState =
            Decode.string
                |> Decode.andThen
                    (\str ->
                        case String.toLower str of
                            "active" ->
                                Decode.succeed GameActive

                            "complete" ->
                                Decode.succeed GameComplete

                            _ ->
                                Decode.succeed GamePending
                    )
    in
    Decode.map6
        (Game Nothing)
        (Decode.field "id" Decode.string)
        (Decode.field "name" Decode.string |> Decode.maybe)
        (Decode.succeed False)
        (Decode.field "coords" coordsDecoder)
        (Decode.field "state" decodeGameState
            |> Decode.maybe
            |> Decode.map (Maybe.withDefault GamePending)
        )
        (Decode.field "game_positions" (Decode.list sideDecoder))



---- UPDATE ----


type Msg
    = GenerateNextGameId Random.Seed
    | DragDropMsg (DragDrop.Msg DraggableId DroppableId)
    | EditBracketName
    | UpdateBracketName String
    | CloseEditBracketName
    | AddGroup
    | EditGroup Group
    | ToggleGroup Group
    | UpdateGroupName Group String
    | CloseEditGroup Group
    | RemoveGroup Group
    | AddGame Coords
    | RemoveGame Game
    | EditGame Game
    | UpdateGameName String
    | UpdateSide Int String
    | CloseEditGame
    | Save
    | ConfirmRevert
    | Revert
    | ReceivedTeamsFromServer (WebData (List Team))
    | ReceivedBracketFromServer (WebData Bracket)
    | ReceivedErrorFromServer String
    | ConfirmClear
    | Clear
    | CancelConfirmation


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GenerateNextGameId seed ->
            let
                newUuid : String
                newUuid =
                    Random.step UUID.generator seed
                        |> Tuple.first
                        |> UUID.toString
            in
            ( { model | nextGameId = Just newUuid }, Cmd.none )

        DragDropMsg msg_ ->
            let
                ( model_, result ) =
                    DragDrop.update msg_ model.dragDrop
            in
            ( case result of
                Just ( DraggableGame gameId, DroppableCell coords, _ ) ->
                    let
                        updatedBracket : Bracket -> Bracket
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

                Just ( DraggableResult from, DroppableSide ( toGameId, toPosition ), _ ) ->
                    let
                        updatedGame : Game -> Game
                        updatedGame game =
                            if game.id == toGameId then
                                let
                                    updatedSide side =
                                        if side.position == toPosition then
                                            { side | assignment = Just from }

                                        else
                                            side
                                in
                                { game | sides = List.map updatedSide game.sides }

                            else
                                game

                        updatedBracket : Bracket -> Bracket
                        updatedBracket bracket =
                            { bracket
                                | games =
                                    clearAssignmentFromAllGames bracket.games from
                                        |> List.map updatedGame
                                        |> assignTeamsForCompletedGames
                            }
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

        EditBracketName ->
            ( { model | overlay = Just EditingBracketName }
            , Cmd.none
            )

        UpdateBracketName name ->
            let
                updatedBracket : Bracket -> Bracket
                updatedBracket bracket =
                    { bracket | name = name }
            in
            ( { model
                | bracket = RemoteData.map updatedBracket model.bracket
                , changed = True
              }
            , Cmd.none
            )

        CloseEditBracketName ->
            ( { model
                | overlay = Nothing
                , changed = True
              }
            , Cmd.none
            )

        AddGroup ->
            let
                updatedBracket : Bracket -> Bracket
                updatedBracket bracket =
                    let
                        nextGroupId : Int
                        nextGroupId =
                            List.length bracket.groups + 1

                        newGroup : Group
                        newGroup =
                            Group nextGroupId ("Group " ++ String.fromInt nextGroupId) True
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
                    List.Extra.updateIf (\g -> g.id == updatedGroup.id) (\g -> updatedGroup) groups

                updatedBracket : Bracket -> Bracket
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
                    List.Extra.updateIf (\g -> g.id == group.id) (\g -> group) groups

                updatedBracket : Bracket -> Bracket
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
                updatedBracket : Bracket -> Bracket
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
                            case model.nextGameId of
                                Just id ->
                                    games
                                        ++ [ Game Nothing
                                                id
                                                Nothing
                                                False
                                                coords
                                                GamePending
                                                [ Side 0 Nothing Nothing Nothing
                                                , Side 1 Nothing Nothing Nothing
                                                ]
                                           ]

                                Nothing ->
                                    games

                        _ ->
                            games

                updatedBracket : Bracket -> Bracket
                updatedBracket bracket =
                    { bracket | games = addGame bracket.games }
            in
            ( { model
                | changed = True
                , bracket = RemoteData.map updatedBracket model.bracket
              }
            , generateNextGameId
            )

        RemoveGame game ->
            let
                updatedGame : Game -> Game
                updatedGame game_ =
                    if game_.id == game.id then
                        { game_ | markedForDeletion = True, sides = [] }

                    else
                        let
                            updatedSide side =
                                case side.assignment of
                                    Just (WinnerAssignment id) ->
                                        if id == game_.id then
                                            -- Remove the assignment
                                            { side | assignment = Nothing }

                                        else
                                            side

                                    Just (LoserAssignment id) ->
                                        if id == game_.id then
                                            -- Remove the assignment
                                            { side | assignment = Nothing }

                                        else
                                            side

                                    _ ->
                                        side
                        in
                        -- Does the game have a game position assignment to the game we just removed?
                        { game_ | sides = List.map updatedSide game_.sides }

                updatedBracket : Bracket -> Bracket
                updatedBracket bracket =
                    { bracket | games = List.map updatedGame bracket.games }
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

        UpdateGameName name ->
            let
                updatedGame : List Game -> Game -> Game
                updatedGame games game =
                    let
                        maybeName : Maybe String
                        maybeName =
                            case name of
                                "" ->
                                    Nothing

                                _ ->
                                    Just name

                        isNameTaken : Bool
                        isNameTaken =
                            List.any
                                (\g ->
                                    case ( g.name, maybeName ) of
                                        ( Just name1, Just name2 ) ->
                                            String.toUpper name1 == String.toUpper name2

                                        _ ->
                                            False
                                )
                                games
                    in
                    { game
                        | name = maybeName
                        , errorMessage =
                            if isNameTaken then
                                Just "Name is already being used."

                            else
                                Nothing
                    }

                updatedBracket : Game -> Bracket -> Bracket
                updatedBracket game bracket =
                    { bracket | games = List.Extra.updateIf (\g -> g.id == game.id) (\g -> updatedGame bracket.games game) bracket.games }
            in
            ( case model.overlay of
                Just (EditingGame game) ->
                    case model.bracket of
                        Success bracket ->
                            { model
                                | overlay = Just (EditingGame (updatedGame bracket.games game))
                                , changed = True
                                , bracket = Success (updatedBracket game bracket)
                            }

                        _ ->
                            model

                _ ->
                    model
            , Cmd.none
            )

        UpdateSide position assignment ->
            let
                updatedGame : List Team -> List Game -> Game -> Game
                updatedGame teams games game =
                    let
                        typedAssignment : Maybe Assignment
                        typedAssignment =
                            case String.split "_" assignment of
                                x :: xs ->
                                    let
                                        matchesGameId =
                                            case List.head xs of
                                                Just id ->
                                                    case List.Extra.find (\g -> g.id == id) games of
                                                        Just g ->
                                                            Just g.id

                                                        Nothing ->
                                                            Nothing

                                                _ ->
                                                    Nothing

                                        matchesTeamId =
                                            case List.head xs of
                                                Just idStr ->
                                                    case String.toInt idStr of
                                                        Just id ->
                                                            case List.Extra.find (\team -> team.id == id) teams of
                                                                Just team ->
                                                                    Just team.id

                                                                Nothing ->
                                                                    Nothing

                                                        Nothing ->
                                                            Nothing

                                                _ ->
                                                    Nothing
                                    in
                                    case x of
                                        "team" ->
                                            -- A team was selected
                                            Maybe.map TeamAssignment matchesTeamId

                                        "winner" ->
                                            Maybe.map WinnerAssignment matchesGameId

                                        "loser" ->
                                            Maybe.map LoserAssignment matchesGameId

                                        _ ->
                                            Nothing

                                _ ->
                                    Nothing

                        updatedSide side =
                            if side.position == position then
                                { side
                                    | assignment = typedAssignment
                                    , teamId =
                                        case typedAssignment of
                                            Just (TeamAssignment id) ->
                                                -- We also need to set the teamId if one was assigned.
                                                Just id

                                            _ ->
                                                Nothing
                                }

                            else
                                side
                    in
                    { game | sides = List.map updatedSide game.sides }

                updatedBracket : Game -> List Team -> Bracket -> Bracket
                updatedBracket game teams bracket =
                    let
                        shouldUpdateGame g =
                            if g.id == game.id then
                                updatedGame teams bracket.games game

                            else
                                g
                    in
                    { bracket | games = List.map shouldUpdateGame bracket.games |> assignTeamsForCompletedGames }
            in
            ( case ( model.overlay, model.teams, model.bracket ) of
                ( Just (EditingGame game), Success teams, Success bracket ) ->
                    { model
                        | overlay = Just (EditingGame (updatedGame teams bracket.games game))
                        , changed = True
                        , bracket = Success (updatedBracket game teams bracket)
                    }

                _ ->
                    model
            , Cmd.none
            )

        CloseEditGame ->
            ( { model | overlay = Nothing }, Cmd.none )

        Save ->
            ( { model | changed = False }
            , saveBracket model.flags model.bracket
            )

        ConfirmRevert ->
            ( { model | overlay = Just RevertConfirmation }, Cmd.none )

        Revert ->
            ( { model
                | overlay = Nothing
                , changed = False
                , bracket = Loading
              }
            , loadBracket model.flags
            )

        ReceivedTeamsFromServer result ->
            ( { model | teams = result }
            , Cmd.none
            )

        ReceivedBracketFromServer result ->
            let
                sortedGroups groups =
                    List.sortBy .id groups

                updatedBracket bracket =
                    { bracket | groups = sortedGroups bracket.groups }
            in
            ( { model
                | lastError =
                    case result of
                        Failure e ->
                            case e of
                                Http.BadStatus 422 ->
                                    -- Validation error
                                    Just "Validation failed and bracket was not saved. Possibly this bracket name is already being used in your event."

                                _ ->
                                    Just "Unable to save bracket. Please try again later."

                        _ ->
                            Nothing
                , bracket =
                    case result of
                        Success bracket ->
                            Success (updatedBracket bracket)

                        Loading ->
                            Loading

                        _ ->
                            model.bracket
              }
            , Cmd.none
            )

        ReceivedErrorFromServer result ->
            ( { model | lastError = Just result }, Cmd.none )

        ConfirmClear ->
            ( { model | overlay = Just ClearConfirmation }, Cmd.none )

        Clear ->
            let
                newBracket : Bracket -> Bracket
                newBracket bracket =
                    emptyBracket bracket.id bracket.name
            in
            ( { model
                | overlay = Nothing
                , changed = True
                , bracket = RemoteData.map newBracket model.bracket
              }
            , Cmd.none
            )

        CancelConfirmation ->
            ( { model | overlay = Nothing }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



---- VIEW ----


view : Model -> Html Msg
view model =
    case ( model.teams, model.bracket ) of
        ( Success teams, Success bracket ) ->
            viewOnceLoaded model teams bracket

        ( NotAsked, _ ) ->
            h3 [ class "mt-5 text-center" ] [ text "Initializing..." ]

        ( _, NotAsked ) ->
            h3 [ class "mt-5 text-center" ] [ text "Initializing..." ]

        ( Loading, _ ) ->
            h3 [ class "mt-5 text-center" ] [ text "Loading..." ]

        ( _, Loading ) ->
            h3 [ class "mt-5 text-center" ] [ text "Loading..." ]

        ( Failure e, _ ) ->
            p [ class "mt-5 text-center text-danger" ] [ text (buildErrorMessage e) ]

        ( _, Failure e ) ->
            p [ class "mt-5 text-center text-danger" ] [ text (buildErrorMessage e) ]


viewOnceLoaded : Model -> List Team -> Bracket -> Html Msg
viewOnceLoaded { flags, overlay, dragDrop, changed, lastError } teams bracket =
    let
        dragId =
            DragDrop.getDragId dragDrop

        dropId =
            DragDrop.getDropId dragDrop

        modalOpen =
            overlay /= Nothing

        viewErrorMessage =
            case lastError of
                Just e ->
                    div [ class "alert alert-danger w-75" ] [ text e ]

                Nothing ->
                    text ""
    in
    div [ classList [ ( "modal-open", modalOpen ) ] ]
        [ div [ class "py-3" ]
            [ viewErrorMessage
            , viewBracketName bracket.name
            , viewGroups teams bracket dragId dropId
            , button [ class "btn btn-primary", onClick AddGroup ] [ text "Add Group" ]
            , if modalOpen then
                viewOverlay overlay teams bracket

              else
                div [ class "save-buttons" ]
                    [ button
                        [ class "btn btn-primary mr-1"
                        , disabled (not changed || (List.length bracket.games == 0))
                        , onClick Save
                        ]
                        [ text "Save" ]
                    , a
                        [ class "btn btn-secondary"
                        , href flags.backUrl
                        ]
                        [ text "Exit" ]
                    ]
            ]
        , div [ classList [ ( "modal-backdrop", modalOpen ), ( "show", modalOpen ) ] ] []
        ]


viewOverlay : Maybe Overlay -> List Team -> Bracket -> Html Msg
viewOverlay overlay teams bracket =
    div [ class "modal", style "display" "block" ]
        [ div [ class "modal-dialog" ]
            [ case overlay of
                Just EditingBracketName ->
                    viewEditBracketName bracket.name

                Just (EditingGroup group) ->
                    viewEditGroup bracket group

                Just (EditingGame game) ->
                    viewEditGame teams bracket game

                Just RevertConfirmation ->
                    viewRevertConfirmation

                Just ClearConfirmation ->
                    viewClearConfirmation

                Nothing ->
                    text ""
            ]
        ]


viewEditBracketName : String -> Html Msg
viewEditBracketName name =
    let
        hasNoName : Bool
        hasNoName =
            String.trim name == ""
    in
    div [ class "modal-content" ]
        [ div [ class "modal-header" ]
            [ h5 [ class "modal-title" ] [ text "Bracket Name" ] ]
        , div [ class "modal-body" ]
            [ div
                [ class "form-group" ]
                [ input
                    [ class "form-control"
                    , id "editing-bracket-name"
                    , value name
                    , onInput UpdateBracketName
                    ]
                    []
                ]
            ]
        , div [ class "modal-footer d-flex justify-content-between" ]
            [ button
                [ onClick CloseEditBracketName
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


viewEditGroup : Bracket -> Group -> Html Msg
viewEditGroup bracket group =
    let
        games =
            visibleGames bracket.games

        hasNoGames : Bool
        hasNoGames =
            games
                |> List.filter (\g -> g.coords.groupId == group.id)
                |> List.isEmpty
                |> not

        hasNoName : Bool
        hasNoName =
            String.trim group.name == ""
    in
    div [ class "modal-content" ]
        [ div [ class "modal-header" ]
            [ h5 [ class "modal-title" ] [ text "Group Name" ] ]
        , div [ class "modal-body" ]
            [ div
                [ class "form-group" ]
                [ input
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


viewEditGame : List Team -> Bracket -> Game -> Html Msg
viewEditGame teams bracket game =
    let
        games =
            visibleGames bracket.games

        -- Return a list of teams that haven't been assigned to a game yet.
        unassignedTeams : List Team
        unassignedTeams =
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
                    List.any assignedToPosition g.sides

                unassigned team =
                    gamesNotExcluded
                        |> List.filter (assignedTo team)
                        |> List.isEmpty
            in
            List.filter unassigned teams

        -- Return a list of game results that haven't been assigned to another game yet.
        -- Pass in the currently selected assigninment to be included.
        unassignedGameResults : List Assignment
        unassignedGameResults =
            -- TODO: exclude and include params need to be implemented
            let
                allAssignments =
                    let
                        winners =
                            List.map (\g -> Just (WinnerAssignment g.id)) games

                        losers =
                            List.map (\g -> Just (LoserAssignment g.id)) games
                    in
                    winners ++ losers

                notCurrentGame assignment =
                    not ((assignment == Just (WinnerAssignment game.id)) || (assignment == Just (LoserAssignment game.id)))

                notAlreadyAssigned assignment =
                    let
                        assignedToSide side =
                            assignment == side.assignment

                        assignedToGame g =
                            if g.id == game.id then
                                False

                            else
                                List.filter assignedToSide g.sides
                                    |> List.isEmpty
                                    |> not
                    in
                    case assignment of
                        Nothing ->
                            True

                        Just (TeamAssignment _) ->
                            True

                        _ ->
                            -- Check to see if this assignment has already been made against any game, except the current game.
                            List.filter assignedToGame games
                                |> List.isEmpty
            in
            allAssignments
                |> List.filter notCurrentGame
                |> List.filter notAlreadyAssigned
                |> List.filterMap identity

        teamOption : Int -> Side -> Team -> Html Msg
        teamOption index selectedSide team =
            let
                isSelected =
                    case selectedSide.assignment of
                        Just (TeamAssignment id) ->
                            id == team.id

                        _ ->
                            False
            in
            option [ value ("team_" ++ String.fromInt team.id), selected isSelected ] [ text team.name ]

        gameOption : Int -> Side -> Assignment -> Html Msg
        gameOption index selectedSide assignment =
            let
                isSelected =
                    selectedSide.assignment == Just assignment

                optionId =
                    case assignment of
                        WinnerAssignment id ->
                            Just ("winner_" ++ id)

                        LoserAssignment id ->
                            Just ("loser_" ++ id)

                        _ ->
                            Nothing

                optionLabel =
                    let
                        gameName id =
                            List.Extra.find (\g -> g.id == id) games
                                |> Maybe.map (\g -> Maybe.withDefault "TBD" g.name)
                    in
                    case assignment of
                        WinnerAssignment id ->
                            Just ("Winner of " ++ Maybe.withDefault "TBD" (gameName id))

                        LoserAssignment id ->
                            Just ("Loser of " ++ Maybe.withDefault "TBD" (gameName id))

                        _ ->
                            Nothing
            in
            case ( optionId, optionLabel ) of
                ( Just id, Just label ) ->
                    option [ value id, selected isSelected ] [ text label ]

                _ ->
                    text ""

        assignmentOptions : Int -> Side -> List (Html Msg)
        assignmentOptions index selectedSide =
            [ option [] [] ]
                ++ (unassignedTeams
                        |> List.map (teamOption index selectedSide)
                   )
                ++ (unassignedGameResults
                        |> List.map (gameOption index selectedSide)
                   )

        viewSideField index side =
            div
                [ class "form-group" ]
                [ label [ for "editing-game" ] [ text "Team" ]
                , select
                    [ class "form-control"
                    , id "editing-game"
                    , onInput (UpdateSide index)
                    , disabled (game.state /= GamePending && side.assignment /= Nothing)
                    ]
                    (assignmentOptions index side)
                ]
    in
    div [ class "modal-content" ]
        [ div [ class "modal-header" ]
            [ h5 [ class "modal-title" ] [ text "Edit Game" ] ]
        , div [ class "modal-body" ]
            ([ case game.errorMessage of
                Just errorMessage ->
                    div [ class "alert alert-danger" ] [ text errorMessage ]

                Nothing ->
                    text ""
             , div
                [ class "form-group" ]
                [ label [ for "editing-game-name" ] [ text "Game Name" ]
                , input
                    [ class "form-control"
                    , id "editing-game-name"
                    , value (Maybe.withDefault "" game.name)
                    , onInput UpdateGameName
                    ]
                    []
                ]
             ]
                ++ List.indexedMap viewSideField game.sides
            )
        , div [ class "modal-footer d-flex justify-content-between" ]
            [ div [ class "text-left" ]
                [ if game.state == GamePending then
                    text ""

                  else
                    i [ class "text-danger" ] [ text "Teams cannot be assigned after game starts." ]
                ]
            , button [ onClick CloseEditGame, class "btn btn-primary mr-2", disabled (game.errorMessage /= Nothing) ] [ text "Close" ]
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
                [ text "DANGER: This will empty out the bracket so you can start over. Nothing will be saved until you click the Save button, but you will lose any changes made since the last save. Are you sure you want to continue?" ]
            ]
        , div [ class "modal-footer" ]
            [ button [ onClick Clear, class "btn btn-danger mr-2" ] [ text "Continue" ]
            , button [ onClick CancelConfirmation, class "btn btn-secondary mr-2" ] [ text "Cancel" ]
            ]
        ]


viewBracketName : String -> Html Msg
viewBracketName name =
    div
        [ class "d-flex bracket-name" ]
        [ h2
            [ onClick EditBracketName ]
            [ text name ]
        , h2
            [ class "ml-2"
            , onClick EditBracketName
            ]
            [ text "✎" ]
        ]


viewGroups : List Team -> Bracket -> Maybe DraggableId -> Maybe DroppableId -> Html Msg
viewGroups teams bracket dragId dropId =
    div []
        (List.map (viewGroup teams bracket dragId dropId) bracket.groups)


viewGroup : List Team -> Bracket -> Maybe DraggableId -> Maybe DroppableId -> Group -> Html Msg
viewGroup teams bracket dragId dropId group =
    let
        games =
            visibleGames bracket.games

        groupGames =
            List.filter (\g -> g.coords.groupId == group.id) games
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
                    (List.map (viewRow bracket dragId dropId group) (List.range 0 (rowsForGroup group games - 1)))
                , viewGames dragId dropId teams games groupGames
                ]

             else
                [ div [ class "text-muted group-hide" ] [ text "..." ] ]
            )
        ]


viewRow : Bracket -> Maybe DraggableId -> Maybe DroppableId -> Group -> Int -> Html Msg
viewRow bracket dragId dropId group row =
    let
        games =
            visibleGames bracket.games
    in
    tr
        []
        (List.map (viewCell bracket dragId dropId group row) (List.range 0 (colsForGames games - 1)))


viewCell : Bracket -> Maybe DraggableId -> Maybe DroppableId -> Group -> Int -> Int -> Html Msg
viewCell bracket dragId dropId group row col =
    let
        games =
            visibleGames bracket.games

        onCoords =
            Coords group.id col row

        onGame =
            findGameByCoords onCoords games

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
            , case game.state of
                GameComplete ->
                    text ""

                _ ->
                    div
                        [ class "game-delete align-self-end"
                        , onClick (RemoveGame game)
                        ]
                        [ text "✘" ]
            ]
        , div [ class "game-body d-flex" ]
            [ div
                [ class "game-positions flex-fill" ]
                (List.indexedMap (\index side -> viewSide dragId dropId teams games game index side) game.sides)
            , div [ class "align-self-end ml-1" ] (viewResultConnectors dragId game.id)
            ]
        ]


viewSide : Maybe DraggableId -> Maybe DroppableId -> List Team -> List Game -> Game -> Int -> Side -> Html Msg
viewSide dragId dropId teams games onGame position side =
    let
        positionClass =
            if position == 0 then
                ( "game-top", True )

            else
                ( "game-bottom", True )

        dropTarget =
            if onGame.state == GamePending || side.assignment == Nothing then
                -- Only allow assignments for games that haven't started or is missing a team.
                case ( dragId, dropId ) of
                    ( Just (DraggableResult _), Just (DroppableSide gameIdAndPosition) ) ->
                        if gameIdAndPosition == ( onGame.id, position ) then
                            True

                        else
                            False

                    _ ->
                        False

            else
                False

        label =
            case side.assignment of
                Just (TeamAssignment id) ->
                    case List.Extra.find (\t -> t.id == id) teams of
                        Just team ->
                            team.name

                        Nothing ->
                            "TBD"

                Just (WinnerAssignment id) ->
                    case List.Extra.find (\g -> g.id == id) games of
                        Just g ->
                            case side.teamId of
                                Just teamId ->
                                    List.Extra.find (\t -> t.id == teamId) teams
                                        |> Maybe.map .name
                                        |> Maybe.withDefault (Maybe.withDefault "" g.name)

                                Nothing ->
                                    "W: " ++ Maybe.withDefault "TDB" g.name

                        Nothing ->
                            "TBD"

                Just (LoserAssignment id) ->
                    case List.Extra.find (\g -> g.id == id) games of
                        Just g ->
                            case side.teamId of
                                Just teamId ->
                                    List.Extra.find (\t -> t.id == teamId) teams
                                        |> Maybe.map .name
                                        |> Maybe.withDefault (Maybe.withDefault "" g.name)

                                Nothing ->
                                    "L: " ++ Maybe.withDefault "TDB" g.name

                        Nothing ->
                            "TBD"

                Nothing ->
                    "TBD"
    in
    div
        ([ classList [ positionClass, ( "drop-target", dropTarget ) ] ] ++ DragDrop.droppable DragDropMsg (DroppableSide ( onGame.id, position )))
        [ text label ]


viewResultConnectors : Maybe DraggableId -> String -> List (Html Msg)
viewResultConnectors dragId gameId =
    let
        dragging assignment =
            case dragId of
                Just (DraggableResult assignment_) ->
                    assignment == assignment_

                _ ->
                    False
    in
    [ div
        ([ classList
            [ ( "game-result-connector", True )
            , ( "dragging-connector", dragging (WinnerAssignment gameId) )
            ]
         ]
            ++ DragDrop.draggable DragDropMsg (DraggableResult (WinnerAssignment gameId))
        )
        [ div [ class "game-result-connector-icon game-result-connector-winner" ] [ text "W" ]
        ]
    , div
        ([ classList
            [ ( "game-result-connector", True )
            , ( "dragging-connector", dragging (LoserAssignment gameId) )
            ]
         ]
            ++ DragDrop.draggable DragDropMsg (DraggableResult (LoserAssignment gameId))
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
                        connectorForPosition : Int -> Side -> Maybe LineConnector
                        connectorForPosition toPosition side =
                            let
                                fromCoords fromGameId gameResult =
                                    case List.Extra.find (\g -> g.id == fromGameId) games of
                                        Just fromGame ->
                                            Just
                                                ( fromGame.coords.col * gridSize + 175
                                                , fromGame.coords.row
                                                    * gridSize
                                                    + (if gameResult == Winner then
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
                            case side.assignment of
                                Just (WinnerAssignment fromGameId) ->
                                    case ( fromCoords fromGameId Winner, toCoords ) of
                                        ( Just from, Just to ) ->
                                            Just (LineConnector Winner from to)

                                        _ ->
                                            Nothing

                                Just (LoserAssignment fromGameId) ->
                                    case ( fromCoords fromGameId Loser, toCoords ) of
                                        ( Just from, Just to ) ->
                                            Just (LineConnector Loser from to)

                                        _ ->
                                            Nothing

                                _ ->
                                    Nothing
                    in
                    List.indexedMap connectorForPosition toGame.sides
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
                    (case l.gameResult of
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


main : Program Flags Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
