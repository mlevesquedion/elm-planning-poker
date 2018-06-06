module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import WebSocket
import Set
import Material
import Material.Scheme as Scheme
import Material.Layout as Layout
import Material.Button as Button
import Material.Textfield as Textfield
import Material.Card as Card
import Material.Color as Color
import Material.Options as Options
import Material.List as Lists
import Material.Grid as Grid
import Material.Typography as Typography
import Json.Decode exposing (..)
import Json.Encode as JE


main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Flags =
    { websocketHost : String }



-- MODEL


type UserType
    = Dealer
    | Bidder


type alias Player =
    { name : String
    , id : String
    , vote : Maybe String
    }


type Page
    = HomePage
    | RoomSuccess
    | JoinSuccess
    | JoinFailure
    | VotingPage
    | ResultsPage


type Error
    = NameError
    | RoomNumberError
    | GoalError


type ServerError
    = UnidentifiedError
    | CreateRoomError
    | JoinRoomError
    | AddPlayerError
    | RemovePlayerError
    | NewGoalError
    | NewVoteError


type alias Model =
    { user : UserType
    , players : List Player
    , userName : String
    , userId : String
    , roomNumber : String
    , currentGoal : String
    , nextGoal : String
    , currentVote : String
    , page : Page
    , error : Maybe Error
    , webSocketHost : String
    , serverError : Maybe ServerError
    , chainCommand : Maybe (Cmd Msg)
    , mdl :
        Material.Model
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { user = Bidder
      , players = []
      , userName = ""
      , userId = ""
      , roomNumber = ""
      , currentGoal = ""
      , nextGoal = ""
      , currentVote = ""
      , page = HomePage
      , error = Nothing
      , webSocketHost = flags.websocketHost
      , chainCommand = Nothing
      , serverError = Nothing
      , mdl =
            Material.model
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = CreateRoom
    | CreateGoal
    | JoinRoom
    | ServerMsg String
    | UpdateName String
    | UpdateRoomNumber String
    | UpdateNextGoal String
    | UpdateVote String
    | GoHome
    | Mdl (Material.Msg Msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CreateRoom ->
            if String.isEmpty model.userName then
                { model | error = Just NameError } ! [ Cmd.none ]
            else
                model ! [ createRoom model ]

        CreateGoal ->
            if String.isEmpty model.nextGoal then
                { model | error = Just GoalError } ! [ Cmd.none ]
            else
                model ! [ createGoal model ]

        JoinRoom ->
            if String.isEmpty model.roomNumber then
                { model | error = Just RoomNumberError } ! [ Cmd.none ]
            else if String.isEmpty model.userName then
                { model | error = Just NameError } ! [ Cmd.none ]
            else
                model ! [ joinRoom model ]

        ServerMsg json ->
            let
                newModel =
                    decodeServerMsg json model

                command =
                    case newModel.chainCommand of
                        Just newCommand ->
                            newCommand

                        Nothing ->
                            Cmd.none
            in
                { newModel | chainCommand = Nothing } ! [ command ]

        UpdateName newName ->
            { model | userName = newName } ! [ Cmd.none ]

        UpdateNextGoal newGoal ->
            { model | nextGoal = newGoal } ! [ Cmd.none ]

        UpdateRoomNumber newRoomNumber ->
            { model | roomNumber = newRoomNumber } ! [ Cmd.none ]

        UpdateVote newVote ->
            { model | currentVote = newVote } ! [ sendVote model newVote ]

        GoHome ->
            { model | page = HomePage } ! [ Cmd.none ]

        Mdl msg_ ->
            Material.update Mdl msg_ model


decodeServerMsg : String -> Model -> Model
decodeServerMsg json model =
    let
        type_ =
            Result.withDefault "404" (decodeString (field "type" string) json)
    in
        case type_ of
            "room_success" ->
                let
                    roomNumber =
                        decodeString (field "room_number" string) json

                    userId =
                        decodeString (field "player_id" string) json
                in
                    case ( roomNumber, userId ) of
                        ( Ok room, Ok id ) ->
                            { model | page = RoomSuccess, user = Dealer, roomNumber = room, userId = id, players = (Player model.userName id Nothing :: model.players) }

                        _ ->
                            { model | serverError = Just CreateRoomError }

            "join_failure" ->
                { model | page = JoinFailure, roomNumber = "" }

            "join_success" ->
                let
                    userId =
                        decodeString (field "player_id" string) json
                in
                    case userId of
                        Ok id ->
                            { model | page = JoinSuccess, userId = id }

                        _ ->
                            { model | serverError = Just JoinRoomError }

            "add_player" ->
                let
                    playerName =
                        decodeString (field "player_name" string) json

                    playerId =
                        decodeString (field "player_id" string) json
                in
                    case ( playerName, playerId ) of
                        ( Ok name, Ok id ) ->
                            { model
                                | players = (Player name id Nothing) :: model.players
                                , chainCommand = Just (sendGoal id model)
                            }

                        _ ->
                            { model | serverError = Just AddPlayerError }

            "remove_player" ->
                let
                    playerId =
                        decodeString (field "player_id" string) json
                in
                    case playerId of
                        Ok id ->
                            { model | players = List.filter ((/=) id << .id) model.players }

                        _ ->
                            { model | serverError = Just RemovePlayerError }

            "new_goal" ->
                let
                    newGoal =
                        decodeString (field "goal" string) json
                in
                    case newGoal of
                        Ok goal ->
                            { model
                                | page = VotingPage
                                , currentGoal = goal
                                , nextGoal = ""
                                , currentVote = ""
                                , error = Nothing
                                , players = resetVotes model.players
                            }

                        _ ->
                            { model | serverError = Just NewGoalError }

            "new_vote" ->
                let
                    newVote =
                        decodeString (field "vote" string) json

                    playerId =
                        decodeString (field "player_id" string)
                            json
                in
                    case ( newVote, playerId ) of
                        ( Ok vote, Ok id ) ->
                            let
                                players =
                                    List.map
                                        (\player ->
                                            if player.id == id then
                                                Player player.name player.id (Just vote)
                                            else
                                                player
                                        )
                                        model.players

                                page =
                                    (if List.all hasVoted players then
                                        ResultsPage
                                     else
                                        VotingPage
                                    )
                            in
                                { model
                                    | players = players
                                    , page = page
                                }

                        _ ->
                            { model | serverError = Just NewVoteError }

            _ ->
                model


hasVoted : Player -> Bool
hasVoted player =
    case player.vote of
        Just vote ->
            True

        Nothing ->
            False


resetVotes : List Player -> List Player
resetVotes players =
    List.map
        (\player -> Player player.name player.id Nothing)
        players


createRoom : Model -> Cmd Msg
createRoom model =
    let
        host =
            model.webSocketHost
    in
        WebSocket.send host
            (JE.encode 0
                (JE.object
                    [ ( "type", JE.string "create_room" )
                    ]
                )
            )


sendVote : Model -> String -> Cmd Msg
sendVote model vote =
    let
        host =
            model.webSocketHost
    in
        WebSocket.send host
            (JE.encode 0
                (JE.object
                    [ ( "type", JE.string "new_vote" )
                    , ( "vote", JE.string vote )
                    , ( "player_id", JE.string model.userId )
                    , ( "room_number", JE.string model.roomNumber )
                    ]
                )
            )


createGoal : Model -> Cmd Msg
createGoal model =
    let
        host =
            model.webSocketHost
    in
        WebSocket.send host
            (JE.encode 0
                (JE.object
                    [ ( "type", JE.string "create_goal" )
                    , ( "goal", JE.string model.nextGoal )
                    , ( "room_number", JE.string model.roomNumber )
                    ]
                )
            )


sendGoal : String -> Model -> Cmd Msg
sendGoal playerId model =
    let
        host =
            model.webSocketHost

        goal =
            if String.isEmpty model.nextGoal then
                model.currentGoal
            else
                model.nextGoal
    in
        WebSocket.send host
            (JE.encode 0
                (JE.object
                    [ ( "type", JE.string "send_goal" )
                    , ( "goal", JE.string goal )
                    , ( "room_number", JE.string model.roomNumber )
                    , ( "player_id", JE.string playerId )
                    ]
                )
            )


joinRoom : Model -> Cmd Msg
joinRoom model =
    let
        host =
            model.webSocketHost
    in
        WebSocket.send host
            (JE.encode 0
                (JE.object
                    [ ( "type", JE.string "join_room" )
                    , ( "room_number", JE.string model.roomNumber )
                    , ( "name", JE.string model.userName )
                    ]
                )
            )



-- VIEW


view : Model -> Html Msg
view model =
    Layout.render Mdl
        model.mdl
        [ Layout.fixedHeader
        , Layout.scrolling
        ]
        { header =
            [ h1
                [ style [ ( "padding", "2rem" ) ] ]
                [ text <|
                    "Planning poker"
                        ++ (if not <| String.isEmpty model.roomNumber then
                                " @ room " ++ model.roomNumber
                            else
                                ""
                           )
                ]
            ]
        , drawer = []
        , tabs = ( [], [] )
        , main = [ viewPage model ]
        }


viewPage : Model -> Html Msg
viewPage model =
    let
        rendering =
            case model.page of
                HomePage ->
                    viewHomePage model

                RoomSuccess ->
                    viewRoomSuccess model

                JoinSuccess ->
                    viewJoinSuccess model

                JoinFailure ->
                    viewJoinFailure model

                VotingPage ->
                    if String.isEmpty model.currentGoal then
                        viewJoinSuccess model
                    else
                        viewVotingPage model

                ResultsPage ->
                    viewResultsPage model

        serverError =
            model.serverError
    in
        div []
            [ rendering
            , Options.styled p
                [ Typography.title, Typography.center ]
                [ if serverError /= Nothing then
                    (text <| "Server error: " ++ toString (Maybe.withDefault UnidentifiedError serverError))
                  else
                    text ""
                ]
            ]
            |> Scheme.topWithScheme Color.Teal Color.LightGreen


viewHomePage : Model -> Html Msg
viewHomePage model =
    Grid.grid []
        [ Grid.cell [ Grid.size Grid.All 6 ]
            [ Card.view []
                [ Card.actions
                    []
                    [ Textfield.render Mdl
                        [ 0, 0 ]
                        model.mdl
                        [ Textfield.label "Your name"
                        , Textfield.floatingLabel
                        , Textfield.value model.userName
                        , Textfield.error ("Please enter a name")
                            |> Options.when (model.error == Just NameError)
                        , Options.onInput UpdateName
                        ]
                        []
                    ]
                ]
            ]
        , Grid.cell [ Grid.size Grid.All 6 ]
            [ Card.view []
                [ Card.text [ Card.expand ]
                    []

                -- Filler
                , Card.actions
                    []
                    [ Button.render Mdl
                        [ 0, 1 ]
                        model.mdl
                        [ Button.raised
                        , Button.ripple
                        , Button.colored
                        , Options.onClick CreateRoom
                        ]
                        [ text "Create a room" ]
                    ]
                , Card.text [ Card.expand ]
                    []

                -- Filler
                , Card.actions
                    []
                    [ Textfield.render Mdl
                        [ 0, 2 ]
                        model.mdl
                        [ Textfield.label "Existing room number"
                        , Textfield.floatingLabel
                        , Textfield.value model.roomNumber
                        , Textfield.error ("Please enter a room number")
                            |> Options.when (model.error == Just RoomNumberError)
                        , Options.onInput UpdateRoomNumber
                        ]
                        []
                    ]
                , Card.actions
                    []
                    [ Button.render Mdl
                        [ 0, 3 ]
                        model.mdl
                        [ Button.raised
                        , Button.colored
                        , Button.ripple
                        , Options.onClick JoinRoom
                        ]
                        [ text "Join a room" ]
                    ]
                ]
            ]
        ]


viewRoomSuccess : Model -> Html Msg
viewRoomSuccess model =
    Grid.grid []
        [ Grid.cell [ Grid.size Grid.All 6 ]
            [ Card.view []
                [ Card.title []
                    [ Card.head [] [ text ("Well done, " ++ model.userName ++ ". Your room was successfully created! Tell your friends. 🍻 Your room number is " ++ model.roomNumber ++ ".") ] ]
                ]
            ]
        , Grid.cell [ Grid.size Grid.All 6 ]
            [ Card.view []
                [ Card.title []
                    [ Card.head [] [ text "Enter your first development goal: " ] ]
                , Card.actions
                    []
                    [ Textfield.render Mdl
                        [ 1, 0 ]
                        model.mdl
                        [ Textfield.value model.nextGoal
                        , Textfield.error ("Please enter a goal")
                            |> Options.when (model.error == Just GoalError)
                        , Options.onInput UpdateNextGoal
                        , Textfield.textarea
                        ]
                        []
                    ]
                , Card.actions
                    []
                    [ Button.render Mdl
                        [ 1, 1 ]
                        model.mdl
                        [ Button.raised
                        , Button.colored
                        , Button.ripple
                        , Options.onClick CreateGoal
                        ]
                        [ text "Play poker" ]
                    ]
                ]
            ]
        , Grid.cell [ Grid.size Grid.All 6 ]
            [ Options.styled Html.h4
                [ Typography.headline ]
                [ text "Currently in room: " ]
            , Lists.ul
                []
                (List.map
                    (Lists.li []
                        << (\name ->
                                [ Lists.content []
                                    [ text name
                                    ]
                                ]
                           )
                        << .name
                    )
                    model.players
                )
            ]
        ]


viewJoinSuccess : Model -> Html Msg
viewJoinSuccess model =
    Card.view []
        [ Card.title []
            [ Card.head [] [ text ("Welcome, " ++ model.userName ++ ". You have successfully joined room " ++ model.roomNumber ++ ". 🍺 Wait for the dealer to determine a development goal.") ] ]
        ]


viewJoinFailure : Model -> Html Msg
viewJoinFailure model =
    Card.view []
        [ Card.title []
            [ Card.head [] [ text ("No such room. 😔") ] ]
        , Card.actions
            []
            [ Button.render Mdl
                [ 3, 0 ]
                model.mdl
                [ Button.raised
                , Button.colored
                , Button.ripple
                , Options.onClick GoHome
                ]
                [ text "Back to safety" ]
            ]
        ]


possibleVotes : List String
possibleVotes =
    [ "1", "2", "3", "5", "8", "13", "?", "☕" ]


viewVotingPage : Model -> Html Msg
viewVotingPage model =
    Grid.grid []
        ([ Grid.cell [ Grid.size Grid.All 6 ]
            [ Card.view []
                [ Card.title []
                    [ Card.head []
                        [ text model.currentGoal
                        ]
                    ]
                , Card.text []
                    [ text "Place your bet: " ]
                ]
            ]
         , Grid.cell [ Grid.size Grid.All 6 ] [] -- Filler
         ]
            ++ (List.indexedMap
                    (\index vote ->
                        Grid.cell [ Grid.size Grid.All 1 ]
                            [ Button.render Mdl
                                [ 4, index ]
                                model.mdl
                                [ Button.raised
                                , (if vote == model.currentVote then
                                    Button.colored
                                   else
                                    Button.accent
                                  )
                                , Button.ripple
                                , Options.onClick (UpdateVote vote)
                                ]
                                [ text vote ]
                            ]
                    )
                    possibleVotes
               )
            ++ [ Grid.cell [ Grid.size Grid.All 4 ]
                    []

               -- Filler
               , (if model.user == Dealer then
                    Grid.cell []
                        ([ Card.view []
                            [ Card.text []
                                [ text <|
                                    showVotingProgress model.players
                                ]
                            , Card.actions
                                []
                                (List.map
                                    renderPlayer
                                    model.players
                                )
                            ]
                         ]
                        )
                  else
                    Grid.cell [] []
                  -- Filler
                 )
               ]
        )


showVotingProgress : List Player -> String
showVotingProgress players =
    let
        remaining =
            votersRemaining players

        pluralize =
            remaining /= 1

        plural =
            if pluralize then
                "s"
            else
                ""
    in
        "Voting progress : "
            ++ (toString remaining)
            ++ " vote"
            ++ plural
            ++ " remaining"


votersRemaining : List Player -> Int
votersRemaining players =
    let
        playerCount =
            List.length players

        voteCount =
            List.length <| List.filter (((/=) Nothing) << .vote) players
    in
        playerCount - voteCount


summarizeVotes : List Player -> Html Msg
summarizeVotes players =
    let
        votes =
            List.filterMap .vote players

        toNumber vote =
            let
                number =
                    String.toInt vote
            in
                case number of
                    Ok n ->
                        Just n

                    Err _ ->
                        Nothing

        numericVotes =
            List.filterMap toNumber votes

        consensus =
            Set.size (Set.fromList votes) == 1

        sum =
            List.sum numericVotes |> toFloat

        count =
            List.length numericVotes |> toFloat

        mean =
            sum / count

        result =
            if consensus then
                "Everyone agrees 🎉 : it's " ++ (Maybe.withDefault "ERROR" (List.head votes))
            else
                "The mean is : " ++ toString (round mean)
    in
        text result


viewResultsPage : Model -> Html Msg
viewResultsPage model =
    Grid.grid []
        [ Grid.cell [ Grid.size Grid.All 6 ]
            [ Card.view []
                [ Card.title []
                    [ Card.head [] [ text <| "Goal : " ++ model.currentGoal ] ]
                ]
            ]
        , Grid.cell [ Grid.size Grid.All 6 ]
            [ Card.view []
                [ Card.title []
                    [ Card.head [] [ summarizeVotes model.players ] ]
                ]
            ]
        , Grid.cell [ Grid.size Grid.All 6 ]
            (if model.user == Dealer then
                (List.map
                    renderPlayerWithVote
                    model.players
                )
             else
                []
             -- Filler
            )
        , (if model.user == Dealer then
            Grid.cell [ Grid.size Grid.All 6 ]
                [ Card.view []
                    [ Card.title []
                        [ Card.head [] [ text "Enter your next development goal: " ] ]
                    , Card.actions
                        []
                        [ Textfield.render Mdl
                            [ 5, 0 ]
                            model.mdl
                            [ Textfield.value model.nextGoal
                            , Textfield.error ("Please enter a goal")
                                |> Options.when (model.error == Just GoalError)
                            , Options.onInput UpdateNextGoal
                            , Textfield.textarea
                            ]
                            []
                        ]
                    , Card.actions
                        []
                        [ Button.render Mdl
                            [ 5, 1 ]
                            model.mdl
                            [ Button.raised
                            , Button.colored
                            , Button.ripple
                            , Options.onClick CreateGoal
                            ]
                            [ text "Next round" ]
                        ]
                    ]
                ]
           else
            Grid.cell [] []
           -- Filler
          )
        ]


renderPlayer : Player -> Html Msg
renderPlayer player =
    (Lists.li []
        [ Lists.content []
            [ Lists.icon
                (case player.vote of
                    Just vote ->
                        "favorite"

                    Nothing ->
                        "favorite_border"
                )
                []
            , text player.name
            ]
        ]
    )


renderPlayerWithVote : Player -> Html Msg
renderPlayerWithVote player =
    let
        vote =
            (Maybe.withDefault "No vote" player.vote)
    in
        (Lists.li []
            [ Lists.content []
                [ text (player.name ++ " : " ++ vote)
                ]
            ]
        )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    WebSocket.listen model.webSocketHost ServerMsg
