module Main exposing (..)

import Browser
import Html exposing (Html)
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Element.Border as Border
import Array exposing (Array)
import Html exposing (div)
import Maybe exposing (Maybe)
import Dict exposing (Dict)
import Html.Attributes exposing (name)
import Random exposing (generate)
import Random.List exposing (shuffle)
import Task
import Maybe exposing (andThen)
import Html.Attributes exposing (kind)
import Dict
import Tuple3 exposing (third)
import Element.Region exposing (description)
import Maybe exposing (withDefault)



-- MAIN
main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none






-- MODEL
type alias Model =
    { roundCounter : Int
    , navPoints : Int
    , discoveryPoints : Int
    , adventurers : Dict String Adventurer
    , activeAdvName : String
    , activeMove : (Maybe Move, String)
    , previousMove : (Maybe Move, String)
    , segment : Maybe Segment
    , sessionLocations : List Int
    , testLocation : Int
    }

type alias Adventurer =
    { name : String
    , canMove : Bool
    }

type alias Segment =
    { kind : SomePlace
    , description : Int
    , openings : Int
    }

type Move
    = Orientate
    | DelveAhead
    | GoWatchfully
    | Forage
    | Prod
    | Inspect
    | TakeaRisk
    | UseIngenuity
    | Fight
    | EnemyMove

type SomePlace
    = Passage
    | Area
    | Location



init : () -> (Model, Cmd Msg)
init _ =
    (initialModel, rollSessionLocations)



initialModel : Model
initialModel =
    { roundCounter = 0
    , navPoints = 0
    , discoveryPoints = 0
    , adventurers =
        Dict.fromList
            [ ("The Nameless One", Adventurer "The Nameless One" True)
            , ("Fake Player", Adventurer "Fake Player" True)
            , ("Sir Placeholder", Adventurer "Sir Placeholder" True)
            ]
    , activeAdvName = "Someone"
    , activeMove = (Nothing, "do something")
    , previousMove = (Nothing, "do something")
    , segment = Just 
        { kind = Area
        , description = 0
        , openings = 0
        }
    , sessionLocations = []
    , testLocation = 0
    }




type Msg =
    InitLocations (List Int)
    | SetAction String
    | ActivateAdv Adventurer
    | MenuAction
    | Confirm
    --| EnemyTurn
    | SetMove (Move, String)
    | SegmentRolls Segment






-- UPDATE
update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
    case msg of
        InitLocations list ->
            (initSessionLocations model list, Cmd.none)

        SetAction "Navigate" ->
            (model, Cmd.none)
        SetAction "Search" ->
            (model, Cmd.none)
        SetAction "SolveProblems" ->
            (model, Cmd.none)
        SetAction _ ->
            (model, Cmd.none)
        ActivateAdv adv ->
            case adv.canMove of
                True -> ({model | activeAdvName = adv.name } , Cmd.none)
                False -> ({model | activeAdvName = "Someone"} , Cmd.none)
        
        SetMove (move, name) ->
            let
                allHaveMoved =
                    Dict.values model.adventurers
                    |> List.all (\adv -> adv.canMove == False)
            in
            if allHaveMoved then
                (model, Cmd.none)
            else
            ({model | activeMove = (Just move, name)} , Cmd.none)
        
        Confirm  ->
            case Tuple.first model.activeMove of

                Just Orientate -> -- Move type, not Msg type
                    if model.navPoints < 3
                    then (doOrientate model, Cmd.none)
                    else (model, Cmd.none)

                Just DelveAhead ->
                    (model, rollDelveAhead model)
            
                Just GoWatchfully ->
                    (model, Cmd.none)
                Just Forage ->
                    (model, Cmd.none)
                Just Prod ->
                    (model, Cmd.none)
                Just Inspect ->
                    (model, Cmd.none)
                Just TakeaRisk ->
                    (model, Cmd.none)
                Just UseIngenuity ->
                    (model, Cmd.none)
                Just Fight ->
                    (model, Cmd.none)
                
                Just EnemyMove ->
                    (doEnemyMove model, Cmd.none)
                
                Nothing ->
                    (model, Cmd.none)

        SegmentRolls nuSegment ->
            (doDelveAhead model nuSegment, Cmd.none)
            
        MenuAction ->
            (model, Cmd.none)
        
        {- EnemyTurn ->
            (model, Cmd.none) -}



initSessionLocations : Model -> (List Int) -> Model
initSessionLocations model list =
    {model | sessionLocations = list}



routineUpdates : Model -> Model
routineUpdates model =
    let
        nuModel : Model
        nuModel =
            {model
            | roundCounter = model.roundCounter +1
            , adventurers = updateAdvCanMove model
            , activeAdvName = initialModel.activeAdvName
            , activeMove = initialModel.activeMove
            }
    in
    if
        Dict.values nuModel.adventurers
        |> List.all (\adv -> adv.canMove == False)
    then
        { nuModel| activeMove = (Just EnemyMove, "perforn an Enemy Move") }
    else
        nuModel



toggleCanMove : Adventurer -> Adventurer
toggleCanMove adventurer =
  { adventurer | canMove = not adventurer.canMove }

  

updateAdvCanMove : Model -> Dict String Adventurer
updateAdvCanMove model =
    let
        allHaveMoved =
            Dict.values model.adventurers
            |> List.all (\adv -> adv.canMove == False)
    in
    if allHaveMoved then
         Dict.map (always toggleCanMove) model.adventurers
    else
        Dict.update
            model.activeAdvName
            (\adv -> Maybe.map toggleCanMove adv)
            model.adventurers



doOrientate : Model -> Model
doOrientate model =
    { model 
    | navPoints = model.navPoints +1
    , previousMove = (Just Orientate, "Orientate")
    } |> routineUpdates



doDelveAhead : Model -> Segment-> Model
doDelveAhead model nuSegment =
    { model 
    | segment = Just nuSegment
    , discoveryPoints = discoveryUpdate model nuSegment
    , previousMove = (Just DelveAhead, "DelveAhead")
    , sessionLocations = sessionLocationsUpdate model nuSegment
    , testLocation = justAtest model
    } |> routineUpdates


justAtest : Model -> Int
justAtest model = 
    let
        head = model.sessionLocations |> List.head
    in
        case head of
            Nothing -> 0
            Just n -> n

sessionLocationsUpdate : Model -> Segment -> List Int
sessionLocationsUpdate model nuSegment =
    if nuSegment.kind == Location then
        model.sessionLocations |> List.drop 1
    else
        model.sessionLocations



discoveryUpdate : Model -> Segment -> Int
discoveryUpdate model nuSegment =
    case Just nuSegment of
        Nothing -> 0
        Just segment ->
            case segment.kind of
               Location -> model.discoveryPoints - model.discoveryPoints
               Passage -> model.discoveryPoints + 1
               Area -> model.discoveryPoints + 1





rollSessionLocations : Cmd Msg
rollSessionLocations =
    List.range 1 6
    |> Random.List.shuffle
    |> Random.generate InitLocations


rollDelveAhead : Model -> Cmd Msg
rollDelveAhead model =
    Random.map3
            Segment
            (kindRoll model)
            descriptionRoll
            openingsRoll
    |>
    Random.generate SegmentRolls



kindRoll : Model -> Random.Generator SomePlace
kindRoll model =
    let
        locationChance =
            if (model.sessionLocations |> List.length) > 0 then
                (0 + model.discoveryPoints |> toFloat)
            else
                0
    in 
    Random.weighted
        (3, Passage )
        [ (2, Area )
        , (locationChance , Location )
        ]



descriptionRoll : Random.Generator Int
descriptionRoll =
    Random.int 1 6



openingsRoll : Random.Generator Int
openingsRoll = Random.int 1 6



segmentAccess : Model ->  Segment
segmentAccess model =
    case model.segment of
        Nothing ->
            { kind = Area
            , description = 0
            , openings = 0
            }
        Just segment -> segment



segmentText : Model -> String
segmentText model =
    let
        mySegment : Segment
        mySegment = segmentAccess model  -- model.segment
        

        description = -- model.segment.description    
            if mySegment.kind == Location
            then
                case (model.sessionLocations |> List.head) of
                    Just n -> n
                    Nothing -> 0
            else mySegment.description
        

    in
    case mySegment.kind of
        Passage ->
            case description of
               1 -> "an ascending passage"
               2 -> "a descending passage"
               3 -> "a twisting passage"
               4 -> "a forking passage"
               5 -> "an unstable passage"
               6 -> "an obstructed passage"
               _ -> "Passage Error"
        Area ->
            case description of
               0 -> "a small space, mostly empty, currently safe"
               1 -> "a small area"
               2 -> "a big area"
               3 -> "a vast area"
               4 -> "a luxurious area"
               5 -> "a ruined area"
               6 -> "an eerie area"
               _ -> "Area Error"
        Location ->
            case description of
               1 -> "a chance to get out"
               2 -> "a shot at the quest"
               3 -> "a great treasure"
               4 -> "a brush with evil"
               5 -> "?a"
               6 -> "?b"
               _ -> "Location Error"





openingsText1 : Model -> String
openingsText1 model =
    let
        mySegment : Segment
        mySegment = segmentAccess model
    in
    case mySegment.openings of
        0 -> "NO other"
        1 -> "NO other"
        2 -> "ONE other"
        3 -> "ONE other"
        4 -> "TWO other"
        5 -> "TWO other"
        6 -> "MANY other"
        _ -> "Openings Error"



openingsText2 : Model -> String
openingsText2 model =
    let
        plural : Segment
        plural = segmentAccess model
    in
    "there seems to be "
    ++ openingsText1 model ++
    " obvious opening"
    ++ (if plural.openings > 3 then "s" else "") ++
    " in addition to the one from whence you came in"



doEnemyMove : Model -> Model
doEnemyMove model = 
    { model
    | adventurers = updateAdvCanMove model
    , activeMove = initialModel.activeMove
    }




















-- VIEW
view : Model -> Html Msg
view model =
    Element.layout
        [ Background.color (rgb255 220 220 220) ]
        ( row [centerX]
            [ column
                [ centerX
                , Border.color (rgb255 0 0 0)
                , Border.width 1
                , padding 10
                , spacing 30
                , width fill
                ]
                [ menuRow
                , basicInstructionsRow
                , advSelectionRow model
                , moveSelectionRow model
                ]
            , column
                [ centerX
                , Border.color (rgb255 0 0 0)
                , Border.width 1
                , padding 10
                , spacing 30
                , width fill
                , height fill
                ]
                [ statsRow model
                , sitchRow model
                ]
            ]
        )



stdColumn : List (Attribute msg)
stdColumn  =
    [ width (fillPortion 1)
    , padding 10
    , spacing 10
    , Background.color (rgb255 180 180 180)
    , height fill
    , Font.justify
    ]



menuRow : Element Msg
menuRow =
    row
        [ spacing 10
        , centerX
        ]
        [ otherButtons MenuAction "Adventurer Sheets"
        , otherButtons MenuAction "Rules Summary"
        , otherButtons MenuAction "Save & Exit"
        ]



basicInstructionsRow : Element msg
basicInstructionsRow =
    row
        []
        [ paragraph
            []
            [ el [Font.bold] (text "1. ")
            , text "Freely describe what your Adventurer is doing. When this description matches a Move, do the Move." ]
        ]



advSelectionRow : Model -> Element Msg
advSelectionRow model =
    row
        [ width fill, spacing 30 ]
        [ column
            [ width (fillPortion 1), height fill]
            [ paragraph
                []
                [ el [Font.bold] (text "2. ")
                , text "Who is making a Move?"
                ]
            , el [Font.italic, centerX] (text "(select just one)")
            ]
        , column
            [ width (fillPortion 2), spacing 15 ]
            ( Dict.map advButtons model.adventurers
            |> Dict.values
            )
        ]

    

moveSelectionRow : Model -> Element Msg
moveSelectionRow model =
    column
        []
        [ paragraph
                    []
                    [ el [Font.bold] (text "3. ")
                    , text "Which Move is being made?"]
    
        , row
            [ padding 10
            , spacing 30
            , width fill
            ]
            [ column [width (fillPortion 1)]
                [ el
                    [centerX
                    , Font.bold
                    , padding 10] (text "NAVIGATE" )

                , paragraph ( stdColumn ++ 
                    [ Font.bold
                    , Font.italic ] )
                    [ moveButtons model (Orientate, "Orientate")
                    ]
                , paragraph stdColumn
                    [ text "When you spend time consulting your maps and making sense of the area’s layout..."]
                
                , text "|"
                

                , paragraph (stdColumn ++ [Font.bold, Font.italic])
                    [moveButtons model (DelveAhead, "Delve Ahead")]
                , paragraph stdColumn
                    [ text "When you step into a new section hastily, carelessly or blindly..." ]
                
                , text "|"

                , paragraph (stdColumn ++ [Font.bold, Font.italic])
                    [moveButtons model (GoWatchfully, "Go Watchfully")]
                , paragraph stdColumn
                    [ text "When you step into a new section slowly and carefully..." ]
                
                ]

            , column [width (fillPortion 1)]
                [ el
                    [centerX
                    , Font.bold
                    , padding 10] (text "SEARCH" )

                , paragraph (stdColumn ++ [Font.bold, Font.italic])
                    [ moveButtons model (Forage, "Forage")
                    ]
                , paragraph stdColumn
                    [ text "When you spend time looking around to find something you need..."]
                
                , text "|"

                , paragraph (stdColumn ++ [Font.bold, Font.italic])
                    [ moveButtons model (Prod, "Prod")
                    ]
                , paragraph stdColumn
                    [ text "When you manipulate or get very close to something specific in the current segment..." ]
                
                , text "|"

                , paragraph (stdColumn ++ [Font.bold, Font.italic])
                    [ moveButtons model (Inspect, "Inspect")
                    ]
                , paragraph stdColumn
                    [ text "When you investigate something specific within the current segment with great caution or from a moderate distance..." ]
                ]

            , column [width (fillPortion 1)]
                [ el
                    [centerX
                    , Font.bold
                    , padding 10] (text "OVEWRCOME" )

                , paragraph (stdColumn ++ [Font.bold, Font.italic])
                    [ moveButtons model (TakeaRisk, "Take a Risk")
                    ]
                , paragraph stdColumn
                    [ text "When you try to overcome an obstacle with your own direct actions..."]
                
                , text "|"

                , paragraph (stdColumn ++ [Font.bold, Font.italic])
                    [ moveButtons model (UseIngenuity, "Use Ingenuity")
                    ]
                , paragraph stdColumn
                    [ text "When you try to build, repair, craft or produce a thing..." ]
                
                , text "|"

                , paragraph (stdColumn ++ [Font.bold, Font.italic])
                    [ moveButtons model (Fight, "Fight")
                    ]
                , paragraph stdColumn
                    [ text "When you fight a dangerous opponent..." ]
                ]
            ]
        ]



statsRow : Model -> Element Msg
statsRow model = 
    row
        [ width fill
        --, Background.color (rgb255 150 150 150)
        ]
        [ column
            [ width (fillPortion 2)
            , padding 10
            --, Background.color (rgb255 100 100 100) 
            ]
            [ column [] (bearingsText model)
            ]
        , column
            [ width (fillPortion 1)
            , padding 10
            , alignTop
            ]
            [ text ("This is Round " ++ (String.fromInt model.roundCounter) )
            , el [] (text "")
            , text ("Discovery rating : " ++ (String.fromInt model.discoveryPoints) )
            ]
        ]



sitchRow : Model -> Element Msg
sitchRow model =
    let
        advName =
            model.activeAdvName
        activeMove =
            Tuple.second model.activeMove
        previousMove =
            Tuple.first model.previousMove
        mySegment = segmentText model
        myOpenings = openingsText2 model
    in
    row
        [ centerX
        , padding 10
        , spacing 30
        , Background.color (rgb255 211 211 211)
        ]
        [ column (stdColumn ++ [width (fillPortion 2) ] )
            [ paragraph
                [ Background.color (rgb255 211 211 211)
                , padding 5
                ]
                [text (
                    if previousMove == Just DelveAhead then "You find yourself in..."
                    else "Current section elements:"
                ) ]

            , column
                []
                [ bulletListBuilder mySegment
                , bulletListBuilder myOpenings
                ]
            , el [] (text "")
            , paragraph
                [ Background.color (rgb255 211 211 211)
                , padding 5]
                [text "You face"]

            , column
                []
                [bulletListBuilder mySegment]
            ]

        , column (stdColumn ++ [width (fillPortion 1) ] )
            [ paragraph
                []
                [ el [Font.bold] (text advName)
                , text ( " is about to ")
                , el [Font.bold] (text activeMove)
                , text ( "." )
                ]
            , el [] (text "")
            , column
                [spacing 10]
                (playerPrompt model)
            ]
        ]



bulletListBuilder : String -> Element msg
bulletListBuilder myList =
    el [] ( paragraph [padding 5, spacing 5] [text ("+ " ++ myList) ] )



playerPrompt : Model -> List (Element Msg)
playerPrompt model =
    if model.activeAdvName == "Someone" && model.activeMove == (Nothing, "do something")
    then
        [paragraph [][text "Select an Adventurer and a Move."]]
    
    else if model.activeAdvName == "Someone"
    then
        if Tuple.first model.activeMove == Just EnemyMove
        then playerTasks model
        else [paragraph [][text "Select an Adventurer."]]
    
    else if model.activeMove == (Nothing,"do something")
    then [paragraph [][text "Select a Move."]]

    else playerTasks model



playerTasks : Model -> List (Element Msg)
playerTasks model =
    let
        switch =
            case Tuple.first model.activeMove of

                Just Orientate ->
                    ( "...say where you think you should go next, and explain why you think so."
                    , ( "You get your bearings. (max. "
                        ++
                        ( 3 - model.navPoints |> String.fromInt )
                        ++
                        " times)"
                        )
                    )
                
                Just DelveAhead ->
                    ( "...you are already INSIDE the new section!"
                    , "Others can be with you, if they want, but they'll share the risks."
                    )

                Just GoWatchfully ->
                    ( "...you are just OUTSIDE or already INSIDE the new section, your choice"
                    , "Others can be with you, if they want, but they'll share the risks."
                    )
                
                Just Forage ->
                    ( "...say WHY it makes sense that such a thing would be available here."
                    , "If anyone objects, you can't make this Move."
                    )
                
                Just Prod ->
                    ( "...say exactly HOW you do it."
                    , "Say also what you are afraid could go wrong."
                    )
                
                Just Inspect ->
                    ( "...say exactly HOW you do it"
                    , "Say also what you are afraid could go wrong."
                    )

                Just TakeaRisk ->
                    ( "...say exactly HOW you do it."
                    , "Check if Traits or Help apply and spend them if you want."
                    )
                
                Just UseIngenuity ->
                    ( "...the Enemy will tell you which Materials, Tools and Knowledges are required and how much Time is needed."
                    , "If you have everything ready at hand, say HOW you do it, then it is done."
                    )
                
                Just Fight ->
                    ( "...you are already INSIDE the new section!"
                    , "Others can be with you, if they want, but they'll share the risks."
                    )
                
                Just EnemyMove ->
                    ( "Next, the Enemy will take their turn."
                    , "Click the button when you are ready!"
                    )

                Nothing ->
                    ("","")
    in
        playerTaskStructure model (Tuple.first switch) (Tuple.second switch)



playerTaskStructure : Model -> String -> String -> List (Element Msg)
playerTaskStructure model playerTaskOne playerTaskTwo =
    [el [] 
        ( paragraph
            [ Background.color (rgb255 200 200 200)
            , padding 5]
            [ text playerTaskOne ]
        )
    , el [] (text "")
    , el [] 
        (paragraph
            [ Background.color (rgb255 200 200 200)
            , padding 5]
            [ text playerTaskTwo ]
        )
    , el [] (text "")
    , --otherButtons Confirm "Confirm?"
    buttonSwitch model
    ]



buttonSwitch : Model -> Element Msg
buttonSwitch model =
    if
        Dict.values model.adventurers
        |> List.all (\adv -> adv.canMove == False)
    then
        otherButtons Confirm "Enemy Turn"
    else
        otherButtons Confirm "Confirm?"



bearingsText : Model -> List (Element msg)
bearingsText model = 
    let
        navStatus =
            if model.navPoints == 0
            then "You are kinda lost."
            else "You've got your bearings!"
        navQuality =
            if model.navPoints == 0
            then "not"
            else if model.navPoints == 1
            then "slightly"
            else if model.navPoints == 2
            then "moderately"
            else "greatly"
    in
        [ paragraph []
            [ text navStatus ]
        , paragraph []
            [ text ( "The next Delve or Go Watchfully will be " ++ navQuality ++ " improved." ) ]
        ]



advButtons : String -> Adventurer -> Element Msg
advButtons key adv =
    el
        [ Border.solid
        , Border.color (rgb255 0 0 0)
        , Border.width 1
        , Border.rounded 10
        , padding 5
        , advButtonOverColor adv
        ]
        (Input.button []
            { onPress = Just <| ActivateAdv adv
            , label = text adv.name
            }
        )



advButtonOverColor : Adventurer -> Attribute msg
advButtonOverColor adv =
    if adv.canMove == False
    then mouseOver [ Background.color (rgb255 255 0 0) ]
    else mouseOver [ Background.color (rgb255 0 255 0) ]



moveButtons : Model -> (Move, String) -> Element Msg
moveButtons model (move, label) =
    el
        [ Border.solid
        , Border.color (rgb255 0 0 0)
        , Border.width 1
        , Border.rounded 10
        , padding 5
        , centerX
        , mouseOver [ moveButtonsMouseover model ]
        ]
        (Input.button []
            { onPress = Just (SetMove (move, label) )
            , label = text label
            }
        )



moveButtonsMouseover : Model -> Attr decorative msg
moveButtonsMouseover model =
    let
        allHaveMoved =
            Dict.values model.adventurers
            |> List.all (\adv -> adv.canMove == False)
    in
        if allHaveMoved then
            Background.color (rgb255 255 0 0)
        else
            Background.color (rgb255 0 255 0)



otherButtons : Msg -> String -> Element Msg
otherButtons msg name =
    el
        [ Border.solid
        , Border.color (rgb255 0 0 0)
        , Border.width 1
        , Border.rounded 10
        , padding 5
        , centerX
        , mouseOver [ Background.color (rgb255 0 255 0) ]
        ]
        (Input.button []
            { onPress = Just msg
            , label = text name
            }
        )