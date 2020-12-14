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
import Html exposing (a)




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
    { turnCounter : Int
    , navigationPts : Int
    , discoveryPts : Int
    , perilPts : Int
    , adventurers : Dict String Adventurer
    , activeAdvName : String
    , activeMove : (Maybe Move, String)
    , previousMove : (Maybe Move, String)
    , segment : Maybe Segment
    , sessionLocationList : List Int
    }



type alias Adventurer =
    { name : String
    , canMove : Bool
    }



type alias Segment =
    { placeKind : SomePlace
    , placeText : Int
    , placeOpenings : Int
    , thingKind : SomeThing
    , thingText : Int
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



type SomeThing
    = Oddity
    | Obstacle
    | Threat


init : () -> (Model, Cmd Msg)
init _ =
    (initialModel, rollSessionLocations)



initialModel : Model
initialModel =
    { turnCounter = 0
    , navigationPts = 0
    , discoveryPts = 0
    , perilPts = 0
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
        { placeKind = Area
        , placeText = 0
        , placeOpenings = 0
        , thingKind = Oddity
        , thingText = 0
        }
    , sessionLocationList = []
    }




type Msg =
    InitLocations (List Int)
    | SetAction String
    | ActivateAdv Adventurer
    | Navigate
    | Search
    | Overcome
    | Rest
    | AskTheGM
    | DummyMsg
    | Confirm
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
                    if model.navigationPts < 3
                    then (doOrientate model, Cmd.none)
                    else (model, Cmd.none)

                Just DelveAhead ->
                    (doDelveAhead model, explorationRoll model)
            
                Just GoWatchfully ->
                    (doGoWatchfully model, explorationRoll model)
                
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
            (doExploration model nuSegment, Cmd.none)

        Navigate ->
            (model, Cmd.none)

        Search ->
            (model, Cmd.none)

        Overcome ->
            (model, Cmd.none)

        Rest ->
            (model, Cmd.none)

        AskTheGM ->
            (model, Cmd.none)

        DummyMsg ->
            (model, Cmd.none)
        
        {- EnemyTurn ->
            (model, Cmd.none) -}



initSessionLocations : Model -> (List Int) -> Model
initSessionLocations model list =
    {model | sessionLocationList = 0 :: list}




routineUpdates : Model -> Model
routineUpdates oldModel =
    let
        {- ifDelveTurn =
            if Tuple.first oldModel.previousMove == Just DelveAhead
            then 0
            else 1 -}
        
        ifDelveHasMoved =
            if Tuple.first oldModel.previousMove == Just DelveAhead
            then oldModel.adventurers
            else updateAdvCanMove oldModel

        nuModel : Model
        nuModel =
            { oldModel
            | turnCounter = oldModel.turnCounter + 1 --ifDelveTurn
            , adventurers = ifDelveHasMoved
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
    | navigationPts = model.navigationPts +1
    --, previousMove = (Just Orientate, "Orientate")
    } |> routineUpdates



doExploration : Model -> Segment-> Model
doExploration model nuSegment =
    { model 
    | segment = Just nuSegment
    , discoveryPts = discoveryUpdate model nuSegment
    , perilPts = perilUpdate model nuSegment
    , sessionLocationList = sessionLocationsUpdate model nuSegment
    } |> routineUpdates



doDelveAhead : Model -> Model
doDelveAhead model =
    { model
    | previousMove = (Just DelveAhead, "DelveAhead")
    }



doGoWatchfully : Model -> Model
doGoWatchfully model =
    { model
    | previousMove = (Just GoWatchfully, "Go Watchfully")
    }



doForage : Model -> Model
doForage model = 
    { model
    | previousMove = (Just Forage, "Forage")
    }


sessionLocationsUpdate : Model -> Segment -> List Int
sessionLocationsUpdate model nuSegment =
    if nuSegment.placeKind == Location then
        model.sessionLocationList |> List.drop 1
    else
        model.sessionLocationList



discoveryUpdate : Model -> Segment -> Int
discoveryUpdate model nuSegment =
    case Just nuSegment of
        Nothing -> 0
        Just segment ->
            case segment.placeKind of
               Location -> model.discoveryPts - model.discoveryPts
               Passage -> model.discoveryPts + 1
               Area -> model.discoveryPts + 1



perilUpdate : Model -> Segment -> Int
perilUpdate model nuSegment =
    case Just nuSegment of
        Nothing -> 0
        Just segment ->
            case segment.thingKind of
               Threat -> model.perilPts - model.perilPts
               Obstacle -> model.perilPts + 1
               Oddity -> model.perilPts + 1



rollSessionLocations : Cmd Msg
rollSessionLocations =
    List.range 1 6
    |> Random.List.shuffle
    |> Random.generate InitLocations



explorationRoll : Model -> Cmd Msg
explorationRoll model =
    Random.map5
            Segment
            (placeKindRoll model)
            basicRoll
            placeOpeningsRoll
            (thingKindRoll model)
            basicRoll
    |>
    Random.generate SegmentRolls



placeKindRoll : Model -> Random.Generator SomePlace
placeKindRoll model =
    let
        locationChance =
            if (model.sessionLocationList |> List.length) > 1 then
                (0 + model.discoveryPts |> toFloat)
            else
                0
    in 
    Random.weighted
        (3, Passage )
        [ (2, Area )
        , (locationChance , Location )
        ]



basicRoll : Random.Generator Int
basicRoll =
    Random.int 1 6



placeOpeningsRoll : Random.Generator Int
placeOpeningsRoll = Random.int 1 6



thingKindRoll : Model -> Random.Generator SomeThing
thingKindRoll model =
    let
        threatChance =
            1 + model.perilPts |> toFloat
    in 
    Random.weighted
        (3, Oddity )
        [ (2, Obstacle )
        , (threatChance , Threat )
        ]



segmentAccess : Model ->  Segment
segmentAccess model =
    case model.segment of
        Nothing ->
            { placeKind = Area
            , placeText = 0
            , placeOpenings = 0
            , thingKind = Oddity
            , thingText = 0
            }
        Just segment -> segment



somePlaceText : Model -> String
somePlaceText model =
    let
        mySegment : Segment
        mySegment = segmentAccess model  -- model.segment
        

        somePlace = -- model.segment.description    
            if mySegment.placeKind == Location
            then
                case (model.sessionLocationList |> List.head) of
                    Just n -> n
                    Nothing -> 0
            else mySegment.placeText

    in
    case mySegment.placeKind of

        Passage ->
            case somePlace of
               1 -> "an ascending passage"
               2 -> "a descending passage"
               3 -> "a twisting passage"
               4 -> "a forking passage"
               5 -> "an unstable passage"
               6 -> "an obstructed passage"
               _ -> "Passage Error"

        Area ->
            case somePlace of
               0 -> "a small space, mostly empty, currently safe"
               1 -> "a small area"
               2 -> "a big area"
               3 -> "a vast area"
               4 -> "a luxurious area"
               5 -> "a ruined area"
               6 -> "an eerie area"
               _ -> "Area Error"

        Location ->
            case somePlace of
               1 -> "a place that gives you a chance to get out"
               2 -> "a place that gives you a shot at the quest"
               3 -> "a place that holds a great treasure"
               4 -> "a place that reveals a great evil"
               5 -> "a place that presents beauty among ugliness"
               6 -> "a place that represents your deepest fear"
               _ -> "Location Error"



placeOpeningsText1 : Model -> String
placeOpeningsText1 model =
    let
        mySegment : Segment
        mySegment = segmentAccess model
    in
    case mySegment.placeOpenings of
        0 -> "NO other"
        1 -> "NO other"
        2 -> "ONE other"
        3 -> "ONE other"
        4 -> "TWO other"
        5 -> "TWO other"
        6 -> "MANY other"
        _ -> "Openings Error"



placeOpeningsText2 : Model -> String
placeOpeningsText2 model =
    let
        plural : Segment
        plural = segmentAccess model
    in
    "there seems to be "
    ++ placeOpeningsText1 model ++
    " obvious opening"
    ++ (if plural.placeOpenings > 3 then "s" else "") ++
    " in addition to the one from whence you came in"



someThingText : Model -> String
someThingText model =
    let
        mySegment : Segment
        mySegment = segmentAccess model  -- model.segment
        

        someThing = mySegment.thingText

    in
    case mySegment.thingKind of
    
        Oddity ->
            case someThing of
               0 -> "nothing much is going on here. For now."
               1 -> "alluring"
               2 -> "alarming"
               3 -> "out of place"
               4 -> "old"
               5 -> "new"
               6 -> "alive"
               _ -> "Oddity Error"

        Obstacle ->
            case someThing of
               1 -> "natural"
               2 -> "complex"
               3 -> "manufactured"
               4 -> "unnatural"
               5 -> "architectural"
               6 -> "alive"
               _ -> "Obstacle Error"

        Threat ->
            case someThing of
               1 -> "a trap"
               2 -> "a trap"
               3 -> "a dweller"
               4 -> "a dweller"
               5 -> "that is part of the environment itself"
               6 -> "that is part of the environment itself"
               _ -> "Threat Error"



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
                , basicInstructionsRow model
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
                [ scoresRow model
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
        [ otherButtons DummyMsg "Adventurer Sheets"
        , otherButtons DummyMsg "Rules Summary"
        , otherButtons DummyMsg "Save & Exit"
        ]



basicInstructionsRow : Model -> Element msg
basicInstructionsRow model =
    row
        [ spacing 10
        , padding 10
        ]

        [ column
            [ width (fillPortion 5)
            , spacing 10
            ]
            [ paragraph
                []
                [ el [Font.bold] (text "1. ")
                , text "All Players have a free flowing conversation about what their Adventurers say and do."
                ]
            , paragraph
                []
                [text "Only describe what "
                , el [Font.bold] (text "your")
                , text " Adventurer says and does."
                ]
            , paragraph
                []
                [ text " When this description matches a Move, then "
                , el [Font.bold] (text "do the Move.")
                ]
            ]

        , column
            [ Border.width 2
            , Border.rounded 10
            , Border.color (rgb255 0 100 0)
            , Background.color (rgb255 0 250 250)
            , padding 10
            , spacing 5
            , width (fillPortion 1)
            ]
            [ el [Font.bold, centerX] (text "Delve Stats")
            , text ("Discovery : " ++ (model.discoveryPts |> String.fromInt) )
            , text ("Peril : " ++ (model.perilPts |> String.fromInt) )
            , text ("Navigation : " ++ (model.navigationPts |> String.fromInt) )
            ]
        ]



advSelectionRow : Model -> Element Msg
advSelectionRow model =
    row
        [ width fill, spacing 30 ]
        [ column
            [ width fill
            , height fill
            , spacing 10
            ]
            [ paragraph
                []
                [ el [Font.bold] (text "2. ")
                , text "Who is making a Move?"
                ]
            , el [Font.italic] (text "(select just one)")
            , wrappedRow
            [spacing 10]
            ( Dict.map advButtons model.adventurers
            |> Dict.values
            )
            ]
        
        {- , column
            [ width (fillPortion 2), spacing 15 ]
            ( Dict.map advButtons model.adventurers
            |> Dict.values
            ) -}
        ]

    

moveSelectionRow : Model -> Element Msg
moveSelectionRow model =
    column
        [width fill
        , spacing 10
        ]
        [ paragraph
                    []
                    [ el [Font.bold] (text "3. ")
                    , text "Which Move is being made?"
                    ]

        , wrappedRow
            [spacing 10]
            [ otherButtons Navigate "When you NAVIGATE the current area."
            , otherButtons Search "When you act to SEARCH the current area."
            , otherButtons Overcome "When you try to OVERCOME an obstacle or challange."
            , otherButtons Rest "When you let time PASS to get respite or get ready."
            , otherButtons AskTheGM "When you ASK a question about the situation."
            ]

        , row
            [ padding 10
            , spacing 30
            , width fill
            ]
            [ column -- Navigate
                [ width (fillPortion 1)
                , spacing 10
                , height fill
                ]
                [ el
                    [centerX
                    , Font.bold
                    , padding 10]
                    (text "NAVIGATE")

                , column []
                    [ paragraph
                        ( stdColumn ++ [ Font.bold, Font.italic] )
                        [ moveButtons model (Orientate, "Orientate") ]
                    , paragraph stdColumn
                        [ text "When you spend time consulting your maps and making sense of the area’s layout..."]
                    ]
                , column []
                    [ paragraph
                        ( stdColumn ++ [ Font.bold, Font.italic] )
                        [ moveButtons model (DelveAhead, "Delve Ahead") ]
                    , paragraph stdColumn
                        [ text "When you step into a new section hastily, carelessly or blindly..." ]
                    ]
                , column []
                    [ paragraph
                        ( stdColumn ++ [ Font.bold, Font.italic] )
                        [ moveButtons model (GoWatchfully, "Go Watchfully") ]
                    , paragraph stdColumn
                        [ text "When you step into a new section slowly and carefully..." ]
                    ]
                ]
            
            , column -- Search
                [ width (fillPortion 1)
                , spacing 10
                , height fill
                ]
                [ el
                    [centerX
                    , Font.bold
                    , padding 10] (text "SEARCH" )

                , column []
                    [ paragraph (stdColumn ++ [Font.bold, Font.italic])
                        [ moveButtons model (Forage, "Forage")
                        ]
                    , paragraph stdColumn
                        [ text "When you spend time looking around to find something you need..."]
                    ]

                , column []
                    [ paragraph (stdColumn ++ [Font.bold, Font.italic])
                        [ moveButtons model (Prod, "Prod")
                        ]
                    , paragraph stdColumn
                        [ text "When you manipulate or get very close to something specific in the current segment..." ]
                    ]

                , column []
                    [ paragraph (stdColumn ++ [Font.bold, Font.italic])
                        [ moveButtons model (Inspect, "Inspect")
                        ]
                    , paragraph stdColumn
                        [ text "When you investigate something specific within the current segment with great caution or from a moderate distance..." ]
                    ]
                ]

            , column -- Overcome
                [ width (fillPortion 1)
                , spacing 10
                , height fill
                --, Background.color (rgb255 255 0 0)
                ]
                [ el
                    [centerX
                    , Font.bold
                    , padding 10] (text "OVEWRCOME" )

                , column []
                    [ paragraph (stdColumn ++ [Font.bold, Font.italic])
                    [ moveButtons model (TakeaRisk, "Take a Risk")
                    ]
                , paragraph stdColumn
                    [ text "When you try to overcome an obstacle with your own direct actions..."]
                
                ]

                , column []
                    [ paragraph (stdColumn ++ [Font.bold, Font.italic])
                    [ moveButtons model (UseIngenuity, "Use Ingenuity")
                    ]
                , paragraph stdColumn
                    [ text "When you try to build, repair, craft or produce a thing..." ]
                
                ]

                , column []
                    [ paragraph (stdColumn ++ [Font.bold, Font.italic])
                    [ moveButtons model (Fight, "Fight")
                    ]
                , paragraph stdColumn
                    [ text "When you fight a dangerous opponent..." ]
                    ]
                ]
            ]
        ]
    

scoresRow : Model -> Element Msg
scoresRow model = 
    row
        [ width fill
        ]
        [ column
            [ width (fillPortion 2)
            , padding 10
            ]
            [ column [] (bearingsText model)
            ]
        , column
            [ width (fillPortion 1)
            , padding 10
            , alignTop
            ]
            [ text ("This is Turn " ++ (String.fromInt model.turnCounter) )
            , el [] (text "")
            , text ("Discovery rating : " ++ (String.fromInt model.discoveryPts) )
            , el [] (text "")
            , text ("Peril rating : " ++ (String.fromInt model.perilPts) )
            ]
        ]



sitchRow : Model -> Element Msg
sitchRow model =
    row
        [ centerX
        , padding 10
        , spacing 10
        , Background.color (rgb255 211 211 211)
        ]
        [ column
        -- left side container
            (stdColumn ++ 
            [ width (fillPortion 2)
            , spacing 50
            ]
            )

            [ somePlaceBox model
            , someThingBox model 
            , someMoveBox model
            ]

        -- right side container
        , column
            (stdColumn ++
            [ width (fillPortion 1)
            ]
            )
            
            (advPromptBox model)
        ]




somePlaceBox : Model -> Element msg
somePlaceBox model =
    column
        [spacing 10
        , width fill]
        [ paragraph
            [ Background.color (rgb255 211 211 211)
            , padding 5
            ]
            [text ("You find yourself in...") ]

        , column
            []
            [ bulletListBuilder (somePlaceText model)
            , bulletListBuilder (placeOpeningsText2 model)
            ]
        ]



someThingBox : Model -> Element msg
someThingBox model =
    let
        previousMove = Tuple.first model.previousMove
        myThingText = someThingText model
    in
        column
            [spacing 10
            , width fill]
            [ paragraph
                [ Background.color (rgb255 211 211 211)
                , padding 5
                ]
                [text (
                    if
                        (previousMove == Just DelveAhead) || (previousMove == Just GoWatchfully)
                    then
                        "Here you face something clearly " ++
                            case (segmentAccess model).thingKind of
                                Oddity -> "NOTICEABE because of how it is..."
                                Obstacle -> "PROBLEMATIC that lays in your way. It is something..."
                                Threat -> "DANGEROUS..."
                    else
                        "From what you can tell..."
                    )
                ]

            , column
                []
                [ bulletListBuilder myThingText
                , if
                        (segmentAccess model).thingKind == Threat
                    then if
                        (Tuple.first model.previousMove == Just DelveAhead)
                        then
                            bulletListBuilder ("the threat is ALREADY aware of your presence!")
                        else
                            bulletListBuilder ("the threat is NOT aware of your presence, for now.")
                    else text ""
                , if
                        (segmentAccess model).thingKind == Threat
                    then if
                        (Tuple.first model.previousMove == Just DelveAhead)
                        then 
                            bulletListBuilder ("it takes action before you can react: how?")
                        else 
                            bulletListBuilder ("what is it doing?")
                    else text ""
                ]
            ]

someMoveBox : Model-> Element msg
someMoveBox model =
    let
        content =
            case Tuple.first model.previousMove of
                Just Orientate -> ""

                Just DelveAhead -> ""
            
                Just GoWatchfully -> ""
                
                Just Forage -> "Your foraging reveals..."
                    
                Just Prod -> ""
                Just Inspect -> ""
                Just TakeaRisk -> ""
                Just UseIngenuity -> ""
                Just Fight -> ""
                
                Just EnemyMove -> ""

                _-> "some move error"
    in
    column
        [spacing 10
        , width fill]
        [ paragraph
            [ Background.color (rgb255 211 211 211)
            , padding 5
            ]
            [text ( content ) ]

        , column
            []
            [ bulletListBuilder (somePlaceText model)
            , bulletListBuilder (placeOpeningsText2 model)
            ]
        ]


advPromptBox : Model -> List (Element Msg)
advPromptBox model =
    

        [ paragraph
            []
            [ el [Font.bold] (text model.activeAdvName )
            , text ( " is about to ")
            , el [Font.bold] (text <| Tuple.second model.activeMove )
            , text ( "." )
            ]
        
        , column
            [spacing 30]
            (playerPrompt model)
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
        then playerTaskText model
        else [paragraph [][text "Select an Adventurer."]]
    
    else if model.activeMove == (Nothing,"do something")
    then [paragraph [][text "Select a Move."]]

    else playerTaskText model
        
        



playerTaskText : Model -> List (Element Msg)
playerTaskText model =
    let
        content =
            case Tuple.first model.activeMove of

                Just Orientate ->
                    [ "Say where you think you should go next, and explain why you think so."
                    , ( "You get your bearings. (max. "
                        ++
                        ( 3 - model.navigationPts |> String.fromInt )
                        ++
                        " times)"
                        )
                    ]
                
                Just DelveAhead ->
                    [ "You will be already INSIDE the new section!"
                    , "Others can be with you, if they want, but they'll share the risks."
                    , "Delving Ahead is riskier than Going Watchfully..."
                    , "...but you can perform one extra Move."
                    ]

                Just GoWatchfully ->
                    [ "You will be just OUTSIDE or already INSIDE the new section, your choice"
                    , "Others can be with you, if they want, but they'll share the risks."
                    , "Going Watchfully is safer than Delving Ahead."
                    ]
                
                Just Forage ->
                    [ "Say WHY it makes sense that such a thing would be available here."
                    , "If no one objects, you can Confirm this Move."
                    , "more Discovery = more stuff to find"
                    , "more Peril = more danger"
                    ]
                
                Just Prod ->
                    [ "...say exactly HOW you do it."
                    , "Say also what you are afraid could go wrong."
                    ]
                
                Just Inspect ->
                    [ "...say exactly HOW you do it"
                    , "Say also what you are afraid could go wrong."
                    ]

                Just TakeaRisk ->
                    [ "...say exactly HOW you do it."
                    , "Check if Traits or Help apply and spend them if you want."
                    ]
                
                Just UseIngenuity ->
                    [ "...the Enemy will tell you which Materials, Tools and Knowledges are required and how much Time is needed."
                    , "If you have everything ready at hand, say HOW you do it, then it is done."
                    ]
                
                Just Fight ->
                    [ "...you are already INSIDE the new section!"
                    , "Others can be with you, if they want, but they'll share the risks."
                    ]
                
                Just EnemyMove ->
                    [ "Next, the Enemy will take their turn."
                    , "Click the button when you are ready!"
                    ]

                Nothing ->
                    ["",""]
    in
        playerTaskStructure model content



playerTaskStructure : Model -> List String -> List (Element Msg)
playerTaskStructure model taskList =
    let
        color =  Background.color (rgb255 211 211 211)
        list =
            List.map
            (\string ->
                paragraph
                    [ color
                    , padding 5
                    ]
                    [ text string ]
            ) taskList
    in
        List.append list [buttonSwitch model]
    



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
            if model.navigationPts == 0
            then "You are kinda lost."
            else "You've got your bearings!"
        navQuality =
            if model.navigationPts == 0
            then "not"
            else if model.navigationPts == 1
            then "slightly"
            else if model.navigationPts == 2
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
            , label = paragraph [] [el [] (text name)]
            }
        )