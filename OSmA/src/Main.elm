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
import Task
import Maybe exposing (andThen)


-- MAIN
main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL
type alias Model =
    { roundCounter : Int
    , navPoints : Int
    , discoveryPoints : Int
    , somePlaceStatus : List String
    , someThingStatus : List String
    , adventurers : Dict String Adventurer
    , activeAdvName : String
    , activeMove : (Maybe Move, String)
    , dieRoll : Int
    }

type alias Adventurer =
    { name : String
    , canMove : Bool
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

init : () -> (Model, Cmd Msg)
init _ =
    (initialModel, Cmd.none)

initialModel : Model
initialModel =
    { roundCounter = 0
    , navPoints = 0
    , discoveryPoints = 0
    , somePlaceStatus =
        [ "a simple, safe, small space" ]
    , someThingStatus =
        [ "nothing threatening"
        , "you are temporarily sheltered from whatever was chasing you  "
        ]
    , adventurers =
        Dict.fromList
            [ ("The Nameless One", Adventurer "The Nameless One" True)
            , ("Fake Player", Adventurer "Fake Player" True)
            , ("Sir Placeholder", Adventurer "Sir Placeholder" True)
            ]
    , activeAdvName = "Someone"
    , activeMove = (Nothing, "do something")
    , dieRoll = 0
    }
    

type Msg =
    SetAction String
    | ActivateAdv Adventurer
    | MenuAction
    | Confirm
    | SetMove (Move, String)
    | Roll
    | NewRoll Int
    | DelveTasks Int


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none






-- UPDATE
update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
    case msg of
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
                    

        Confirm  ->
            if model.activeAdvName == "Someone"
            then (model, Cmd.none)
            else
                case Tuple.first model.activeMove of

                    Just Orientate ->
                        if model.navPoints < 3
                        then (doOrientate model, Cmd.none)
                        else (model, Cmd.none)

                    Just DelveAhead ->
                        (model, Random.generate DelveTasks (Random.int 1 6) )
                
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
                    Nothing ->
                        (model, Cmd.none)
        DelveTasks roll ->
            (doDelveAhead model roll, Cmd.none)
            
        MenuAction ->
            (model, Cmd.none)
        SetMove (move, name) ->
            ({model | activeMove = (Just move, name)} , Cmd.none)
        
        Roll ->
            ( model
            , Random.generate NewRoll (Random.int 1 6) 
            )
        NewRoll newRoll ->
            ( { model | dieRoll = newRoll }
            , Cmd.none
            )



doOrientate : Model -> Model
doOrientate model =
    { model 
    | navPoints = model.navPoints +1
    , adventurers = updateAdvCanMove model
    , activeAdvName = initialModel.activeAdvName
    , activeMove = initialModel.activeMove
    }


doDelveAhead : Model -> Int -> Model
doDelveAhead model roll =
    { model
    | somePlaceStatus = revealSomeplace roll 
    , discoveryPoints = model.discoveryPoints +1
    , adventurers = updateAdvCanMove model
    , activeAdvName = initialModel.activeAdvName
    , activeMove = initialModel.activeMove
    }
        


revealSomeplace : Int -> List String
revealSomeplace roll =
    if roll < 4 then
       let
        passageText =
            case roll of
                1 -> "an ascending passage"
                2 -> "a descending passage"
                3 -> "a twisting passage"
                4 -> "a forking passage"
                5 -> "an unstable passage"
                6 -> "an obstructed passage"
                _ -> "error"
          
        in
            [passageText]
            
    else if roll < 6 then
        let
            areaText =
                case roll of
                    1 -> "a small area"
                    2 -> "a big area"
                    3 -> "a vast area"
                    4 -> "a luxurious area"
                    5 -> "a ruined area"
                    6 -> "an eerie area"
                    _ -> "error"
            openingsText =
                if roll == 1 then "no"
                else if roll < 4 then "one"
                else if roll < 6 then "two"
                else "three or more"
        in
            [areaText
            , ("beside the one you came in through there are " ++ openingsText ++ " other openings")
            ]
    else
        let
            locationText =
                case roll of
                    1 -> "a chance to get out"
                    2 -> "a shot at the quest"
                    3 -> "a great treasure"
                    4 -> "a brush with evil"
                    5 -> "?"
                    6 -> "??"
                    _ -> "error"
        in
            
            [ ("a location that offers " ++ locationText)
            , ("freely describe it as a Passage or Area that will suit the needs of this special place")
            ]
            



    


      




updateAdvCanMove : Model -> Dict String Adventurer
updateAdvCanMove model =
    let
        setCanMove : Bool -> Adventurer -> Adventurer
        setCanMove newCanMove adventurer =
            { adventurer | canMove = newCanMove }
    in
        Dict.update
            model.activeAdvName
            (\maybeAdventurer ->
                case maybeAdventurer of
                    Just adventurer ->
                        adventurer
                            |> setCanMove False
                            |> Just
                    Nothing -> Nothing
            ) model.adventurers
    


       


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
                , width (px 800)
                ]
                [ menuRow
                , promptRow
                , selectionRow model
                , movesRow model
                ]
            , column
                [ centerX
                , Border.color (rgb255 0 0 0)
                , Border.width 1
                , padding 10
                , spacing 30
                , width (px 800)
                , height fill
                ]
                [ statsRow model
                , sitchRow model
                ]
            ]
        )



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
            , text ( model.dieRoll |> String.fromInt )
            ]
        ]


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


promptRow : Element msg
promptRow =
    row
        []
        [ paragraph
            []
            [ el [Font.bold] (text "1. ")
            , text "Freely describe what your Adventurer is doing. When this description matches a Move, do the Move." ]
        ]


selectionRow : Model -> Element Msg
selectionRow model =
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


listOfAdv : { a | adventurers : Dict k v } -> List (k, v)
listOfAdv model =
    Dict.toList model.adventurers
    

movesRow : Model -> Element Msg
movesRow model =
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
                    [ moveButtons (Orientate, "Orientate")
                    ]
                , paragraph stdColumn
                    [ text "When you spend time consulting your maps and making sense of the area’s layout..."]
                
                , text ""

                , paragraph (stdColumn ++ [Font.bold, Font.italic])
                    [moveButtons (DelveAhead, "Delve Ahead")]
                , paragraph stdColumn
                    [ text "When you step into a new section hastily, carelessly or blindly..." ]
                
                , text ""

                , paragraph (stdColumn ++ [Font.bold, Font.italic])
                    [moveButtons (GoWatchfully, "Go Watchfully")]
                , paragraph stdColumn
                    [ text "When you step into a new section slowly and carefully..." ]
                
                ]

            , column [width (fillPortion 1)]
                [ el
                    [centerX
                    , Font.bold
                    , padding 10] (text "SEARCH" )

                , paragraph (stdColumn ++ [Font.bold, Font.italic])
                    [ moveButtons (Forage, "Forage")
                    ]
                , paragraph stdColumn
                    [ text "When you spend time looking around to find something you need..."]
                
                , text ""

                , paragraph (stdColumn ++ [Font.bold, Font.italic])
                    [ moveButtons (Prod, "Prod")
                    ]
                , paragraph stdColumn
                    [ text "When you manipulate or get very close to something specific in the current segment..." ]
                
                , text ""

                , paragraph (stdColumn ++ [Font.bold, Font.italic])
                    [ moveButtons (Inspect, "Inspect")
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
                    [ moveButtons (TakeaRisk, "Take a Risk")
                    ]
                , paragraph stdColumn
                    [ text "When you try to overcome an obstacle with your own direct actions..."]
                
                , text ""

                , paragraph (stdColumn ++ [Font.bold, Font.italic])
                    [ moveButtons (UseIngenuity, "Use Ingenuity")
                    ]
                , paragraph stdColumn
                    [ text "When you try to build, repair, craft or produce a thing..." ]
                
                , text ""

                , paragraph (stdColumn ++ [Font.bold, Font.italic])
                    [ moveButtons (Fight, "Fight")
                    ]
                , paragraph stdColumn
                    [ text "When you fight a dangerous opponent..." ]
                ]
            ]
        ]




bulletListBuilder : String -> Element msg
bulletListBuilder myList =
    el [] ( paragraph [padding 5, spacing 5] [text ("+ " ++ myList) ] )


sitchRow : Model -> Element Msg
sitchRow model =
    let
        advName =
            model.activeAdvName
        activeMove =
            Tuple.second model.activeMove
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
                [text "This segment is"]

            , column
                []
                (List.map bulletListBuilder model.somePlaceStatus)
            , el [] (text "")
            , paragraph
                [ Background.color (rgb255 211 211 211)
                , padding 5]
                [text "You face"]

            , column
                []
                (List.map bulletListBuilder model.someThingStatus)
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
                []
                (playerPrompt model)
            ]
        ]
        


playerPrompt : Model -> List (Element Msg)
playerPrompt model =
    if model.activeAdvName == "Someone" && model.activeMove == (Nothing, "do something")
    then [paragraph [][text "Select an Adventurer and a Move."]]
    
    else if model.activeAdvName == "Someone"
    then [paragraph [][text "Select an Adventurer."]]
    
    else if model.activeMove == (Nothing,"do something")
    then [paragraph [][text "Select a Move."]]
    
    else playerTasks model


playerTaskStructure : String -> String -> List (Element Msg)
playerTaskStructure playerTaskOne playerTaskTwo =
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
    , otherButtons Confirm "Confirm?"
    ]

playerTasks : Model -> List (Element Msg)
playerTasks model =
    case Tuple.first model.activeMove of

        Just Orientate ->
            let
                ptOne = "...say where you think you should go next, and explain why you think so."
                ptTwo =
                    ( "You get your bearings. (max. "
                    ++
                    ( 3 - model.navPoints |> String.fromInt )
                    ++
                    " times)"
                    )
            in
                playerTaskStructure ptOne ptTwo
           
        Just DelveAhead ->
            let
                ptOne = "...you are already INSIDE the new section!"
                ptTwo = "Others can be with you, if they want, but they'll share the risks."
            in
                playerTaskStructure ptOne ptTwo

        Just GoWatchfully ->
            let
                ptOne = "...you are just OUTSIDE or already INSIDE the new section, your choice"
                ptTwo = "Others can be with you, if they want, but they'll share the risks."
            in
                playerTaskStructure ptOne ptTwo
        
        Just Forage ->
            let
                ptOne = "...say WHY it makes sense that such a thing would be available here."
                ptTwo = "If anyone objects, you can't make this Move."
            in
                playerTaskStructure ptOne ptTwo
        
        Just Prod ->
            let
                ptOne = "...say exactly HOW you do it."
                ptTwo = "Say also what you are afraid could go wrong."
            in
                playerTaskStructure ptOne ptTwo
           
        Just Inspect ->
            let
                ptOne = "...say exactly HOW you do it"
                ptTwo = "Say also what you are afraid could go wrong."
            in
                playerTaskStructure ptOne ptTwo

        Just TakeaRisk ->
            let
                ptOne = "...say exactly HOW you do it."
                ptTwo = "Check if Traits or Help apply and spend them if you want."
            in
                playerTaskStructure ptOne ptTwo
        
        Just UseIngenuity ->
            let
                ptOne = "...the Enemy will tell you which Materials, Tools and Knowledges are required and how much Time is needed."
                ptTwo = "If you have everything ready at hand, say HOW you do it, then it is done."
            in
                playerTaskStructure ptOne ptTwo
           
        Just Fight ->
            let
                ptOne = "...you are already INSIDE the new section!"
                ptTwo = "Others can be with you, if they want, but they'll share the risks."
            in
                playerTaskStructure ptOne ptTwo

        Nothing ->
            [paragraph
                []
                [ text "" ]
            ]

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
        --, el [] ( text "")
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





moveButtons : (Move, String) -> Element Msg
moveButtons (move, label) =
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
            { onPress = Just (SetMove (move, label) )
            , label = text label
            }
        )

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


 