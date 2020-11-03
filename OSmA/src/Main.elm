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



-- MAIN
main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }



-- MODEL
type alias Model =
    { roundCounter : Int
    , navPoints : Int
    , sitchStatus : List String
    , advList : List Adventurer
    , activeAdv : Maybe Adventurer
    , activeMove : (Maybe Move, String)
    }

type alias Adventurer =
    { name : String
    , canMove : Bool
    , activeAdv : Bool
    }


type Move
    = Orientate
    | DelveAhead
    | GoWatchfully




init : Model
init =
    { roundCounter = 0
    , navPoints = 0
    , sitchStatus =
        [ "You are in a simple, safe, small space."
        , "You are temporarily sheltered from whatever was chasing you."
        ]
    , advList =
        [ { name = "The Nameless One" , canMove = False, activeAdv = False }
        , { name = "Fake Player" , canMove = True, activeAdv = False }
        , { name = "Sir Placeholder" , canMove = True, activeAdv = False }
        ]
    , activeAdv = Nothing
    , activeMove = (Nothing, "do something")
    }









type Msg =
    SetAction String
    | ActivateAdv Adventurer
    | MenuAction
    | Reorient
    | Confirm
    | SetMove (Move, String)










-- UPDATE
update : Msg -> Model -> Model
update msg model = 
    case msg of
        SetAction "Navigate" ->
            model
        SetAction "Search" ->
            model
        SetAction "SolveProblems" ->
            model
        SetAction _ ->
            model
        ActivateAdv adv ->
            if adv.canMove == True
            then {model | activeAdv = Just adv}
            else {model | activeAdv = Nothing}      
        Reorient ->
            model

        Confirm  ->
            if model.activeAdv == Nothing
            then model
            else
                let
                    newModel =
                        case Tuple.first model.activeMove of
                            Just Orientate ->
                                {model | navPoints = model.navPoints +1}
                            Just DelveAhead ->
                                {model | navPoints = model.navPoints +1}
                            Just GoWatchfully ->
                                {model | navPoints = model.navPoints +1}
                            Nothing ->
                                model
                in
                    {newModel 
                    | activeAdv = init.activeAdv
                    , activeMove = init.activeMove
                    }
        MenuAction ->
            model
        SetMove (move, name) ->
            {model | activeMove = (Just move, name)}



    


       


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
                [ text ("This is Round " ++ (String.fromInt model.roundCounter) )
                , text ("You have " ++ (String.fromInt model.navPoints) ++ " Navigation points." )
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
            (List.map advButtons model.advList)
        ]



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
            --, Background.color (rgb255 211 211 211)
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
                    [text "Search and Scavenge"]
                , paragraph stdColumn
                    [ text "When you spend time looking around to find something you need..."]
                
                , text ""

                , paragraph (stdColumn ++ [Font.bold, Font.italic])
                    [text "Prod"]
                , paragraph stdColumn
                    [ text "When you manipulate or get very close to something specific in the current segment..." ]
                
                , text ""

                , paragraph (stdColumn ++ [Font.bold, Font.italic])
                    [text "Inspect"]
                , paragraph stdColumn
                    [ text "When you investigate something specific within the current segment with great caution or from a moderate distance..." ]
                
                ]

            , column [width (fillPortion 1)]
                [ el
                    [centerX
                    , Font.bold
                    , padding 10] (text "OVEWRCOME" )

                , paragraph (stdColumn ++ [Font.bold, Font.italic])
                    [text "Take a Risk"]
                , paragraph stdColumn
                    [ text "When you try to overcome an obstacle with your own direct actions..."]
                
                , text ""

                , paragraph (stdColumn ++ [Font.bold, Font.italic])
                    [text "Use Ingenuity"]
                , paragraph stdColumn
                    [ text "When you try to build, repair, craft or produce a thing..." ]
                
                , text ""

                , paragraph (stdColumn ++ [Font.bold, Font.italic])
                    [text "Fight!"]
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
        activeAdv =
            Maybe.map .name model.activeAdv |> Maybe.withDefault "Someone"
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
            (List.map bulletListBuilder model.sitchStatus)
        
        , column (stdColumn ++ [width (fillPortion 1) ] )
            [ paragraph
                []
                [ el [Font.bold] (text activeAdv)
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
    if model.activeAdv == Nothing && model.activeMove == (Nothing, "do something")
    then [paragraph [][text "Select an Adventurer and a Move."]]
    
    else if model.activeAdv == Nothing
    then [paragraph [][text "Select an Adventurer."]]
    
    else if model.activeMove == (Nothing,"do something")
    then [paragraph [][text "Select a Move."]]
    
    else playerTasks model

playerTasks : Model -> List (Element Msg)
playerTasks model =
    case Tuple.first model.activeMove of
        Just Orientate ->
            [paragraph
                []
                [ text "...say what you think you have learned by getting your bearings."
                
                ]
            , el [] (text "")
            , otherButtons Confirm "Confirm?"
            ]
        Just DelveAhead ->
            [el []
                ( paragraph
                    []
                    [ text "- say who is with you (they must agree)"])
            , el [] (text "")
            , el [] (paragraph
                    []
                    [ text "- you all are already inside the new section"]
                )
            , el [] (text "")
            , otherButtons Confirm "Confirm?"
            ]
        Just GoWatchfully ->
            [paragraph
                []
                [ text "...say what you think you have learned by getting your bearings."
                , otherButtons Confirm "Confirm?"
                ]]
        Nothing ->
            [paragraph
                []
                [ text "...say what you think you have learned by getting your bearings."
                , otherButtons Confirm "Confirm?"
                ]]


advButtons : Adventurer -> Element Msg
advButtons adv =
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


getAdvNames : Model -> List String
getAdvNames model  =
    List.map .name model.advList


getName : Model -> Int -> String
getName model index =
    List.map .name model.advList
    |> Array.fromList
    |> Array.get index
    |> maybeToString

maybeToString : Maybe String -> String
maybeToString x =
    case x of
        Just y -> y
        Nothing -> "Error!"









      