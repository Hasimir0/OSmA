module Main exposing (..)

import Browser
import Html exposing (Html)
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Element.Border as Border
import Array exposing (Array)



-- MAIN
main : Program () StatusModel Msg
main =
    Browser.sandbox
        { init = statusModel
        , update = update
        , view = view
        }



-- MODEL
type alias StatusModel =
    { roundCounter : Int
    , navPoints : Int
    , sitchStatus : List String
    }

statusModel : StatusModel
statusModel =
    { roundCounter = 0
    , navPoints = 0
    , sitchStatus =
        [ "You are in a simple, safe, small space."
        , "You are temporarily sheltered from whatever was chasing you."
        ]
    }

type alias Adventurer =
    { name : String
    , canMove : Bool
    , isActive : Bool
    }

type alias AdvList = List Adventurer

advList : AdvList
advList =
        [ { name = "The Nameless One" , canMove = True, isActive = False }
        , { name = "Fake Player" , canMove = True, isActive = False }
        , { name = "Sir Placeholder" , canMove = True, isActive = False }
        ]

type Msg =
    Navigate
    | Search
    | SolveProblems
    | ActivateAdv



-- UPDATE
update : Msg -> StatusModel -> StatusModel
update msg model = 
    case msg of
        Navigate ->
            model
        Search ->
            model
        SolveProblems ->
            model
        ActivateAdv ->
            model 



       


-- VIEW
view : StatusModel -> Html msg
view model =
    Element.layout
        [ Background.color (rgb255 240 240 240)
        ]
        ( column
            [ centerX
            , Border.color (rgb255 0 0 0)
            , Border.width 1
            , padding 10
            , spacing 30
            , width (px 800)
            ]
            [ menuRow
            , sitchRow model
            , selectionRow model
            , movesRow
            ]
        )

menuRow : Element msg
menuRow =
    row
        [ spacing 10
        , centerX
        ]
        [ myButtons "Adventurer Sheets"
        , myButtons "Rules Summary"
        , myButtons "Save & Exit"
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
            

bulletListBuilder : String -> Element msg
bulletListBuilder myList =
    el [] ( paragraph [padding 5, spacing 5] [text ("+ " ++ myList) ] )

sitchRow : StatusModel -> Element msg
sitchRow model =
    row
        [ centerX
        , padding 10
        , spacing 30
        , Background.color (rgb255 211 211 211)
        ]
        [ column stdColumn
            (List.map bulletListBuilder statusModel.sitchStatus)
            

        , column stdColumn
            [ paragraph
                [spacing 5]
                [ text "...but maybe this is not right! Do you want to " ]
            , paragraph
                []
                [ myButtons "Get Your Bearings"
                , text " ?"
                ]
            , text (" (" ++ String.fromInt statusModel.navPoints ++ " left)")
            ]

        , column stdColumn
            [ paragraph
                []
                [ text "Someone is about to do something" ]
            , myButtons "Confirm?"
            ]
        ]

selectionRow : StatusModel -> Element msg
selectionRow model =
    row
        [ spacing 30 ]
        [ column
            []
            [ text ("This is Round " ++ (String.fromInt model.roundCounter) )
            , text "Who will make the next Move?"
            ]
        , column
            [ spacing 30 ]
            [ myButtons (getName model 0)
            , myButtons (getName model 1)
            , myButtons (getName model 2)
            ]
        ]


movesRow : Element msg
movesRow =
    row
        [ padding 10
        , spacing 30
        , width fill
        , Background.color (rgb255 211 211 211)
        ]
        [ column stdColumn
            [ text "NAVIGATE"
            , text "Get Your Bearings"
            , text "Delve Ahead"
            , text "Go Watchfully"
            ]

        , column stdColumn
            [ text "SEARCH"
            , text "Scavange"
            , text "Prod"
            , text "Inspect"
            ]

        , column stdColumn
            [ text "OVERCOME"
            , text "Take a Risk"
            , text "Use Ingenuity"
            , text "Fight!"
            ]
        ]

myButtons : String -> Element msg
myButtons thisName =
    el
        [ Border.solid
        , Border.color (rgb255 0 0 0)
        , Border.width 1
        , Border.rounded 10
        , padding 5
        ] (Input.button []
                { onPress = Nothing
                , label = text thisName
                }
            )



-- getNamesS


getName : StatusModel -> Int -> String
getName model index =
    List.map .name advList --model.advList
    |> Array.fromList
    |> Array.get index
    |> maybeToString

maybeToString : Maybe String -> String
maybeToString x =
    case x of
        Just y -> y
        Nothing -> "Error!"