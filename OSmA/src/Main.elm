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



-- MAIN
main : Program () Model Msg
main =
    Browser.sandbox
        { init = defaultModel
        , update = update
        , view = view
        }



-- MODEL
type alias Model =
    { roundCounter : Int
    , navPoints : Int
    , sitchStatus : List String
    , advList : List Adventurer
    , isActive : Maybe Adventurer
    }

type alias Adventurer =
    { name : String
    , canMove : Bool
    , isActive : Bool
    }

defaultModel : Model
defaultModel =
    { roundCounter = 0
    , navPoints = 0
    , sitchStatus =
        [ "You are in a simple, safe, small space."
        , "You are temporarily sheltered from whatever was chasing you."
        ]
    , advList =
        [ { name = "The Nameless One" , canMove = True, isActive = False }
        , { name = "Fake Player" , canMove = True, isActive = False }
        , { name = "Sir Placeholder" , canMove = True, isActive = False }
        ]
    , isActive = Nothing
    }

type Msg =
    SetAction String
    | ActivateAdv Adventurer
    | MenuAction
    | Reorient
    | Confirm



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
            {model | isActive = Just adv}
        Reorient ->
            model
        Confirm ->
         model
        MenuAction ->
            model



    


       


-- VIEW
view : Model -> Html Msg
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

menuRow : Element Msg
menuRow =
    row
        [ spacing 10
        , centerX
        ]
        [ {- myButtons MenuAction "Adventurer Sheets"
        , myButtons MenuAction "Rules Summary"
        , myButtons MenuAction "Save & Exit" -}
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

sitchRow : Model -> Element Msg
sitchRow model =
    let
        activeAdv =
            Maybe.map .name model.isActive |> Maybe.withDefault "Someone"
    in
    row
        [ centerX
        , padding 10
        , spacing 30
        , Background.color (rgb255 211 211 211)
        ]
        [ column stdColumn
            (List.map bulletListBuilder model.sitchStatus)
            
        , column stdColumn
            [ paragraph
                [spacing 5]
                [ text "...but maybe this is not right! Do you want to " ]
            , paragraph
                []
                [ myButtons Reorient "Get Your Bearings"
                , text " ?"
                ]
            , text (" (" ++ String.fromInt model.navPoints ++ " left)")
            ]

        , column stdColumn
            [ paragraph
                []
                [ text (activeAdv ++ " is about to do something") ]
            , myButtons Confirm "Confirm?"
            ]
        ]

selectionRow : Model -> Element Msg
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
            (List.map advButtons model.advList)
        ]




    --|> myButtons (Just ActivateAdv "test") (name)


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


advButtons : Adventurer -> Element Msg
advButtons adv =
    el
        [ Border.solid
        , Border.color (rgb255 0 0 0)
        , Border.width 1
        , Border.rounded 10
        , padding 5
        ]
        (Input.button []
            { onPress = Just <| ActivateAdv adv
            , label = text adv.name
            }
        )


myButtons : Msg -> String -> Element Msg
myButtons msg label =
    el
        [ Border.solid
        , Border.color (rgb255 0 0 0)
        , Border.width 1
        , Border.rounded 10
        , padding 5
        ]
        (Input.button []
            { onPress = Just msg
            , label = text label
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