module Main exposing (..)

import Animator
import Animator.Inline
import Browser
import Collapsable
import Color
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Time



-- Main


main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- Model


type alias Model =
    { widgets : Collapsable.Collection }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { widgets =
            Collapsable.collectionFromList
                [ ( "One", Collapsable.init )
                , ( "Two", Collapsable.init )
                ]
      }
    , Cmd.none
    )



-- Update


type Msg
    = GotCollapsableMsg String Collapsable.Msg
    | UserClickedCollapsableWidget String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotCollapsableMsg widgetID subMsg ->
            ( { model | widgets = Collapsable.updateCollection widgetID subMsg model.widgets }
            , Cmd.none
            )

        UserClickedCollapsableWidget widgetID ->
            ( { model | widgets = Collapsable.updateCollection widgetID Collapsable.ToggleView model.widgets }
            , Cmd.none
            )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    model.widgets
        |> Collapsable.collectionToSubscriptions GotCollapsableMsg
        |> Sub.batch



-- View


view : Model -> Browser.Document Msg
view model =
    { title = "Coninuous UI Example"
    , body =
        [ layout
            [ width fill
            , height fill
            , padding 50
            , Background.color orange
            ]
            (column
                [ width fill, height fill, spacing 5 ]
                [ Collapsable.viewFromCollection
                    model.widgets
                    "One"
                    { onToggle = UserClickedCollapsableWidget "One"
                    , label = "My Collapsable Thing"
                    , labelColor = Color.charcoal
                    , content = contentView
                    , contentHeight = 200
                    }
                , Collapsable.viewFromCollection
                    model.widgets
                    "Two"
                    { onToggle = UserClickedCollapsableWidget "Two"
                    , label = "My Other Collapsable Thing"
                    , labelColor = Color.charcoal
                    , content = contentView
                    , contentHeight = 300
                    }
                ]
            )
        ]
    }


orange =
    rgb255 250 127 116


white =
    rgb255 255 255 255


contentView =
    column
        [ Background.color white
        , spacing 20
        , padding 10
        ]
        [ someText
        , someText
        , someText
        , someText
        ]


someText =
    paragraph [ Background.color (rgb255 240 240 240), Font.color orange, padding 10 ]
        [ text "Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book. It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged. It was popularised in the 1960s with the release of Letraset sheets containing Lorem Ipsum passages, and more recently with desktop publishing software like Aldus PageMaker including versions of Lorem Ipsum." ]
