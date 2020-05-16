module Collapsable exposing
    ( Collection
    , Model
    , Msg(..)
    , animator
    , collectionFromList
    , collectionToSubscriptions
    , init
    , subscriptions
    , update
    , updateCollection
    , view
    , viewFromCollection
    )

import Animator
import Color
import Dict exposing (Dict)
import Element exposing (..)
import Element.Border as Border
import Element.Events
import Element.Font as Font
import Html.Attributes
import Svg
import Svg.Attributes
import Time


type Collection
    = MyCollapsableCollection (Dict String Model)


collectionFromList : List ( String, Model ) -> Collection
collectionFromList keyVals =
    keyVals
        |> Dict.fromList
        |> MyCollapsableCollection


updateCollection : String -> Msg -> Collection -> Collection
updateCollection widgetID msg (MyCollapsableCollection widgets) =
    case Dict.get widgetID widgets of
        Nothing ->
            MyCollapsableCollection widgets

        Just widget ->
            widgets
                |> Dict.insert widgetID (update msg widget)
                |> MyCollapsableCollection


collectionToSubscriptions : (String -> Msg -> msg) -> Collection -> List (Sub msg)
collectionToSubscriptions subHandler (MyCollapsableCollection widgets) =
    widgets
        |> Dict.toList
        |> List.map
            (\( widgetID, widget ) ->
                Sub.map (subHandler widgetID) (subscriptions widget)
            )


viewFromCollection :
    Collection
    -> String
    ->
        { onToggle : msg
        , label : String
        , labelColor : Color.Color
        , content : Element msg
        , contentHeight : Float
        }
    -> Element msg
viewFromCollection (MyCollapsableCollection widgets) widgetId props =
    case Dict.get widgetId widgets of
        Nothing ->
            none

        Just widget ->
            view props widget


type Model
    = Collapsable
        { isCollapsed : Animator.Timeline Bool
        }


init : Model
init =
    Collapsable
        { isCollapsed = Animator.init True
        }


animator : Animator.Animator Model
animator =
    Animator.watching
        (\model ->
            case model of
                Collapsable options ->
                    options.isCollapsed
        )
        (\newState model ->
            case model of
                Collapsable options ->
                    Collapsable { options | isCollapsed = newState }
        )
        Animator.animator


type Msg
    = Tick Time.Posix
    | ToggleView


update : Msg -> Model -> Model
update msg model =
    case msg of
        Tick newTime ->
            model
                |> Animator.update newTime animator

        ToggleView ->
            case model of
                Collapsable options ->
                    let
                        newValue =
                            not (Animator.current options.isCollapsed)
                    in
                    Collapsable { options | isCollapsed = options.isCollapsed |> Animator.go Animator.slowly newValue }


subscriptions : Model -> Sub Msg
subscriptions model =
    animator
        |> Animator.toSubscription Tick model



-- VIEW


view :
    { onToggle : msg
    , label : String
    , labelColor : Color.Color
    , content : Element msg
    , contentHeight : Float
    }
    -> Model
    -> Element msg
view props (Collapsable options) =
    column
        [ width fill
        , height shrink
        , Border.width 1
        ]
        [ row
            [ pointer
            , Element.Events.onClick props.onToggle
            , width fill
            , padding 10
            , htmlAttribute (Html.Attributes.style "color" (Color.toCssString props.labelColor))
            , htmlAttribute (Html.Attributes.style "background-color" "white")
            , Font.bold
            ]
            [ el [] (text props.label)
            , case Animator.current options.isCollapsed of
                True ->
                    el [ alignRight, centerY ] (openCaretIcon props.labelColor)

                False ->
                    el [ alignRight, centerY ] (collapsedCaretIcon props.labelColor)
            ]
        , props.content
            |> el
                [ scrollbarY
                , Animator.linear
                    options.isCollapsed
                    (\state ->
                        if state then
                            Animator.at 0

                        else
                            Animator.at props.contentHeight
                    )
                    |> round
                    |> px
                    |> height
                ]
        ]


openCaretIcon color =
    Svg.svg
        [ Svg.Attributes.width "28"
        , Svg.Attributes.height "16"
        , Svg.Attributes.fill "none"
        , Svg.Attributes.viewBox "0 0 28 16"
        ]
        [ Svg.path
            [ Svg.Attributes.stroke (Color.toCssString color)
            , Svg.Attributes.strokeWidth "4"
            , Svg.Attributes.d "M26 14L14 3L2.00001 14"
            ]
            []
        ]
        |> html
        |> el [ width (px 28), height (px 16) ]


collapsedCaretIcon color =
    Svg.svg
        [ Svg.Attributes.width "28"
        , Svg.Attributes.height "16"
        , Svg.Attributes.fill "none"
        , Svg.Attributes.viewBox "0 0 28 16"
        ]
        [ Svg.path
            [ Svg.Attributes.stroke (Color.toCssString color)
            , Svg.Attributes.strokeWidth "4"
            , Svg.Attributes.d "M2 2L14 13L26 2"
            ]
            []
        ]
        |> html
        |> el [ width (px 28), height (px 16) ]
