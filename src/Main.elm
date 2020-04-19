module Main exposing (..)

import Animator
import Animator.Inline
import Browser
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


animator : Animator.Animator Model
animator =
    Animator.animator
        |> Animator.watching
            .notifyTimeline
            (\newBtnState model ->
                { model | notifyTimeline = newBtnState }
            )



-- Model


type alias Model =
    { notifyTimeline : Animator.Timeline NotifyEmailBtn
    , notifyEmail : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { notifyTimeline = Animator.init READY
      , notifyEmail = ""
      }
    , Cmd.none
    )



-- Update


type Msg
    = Tick Time.Posix
    | UserClickedNotifyMe
    | UserChangedNotifyEmail String
    | UserClickedSendBtn


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            ( model
                |> Animator.update newTime animator
            , Cmd.none
            )

        UserClickedNotifyMe ->
            ( { model
                | notifyTimeline =
                    model.notifyTimeline
                        |> Animator.queue
                            [ Animator.event Animator.slowly INTERMEDIATE_A
                            , Animator.wait (Animator.millis 50)
                            , Animator.event Animator.slowly EDIT_EMAIL
                            ]
              }
            , Cmd.none
            )

        UserChangedNotifyEmail newEmail ->
            ( { model | notifyEmail = newEmail }, Cmd.none )

        UserClickedSendBtn ->
            ( { model
                | notifyTimeline =
                    model.notifyTimeline
                        |> Animator.queue
                            [ Animator.event Animator.slowly INTERMEDIATE_B
                            , Animator.wait (Animator.millis 50)
                            , Animator.event Animator.slowly INTERMEDIATE_C
                            , Animator.wait (Animator.millis 50)
                            , Animator.event Animator.slowly SUBMITTED
                            , Animator.wait (Animator.seconds 3)
                            , Animator.event Animator.slowly INTERMEDIATE_D
                            , Animator.wait (Animator.millis 50)
                            , Animator.event Animator.slowly READY
                            ]
              }
            , Cmd.none
            )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    animator
        |> Animator.toSubscription Tick model



-- View


view : Model -> Browser.Document Msg
view model =
    { title = "Coninuous UI Example"
    , body =
        [ layout
            [ width fill
            , height fill
            , Background.color orange
            ]
            (btnContinuousView model)
        ]
    }


orange =
    rgb255 250 127 116


white =
    rgb255 255 255 255



-- NotifyEmailBtn


type NotifyEmailBtn
    = READY
    | INTERMEDIATE_A
    | EDIT_EMAIL
    | INTERMEDIATE_B
    | INTERMEDIATE_C
    | SUBMITTED
    | INTERMEDIATE_D


btnContinuousView : Model -> Element Msg
btnContinuousView model =
    row
        [ centerX
        , centerY
        , Animator.linear
            model.notifyTimeline
            (\state ->
                case state of
                    READY ->
                        Animator.at 50

                    SUBMITTED ->
                        Animator.at 50

                    INTERMEDIATE_D ->
                        Animator.at 50

                    _ ->
                        Animator.at 10
            )
            |> round
            |> Border.rounded
        , Background.color white
        , Animator.linear
            model.notifyTimeline
            (\state ->
                case state of
                    READY ->
                        Animator.at 150

                    SUBMITTED ->
                        Animator.at 150

                    INTERMEDIATE_D ->
                        Animator.at 150

                    _ ->
                        Animator.at 350
            )
            |> round
            |> px
            |> width
        , height (px 80)
        ]
        (case Animator.current model.notifyTimeline of
            READY ->
                [ notifyMeBtn model.notifyTimeline ]

            INTERMEDIATE_A ->
                [ notifyMeBtn model.notifyTimeline ]

            EDIT_EMAIL ->
                [ notifyEmailWithSubmit model.notifyEmail model.notifyTimeline ]

            INTERMEDIATE_B ->
                [ notifyEmailWithSubmit model.notifyEmail model.notifyTimeline ]

            INTERMEDIATE_C ->
                [ thankyouLabel model.notifyTimeline ]

            SUBMITTED ->
                [ thankyouLabel model.notifyTimeline ]

            INTERMEDIATE_D ->
                [ thankyouLabel model.notifyTimeline ]
        )


notifyMeBtn notifyTimeline =
    Input.button
        [ Font.color orange
        , centerX
        , Animator.Inline.opacity
            notifyTimeline
            (\state ->
                case state of
                    READY ->
                        Animator.at 1

                    _ ->
                        Animator.at 0
            )
            |> Element.htmlAttribute
        , Animator.Inline.scale
            notifyTimeline
            (\state ->
                case state of
                    READY ->
                        Animator.at 1

                    _ ->
                        Animator.at 0
            )
            |> Element.htmlAttribute
        ]
        { onPress = Just UserClickedNotifyMe
        , label =
            el
                [ centerX
                , centerY
                , paddingXY 0 20
                ]
                (text "Notify Me")
        }


notifyEmailWithSubmit emailValue notifyTimeline =
    row
        [ width fill
        , height fill
        , paddingXY 10 10
        , spacing 10
        , Animator.Inline.opacity
            notifyTimeline
            (\state ->
                case state of
                    EDIT_EMAIL ->
                        Animator.at 1

                    _ ->
                        Animator.at 0
            )
            |> Element.htmlAttribute
        , Animator.Inline.scale
            notifyTimeline
            (\state ->
                case state of
                    EDIT_EMAIL ->
                        Animator.at 1

                    _ ->
                        Animator.at 0
            )
            |> Element.htmlAttribute
        ]
        [ Input.email
            [ Border.width 0 ]
            { onChange = UserChangedNotifyEmail
            , text = emailValue
            , placeholder = Just (Input.placeholder [] (text "email@address.org"))
            , label = Input.labelHidden ""
            }
            |> el
                [ centerY
                , width fill
                , Border.rounded 10
                , paddingXY 0 10
                ]
        , Input.button
            [ Font.color white
            , paddingXY 15 15
            , centerX
            , centerY
            , Background.color orange
            , alignRight
            , Border.rounded 10
            ]
            { onPress = Just UserClickedSendBtn
            , label = el [ centerX, centerY ] (text "Send")
            }
        ]


thankyouLabel notifyTimeline =
    el
        [ Font.color orange
        , centerX
        , Animator.Inline.opacity
            notifyTimeline
            (\state ->
                case state of
                    SUBMITTED ->
                        Animator.at 1

                    _ ->
                        Animator.at 0
            )
            |> Element.htmlAttribute
        ]
        (text "Thank you!")
