module Main exposing (..)

import Browser
import Char exposing (fromCode, toCode)
import Color
import Html exposing (Html, button, div, input, span, text)
import Html.Attributes exposing (type_)
import Html.Events exposing (on)
import Json.Decode
import Keyboard.Event exposing (KeyboardEvent, decodeKeyboardEvent)
import Keyboard.Key as Key
import List.Extra as ListExtra
import TypedSvg exposing (circle, svg, text_)
import TypedSvg.Attributes exposing (cx, cy, fill, fontFamily, fontSize, r, stroke, strokeWidth, viewBox, x, y)
import TypedSvg.Core as SvgCore exposing (Svg, attribute)
import TypedSvg.Types exposing (Paint(..), px)



-- MAIN


main =
    Browser.sandbox { init = init 0 12345 123, update = update, view = view }


type DigitalNumber
    = DigitalNumber Int Int (List Int)


asciiCodeForZero =
    48


leftPadList : List a -> a -> Int -> List a
leftPadList xs a len =
    let
        xsLen =
            List.length xs

        remainder =
            len - xsLen
    in
    if remainder > 0 then
        List.repeat remainder a ++ xs

    else
        xs


makeDigitalNumber : Int -> Int -> Int -> DigitalNumber
makeDigitalNumber minV maxV n =
    let
        nStr : String
        nStr =
            String.fromInt n

        nListChar : List Char
        nListChar =
            String.toList nStr

        numberOfDigits : Int
        numberOfDigits =
            String.length <| String.fromInt <| max (abs minV) (abs maxV)

        -- Here, theoretically, stuff could break. If we
        -- have a string that contains things other than
        -- numbers, we have values outside 0, 1 here. So
        -- we could return "Maybe (List Int)", but that is
        -- so cumbersome and unnecesary since we know we have a positive integer in front of us
        codeToNumber : Int -> Int
        codeToNumber code =
            code - asciiCodeForZero
    in
    DigitalNumber minV maxV (leftPadList (List.map (codeToNumber << toCode) nListChar) 0 numberOfDigits)


digitalNumberValue : DigitalNumber -> Int
digitalNumberValue (DigitalNumber _ _ digits) =
    List.foldl (\newDigit oldNumber -> oldNumber * 10 + newDigit) 0 digits


digitalNumberChangeDigit : (Int -> Int) -> DigitalNumber -> Int -> DigitalNumber
digitalNumberChangeDigit f (DigitalNumber minV maxV digits) n =
    let
        newDigits =
            ListExtra.updateAt n f digits

        newValue =
            digitalNumberValue (DigitalNumber minV maxV newDigits)
    in
    DigitalNumber minV maxV <|
        if newValue < minV || newValue > maxV then
            digits

        else
            newDigits


digitalNumberIncreaseDigit : DigitalNumber -> Int -> DigitalNumber
digitalNumberIncreaseDigit =
    let
        increaseDigit d =
            if d < 9 then
                d + 1

            else
                d
    in
    digitalNumberChangeDigit increaseDigit


digitalNumberDecreaseDigit : DigitalNumber -> Int -> DigitalNumber
digitalNumberDecreaseDigit =
    let
        decreaseDigit d =
            if d > 0 then
                d - 1

            else
                d
    in
    digitalNumberChangeDigit decreaseDigit


digitalNumberNumberOfDigits : DigitalNumber -> Int
digitalNumberNumberOfDigits (DigitalNumber minV maxV n) =
    String.length <| String.fromInt <| max (abs minV) (abs maxV)


digitalNumberToChars : DigitalNumber -> List Char
digitalNumberToChars (DigitalNumber minValue maxValue digits) =
    List.map (\x -> fromCode (x + asciiCodeForZero)) digits



-- MODEL


type alias Model =
    { number : DigitalNumber
    , currentDigitIndex : Int
    , hasFocus : Bool
    }


init : Int -> Int -> Int -> Model
init minValue maxValue currentValue =
    { number = makeDigitalNumber minValue maxValue currentValue
    , currentDigitIndex = 0
    , hasFocus = False
    }


type Msg
    = HandleKeyboardEvent KeyboardEvent


update : Msg -> Model -> Model
update msg model =
    case msg of
        HandleKeyboardEvent { keyCode } ->
            case keyCode of
                Key.Up ->
                    { model | number = digitalNumberIncreaseDigit model.number model.currentDigitIndex }

                Key.Down ->
                    { model | number = digitalNumberDecreaseDigit model.number model.currentDigitIndex }

                Key.Left ->
                    { model
                        | currentDigitIndex =
                            if model.currentDigitIndex > 0 then
                                model.currentDigitIndex - 1

                            else
                                0
                    }

                Key.Right ->
                    { model
                        | currentDigitIndex =
                            if model.currentDigitIndex < digitalNumberNumberOfDigits model.number - 1 then
                                model.currentDigitIndex + 1

                            else
                                model.currentDigitIndex
                    }

                _ ->
                    model



-- VIEW


svgTabindex : Int -> SvgCore.Attribute msg
svgTabindex =
    attribute "tabindex" << String.fromInt


digitFontSize : Int
digitFontSize =
    30


viewDigit : Bool -> Float -> Float -> Char -> Svg msg
viewDigit selected xV yV d =
    text_
        [ x (px xV)
        , y (px yV)
        , fontSize (px (toFloat digitFontSize))
        , fontFamily [ "sans-serif" ]
        , fill <|
            Paint
                (if selected then
                    Color.red

                 else
                    Color.blue
                )
        ]
        [ SvgCore.text (String.fromChar d) ]


view : Model -> Html Msg
view model =
    let
        digitalNumberChars =
            digitalNumberToChars model.number
    in
    div []
        [ input [ type_ "text" ] []
        , svg
            [ viewBox 0 0 800 600
            , svgTabindex 0
            , on "keydown" <| Json.Decode.map HandleKeyboardEvent decodeKeyboardEvent
            ]
            (List.indexedMap
                (\i ->
                    viewDigit
                        (i == model.currentDigitIndex)
                        (toFloat (i * digitFontSize))
                        30.0
                )
                digitalNumberChars
            )

        -- , svg [ viewBox 0 0 800 600 ]
        --     [ circle
        --         [ cx (px 100)
        --         , cy (px 100)
        --         , r (px 30)
        --         , fill <| Paint Color.blue
        --         , strokeWidth (px 2)
        --         , stroke <| Paint <| Color.rgba 0.8 0 0 0.5
        --         , svgTabindex 0
        --         ]
        --         []
        --     ]
        ]
