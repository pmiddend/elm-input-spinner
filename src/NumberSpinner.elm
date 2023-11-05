module NumberSpinner exposing (CursorPosition(..), Model, Msg, SpinnerBounds, init, moveCursorLeft, moveCursorRight, update, view)

import Color
import DecimalNumber as Decimal
import DigitalNumber
import Html exposing (Html, button, div, input, span, text)
import Html.Attributes exposing (type_)
import Html.Events exposing (preventDefaultOn, stopPropagationOn)
import Json.Decode
import Keyboard.Event exposing (KeyboardEvent, considerKeyboardEvent, decodeKeyboardEvent)
import Keyboard.Key as Key
import Set
import TypedSvg exposing (circle, svg, text_)
import TypedSvg.Attributes exposing (cx, cy, fill, fontFamily, fontSize, r, stroke, strokeWidth, viewBox, x, y)
import TypedSvg.Core as SvgCore exposing (Svg, attribute)
import TypedSvg.Types exposing (Paint(..), px)


type alias SpinnerBounds =
    { boundsDecimals : Int
    , boundsIntegers : Int
    , hasSign : Bool
    }


type CursorPosition
    = OnSign SpinnerBounds
    | OnInteger SpinnerBounds Int
    | OnDecimal SpinnerBounds Int


isCursorOnSign : CursorPosition -> Bool
isCursorOnSign x =
    case x of
        OnSign _ ->
            True

        _ ->
            False


isCursorOnInteger : Int -> CursorPosition -> Bool
isCursorOnInteger i x =
    case x of
        OnInteger _ j ->
            j == i

        _ ->
            False


isCursorOnDecimal : Int -> CursorPosition -> Bool
isCursorOnDecimal i x =
    case x of
        OnDecimal _ j ->
            j == i

        _ ->
            False


type alias Model =
    { number : DigitalNumber.DigitalNumber
    , cursorPosition : CursorPosition
    , hasFocus : Bool
    }


init : Int -> Decimal.DecimalNumber -> Decimal.DecimalNumber -> Decimal.DecimalNumber -> Model
init decimalPlaces minValue maxValue currentValue =
    let
        number =
            DigitalNumber.make decimalPlaces minValue maxValue currentValue
    in
    { number = number
    , cursorPosition =
        OnInteger
            { boundsDecimals = decimalPlaces
            , boundsIntegers = DigitalNumber.numberOfIntegerDigits number
            , hasSign = DigitalNumber.hasSign number
            }
            0
    , hasFocus = False
    }


type Msg
    = HandleKeyboardEvent KeyboardEvent


moveCursorLeft : CursorPosition -> CursorPosition
moveCursorLeft x =
    case x of
        OnSign bounds ->
            OnSign bounds

        OnInteger bounds i ->
            if i == 0 then
                if bounds.hasSign then
                    OnSign bounds

                else
                    OnInteger bounds i

            else if i >= 0 then
                OnInteger bounds (i - 1)

            else
                OnInteger bounds i

        OnDecimal bounds i ->
            if i == 0 then
                OnInteger bounds (bounds.boundsIntegers - 1)

            else
                OnDecimal bounds (i - 1)


moveCursorRight : CursorPosition -> CursorPosition
moveCursorRight x =
    case x of
        OnSign bounds ->
            OnInteger bounds 0

        OnInteger bounds i ->
            if i < bounds.boundsIntegers - 1 then
                OnInteger bounds (i + 1)

            else if bounds.boundsDecimals > 0 then
                OnDecimal bounds 0

            else
                OnInteger bounds i

        OnDecimal bounds i ->
            if i < bounds.boundsDecimals - 1 then
                OnDecimal bounds (i + 1)

            else
                OnDecimal bounds i


nonPropagatedKeys : List Key.Key
nonPropagatedKeys =
    [ Key.Up, Key.Down, Key.Left, Key.Right ]


update : Msg -> Model -> Model
update msg model =
    case msg of
        HandleKeyboardEvent { keyCode } ->
            case keyCode of
                Key.Up ->
                    { model
                        | number =
                            case model.cursorPosition of
                                OnSign _ ->
                                    DigitalNumber.increaseSign model.number

                                OnInteger _ i ->
                                    DigitalNumber.increaseIntegerDigit model.number i

                                OnDecimal _ i ->
                                    DigitalNumber.increaseDecimalDigit model.number i
                    }

                Key.Down ->
                    { model
                        | number =
                            case model.cursorPosition of
                                OnSign _ ->
                                    DigitalNumber.decreaseSign model.number

                                OnInteger _ i ->
                                    DigitalNumber.decreaseIntegerDigit model.number i

                                OnDecimal _ i ->
                                    DigitalNumber.decreaseDecimalDigit model.number i
                    }

                Key.Left ->
                    { model | cursorPosition = moveCursorLeft model.cursorPosition }

                Key.Right ->
                    { model | cursorPosition = moveCursorRight model.cursorPosition }

                _ ->
                    model



-- VIEW


svgTabindex : Int -> SvgCore.Attribute msg
svgTabindex =
    attribute "tabindex" << String.fromInt


digitFontSize : Int
digitFontSize =
    30


viewDigit : Bool -> Int -> Float -> Char -> Svg msg
viewDigit selected xV yV d =
    text_
        [ x (px (toFloat <| xV * digitFontSize))
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
            DigitalNumber.integerChars model.number

        yPos =
            30.0

        hasSignIdx =
            if DigitalNumber.hasSign model.number then
                1

            else
                0

        absValueChars : List (Svg msg)
        absValueChars =
            List.indexedMap
                (\i ->
                    viewDigit
                        -- Confusing, I know, but the
                        -- currentDigitIndex is always zero-based and
                        -- includes the sign (so to speak), even if we
                        -- have none. So we have to add 1 here
                        (isCursorOnInteger i model.cursorPosition)
                        (i + hasSignIdx)
                        yPos
                )
                digitalNumberChars

        signChar : List (Svg msg)
        signChar =
            if DigitalNumber.hasSign model.number then
                [ viewDigit (isCursorOnSign model.cursorPosition) 0 yPos <|
                    if DigitalNumber.isNegative model.number then
                        '-'

                    else
                        '+'
                ]

            else
                []

        decimalStartIdx : Int
        decimalStartIdx =
            List.length digitalNumberChars + hasSignIdx

        decimalDigitChars : List (Svg msg)
        decimalDigitChars =
            if DigitalNumber.hasDecimals model.number then
                viewDigit
                    False
                    decimalStartIdx
                    yPos
                    ','
                    :: List.indexedMap
                        (\i d ->
                            viewDigit
                                (isCursorOnDecimal i model.cursorPosition)
                                (decimalStartIdx + 1 + i)
                                yPos
                                d
                        )
                        (DigitalNumber.decimalChars model.number)

            else
                []
    in
    div []
        [ text
            ("Current: "
                ++ DigitalNumber.valueToString model.number
                ++ ", "
                ++ "Min: "
                ++ DigitalNumber.minValueToString model.number
                ++ ", Max: "
                ++ DigitalNumber.maxValueToString model.number
            )
        , svg
            [ viewBox 0 0 400 300
            , svgTabindex 0
            , preventDefaultOn "keydown" <|
                Json.Decode.map (\e -> ( HandleKeyboardEvent e, True ))
                    (considerKeyboardEvent
                        (\e ->
                            if List.member e.keyCode nonPropagatedKeys then
                                Just e

                            else
                                Nothing
                        )
                    )
            ]
            (signChar ++ absValueChars ++ decimalDigitChars)
        ]
