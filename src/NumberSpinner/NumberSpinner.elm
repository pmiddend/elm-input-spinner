module NumberSpinner.NumberSpinner exposing
    ( CursorPosition(..)
    , Model
    , Msg
    , SpinnerBounds
    , getDecimalValue
    , init
    , isMutatingMsg
    , moveCursorLeft
    , moveCursorRight
    , setValue
    , update
    , valueAsFloat
    , view
    )

import Color
import NumberSpinner.DecimalNumber as Decimal
import NumberSpinner.DigitalNumber as DigitalNumber
import Html exposing (Html, div)
import Html.Events exposing (onBlur, onFocus, preventDefaultOn)
import Json.Decode
import Keyboard.Event exposing (KeyboardEvent, considerKeyboardEvent)
import Keyboard.Key as Key
import TypedSvg exposing (circle, g, line, rect, svg, text_)
import TypedSvg.Attributes exposing (cx, cy, dominantBaseline, fill, fontFamily, fontSize, height, r, rx, stroke, style, textAnchor, transform, viewBox, width, x, x1, x2, y, y1, y2)
import TypedSvg.Core as SvgCore exposing (Svg, attribute)
import TypedSvg.Events as SvgEvents
import TypedSvg.Types
    exposing
        ( AnchorAlignment(..)
        , DominantBaseline(..)
        , Paint(..)
        , Transform(..)
        , em
        , px
        )


{-| We use this class for the "cursor left" and "cursor right" motions in conjunction with the CursorPosition type (see below). We want to define cursor movement without regards to the current number displayed. For movement, we need the left and right bounds, which we store here.
-}
type alias SpinnerBounds =
    { boundsDecimals : Int
    , boundsIntegers : Int
    , hasSign : Bool
    }


{-| A type signifying where the cursor currently is, and where it can move.
-}
type CursorPosition
    = OnSign SpinnerBounds
    | OnInteger SpinnerBounds Int
    | OnDecimal SpinnerBounds Int


{-| Getter function for the bounds (since they're stored separately in each choice (OnSign, OnInteger, OnDecimal), we could have stored those differently as well (say, a tuple))
-}
cursorPositionBounds : CursorPosition -> SpinnerBounds
cursorPositionBounds x =
    case x of
        OnSign bounds ->
            bounds

        OnInteger bounds _ ->
            bounds

        OnDecimal bounds _ ->
            bounds


{-| Is the cursor currently on the plus/minus sign?
-}
isCursorOnSign : CursorPosition -> Bool
isCursorOnSign x =
    case x of
        OnSign _ ->
            True

        _ ->
            False


{-| Is the cursor currently on an integer digit
-}
isCursorOnInteger : Int -> CursorPosition -> Bool
isCursorOnInteger i x =
    case x of
        OnInteger _ j ->
            j == i

        _ ->
            False


{-| Is the cursor currently on a decimal digit
-}
isCursorOnDecimal : Int -> CursorPosition -> Bool
isCursorOnDecimal i x =
    case x of
        OnDecimal _ j ->
            j == i

        _ ->
            False


type alias Model msg =
    { number : DigitalNumber.DigitalNumber
    , cursorPosition : CursorPosition
    , hasFocus : Bool

    -- Convert our "number spinner" internal message to an "outside" message
    , toMsg : Msg -> msg
    }


valueAsFloat : Model msg -> Float
valueAsFloat { number } =
    DigitalNumber.valueAsFloat number


init : Int -> Decimal.DecimalNumber -> Decimal.DecimalNumber -> Decimal.DecimalNumber -> (Msg -> msg) -> Model msg
init decimalPlaces minValue maxValue currentValue toMsg =
    let
        number =
            DigitalNumber.make decimalPlaces minValue maxValue currentValue
    in
    { number = number
    , cursorPosition =
        -- For no particular reason, the "default" cursor position is on the first integral digit. This you can see when you "tab" through the input fields and select a spinner.
        OnInteger
            { boundsDecimals = decimalPlaces
            , boundsIntegers = DigitalNumber.numberOfIntegerDigits number
            , hasSign = DigitalNumber.hasSign number
            }
            0
    , hasFocus = False
    , toMsg = toMsg
    }


type Msg
    = HandleKeyboardEvent KeyboardEvent
    | IncreaseIntegerDigit Int
    | IncreaseDecimalDigit Int
    | FlipSign
    | DecreaseIntegerDigit Int
    | DecreaseDecimalDigit Int
    | FocusSign
    | FocusIntegerDigit Int
    | FocusDecimalDigit Int
    | SetFocus Bool


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


{-| All keys that we capture
-}
nonPropagatedKeys : List Key.Key
nonPropagatedKeys =
    [ Key.Up
    , Key.Down
    , Key.Left
    , Key.Right
    , Key.Backspace
    , Key.Zero
    , Key.One
    , Key.Two
    , Key.Three
    , Key.Four
    , Key.Five
    , Key.Six
    , Key.Seven
    , Key.Eight
    , Key.Nine
    , Key.NumpadZero
    , Key.NumpadOne
    , Key.NumpadTwo
    , Key.NumpadThree
    , Key.NumpadFour
    , Key.NumpadFive
    , Key.NumpadSix
    , Key.NumpadSeven
    , Key.NumpadEight
    , Key.NumpadNine
    ]


{-| Clumsy function to convert a keycode (numpad or number row) to an actual number
-}
keyCodeToDigit : Key.Key -> Maybe Int
keyCodeToDigit x =
    case x of
        Key.Zero ->
            Just 0

        Key.One ->
            Just 1

        Key.Two ->
            Just 2

        Key.Three ->
            Just 3

        Key.Four ->
            Just 4

        Key.Five ->
            Just 5

        Key.Six ->
            Just 6

        Key.Seven ->
            Just 7

        Key.Eight ->
            Just 8

        Key.Nine ->
            Just 9

        Key.NumpadZero ->
            Just 0

        Key.NumpadOne ->
            Just 1

        Key.NumpadTwo ->
            Just 2

        Key.NumpadThree ->
            Just 3

        Key.NumpadFour ->
            Just 4

        Key.NumpadFive ->
            Just 5

        Key.NumpadSix ->
            Just 6

        Key.NumpadSeven ->
            Just 7

        Key.NumpadEight ->
            Just 8

        Key.NumpadNine ->
            Just 9

        _ ->
            Nothing


isMutatingMsg : Msg -> Bool
isMutatingMsg msg =
    case msg of
        IncreaseIntegerDigit _ ->
            True

        IncreaseDecimalDigit _ ->
            True

        DecreaseIntegerDigit _ ->
            True

        DecreaseDecimalDigit _ ->
            True

        FlipSign ->
            True

        HandleKeyboardEvent { keyCode } ->
            case keyCode of
                Key.Up ->
                    True

                Key.Down ->
                    True

                _ ->
                    case keyCodeToDigit keyCode of
                        Just _ ->
                            True

                        _ ->
                            False

        _ ->
            False


update : Msg -> Model msg -> ( Model msg, Cmd msg )
update msg model =
    case msg of
        FocusSign ->
            ( model, Cmd.none )

        SetFocus newFocus ->
            ( { model | hasFocus = newFocus }, Cmd.none )

        FocusIntegerDigit i ->
            ( { model | cursorPosition = OnInteger (cursorPositionBounds model.cursorPosition) i }, Cmd.none )

        FocusDecimalDigit i ->
            ( { model | cursorPosition = OnDecimal (cursorPositionBounds model.cursorPosition) i }, Cmd.none )

        IncreaseIntegerDigit i ->
            let
                newModel =
                    { model
                        | number = DigitalNumber.increaseIntegerDigit model.number i
                        , cursorPosition = OnInteger (cursorPositionBounds model.cursorPosition) i
                    }
            in
            ( newModel, Cmd.none )

        IncreaseDecimalDigit i ->
            let
                newModel =
                    { model
                        | number = DigitalNumber.increaseDecimalDigit model.number i
                        , cursorPosition = OnDecimal (cursorPositionBounds model.cursorPosition) i
                    }
            in
            ( newModel, Cmd.none )

        DecreaseIntegerDigit i ->
            let
                newModel =
                    { model
                        | number = DigitalNumber.decreaseIntegerDigit model.number i
                        , cursorPosition = OnInteger (cursorPositionBounds model.cursorPosition) i
                    }
            in
            ( newModel, Cmd.none )

        DecreaseDecimalDigit i ->
            let
                newModel =
                    { model
                        | number = DigitalNumber.decreaseDecimalDigit model.number i
                        , cursorPosition = OnDecimal (cursorPositionBounds model.cursorPosition) i
                    }
            in
            ( newModel, Cmd.none )

        FlipSign ->
            let
                newModel =
                    { model | number = DigitalNumber.increaseSign model.number }
            in
            ( newModel, Cmd.none )

        HandleKeyboardEvent { keyCode } ->
            let
                newModel =
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

                        Key.Backspace ->
                            { model | cursorPosition = moveCursorLeft model.cursorPosition }

                        Key.Right ->
                            { model | cursorPosition = moveCursorRight model.cursorPosition }

                        _ ->
                            case keyCodeToDigit keyCode of
                                Just digit ->
                                    case model.cursorPosition of
                                        OnSign _ ->
                                            model

                                        OnInteger _ i ->
                                            { model
                                                | cursorPosition = moveCursorRight model.cursorPosition
                                                , number = DigitalNumber.replaceIntegerDigit i digit model.number
                                            }

                                        OnDecimal _ i ->
                                            { model
                                                | cursorPosition = moveCursorRight model.cursorPosition
                                                , number = DigitalNumber.replaceDecimalDigit i digit model.number
                                            }

                                Nothing ->
                                    model
            in
            ( newModel, Cmd.none )


svgTabindex : Int -> SvgCore.Attribute msg
svgTabindex =
    attribute "tabindex" << String.fromInt


digitFontSize : Int
digitFontSize =
    30


getValue : Model msg -> DigitalNumber.DigitalNumber
getValue m =
    m.number


getDecimalValue : Model msg -> Decimal.DecimalNumber
getDecimalValue m =
    DigitalNumber.getValue (getValue m)


setValue : Model msg -> Decimal.DecimalNumber -> Model msg
setValue m n =
    -- This has no validation!
    { m | number = DigitalNumber.modifyValue m.number (always n) }


{-| Display a single digit, either decimal or integral (they'll look the same)
-}
viewDigit : Bool -> Msg -> Bool -> Int -> Char -> Svg Msg
viewDigit hasFocus clickMsg selected xV d =
    g []
        [ -- Optional rectangle around it if we're focusing on this particular digit
          rect
            [ fill PaintNone
            , stroke
                -- "hasFocus" refers to the whole number spinner, "selected" refers to this particular digit
                (if selected && hasFocus then
                    Paint Color.black

                 else
                    PaintNone
                )

            -- Move "xV" digits to the right
            , x (px (toFloat <| xV * digitFontSize))

            -- Move slightly up (looks better)
            , y (px -2)
            , width (px (toFloat digitFontSize))
            , height (px (toFloat digitFontSize))

            -- Little bit of a rounded corner
            , rx (px 3)
            ]
            []
        , text_
            [ -- Move to the middle of the character (and use AnchorMiddle to set that as the base position)
              x (px ((toFloat xV + 0.5) * toFloat digitFontSize))

            -- Move the the middle of the character (and use DominantBaselineMiddle) to have this as the base position
            , y (px (toFloat digitFontSize * 0.5))
            , textAnchor AnchorMiddle
            , dominantBaseline DominantBaselineMiddle
            , fontSize (px (toFloat digitFontSize))

            -- Monospace, so all cells are equal in size
            , fontFamily [ "monospace" ]
            , fill (Paint Color.black)

            -- Otherwise this has the text input cursor, which is misleading
            , style "cursor: pointer"

            -- Clicking on the digit should focus it, but here, we just emit "clickMsg".
            , SvgEvents.onClick clickMsg
            ]
            [ SvgCore.text (String.fromChar d) ]
        ]


view : Model msg -> Html msg
view model =
    let
        digitalNumberChars =
            DigitalNumber.integerChars model.number

        hasSignIdx =
            if DigitalNumber.hasSign model.number then
                1

            else
                0

        absValueChars : List (Svg Msg)
        absValueChars =
            List.indexedMap
                (\i ->
                    viewDigit
                        model.hasFocus
                        (FocusIntegerDigit i)
                        -- Confusing, I know, but the
                        -- currentDigitIndex is always zero-based and
                        -- includes the sign (so to speak), even if we
                        -- have none. So we have to add 1 here
                        (isCursorOnInteger i model.cursorPosition)
                        (i + hasSignIdx)
                )
                digitalNumberChars

        signChar : List (Svg Msg)
        signChar =
            if DigitalNumber.hasSign model.number then
                [ viewDigit model.hasFocus FocusSign (isCursorOnSign model.cursorPosition) 0 <|
                    if DigitalNumber.isNegative model.number then
                        '-'

                    else
                        '+'
                ]

            else
                []

        decimalDigitChars : List (Svg Msg)
        decimalDigitChars =
            if DigitalNumber.hasDecimals model.number then
                let
                    decimalStartIdx : Int
                    decimalStartIdx =
                        List.length digitalNumberChars + hasSignIdx
                in
                viewDigit model.hasFocus
                    (FocusDecimalDigit 0)
                    False
                    decimalStartIdx
                    -- Not really a digit, as you can see, but still, same viewDigit function
                    '.'
                    :: List.indexedMap
                        (\i ->
                            viewDigit model.hasFocus
                                (FocusDecimalDigit i)
                                (isCursorOnDecimal i model.cursorPosition)
                                (decimalStartIdx + 1 + i)
                        )
                        (DigitalNumber.decimalChars model.number)

            else
                []

        textualInformation =
            g [ transform [ Translate 0 (toFloat digitFontSize) ] ] (signChar ++ absValueChars ++ decimalDigitChars)

        -- This draws the "arrow up" character and the circle around it; it's centered around digitFontSize/2 so we have to translate that to actually use it
        arrowUp : Msg -> Svg Msg
        arrowUp msg =
            let
                ccx =
                    toFloat digitFontSize / 2.0

                ccy =
                    toFloat digitFontSize / 2.0

                cr =
                    toFloat digitFontSize / 3.0

                lineLength =
                    0.6

                wingLength =
                    cr * 0.4
            in
            g [ SvgEvents.onClick msg, style "cursor: pointer" ]
                [ circle
                    [ cx (px ccx)
                    , cy (px ccy)
                    , r (px cr)
                    , fill (Paint Color.white)
                    , stroke (Paint Color.grey)
                    ]
                    []

                -- vertical line
                , line
                    [ x1 (px ccx)
                    , x2 (px ccx)
                    , y1 (px (ccy + cr * lineLength))
                    , y2 (px (ccy - cr * lineLength))
                    , stroke (Paint Color.black)
                    ]
                    []

                -- left arrow wing
                , line
                    [ x1 (px ccx)
                    , x2 (px (ccx - wingLength))
                    , y1 (px (ccy - cr * lineLength))
                    , y2 (px (ccy - cr * lineLength + wingLength))
                    , stroke (Paint Color.black)
                    ]
                    []

                -- right arrow wing
                , line
                    [ x1 (px ccx)
                    , x2 (px (ccx + wingLength))
                    , y1 (px (ccy - cr * lineLength))
                    , y2 (px (ccy - cr * lineLength + wingLength))
                    , stroke (Paint Color.black)
                    ]
                    []
                ]

        -- Rotate the arrow up to get an arrow down!
        arrowDown msg =
            g [ transform [ Rotate 180 (toFloat digitFontSize / 2.0) (toFloat digitFontSize / 2.0) ] ] [ arrowUp msg ]

        -- Arrows on top of the digits
        arrowsTop : List (Svg Msg)
        arrowsTop =
            let
                integers =
                    DigitalNumber.numberOfIntegerDigits model.number

                hasSign =
                    DigitalNumber.hasSign model.number
            in
            (if hasSign then
                [ arrowUp FlipSign ]

             else
                []
            )
                ++ (List.map
                        (\i ->
                            g
                                [ transform
                                    [ Translate
                                        (toFloat
                                            (if hasSign then
                                                i

                                             else
                                                i - 1
                                            )
                                            * toFloat digitFontSize
                                        )
                                        0
                                    ]
                                ]
                                [ arrowUp (IncreaseIntegerDigit (i - 1)) ]
                        )
                        (List.range 1 integers)
                        ++ List.map
                            (\i ->
                                g
                                    [ transform
                                        [ Translate
                                            (toFloat
                                                (if hasSign then
                                                    integers + i + 1

                                                 else
                                                    integers + i
                                                )
                                                * toFloat digitFontSize
                                            )
                                            0
                                        ]
                                    ]
                                    [ arrowUp (IncreaseDecimalDigit (i - 1)) ]
                            )
                            (List.range 1 (DigitalNumber.numberOfDecimalDigits model.number))
                   )

        -- Arrows on the bottom of the digits
        arrowsBottom =
            let
                integers =
                    DigitalNumber.numberOfIntegerDigits model.number

                hasSign =
                    DigitalNumber.hasSign model.number
            in
            [ g [ transform [ Translate 0 (2.0 * toFloat digitFontSize - 4) ] ]
                ((if hasSign then
                    [ arrowDown FlipSign ]

                  else
                    []
                 )
                    ++ (List.map
                            (\i ->
                                g
                                    [ transform
                                        [ Translate
                                            (toFloat
                                                (if hasSign then
                                                    i

                                                 else
                                                    i - 1
                                                )
                                                * toFloat digitFontSize
                                            )
                                            0
                                        ]
                                    ]
                                    [ arrowDown (DecreaseIntegerDigit (i - 1)) ]
                            )
                            (List.range 1 integers)
                            ++ List.map
                                (\i ->
                                    g
                                        [ transform
                                            [ Translate
                                                (toFloat
                                                    (if hasSign then
                                                        integers + i + 1

                                                     else
                                                        integers + i
                                                    )
                                                    * toFloat digitFontSize
                                                )
                                                0
                                            ]
                                        ]
                                        [ arrowDown (DecreaseDecimalDigit (i - 1)) ]
                                )
                                (List.range 1 (DigitalNumber.numberOfDecimalDigits model.number))
                       )
                )
            ]

        numberOfCharacters =
            DigitalNumber.numberOfIntegerDigits model.number
                + DigitalNumber.numberOfDecimalDigits model.number
                + (if DigitalNumber.hasDecimals model.number then
                    1

                   else
                    0
                  )
                + (if DigitalNumber.hasSign model.number then
                    1

                   else
                    0
                  )
    in
    Html.map model.toMsg <|
        div []
            [ svg
                [ viewBox 0
                    0
                    (toFloat <|
                        digitFontSize
                            * numberOfCharacters
                    )
                    (toFloat digitFontSize * 3.0)

                -- With this, the widget is focusable via tab
                , svgTabindex 0
                , width (em (toFloat numberOfCharacters))
                , onFocus (SetFocus True)
                , onBlur (SetFocus False)
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
                (textualInformation :: (arrowsTop ++ arrowsBottom))
            ]
