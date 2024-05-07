module Example exposing (..)

import Char
import DecimalNumber as Decimal
import DigitalNumber as DN
import Expect
import Fuzz exposing (int, intRange, list)
import List
import NumberSpinner as NS
import Test exposing (Test, describe, fuzz, test)

fromStringWithDefault = Decimal.fromString >> Maybe.withDefault (Decimal.fromInt 0)

suite : Test
suite =
    describe "numbers"
        [ describe "decimal numbers"
            [ fuzz int
                "from int to string (fuzzed)"
                (\x ->
                    Expect.equal
                        (String.fromInt x)
                        (Decimal.toString (Decimal.fromInt x))
                )
            , test "from integral and decimals to string" (\_ -> Expect.equal "123.45" (Decimal.toString (fromStringWithDefault "123.45")))
            , test "from integral and decimals to string with -1.1" (\_ -> Expect.equal "-1.1" (Decimal.toString (fromStringWithDefault "-1.1")))
            , fuzz
                (Fuzz.map2
                    (\integralPart decimalDigits -> ( integralPart, decimalDigits ))
                    int
                    (list (intRange 0 9))
                )
                "get decimal integral part"
                (\( integralPart, decimalDigits ) ->
                    Expect.equal integralPart
                        (Decimal.integralPart <|
                            fromStringWithDefault (String.fromInt integralPart ++ "." ++ String.join "" (List.map String.fromInt decimalDigits))
                        )
                )
            , fuzz
                (Fuzz.map2
                    (\integralPart decimalDigits -> ( integralPart, decimalDigits ))
                    int
                    (list (intRange 0 9))
                )
                "get decimal decimal part"
                (\( integralPart, decimalDigits ) ->
                    Expect.equal (List.map (\x -> Char.fromCode (x + 48)) decimalDigits)
                        (Decimal.decimalDigits <| fromStringWithDefault (String.fromInt integralPart ++ "." ++ String.join "" (List.map String.fromInt decimalDigits)))
                )
            ]
        , describe "digital numbers"
            [ test "increase integer digit, not digit overflow"
                (\_ ->
                    let
                        inputValue =
                            DN.make 0 (Decimal.fromInt 0) (Decimal.fromInt 10000) (Decimal.fromInt 5000)

                        result =
                            DN.increaseIntegerDigit inputValue 1
                    in
                    Expect.equal [ '0', '6', '0', '0', '0' ] (DN.integerChars result)
                )
            , test "increase integer digit, digit overflow"
                (\_ ->
                    let
                        inputValue =
                            DN.make 0 (Decimal.fromInt 0) (Decimal.fromInt 10000) (Decimal.fromInt 9000)

                        result =
                            DN.increaseIntegerDigit inputValue 1
                    in
                    Expect.equal [ '1', '0', '0', '0', '0' ] (DN.integerChars result)
                )
            , test "increase integer digit, value overflow"
                (\_ ->
                    let
                        inputValue =
                            DN.make 0 (Decimal.fromInt 0) (Decimal.fromInt 10000) (Decimal.fromInt 9999)

                        result =
                            DN.increaseIntegerDigit inputValue 1
                    in
                    Expect.equal [ '0', '9', '9', '9', '9' ] (DN.integerChars result)
                )
            , test "decrease integer digit, no value underflow"
                (\_ ->
                    let
                        inputValue =
                            DN.make 0 (Decimal.fromInt 0) (Decimal.fromInt 10000) (Decimal.fromInt 5000)

                        result =
                            DN.decreaseIntegerDigit inputValue 2
                    in
                    Expect.equal [ '0', '4', '9', '0', '0' ] (DN.integerChars result)
                )
            , test "decrease integer digit, value underflow"
                (\_ ->
                    let
                        inputValue =
                            DN.make 0 (Decimal.fromInt 0) (Decimal.fromInt 10000) (Decimal.fromInt 0)

                        result =
                            DN.decreaseIntegerDigit inputValue 1
                    in
                    Expect.equal [ '0', '0', '0', '0', '0' ] (DN.integerChars result)
                )
            , test "increase decimal digit, no digit overflow"
                (\_ ->
                    let
                        inputValue =
                            DN.make 4 (Decimal.fromInt 0) (Decimal.fromInt 1000) (fromStringWithDefault "500.1234")

                        result =
                            DN.increaseDecimalDigit inputValue 1
                    in
                    Expect.equal [ '1', '3', '3', '4' ] (DN.decimalChars result)
                )
            , test "increase decimal digit,  digit overflow"
                (\_ ->
                    let
                        inputValue =
                            DN.make 4 (Decimal.fromInt 0) (Decimal.fromInt 1000) (fromStringWithDefault "500.1934")

                        result =
                            DN.increaseDecimalDigit inputValue 1
                    in
                    Expect.equal [ '2', '0', '3', '4' ] (DN.decimalChars result)
                )
            , test "decrease decimal digit, no digit overflow"
                (\_ ->
                    let
                        inputValue =
                            DN.make 4 (Decimal.fromInt 0) (Decimal.fromInt 1000) (fromStringWithDefault "500.1234")

                        result =
                            DN.decreaseDecimalDigit inputValue 1
                    in
                    Expect.equal [ '1', '1', '3', '4' ] (DN.decimalChars result)
                )
            , test "decrease decimal digit,  digit overflow"
                (\_ ->
                    let
                        inputValue =
                            DN.make 4 (Decimal.fromInt 0) (Decimal.fromInt 1000) (fromStringWithDefault "500.1034")

                        result =
                            DN.decreaseDecimalDigit inputValue 1
                    in
                    Expect.equal [ '0', '9', '3', '4' ] (DN.decimalChars result)
                )
            , test "replace integer digit with higher digit"
                (\_ ->
                    let
                        inputValue =
                            DN.make 4 (Decimal.fromInt 0) (Decimal.fromInt 1000) (fromStringWithDefault "500.1234")

                        result =
                            DN.replaceIntegerDigit 1 7 inputValue
                    in
                    Expect.equal [ '0', '7', '0', '0' ] (DN.integerChars result)
                )
            , test "replace integer digit with lower digit"
                (\_ ->
                    let
                        inputValue =
                            DN.make 4 (Decimal.fromInt 0) (Decimal.fromInt 1000) (fromStringWithDefault "500.1234")

                        result =
                            DN.replaceIntegerDigit 1 2 inputValue
                    in
                    Expect.equal [ '0', '2', '0', '0' ] (DN.integerChars result)
                )
            , test "replace decimal digit with higher digit (first position)"
                (\_ ->
                    let
                        inputValue =
                            DN.make 4 (Decimal.fromInt 0) (Decimal.fromInt 1000) (fromStringWithDefault "500.1234")

                        result =
                            DN.replaceDecimalDigit 0 7 inputValue
                    in
                    Expect.equal [ '7', '2', '3', '4' ] (DN.decimalChars result)
                )
            , test "replace decimal digit with higher digit (second position)"
                (\_ ->
                    let
                        inputValue =
                            DN.make 4 (Decimal.fromInt 0) (Decimal.fromInt 1000) (fromStringWithDefault "500.1234")

                        result =
                            DN.replaceDecimalDigit 1 7 inputValue
                    in
                    Expect.equal [ '1', '7', '3', '4' ] (DN.decimalChars result)
                )
            ]
        , describe "number spinner"
            [ test "no sign, no decimals, cursor position"
                (\_ ->
                    let
                        model : NS.Model NS.Msg
                        model =
                            NS.init 0 (Decimal.fromInt 0) (Decimal.fromInt 1000) (Decimal.fromInt 500) identity

                        bounds : NS.SpinnerBounds
                        bounds =
                            { boundsDecimals = 0, boundsIntegers = 4, hasSign = False }
                    in
                    Expect.equal (NS.OnInteger bounds 0) model.cursorPosition
                )
            , test "cursor position, move left if no sign"
                (\_ ->
                    let
                        bounds : NS.SpinnerBounds
                        bounds =
                            { boundsDecimals = 0, boundsIntegers = 4, hasSign = False }

                        position : NS.CursorPosition
                        position =
                            NS.OnInteger bounds 0

                        newPosition =
                            NS.moveCursorLeft position
                    in
                    Expect.equal (NS.OnInteger bounds 0) newPosition
                )
            , test "cursor position, move left with sign"
                (\_ ->
                    let
                        bounds : NS.SpinnerBounds
                        bounds =
                            { boundsDecimals = 0, boundsIntegers = 4, hasSign = True }

                        position : NS.CursorPosition
                        position =
                            NS.OnInteger bounds 0

                        newPosition =
                            NS.moveCursorLeft position
                    in
                    Expect.equal (NS.OnSign bounds) newPosition
                )
            , test "cursor position, move right from sign"
                (\_ ->
                    let
                        bounds : NS.SpinnerBounds
                        bounds =
                            { boundsDecimals = 0, boundsIntegers = 4, hasSign = True }

                        position : NS.CursorPosition
                        position =
                            NS.OnSign bounds

                        newPosition =
                            NS.moveCursorRight position
                    in
                    Expect.equal (NS.OnInteger bounds 0) newPosition
                )
            , test "cursor position, move right on integers"
                (\_ ->
                    let
                        bounds : NS.SpinnerBounds
                        bounds =
                            { boundsDecimals = 2, boundsIntegers = 4, hasSign = True }

                        position : NS.CursorPosition
                        position =
                            NS.OnInteger bounds 0

                        newPosition =
                            NS.moveCursorRight position
                    in
                    Expect.equal (NS.OnInteger bounds 1) newPosition
                )
            , test "cursor position, move right on integers but at right end, no decimals"
                (\_ ->
                    let
                        bounds : NS.SpinnerBounds
                        bounds =
                            { boundsDecimals = 0, boundsIntegers = 4, hasSign = True }

                        position : NS.CursorPosition
                        position =
                            NS.OnInteger bounds 3

                        newPosition =
                            NS.moveCursorRight position
                    in
                    Expect.equal (NS.OnInteger bounds 3) newPosition
                )
            , test "cursor position, move right on integers but at right end, with decimals"
                (\_ ->
                    let
                        bounds : NS.SpinnerBounds
                        bounds =
                            { boundsDecimals = 2, boundsIntegers = 4, hasSign = True }

                        position : NS.CursorPosition
                        position =
                            NS.OnInteger bounds 3

                        newPosition =
                            NS.moveCursorRight position
                    in
                    Expect.equal (NS.OnDecimal bounds 0) newPosition
                )
            , test "cursor position, move left on decmials but at left end"
                (\_ ->
                    let
                        bounds : NS.SpinnerBounds
                        bounds =
                            { boundsDecimals = 2, boundsIntegers = 4, hasSign = True }

                        position : NS.CursorPosition
                        position =
                            NS.OnDecimal bounds 0

                        newPosition =
                            NS.moveCursorLeft position
                    in
                    Expect.equal (NS.OnInteger bounds 3) newPosition
                )
            ]
        ]
