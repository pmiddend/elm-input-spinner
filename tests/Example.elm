module Example exposing (..)

import DecimalNumber as Decimal
import Expect exposing (Expectation)
import Fuzz exposing (int, intRange, list, listOfLengthBetween)
import List
import Test exposing (Test, describe, fuzz, test)


suite : Test
suite =
    describe "decimal numbers"
        [ fuzz int
            "from int to string (fuzzed)"
            (\x ->
                Expect.equal
                    (String.fromInt x ++ ".00")
                    (Decimal.toString (Decimal.fromInt 2 x))
            )
        , test "from integral and decimals to string" (\_ -> Expect.equal "123.45" (Decimal.toString (Decimal.fromIntegralAndDecimals 2 123 [ 4, 5 ])))
        , test "from integral and decimals to string with -1.1" (\_ -> Expect.equal "-1.1" (Decimal.toString (Decimal.fromIntegralAndDecimals 1 -1 [ 1 ])))
        , fuzz (listOfLengthBetween 0 13 (intRange 0 9))
            "from integral and decimals to string (fuzzed)"
            (\xs ->
                let
                    value =
                        Decimal.toString (Decimal.fromIntegralAndDecimals (List.length xs) 123 xs)
                in
                if List.isEmpty xs then
                    Expect.equal "123" value

                else
                    Expect.equal ("123." ++ String.join "" (List.map String.fromInt xs)) value
            )
        , fuzz
            (Fuzz.map2
                (\decimalDigits integralPart -> ( decimalDigits, integralPart ))
                (listOfLengthBetween 0 13 (intRange 0 9))
                int
            )
            "get decimal integral part"
            (\( decimalDigits, integralPart ) ->
                Expect.equal integralPart
                    (Decimal.integralPart <|
                        Decimal.fromIntegralAndDecimals
                            (List.length decimalDigits)
                            integralPart
                            decimalDigits
                    )
            )
        ]
