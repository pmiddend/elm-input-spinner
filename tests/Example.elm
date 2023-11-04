module Example exposing (..)

import DigitalNumber as DN
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (Test, describe)


suite : Test
suite =
    describe "digital numbers"
        [ test "output without sign and decimals" (\_ -> DN.make minValue maxValue currentValue) ]
