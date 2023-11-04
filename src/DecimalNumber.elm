module DecimalNumber exposing (DecimalNumber)

import Numeric.Decimal as NumericDecimal exposing (Decimal)
import Numeric.Decimal.Rounding exposing (RoundingAlgorythm(..))
import Numeric.Nat exposing (nat0, toInt)
import Numeric.Rational as Rational


type MyDecimals
    = MyDecimals


type alias DecimalNumber =
    Decimal MyDecimals Int
