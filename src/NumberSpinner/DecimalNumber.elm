module NumberSpinner.DecimalNumber exposing
    ( DecimalNumber
    , fromFloat, fromInt, fromString, tenToThePowerMinus, tenToThePower, toFloat, toString
    , add, mul, subtract
    , decimalDigits, integralPart
    , gt, lt
    )

{-| Since the spinner isn't based on float/double, we need a module that provides us with an "infinite precision" numeric type. This module uses elm-decimal for that purpose.


# Types

@docs DecimalNumber


# Construct

@docs fromFloat, fromInt, fromString, tenToThePowerMinus, tenToThePower, toFloat, toString


# Combine

@docs add, mul, subtract


# Deconstruct

@docs decimalDigits, integralPart


# Compare

@docs gt, lt

-}

import Decimal
import List
import String.Extra as StringExtra


{-| Type alias for the Decimal library (to make the choice of library opaque at this point)
-}
type alias DecimalNumber =
    Decimal.Decimal


{-| For a decimal number, return the integral part (the part before the decimal point). NOTE: This is not infinite precision now, of course. If we have more than 2^32 (or something) as integer, this will fail (and return -1337, somewhat randomly)
-}
integralPart : DecimalNumber -> Int
integralPart =
    Maybe.withDefault -1337 << Maybe.andThen String.toInt << List.head << String.split "." << Decimal.toString


{-| Return just the decimal digits of the decimal number (as chars, since we don't do any calculations with it, we jut display it)
-}
decimalDigits : DecimalNumber -> List Char
decimalDigits =
    String.toList << StringExtra.rightOf "." << Decimal.toString


{-| Return a floating-point representation of the number. Use with caution, of course. But might be fine to send the number over the wire (where precision can be safely lost - Tango uses floats anyways, for example)
-}
toFloat : DecimalNumber -> Float
toFloat =
    Decimal.toFloat


{-| Convert decimal to string
-}
toString : DecimalNumber -> String
toString =
    Decimal.toString


{-| Convert from a floating point value. This function is a bit weird, since it uses Decimal.fromFloat, which can fail. I don't know when this fails. Hopefully, returning 0 in that case is fine.
-}
fromFloat : Float -> DecimalNumber
fromFloat =
    Maybe.withDefault (fromInt 0) << Decimal.fromFloat


{-| Convert from a string
-}
fromString : String -> Maybe DecimalNumber
fromString =
    Decimal.fromString


{-| Implement "less than"
-}
lt : DecimalNumber -> DecimalNumber -> Bool
lt =
    Decimal.lt


{-| Implement "greater than"
-}
gt : DecimalNumber -> DecimalNumber -> Bool
gt =
    Decimal.gt


{-| Add two decimal numbers
-}
add : DecimalNumber -> DecimalNumber -> DecimalNumber
add =
    Decimal.add


{-| Subtract two decimal numbers
-}
subtract : DecimalNumber -> DecimalNumber -> DecimalNumber
subtract =
    Decimal.sub


{-| Multiply two decimal numbers
-}
mul : DecimalNumber -> DecimalNumber -> DecimalNumber
mul =
    Decimal.mul


{-| Convert from integer (meaning have 0 as decimal part)
-}
fromInt : Int -> DecimalNumber
fromInt =
    Decimal.fromInt


{-| Implement 10^(-i) which is used to change decimal digits of a number
-}
tenToThePowerMinus : Int -> DecimalNumber
tenToThePowerMinus whichDigit =
    Decimal.fromIntWithExponent 1 -whichDigit


{-| Implement 10^i which is used to change integral digits of a number
-}
tenToThePower : Int -> DecimalNumber
tenToThePower whichDigit =
    Decimal.fromIntWithExponent 1 whichDigit
