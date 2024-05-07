module NumberSpinner.ListUtilities exposing (..)


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


rightPadList : List a -> a -> Int -> List a
rightPadList xs a len =
    let
        xsLen =
            List.length xs

        remainder =
            len - xsLen
    in
    if remainder > 0 then
        xs ++ List.repeat remainder a

    else
        xs
