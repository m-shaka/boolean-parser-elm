module BooleanParser exposing (parseString, evalString)

import Parser exposing (..)


type Term
    = T
    | F
    | Not Term
    | And Term Term
    | Or Term Term


term : Parser Term
term =
    let
        true =
            succeed T |. keyword "T"

        false =
            succeed F |. keyword "F"

        bracket =
            succeed identity
                |. symbol "("
                |. spaces
                |= lazy (\_ -> term)
                |. spaces

        not_ =
            succeed Not
                |. keyword "not"
                |. spaces
                |= oneOf [ true, false, lazy (\_ -> not_), bracket ]
    in
        succeed identity
            |. spaces
            |= oneOf
                [ bracket, true, false, not_ ]
            |. spaces
            |> andThen
                (\t ->
                    oneOf
                        [ succeed (And t)
                            |. symbol "&&"
                            |. spaces
                            |= lazy (\_ -> term)
                        , succeed (Or t)
                            |. symbol "||"
                            |. spaces
                            |= lazy (\_ -> term)
                        , succeed t
                            |. symbol ")"
                        , succeed t
                            |. end
                        ]
                )


spaces : Parser ()
spaces =
    ignore zeroOrMore (\char -> char == ' ')


parseString : String -> Result Error Term
parseString s =
    run term s


evalTerm : Term -> Bool
evalTerm term =
    case term of
        T ->
            True

        F ->
            False

        Not t ->
            not (evalTerm t)

        And t1 t2 ->
            (evalTerm t1) && (evalTerm t2)

        Or t1 t2 ->
            (evalTerm t1) || (evalTerm t2)


evalString : String -> String
evalString s =
    case parseString s of
        Ok value ->
            if evalTerm value then
                "True"
            else
                "False"

        Err err ->
            "Parse Error"
