module BooleanParser exposing (parseString, evalString)

import Parser exposing (..)


type Term
    = T
    | F
    | Not Term
    | And Term Term
    | Or Term Term


base : Parser Term
base =
    oneOf
        [ succeed T |. keyword "T"
        , succeed F |. keyword "F"
        , succeed Not
            |. keyword "!"
            |= lazy (\_ -> factor)
        ]


factor : Parser Term
factor =
    oneOf
        [ succeed identity
            |. symbol "("
            |. spaces
            |= lazy (\_ -> additiveExpr)
            |. spaces
            |. symbol ")"
        , succeed identity
            |= base
        ]


multiplicativeExpr : Parser Term
multiplicativeExpr =
    succeed identity
        |= lazy (\_ -> factor)
        |. spaces
        |> andThen
            (\t ->
                oneOf
                    [ succeed (And t)
                        |. symbol "&&"
                        |. spaces
                        |= lazy (\_ -> multiplicativeExpr)
                    , succeed t
                    ]
            )


additiveExpr : Parser Term
additiveExpr =
    succeed identity
        |= lazy (\_ -> multiplicativeExpr)
        |. spaces
        |> andThen
            (\t ->
                oneOf
                    [ succeed (Or t)
                        |. symbol "||"
                        |. spaces
                        |= lazy (\_ -> additiveExpr)
                    , succeed t
                    ]
            )


expr : Parser Term
expr =
    succeed identity
        |. spaces
        |= additiveExpr
        |. spaces
        |. end


spaces : Parser ()
spaces =
    ignore zeroOrMore (\char -> char == ' ')


parseString : String -> Result Error Term
parseString s =
    run expr s


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
