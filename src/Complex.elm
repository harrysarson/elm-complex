module Complex exposing
    ( Complex
    , complex, polar, real, imaginary
    , nan, infinity, zero, unity
    , isInfinite, isNan
    , add, subtract, multiply, divide, pow, exp, log, conjugate
    , toCartesian, toPolar, toString, fromString
    )

{-| A library for complex calculations.


## Type

@docs Complex


## Build

@docs complex, polar, real, imaginary


## Special Values

@docs nan, infinity, zero, unity


## Checks

@docs isInfinite, isNan


## Operations

@docs add, subtract, multiply, divide, pow, exp, log, conjugate


## Conversions

@docs toCartesian, toPolar, toString, fromString


# Riemann Sphere

The [Riemann sphere](https://plus.maths.org/content/maths-minute-riemann-sphere) is a way of extending the
complex plane to allow infinite values. All functions in this libary handle infinite values consistantly
with [Wolfram Alpha's](https://www.wolframalpha.com/) `ComplexInfinity`.


# Internal Representation

Complex numbers are represented as a Union Type:

    type Complex
        = Complex
            { re : Float
            , im : Float
            }
        | ComplexNan
        | ComplexInfinity

A complex number will either be finite (`Complex`), NaN or Infinite.
It _should_ be impossible to create a complex number which is finite but has NaN or Infinite components.
If you manage to create one please file an issue.

-}

import Internal.Complex
import Parser exposing ((|.), (|=), Parser)



-- Type --


{-| A complex number. See [Internal Representation](#internal-representation) for more details.
-}
type alias Complex =
    Internal.Complex.Complex



-- Build --


{-| Construct a complex number from real and imaginary parts.
-}
complex : Float -> Float -> Complex
complex =
    Internal.Complex.complex


{-| Construct a real number.
-}
real : Float -> Complex
real re =
    complex re 0


{-| Construct an imaginary number.
-}
imaginary : Float -> Complex
imaginary im =
    complex 0 im


{-| Construct a complex number from magnitude and angle.
-}
polar : Float -> Float -> Complex
polar rho theta =
    if Basics.isInfinite rho then
        infinity

    else
        complex (rho * Basics.cos theta) (rho * Basics.sin theta)


{-| A complex "not a number" value.

    nan == divide (complex 0 0) (complex 0 0)

-}
nan : Complex
nan =
    Internal.Complex.complex (0 / 0) 0


{-| Complex infinity.
Infinity is 'very near' large complex numbers defined on the [Riemann sphere.](https://www.wikiwand.com/en/Riemann_sphere)

    infinity == divide (complex 1 0) (complex 0 0)

-}
infinity : Complex
infinity =
    Internal.Complex.complex (1 / 0) 0


{-| Zero as a complex number.

    zero == complex 0 0

-}
zero : Complex
zero =
    complex 0 0


{-| One as a complex number.

    unity == complex 1 0

-}
unity : Complex
unity =
    complex 1 0



-- Checks --


{-| Test if a complex number is infinite.

    isInfinite infinity == True

    isInfinite nan == False

    isInfinite (complex 1 6) == False

**Note:** you can also use `c == infinity` to test for infinity values.

-}
isInfinite : Complex -> Bool
isInfinite c =
    c == infinity


{-| Test if a complex number is not a number.

    isNan nan == True

    isNan infinity == False

    isNan (complex 1 6) == False

**Note:** you _could_ use `c == nan` to check for NaN values but it is
not recommended as this behavour is different to that of floats (as `0/0 /= 0/0`).

-}
isNan : Complex -> Bool
isNan c =
    c == nan



-- Operations --


{-| Add two complex numbers.

    add (complex 4 1) (complex 1 -7) == complex 5 -6

-}
add : Complex -> Complex -> Complex
add lhs rhs =
    if isInfinite lhs && isInfinite rhs then
        nan

    else
        let
            a =
                toCartesian lhs

            b =
                toCartesian rhs
        in
        complex (a.re + b.re) (a.im + b.im)


{-| Subtract the second argument (rhs) from the first (lhs).

    subtract (complex 8 2) (complex 2 3) == complex 6 -1

-}
subtract : Complex -> Complex -> Complex
subtract lhs rhs =
    if isInfinite lhs && isInfinite rhs then
        nan

    else
        let
            a =
                toCartesian lhs

            b =
                toCartesian rhs
        in
        complex (a.re - b.re) (a.im - b.im)


{-| Multiply two complex numbers.

    multiply (complex 0 4) (complex 1 2) == complex -8 4

-}
multiply : Complex -> Complex -> Complex
multiply lhs rhs =
    if isInfinite lhs || isInfinite rhs then
        if
            (lhs == zero)
                || (rhs == zero)
                || isNan lhs
                || isNan rhs
        then
            nan

        else
            infinity

    else
        let
            a =
                toCartesian lhs

            b =
                toCartesian rhs
        in
        complex (a.re * b.re - a.im * b.im) (a.re * b.im + a.im * b.re)


{-| Divide the first argument (lhs) by the (rhs).

    multiply (complex -12 4.5) (complex 3 0) == complex -4 1.5

-}
divide : Complex -> Complex -> Complex
divide lhs rhs =
    if isInfinite lhs then
        if isNan rhs || isInfinite rhs then
            nan

        else
            infinity

    else if isInfinite rhs then
        if isNan lhs then
            -- note: lhs cannot be infinite due to earlier conditional
            nan

        else
            zero

    else if rhs == zero then
        if isNan lhs then
            nan

        else
            infinity

    else
        let
            numerator =
                toCartesian lhs

            denumerator =
                toCartesian rhs

            answserDenumerator =
                denumerator.re ^ 2 + denumerator.im ^ 2
        in
        complex
            ((numerator.re * denumerator.re + numerator.im * denumerator.im) / answserDenumerator)
            ((numerator.im * denumerator.re - numerator.re * denumerator.im) / answserDenumerator)


{-| Raise a complex number to the power of e.

    exp (imaginary pi) == real -1 -- allowing for floating point rounding errors .

-}
exp : Complex -> Complex
exp z =
    if isInfinite z then
        nan

    else
        let
            a =
                toCartesian z
        in
        polar (Basics.e ^ a.re) a.im


{-| Raise the first argument (lhs) to the power of the second (rhs).

    pow (imaginary 2) (real 4) == real 16

-}
pow : Complex -> Complex -> Complex
pow base exponent =
    if isInfinite base then
        if isNan exponent then
            nan

        else
            let
                { re, im } =
                    toCartesian exponent
            in
            if im == 0 && re > 0 then
                infinity

            else if re < 0 then
                zero

            else if re == 0 && im == 0 then
                unity

            else
                nan

    else if base == zero then
        if exponent == zero || isNan exponent || isInfinite exponent then
            nan

        else
            zero

    else
        exp <| multiply exponent (log base)


{-| Compute natural logarithm of a complex number.

    log (real -10) == complex 2.302585 pi -- allowing for floating point rounding errors.

-}
log : Complex -> Complex
log z =
    if isInfinite z then
        infinity

    else
        let
            { abs, arg } =
                toPolar z
        in
        complex (Basics.logBase Basics.e abs) arg


{-| Compute the conjugate of a complex number

    conjugate (complex 6 8) == complex 6 -8

-}
conjugate : Complex -> Complex
conjugate c =
    let
        { re, im } =
            toCartesian c
    in
    complex re -im



-- Conversions


{-| Convert to string.

    toString (complex 5 1) == "5+i"

-}
toString : Complex -> String
toString c =
    if isNan c then
        "nan"

    else if isInfinite c then
        "̃∞"

    else if c == zero then
        "0"

    else
        let
            { re, im } =
                toCartesian c

            stringRe =
                if re == 0 then
                    ""

                else
                    String.fromFloat re

            stringIm =
                if im == 0 then
                    ""

                else
                    (if im > 0 then
                        if re /= 0 then
                            "+"

                        else
                            ""

                     else
                        "-"
                    )
                        ++ (if Basics.abs im /= 1 then
                                String.fromFloat (Basics.abs im)

                            else
                                ""
                           )
                        ++ "i"
        in
        stringRe ++ stringIm


{-| Create a complex number from a string.

Strings must consist of

  - optionally: a float possibly including a sign
  - zero or one of
      - the character `'i'`.
      - a sign followed by the character `'i'`
      - a sign followed by another float followed by the character `'i'`.

Whitespace is allowed anywhere between the real part and imaginary parts and between the imaginary
part and the character `'i'`.

    fromString "4 + 6i" == Just (complex 4 6)

-}
fromString : String -> Maybe Complex
fromString =
    Parser.run complexParser
        >> Result.toMaybe


complexParser : Parser Complex
complexParser =
    Parser.oneOf
        [ Parser.symbol "i" |> Parser.map (always <| imaginary 1)
        , Parser.symbol "+i" |> Parser.map (always <| imaginary 1)
        , Parser.symbol "-i" |> Parser.map (always <| imaginary -1)
        , (Parser.succeed (*)
            |= Parser.oneOf
                [ Parser.succeed -1
                    |. Parser.symbol "-"
                , Parser.succeed 1
                    |. Parser.symbol "+"
                , Parser.succeed 1
                ]
            |= Parser.float
            |. Parser.spaces
          )
            |> Parser.andThen
                (\firstFloat ->
                    Parser.oneOf
                        [ Parser.succeed (imaginary firstFloat)
                            |. Parser.symbol "i"
                        , Parser.succeed (complex firstFloat)
                            |= Parser.backtrackable
                                (Parser.oneOf
                                    [ Parser.symbol "-" |> Parser.map (always -1)
                                    , Parser.symbol "+" |> Parser.map (always 1)
                                    ]
                                )
                            |. Parser.symbol "i"
                        , Parser.succeed (complex firstFloat)
                            |= (Parser.succeed (*)
                                    |= Parser.oneOf
                                        [ Parser.succeed -1
                                            |. Parser.symbol "-"
                                        , Parser.succeed 1
                                            |. Parser.symbol "+"
                                        ]
                                    |. Parser.spaces
                                    |= Parser.float
                               )
                            |. Parser.spaces
                            |. Parser.symbol "i"
                        , Parser.succeed (real firstFloat)
                        ]
                        |. Parser.end
                )
        ]


{-| Get real and imaginary parts of a complex number.

    toCartesian (complex 4 7) == { re = 4, im = 7 }

    toCartesian (divide (real 1) (real 0)) == { re = 1 / 0, im = 1 / 0 }

-}
toCartesian : Complex -> { re : Float, im : Float }
toCartesian =
    Internal.Complex.toCartesian


{-| Get absolute value and argument of a complex number.

    toPolar (complex 4 7) == { abs = 8.0622577, arg = 0.5191461 } -- allowing for floating point rounding errors.

    toPolar (divide (real 1) (real 0)) == { abs = 1 / 0, arg = 0 / 0 }

-}
toPolar : Complex -> { abs : Float, arg : Float }
toPolar =
    Internal.Complex.toPolar
