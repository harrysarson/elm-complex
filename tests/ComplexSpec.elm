module ComplexSpec exposing (tests)

import Complex exposing (Complex)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Test exposing (describe, fuzz, fuzz2, fuzz3, test)


tolerance =
    Expect.AbsoluteOrRelative 1.0e-10 1.0e-10


tests : Test.Test
tests =
    describe "Complex numbers"
        [ describe "Build"
            [ describe "complex"
                [ fuzz2 Fuzz.float Fuzz.float "build finite complex number" <|
                    \re im ->
                        Complex.complex re im
                            |> Expect.all
                                [ Expect.notEqual Complex.nan
                                , Expect.notEqual Complex.infinity
                                ]
                , describe "build infinite complex number"
                    [ fuzz Fuzz.float "infinite imaginary part" <|
                        \re ->
                            Complex.complex re (1 / 0)
                                |> Expect.equal Complex.infinity
                    , fuzz Fuzz.float "infinite real part" <|
                        \im ->
                            Complex.complex (1 / 0) im
                                |> Expect.equal Complex.infinity
                    , test "infinite imaginary and real parts" <|
                        \() ->
                            Complex.complex (1 / 0) (1 / 0)
                                |> Expect.equal Complex.infinity
                    ]
                , describe "build 'not a number' complex number"
                    [ fuzz Fuzz.float "nan imaginary part" <|
                        \re ->
                            Complex.complex re (0 / 0)
                                |> Expect.equal Complex.nan
                    , fuzz Fuzz.float "infinite real part" <|
                        \im ->
                            Complex.complex (0 / 0) im
                                |> Expect.equal Complex.nan
                    , test "nan imaginary and real parts" <|
                        \() ->
                            Complex.complex (0 / 0) (0 / 0)
                                |> Expect.equal Complex.nan
                    , test "nan real and infinite imaginary part" <|
                        \() ->
                            Complex.complex (0 / 0) (1 / 0)
                                |> Expect.equal Complex.nan
                    ]
                ]
            , fuzz fuzzMessyFloat "real" <|
                \x ->
                    Complex.real x
                        |> equalComplex (Complex.complex x 0)
            , fuzz fuzzMessyFloat "imaginary" <|
                \x ->
                    Complex.imaginary x
                        |> equalComplex (Complex.complex 0 x)
            , describe "polar"
                [ fuzz2 Fuzz.float fuzzMessyFloat "finite magnitude" <|
                    \r phi ->
                        Complex.polar r phi
                            |> equalComplex
                                (Complex.complex (r * Basics.cos phi) (r * Basics.sin phi))
                , fuzz2 Fuzz.float fuzzMessyFloat "infinite magitude" <|
                    \r phi ->
                        if r == 0 then
                            Expect.pass

                        else
                            Complex.polar (r / 0) phi
                                |> Expect.equal Complex.infinity
                , fuzz fuzzMessyFloat "nan magitude" <|
                    \phi ->
                        Complex.polar (0 / 0) phi
                            |> Expect.equal Complex.nan
                ]
            ]
        , describe "Special Values"
            [ test "nan" <|
                \() ->
                    Complex.nan
                        |> Expect.all
                            [ Complex.isNan
                                >> Expect.true "should be nan"
                            , Complex.isInfinite
                                >> Expect.false "should not be infinite"
                            ]
            , test "infinity" <|
                \() ->
                    Complex.infinity
                        |> Expect.all
                            [ Complex.isNan
                                >> Expect.false "should not be nan"
                            , Complex.isInfinite
                                >> Expect.true "should be infinite"
                            ]
            , test "zero" <|
                \() ->
                    Complex.zero
                        |> equalComplex (Complex.complex 0 0)
            , test "unity" <|
                \() ->
                    Complex.unity
                        |> equalComplex (Complex.complex 1 0)
            ]
        , describe "Checks"
            [ fuzz fuzzMessyComplex "isInfinite" <|
                \c ->
                    Complex.isInfinite c
                        |> Expect.equal (c == Complex.infinity)
            , fuzz fuzzMessyComplex "isNan" <|
                \c ->
                    Complex.isNan c
                        |> Expect.equal (c == Complex.nan)
            ]
        , describe "Operations"
            [ describe "add"
                [ fuzz2 fuzzMessyComplex fuzzComplex "is commutative" <|
                    \lhs rhs ->
                        Complex.add lhs rhs
                            |> equalComplex (Complex.add rhs lhs)
                , fuzz2
                    (Fuzz.tuple ( Fuzz.float, Fuzz.float ))
                    (Fuzz.tuple ( Fuzz.float, Fuzz.float ))
                    "finite + finite"
                  <|
                    \( a, b ) ( c, d ) ->
                        Complex.add (Complex.complex a b) (Complex.complex c d)
                            |> equalComplex (Complex.complex (a + c) (b + d))
                , binaryOperatorNanTest Complex.add
                , fuzz fuzzComplex "infinite + finite" <|
                    \c ->
                        Complex.add Complex.infinity c
                            |> Expect.equal Complex.infinity
                , test "infinite + infinite" <|
                    \() ->
                        Complex.add Complex.infinity Complex.infinity
                            |> Expect.equal Complex.nan
                ]
            , describe "subtract"
                [ fuzz2 fuzzMessyComplex fuzzComplex "is commutative with flipped sign" <|
                    \lhs rhs ->
                        Complex.subtract lhs rhs
                            |> equalComplex
                                (Complex.subtract (Complex.complex 0 0) (Complex.subtract rhs lhs))
                , fuzz2
                    (Fuzz.tuple ( Fuzz.float, Fuzz.float ))
                    (Fuzz.tuple ( Fuzz.float, Fuzz.float ))
                    "finite - finite"
                  <|
                    \( a, b ) ( c, d ) ->
                        Complex.subtract (Complex.complex a b) (Complex.complex c d)
                            |> equalComplex (Complex.complex (a - c) (b - d))
                , binaryOperatorNanTest Complex.subtract
                , fuzz fuzzComplex "infinite - finite" <|
                    \c ->
                        Complex.subtract Complex.infinity c
                            |> Expect.equal Complex.infinity
                , test "infinite - infinite" <|
                    \() ->
                        Complex.subtract Complex.infinity Complex.infinity
                            |> Expect.equal Complex.nan
                ]
            , describe "multiply"
                [ fuzz2 fuzzMessyComplex fuzzComplex "is commutative" <|
                    \lhs rhs ->
                        Complex.multiply lhs rhs
                            |> equalComplex (Complex.multiply rhs lhs)
                , fuzz2
                    (Fuzz.tuple ( Fuzz.float, Fuzz.float ))
                    (Fuzz.tuple ( Fuzz.float, Fuzz.float ))
                    "finite x finite"
                  <|
                    \( a, b ) ( c, d ) ->
                        Complex.multiply (Complex.complex a b) (Complex.complex c d)
                            |> equalComplex (Complex.complex (a * c - b * d) (a * d + b * c))
                , binaryOperatorNanTest Complex.multiply
                , fuzz fuzzComplex "infinite x finite" <|
                    \c ->
                        Complex.multiply Complex.infinity c
                            |> (if c == Complex.complex 0 0 then
                                    always Expect.pass

                                else
                                    Expect.equal Complex.infinity
                               )
                , test "infinite x zero" <|
                    \() ->
                        Complex.multiply Complex.infinity (Complex.complex 0 0)
                            |> Expect.equal Complex.nan
                , test "infinite x infinite" <|
                    \() ->
                        Complex.multiply Complex.infinity Complex.infinity
                            |> Expect.equal Complex.infinity
                ]
            , describe "divide"
                [ fuzz2 fuzzMessyComplex fuzzComplex "flipping args gives inverse" <|
                    \lhs rhs ->
                        Complex.divide lhs rhs
                            |> equalComplex
                                (Complex.divide (Complex.complex 1 0) (Complex.divide rhs lhs))
                , fuzz2
                    (Fuzz.tuple ( Fuzz.float, Fuzz.float ))
                    (Fuzz.tuple ( Fuzz.float, Fuzz.float ))
                    "finite / finite"
                  <|
                    \( a, b ) ( c, d ) ->
                        let
                            denumerator =
                                c ^ 2 + d ^ 2
                        in
                        if c /= 0 || d /= 0 then
                            Complex.divide (Complex.complex a b) (Complex.complex c d)
                                |> equalComplex
                                    (Complex.complex
                                        ((a * c + b * d) / denumerator)
                                        ((b * c - a * d) / denumerator)
                                    )

                        else
                            Expect.pass
                , fuzz fuzzComplex "finite / zero" <|
                    \c ->
                        Complex.divide c (Complex.complex 0 0)
                            |> Expect.equal Complex.infinity
                , binaryOperatorNanTest Complex.divide
                , fuzz fuzzComplex "infinite / finite" <|
                    \c ->
                        Complex.divide Complex.infinity c
                            |> Expect.equal Complex.infinity
                , fuzz fuzzComplex "finite / infinite" <|
                    \c ->
                        Complex.divide c Complex.infinity
                            |> equalComplex (Complex.complex 0 0)
                , test "infinite / infinite" <|
                    \() ->
                        Complex.divide Complex.infinity Complex.infinity
                            |> Expect.equal Complex.nan
                , describe "regressions"
                    [ test "infinity / very small" <|
                        \() ->
                            Complex.divide Complex.infinity (Complex.complex 1.0e-5 2.0e-8)
                                |> Expect.equal Complex.infinity
                    ]
                ]
            , describe "exp"
                [ fuzz2 Fuzz.float Fuzz.float "e ^ finite" <|
                    \a b ->
                        Complex.complex a b
                            |> Complex.exp
                            |> equalComplex (Complex.polar (e ^ a) b)
                , test "e ^ infinite" <|
                    \() ->
                        Complex.infinity
                            |> Complex.exp
                            |> Expect.equal Complex.nan
                , test "e ^ nan" <|
                    \() ->
                        Complex.nan
                            |> Complex.exp
                            |> Expect.equal Complex.nan
                ]
            , describe "pow"
                [ fuzz2 fuzzComplex fuzzComplex "finite ^ finite" <|
                    \a b ->
                        Complex.pow a b
                            |> (if a == Complex.zero then
                                    if b == Complex.zero then
                                        Expect.equal Complex.nan

                                    else
                                        equalComplex Complex.zero

                                else
                                    equalComplex
                                        (Complex.exp <| Complex.multiply b (Complex.log a))
                               )
                , test "0 ^ 0" <|
                    \() ->
                        Complex.pow Complex.zero Complex.zero
                            |> Expect.equal Complex.nan
                , fuzz2 (Fuzz.floatRange 1.0e-30 1.0e30) Fuzz.float "(real and positive) ^ real" <|
                    \a b ->
                        Complex.pow (Complex.real a) (Complex.real b)
                            |> equalComplex
                                (Complex.real (a ^ b))
                , fuzz2 (Fuzz.floatRange 1.0e-30 1.0e30) Fuzz.float "(real and negative) ^ real" <|
                    \a b ->
                        Complex.pow (Complex.real -a) (Complex.real b)
                            |> equalComplex
                                (Complex.polar (a ^ b) (pi * b))
                , fuzz2 (Fuzz.floatRange 1.0e-30 1.0e30) Fuzz.float "(imaginary and positive) ^ real" <|
                    \a b ->
                        Complex.pow (Complex.imaginary a) (Complex.real b)
                            |> equalComplex
                                (Complex.polar
                                    (a ^ b)
                                    (pi / 2 * b)
                                )
                , fuzz2 (Fuzz.floatRange 1.0e-30 1.0e30) Fuzz.float "(imaginary and negative) ^ real" <|
                    \a b ->
                        Complex.pow (Complex.imaginary -a) (Complex.real b)
                            |> equalComplex
                                (Complex.polar
                                    (a ^ b)
                                    (-pi / 2 * b)
                                )
                , binaryOperatorNanTest Complex.pow
                , fuzz fuzzComplex "finite ^ infinite" <|
                    \c ->
                        Complex.pow c Complex.infinity
                            |> Expect.equal Complex.nan
                , fuzz (Fuzz.floatRange 1.0e-100 1.0e100) "infinity ^ (real and positive)" <|
                    \x ->
                        Complex.pow Complex.infinity (Complex.complex x 0)
                            |> Expect.equal Complex.infinity
                , fuzz2 (Fuzz.floatRange -1.0e100 -1.0e-100) Fuzz.float "infinity ^ (negative real part)" <|
                    \x y ->
                        Complex.pow Complex.infinity (Complex.complex x y)
                            |> equalComplex (Complex.complex 0 0)
                , test "infinity ^ infinity" <|
                    \() ->
                        Complex.pow Complex.infinity Complex.infinity
                            |> Expect.equal Complex.nan
                , test "infinity ^ 0" <|
                    \() ->
                        Complex.pow Complex.infinity (Complex.complex 0 0)
                            |> equalComplex (Complex.complex 1 0)
                , fuzz fuzzComplex "infinity ^ (any other complex number)" <|
                    \c ->
                        let
                            cart =
                                Complex.toCartesian c
                        in
                        if
                            (cart.re > 0 && cart.im == 0)
                                || cart.re
                                < 0
                                || c
                                == Complex.complex 0 0
                        then
                            Expect.pass

                        else
                            Complex.pow Complex.infinity c
                                |> Expect.equal Complex.nan
                ]
            , describe "log"
                [ fuzz2 Fuzz.float Fuzz.float "log(finite)" <|
                    \a b ->
                        let
                            ( abs, arg ) =
                                ( a, b ) |> Basics.toPolar
                        in
                        Complex.complex a b
                            |> Complex.log
                            |> equalComplex
                                (Complex.complex
                                    (Basics.logBase Basics.e abs)
                                    arg
                                )
                , fuzz Fuzz.float "log(real)" <|
                    \x ->
                        Complex.real x
                            |> Complex.log
                            |> equalComplex
                                (Complex.complex
                                    (logBase e <| abs x)
                                    (if x >= 0 then
                                        0

                                     else
                                        pi
                                    )
                                )
                , test "log(infinite)" <|
                    \() ->
                        Complex.infinity
                            |> Complex.log
                            |> Expect.equal Complex.infinity
                , test "log(nan)" <|
                    \() ->
                        Complex.nan
                            |> Complex.exp
                            |> Expect.equal Complex.nan
                ]
            , describe "sqrt"
                [ fuzz fuzzPositive "sqrt(positive real)" <|
                    \re ->
                        Complex.real (re ^ 2)
                            |> Complex.sqrt
                            |> equalComplex (Complex.real re)
                , fuzz fuzzPositive "sqrt(negative real)" <|
                    \re ->
                        Complex.real -(re ^ 2)
                            |> Complex.sqrt
                            |> equalComplex (Complex.imaginary re)
                , fuzz fuzzPositive "sqrt(postive imaginary)" <|
                    \a ->
                        Complex.imaginary (a ^ 2)
                            |> Complex.sqrt
                            |> equalComplex (Complex.polar a (pi / 4))
                , fuzz fuzzPositive "sqrt(negative imaginary)" <|
                    \a ->
                        Complex.imaginary -(a ^ 2)
                            |> Complex.sqrt
                            |> equalComplex (Complex.polar a (-pi / 4))
                , fuzz2 fuzzPositive (Fuzz.floatRange -pi pi) "sqrt(complex)" <|
                    \r phi ->
                        Complex.polar r phi
                            |> Complex.sqrt
                            |> equalComplex (Complex.polar (sqrt r) (phi / 2))
                , test "sqrt(infinite)" <|
                    \() ->
                        Complex.infinity
                            |> Complex.sqrt
                            |> Expect.equal Complex.infinity
                , test "sqrt(nan)" <|
                    \() ->
                        Complex.nan
                            |> Complex.sqrt
                            |> Expect.equal Complex.nan
                ]
            , describe "conjugate"
                [ fuzz2 Fuzz.float Fuzz.float "conjugate(finite)" <|
                    \a b ->
                        Complex.complex a b
                            |> Complex.conjugate
                            |> equalComplex
                                (Complex.complex a -b)
                , test "conjugate(infinite)" <|
                    \() ->
                        Complex.infinity
                            |> Complex.conjugate
                            |> Expect.equal Complex.infinity
                , test "conjugate(nan)" <|
                    \() ->
                        Complex.nan
                            |> Complex.conjugate
                            |> Expect.equal Complex.nan
                ]
            ]
        , describe "Conversions"
            [ describe "toString"
                [ fuzz Fuzz.float "real number" <|
                    \x ->
                        Complex.real x
                            |> Complex.toString
                            |> Expect.equal (String.fromFloat x)
                , fuzz Fuzz.float "imaginary number" <|
                    \x ->
                        if x == 0 then
                            Expect.pass

                        else
                            Complex.imaginary x
                                |> Complex.toString
                                |> Expect.equal (String.fromFloat x ++ "i")
                , fuzz fuzzComplex "finite value contains i" <|
                    \c ->
                        let
                            imPart =
                                c |> Complex.toCartesian |> .im
                        in
                        Complex.toString c
                            |> String.contains "i"
                            |> Expect.equal (not <| imPart == 0)
                            |> Expect.onFail
                                ((if imPart == 0 then
                                    "not"

                                  else
                                    ""
                                 )
                                    ++ "contain i"
                                )
                , describe "hand picked test cases"
                    (List.map
                        (\( string, re, im ) ->
                            test string <|
                                \() ->
                                    Complex.complex re im
                                        |> Complex.toString
                                        |> Expect.equal string
                        )
                        stringTests
                    )
                , test "not a number value" <|
                    \() ->
                        Complex.toString Complex.nan
                            |> Expect.equal "nan"
                , test "infinite value" <|
                    \() ->
                        Complex.toString Complex.infinity
                            |> Expect.equal "̃∞"
                ]
            , describe "fromString" <|
                [ fuzz Fuzz.float "real number" <|
                    \x ->
                        String.fromFloat x
                            |> Complex.fromString
                            |> equalJustComplex (Complex.real x)
                , fuzz Fuzz.float "imaginary number" <|
                    \x ->
                        String.fromFloat x
                            ++ "i"
                            |> Complex.fromString
                            |> equalJustComplex (Complex.imaginary x)
                , describe "hand picked test cases"
                    (List.map
                        (\( string, re, im ) ->
                            test string <|
                                \() ->
                                    Complex.fromString string
                                        |> equalJustComplex (Complex.complex re im)
                        )
                        (stringTests
                            ++ [ ( "+i", 0, 1 )
                               , ( "+5", 5, 0 )
                               , ( "+7.2e+3 + 123e+8 i", 7.2e3, 1.23e10 )
                               ]
                        )
                    )
                , describe "whitespace"
                    [ describe "parses allowed whitespace"
                        ([ ( "-6 +   5 i", -6, 5 )
                         , ( "1 + i", 1, 1 )
                         ]
                            |> List.map
                                (\( input, re, im ) ->
                                    test input <|
                                        \() ->
                                            Complex.fromString input
                                                |> equalJustComplex (Complex.complex re im)
                                )
                        )
                    , describe "errors on disallowed whitespace"
                        [ test "within a float" <|
                            \() ->
                                Complex.fromString "5. 2"
                                    |> Expect.equal Nothing
                        , test "within the second float" <|
                            \() ->
                                Complex.fromString "5+4 e2j"
                                    |> Expect.equal Nothing
                        , test "between sign and first float" <|
                            \() ->
                                Complex.fromString "+ 5-2j"
                                    |> Expect.equal Nothing
                        , test "before first float" <|
                            \() ->
                                Complex.fromString " 5-2j"
                                    |> Expect.equal Nothing
                        , test "after last float" <|
                            \() ->
                                Complex.fromString "5-2j "
                                    |> Expect.equal Nothing
                        ]
                    ]
                , describe "invalid input" <|
                    [ test "missing i" <|
                        \() ->
                            Complex.fromString "8 + 2"
                                |> Expect.equal Nothing
                    , test "two i's" <|
                        \() ->
                            Complex.fromString "-8i + 2 i"
                                |> Expect.equal Nothing
                    , test "just a +" <|
                        \() ->
                            Complex.fromString "+"
                                |> Expect.equal Nothing
                    ]
                ]
            , describe "toCartesian" <|
                [ fuzz2 Fuzz.float Fuzz.float "finite complex number" <|
                    \re im ->
                        Complex.complex re im
                            |> Complex.toCartesian
                            |> Expect.all
                                [ .re >> Expect.within tolerance re
                                , .im >> Expect.within tolerance im
                                ]
                , test "infinite complex number" <|
                    \() ->
                        Complex.infinity
                            |> Complex.toCartesian
                            |> Expect.all
                                ([ .re
                                 , .im
                                 ]
                                    |> List.map ((<<) (\x -> x == (1 / 0)))
                                    |> List.map ((<<) (Expect.true "positive infinite number"))
                                )
                , test "nan complex number" <|
                    \() ->
                        Complex.nan
                            |> Complex.toCartesian
                            |> Expect.all
                                ([ .re
                                 , .im
                                 ]
                                    |> List.map ((<<) Basics.isNaN)
                                    |> List.map ((<<) (Expect.true "should be nan"))
                                )
                ]
            , describe "toPolar" <|
                [ fuzz2 Fuzz.float Fuzz.float "finite complex number" <|
                    \re im ->
                        let
                            ( expectedAbs, expectedArg ) =
                                Basics.toPolar ( re, im )
                        in
                        Complex.complex re im
                            |> Complex.toPolar
                            |> Expect.all
                                [ .abs >> Expect.within tolerance expectedAbs
                                , .arg >> Expect.within tolerance expectedArg
                                ]
                , test "infinite complex number" <|
                    \() ->
                        Complex.infinity
                            |> Complex.toPolar
                            |> Expect.all
                                [ .abs >> Expect.within tolerance (1 / 0)
                                , .arg >> Basics.isNaN >> Expect.true "should be nan"
                                ]
                , test "nan complex number" <|
                    \() ->
                        Complex.nan
                            |> Complex.toPolar
                            |> Expect.all
                                ([ .abs
                                 , .arg
                                 ]
                                    |> List.map ((<<) Basics.isNaN)
                                    |> List.map ((<<) (Expect.true "should be nan"))
                                )
                ]
            ]
        ]


stringTests : List ( String, Float, Float )
stringTests =
    [ ( "0", 0, 0 )
    , ( "24+76.5i", 24, 76.5 )
    , ( "-56+2i", -56, 2 )
    , ( "12.3-76i", 12.3, -76 )
    , ( "-89-38i", -89, -38 )
    , ( "1e+25-1.7i", 1.0e25, -1.7 )
    , ( "-1e-53+99i", -1.0e-53, 99 )
    , ( "-5.1-5.2e+42i", -5.1, -5.2e42 )
    , ( "-5+i", -5, 1 )
    , ( "5-i", 5, -1 )
    , ( "i", 0, 1 )
    , ( "-i", 0, -1 )
    ]


fuzzMessyFloat : Fuzzer Float
fuzzMessyFloat =
    Fuzz.oneOf
        [ Fuzz.constant (1 / 0)
        , Fuzz.constant (-1 / 0)
        , Fuzz.constant (0 / 0)
        , Fuzz.float
        ]


fuzzPositive : Fuzzer Float
fuzzPositive =
    Fuzz.float
        |> Fuzz.map abs


fuzzComplex : Fuzzer Complex
fuzzComplex =
    Fuzz.map2 Complex.complex Fuzz.float Fuzz.float


fuzzMessyComplex : Fuzzer Complex
fuzzMessyComplex =
    Fuzz.oneOf
        [ Fuzz.constant Complex.infinity
        , Fuzz.constant Complex.infinity
        , fuzzComplex
        ]


equalComplex : Complex -> Complex -> Expectation
equalComplex expected =
    if Complex.isNan expected then
        Expect.equal Complex.nan

    else if Complex.isInfinite expected then
        Expect.equal Complex.infinity

    else
        \actual ->
            let
                lhs =
                    Complex.toCartesian actual

                rhs =
                    Complex.toCartesian expected
            in
            lhs
                |> Expect.all
                    [ .re >> Expect.within tolerance rhs.re
                    , .im >> Expect.within tolerance rhs.im
                    ]
                |> Expect.onFail ("expected " ++ Complex.toString expected ++ " ~= actual " ++ Complex.toString actual)


equalJustComplex : Complex -> Maybe Complex -> Expectation
equalJustComplex expected =
    \maybeC ->
        case maybeC of
            Just c ->
                equalComplex expected c

            Nothing ->
                Expect.equal (Just expected) Nothing


binaryOperatorNanTest : (Complex -> Complex -> Complex) -> Test.Test
binaryOperatorNanTest func =
    fuzz fuzzMessyComplex "nan propagates" <|
        \c ->
            func
                |> Expect.all
                    [ (\f -> f Complex.nan c)
                        >> Expect.equal Complex.nan
                    , (\f -> f c Complex.nan)
                        >> Expect.equal Complex.nan
                    ]
