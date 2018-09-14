module Internal.Complex exposing (Complex, complex, toCartesian, toPolar)


type Complex
    = Complex
        { re : Float
        , im : Float
        }
    | ComplexNan
    | ComplexInfinity


complex : Float -> Float -> Complex
complex re im =
    if Basics.isNaN re || Basics.isNaN im then
        ComplexNan

    else if Basics.isInfinite re || Basics.isInfinite im then
        ComplexInfinity

    else
        Complex
            { re = re
            , im = im
            }


toCartesian : Complex -> { re : Float, im : Float }
toCartesian c =
    case c of
        Complex cartesian ->
            cartesian

        ComplexNan ->
            cartesianNan

        ComplexInfinity ->
            cartesianInfinity


toPolar : Complex -> { abs : Float, arg : Float }
toPolar c =
    case c of
        Complex { re, im } ->
            { abs = Basics.sqrt (re ^ 2 + im ^ 2)
            , arg = Basics.atan2 im re
            }

        ComplexNan ->
            polarNan

        ComplexInfinity ->
            polarInfinity


cartesianNan : { re : Float, im : Float }
cartesianNan =
    { re = 0 / 0
    , im = 0 / 0
    }


cartesianInfinity : { re : Float, im : Float }
cartesianInfinity =
    { re = 1 / 0
    , im = 1 / 0
    }


polarNan : { abs : Float, arg : Float }
polarNan =
    { abs = 0 / 0
    , arg = 0 / 0
    }


polarInfinity : { abs : Float, arg : Float }
polarInfinity =
    { abs = 1 / 0
    , arg = 0 / 0
    }
