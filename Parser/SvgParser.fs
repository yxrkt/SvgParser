namespace Svg

open System

type Point = { X : float; Y : float } with
    static member Zero = { X = 0.0; Y = 0.0 }
    static member (+) (a, b) = { X = a.X + b.X; Y = a.Y + b.Y }
type Size = { Width : float; Height : float }

type Command =
    | MoveTo of IsAbsolute : bool * Point : Point
    | ClosePath
    | LineTo of IsAbsolute : bool * Point : Point
    | HorizontalLineTo of IsAbsolute : bool * X : float
    | VerticalLineTo of IsAbsolute : bool * Y : float
    | CurveTo of IsAbsolute : bool * Point1 : Point * Point2 : Point * Point3 : Point
    | SmoothCurveTo of IsAbsolute : bool * Point1 : Point * Point2 : Point
    | QuadraticCurveTo of IsAbsolute : bool * Point1 : Point * Point2 : Point
    | SmoothQuadraticCurveTo of IsAbsolute : bool * Point : Point
    | EllipticalArc of IsAbsolute : bool * Size : Size * Rotation : float * LargeArcFlag : bool * SweepFlag : bool * Point : Point

module Parser =

    let internal toFloat (input : char list) = new string (input |> List.toArray) |> Double.Parse

    let (|Wsp|_|) = function
        | s::rest when Char.IsWhiteSpace s -> Some rest
        | _ -> None

    let rec (|WspSequence|) = function
        | Wsp (WspSequence rest) -> rest
        | rest -> rest

    let (|MatchOne|_|) (names : char list) = function
        | a::rest when List.contains a names -> Some (a, rest)
        | _ -> None

    let (|Digit|_|) = function
        | d::rest when Char.IsDigit d -> Some (d, rest)
        | _ -> None

    let rec (|DigitSequence|_|) = function
        | Digit (head, DigitSequence (tail, rest)) -> Some (head::tail, rest)
        | Digit (d, rest) -> Some ([d], rest)
        | _ -> None

    let (|Sign|_|) = function
        | MatchOne ['+'; '-'] (sign, rest) -> Some (sign, rest)
        | _ -> None

    let (|Exponent|_|) = function
        | MatchOne ['e'; 'E'] (e, Sign (sign, DigitSequence (digits, rest)))
            -> Some (e::sign::digits, rest)
        | MatchOne ['e'; 'E'] (e, DigitSequence (digits, rest))
            -> Some (e::digits, rest)
        | _ -> None

    let (|FractionalConstant|_|) = function
        | DigitSequence (whole, '.'::DigitSequence (fraction, rest))
            -> Some (whole @ '.'::fraction, rest)
        | '.'::DigitSequence (fraction, rest)
            -> Some ('.'::fraction, rest)
        | DigitSequence (whole, '.'::rest)
            -> Some (whole, rest)
        | _ -> None

    let (|FloatingPointConstant|_|) = function
        | FractionalConstant (constant, Exponent (exp, rest))
            -> Some (constant @ exp, rest)
        | FractionalConstant (constant, rest)
            -> Some (constant, rest)
        | DigitSequence (digits, Exponent (exp, rest))
            -> Some (digits @ exp, rest)
        | _ -> None

    let (|IntegerConstant|_|) = (|DigitSequence|_|)

    let (|CommaWsp|_|) = function
        | Wsp (WspSequence (','::WspSequence rest)) -> Some rest
        | Wsp (WspSequence rest) -> Some rest
        | ','::WspSequence rest -> Some rest
        | _ -> None

    let (|OptionalCommaWsp|) = function
        | WspSequence (','::WspSequence rest) -> rest
        | WspSequence rest -> rest

    let (|Flag|_|) = function
        | '0'::rest -> Some (false, rest)
        | '1'::rest -> Some (true, rest)
        | _ -> None

    let (|Number|_|) = function
        | Sign (sign, FloatingPointConstant (number, rest))
            -> Some (sign::number |> toFloat, rest)
        | FloatingPointConstant (number, rest)
            -> Some (number |> toFloat, rest)
        | Sign (sign, IntegerConstant (number, rest))
            -> Some (sign::number |> toFloat, rest)
        | IntegerConstant (number, rest)
            -> Some (number |> toFloat, rest)
        | _ -> None

    let (|NonnegativeNumber|_|) = function
        | IntegerConstant (number, rest)
        | FloatingPointConstant (number, rest)
            -> Some (number |> toFloat, rest)
        | _ -> None

    let (|Coordinate|_|) = (|Number|_|)

    let (|CoordinatePair|_|) = function
        | Coordinate (x, OptionalCommaWsp (Coordinate (y, rest)))
        | Coordinate (x, Coordinate (y, rest))
            -> Some ({ X = x; Y = y }, rest)
        | _ -> None

    let (|EllipticalArcArgument|_|) isAbsolute = function
        | NonnegativeNumber (width, OptionalCommaWsp (NonnegativeNumber (height, OptionalCommaWsp (Number (angle, CommaWsp (Flag (largeArc, OptionalCommaWsp (Flag (sweepCW, OptionalCommaWsp (CoordinatePair (point, rest)))))))))))
            -> Some (Command.EllipticalArc (isAbsolute, { Width = width; Height = height }, angle, largeArc, sweepCW, point), rest)
        | _ -> None

    let rec (|EllipticalArcArgumentSequence|_|) isAbsolute = function
        | EllipticalArcArgument isAbsolute (command, OptionalCommaWsp (EllipticalArcArgumentSequence isAbsolute (seq, rest)))
            -> Some (command::seq, rest)
        | EllipticalArcArgument isAbsolute (command, rest)
            -> Some ([command], rest)
        | _ -> None

    let (|EllipticalArc|_|) = function
        | 'A'::WspSequence (EllipticalArcArgumentSequence true result)
        | 'a'::WspSequence (EllipticalArcArgumentSequence false result)
            -> Some result
        | _ -> None

    let rec (|SmoothQuadraticCurveToArgumentSequence|_|) isAbsolute = function
        | CoordinatePair (point, OptionalCommaWsp (SmoothQuadraticCurveToArgumentSequence isAbsolute (tail, rest)))
            -> Some ((Command.SmoothQuadraticCurveTo (isAbsolute, point))::tail, rest)
        | CoordinatePair (point, rest)
            -> Some ([Command.SmoothQuadraticCurveTo (isAbsolute, point)], rest)
        | _ -> None

    let (|SmoothQuadraticCurveTo|_|) = function
        | 'T'::WspSequence (SmoothQuadraticCurveToArgumentSequence true result)
        | 't'::WspSequence (SmoothQuadraticCurveToArgumentSequence false result)
            -> Some result
        | _ -> None

    let (|QuadraticCurveToArgument|_|) isAbsolute = function
        | CoordinatePair (point1, OptionalCommaWsp (CoordinatePair (point2, rest)))
            -> Some (Command.QuadraticCurveTo (isAbsolute, point1, point2), rest)
        | _ -> None

    let rec (|QuadraticCurveToArgumentSequence|_|) isAbsolute = function
        | QuadraticCurveToArgument isAbsolute (command, OptionalCommaWsp (QuadraticCurveToArgumentSequence isAbsolute (tail, rest)))
            -> Some (command::tail, rest)
        | QuadraticCurveToArgument isAbsolute (command, rest)
            -> Some ([command], rest)
        | _ -> None

    let (|QuadraticCurveTo|_|) = function
        | 'Q'::WspSequence (QuadraticCurveToArgumentSequence true result)
        | 'q'::WspSequence (QuadraticCurveToArgumentSequence false result)
            -> Some result
        | _ -> None

    let (|SmoothCurveToArgument|_|) isAbsolute = function
        | CoordinatePair (point1, OptionalCommaWsp (CoordinatePair (point2, rest)))
            -> Some (Command.SmoothCurveTo (isAbsolute, point1, point2), rest)
        | _ -> None

    let rec (|SmoothCurveToArgumentSequence|_|) isAbsolute = function
        | SmoothCurveToArgument isAbsolute (command, OptionalCommaWsp (SmoothCurveToArgumentSequence isAbsolute (tail, rest)))
            -> Some (command::tail, rest)
        | SmoothCurveToArgument isAbsolute (command, rest)
            -> Some ([command], rest)
        | _ -> None

    let (|SmoothCurveTo|_|) = function
        | 'S'::WspSequence (SmoothCurveToArgumentSequence true result)
        | 's'::WspSequence (SmoothCurveToArgumentSequence false result)
            -> Some result
        | _ -> None

    let (|CurveToArgument|_|) isAbsolute = function
        | CoordinatePair (point1, OptionalCommaWsp (CoordinatePair (point2, OptionalCommaWsp (CoordinatePair (point3, rest)))))
            -> Some (Command.CurveTo (isAbsolute, point1, point2, point3), rest)
        | _ -> None

    let rec (|CurveToArgumentSequence|_|) isAbsolute = function
        | CurveToArgument isAbsolute (command, OptionalCommaWsp (CurveToArgumentSequence isAbsolute (tail, rest)))
            -> Some (command::tail, rest)
        | CurveToArgument isAbsolute (command, rest)
            -> Some ([command], rest)
        | _ -> None

    let (|CurveTo|_|) = function
        | 'C'::WspSequence (CurveToArgumentSequence true result)
        | 'c'::WspSequence (CurveToArgumentSequence false result)
            -> Some result
        | _ -> None

    let rec (|VerticalLineToArgumentSequence|_|) isAbsolute = function
        | Coordinate (y, OptionalCommaWsp (VerticalLineToArgumentSequence isAbsolute (tail, rest)))
            -> Some ((Command.VerticalLineTo (isAbsolute, y))::tail, rest)
        | Coordinate (y, rest)
            -> Some ([Command.VerticalLineTo (isAbsolute, y)], rest)
        | _ -> None

    let (|VerticalLineTo|_|) = function
        | 'V'::WspSequence (VerticalLineToArgumentSequence true result)
        | 'v'::WspSequence (VerticalLineToArgumentSequence false result)
            -> Some result
        | _ -> None

    let rec (|HorizontalLineToArgumentSequence|_|) isAbsolute = function
        | Coordinate (x, OptionalCommaWsp (VerticalLineToArgumentSequence isAbsolute (tail, rest)))
            -> Some ((Command.HorizontalLineTo (isAbsolute, x))::tail, rest)
        | Coordinate (x, rest)
            -> Some ([Command.HorizontalLineTo (isAbsolute, x)], rest)
        | _ -> None

    let (|HorizontalLineTo|_|) = function
        | 'H'::WspSequence (HorizontalLineToArgumentSequence true result)
        | 'h'::WspSequence (HorizontalLineToArgumentSequence false result)
            -> Some result
        | _ -> None

    let rec (|LineToArgumentSequence|_|) isAbsolute = function
        | CoordinatePair (point, OptionalCommaWsp (LineToArgumentSequence isAbsolute (tail, rest)))
            -> Some ((Command.LineTo (isAbsolute, point))::tail, rest)
        | CoordinatePair (point, rest)
            -> Some ([Command.LineTo (isAbsolute, point)], rest)
        | _ -> None

    let (|LineTo|_|) = function
        | 'L'::WspSequence (LineToArgumentSequence true result)
        | 'l'::WspSequence (LineToArgumentSequence false result)
            -> Some result
        | _ -> None

    let (|ClosePath|_|) = function
        | 'Z'::rest
        | 'z'::rest
            -> Some ([Command.ClosePath], rest)
        | _ -> None

    let (|MoveToArgumentSequence|_|) isAbsolute = function
        | CoordinatePair (point, OptionalCommaWsp (LineToArgumentSequence isAbsolute (tail, rest)))
            -> Some ((Command.MoveTo (isAbsolute, point))::tail, rest)
        | CoordinatePair (point, rest)
            -> Some ([Command.MoveTo (isAbsolute, point)], rest)
        | _ -> None

    let (|MoveTo|_|) = function
        | 'M'::WspSequence (MoveToArgumentSequence true result)
        | 'm'::WspSequence (MoveToArgumentSequence false result)
            -> Some result
        | _ -> None

    let (|DrawToCommand|_|) = function
        | ClosePath result
        | LineTo result
        | HorizontalLineTo result
        | VerticalLineTo result
        | CurveTo result
        | SmoothCurveTo result
        | QuadraticCurveTo result
        | SmoothQuadraticCurveTo result
        | EllipticalArc result
            -> Some result
        | _ -> None

    let rec (|DrawToCommands|_|) = function
        | DrawToCommand (seq1, WspSequence (DrawToCommands (seq2, rest)))
            -> Some (seq1 @ seq2, rest)
        | DrawToCommand result
            -> Some result
        | _ -> None

    let (|MoveToDrawToCommandGroup|_|) = function
        | MoveTo (seq1, WspSequence (DrawToCommands (seq2, rest)))
            -> Some (seq1 @ seq2, rest)
        | MoveTo (seq, WspSequence rest)
            -> Some (seq, rest)
        | _ -> None

    let rec (|MoveToDrawToCommandGroups|_|) = function
        | MoveToDrawToCommandGroup (group, WspSequence (MoveToDrawToCommandGroups (tail, rest)))
            -> Some (group::tail, rest)
        | MoveToDrawToCommandGroup (group, rest)
            -> Some ([group], rest)
        | _ -> None

    let (|SvgPath|) = function
        | WspSequence (MoveToDrawToCommandGroups (groups, WspSequence rest))
            -> groups, rest
        | WspSequence rest
            -> [], rest

    let internal parse (input : string) =
        match input.ToCharArray () |> Array.toList with
        | SvgPath (groups, []) -> groups
        | unparsed -> failwithf "Invalid input to SVG parser.\n\t %s" (new string (unparsed |> List.toArray))

    let Parse (input : string) =
        input
        |> parse
        |> List.map (fun list -> list |> List.toArray)
        |> List.toArray