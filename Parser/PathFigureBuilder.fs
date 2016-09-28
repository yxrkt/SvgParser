namespace Svg

type SweepDirection =
    | Counterclockwise
    | Clockwise

type PathSegment =
    | LineSegment of Point : Point
    | PolyLineSegment of Points : Point array
    | BezierSegment of Point1 : Point * Point2 : Point * Point3 : Point
    | PolyBezierSegment of Points : Point array
    | QuadraticBezierSegment of Point1 : Point * Point2 : Point
    | PolyQuadraticBezierSegment of Points : Point array
    | ArcSegment of Point : Point * Size : Size * RotationAngle : float * IsLargeArc : bool * SweepDirection : SweepDirection

type PathFigure =
    { IsClosed : bool;
      StartPoint : Point;
      Segments : PathSegment array }

module PathFigureBuilder =

    let private transformPoint isAbsolute location coord =
        if isAbsolute then coord
        else location + coord

    let private reflectPoint origin point =
        { X = origin.X + (origin.X - point.X)
          Y = origin.Y + (origin.Y - point.Y) }

    let (|LineSegment|_|) location = function
        | Command.LineTo (isAbsolute, point)::rest ->
            let point = transformPoint isAbsolute location point
            Some (point, rest)
        | Command.HorizontalLineTo (isAbsolute, x)::rest ->
            let x = if isAbsolute then x else location.X + x
            Some ({ location with X = x }, rest)
        | Command.VerticalLineTo (isAbsolute, y)::rest ->
            let y = if isAbsolute then y else location.Y + y
            Some ({ location with Y = y }, rest)
        | _ -> None

    let rec (|LineSegmentList|_|) location = function
        | LineSegment location (point, rest) ->
            match rest with
            | LineSegmentList point (tail, rest) ->
                Some (point::tail, rest)
            | _ -> Some ([point], rest)
        | _ -> None

    let (|LineSegments|_|) location = function
        | LineSegmentList location ([point], rest) ->
            Some (point, PathSegment.LineSegment point, rest)
        | LineSegmentList location (points, rest) ->
            let points = points |> List.toArray
            Some (points |> Array.last, PathSegment.PolyLineSegment points, rest)
        | _ -> None

    let (|BezierSegment|_|) location lastPoint2 = function
        | Command.CurveTo (isAbsolute, point1, point2, point3)::rest ->
            let transform = transformPoint isAbsolute location
            let point1 = point1 |> transform
            let point2 = point2 |> transform
            let point3 = point3 |> transform
            Some ((point1, point2, point3), rest)
        | Command.SmoothCurveTo (isAbsolute, point2, point3)::rest ->
            let transform = transformPoint isAbsolute location
            let point1 = lastPoint2 |> reflectPoint location
            let point2 = point2 |> transform
            let point3 = point3 |> transform
            Some ((point1, point2, point3), rest)
        | _ -> None

    let rec (|BezierSegmentList|_|) location lastPoint2 = function
        | BezierSegment location lastPoint2 ((point1, point2, point3) as points, rest) ->
            match rest with
            | BezierSegmentList point3 point2 (tail, rest) ->
                Some (points::tail, rest)
            | _ -> Some ([points], rest)
        | _ -> None

    let (|BezierSegments|_|) location =
        function
        | BezierSegmentList location location ([(_, _, point3) as points], rest) ->
            Some (point3, PathSegment.BezierSegment points, rest)
        | BezierSegmentList location location (pointsList, rest) ->
            let points =
                pointsList
                |> List.collect (fun (point1, point2, point3) -> [point1; point2; point3])
                |> List.toArray
            Some (points |> Array.last, PathSegment.PolyBezierSegment points, rest)
        | _ -> None

    let (|QuadraticBezierSegment|_|) location lastPoint1 = function
        | Command.QuadraticCurveTo (isAbsolute, point1, point2)::rest ->
            let transform = transformPoint isAbsolute location
            let point1 = point1 |> transform
            let point2 = point2 |> transform
            Some ((point1, point2), rest)
        | Command.SmoothQuadraticCurveTo (isAbsolute, point2)::rest ->
            let transform = transformPoint isAbsolute location
            let point1 = lastPoint1 |> reflectPoint location
            let point2 = point2 |> transform
            Some ((point1, point2), rest)
        | _ -> None

    let rec (|QuadraticBezierSegmentList|_|) location lastPoint1 = function
        | QuadraticBezierSegment location lastPoint1 ((point1, point2) as points, rest) ->
            match rest with
            | QuadraticBezierSegmentList point2 point1 (tail, rest) ->
                Some (points::tail, rest)
            | _ -> Some ([points], rest)
        | _ -> None

    let (|QuadraticBezierSegments|_|) location = function
        | QuadraticBezierSegmentList location location ([(_, point2) as points], rest) ->
            Some (point2, PathSegment.QuadraticBezierSegment points, rest)
        | QuadraticBezierSegmentList location location (pointsList, rest) ->
            let points =
                pointsList
                |> List.collect (fun (point1, point2) -> [point1; point2])
                |> List.toArray
            Some (points |> Array.last, PathSegment.PolyQuadraticBezierSegment points, rest)
        | _ -> None

    let (|ArcSegment|_|) location = function
        | Command.EllipticalArc (isAbsolute, size, rotation, largeArcFlag, sweepFlag, point)::rest ->
            let point = transformPoint isAbsolute location point
            let sweep = if sweepFlag then Clockwise else Counterclockwise
            Some (point, PathSegment.ArcSegment (point, size, rotation, largeArcFlag, sweep), rest)
        | _ -> None

    let (|PathSegment|_|) location = function
        | LineSegments location (location2, segment, rest)
        | BezierSegments location (location2, segment, rest)
        | QuadraticBezierSegments location (location2, segment, rest)
        | ArcSegment location (location2, segment, rest) ->
            Some (location2, segment, rest)
        | _ -> None

    let rec (|PathSegmentList|) location = function
        | PathSegment location (location2, segment, rest) ->
            match rest with
            | PathSegmentList location2 (location3, tail, rest) ->
                (location3, segment::tail, rest)
        | rest -> (location, [], rest)

    let (|OptionalClosePath|) = function
        | ClosePath _::rest -> (true, rest)
        | rest -> (false, rest)

    let (|PathFigure|_|) location = function
        | MoveTo (isAbsolute, point)::rest ->
            let startPoint = transformPoint isAbsolute location point
            match rest with
            | PathSegmentList startPoint (location2, segments, OptionalClosePath (isClosed,rest)) ->
                let segments = segments |> List.toArray
                Some (location2, { IsClosed = isClosed; StartPoint = startPoint; Segments = segments }, rest)
        | _ -> None

    let rec (|PathFigureGroup|_|) location = function
        | PathFigure location (location2, figure, rest) ->
            match rest with
            | PathFigureGroup location2 (location3, tail, rest) ->
                Some (location3, figure::tail, rest)
            | rest ->
                Some (location2, [figure], rest)
        | _ -> None

    let rec (|PathFigureCollection|) = function
        | PathFigureGroup Point.Zero (_, group1, [])::(PathFigureCollection (group2, rest)) ->
            group1 @ group2, rest
        | PathFigureGroup Point.Zero (_, group, [])::rest ->
            group, rest
        | rest -> [], rest

    let BuildFigures (input : string) =
        match Parser.parse input with
        | PathFigureCollection (figures, []) ->
            figures |> List.toArray
        | unconsumed ->
            failwithf "Building path figures failed: Invalid command sequence.\n\t %A" unconsumed
