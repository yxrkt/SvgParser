using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Windows;
using System.Windows.Media;

namespace SvgParserSample
{
	/// <summary>
	/// Interaction logic for MainWindow.xaml
	/// </summary>
	public partial class MainWindow : Window
	{
		public MainWindow()
		{
			InitializeComponent();

			this.Loaded += (sender, e) => Trace.WriteLineIf(false, this);

			/*
			"M12.4,1.1v80.3a14 14 45 1 0 17,17H122l-33.2-29.2 c-7.4-6.6-25.1-2.9-32.6-9.5c-6.2-5.5-2.2-21.1-8.4-26.6C36.1,22,24.2,11.5,12.4,1.1z"
			 */

			//string moveTo = "M0,0 10,0 10,10 0,10z";
			//var myGeo = GetGeometry(moveTo);
			//var msGeo = Geometry.Parse(moveTo);

			//Trace.WriteLine(myGeo);
			//Trace.WriteLine(msGeo);
		}

		private Geometry GetGeometry(string input)
		{
			var figures =
				from figure in Svg.PathFigureBuilder.BuildFigures(input)
				select new PathFigure(
					figure.StartPoint.AsPoint(),
					figure.Segments.AsPathSegments(),
					figure.IsClosed);
			return new PathGeometry(figures);
		}
	}

	public static class SvgExtensions
	{
		private const bool IsStrokedDefault = true;

		public static Point AsPoint(this Svg.Point self)
		{
			return new Point(self.X, self.Y);
		}

		public static Size AsSize(this Svg.Size self)
		{
			return new Size(self.Width, self.Height);
		}

		public static SweepDirection AsSweepDirection(this Svg.SweepDirection self)
		{
			return (SweepDirection)self.Tag;
		}

		public static IEnumerable<Point> AsPoints(this Svg.Point[] self)
		{
			return self.Select(p => p.AsPoint());
		}

		public static PathSegment AsPathSegment(this Svg.PathSegment self)
		{
			Svg.PathSegment.LineSegment lineSegment;
			Svg.PathSegment.PolyLineSegment polyLineSegment;
			Svg.PathSegment.BezierSegment bezierSegment;
			Svg.PathSegment.PolyBezierSegment polyBezierSegment;
			Svg.PathSegment.QuadraticBezierSegment quadraticBezierSegment;
			Svg.PathSegment.PolyQuadraticBezierSegment polyQuadraticBezierSegment;
			Svg.PathSegment.ArcSegment arcSegment;

			if ((lineSegment = self as Svg.PathSegment.LineSegment) != null)
			{
				return new LineSegment(lineSegment.Point.AsPoint(), IsStrokedDefault);
			}
			else if ((polyLineSegment = self as Svg.PathSegment.PolyLineSegment) != null)
			{
				return new PolyLineSegment(polyLineSegment.Points.AsPoints(), IsStrokedDefault);
			}
			else if ((bezierSegment = self as Svg.PathSegment.BezierSegment) != null)
			{
				return new BezierSegment(
					bezierSegment.Point1.AsPoint(),
					bezierSegment.Point2.AsPoint(),
					bezierSegment.Point3.AsPoint(),
					IsStrokedDefault);
			}
			else if ((polyBezierSegment = self as Svg.PathSegment.PolyBezierSegment) != null)
			{
				return new PolyBezierSegment(polyBezierSegment.Points.AsPoints(), IsStrokedDefault);
			}
			else if ((quadraticBezierSegment = self as Svg.PathSegment.QuadraticBezierSegment) != null)
			{
				return new QuadraticBezierSegment(
					quadraticBezierSegment.Point1.AsPoint(),
					quadraticBezierSegment.Point2.AsPoint(),
					IsStrokedDefault);
			}
			else if ((polyQuadraticBezierSegment = self as Svg.PathSegment.PolyQuadraticBezierSegment) != null)
			{
				return new PolyQuadraticBezierSegment(
					polyQuadraticBezierSegment.Points.AsPoints(),
					IsStrokedDefault);
			}
			else if ((arcSegment = self as Svg.PathSegment.ArcSegment) != null)
			{
				return new ArcSegment(
					arcSegment.Point.AsPoint(),
					arcSegment.Size.AsSize(),
					arcSegment.RotationAngle,
					arcSegment.IsLargeArc,
					arcSegment.SweepDirection.AsSweepDirection(),
					IsStrokedDefault);
			}
			else
			{
				throw new Exception("fuckin thing sucks");
			}
		}

		public static IEnumerable<PathSegment> AsPathSegments(this Svg.PathSegment[] self)
		{
			return self.Select(segment => segment.AsPathSegment());
		}
	}
}
