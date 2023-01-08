package observatory

import com.sksamuel.scrimage.ImmutableImage
import com.sksamuel.scrimage.pixels.Pixel
import com.sksamuel.scrimage.metadata.ImageMetadata
import com.sksamuel.scrimage.implicits.given
import scala.collection.parallel.CollectionConverters.given
import scala.math._

/**
  * 2nd milestone: basic visualization
  */
object Visualization extends VisualizationInterface:

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Temperature)], location: Location): Temperature =
    val distsTempratures = temperatures.map((loc, temperature) => {
      if location == loc then (0.0, temperature)
      else if location.lat == -loc.lat && abs(location.lon - loc.lon) == 180 then (6371.0 * Pi, temperature)
      else
        val deltalon = abs(loc.lon - location.lon)
        val delta = acos(sin(toRadians(loc.lat)) * sin(toRadians(location.lat)) + cos(toRadians(loc.lat)) * cos(toRadians(location.lat)) * cos(toRadians(deltalon)))
        (6371.0 * delta, temperature)
    })
    
    distsTempratures.find((dist, _) => dist < 1) match
      case Some(value) => value._2
      case None => 
        val (a, b) = distsTempratures.foldLeft((0.0, 0.0))((acc, dt) => {
          val w = 1.0 / pow(dt._1, 6)
          (acc._1 + w * dt._2, acc._2 + w)
        })
        a / b
  /**
    * @param points Pairs containing a value and its associated color
    * @param value The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Temperature, Color)], value: Temperature): Color =
      val sortedPoints = points.toList.sortBy(_._1)
      sortedPoints.zip(sortedPoints.tail).find((p1, p2) => value >= p1._1 && value < p2._1) match
        case Some(((t, c), _)) if t == value => c
        case Some((t1, c1), (t2, c2)) => 
          val ratio = (value - t1) / (t2 - t1)
          Color(
            math.round(c1.red + (c2.red - c1.red) * ratio).toInt,
            math.round(c1.green + (c2.green - c1.green) * ratio).toInt,
            math.round(c1.blue + (c2.blue - c1.blue) * ratio).toInt
          )
        case None if value < sortedPoints(0)._1  => sortedPoints(0)._2
        case None => sortedPoints(sortedPoints.length - 1)._2
      


  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)]): ImmutableImage =
    val pixels = (for {
      x <- 0 until 360
      y <- 0 until 180
    } yield {
        (x, y, interpolateColor(
                colors, 
                predictTemperature(temperatures, Location(90 - y, x - 180))
               )
        )
      }
    )
    .map((x, y, color) => Pixel(x, y, color.red, color.green, color.blue, 255))

    ImmutableImage.wrapPixels(360, 180, pixels.toArray, ImageMetadata.empty)