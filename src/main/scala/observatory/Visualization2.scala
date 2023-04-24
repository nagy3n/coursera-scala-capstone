package observatory

import com.sksamuel.scrimage.ImmutableImage
import com.sksamuel.scrimage.pixels.Pixel
import com.sksamuel.scrimage.metadata.ImageMetadata
import scala.collection.parallel.CollectionConverters.given

/**
  * 5th milestone: value-added information visualization
  */
object Visualization2 extends Visualization2Interface:

  /**
    * @param point (x, y) coordinates of a point in the grid cell
    * @param d00 Top-left value
    * @param d01 Bottom-left value
    * @param d10 Top-right value
    * @param d11 Bottom-right value
    * @return A guess of the value at (x, y) based on the four known values, using bilinear interpolation
    *         See https://en.wikipedia.org/wiki/Bilinear_interpolation#Unit_Square
    */
  def bilinearInterpolation(
    point: CellPoint,
    d00: Temperature,
    d01: Temperature,
    d10: Temperature,
    d11: Temperature
  ): Temperature =
    d00 * (1 - point.x) * (1 - point.y) + d01 * (1 - point.x) * point.y + d10 * point.x * (1 - point.y) + d11 * point.x * point.y

  /**
    * @param grid Grid to visualize
    * @param colors Color scale to use
    * @param tile Tile coordinates to visualize
    * @return The image of the tile at (x, y, zoom) showing the grid using the given color scale
    */
  def visualizeGrid(
    grid: GridLocation => Temperature,
    colors: Iterable[(Temperature, Color)],
    tile: Tile
  ): ImmutableImage =
    val (diffX, diffY) = ((tile.x * math.pow(2, 8)).toInt, (tile.y * math.pow(2, 8)).toInt)
    val pixels = (for {
      j <- (0 until 256).par
      i <- 0 until 256
    } yield {
      val location = Interaction.tileLocation(Tile(i + diffX, j + diffY, 8 + tile.zoom))

      val (lat0, lat1) = (math.floor(location.lat).toInt, math.ceil(location.lat).toInt)
      val (lon0, lon1) = (math.floor(location.lon).toInt, math.ceil(location.lon).toInt)
      val cellPoint = CellPoint(location.lon - lon0, lat1 - location.lat)
      val (d00, d01, d10, d11) = (
        grid(GridLocation(lat1, lon0)),
        grid(GridLocation(lat0, lon0)),
        grid(GridLocation(lat1, lon1)),
        grid(GridLocation(lat0, lon1))
      )

      (i, j, Visualization.interpolateColor(
              colors, 
              bilinearInterpolation(cellPoint, d00, d01, d10, d11)
            )
      )
    }).map((x, y, color) => Pixel(x, y, color.red, color.green, color.blue, 255))

    ImmutableImage.wrapPixels(256, 256, pixels.toArray, ImageMetadata.empty)

