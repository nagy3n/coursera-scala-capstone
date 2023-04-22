package observatory

import com.sksamuel.scrimage.ImmutableImage
import com.sksamuel.scrimage.pixels.Pixel
import com.sksamuel.scrimage.metadata.ImageMetadata
import scala.collection.parallel.CollectionConverters.given

/**
  * 3rd milestone: interactive visualization
  */
object Interaction extends InteractionInterface:

  /**
    * @param tile Tile coordinates
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(tile: Tile): Location =
    val n = math.pow(2, tile.zoom)
    val lon_deg = (tile.x * 360.0) / n - 180.0
    val lat_rad = math.atan(math.sinh(math.Pi * (1 - (2 * tile.y) / n)))
    val lat_deg = math.toDegrees(lat_rad)
    Location(lat_deg, lon_deg)


  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @param tile Tile coordinates
    * @return A 256×256 image showing the contents of the given tile
    */
  def tile(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)], tile: Tile): ImmutableImage =
    val (diffX, diffY) = ((tile.x * math.pow(2, 8)).toInt, (tile.y * math.pow(2, 8)).toInt)
    val pixels = (for {
      j <- (0 until 256).par
      i <- 0 until 256
    } yield {
      (i, j, Visualization.interpolateColor(
              colors, 
              Visualization.predictTemperature(temperatures, tileLocation(Tile(i + diffX, j + diffY, 8 + tile.zoom)))
              )
      )
    }).map((x, y, color) => Pixel(x, y, color.red, color.green, color.blue, 255))

    ImmutableImage.wrapPixels(256, 256, pixels.toArray, ImageMetadata.empty)

    

  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    * @param yearlyData Sequence of (year, data), where `data` is some data associated with
    *                   `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](
    yearlyData: Iterable[(Year, Data)],
    generateImage: (Year, Tile, Data) => Unit
  ): Unit =
    for {
      zoom <- 0 to 3
      y <- 0 until math.pow(2, zoom).toInt
      x <- 0 until math.pow(2, zoom).toInt
      (year, data) <- yearlyData
    } yield generateImage(year, Tile(x, y, zoom), data)

