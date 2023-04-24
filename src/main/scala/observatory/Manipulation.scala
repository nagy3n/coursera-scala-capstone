package observatory

import scala.collection.parallel.CollectionConverters.given

/**
  * 4th milestone: value-added information
  */
object Manipulation extends ManipulationInterface:

  /**
    * @param temperatures Known temperatures
    * @return A function that, given a latitude in [-89, 90] and a longitude in [-180, 179],
    *         returns the predicted temperature at this location
    */
  def makeGrid(temperatures: Iterable[(Location, Temperature)]): GridLocation => Temperature =
    gridLocation => Visualization.predictTemperature(temperatures, Location(gridLocation.lat, gridLocation.lon))

  /**
    * @param temperaturess Sequence of known temperatures over the years (each element of the collection
    *                      is a collection of pairs of location and temperature)
    * @return A function that, given a latitude and a longitude, returns the average temperature at this location
    */
  def average(temperaturess: Iterable[Iterable[(Location, Temperature)]]): GridLocation => Temperature =
    gridLocation => 
      val temps = for {
        temperatures <- temperaturess
      } yield makeGrid(temperatures)(gridLocation)
      temps.reduce(_ + _) / temps.size

  /**
    * @param temperatures Known temperatures
    * @param normals A grid containing the “normal” temperatures
    * @return A grid containing the deviations compared to the normal temperatures
    */
  def deviation(temperatures: Iterable[(Location, Temperature)], normals: GridLocation => Temperature): GridLocation => Temperature =
    gridLocation =>
      val gridTemperature = makeGrid(temperatures)(gridLocation)
      val normalTemperature = normals(gridLocation)
      gridTemperature - normalTemperature


// private def memoizeFnc[K, V](f: K => V): K => V = {
//   val cache = collection.mutable.Map.empty[K, V]

//   k =>
//     cache.getOrElse(k, {
//       cache update(k, f(k))
//       cache(k)
//     })
// }
