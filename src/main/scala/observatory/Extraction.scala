package observatory

import java.time.LocalDate
import scala.io.Source
import scala.util.Try

/**
  * 1st milestone: data extraction
  */

object Extraction extends ExtractionInterface:
  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Year, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Temperature)] =
      val stations = prepareStations(stationsFile).toMap
      val temperatures = prepareTemperatures(year, temperaturesFile)
      temperatures.map((key, date, temp) => {
        val stationLocation = stations.getOrElse(key, null)
        (date, stationLocation, temp)  
      }).filter(_._2 != null)
      

      

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Temperature)]): Iterable[(Location, Temperature)] =
    records.map((_, location, temp) => location -> temp).groupMap(_._1)(_._2).view.mapValues(temps => {
      val (s, c) = temps.foldLeft((0.0, 0))((acc, v) => (acc._1 + v, acc._2 + 1))
      s / c
    })

  def prepareStations(stationsFile: String): Iterable[((String, String), Location)] = 
    Source.fromInputStream(getClass.getResourceAsStream(stationsFile), "utf-8").getLines().toList
      .map(l => l.split(",")).filter(r => r.length == 4).map(r => Try(((r(0), r(1)), Location(r(2).toDouble, r(3).toDouble))))
      .filter(r => r.isSuccess).map(r => r.get)

  def prepareTemperatures(year: Year, temperaturesFile: String): Iterable[((String, String), LocalDate, Temperature)] =
    def toCelcius(f: Double): Double = (f - 32.0) * (5.0/9.0)

    Source.fromInputStream(getClass.getResourceAsStream(temperaturesFile), "utf-8").getLines().toList
      .map(l => l.split(",")).filter(r => r.length == 5).map(r => Try(((r(0), r(1)), LocalDate.of(year, r(2).toInt, r(3).toInt), toCelcius(r(4).toDouble))))
      .filter(r => r.isSuccess).map(r => r.get)