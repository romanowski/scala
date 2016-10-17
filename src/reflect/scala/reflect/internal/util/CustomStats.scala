package scala.reflect.internal.util

import java.io.File
import java.io.FileOutputStream
import java.util.Properties

import scala.reflect.internal.settings.MutableSettings

class CustomStats (settings: MutableSettings) {
  private var stats: Map[String, Iterable[(String, Any)]] = Map.empty

  var enable = true

  def currentTime = if (enable) System.nanoTime() else -1L

  var macrosTimes: Map[String, Long] = Map.empty
  var macrosCounts: Map[String, Long] = Map.empty
  var classLoadingTime: Long = -1
  var packagesLoadingTime: Long = -1
  var packagesListTime: Long = -1
  var classesLoadingTime: Long = -1
  var classpathCreationTime: Long = -1
  var loadingClassBytes: Long = -1
  var innerClassesLoading: Long = -1

  def put(name: String, value: Iterable[(String, Any)]) = {
    stats += name -> value
  }
  def put(name: String, value: Any) = {
    stats += name -> Map(name -> value)
  }

  def store(output: File) = { // TODO add proper settings with custom stats location
    put("macros.times", macrosTimes.toSeq.sortBy(- _._2))
    put("macros.counts", macrosCounts.toSeq.sortBy(- _._2))
    put("packagesLoading.time", packagesLoadingTime)
    put("packagesListing.time", packagesListTime)
    put("classesLoading.time", classLoadingTime)
    put("classpathCreation.time", classpathCreationTime)
    put("classBytesLoading.time", loadingClassBytes)
    put("innerClassesLoading.time", innerClassesLoading)

    def printMap(m: Iterable[(String, Any)]) = m.map {
      case (k, v) => s"\t$k\t$v"
    }.mkString("\n")

    val orderedStats = stats.toSeq.sortBy(_._1)

    val content = orderedStats.map {
      case (key, values) =>
        if (values.size == 1) s"$key\t${ values.head._2 }"
        else s"$key\n${ printMap(values) }"
    }.mkString("\n")

    output match {
      case null =>
      case _ if output.isDirectory =>
        val fileName = s"${output.getParentFile.getName}-${output.getName}.txt"
        val target = new File(output.getParentFile, fileName)
        val fos = new FileOutputStream(target)
        try fos.write(content.getBytes)
        finally fos.close()
      case _ =>
    }
  }
}