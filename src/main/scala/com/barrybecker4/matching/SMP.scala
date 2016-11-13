package com.barrybecker4.matching

import scala.collection.immutable.TreeMap
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
//import java.util._
//import scala.collection.JavaConversions._

class SMP {

  def run(guys: List[String], girls: List[String],
          guyPrefers: Map[String, List[String]], girlPrefers: Map[String, List[String]]): Boolean = {

    var matches = matching(guys, guyPrefers, girlPrefers)
    matches.foreach { e => println(s"${e._1} is engaged to ${e._2}") }
    if (checkMatches(guys, girls, matches, guyPrefers, girlPrefers))
      println("Marriages are stable")
    else
      println("Marriages are unstable")

    val tmp = matches(girls.head)
    matches += girls.head -> matches(girls(1))
    matches += girls(1) -> tmp
    println(girls.head + " and " + girls(1) + " have switched partners")
    if (checkMatches(guys, girls, matches, guyPrefers, girlPrefers))
      println("Marriages are stable")
    else
      println("Marriages are unstable")
    true
  }

  private def matching(guys: Iterable[String],
                       guyPrefers: Map[String, List[String]],
                       girlPrefers: Map[String, List[String]]): Map[String, String] = {
    var engagements = new TreeMap[String, String]
    var freeGuys = guys.toList

    while (freeGuys.nonEmpty) {
      val guy = freeGuys.head
      freeGuys = freeGuys.tail
      val guy_p = guyPrefers(guy)
      var break = false
      for (girl <- guy_p)
        if (!break)
          if (!engagements.contains(girl)) {
            engagements += girl -> guy
            break = true
          }
          else {
            val other_guy = engagements(girl)
            val girl_p = girlPrefers(girl)
            if (girl_p.indexOf(guy) < girl_p.indexOf(other_guy)) {
              engagements += girl -> guy
              freeGuys +:= other_guy
              break = true
            }
          }
    }

    engagements
  }

  private def checkMatches(guys: Iterable[String], girls: Iterable[String],
                           matches: Map[String, String],
                           guyPrefers: Map[String, List[String]],
                           girlPrefers: Map[String, List[String]]): Boolean = {
    //if (!matches.keySet.containsAll(girls) || !matches.values.containsAll(guys))
    if (!(girls.forall(matches.contains) && guys.forall(matches.values.toSet.contains)))
      return false

    var invertedMatches = new TreeMap[String, String]
    matches.foreach {
      invertedMatches += _.swap
    }

    for ((k, v) <- matches) {
      val shePrefers = girlPrefers(k)
      var sheLikesBetter: List[String] = List[String]()
      //val foo: List[String] = shePrefers.slice(0, shePrefers.indexOf(v))
      sheLikesBetter ++= shePrefers.slice(0, shePrefers.indexOf(v))
      val hePrefers = guyPrefers(v)
      var heLikesBetter = List[String]()
      heLikesBetter ++= hePrefers.slice(0, hePrefers.indexOf(k))

      for (guy <- sheLikesBetter) {
        val fiance = invertedMatches(guy)
        val guy_p = guyPrefers(guy)
        if (guy_p.indexOf(fiance) > guy_p.indexOf(k)) {
          println(s"$k likes $guy better than $v and $guy likes $k better than their current partner")
          return false
        }
      }

      for (girl <- heLikesBetter) {
        val fiance = matches(girl)
        val girl_p = girlPrefers(girl)
        if (girl_p.indexOf(fiance) > girl_p.indexOf(v)) {
          println(s"$v likes $girl better than $k and $girl likes $v better than their current partner")
          return false
        }
      }
    }
    true
  }

}