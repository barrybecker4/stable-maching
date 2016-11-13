package com.barrybecker4.matching

import scala.collection.immutable.TreeMap

/**
  * Find stable marriage matching if possible.
  * Code derived from http://rosettacode.org/wiki/Stable_marriage_problem#Scala
  */
class StableMarriageFinder {

  /** @return proposed engagements found by stable marriage algorithm */
  def findMatches(guys: Iterable[String],
                  guyPrefers: Map[String, List[String]],
                  girlPrefers: Map[String, List[String]]): Map[String, String] = {

    var engagements = new TreeMap[String, String]
    var freeGuys = guys.toList

    while (freeGuys.nonEmpty) {
      val guy = freeGuys.head
      freeGuys = freeGuys.tail
      val guyPreferences = guyPrefers(guy)
      var done = false
      for (girl <- guyPreferences) {
        if (!done) {
          if (!engagements.contains(girl)) {
            engagements += girl -> guy
            done = true
          }
          else {
            val other_guy = engagements(girl)
            val girl_p = girlPrefers(girl)
            if (girl_p.indexOf(guy) < girl_p.indexOf(other_guy)) {
              engagements += girl -> guy
              freeGuys +:= other_guy
              done = true
            }
          }
        }
      }
    }

    engagements
  }

  /** @return true if the matches are stable */
  def checkMatches(guys: Iterable[String], girls: Iterable[String],
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