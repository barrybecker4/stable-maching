package com.barrybecker4.matching

import scala.collection.immutable.TreeMap

/**
  * Find stable marriage matching if possible.
  * Code derived from http://rosettacode.org/wiki/Stable_marriage_problem#Scala
  */
class StableMarriageFinder {

  /** @return proposed engagements found by stable marriage algorithm */
  def findMatches(preferences: MarriagePreferences): Map[String, String] = {

    var engagements = new TreeMap[String, String]
    var freeGuys = preferences.guys.toList

    while (freeGuys.nonEmpty) {
      val guy = freeGuys.head
      freeGuys = freeGuys.tail
      val guyPreferences = preferences.guyPrefers(guy)
      var done = false
      var i = 0
      while (!done && i < guyPreferences.length) {
        val girl = guyPreferences(i)
        if (!engagements.contains(girl)) {
          engagements += girl -> guy
          done = true
        }
        else {
          val other_guy = engagements(girl)
          val girlPreference = preferences.girlPrefers(girl)
          if (girlPreference.indexOf(guy) < girlPreference.indexOf(other_guy)) {
            engagements += girl -> guy
            freeGuys +:= other_guy
            done = true
          }
        }
        i += 1
      }
    }

    engagements
  }

  /** @return true if the matches are stable */
  def checkMatches(preferences: MarriagePreferences,
                   matches: Map[String, String]): Boolean = {
    //if (!matches.keySet.containsAll(girls) || !matches.values.containsAll(guys))
    if (!(preferences.girls.forall(matches.contains) && preferences.guys.forall(matches.values.toSet.contains)))
      return false

    var invertedMatches = new TreeMap[String, String]
    matches.foreach {
      invertedMatches += _.swap
    }

    for ((k, v) <- matches) {
      val shePrefers = preferences.girlPrefers(k)
      var sheLikesBetter: List[String] = List[String]()
      sheLikesBetter ++= shePrefers.slice(0, shePrefers.indexOf(v))
      val hePrefers = preferences.guyPrefers(v)
      var heLikesBetter = List[String]()
      heLikesBetter ++= hePrefers.slice(0, hePrefers.indexOf(k))

      for (guy <- sheLikesBetter) {
        val fiance = invertedMatches(guy)
        val guy_p = preferences.guyPrefers(guy)
        if (guy_p.indexOf(fiance) > guy_p.indexOf(k)) {
          println(s"$k likes $guy better than $v and $guy likes $k better than their current partner")
          return false
        }
      }

      for (girl <- heLikesBetter) {
        val fiance = matches(girl)
        val girl_p = preferences.girlPrefers(girl)
        if (girl_p.indexOf(fiance) > girl_p.indexOf(v)) {
          println(s"$v likes $girl better than $k and $girl likes $v better than their current partner")
          return false
        }
      }
    }
    true
  }

}