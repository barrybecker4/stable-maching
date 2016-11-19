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
    var freeGuys = preferences.guys

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
          val otherGuy = engagements(girl)
          val girlPreference = preferences.girlPrefers(girl)
          if (girlPreference.indexOf(guy) < girlPreference.indexOf(otherGuy)) {
            engagements += girl -> guy
            freeGuys +:= otherGuy
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
    if (!(preferences.girls.forall(matches.contains) && preferences.guys.forall(matches.values.toSet.contains)))
      return false

    var invertedMatches = new TreeMap[String, String]
    matches.foreach {
      invertedMatches += _.swap
    }

    for ((someGirl, someGuy) <- matches) {
      val shePrefers = preferences.girlPrefers(someGirl)
      var sheLikesBetter: List[String] = List[String]()
      sheLikesBetter ++= shePrefers.slice(0, shePrefers.indexOf(someGuy))
      val hePrefers = preferences.guyPrefers(someGuy)
      var heLikesBetter = List[String]()
      heLikesBetter ++= hePrefers.slice(0, hePrefers.indexOf(someGirl))

      for (guy <- sheLikesBetter) {
        val fiance = invertedMatches(guy)
        val guyPrefs = preferences.guyPrefers(guy)
        if (guyPrefs.indexOf(fiance) > guyPrefs.indexOf(someGirl)) {
          println(s"$someGirl likes $guy better than $someGuy and $guy likes $someGirl better than their current partner")
          return false
        }
      }

      for (girl <- heLikesBetter) {
        val fiance = matches(girl)
        val girlPrefs = preferences.girlPrefers(girl)
        if (girlPrefs.indexOf(fiance) > girlPrefs.indexOf(someGuy)) {
          println(s"$someGuy likes $girl better than $someGirl and $girl likes $someGuy better than their current partner")
          return false
        }
      }
    }
    true
  }

}