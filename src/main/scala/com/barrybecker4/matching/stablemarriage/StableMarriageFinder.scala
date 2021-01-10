package com.barrybecker4.matching.stablemarriage

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

}