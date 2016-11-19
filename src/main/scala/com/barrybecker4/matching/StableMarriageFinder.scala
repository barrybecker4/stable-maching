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

    val invertedMatches = matches.map(_.swap)

    for ((someGirl, someGuy) <- matches) {
      val shePrefers = preferences.girlPrefers(someGirl)
      val sheLikesBetter: List[String] = shePrefers.slice(0, shePrefers.indexOf(someGuy))
      val hePrefers = preferences.guyPrefers(someGuy)
      val heLikesBetter = hePrefers.slice(0, hePrefers.indexOf(someGirl))

      if (checkForInstability(someGirl, someGuy, sheLikesBetter, preferences.guyPrefers, invertedMatches) ||
        checkForInstability(someGuy, someGirl, heLikesBetter, preferences.girlPrefers, matches))
        return false
    }
    true
  }

  /** @return true if there is a girl who likes another guy better than current partner,
    *  and that guy likes her better than his current partner (or vice versa)
    */
  private  def checkForInstability(person: String, otherPerson: String, personLikesBetter: List[String],
                                   prefs: Map[String, List[String]], theMatches: Map[String, String]): Boolean = {
    for (p <- personLikesBetter) {
      val fiance = theMatches(p)
      val personPrefs = prefs(p)
      if (personPrefs.indexOf(fiance) > personPrefs.indexOf(person)) {
        println(s"$person likes $p better than $otherPerson and $p likes $person better than their current partner")
        return true
      }
    }
    false
  }

}