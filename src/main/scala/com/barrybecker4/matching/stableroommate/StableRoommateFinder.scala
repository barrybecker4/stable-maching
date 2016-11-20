package com.barrybecker4.matching.stableroommate

import com.barrybecker4.matching.stablemarriage.MarriagePreferences

import scala.collection.immutable.TreeMap

/**
  * Find stable marriage matching if possible.
  * Code derived from https://en.wikipedia.org/wiki/Stable_roommates_problem
  */
class StableRoommateFinder {

  /** @return proposed pairings found by stable roommate algorithm */
  def findMatches(preferences: RoommatePreferences): Map[String, String] = {

    var pairings = new TreeMap[String, String]
    var freePeople = preferences.people

    while (freePeople.nonEmpty) {
      val person = freePeople.head
      freePeople = freePeople.tail
      val prefs = preferences.prefers(person)
      var done = false
      var i = 0
      while (!done && i < prefs.length) {
        val candidate = prefs(i)
        if (!pairings.contains(candidate)) {
          pairings += candidate -> person
          done = true
        }
        else {
          val otherGuy = pairings(candidate)
          val candidatePrefs = preferences.prefers(candidate)
          if (candidatePrefs.indexOf(person) < candidatePrefs.indexOf(otherGuy)) {
            pairings += candidate -> person
            freePeople +:= otherGuy
            done = true
          }
        }
        i += 1
      }
    }

    pairings
  }

  /** @return true if the matches are stable */
  def checkMatches(preferences: RoommatePreferences,
                   matches: Map[String, String]): Boolean = {
    if (!preferences.people.forall(matches.contains))
      return false

    val invertedMatches = matches.map(_.swap)

    for ((someGuy, someOtherGuy) <- matches) {
      val someGuyPrefers = preferences.prefers(someGuy)
      val someGuyLikesBetter: List[String] = someGuyPrefers.slice(0, someGuyPrefers.indexOf(someOtherGuy))
      val someOtherGuyPrefers = preferences.prefers(someOtherGuy)
      val someOtherGuyLikesBetter = someOtherGuyPrefers.slice(0, someOtherGuyPrefers.indexOf(someGuy))

      if (checkForInstability(someGuy, someOtherGuy, someGuyLikesBetter, preferences.prefers, invertedMatches))
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