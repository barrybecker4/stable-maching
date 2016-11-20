package com.barrybecker4.matching.stableroommate

import scala.collection.immutable.TreeMap
import scala.collection.mutable

/**
  * Find stable marriage matching if possible.
  * Code derived from https://en.wikipedia.org/wiki/Stable_roommates_problem
  */
class StableRoommateFinder {

  /** @return proposed pairings found by stable roommate algorithm */
  def findMatches(preferences: RoommatePreferences): Map[String, String] = {

    // Phase 1
    val pairings = findInitialPairings(preferences)

    // Phase 2
    val reducedPrefs = findReducedPrefs(pairings, preferences)

    println("reduced = " + reducedPrefs.map(x => x._1 + " -> " + x._2.mkString(", ")).mkString("\n"))

    // Phase 3
    val finalPrefs = reduceBasedOnCycles(pairings, reducedPrefs)

    pairings

  }

  private def findInitialPairings(preferences: RoommatePreferences): Map[String, String] = {

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
          val currentMatch = pairings(candidate)
          val candidatePrefs = preferences.prefers(candidate)
          if (candidatePrefs.indexOf(person) < candidatePrefs.indexOf(currentMatch)) {
            pairings += candidate -> person
            freePeople +:= currentMatch
            done = true
          }
        }
        i += 1
      }
    }

    pairings
  }

  private def findReducedPrefs(pairings: Map[String, String],
                               preferences: RoommatePreferences): mutable.Map[String, List[String]] = {
    val reducedPrefs = collection.mutable.Map[String, List[String]]() ++= preferences.prefers

    preferences.people.foreach(person => {
      val currentMatch = pairings(person)
      val currentPrefs = reducedPrefs(currentMatch)
      reducedPrefs.put(currentMatch, currentPrefs.takeWhile(x => x != person) :+ person)
    })

    reducedPrefs
  }

  private def reduceBasedOnCycles(pairings: Map[String, String],
                                  reducedPrefs: mutable.Map[String, List[String]]): Map[String, List[String]] = {

    var done = false

    while (!done) {
      val firstPerson = reducedPrefs.find(x => x._2.length > 1).map(_._1)
      if (firstPerson.isDefined) {
        var lastPrefsList: List[String] = List(firstPerson.get)
        var secondPrefsList: List[String] = List()

        // repeat this process until someone has an empty preference list (no stable pairing),
        // or until everyone has exactly one person in their list (stable)

        // repeat until same person appears twice in one of the two lists
        while (!done) {
          val secondPref = reducedPrefs.get(lastPrefsList.last).tail.head.toString()
          if (secondPrefsList.contains(secondPref)) done = true // dupe name
          secondPrefsList :+= secondPref
          val lastPref = reducedPrefs(secondPref).last
          if (lastPrefsList.contains(lastPref)) done = true // dupe name
          lastPrefsList :+= lastPref
        }

        if (done) {
          lastPrefsList = lastPrefsList.tail // get the 2 lists to align
          while (lastPrefsList.nonEmpty) {
            val p1 = lastPrefsList.head
            val p2 = secondPrefsList.head
            reducedPrefs(p1) = reducedPrefs(p1).filter(_ != p2)
            reducedPrefs(p2) = reducedPrefs(p2).filter(_ != p1)
            lastPrefsList = lastPrefsList.tail
            secondPrefsList = secondPrefsList.tail
          }
        }
      } else done = true
    }

    reducedPrefs.toMap
  }

  /*private def removePrefsAfter(person:String, currentPrefs: List[String]): List[String] = {
  }*/


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