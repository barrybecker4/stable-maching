package com.barrybecker4.matching.stableroommate

import scala.collection.immutable.TreeMap
import scala.collection.mutable

/**
  * Find stable roommate paring based on supplied preferences if possible.
  * There must be an even number of people, and no cycles that would prevent a stable pairing.
  * Code derived from https://en.wikipedia.org/wiki/Stable_roommates_problem
  */
class StableRoommateFinder {

  /** @return proposed pairings found by stable roommate algorithm */
  def findMatches(preferences: RoommatePreferences): Map[String, String] = {

    // Phase 1
    val reducedPrefs = findInitialPairings(preferences)
    println("\nreduced = \n" + reducedPrefs.map(x => x._1 + " -> " + x._2.mkString(", ")).mkString("\n")) // correct

    // Phase 2
    reduceBasedOnCycles(reducedPrefs)
    println("\nfinalReducedPrefs = \n" + reducedPrefs.map(x => x._1 + " -> " + x._2.mkString(", ")).mkString("\n"))

    // If stable, each persons preference list should be of length 1
    findPairings(reducedPrefs)
  }

  /**
    * Participants propose to each other, in a manner similar to that of the Gale-Shapley algorithm for
    * the stable marriage problem. Consider two participants, person and candidate.
    * If candidate holds a proposal from person, then we remove from candidates's
    * list all participants x after person, and symmetrically, for each removed participant x,
    * we remove candidate from x's list, so that candidate is first in person's list;
    * and person, last in candidates's. The resulting reduced set of preference lists together
    * is called the Phase 1 table. In this table, if any reduced list is empty, then there is no stable matching.
    * Otherwise, the Phase 1 table is a stable table.
    */
  private def findInitialPairings(preferences: RoommatePreferences): mutable.Map[String, List[String]] = {

    var pairings = new TreeMap[String, String]
    var freePeople = preferences.people
    val reducedPrefs = mutable.Map[String, List[String]]() ++= preferences.prefers

    while (freePeople.nonEmpty) {
      val person = freePeople.head
      freePeople = freePeople.tail
      val prefs = reducedPrefs(person)
      var done = false
      var i = 0
      while (!done && i < prefs.length) {
        val candidate = prefs(i)
        if (!pairings.contains(candidate)) {
          pairings += candidate -> person
          rejectSymmetrically(person, candidate, reducedPrefs)
          done = true
        }
        else {
          val currentMatch = pairings(candidate)
          val candidatePrefs = reducedPrefs(candidate)
          if (candidatePrefs.indexOf(person) < candidatePrefs.indexOf(currentMatch)) {
            pairings += candidate -> person
            freePeople +:= currentMatch
            rejectSymmetrically(person, candidate, reducedPrefs)
            done = true
          }
        }
        i += 1
      }
    }

    println("\ninitial pairings = " + pairings.map(x => x._1 + " -> " + x._2).mkString("\n"))
    reducedPrefs
  }

  // remove from candidates's list all participants x after person
  private def rejectSymmetrically(person: String, candidate: String,
                                  reducedPrefs: mutable.Map[String, List[String]]): Unit = {
    val candidatePrefs = reducedPrefs(candidate)
    val (keep, drop) = candidatePrefs.splitAt(candidatePrefs.indexOf(person) + 1)
    reducedPrefs.put(candidate, keep)
    drop.foreach(x => {
      reducedPrefs.put(x, reducedPrefs(x).filter(_ != candidate))
    })
  }

  /**
    * for all cycles in (p1 ... p2) and (q1 ... q2) such that:
    *   qi is the second preference of pi and pi+1 is the last preference of qi do
    *       for i = 1 to n-1
    *           rejectSymmetrically(qi, pi+1)
    * @return final reduced preferences
    */
  private def reduceBasedOnCycles(reducedPrefs: mutable.Map[String, List[String]]): Unit = {
    var done = false

    // Repeat this process until someone has an empty preference list (no stable pairing),
    // or until everyone has exactly one person in their list (stable)
    while (!done) {
      // find a person with more than 1 person in their preference list
      val firstPerson = reducedPrefs.find(_._2.length > 1).map(_._1)
      if (firstPerson.isDefined) {
        var lastPrefsList: List[String] = List(firstPerson.get)
        var secondPrefsList: List[String] = List()

        // Find a cycle if one exists...
        // repeat until same person appears twice in one of the two lists
        while (!done) {
          println("second pref list (for " + lastPrefsList.last + ") = " + reducedPrefs(lastPrefsList.last).mkString(", ")
            + " and size = " + reducedPrefs(lastPrefsList.last).length)
          if (reducedPrefs(lastPrefsList.last).size > 1) {
            val secondPref = reducedPrefs(lastPrefsList.last).tail.head
            println("secondPref = " + secondPref + " test to see if in " + secondPrefsList)
            println("does secondPrefsList " + secondPrefsList.mkString(", ")
              + " contain " + secondPref + "? " + secondPrefsList.contains(secondPref))
            if (secondPrefsList.contains(secondPref)) {
              done = true // dupe name
            }
            secondPrefsList :+= secondPref
            println("added " + secondPref + " to secondPrefsList = " + secondPrefsList.mkString(", "))
            if (reducedPrefs(secondPref).isEmpty) {
              throw new IllegalArgumentException("Cannot find a stable matching.")
            }
            val lastPref = reducedPrefs(secondPref).last
            println("does lastPrefsList " + lastPrefsList.mkString(", ")
              + " contain " + lastPref + "? " + lastPrefsList.contains(lastPref))
            if (lastPrefsList.contains(lastPref))
              done = true // dupe name
            lastPrefsList :+= lastPref
            println("added " + lastPref + " to lastPrefsList = " + lastPrefsList.mkString(", "))
          }
          else done = true  // there was no second preference
        }
        println(" - lastPrefslist = " + lastPrefsList.mkString(", "))
        println(" - secondPrefslist = " + secondPrefsList.mkString(", "))

        // ... and symmetrically remove it if one was found
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
          println("\nreducedPrefs after symmetric removal = \n"
            + reducedPrefs.map(x => x._1 + " -> " + x._2.mkString(", ")).mkString("\n") + "\n")
          done = false
        }
      } else done = true
    }
  }

  def findPairings(reducedPrefs: mutable.Map[String, List[String]]): Map[String, String] = {
    val pairings = mutable.Map[String, String]()
    println("Reduced prefs keys = " + reducedPrefs.keys.mkString(", "))
    reducedPrefs.foreach(entry => {
      if (entry._2.isEmpty) throw new IllegalArgumentException("Cannot find a stable matching.")
      val favorite = entry._2.head
      println("The favorite for " + entry._1 + " is " + favorite +" among " + reducedPrefs.keys.mkString(", "))
      if (reducedPrefs.contains(favorite)) {
        val orig = reducedPrefs(favorite).head
        require (orig == entry._1, "Expected that " + entry._1 + " and the first element of " + favorite + "'s prefs: "
          + reducedPrefs(favorite).mkString(", ") + "  would be the same.")
        pairings.put(entry._1, favorite)
        println("about to remove " + favorite + "'s list = "
          + reducedPrefs(favorite).mkString(", ") + " it should contain only "+ entry._1)
        reducedPrefs.remove(favorite)
      } else {
        throw new IllegalArgumentException("Cannot find a stable matching.")
      }
    })
    pairings.toMap
  }

}