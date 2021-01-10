package com.barrybecker4.matching.stablemarriage

class StableMarriageVerifier {

  /** @return true if the matches are stable
    */
  def verifyStable(preferences: MarriagePreferences,
    matches: Map[String, String]): Boolean = {

    if (!everyoneMatched(preferences, matches))
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

  /** @return true if all boys and girls are matched with someone.
    */
  private def everyoneMatched(preferences: MarriagePreferences, matches: Map[String, String]): Boolean = {
    val allBoysMatched = preferences.girls.forall(matches.contains)
    val allGirlsMatched = preferences.guys.forall(matches.values.toSet.contains)
    allBoysMatched && allGirlsMatched
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
