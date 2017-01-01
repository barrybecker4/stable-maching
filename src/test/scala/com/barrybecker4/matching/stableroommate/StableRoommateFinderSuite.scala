package com.barrybecker4.matching.stableroommate

import org.scalatest.FunSuite

/**
  * @author Barry Becker
  */
class StableRoommateFinderSuite extends FunSuite {

  val srp = new StableRoommateFinder()

  val case1 = new RoommatePreferences(
    people = List("abe", "bob", "col", "dan", "ed", "fred", "gav", "hal", "ian", "jon",
      "abi", "bea", "cath", "dee", "eve", "fay", "gay", "hope", "ivy", "jan"),
    prefers = Map(
      "abe" -> List("abi", "eve", "cath", "ivy", "jan", "dee", "fay", "bea", "hope", "gay"),
      "bob" -> List("cath", "hope", "abi", "dee", "eve", "fay", "bea", "jan", "ivy", "gay"),
      "col" -> List("hope", "eve", "abi", "dee", "bea", "fay", "ivy", "gay", "cath", "jan"),
      "dan" -> List("ivy", "fay", "dee", "gay", "hope", "eve", "jan", "bea", "cath", "abi"),
      "ed" -> List("jan", "dee", "bea", "cath", "fay", "eve", "abi", "ivy", "hope", "gay"),
      "fred" -> List("bea", "abi", "dee", "gay", "eve", "ivy", "cath", "jan", "hope", "fay"),
      "gav" -> List("gay", "eve", "ivy", "bea", "cath", "abi", "dee", "hope", "jan", "fay"),
      "hal" -> List("abi", "eve", "hope", "fay", "ivy", "cath", "jan", "bea", "gay", "dee"),
      "ian" -> List("hope", "cath", "dee", "gay", "bea", "abi", "fay", "ivy", "jan", "eve"),
      "jon" -> List("abi", "fay", "jan", "gay", "eve", "bea", "dee", "cath", "ivy", "hope"),
      "abi" -> List("bob", "fred", "jon", "gav", "ian", "abe", "dan", "ed", "col", "hal"),
      "bea" -> List("bob", "abe", "col", "fred", "gav", "dan", "ian", "ed", "jon", "hal"),
      "cath" -> List("fred", "bob", "ed", "gav", "hal", "col", "ian", "abe", "dan", "jon"),
      "dee" -> List("fred", "jon", "col", "abe", "ian", "hal", "gav", "dan", "bob", "ed"),
      "eve" -> List("jon", "hal", "fred", "dan", "abe", "gav", "col", "ed", "ian", "bob"),
      "fay" -> List("bob", "abe", "ed", "ian", "jon", "dan", "fred", "gav", "col", "hal"),
      "gay" -> List("jon", "gav", "hal", "fred", "bob", "abe", "col", "ed", "dan", "ian"),
      "hope" -> List("gav", "jon", "bob", "abe", "ian", "dan", "hal", "ed", "col", "fred"),
      "ivy" -> List("ian", "col", "hal", "gav", "fred", "bob", "abe", "ed", "jon", "dan"),
      "jan" -> List("ed", "hal", "gav", "abe", "bob", "jon", "col", "ian", "fred", "dan"))
  )

  test("test stable roommates: trivial 2 person case") {

    val twoPeople = new RoommatePreferences(2,
      prefers = Map(
        1 -> List(2),
        2 -> List(1)
    ))

    val matches = srp.findMatches(twoPeople)

    assertResult(Map("2" -> "1")) { matches }
  }

  test("test stable roommates: trivial 4 person case") {

    val fourPeople = new RoommatePreferences(2,
      prefers = Map(
        1 -> List(2, 4),
        2 -> List(1, 3),
        3 -> List(2, 4),
        4 -> List(3, 1)
      ))

    val matches = srp.findMatches(fourPeople)

    assertResult(Map("2" -> "1", "4" -> "3")) { matches }
  }

  test("test stable roommates: unstable 4 person case (has cycle) (no one likes D") {

    val fourPeople = new RoommatePreferences(
      people = List("A", "B", "C", "D"),
      prefers = Map(
        "A" -> List("B", "C", "D"),
        "B" -> List("C", "A", "D"),
        "C" -> List("A" ,"B", "D"),
        "D" -> List("A", "B", "C")
      ))

    // no stable solution possible.
    try {
      srp.findMatches(fourPeople)
      fail("Should not get here")
    } catch {
      case ie: IllegalArgumentException => // success
      case e: Throwable => "Unexpected" + e.printStackTrace()
    }
  }


  test("test stable roommate for case1") {

    val matches = srp.findMatches(case1)

    assertResult(Map(
      "dee" -> "col", "ed" -> "jan", "fred" -> "bea", "eve" -> "hal", "ian" -> "hope",
      "abe" -> "ivy", "fay" -> "dan", "abi" -> "jon", "bob" -> "cath", "gay" -> "gav")
    ) { matches }
  }

  test("test stable roommates: 6 person case from https://www.youtube.com/watch?v=5QLxAp8mRKo&t=15s") {

    val case2 = new RoommatePreferences(6,
      prefers = Map(
        1 -> List(2, 4, 6, 3, 5),
        2 -> List(4, 5, 6, 1, 3),
        3 -> List(4, 5, 6, 1, 2),
        4 -> List(6, 3, 1, 5, 2),
        5 -> List(6, 3, 4, 2, 1),
        6 -> List(1, 2, 4, 3, 5))
    )

    val matches = srp.findMatches(case2)

    assertResult(Map("2" -> "5", "1" -> "6", "4" -> "3")) { matches }
  }

  test("test stable roommates: integers only (no stable matching)") {

    val case2 = new RoommatePreferences(6,
      prefers = Map(
        1 -> List(3, 4, 2, 6, 5),
        2 -> List(6, 5, 4, 1, 3),
        3 -> List(2, 4, 5, 1, 6),
        4 -> List(5, 2, 3, 6, 1),
        5 -> List(3, 1, 2, 4, 6),
        6 -> List(5, 1, 3, 4, 2))
    )

    try {
      srp.findMatches(case2)
      fail("Should not get here")
    } catch {
      case ie: IllegalArgumentException => // success
      case e: Throwable => "Unexpected" + e.printStackTrace()
    }
  }

  test("test stable roommates: integers only (2 possible stable matchings)") {

    val case2 = new RoommatePreferences(6,
      prefers = Map(
        1 -> List(3, 4, 2, 6, 5),
        2 -> List(6, 5, 4, 1, 3),
        3 -> List(2, 4, 5, 1, 6),
        4 -> List(5, 2, 3, 6, 1),
        5 -> List(3, 1, 2, 4, 6),
        6 -> List(5, 1, 3, 4, 2))
    )

    try {
      srp.findMatches(case2)
      fail("Should not get here")
    } catch {
      case ie: IllegalArgumentException => // success
      case e: Throwable => "Unexpected" + e.printStackTrace()
    }
  }

  test("test stable roommate performance for large problem") {
    val rand = new scala.util.Random(1)
    val n = 100

    def prefs(num: Int) = for (i <- 1 to num) yield { i -> List.fill(num)(rand.nextInt(num) + 1) }
    val mp = new RoommatePreferences(n, prefers = prefs(n).toMap)

    val srp = new StableRoommateFinder()
    try {
      srp.findMatches(mp)
      fail("Should not get here")
    } catch {
      case ie: IllegalArgumentException => // success
      case e: Throwable => "Unexpected" + e.printStackTrace()
    }

    /*
    assertResult(Map("1" -> "24", "10" -> "82", "100" -> "17", "11" -> "30", "12" -> "83", "13" -> "38",
      "14" -> "85", "15" -> "19", "16" -> "71", "17" -> "47", "18" -> "90", "19" -> "16",
      "2" -> "14", "20" -> "73", "21" -> "62", "22" -> "25", "23" -> "64", "24" -> "5", "25" -> "27",
      "26" -> "34", "27" -> "20", "28" -> "76", "29" -> "22", "3" -> "36", "30" -> "10", "31" -> "44",
      "32" -> "54", "33" -> "95", "34" -> "78", "35" -> "45", "36" -> "79", "37" -> "72", "38" -> "49",
      "39" -> "77", "4" -> "32", "40" -> "100", "41" -> "35", "42" -> "18", "43" -> "12", "44" -> "94",
      "45" -> "60", "46" -> "67", "47" -> "8", "48" -> "11", "49" -> "65", "5" -> "81", "50" -> "98",
      "51" -> "29", "52" -> "46", "53" -> "91", "54" -> "70", "55" -> "1", "56" -> "68", "57" -> "86",
      "58" -> "9", "59" -> "93", "6" -> "21", "60" -> "52", "61" -> "80", "62" -> "31", "63" -> "59",
      "64" -> "88", "65" -> "57", "66" -> "66", "67" -> "87", "68" -> "61", "69" -> "43", "7" -> "50",
      "70" -> "33", "71" -> "13", "72" -> "99", "73" -> "28", "74" -> "53", "75" -> "2", "76" -> "7",
      "77" -> "41", "78" -> "84", "79" -> "74", "8" -> "3", "80" -> "40", "81" -> "55", "82" -> "39",
      "83" -> "97", "84" -> "37", "85" -> "23", "86" -> "51", "87" -> "89", "88" -> "92", "89" -> "15",
      "9" -> "58", "90" -> "6", "91" -> "26", "92" -> "96", "93" -> "56", "94" -> "63", "95" -> "4",
      "96" -> "42", "97" -> "69", "98" -> "75", "99" -> "48")) { matches }
     */
  }

  /*
  // Take 6 seconds on skylake.
  test("test stable roommate performance for very large problem") {
    val rand = new scala.util.Random(1)
    val n = 100

    def prefs(num: Int) = for (i <- 1 to num) yield { i -> List.fill(num)(rand.nextInt(num) + 1) }
    val mp = new RoommatePreferences(n, prefers = prefs(n).toMap)

    val matches = srp.findMatches(mp)

    assertResult(n) { matches.size }
  }*/

  /*
  test("test stable roommates: integers only") {

    val case2 = new RoommatePreferences(6,
      prefers = Map(
        1 -> List(3, 4, 2, 6, 5),
        2 -> List(6, 5, 4, 1, 3),
        3 -> List(2, 4, 5, 1, 6),
        4 -> List(5, 2, 3, 6, 1),
        5 -> List(3, 1, 2, 4, 6),
        6 -> List(5, 1, 3, 4, 2))
    )

    val matches = srp.findMatches(case2)

    assertResult(Map("2" -> "1", "5" -> "2", "4" -> "3")) { matches } // this is wrong
    //  "1" -> "6", "2" -> "4", "3" -> "5")) // this is right
  }
   */
}