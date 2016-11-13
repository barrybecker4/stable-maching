package com.barrybecker4.matching

import org.scalatest.FunSuite

/**
  * @author Barry Becker
  */
class StableMarriageFinderSuite extends FunSuite {

  val case1: MarriagePreferences = new MarriagePreferences(
    guys = List("abe", "bob", "col", "dan", "ed", "fred", "gav", "hal", "ian", "jon"),
    girls = List("abi", "bea", "cath", "dee", "eve", "fay", "gay", "hope", "ivy", "jan"),
    guyPrefers = Map(
      "abe" -> List("abi", "eve", "cath", "ivy", "jan", "dee", "fay", "bea", "hope", "gay"),
      "bob" -> List("cath", "hope", "abi", "dee", "eve", "fay", "bea", "jan", "ivy", "gay"),
      "col" -> List("hope", "eve", "abi", "dee", "bea", "fay", "ivy", "gay", "cath", "jan"),
      "dan" -> List("ivy", "fay", "dee", "gay", "hope", "eve", "jan", "bea", "cath", "abi"),
      "ed" -> List("jan", "dee", "bea", "cath", "fay", "eve", "abi", "ivy", "hope", "gay"),
      "fred" -> List("bea", "abi", "dee", "gay", "eve", "ivy", "cath", "jan", "hope", "fay"),
      "gav" -> List("gay", "eve", "ivy", "bea", "cath", "abi", "dee", "hope", "jan", "fay"),
      "hal" -> List("abi", "eve", "hope", "fay", "ivy", "cath", "jan", "bea", "gay", "dee"),
      "ian" -> List("hope", "cath", "dee", "gay", "bea", "abi", "fay", "ivy", "jan", "eve"),
      "jon" -> List("abi", "fay", "jan", "gay", "eve", "bea", "dee", "cath", "ivy", "hope")),
    girlPrefers = Map(
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


  test("test stable marriage") {

    val smp: StableMarriageFinder = new StableMarriageFinder()
    val matches = smp.findMatches(case1)

    assertResult(Map(
      "abi" -> "jon",
      "bea" -> "fred",
      "cath" -> "bob",
      "dee" -> "col",
      "eve" -> "hal",
      "fay" -> "dan",
      "gay" -> "gav",
      "hope" -> "ian",
      "ivy" -> "abe",
      "jan" -> "ed")) { matches }

    assertResult(true, "Matches were unexpectedly not stable") {
      smp.checkMatches(case1, matches)
    }
  }

  test("test unstable marriage") {

    val smp: StableMarriageFinder = new StableMarriageFinder()
    var matches = smp.findMatches(case1)

    val tmp = matches(case1.girls.head)
    matches += case1.girls.head -> matches(case1.girls(1))
    matches += case1.girls(1) -> tmp

    println(case1.girls.head + " and " + case1.girls(1) + " have switched partners")

    assertResult(false, "Matches were unexpectedly stable") {
      smp.checkMatches(case1, matches)
    }
  }

  test("test unstable marriage: case 2") {

    val case2 = new MarriagePreferences(4,
      guyPrefers = Map(
        1 -> List(4, 3, 1, 2),
        2 -> List(2, 1, 3, 4),
        3 -> List(1, 3, 4, 2),
        4 -> List(4, 3, 1, 2)),
      girlPrefers = Map(
        1 -> List(3, 2, 4, 1),
        2 -> List(2, 3, 1, 4),
        3 -> List(3, 1, 2, 4),
        4 -> List(3, 2, 4, 1))
    )

    val smp: StableMarriageFinder = new StableMarriageFinder()
    val matches = smp.findMatches(case2)

    assertResult(Map(
      "1" -> "3",
      "2" -> "2",
      "3" -> "1",
      "4" -> "4")) { matches }

    assertResult(false, "Matches were unexpectedly stable") {
      smp.checkMatches(case1, matches)
    }
  }

}