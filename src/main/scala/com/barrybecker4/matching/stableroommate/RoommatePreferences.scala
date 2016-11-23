package com.barrybecker4.matching.stableroommate

/**
  * Encapsulates preferences for the people to be paired.
  *
  * @param people list of people to be paired
  * @param prefers preferences as list of people in preferred order
  */
class RoommatePreferences(val people: List[String],
                          val prefers: Map[String, List[String]]) {

  /**
    * Alternate constructor for when input is specified as integer indices.
    * @param numPairs number of people. They will both be labeled "1", "2", ..."n".
    */
  def this(numPairs: Int, prefers: Map[Int, List[Int]]) {
    this(
      List.range(1, numPairs + 1).map(_.toString),
      prefers.map(entry => entry._1.toString -> entry._2.map(_.toString))
    )
  }
}
