package com.barrybecker4.matching.stablemarriage

/**
  * Encapsulates preferences for the guys and girls.
  * @param guys of boys
  * @param girls names of girls
  * @param guyPrefers boy preferences as list of girls in preferred order
  * @param girlPrefers girl preferences as list of boys in preferred order
  */
class MarriagePreferences(val guys: List[String],
                          val girls: List[String],
                          val guyPrefers: Map[String, List[String]],
                          val girlPrefers: Map[String, List[String]]) {

  /**
    * Alternate constructor for when input is specified as integer indices.
    * @param numPairs number of men, and number of women. They will both be labeled "1", "2", ..."n".
    */
  def this(numPairs: Int,
           guyPrefers: Map[Int, List[Int]],
           girlPrefers: Map[Int, List[Int]]) {
    this(List.range(1, numPairs + 1).map(_.toString), List.range(1, numPairs + 1).map(_.toString),
      guyPrefers.map(entry => entry._1.toString -> entry._2.map(_.toString)),
      girlPrefers.map(entry => entry._1.toString -> entry._2.map(_.toString)))
  }
}
