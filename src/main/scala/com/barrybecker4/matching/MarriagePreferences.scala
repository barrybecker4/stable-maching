package com.barrybecker4.matching

/**
  * Encapsulates preferences for the guys and girls.
  */
class MarriagePreferences(val guys: List[String],
                          val girls: List[String],
                          val guyPrefers: Map[String, List[String]],
                          val girlPrefers: Map[String, List[String]])
