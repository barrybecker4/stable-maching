package com.barrybecker4.matching

import org.scalatest.FunSuite

/**
  * @author Barry Becker
  */
class SMPSuite extends FunSuite {

  test("test stable marriage") {


    val guys: List[String] = "abe" :: "bob" :: "col" :: "dan" :: "ed" :: "fred" :: "gav" :: "hal" :: "ian" :: "jon" :: Nil
    val girls: List[String] = "abi" :: "bea" :: "cath" :: "dee" :: "eve" :: "fay" :: "gay" :: "hope" :: "ivy" :: "jan" :: Nil

    val guyPrefers: Map[String, List[String]] = Map(
      "abe" -> List("abi", "eve", "cath", "ivy", "jan", "dee", "fay", "bea", "hope", "gay"),
      "bob" -> List("cath", "hope", "abi", "dee", "eve", "fay", "bea", "jan", "ivy", "gay"),
      "col" -> List("hope", "eve", "abi", "dee", "bea", "fay", "ivy", "gay", "cath", "jan"),
      "dan" -> List("ivy", "fay", "dee", "gay", "hope", "eve", "jan", "bea", "cath", "abi"),
      "ed" -> List("jan", "dee", "bea", "cath", "fay", "eve", "abi", "ivy", "hope", "gay"),
      "fred" -> List("bea", "abi", "dee", "gay", "eve", "ivy", "cath", "jan", "hope", "fay"),
      "gav" -> List("gay", "eve", "ivy", "bea", "cath", "abi", "dee", "hope", "jan", "fay"),
      "hal" -> List("abi", "eve", "hope", "fay", "ivy", "cath", "jan", "bea", "gay", "dee"),
      "ian" -> List("hope", "cath", "dee", "gay", "bea", "abi", "fay", "ivy", "jan", "eve"),
      "jon" -> List("abi", "fay", "jan", "gay", "eve", "bea", "dee", "cath", "ivy", "hope"))


    val girlPrefers: Map[String, List[String]] = Map(
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


    val smp: SMP = new SMP()

    assert(smp.run(guys, girls, guyPrefers, girlPrefers))
  }

  /*
  test("test stable marriage fail") {
    assert(false)
  }*/

}