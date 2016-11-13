package com.barrybecker4.matching

import org.scalatest.FunSuite

/**
  * @author Barry Becker
  */
class SMPSuite extends FunSuite {

  test("test stable marriage") {

    SMP.run()
    assert(true)
  }

}