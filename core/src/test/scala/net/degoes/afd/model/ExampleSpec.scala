package net.degoes.afd

import zio.test._ 
import zio.test.TestAspect._

object ExampleSpec extends ZIOSpecDefault {
  
  def spec = 
    suite("ExampleSpec") {
      test("example test") {
        assertTrue(true)
      }
    }
}