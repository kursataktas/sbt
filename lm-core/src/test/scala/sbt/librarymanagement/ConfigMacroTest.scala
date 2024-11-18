package sbt.librarymanagement

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class ConfigMacroTest extends AnyFunSpec with Matchers {
  describe("Configurations.config") {
    it("should validate the ID in compile time") {
      """val A = Configurations.config("a")""" should compile
      """val b = Configurations.config("b")""" shouldNot compile
    }
  }
}