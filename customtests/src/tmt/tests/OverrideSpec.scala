package tmt.tests

import TestRunCompiler._
import org.scalatest._

class OverrideSpec extends FlatSpec with Matchers {

	val location = "override/"

	def tryTest(testName: String) = {
		if (!TestRunCompiler(location + testName))
			fail(s"""Test "$testName" failed.""" + TestRunCompiler.getLastOutput)
	}
	
	"Overriding methods" should "be compatible with overridden methods" in {
		tryTest("override1")
	}
}