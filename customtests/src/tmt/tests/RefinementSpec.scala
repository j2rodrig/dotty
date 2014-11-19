package tmt.tests

import TestRunCompiler._
import org.scalatest._

class RefinementSpec extends FlatSpec with Matchers {

	val location = "refinement/"

	def tryTest(testName: String) = {
		if (!TestRunCompiler(location + testName))
			fail(s"""Test "$testName" failed.""" + TestRunCompiler.getLastOutput)
	}
	
	"A generic type" should "only be assignable if type parameters are compatible" in {
		tryTest("listtype1")  // NOTE: re-examine test when receiver/parameter mutability is implemented
	}
	it should "follow viewpoint adaptation rules" in {
		tryTest("listtype2")  // NOTE: re-examine test when dotty's annotation-dropping bug is fixed
		tryTest("listtype3")  // NOTE: re-examine test when receiver/parameter mutability is implemented
		tryTest("listtype4")  // NOTE: re-examine test when receiver/parameter mutability is implemented
	}

	"Recursively-defined polymorphic parameters" should "not cause infinite loops" in {
		tryTest("polydef1")
	}
}