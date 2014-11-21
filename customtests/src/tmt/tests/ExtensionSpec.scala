package tmt.tests

import TestRunCompiler._
import org.scalatest._

class OverrideSpec extends FlatSpec with Matchers {

	val location = "extension/"

	def tryTest(testName: String) = {
		if (!TestRunCompiler(location + testName))
			fail(s"""Test "$testName" failed.""" + TestRunCompiler.getLastOutput)
	}
	
	"An outer-scope parameter" should "TMT-check against its outer-scope reference" in {
		tryTest("outerparams1")
	}
}