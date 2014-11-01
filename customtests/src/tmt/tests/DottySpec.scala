/**
These tests are standard dotty compiler unit tests that have failed in the past.
They are included here so we don't re-break tests that have previously been fixed
	(that is, so these tests can be re-checked without having to re-run the entire sbt test suite).
*/

package tmt.tests

import TestRunCompiler._
import org.scalatest._

class DottySpec extends FlatSpec with Matchers {

	val location = "dotty/"

	def tryTest(testName: String) = {
		if (!TestRunCompiler(location + testName))
			fail(s"""Test "$testName" failed.""" + TestRunCompiler.getLastOutput)
	}
	
	"Dotty tests" should "pass" in {
		tryTest("desugar")
	}
	
	"Errors" should "not cause stack overflows" in {
		tryTest("tailrec-2")
	}
}