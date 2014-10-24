package tmt.tests

import scala.language.postfixOps
import collection.mutable.StringBuilder
import sys.process._
import java.io.ByteArrayInputStream

object TestRunCompiler {
	def apply(testName: String): Boolean = {
		apply(s"tests/$testName.scala", s"tests/expected/$testName.out", s"tests/expected/$testName.err")
	}
	
	/// Runs the Dotty compiler on a given input file, testing its standard output and standard error
	/// against the contents of the given output/error files.
	/// Returns a DIFF of the expected vs. actual outputs. If expected is the same as actual,
	/// then the diff will be empty.
	def apply(inputFile: String, expectedOutputFile: String, expectedErrorFile: String): Boolean = {
		val outbuffer = new StringBuilder(4096)
		val errbuffer = new StringBuilder(4096)
		val logger = ProcessLogger(
			o => outbuffer ++= s"$o\n",
			e => errbuffer ++= s"$e\n")
		s"../bin/dotc ${inputFile}" ! logger

		outbuffer.toString.equals(s"cat ${expectedOutputFile}"!!) &&
			errbuffer.toString.equals(s"cat ${expectedErrorFile}"!!)
	}
}


// Language design notes:
// What would it mean if the apply methods could be moved outside any class/object?
// ........
