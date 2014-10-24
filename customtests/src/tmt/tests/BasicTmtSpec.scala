package tmt.tests

import TestRunCompiler._
import org.scalatest._

class BasicTmtSpec extends FlatSpec with Matchers {

	def tryTest(testName: String) = {
		if (!TestRunCompiler(testName)) fail(s"""Test "$testName" failed.""")
	}

	"A variable" should "require its right-hand-side TMT to be compatible with its declared TMT" in {
		tryTest("vardef1")
		tryTest("vardef2")
	}
	it should "have the same TMT as the right-hand-side if no TMT is given" in {
		tryTest("varcopydef1")
	}
	
	"An expression definition" should "require its right-hand-side TMT to be compatible with its declared TMT" in {
		tryTest("exprdef1")
		tryTest("exprdef2")
	}
	it should "have the same TMT as the right-hand-side if no TMT is given" in {
		tryTest("exprcopydef1")
	}
	
	"A method" should "require its right-hand-side TMT to be compatible with its declared TMT" in {
		tryTest("methdef1")
		tryTest("methdef2")
	}
	it should "have the same TMT as the right-hand-side if no TMT is given" in {
		tryTest("methcopydef1")
	}
	
	/*"A polymorphic method" should "require its right-hand-side TMT to be compatible with its declared TMT" in {
		tryTest("polymethdef1")
		tryTest("polymethdef2")
	}
	it should "have the same TMT as the right-hand-side if no TMT is given" in {
		tryTest("polymethcopydef1")
	}*/
	
	"An assignment" should "not write to objects accessed via readonly references" in {
		tryTest("assignpath1")
	}
	it should "check TMT constraints" in {
		tryTest("assignvars1")
		tryTest("assignvars2")
	}
	
	"An access path" should "transitively preserve the readonly constraint" in {
		tryTest("accesspath1")
		tryTest("accesspath2")
	}
	
	"A method application" should "check that arguments match parameters" in {
		//assert(TestRunCompiler("arguments1"))
		//TestRunCompiler("arguments2")
	}
		
	it should "result in correct TMT constraints" in {
		tryTest("results1")
		//TestRunCompiler("results2")
	}

	/*"Dotty compiler" should "run" in {
		TestRunCompiler("D")
		//TestRunCompiler("../t/D.scala", "D.out", "D.err")
	}*/

  /*"A Stack" should "pop values in last-in-first-out order" in {
    val stack = new Stack[Int]
    stack.push(1)
    stack.push(2)
    stack.pop() should be (2)
    stack.pop() should be (1)
  }

  it should "throw NoSuchElementException if an empty stack is popped" in {
    val emptyStack = new Stack[Int]
    a [NoSuchElementException] should be thrownBy {
      emptyStack.pop()
    } 
  }*/
  
  
}
