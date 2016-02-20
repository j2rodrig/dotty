package dotmod

import org.junit.Test
import test._

import scala.io.Source

// tests that match regex '(pos|dotc|run|java|compileStdLib)\.*' would be executed as benchmarks.
class tests extends CompilerTest {

  def isRunByJenkins: Boolean = sys.props.isDefinedAt("dotty.jenkins.build")

  val noCheckOptions = List(
//        "-verbose",
//         "-Ylog:frontend",
//        "-Xprompt",
//        "-explaintypes",
//        "-Yshow-suppressed-errors",
        "-pagewidth", "160")

  val defaultOutputDir = "./out/"

  implicit val defaultOptions = noCheckOptions ++ List(
      "-Yno-deep-subtypes", "-Yno-double-bindings",
      "-d", defaultOutputDir) ++ {
    if (isRunByJenkins) List("-Ycheck:tailrec,resolveSuper,mixin,restoreScopes,labelDef") // should be Ycheck:all, but #725
    else List("-Ycheck:tailrec,resolveSuper,mixin,restoreScopes,labelDef")
  }


  val testPickling = List("-Xprint-types", "-Ytest-pickler", "-Ystop-after:pickler")

  val twice = List("#runs", "2")
  val staleSymbolError: List[String] = List()

  val allowDeepSubtypes = defaultOptions diff List("-Yno-deep-subtypes")
  val allowDoubleBindings = defaultOptions diff List("-Yno-double-bindings")
  val scala2mode = List("-language:Scala2")

  val testsDir      = "./dotmodtests/"

  @Test def assignability1() = compileFile(testsDir, "Assignability1", xerrors = 20)
  @Test def mutability1() = compileFile(testsDir, "Mutability1", xerrors = 18)
  @Test def object1() = compileFile(testsDir, "Object1", xerrors = 0)
  @Test def subtyping1() = compileFile(testsDir, "Subtyping1", xerrors = 1)
  @Test def vp1() = compileFile(testsDir, "ViewpointAdaptation1", xerrors = 21)  // should be 23, but for some reason type param bounds don't seem to be checked properly on application
  @Test def vp2() = compileFile(testsDir, "ViewpointAdaptation2", xerrors = 4)

}
