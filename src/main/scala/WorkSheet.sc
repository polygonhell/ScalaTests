import ASTExperiments._
import ASTExperiments.Syntax._
import scalaz._
import Scalaz._

repeat(repeat(reduce) >>> repeat(constantFold)) (testExpr)
testExpr
//val reducer = repeatM(reduceM)
//reducer(testExpr)(RewriteState(0))
//val a = (repeatM(repeatM(reduceM) >>> repeatM(constantFoldM)))(testExpr)(RewriteState(0))
//a._1
//a._2

