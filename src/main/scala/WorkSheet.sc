import ASTExperiments._
import ASTExperiments.Syntax._
import scalaz._
import Scalaz._

constantFoldM(testExpr)(RewriteState(0))

constantFold(testExpr)

