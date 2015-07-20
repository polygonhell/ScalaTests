import scala.language.higherKinds
import scalaz._
import scalaz.Scalaz._

/**
 * Created by rob on 7/19/15.
 */
object ASTExperiments {



  sealed trait Expr

  final case class IntVal(i: Int) extends Expr
  final case class Neg(a: Expr) extends Expr
  final case class Add(a:Expr, b:Expr) extends Expr
  final case class Sub(a:Expr, b:Expr) extends Expr
  final case class Mul(a:Expr, b:Expr) extends Expr
  final case class Div(a:Expr, b:Expr) extends Expr


  val testExpr = Add(IntVal(4), Neg(Mul(IntVal(4), Add(IntVal(2), IntVal(3)))))


  // Bottom up rewite
  def bottomUpM[M[_]: Monad](fn: Expr => M[Expr], e: Expr) : M[Expr] = {
    val processed : M[Expr] = e match {
      case IntVal(i: Int) => i.point[M].map(IntVal)
      case Neg(a: Expr) => bottomUpM(fn, a).map(Neg)
      case Add(a: Expr, b: Expr) => (bottomUpM(fn, a) |@| bottomUpM(fn, b)) {Add}
      case Sub(a: Expr, b: Expr) => (bottomUpM(fn, a) |@| bottomUpM(fn, b)) {Sub}
      case Mul(a: Expr, b: Expr) => (bottomUpM(fn, a) |@| bottomUpM(fn, b)) {Mul}
      case Div(a: Expr, b: Expr) => (bottomUpM(fn, a) |@| bottomUpM(fn, b)) {Div}
    }
    processed.flatMap(fn)
  }

  def bottomUp(fn: Expr => Expr, e:Expr) : Expr = bottomUpM(fn andThen(_.point[Id]), e).copoint

  // Composition operators for the two function types supported by the tree Walk
  object Syntax {
    implicit class ASTSyntax[M[_]: Monad](x: Expr => M[Expr]) {
      def andThen(that: Expr => M[Expr]) : Expr => M[Expr] = a => x(a).flatMap(that)
      var >>> = andThen _
    }

    implicit class ASTSyntax2(x: Expr => Expr) {
      // andThen already defined for single argument functions
      def >>>(that : Expr => Expr) = x andThen that
    }

  }

  import Syntax._

  case class RewriteState( constantsFolded : Int ) {
    def accumConstantsFolded(v:Int) = this.copy(constantsFolded = constantsFolded+v)
  }
  type RewriteStateM[A] = scalaz.State[RewriteState, A]

  def constantFoldM(e: Expr) : RewriteStateM[Expr] = {
    def binOp (a: Int, b: Int, fn: (Int, Int) => Int)  : RewriteStateM[Expr] = {
      for {
        s <- modify[RewriteState](_.accumConstantsFolded(2))
      } yield IntVal(fn (a, b))
    }

    def fn(e:Expr) : RewriteStateM[Expr] = e match {
      case Neg(IntVal(i)) =>
        for {
          s <- modify[RewriteState](_.accumConstantsFolded(1))
        } yield IntVal(-i)

      case Add(IntVal(a), IntVal(b)) => binOp(a, b, _ + _)
      case Sub(IntVal(a), IntVal(b)) => binOp(a, b, _ - _)
      case Mul(IntVal(a), IntVal(b)) => binOp(a, b, _ * _)
      case Div(IntVal(a), IntVal(b)) => binOp(a, b, _ / _)
      case _ => e.point[RewriteStateM]
    }

    bottomUpM(fn, e)
  }


  // Stateless version
  def constantFold(e: Expr) : Expr = {
    def binOp(a: Int, b: Int, fn: (Int, Int) => Int): Expr = IntVal(fn(a, b))

    def fn(e: Expr): Expr = e match {
      case Neg(IntVal(i)) => IntVal(-i)
      case Add(IntVal(a), IntVal(b)) => binOp(a, b, _ + _)
      case Sub(IntVal(a), IntVal(b)) => binOp(a, b, _ - _)
      case Mul(IntVal(a), IntVal(b)) => binOp(a, b, _ * _)
      case Div(IntVal(a), IntVal(b)) => binOp(a, b, _ / _)
      case _ => e
    }
    bottomUp(fn, e)
  }


}
