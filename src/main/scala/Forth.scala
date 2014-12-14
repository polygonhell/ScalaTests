
/**
 * Created by rob on 12/13/14.
 */

import scalaz.Functor
import scalaz.Free
import scalaz.State
import scalaz.~>


sealed trait ForthOperators[A]

final case class Push[A](value: Int, o: A) extends ForthOperators[A]
final case class Add[A](o: A) extends ForthOperators[A]
final case class Mul[A](o: A) extends ForthOperators[A]
final case class Dup[A](o: A) extends ForthOperators[A]
final case class End[A](o: A) extends ForthOperators[A]


object Forth {
  implicit val ForthFunctor: Functor[ForthOperators] = new Functor[ForthOperators] {
    def map[A, B](fa: ForthOperators[A])(f: A => B): ForthOperators[B] =
      fa match {
        case Push(value, cont) => Push(value, f(cont))
        case Add(cont) => Add(f(cont))
        case Mul(cont) => Mul(f(cont))
        case Dup(cont) => Dup(f(cont))
        case End(cont) => End(f(cont))
      }
  }

  type ForthProg[A] = Free[ForthOperators, A]

  import scala.language.implicitConversions
  implicit def liftForth[A](forth: ForthOperators[A]): ForthProg[A] = Free.liftF(forth)

  def push(value: Int)  = Push(value, ())
  def add = Add(())
  def mul = Mul(())
  def dup = Dup(())
  def end = End(())

}


object RunProgram  {
  import Forth._

  // Note this is not tail recursive
  final def runProgram(stack: List[Int], program: ForthProg[Unit]): List[Int] = program.fold(
  _ => stack, {
    case Push(value, cont) =>
      runProgram(value :: stack, cont)
    case Add(cont) =>
      val a :: b :: tail = stack
      runProgram((a + b) :: tail, cont)
    case Mul(cont) =>
      val a :: b :: tail = stack
      runProgram((a * b) :: tail, cont)
    case Dup(cont) =>
      val a :: tail = stack
      runProgram(a :: a :: tail, cont)
    case End(cont) =>
      stack
  })


  def printFn( program: ForthOperators[ForthProg[Unit]]) : ForthProg[Unit] = program match {
    case Push(v, cont) =>
      println(s"Push $v")
      cont
    case Add(cont) =>
      println(s"Add")
      cont
    case Mul(cont) =>
      println(s"Mul")
      cont
    case Dup(cont) =>
      println(s"Dup")
      cont
    case End(cont) =>
      println(s"End")
      Free.point(())
  }

  def runFn(stack: List[Int], program: ForthOperators[ForthProg[Unit]]): (List[Int], ForthProg[Unit]) = program match {
    case Push(value, cont) =>
      (value :: stack, cont)
    case Add(cont) =>
      val a :: b :: tail = stack
      ((a + b) :: tail, cont)
    case Mul(cont) =>
      val a :: b :: tail = stack
      ((a * b) :: tail, cont)
    case Dup(cont) =>
      val a :: tail = stack
      (a :: a :: tail, cont)
    case End(cont) =>
      (stack, Free.point(()))
  }


}

object NT {
  type Stack = List[Int]
  type StackState[A] = State[Stack, A]

  def runProgram: ForthOperators ~> StackState = new (ForthOperators ~> StackState) {
    def apply[A](t: ForthOperators[A]) : StackState[A] = t match {
      case Push(value : Int, cont) =>
        State((a: Stack) => (value::a, cont))
      case Add(cont) =>
        State((stack : Stack) => {
          val a :: b :: tail = stack
          ((a + b) :: tail, cont)
        })
      case Mul(cont) =>
        State((stack : Stack) => {
          val a :: b :: tail = stack
          ((a * b) :: tail, cont)
        })
      case Dup(cont) =>
        State((stack : Stack) => {
          val a :: tail = stack
          (a :: a :: tail, cont)
        })
      case End(cont) =>
        // This doesn't work as intended there may not
        // be a way to do this using ~>
        State((a : Stack) => (a, cont))
    }
  }

  import scalaz.Id._

  def printProgram: ForthOperators ~> Id = new (ForthOperators ~> Id) {
    def apply[A](t: ForthOperators[A]): Id[A] = t match {
      case Push(value: Int, cont) =>
        println(s"Push $value")
        cont
      case Add(cont) =>
        println("Add")
        cont
      case Mul(cont) =>
        println("Mul")
        cont
      case Dup(cont) =>
        println("Dup")
        cont
      case End(cont) =>
        println("End")
        cont
    }
  }




}

object test  {
  def main(args: Array[String])  = {
    import Forth._

    val square = for {
      _ <- dup
      _ <- mul
    } yield ()

    val testProg = for {
      _ <- push(3)
      _ <- square
      _ <- push(4)
      _ <- square
      _ <- add
    } yield ()


    val newTest = for {
      _ <- push(5)
      _ <- push(6)
    } yield ()


    //
    //Free.runFC(testProg)(NT.runProgram).exec(List[Int]()).out

    // Using Natural Transformations
    println(testProg.foldMap(NT.runProgram).exec(List[Int]()))

    testProg.foldMap(NT.printProgram)

    println("=======")
    testProg.go(RunProgram.printFn)
    println("=======")

    // Run 3 Steps
    testProg.bounce(RunProgram.printFn).bounce(RunProgram.printFn).bounce(RunProgram.printFn)

    println("=======")

    println (testProg.foldRun(List[Int]())(RunProgram.runFn)._1)


    import RunProgram.runFn
    println(testProg.foldRun(List[Int]())(runFn))

    def runIt = (a : Free[ForthOperators, Unit]) => a.foldRun(List[Int]())(RunProgram.runFn)._1
    println (runIt(testProg))

    import RunProgram.runProgram
    println (runProgram(Nil, testProg))


  }
}

