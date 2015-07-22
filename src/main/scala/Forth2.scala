package forth2

/**
 * Created by rob on 12/13/14.
 */

import scalaz.Free
import scalaz.State
import scalaz.~>


sealed trait ForthOperators[A]

final case class Push(value: Int) extends ForthOperators[Unit]
final case object Add extends ForthOperators[Unit]
final case object Mul extends ForthOperators[Unit]
final case object Dup extends ForthOperators[Unit]


object Forth {
  type ForthProg[A] = Free.FreeC[ForthOperators, A]

  import scala.language.implicitConversions
  implicit def liftForth[A](forth: ForthOperators[A]): ForthProg[A] = Free.liftFC(forth)

  def push(value: Int)  = Push(value)
  def add = Add
  def mul = Mul
  def dup = Dup
}



object Transforms {
  type Stack = List[Int]
  type StackState[A] = State[Stack, A]

  def runProgram: ForthOperators ~> StackState = new (ForthOperators ~> StackState) {
    def apply[A](t: ForthOperators[A]) : StackState[A] = t match {
      case Push(value : Int) =>
        State((a: Stack) => ((value::a), ()))
      case Add =>
        State((stack : Stack) => {
          val a :: b :: tail = stack
          ((a + b) :: tail, ())
        })
      case Mul =>
        State((stack : Stack) => {
          val a :: b :: tail = stack
          ((a * b) :: tail, ())
        })
      case Dup =>
        State((stack : Stack) => {
          val a :: tail = stack
          (a :: a :: tail, ())
        })
    }
  }

  import scalaz.Id._

  def printProgram: ForthOperators ~> Id = new (ForthOperators ~> Id) {
    def apply[A](t: ForthOperators[A]): Id[A] = t match {
      case Push(value: Int) =>
        println(s"Push $value")
      case Add =>
        println("Add")
      case Mul =>
        println("Mul")
      case Dup =>
        println("Dup")
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
    import Transforms.printProgram
    Free.runFC(testProg)(printProgram)



    // Using Natural Transformations
    import Transforms.runProgram
    println(Free.runFC(testProg)(runProgram).exec(List[Int]()))

  }
}

