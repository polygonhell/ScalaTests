/**
 * Created by rob on 12/20/14.
 */


import scalaz.State
import scalaz.syntax.state._


object StackContainer {

  type Stack[A] = State[List[Int], A]

  def push(a: Int) : Stack[Unit] = State.modify((xs: List[Int]) => a::xs)

  val pop : Stack[Int] = {
    for {
      stack <- State.get[List[Int]]
      _ <- State.put(stack.tail)
    } yield(stack.head)
  }

}


object test3 {
  import StackContainer._
  def manipStack: Stack[Int] = {
    for {
      _ <- push(7)
      _ <- push(14)
      _ <- push(21)
      _ <- pop
      a <- pop
    } yield a
  }

  def run = manipStack(List[Int]())
}
