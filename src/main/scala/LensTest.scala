//import scalaz.Lens
import scalaz._
//import std.list._
//import scalaz.syntax._
import Scalaz._
import scala.language.higherKinds
/**
 * Created by rob on 7/18/15.
 */
object LensTest {

  sealed trait MyTree
  final  case class MyNode(name: String, ch: List[MyTree]) extends MyTree
  final case class MyLeaf(name: String) extends MyTree

  val nodeName = Lens.lensu[MyNode, String] (
    (a, value) => a.copy(name = value),
    _.name
  )

  val nodeChildren = Lens.lensu[MyNode, List[MyTree]] (
    (a, v) => a.copy(ch = v),
    _.ch
  )

  val children = Lens.lensu[MyTree, List[MyTree]] (
    (a, v) => a match{
      case n: MyNode => nodeChildren.set(n, v)
      case n: MyLeaf => n
    },
    a => a match {
      case n: MyNode => n.ch
      case n: MyLeaf => List.empty[MyTree]
    }
  )

  val leafName = Lens.lensu[MyLeaf, String] (
    (a, value) => a.copy(name = value),
    _.name
  )

  val treeName = Lens.lensu[MyTree, String] (
    (a, v) => a match {
      case n: MyNode => nodeName.set(n, v)
      case n: MyLeaf => leafName.set(n, v)
    },
    a => a match {
      case n: MyNode => nodeName.get(n)
      case n: MyLeaf => leafName.get(n)
    }

  )


//  def child(i: Int) = children.at(i)



  def spaces(n: Int) : String = " " * n
  def printTree(tree: MyTree, sp: Int = 0) : String = {
    tree match {
      case n: MyNode =>
        (spaces(sp) ++ nodeName.get(n) ++ "\n") ++ ((children.get(n).map{a => printTree(a, sp+2)}).mkString(""))
      case l: MyLeaf =>
        (spaces(sp) ++ leafName.get(l) ++ "\n")
    }
  }


  val exampleTree = MyNode("Hello", List(MyNode("There", List(MyLeaf("What"))), MyLeaf("thing")))
  val exampleTree2 = MyNode("Hello", List(MyNode("There", List(MyLeaf("Poop")))))
  val exampleTree3 = MyNode("Hello", List(MyNode("There", List(MyNode("Poop", List(MyLeaf("Doo")))))))


  def mergeTrees(a: MyTree, b: MyTree) : MyTree = {
    if (treeName.get(a) == treeName.get(b)) {
      // merge the children
      // We need the items in B that are not in A
      val childrenOfA = children.get(a)
      val childrenOfB = children.get(b)
      val aligned = (childrenOfA ++ childrenOfB).groupBy(treeName.get(_))
      val newChildren = aligned.values.map { a => a.length match {
        case 1 => a(0)
        case 2 => mergeTrees(a(0), a(1))
      }}
      val res = a match {
        case nn :MyNode => children.set(nn, newChildren.toList)
        case nn: MyLeaf => MyNode(leafName.get(nn), newChildren.toList)
      }
      res
    } else
      sys.error("ow")
  }


  def walkM[M[_] : Monad, A](f : MyTree => M[MyTree], e: MyTree) : M[MyTree] = e match {
    case in: MyNode =>
      f(in).flatMap(n => children.get(n).map(walkM(f, _)).sequence.map(children.set(n, _)))
    case n: MyLeaf =>
      f(n)
  }

  def walk (f: MyTree => MyTree, e: MyTree) : MyTree = {
    val fn = f andThen(a => a.point[Id])
    val res : Id[MyTree] = walkM(fn, e)
    res.copoint
  }

  def composeTransform(f: MyTree => MyTree, g: MyTree => MyTree) : MyTree => MyTree = {
    walk (f andThen g, _)
  }

  def composeTransformM[M[_] : Monad](f: MyTree => M[MyTree], g: MyTree => M[MyTree]) : MyTree => M[MyTree] = {
    (a: MyTree) => f(a).flatMap(g(_))
  }


  case class RewriteM[A](v: A)

  trait RewriteInstances {
    implicit val rewriteInstance = new Monad[RewriteM] {
      def point[A](a: => A): RewriteM[A] = RewriteM(a)
      def bind[A, B](fa: RewriteM[A])(f: (A) => RewriteM[B]): RewriteM[B] = f(fa.v)
    }
  }

  case object RewriteM extends RewriteInstances


  type RWMap = Map[String, String]
  type RWState[A] = scalaz.State[RWMap, A]

  def transform2(e: MyTree) : RWState[MyTree] = {
    def fn(e: MyTree) : RWState[MyTree] = {
      for {
        m <- get[RWMap]
      } yield {
        val name = treeName.get(e)
        treeName.set(e, m.getOrElse(name, name))
      }
    }
    walkM(fn, e)
  }

  type RWState2[A] = scalaz.State[Int, A]

  def renameTransform(e: MyTree, nameMap: RWMap) : RWState2[MyTree] = {
    def fn(e: MyTree) : RWState2[MyTree] = {
      for {
        count <- get[Int]
        name = treeName.get(e)
        newTree = treeName.set(e, nameMap.getOrElse(name, name))
        _ <- put(if (name == treeName.get(newTree)) count else count+1)
      } yield {
        newTree
      }
    }
    walkM(fn, e)
  }

  val t1 = (a:MyTree) =>  renameTransform(a, Map("There" -> "Three", "Hello" -> "Goodbye"))
  val t2 = (a:MyTree) =>  renameTransform(a, Map("Three" -> "Poop"))
  val uberT = composeTransformM(t1, t2)

  object Syntax {
    implicit class RWState2Syntax[M[_]: Monad](x: MyTree => M[MyTree]) {
      def andThen(that: MyTree => M[MyTree]) : MyTree => M[MyTree] = {
        composeTransformM(x, that)
      }

      var >>> = andThen _
    }
  }

  import Syntax._

  val uberT2 = t1 >>> t2

//  val uberT = composeTransformM(transform2(_)(Map("There" -> "Three", "Hello" -> "Goodbye")), transform2(_)(Map("There" -> "Poop")))


}
