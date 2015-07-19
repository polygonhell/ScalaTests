import LensTest._
import LensTest.Syntax._
import scalaz._
import Scalaz._
//import scalaz.std.list._
//import scalaz.Applicative._
import scalaz.Monad._

printTree (exampleTree)
val aa = mergeTrees(exampleTree, exampleTree2)
printTree(aa)
val bb = mergeTrees(aa, exampleTree3)
printTree(bb)
(1.some |@| 2.some) {_ + _}
1.some.map {_ + 1}
def ff (a: MyTree) : Option[MyTree] = a.some
1.point[Option]
def transform(e: MyTree) = {
  def f (in:MyTree) : MyTree = {
    if (treeName.get(in) == "There") treeName.set(in, "Poop") else in
  }
  descend(f, e)
}
val yy = descend((a => treeName.set(a, "poop")), exampleTree)
printTree(transform(exampleTree))
val map = Map("There" -> "Three", "Hello" -> "Goodbye")
val zz = transform2(exampleTree)(map)
printTree(zz._2)
val aaa = uberT(exampleTree)(0)
aaa._1
printTree(aaa._2)
val aaaa = uberT2(exampleTree)(0)
aaaa._1
printTree(aaaa._2)


val zzz = (t1 >>> t2)(exampleTree)(0)

import scalaz.Kleisli
val zzzz = (Kleisli(t1) >==> t2).run

