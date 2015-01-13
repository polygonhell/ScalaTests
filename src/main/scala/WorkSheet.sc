//val code = Toy.test
//code.length
//val cl = OwnClassLoader.defineClass("pkg.OwnClass", code)
//val instance = cl.newInstance
//val methods = cl.getDeclaredMethods

//Toy.test2
//val code = Toy.test
//val cl = OwnClassLoader.defineClass("OwnClass", code)
//val instance = cl.newInstance()
//val methods = cl.getDeclaredMethods
//methods.foreach { m => m.getParameterTypes match {
//  case Array() =>
//    println (m.invoke(instance))
//  case Array(a) if a == classOf[Array[String]] =>
//    println(m.invoke(instance, Array[String]()))
//  case s =>
//    println ("Unknown " + s)
//}}
//test.main(Array())

import scalaz._
import Scalaz._

def fn(a:Int, b: Int) = a + b

val a = fn _ curried 3

val pf: PartialFunction[Int, Boolean] = { case i if i > 0 => i % 2 == 0}
val lifted = pf.lift
lifted(1)

val aa = Some(3)


def add3(x: Int) = x+3

Monad[Option].lift(add3)

//3.liftM

//add3.liftM
//
//val p = for {
//  a <- 3.some
//  b <- Monad[Option].liftM .lift(add3 _)(a.some)
//} yield(b)









//val add3 = fn.curried(3)
//
//
//3.some
//Some(3)
//
//
//
//test3.run







