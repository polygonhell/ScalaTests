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

