import java.io.{FileOutputStream, PrintWriter}

import jdk.internal.org.objectweb.asm.util.CheckClassAdapter
import org.objectweb.asm._
import org.objectweb.asm.Opcodes._

class Poo {

}


object Toy {

  def test2 = {
    val inStr = classOf[Poo].getResourceAsStream("/Poo.class");

    val classReader = new jdk.internal.org.objectweb.asm.ClassReader(inStr)
    val printWriter = new PrintWriter(System.out)
    CheckClassAdapter.verify(classReader, true, printWriter)

  }

  def test: Array[Byte] = {
    val cw = new ClassWriter(0)//ClassWriter.COMPUTE_MAXS | ClassWriter.COMPUTE_FRAMES)
    cw.visit(V1_6, ACC_PUBLIC | ACC_SUPER, "OwnClass", null, "java/lang/Object", null)

    // Constructor
    val constructorMv = cw.visitMethod(ACC_PUBLIC, "<init>", "()V", null, null)
    constructorMv.visitCode()
    constructorMv.visitVarInsn(ALOAD, 0)
    constructorMv.visitMethodInsn(INVOKESPECIAL, Type.getInternalName(classOf[Object]), "<init>", "()V", false)
    constructorMv.visitInsn(RETURN)

    constructorMv.visitMaxs(2,1)
    constructorMv.visitEnd()

    val helloMv = cw.visitMethod(ACC_PUBLIC, "hello", "()V", null, null)
    helloMv.visitCode()
    helloMv.visitFieldInsn(GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;")
    helloMv.visitLdcInsn("Hello, World")
    helloMv.visitMethodInsn(INVOKEVIRTUAL, "java/io/PrintStream", "println", "(Ljava/lang/String;)V", false)
    helloMv.visitInsn(RETURN)

    helloMv.visitMaxs(2,1)
    helloMv.visitEnd()

    val get2Mv = cw.visitMethod(ACC_PUBLIC, "get", "()Ljava/lang/String;", null, null)
    get2Mv.visitCode()
    get2Mv.visitLdcInsn("Hello, World")
    get2Mv.visitInsn(ARETURN)

    get2Mv.visitMaxs(2,1)
    get2Mv.visitEnd()


    val get1Mv = cw.visitMethod(ACC_PUBLIC, "get1", "()Ljava/lang/Object;", null, null)
    get1Mv.visitCode()
    get1Mv.visitTypeInsn(NEW, Type.getInternalName(classOf[Object]))
    get1Mv.visitInsn(DUP)
    get1Mv.visitMethodInsn(INVOKESPECIAL, Type.getInternalName(classOf[Object]), "<init>", "()V", false)
    get1Mv.visitInsn(ARETURN)

    get1Mv.visitMaxs(2,1)
    get1Mv.visitEnd()

    val mainMv = cw.visitMethod(ACC_PUBLIC | ACC_STATIC, "main", "([Ljava/lang/String;)V", null, null)
    mainMv.visitCode()
    mainMv.visitTypeInsn(NEW, "OwnClass")
    mainMv.visitInsn(DUP);
    mainMv.visitMethodInsn(INVOKESPECIAL, "OwnClass", "<init>", "()V", false);
    mainMv.visitMethodInsn(INVOKEVIRTUAL, "OwnClass", "hello", "()V", false);
    mainMv.visitInsn(RETURN)

    mainMv.visitMaxs(2,1)
    mainMv.visitEnd()



    cw.visitEnd()

    val classReader = new jdk.internal.org.objectweb.asm.ClassReader(cw.toByteArray)
    val printWriter = new PrintWriter(System.out)
    CheckClassAdapter.verify(classReader, true, printWriter)

    val bytes = cw.toByteArray
    val fos = new FileOutputStream("/Users/rob/OwnClass.class")
    fos.write(bytes)
    fos.close

    bytes
  }


}


object OwnClassLoader extends ClassLoader {
  def defineClass(name: String, b: Array[Byte]) : Class[_] = {
    defineClass(name, b, 0, b.length)
  }
}

