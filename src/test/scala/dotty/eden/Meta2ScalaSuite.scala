package scala.meta.eden

import scala.{meta => m}
import dotty.tools.dotc._
import ast.untpd

class Meta2ScalaSuite extends EdenSuite {

  def syntactic(code: String, expect: untpd.Tree) = {
    test(code) {
      val mTree: m.Tree = code
      var convertedTree: untpd.Tree = mTree
      assert(expect.toString == convertedTree.toString)
    }
  }

  def syntactic(code: String, verbose: Boolean = false): Unit = {
    test(code) {
      val dTree: untpd.Tree = code
      val mTree: m.Tree = code
      var convertedTree: untpd.Tree = null

      try { convertedTree = mTree } finally {
        if (convertedTree == null || dTree.toString != convertedTree.toString || verbose)
          debug
      }

      def debug = {
        println("<------------")
        println("code:" + code)
        println("dotty:" + dTree)
        println("meta:" + mTree.structure)
        if (convertedTree != null) println("conv:" + convertedTree)
        println("------------>")
      }

      assert(dTree.toString == convertedTree.toString)
    }
  }

  // literals
  syntactic("null")
  syntactic("true")
  syntactic("false")
  syntactic("56")
  syntactic("-56")
  /*
  syntactic("-56L")
  syntactic("-56l")
  syntactic("0x56")
  syntactic("-0x56")
  syntactic("-0x56L")
  syntactic("-0x56l")
  syntactic("56.45")
  syntactic("-56.45")
  syntactic("-56.45e10")
  syntactic("-56.45e-10")
  syntactic("-56.45e-10F")
  syntactic("-56.45e-10f")
  syntactic("-56.45e-10D")
  syntactic("-56.45e-10d")
  syntactic(".45")
  syntactic(".45e10")
  syntactic(".45e-10")
  syntactic(".45e-10F")
  syntactic(".45e-10f")
  syntactic(".45e-10D")
  syntactic(".45e-10d")
  syntactic("-56.45E10")
  syntactic("-56.45E-10")
  syntactic("-56.45E-10F")
  syntactic("-56.45E-10f")
  syntactic("-56.45E-10D")
  syntactic("-56.45E-10d")
  syntactic("4E10")
  syntactic("4E-10")
  syntactic("4E-10F")
  syntactic("4E-10f")
  syntactic("4E-10D")
  syntactic("4E-10d")
  syntactic("'a'")
  syntactic("'\u0041'")
  syntactic("'\n'")
  syntactic("'\t'")
  syntactic("Hello,\nWorld!")
  syntactic("This string contains a \" character.")
  syntactic("\"\"\"the present string\n" +
    "spans three\n" +
    "lines.\"\"\""
  ) */

  // exprs
  syntactic("""println("hello, world")""")
  syntactic("println(42)")
  syntactic("f(this)")
  syntactic("f(A.this)")
  syntactic("this.age")
  syntactic("C.this.age")
  syntactic("super.age")
  syntactic("super[A].age")
  syntactic("C.super[A].age")
  syntactic("f[Int](3)")
  syntactic("f(x = 3)")
  syntactic("f(x = 3, y = 6 * 8)")
  syntactic("f(x:_*)")
  syntactic("f(m(x):_*)")
  syntactic("a.f(this.age)")
  syntactic("a + b")
  syntactic("a + (b, c)")
  syntactic("a + (b, c, d)")
  syntactic("a + b + c + this.age")
  syntactic("a :+ b")
  syntactic("a +: b")
  syntactic("a +: (b, c)")
  syntactic("a +: (b, c, d)")
  syntactic("a :+ (b, c)")
  syntactic("a :+ (b, c, d)")
  syntactic("(b, c) :+ a")
  syntactic("(b, c, d) :+ a")
  syntactic("(b, c) +: a")
  syntactic("(b, c, d) +: a")
  syntactic("a*")
  syntactic("++a")
  syntactic("!a")
  syntactic("~a")
  syntactic("a++")
  syntactic("a = b")
  syntactic("{ a = 1; b += 2 }")
  syntactic("{ }")
  syntactic("()")
  syntactic("(2)")
  syntactic("(2, 4)")
  syntactic("a -> b")
  syntactic("if (cond) a else b")
  syntactic("if (cond) return a")
  syntactic("while (a > 5) { println(a); a++; }")
  syntactic("do { println(a); a++; } while (a > 5)")
  syntactic("return a")
  syntactic("new List(5)")
  syntactic("new List[Int](5)")
  syntactic("new List[List[Int]](List(5))")
  syntactic("new Map[Int, String]")
  syntactic(
    "new Map[Int, String]()",  // impossible to detect presence of ()
    "new Map[Int, String]"
  )
  syntactic("new Map[Int, String](a -> b)")
  syntactic("new B")
  syntactic("new B()",  "new B") // impossible to detect presence of ()
  syntactic("new c.B")
  syntactic("new C#B")
  syntactic("new o.C#B")
  syntactic("new B { }")
  syntactic("new B { val a = 3 }")
  syntactic("new B { def f(x: Int): Int = x*x }")
  syntactic("new B(3) { println(5); def f(x: Int): Int = x*x }")
  syntactic("throw new A(4)")
  syntactic("try { throw new A(4) } catch { case _: Throwable => 4 } finally { println(6) }")
  syntactic("try f(4) catch { case _: Throwable => 4 } finally println(6)")
  syntactic("try f(4) catch { case _: Throwable => 4 }")
  syntactic("try f(4) catch rescue")
  syntactic("try f(4) finally println(6)")
  syntactic("try {} finally println(6)")
  syntactic("for (arg <- args) result += arg * arg")
  syntactic("for (arg <- args; double = arg * 2) result += arg * arg")
  syntactic("""
    for { i<-1 until n
          j <- 1 until i
          if isPrime(i+j) } yield (i, j)
  """)
  syntactic("""
    for { i<-1 until n
          j <- 1 until i
          k = i + j
          if isPrime(i+j) } yield (i, j)
  """)

  // interpolation
  syntactic("""s"hello, $world"""")
  syntactic("""s"hello, $world, ${1 + 2}"""")

  // patterns
  syntactic("a match { case 5 => ; case 6 => }")
  syntactic("a match { case Some(x) => x; case None => y }")
  syntactic("a match { case Some(x) => x; case _ => y }")
  syntactic("a match { case m @ Some(x) => x; case _ => y }")
  syntactic("a match { case m @ Some(t @ Some(x)) => x; case _ => y }")
  syntactic("a match { case m : Int => x; case _ => y }")
  syntactic("a match { case Some(x: Int) | Some(x: String) => x; case _ => y }")
  syntactic("a match { case Some(x: Int) | Some(x: String) | Some(x: Boolean) => x; case _ => y }")
  syntactic("a match { case Some(x: Int) | Some(x: String) | x: Boolean => x; case _ => y }")
  syntactic("a match { case a.name => ; case _: b.M => ; case `x` => }")
  syntactic("a match { case x::xs =>  }")
  syntactic("a match { case x::y::xs =>  }")
  syntactic("a match { case (x: Int)::(y @ Some(_))::xs =>  }")
  syntactic("a match { case x::xs if x > 0 =>  }")
  syntactic("a match { case p @ (x, y) =>  }")
  syntactic("a match { case (Some(x), None) =>  }")
  syntactic("a match { case () =>  }")
  syntactic("a match { case (x) =>  }")
  // syntactic("a match { case M(a, _*) => }")
  // syntactic("a match { case (_:_*) =>  }")
  // syntactic("x match { case List(head, _*) => x }")
  syntactic("a match { case (_: A) =>  }")
  syntactic("a match { case Some[Int](x) => }")
  syntactic("a match { case x: Option[Int] => }")
  syntactic("a match { case x: Option[_] => }")
  syntactic("a match { case x: A#B => }")
  syntactic("a match { case x: c.M => }")
  syntactic("a match { case x: c.type => }")
  syntactic("a match { case x: M[(A, B)] => }")
  syntactic("a match { case x: ((A, B) => C) => }")
  syntactic("a match { case x: A | B => }")
  syntactic("a match { case x: (A & B) => }")
  syntactic("a match { case x: M[A | B] => false }")
  syntactic("a match { case x: M[A & B] => true }")
  syntactic("""x match { case q"A($a)" => a } """)
  syntactic("x match { case http.`*`(q) => q }")

  // definitions
  syntactic("val a = 3")
  syntactic("val a: Int = 3")
  syntactic("val a: List[List[Int]] = List(List(3))")
  syntactic("val a: Int")
  syntactic("val a, b: Int")
  syntactic("val a, b = 3")
  syntactic("val a, _ = 3")
  syntactic("val Some(Some(x)) = a")
  syntactic("""val q"object $name { ..$stats }" = q"object Test { println(1024) }"""")
  syntactic("val sched = new B with T { def x = 2 }")

  syntactic("var a = 3")
  syntactic("var a: List[Int] = List(3)")
  syntactic("var a: Int")
  syntactic("var a, b: Int")
  syntactic("var a, b = 3")
  syntactic("var a: Int = _")
  syntactic("var Some(Some(x)) = a")
  syntactic("var (Some(_), _) = a")
  syntactic("val _ = a")
  syntactic("var _ = a")

  syntactic("def f(x: Int): Int = x*x")
  syntactic("def f(x: Int = 5): Int = x*x")
  syntactic("def f(x: Int = 5)(y: Int): Int = x*y")
  syntactic("def f(x: Int*): Int = ???")
  syntactic("def f(x: Option[Int]*): Int = ???")
  syntactic("def f[T](x: T): Int = x")
  syntactic("def f[T >: Nothing <: Any ](x: T): Int = x")
  syntactic("def f[T :A](x: T): Int = x")
  syntactic("def f[T :A :B](x: T): Int = x")
  syntactic("def f[T :A[Int]](x: T): Int = x")
  syntactic("def f[T, M[_]](x: M[T]): M[T] = x")
  syntactic("def f[T, M[A]](x: M[T]): M[T] = x")
  syntactic("def f[T, M[A]](x: => M[T]): M[T] = x")
  syntactic("""
    object FilterTest extends Application {
      def filter(xs: List[Int], threshold: Int) = {
        def process(ys: List[Int]): List[Int] =
          if (ys.isEmpty) ys
          else if (ys.head < threshold) ys.head :: process(ys.tail)
          else process(ys.tail)
        process(xs)
      }
      println(filter(List(1, 9, 2, 8, 3, 7, 4), 5))
    }
  """)

  syntactic("type Age = Int")
  syntactic("type Age")
  syntactic("type Age >: Int <: Any")
  syntactic("type Age <: Int + Boolean")
  syntactic("type Container[T]")
  syntactic("type Container[T] = List[T]")
  syntactic("type Container[T] <: List[T] { def isEmpty: Boolean; type M }")
  syntactic("type Container[T] <: List[T] with Set[T] { def isEmpty: Boolean; type M }")
  syntactic("type Container[T] <: List[T] with Set[T] { def isEmpty: Boolean; type M = Int }")
  syntactic("type Container[T] <: List[T] with Set[T] { def isEmpty: Boolean; type M <: Int }")

  syntactic("trait A { def test(x: Int): Boolean; val x: Int }")
  syntactic("trait A { self: B => def test(x: Int): Boolean; val x: Int }")
  syntactic("trait A { self: B => def test(x: Int): Boolean; val x: Int; type Age >: Int <: Any }")
  syntactic("trait A { self: B => def test(x: Int): Boolean; val x: Int; type Age = Int }")
  syntactic("trait A { self: B => def test(x: Int): Boolean; val x: Int; type Container[T] = List[T] }")
  syntactic("trait A[T] extends B with C { def test(x: Int): Boolean; val x: Int }")
  syntactic("trait Service[F[_]]")
  syntactic("class A[T] extends B with C { def test(x: Int): Boolean; val x: Int }")
  syntactic("class A[+T] extends B with C { def test(x: Int): Boolean; val x: Int }")
  syntactic("class A[-T] extends B with C { def test(x: Int): Boolean; val x: Int }")
  syntactic("class A[T <: Any](a: Int) extends B(a) with C[T] { def test(x: Int): Boolean; val x: Int }")
  syntactic("class A[T <: C[T]](a: Int) extends B(a) with C[T] { def test(x: Int): Boolean; val x: Int }")
  syntactic("class A[T <: C[T]](a: Int) extends B(a) with C[T] { def test(x: Int): Boolean; var x: Int = _ }")
  syntactic("object A extends B with C { def test(x: Int): Boolean; val x: Int }")
  syntactic("class Y(x: Int) { def this() = this(1) }")
  syntactic("class Y(x: Int) { private def this() = this()(1) }")
  syntactic("case class Foo")
  syntactic("case object Foo")

  // functions
  syntactic("map(_ + 1)")
  syntactic("f(_ * _)")
  syntactic("map((_: Int) * 2)")
  syntactic("map(if (_) x else y)")
  syntactic("g(_.map(f))")
  syntactic("l.map(_ + 1)")
  syntactic("f(x => x + 1)")
  syntactic("f((x1, x2) => x1 * x2)")
  syntactic("f((x: Int) => (x: Int) * 2)")
  syntactic("f(z => if (z) x else y)")
  syntactic("f(x => x.map(f))")
  syntactic("f(x => x.map(y => y + 1))")
  syntactic("val f: Int => String = _.toString")
  syntactic("f _")
  // syntactic("a.foreach(_ => bar())")
  syntactic("a.foreach(_ + _)")


  // types
  // syntactic("var a: A with B = ???")
  syntactic("var a: m.A = ???")
  syntactic("var a: m.d.e.A = ???")
  syntactic("var a: m.List[m.t.A] = ???")
  syntactic("var a: A#B = ???")
  syntactic("var a: m.A#B = ???")
  syntactic("var a: m.A#B#C = ???")
  syntactic("(f: (A => B) => (C, D) => D)")
  syntactic("(f: ((A, A) => B) => (C, D) => D)")
  syntactic("var a: o.type = ???")
  syntactic("var a: o.x.type = ???")
  syntactic("val a: A | B = ???")
  syntactic("val a: A | B | C = ???")
  syntactic("val a: A & B & C = ???")
  syntactic("val a: M[A + (B, C)] = ???")
  syntactic("val a: M[(A)] = ???")
  syntactic("val a: Option[(Int, String)] = ???")
  // syntactic("trait A[type K, type V]")
  // syntactic("new A[K = String, V = Int]")

  // imports
  syntactic("import a._")
  syntactic("import a.b")
  syntactic("import a.b.c")
  syntactic("import a.b.{c, d}")
  syntactic("import a.b.{c => _, d}")
  syntactic("import a.b.{c => x, d => _, f}")
  syntactic("import a.b, a.c")
  syntactic("import a.b.{c, d}, a.{c => x, d => _, f}")

  // annotation
  syntactic("@static val a: m.A = ???", "@static() val a: m.A = ???")
  syntactic("@static() val a: m.A = ???")
  syntactic("@static() @volatile() val a: m.A = ???")
  syntactic("@foo val Foo(a) = 2", "@foo() val Foo(a) = 2")
  syntactic("@foo val Foo(a, Bar(b)) = 2", "@foo() val Foo(a, Bar(b)) = 2")
  syntactic("@main class App { println(100) }", "@main() class App { println(100) }")
  syntactic("@main() class App { println(100) }")
  syntactic("@optimize(5) def fact(n: Int) = ???")
  syntactic("@optimize(5) @log(3) def fact(n: Int) = ???")
  syntactic("(x: @unchecked)", "(x: @unchecked())")
  syntactic("(x: @unchecked())")
  syntactic("(x: @unchecked @optimize)", "(x: @unchecked() @optimize())")
  syntactic("(x: @unchecked() @optimize(3))")
  syntactic("(x: @unchecked()): @optimize(3)")
  syntactic("trait Foo[-T] extends Comparator[T @uncheckedVariance]",
    "trait Foo[-T] extends Comparator[T @uncheckedVariance()]")
  syntactic("trait Foo[-T] extends Comparator[T @uncheckedVariance()]")
  syntactic("trait Foo[-T] extends Comparator[T @uncheckedVariance() @annot(4)]")
  syntactic("trait Function0[@specialized(Unit, Int, Double) T]")
  syntactic("x match { case m: Type[Any] @unchecked => m }",
    "x match { case m: Type[Any] @unchecked() => m }")
  syntactic("x match { case m: Type[Any] @unchecked() @foo() => m }")

  // modifiers
  syntactic("private val a: Int = 3")
  syntactic("private[core] val a: Int = 3")
  syntactic("private[this] val a: Int = 3")
  syntactic("protected val a: Int = 3")
  syntactic("protected[core] val a: Int = 3")
  syntactic("class A[T <: C[T]](a: Int) extends B(a) with C[T] { def test(x: Int): Boolean; protected val x: Int = 4 }")
  syntactic("class A[T <: C[T]](val a: Int) extends B(a) with C[T] { def test(x: Int): Boolean; protected val x: Int = 4 }")
  syntactic("class A[T <: C[T]](var a: Int) extends B(a) with C[T] { private[core] def test(x: Int): Boolean; protected[core] val x: Int }")
  syntactic("class A[T <: C[T]](private var a: Int) extends B(a) with C[T] { def test(x: Int): Boolean; private[this] val x: Int }")

  // other features
  // - TypeLambdaTree


  // scala tour code snipeets
  syntactic("""
    abstract class Buffer {
      type T
      val element: T
    }
  """)

  syntactic("""
    class Graph {
      class Node {
        var connectedNodes: List[Node] = Nil
        def connectTo(node: Node) =
          if (connectedNodes.find(node.equals).isEmpty)
            connectedNodes = node :: connectedNodes
      }
      var nodes: List[Node] = Nil
      def newNode: Node = {
        val res = new Node
        nodes = res :: nodes
        res
      }
    }
  """)
}
