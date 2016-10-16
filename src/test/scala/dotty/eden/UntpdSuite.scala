package dotty.eden

import scala.{meta => m}

class UntpdSuite extends EdenSuite {
  // literals
  checkUntpd("null")
  checkUntpd("true")
  checkUntpd("false")
  checkUntpd("56")
  checkUntpd("-56")
  /*
  checkUntpd("-56L")
  checkUntpd("-56l")
  checkUntpd("0x56")
  checkUntpd("-0x56")
  checkUntpd("-0x56L")
  checkUntpd("-0x56l")
  checkUntpd("56.45")
  checkUntpd("-56.45")
  checkUntpd("-56.45e10")
  checkUntpd("-56.45e-10")
  checkUntpd("-56.45e-10F")
  checkUntpd("-56.45e-10f")
  checkUntpd("-56.45e-10D")
  checkUntpd("-56.45e-10d")
  checkUntpd(".45")
  checkUntpd(".45e10")
  checkUntpd(".45e-10")
  checkUntpd(".45e-10F")
  checkUntpd(".45e-10f")
  checkUntpd(".45e-10D")
  checkUntpd(".45e-10d")
  checkUntpd("-56.45E10")
  checkUntpd("-56.45E-10")
  checkUntpd("-56.45E-10F")
  checkUntpd("-56.45E-10f")
  checkUntpd("-56.45E-10D")
  checkUntpd("-56.45E-10d")
  checkUntpd("4E10")
  checkUntpd("4E-10")
  checkUntpd("4E-10F")
  checkUntpd("4E-10f")
  checkUntpd("4E-10D")
  checkUntpd("4E-10d")
  checkUntpd("'a'")
  checkUntpd("'\u0041'")
  checkUntpd("'\n'")
  checkUntpd("'\t'")
  checkUntpd("Hello,\nWorld!")
  checkUntpd("This string contains a \" character.")
  checkUntpd("\"\"\"the present string\n" +
    "spans three\n" +
    "lines.\"\"\""
  ) */

  // exprs
  checkUntpd("""println("hello, world")""")
  checkUntpd("println(42)")
  checkUntpd("f(this)")
  checkUntpd("f(A.this)")
  checkUntpd("this.age")
  checkUntpd("C.this.age")
  checkUntpd("super.age")
  checkUntpd("super[A].age")
  checkUntpd("C.super[A].age")
  checkUntpd("f[Int](3)")
  checkUntpd("f(x = 3)")
  checkUntpd("f(x = 3, y = 6 * 8)")
  checkUntpd("f(x:_*)")
  checkUntpd("f(m(x):_*)")
  checkUntpd("a.f(this.age)")
  checkUntpd("a + b")
  checkUntpd("a + (b, c)")
  checkUntpd("a + (b, c, d)")
  checkUntpd("a + b + c + this.age")
  checkUntpd("a :+ b")
  checkUntpd("a +: b")
  checkUntpd("a +: (b, c)")
  checkUntpd("a +: (b, c, d)")
  checkUntpd("a :+ (b, c)")
  checkUntpd("a :+ (b, c, d)")
  checkUntpd("(b, c) :+ a")
  checkUntpd("(b, c, d) :+ a")
  checkUntpd("(b, c) +: a")
  checkUntpd("(b, c, d) +: a")
  checkUntpd("a*")
  checkUntpd("++a")
  checkUntpd("!a")
  checkUntpd("~a")
  checkUntpd("a++")
  checkUntpd("a = b")
  checkUntpd("{ a = 1; b += 2 }")
  checkUntpd("{ }")
  checkUntpd("()")
  checkUntpd("(2)")
  checkUntpd("(2, 4)")
  checkUntpd("a -> b")
  checkUntpd("if (cond) a else b")
  checkUntpd("if (cond) return a")
  checkUntpd("while (a > 5) { println(a); a++; }")
  checkUntpd("do { println(a); a++; } while (a > 5)")
  checkUntpd("return a")
  checkUntpd("new List(5)")
  checkUntpd("new List[Int](5)")
  checkUntpd("new List[List[Int]](List(5))")
  checkUntpd("new Map[Int, String]")
  checkUntpd("new Map[Int, String]()",  // impossible to detect presence of ()
    m.Term.New(
      m.Template(
        Nil,
        List(
          m.Term.ApplyType(
              m.Ctor.Ref.Name("Map"),
              List(m.Type.Name("Int"), m.Type.Name("String"))
          )
        ),
        m.Term.Param(Nil, m.Name.Anonymous(), None, None),
        None
      )
    )
  )
  checkUntpd("new Map[Int, String](a -> b)")
  checkUntpd("new B")
  checkUntpd("new B()",  // impossible to detect presence of ()
    m.Term.New(
      m.Template(
        Nil,
        List(m.Ctor.Ref.Name("B")),
        m.Term.Param(Nil, m.Name.Anonymous(), None, None),
        None
      )
    )
  )
  checkUntpd("new c.B")
  checkUntpd("new C#B")
  checkUntpd("new o.C#B")
  checkUntpd("new B { }")
  checkUntpd("new B { val a = 3 }")
  checkUntpd("new B { def f(x: Int): Int = x*x }")
  checkUntpd("new B(3) { println(5); def f(x: Int): Int = x*x }")
  checkUntpd("throw new A(4)")
  checkUntpd("try { throw new A(4) } catch { case _: Throwable => 4 } finally { println(6) }")
  checkUntpd("try f(4) catch { case _: Throwable => 4 } finally println(6)")
  checkUntpd("try f(4) catch { case _: Throwable => 4 }")
  checkUntpd("try f(4) catch rescue")
  checkUntpd("try f(4) finally println(6)")
  checkUntpd("try {} finally println(6)")
  checkUntpd("for (arg <- args) result += arg * arg")
  checkUntpd("for (arg <- args; double = arg * 2) result += arg * arg")
  checkUntpd("""
    for { i<-1 until n
          j <- 1 until i
          if isPrime(i+j) } yield (i, j)
  """)
  checkUntpd("""
    for { i<-1 until n
          j <- 1 until i
          k = i + j
          if isPrime(i+j) } yield (i, j)
  """)

  // interpolation
  checkUntpd("""s"hello, $world"""")
  checkUntpd("""s"hello, $world, ${1 + 2}"""")

  // patterns
  checkUntpd("a match { case 5 => ; case 6 => }")
  checkUntpd("a match { case Some(x) => x; case None => y }")
  checkUntpd("a match { case Some(x) => x; case _ => y }")
  checkUntpd("a match { case m @ Some(x) => x; case _ => y }")
  checkUntpd("a match { case m @ Some(t @ Some(x)) => x; case _ => y }")
  checkUntpd("a match { case m : Int => x; case _ => y }")
  checkUntpd("a match { case Some(x: Int) | Some(x: String) => x; case _ => y }")
  checkUntpd("a match { case Some(x: Int) | Some(x: String) | Some(x: Boolean) => x; case _ => y }")
  checkUntpd("a match { case Some(x: Int) | Some(x: String) | x: Boolean => x; case _ => y }")

  // definitions
  checkUntpd("val a = 3")
  checkUntpd("val a: Int = 3")
  checkUntpd("val a: List[List[Int]] = List(List(3))")
  checkUntpd("val a: Int")
  checkUntpd("val a, b: Int")
  checkUntpd("val a, b = 3")
  checkUntpd("val Some(Some(x)) = a")

  checkUntpd("var a = 3")
  checkUntpd("var a: List[Int] = List(3)")
  checkUntpd("var a: Int")
  checkUntpd("var a, b: Int")
  checkUntpd("var a, b = 3")
  checkUntpd("var a: Int = _")
  checkUntpd("var Some(Some(x)) = a")

  checkUntpd("def f(x: Int): Int = x*x")
  checkUntpd("def f(x: Int = 5): Int = x*x")
  checkUntpd("def f(x: Int = 5)(y: Int): Int = x*y")
  checkUntpd("def f(x: Int*): Int = ???")
  checkUntpd("def f(x: Option[Int]*): Int = ???")
  checkUntpd("def f[T](x: T): Int = x")
  checkUntpd("def f[T >: Nothing <: Any ](x: T): Int = x")
  checkUntpd("def f[T :A](x: T): Int = x")
  checkUntpd("def f[T :A :B](x: T): Int = x")
  checkUntpd("def f[T :A[Int]](x: T): Int = x")
  checkUntpd("def f[T, M[_]](x: M[T]): M[T] = x")
  checkUntpd("def f[T, M[A]](x: M[T]): M[T] = x")
  checkUntpd("def f[T, M[A]](x: => M[T]): M[T] = x")
  checkUntpd("""
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

  checkUntpd("type Age = Int")
  checkUntpd("type Age")
  checkUntpd("type Age >: Int <: Any")
  checkUntpd("type Age <: Int + Boolean")
  checkUntpd("type Container[T]")
  checkUntpd("type Container[T] = List[T]")
  checkUntpd("type Container[T] <: List[T] { def isEmpty: Boolean; type M }")
  checkUntpd("type Container[T] <: List[T] with Set[T] { def isEmpty: Boolean; type M }")
  checkUntpd("type Container[T] <: List[T] with Set[T] { def isEmpty: Boolean; type M = Int }")
  checkUntpd("type Container[T] <: List[T] with Set[T] { def isEmpty: Boolean; type M <: Int }")

  checkUntpd("trait A { def test(x: Int): Boolean; val x: Int }")
  checkUntpd("trait A { self: B => def test(x: Int): Boolean; val x: Int }")
  checkUntpd("trait A { self: B => def test(x: Int): Boolean; val x: Int; type Age >: Int <: Any }")
  checkUntpd("trait A { self: B => def test(x: Int): Boolean; val x: Int; type Age = Int }")
  checkUntpd("trait A { self: B => def test(x: Int): Boolean; val x: Int; type Container[T] = List[T] }")
  checkUntpd("trait A[T] extends B with C { def test(x: Int): Boolean; val x: Int }")
  checkUntpd("class A[T] extends B with C { def test(x: Int): Boolean; val x: Int }")
  checkUntpd("class A[+T] extends B with C { def test(x: Int): Boolean; val x: Int }")
  checkUntpd("class A[-T] extends B with C { def test(x: Int): Boolean; val x: Int }")
  checkUntpd("class A[T <: Any](a: Int) extends B(a) with C[T] { def test(x: Int): Boolean; val x: Int }")
  checkUntpd("class A[T <: C[T]](a: Int) extends B(a) with C[T] { def test(x: Int): Boolean; val x: Int }")
  checkUntpd("class A[T <: C[T]](a: Int) extends B(a) with C[T] { def test(x: Int): Boolean; var x: Int = _ }")
  checkUntpd("object A extends B with C { def test(x: Int): Boolean; val x: Int }")

  checkUntpd("""
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

  // functions
  checkUntpd("map(_ + 1)")
  checkUntpd("f(_ * _)")
  checkUntpd("map((_: Int) * 2)")
  checkUntpd("map(if (_) x else y)")
  checkUntpd("g(_.map(f))")
  checkUntpd("l.map(_ + 1)")
  checkUntpd("f(x => x + 1)")
  checkUntpd("f((x1, x2) => x1 * x2)")
  checkUntpd("f((x: Int) => (x: Int) * 2)")
  checkUntpd("f(z => if (z) x else y)")
  checkUntpd("f(x => x.map(f))")
  checkUntpd("f(x => x.map(y => y + 1))")
  checkUntpd("val f: Int => String = _.toString")
  checkUntpd("f _")

  // types
  // checkUntpd("var a: A with B = ???")
  checkUntpd("var a: m.A = ???")
  checkUntpd("var a: m.d.e.A = ???")
  checkUntpd("var a: m.List[m.t.A] = ???")
  checkUntpd("var a: A#B = ???")
  checkUntpd("var a: m.A#B = ???")
  checkUntpd("var a: m.A#B#C = ???")
  checkUntpd("(f: (A => B) => (C, D) => D)")
  checkUntpd("(f: ((A, A) => B) => (C, D) => D)")
  checkUntpd("var a: o.type = ???")
  checkUntpd("var a: o.x.type = ???")
  checkUntpd("val a: A | B = ???")
  checkUntpd("val a: A | B | C = ???")
  checkUntpd("val a: A & B & C = ???")
  checkUntpd("val a: M[A + (B, C)] = ???")
  checkUntpd("val a: M[(A)] = ???")
  checkUntpd("val a: Option[(Int, String)] = ???")
  // checkUntpd("trait A[type K, type V]")
  // checkUntpd("new A[K = String, V = Int]")

  // imports
  checkUntpd("import a._")
  checkUntpd("import a.b")
  checkUntpd("import a.b.c")
  checkUntpd("import a.b.{c, d}")
  checkUntpd("import a.b.{c => _, d}")
  checkUntpd("import a.b.{c => x, d => _, f}")
  checkUntpd("import a.b, a.c")
  checkUntpd("import a.b.{c, d}, a.{c => x, d => _, f}")

  // annotation
  // checkUntpd("@static val a: m.A = ???")
  checkUntpd("@static() val a: m.A = ???")
  checkUntpd("@static() @volatile() val a: m.A = ???")
  // checkUntpd("@main class App { println(100) }")
  checkUntpd("@main() class App { println(100) }")
  checkUntpd("@optimize(5) def fact(n: Int) = ???")
  checkUntpd("@optimize(5) @log(3) def fact(n: Int) = ???")
  // checkUntpd("(x: @unchecked)")
  checkUntpd("(x: @unchecked())")
  //checkUntpd("(x: @unchecked @optimize)")
  checkUntpd("(x: @unchecked() @optimize(3))")
  // checkUntpd("trait Foo[-T] extends Comparator[T @uncheckedVariance]")
  checkUntpd("trait Foo[-T] extends Comparator[T @uncheckedVariance()]")
  checkUntpd("trait Foo[-T] extends Comparator[T @uncheckedVariance() @annot(4)]")
  checkUntpd("trait Function0[@specialized(Unit, Int, Double) T]")

  // modifiers
  // checkUntpd("private val a: Int = 3")
  // checkUntpd("private[core] val a: Int = 3")
  // checkUntpd("private[this] val a: Int = 3")
  // checkUntpd("protected val a: Int = 3")
  // checkUntpd("protected[core] val a: Int = 3")
  // checkUntpd("class A[T <: C[T]](a: Int) extends B(a) with C[T] { def test(x: Int): Boolean; protected val x: Int = 4 }")
  // checkUntpd("class A[T <: C[T]](val a: Int) extends B(a) with C[T] { def test(x: Int): Boolean; protected val x: Int = 4 }")
  // checkUntpd("class A[T <: C[T]](var a: Int) extends B(a) with C[T] { private[core] def test(x: Int): Boolean; protected[core] val x: Int }")
  // checkUntpd("class A[T <: C[T]](private var a: Int) extends B(a) with C[T] { def test(x: Int): Boolean; private[this] val x: Int }")

  // other features
  // - TypeLambdaTree

  // scala tour code snipeets
  // checkUntpd("""
  //   abstract class Buffer {
  //     type T
  //     val element: T
  //   }
  // """)
}
