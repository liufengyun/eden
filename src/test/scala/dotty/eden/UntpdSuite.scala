package dotty.eden

class UntpdSuite extends EdenSuite {
  // terms
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
  checkUntpd("a.f(this.age)")
  checkUntpd("a + b")
  checkUntpd("a + b + c + this.age")
  checkUntpd("a :+ b")
  checkUntpd("a +: b")
  checkUntpd("a*")
  checkUntpd("++a")
  checkUntpd("a++")
  checkUntpd("a = b")
  checkUntpd("{ a = 1; b += 2 }")
  checkUntpd("{ }")
  checkUntpd("()")
  checkUntpd("(2)")
  checkUntpd("(2, 4)")
  checkUntpd("a -> b")
  checkUntpd("if (cond) a else b")
  checkUntpd("while (a > 5) { println(a); a++; }")
  checkUntpd("do { println(a); a++; } while (a > 5)")
  checkUntpd("return a")
  checkUntpd("new List(5)")
  checkUntpd("new List[Int](5)")
  checkUntpd("new List[List[Int]](List(5))")
  checkUntpd("new Map[Int, String]()")
  checkUntpd("new B { }")
  checkUntpd("new B { val a = 3 }")
  checkUntpd("new B { def f(x: Int): Int = x*x }")
  checkUntpd("new B(3) { println(5); def f(x: Int): Int = x*x }")
  checkUntpd("throw new A(4)")
  checkUntpd("try { throw new A(4) } catch { case _: Throwable => 4 } finally { println(6) }")
  checkUntpd("try f(4) catch { case _: Throwable => 4 } finally println(6)")
  checkUntpd("try f(4) catch { case _: Throwable => 4 }")
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
  checkUntpd("var Some(Some(x)) = a")
  checkUntpd("def f(x: Int): Int = x*x")
  checkUntpd("def f(x: Int = 5): Int = x*x")
  checkUntpd("def f(x: Int = 5)(y: Int): Int = x*y")
  checkUntpd("def f[T](x: T): Int = x")
  checkUntpd("def f[T >: Nothing <: Any ](x: T): Int = x")
  checkUntpd("def f[T :A](x: T): Int = x")
  checkUntpd("def f[T :A :B](x: T): Int = x")
  checkUntpd("def f[T :A[Int]](x: T): Int = x")
  checkUntpd("def f[T, M[_]](x: M[T]): M[T] = x")
  checkUntpd("def f[T, M[A]](x: M[T]): M[T] = x")
  checkUntpd("def f[T, M[A]](x: => M[T]): M[T] = x")
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
  checkUntpd("object A extends B with C { def test(x: Int): Boolean; val x: Int }")

  // types
  // checkUntpd("var a: A with B = ???")
  checkUntpd("var a: m.A = ???")
  checkUntpd("var a: m.List[m.t.A] = ???")
  checkUntpd("var a: A#B = ???")
  checkUntpd("var a: m.A#B = ???")

  // nested definitions

  // modifiers
  // checkUntpd("class A[T <: C[T]](val a: Int) extends B(a) with C[T] { def test(x: Int): Boolean; val x: Int }")
  // checkUntpd("class A[T <: C[T]](var a: Int) extends B(a) with C[T] { def test(x: Int): Boolean; val x: Int }")
  // checkUntpd("class A[T <: C[T]](private var a: Int) extends B(a) with C[T] { def test(x: Int): Boolean; val x: Int }")

  // other features
//  - TypeLambdaTree
//  - AndTypeTree
//  - OrTypeTree
//  - SelectFromTypeTree
//  - SingletonTypeTree
//  - JavaSeqLiteral
//  - Closure
//  - Pair      // import {a => b}
//  - Import
//  - Annotated

}
