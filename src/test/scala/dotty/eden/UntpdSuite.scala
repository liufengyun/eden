package dotty.eden

class UntpdSuite extends EdenSuite {
  checkUntpd("""println("hello, world")""")
  checkUntpd("println(42)")
  checkUntpd("f(this)")
  checkUntpd("f(A.this)")
  checkUntpd("this.age")
  checkUntpd("f[Int](3)")
  checkUntpd("a.f(this.age)")
  checkUntpd("a + b")
}
