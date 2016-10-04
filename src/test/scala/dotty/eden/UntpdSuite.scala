package dotty.eden

class UntpdSuite extends EdenSuite {
  checkUntpd("""println("hello, world")""")
  checkUntpd("println(42)")
}
