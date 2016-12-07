class MacrosTest extends TestSuite {
  test("main") {
    @main object Test {
      "hello world!"
    }

    assert(Test.stub(null) == "hello world!")
  }

  test("data") {
    @data class Point(x: Int, y: Int)

    val p = Point(40, 2)
    assert(p.x == 40 && p.y == 2)
  }
}
