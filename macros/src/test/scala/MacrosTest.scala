class MacrosTest extends TestSuite {
  test("main") {
    @main object Test {
      "hello world!"
    }

    assert(Test.stub(null) == "hello world!")
  }
}
