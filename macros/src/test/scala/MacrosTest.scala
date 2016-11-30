class MacrosTest extends TestSuite {
  test("helloObject") {
    @hello object Foo
    assert(Hello.hello == "hello, world!")
  }

  test("helloDef") {
    @helloDef def hello = "hello"
    assert(hello == "hello, world!")
  }

  test("helloVal") {
    @helloVal val hello = "hello"
    assert(hello == 1024 + 256)
  }

  test("helloNested") {
    @helloNested val hello = "hello"
    assert(hello == 1024)
  }

  test("helloTrees") {
    @helloTrees val hello = "hello"
    assert(hello == 1024)
  }

  test("helloQuasiInSeq") {
    @helloQuasiInSeq val hello = "hello"
    assert(hello == 1024)
  }

  test("helloRank2") {
    @helloRank2 val hello = "hello"
    assert(hello == 1024)
  }

  test("main") {
    @main object Test {
      "hello world!"
    }

    assert(Test.stub(null) == "hello world!")
  }
}

