import org.junit.Test
import org.junit.Assert._

object MacrosTest {
  def main(args: Array[String]): Unit = {
    helloObject
    helloDef
    helloVal
    helloTrees
    helloNested
    helloQuasiInSeq
    helloRank2
    println("========= All tests pass ==========")
  }

  @Test def helloObject = {
    @hello object Foo
    assert(Hello.hello == "hello, world!")
  }

  @Test def helloDef = {
    @helloDef def hello = "hello"
    assert(hello == "hello, world!")
  }

  @Test def helloVal = {
    @helloVal val hello = "hello"
    assert(hello == 1024 + 256)
  }

  @Test def helloNested = {
    @helloNested val hello = "hello"
    assert(hello == 1024)
  }

  @Test def helloTrees = {
    @helloTrees val hello = "hello"
    assert(hello == 1024)
  }

  @Test def helloQuasiInSeq = {
    @helloQuasiInSeq val hello = "hello"
    assert(hello == 1024)
  }

  @Test def helloRank2 = {
    @helloRank2 val hello = "hello"
    assert(hello == 1024)
  }
}

