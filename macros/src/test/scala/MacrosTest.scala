import org.junit.Test
import org.junit.Assert._

object MacrosTest {
  def main(args: Array[String]): Unit = {
    helloObject
    helloDef
    helloVal
    println("========= All tests pass ==========")
  }

  @Test def helloObject = {
    @hello object Foo
    assert(Hello.hello == 1024)
  }

  @Test def helloDef = {
    @helloDef def hello = "hello"
    assert(hello == 1024)
  }

  @Test def helloVal = {
    @helloVal val hello = "hello"
    assert(hello == 1024)
  }
}

