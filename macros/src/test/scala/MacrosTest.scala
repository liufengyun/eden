import org.junit.Test
import org.junit.Assert._

class MacrosTest {
  @Test def helloObject = {
    @hello object Foo
    assertTrue(Hello.hello == 1024)
  }

  @Test def helloDef = {
    @helloDef def hello = "hello"
    assertTrue(hello == 1024)
  }

  @Test def helloVal = {
    @helloVal val hello = "hello"
    assertTrue(hello == 1024)
  }
}

