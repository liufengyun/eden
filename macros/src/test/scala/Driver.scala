object Driver {
  val tests = new collection.mutable.HashMap[String, () => Unit]()


  def main(args: Array[String]): Unit = {
    // setup tests
    new MacrosTest
    new QuasiquoteSuite

    var success = false
    var failed = 0
    tests.foreach { case (name, code) =>
      try { code(); success = true }
      catch {
        case ex: Throwable =>
          success = false
          failed += 1
          print(Console.WHITE)
          ex.printStackTrace()
      }

      if (success)
        println(s"${Console.GREEN}- $name")
      else
        println(s"${Console.RED}- $name")
    }

    println(s"${Console.WHITE}Total: ${tests.size}, Success: ${tests.size - failed}, Failed: $failed")
  }
}

class TestSuite {
  def test(name: String)(code: => Unit): Unit = {
    assert(!Driver.tests.contains(name))
    Driver.tests.update(name, () => code)
  }
}
