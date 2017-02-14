@main
object Test {
  @xsd("examples/schema.xsd")
  object schema

  import schema._
  val note = new Note("Vassily", "Pupkin", "hello", "This is a test!")

  println(note)
}