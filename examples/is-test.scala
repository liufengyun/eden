@main object Test {
  println("Hello is String: " + scope.is[String]("hello"))
  println("Hello is String & Int: " + scope.both[String, List[Int]]("hello"))
}