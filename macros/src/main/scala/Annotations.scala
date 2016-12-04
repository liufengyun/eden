import scala.`meta`._

class hello extends scala.annotation.MacrosAnnotation {
  def apply(defn: Any): Any = meta {
    q"""
        object Hello {
          def hello = "hello, world!"
        }

     """
  }
}

class helloDef extends scala.annotation.MacrosAnnotation {
  def apply(defn: Any): Any = meta {
    val message = q""""hello, world!""""
    q"""def hello = $message"""
  }
}

class helloVal extends scala.annotation.MacrosAnnotation {
  def apply(defn: Any): Any = meta {
    val i1024 = q"1024"
    val i256 = q"256"
    q"val hello = $i1024 + $i256"
  }
}

class helloNested extends scala.annotation.MacrosAnnotation {
  def apply(defn: Any): Any = meta {
    q"""val hello = ${q"1024"}"""
  }
}

class helloTrees extends scala.annotation.MacrosAnnotation {
  def apply(defn: Any): Any = meta {
    val tp = Some(t"Int") // TODO: an implicit conversion T => Option[T] in meta?
    q"""val hello: $tp = { println(1024); 1024 }"""
  }
}

class helloQuasiInSeq extends scala.annotation.MacrosAnnotation {
  def apply(defn: Any): Any = meta {
    val i1024 = q"1024"
    val tp = Some(t"Int") // TODO: an implicit conversion T => Option[T] in meta?
    q"""val hello: $tp = { println(1024); $i1024 }"""
  }
}

class helloRank2 extends scala.annotation.MacrosAnnotation {
  def apply(defn: Any): Any = meta {
    val stat1 = q"println(1)"
    val stat2 = q"1024"
    val stats = List(stat1, stat2)

    q"""val hello = {print(24); ..$stats ; 1024}"""
  }
}


class main extends scala.annotation.MacrosAnnotation {
  def apply(defn: Any): Any = meta {
    val q"object $name { ..$stats }" = defn
    val main = q"""
      def stub(args: Any): Any = { ..$stats }
    """
    q"object $name { $main }"
  }
}

