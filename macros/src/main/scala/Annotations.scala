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

