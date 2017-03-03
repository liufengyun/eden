# Eden [![Build Status](https://travis-ci.org/liufengyun/eden.svg?branch=master)](https://travis-ci.org/liufengyun/eden)

The dotty version of [paradise](https://github.com/scalameta/paradise) to interface with [scala.meta](https://github.com/scalameta/scalameta).

## Usage

NOTE: Before the PR is merged into Dotty, the following instructions only make sense to our clone of the Dotty compiler. Check the build setting for the `macros` project for more details.

### Command Line Usage

Download the latest assembly [here](https://oss.sonatype.org/content/repositories/releases/me/fengy/eden_2.11/0.1.2/eden_2.11-0.1.2-assembly.jar), then compile with the assembly in class path:

    ./bin/dotc -classpath path/to/eden-assembly.jar macros.scala

### Sbt Usage

Add Eden as a dependency:

    libraryDependencies += "me.fengy" % "eden_2.11" % "0.1.2"

## Macros

More examples can be found below:

- [macro definitions](macros/src/main/scala/Annotations.scala)
- [macro usage](macros/src/test/scala/MacrosTest.scala)

Note: macro definitions have to be compiled before usage.

### Annotation Macros

Definition:

```Scala
import scala.annotation.StaticAnnotation
import scala.meta._
import scala.meta.dialects.Dotty

import scala.collection.immutable.Seq

class main extends StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    val q"object $name { ..$stats }" = defn
    val main = q"""
      def stub(args: Any): Any = { ..$stats }
    """
    q"object $name { $main }"
  }
}
```

Usage:

```Scala
@main object Test {
  "hello world!"
}
```

### Def Macros

Definition:

```Scala
class plus {
  inline def apply(a: Any, b: Any): Any = meta {
    q"$a + $b"
  }
}
```

Usage:

```Scala
val p = new plus
assert(p(3, 5) == 8)
```
