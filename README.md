# case

Imagine that you could harness the power of runtime polymorphism for `case class`es. 

```scala
 @fancy trait A {
   val i : Int
   val s : String
 }
 
 @fancy trait Q extends A {
   val l : Long
 }
 
 @fancy trait B {
   val j : Int
   val k : Int
 }

 @fancy case class C(f : Boolean) extends Q with A with B
```

That's right, you don't have to manually implement `Q`, `A` and `B`!

```scala
val c = C(true, 5, "6", 7L, 8, 9)
c.f && c.i == 5 && c.s == "6" && c.l == 7L && c.j == 8 && c.k == 9
```

That's right, this will be true!

```scala
val A((i,s), rem) = c
```

That's right, that matches! And you can even pass around "`c without A`"!

```scala
val c1 = rem * (11,"12")
c.f && c.i == 11 && c.s == "12" && c.l == 7L && c.j == 8 && c.k == 9
```

That's right, this is true as well!

Cool, isn't it?

A bit more detailed example can be found in [here](doc/example.scala)

Dependencies
------------

This repository contains the `@fancy` macro-paradise macro for Scala 2.11.
To use it, add the following to your `build.sbt`:

```scala
addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

resolvers += "jitpack" at "https://jitpack.io"
libraryDependencies += "com.github.fonmoney" % "case" % "<commit id>"

libraryDependencies <++= scalaVersion(version => Seq(
  "org.scala-lang" % "scala-reflect" % version,
  "com.chuusai" %% "shapeless" % "2.3.2"
))
```

