# case

Imagine that you could harness the power of runtime polymorphism for `case class`es and not even having to write boilerplate. 

```scala
 @fancy trait A {
   val i : Int
   val s : String
 }
 
 @fancy trait Q extends A {
   val l : Long
   abstract val opt : Boolean = true
 }
 
 @fancy trait B {
   val j : Int
   val k : Int
 }

 @fancy case class C(f : Boolean) extends Q with A with B
```

That's right, you don't have to manually override `Q`, `A` and `B`!

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

That's right, this is true as well! There is even a "shortcut" similar to the case classes' `copy` methods:

```scala
val c2 = c1.withA(s = "12")
c.f && c.i == 11 && c.s == "12" && c.l == 7L && c.j == 8 && c.k == 9
```

The fields can even have default values!

```scala
C(true, 5, "6", 7L, 8, 9, true) == c
```

And you even get an `equalsInX` method for every trait that compares only the fields of those traits:

```scala
c1.equalsInB(c) && c1.equalsInQ(c)
``` 

Cool, isn't it?


Complete example of the macro's output
--------------------------------------

See [doc/example.scala](doc/example.scala) and the macro's [ScalaTest suite](src/test/scala/com/fonmoney/fancycase/Test.scala).

Dependencies
------------

This repository contains the `@fancy` macro-paradise macro for Scala 2.11.
To use it, add the following to your `build.sbt`:

```scala
addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

resolvers += "jitpack" at "https://jitpack.io"
libraryDependencies += "com.github.fonmoney" % "case" % "<commit id>"
```

To do
-----

- [ ] annotations on fields which are valid for parameters should appear wherever applicable (e.g. constructor parameter list, apply, ...)
- [ ] proper docs
- [ ] some info output to see a summary of @fancy-generated constructors and defs
- [ ] varargs
- [ ] overriding default values
