package com.fonmoney.fancycase
import org.scalatest._

/**
  * Created by kralikba on 2016.10.31..
  */
@fancy trait B {
  val j : Int
  val k : Int
}

@fancy case class _B() extends B

@fancy trait A {
  val i : Int
  val s : String
}

@fancy trait Q extends A {
  val l : Long
}

@fancy case class _Q() extends Q

@fancy case class C(f : Boolean) extends Q with A with B

class Test extends FlatSpec with Matchers {
  val (f0, i0, s0, l0, j0, k0) = (true, 4, "5", 6L, 7, 8)
  lazy val c = C(f0, i0, s0, l0, j0, k0)

  "@fancy" should "add the fields of the implemented @fancy traits to the constructor in linearization order" in {
    c.f shouldBe f0
    c.i shouldBe i0
    c.s shouldBe s0
    c.l shouldBe l0
    c.j shouldBe j0
    c.k shouldBe k0
  }

  it should "generate correct unapply methods" in {
    val B((j, k), _) = c
    j shouldBe j0
    k shouldBe k0

    val Q(l, _) = c
    l shouldBe l0

    val A((i, s), _) = c
    i shouldBe i0
    s shouldBe s0
  }

  it should "generate correct Remainders" in {
    val B(_, rb) = c
    rb * (11, 12) shouldBe C(f0, i0, s0, l0, 11, 12)

    val Q(_, rl) = c
    rl * (13L) shouldBe C(f0, i0, s0, 13L, j0, k0)

    val A(_, ra) = c
    ra * (14, "15") shouldBe C(f0, 14, "15", l0, j0, k0)
  }

  it should "generate correct fromComponents methods" in {
    val q = _Q.fromComponents(c, c)
    q.i shouldBe i0
    q.s shouldBe s0
    q.l shouldBe l0
    val b = _B.fromComponents(c)
    b.j shouldBe j0
    b.k shouldBe k0

    val c1 = C.fromComponents(c.f, q, q, b)
    c1 shouldBe c
  }
}