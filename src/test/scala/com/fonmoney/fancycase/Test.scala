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
  abstract val e : Boolean = false
}

@fancy trait Q extends A {
  val l : Long
  abstract val d : Double = 3.14
}

@fancy case class _Q() extends Q

@fancy case class C(f : Boolean, x : Boolean = false) extends Q with A with B

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

  it should "generate correct with* methods" in {
    c.withQ(l0+100, 6.28) equals c.copy(l = l0 + 100, d = 6.28)
    c.withQ(l = l0+100) equals c.copy(l = l0 + 100)
    c.withQ(d = 6.28) equals c.copy(d = 6.28)
    val q : Q = c
    q.withQ(l0+100, 6.28) equals c.copy(l = l0 + 100, d = 6.28)
    q.withQ(l = l0+100) equals c.copy(l = l0 + 100)
    q.withQ(d = 6.28) equals c.copy(d = 6.28)

    val q1 = c.withQ(d = 55)
    q1 equals c.withQ(q1)
  }

  it should "generate correct unapply methods" in {
    val B((j, k), _) = c
    j shouldBe j0
    k shouldBe k0

    val Q((l, _), _) = c
    l shouldBe l0

    val A((i, s, _), _) = c
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

  it should "handle case classes' default parameter values correctly" in {
    c.x shouldBe false
    C.fromComponents(c.f, c, c, c).x shouldBe false
    val c1 = C.fromComponents(c.f, c, c, c, true)
    c1.x shouldBe true
    c1 shouldBe C(f0, i0, s0, l0, j0, k0, true)
  }

  it should "handle traits' abstract vals' default values correctly" in {
    c.e shouldBe false
    c.d shouldBe 3.14
    val c1 = C(f0, i0, s0, l0, j0, k0, false, true, 6.28)
    c1.e shouldBe true
    c1.d shouldBe 6.28
    val c2 = C(f0, i0, s0, l0, j0, k0, d = 6.28)
    c2.e shouldBe false
    c2.d shouldBe 6.28
  }

  it should "generate correct equalsIn* methods" in {
    c.withB(11,12).equalsInQ(c)
  }
}