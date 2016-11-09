package com.fonmoney.fancycase

import shapeless.HList

/**
  * Created by kralikba on 2016.11.08..
  */
trait FancyTraitCompanion[X] {
  /** The representation of the abstract vals introduced by `X`.
    * It does not contain any abstract vals introduced by parents of `X`.
    */
  type Repr <: HList
  abstract class Remainder[+Dividend <: X] {
    def *(r : Repr) : Dividend
  }
}