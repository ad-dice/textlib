package com.ad_dice.textlib

sealed trait Align

sealed trait Horizontal extends Align

object Horizontal {
  case object Left extends Horizontal
  case object CenterLeft extends Horizontal
  case object CenterRight extends Horizontal
  case object Right extends Horizontal
}

sealed trait Vertical extends Align

object Vertical {
  case object Top extends Vertical
  case object CenterTop extends Vertical
  case object CenterBottom extends Vertical
  case object Bottom extends Vertical
}
