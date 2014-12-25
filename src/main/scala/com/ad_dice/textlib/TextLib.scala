package com.ad_dice.textlib

import java.awt.{Image, Color, Font, Dimension, Graphics2D, RenderingHints}
import java.awt.geom.AffineTransform
import java.awt.image.BufferedImage
import java.awt.font.FontRenderContext
import java.io.File
import javax.imageio.ImageIO

case class TargetAreaInfo(xt: Int, yt: Int, xb: Int, yb: Int, size: Int)

object TextLib extends App {

    val WidthDefault = 2
    val HeightDefault = 2
    def defaultFontSize: Int = 10 //FontDefault
    /**  definitions for vertical writing */
    def rotationTranslationUpChars = Set('（', '「','『','【')
    def rotationTranslationDownChars = Set('）', '」', '』','】')
    def rotationChars = Set('(', ')', '[',  ']',
      '-', '〜', '~', '‐', '‑', '‒', '–', '―', '−', 'ー', 'ｰ', '一')
    def translationChars = Set(',', '.', '，', '。', '.')
    def translationSmallChars = Set('ゃ', 'ょ', 'ゅ', 'っ', 'ぁ', 'ぇ', 'ぃ', 'ぉ', 'ぅ',
      'ャ', 'ョ', 'ュ', 'ッ', 'ァ', 'ェ', 'ィ', 'ォ', 'ゥ')
    def halfwidthAlphabet = Set('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
      'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
      'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
      'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
      '0', '1', '2', '3', '4', '5', '6', '7', '8', '9','%', '$', '¥')

    /** write strings by vertical */
    def writeVertical(g: Graphics2D, text: String, size: Int, x: Int, y: Int): Unit = {
      val font = g.getFont.deriveFont(size.toFloat)
      g.setFont(font)
      writeVertical(g, text, size, x, y, font)
    }

    def writeVertical(g: Graphics2D, text: String, size: Int, x: Int, y: Int, font:Font, i: Int = 0): Unit = {
      if (text.nonEmpty && size != 0 && g.getFontMetrics(font).stringWidth(text.head.toString) > 0) {
        val char = text.head
        val r = if(g.getFontMetrics(font).stringWidth(char.toString) == 0) 1
                else (size / g.getFontMetrics(font).stringWidth(char.toString))

        if (rotationTranslationUpChars(char)) {
          val transform = AffineTransform.getRotateInstance(Math.toRadians(90), 0, 0)
          g.setFont(font.deriveFont(transform))
          val gap_y = -size / r - size/2
          val gap_x = size / 8
          g.drawString(char.toString, x + gap_x, y + (i + 1) * size + gap_y * 3 / 4)
          writeVertical(g, text.tail, size, x, y - size / 4, font, i + 1)
        }
        else if (rotationTranslationDownChars(char)) {
          val transform = AffineTransform.getRotateInstance(Math.toRadians(90), 0, 0)
          g.setFont(font.deriveFont(transform))
          val gap_y = -size / r
          val gap_x = size / 8
          g.drawString(char.toString, x + gap_x, y + (i + 1) * size + gap_y * 3 / 4)
          writeVertical(g, text.tail, size, x, y - size / 4, font, i + 1)
        }

        else if (rotationChars(char)) {
          val transform = AffineTransform.getRotateInstance(Math.toRadians(90), 0, 0)
          g.setFont(font.deriveFont(transform))
          val gap_y = -size / r
          val gap_x = size / 4
          g.drawString(char.toString, x + gap_x, y + (i + 1) * size + gap_y)
          writeVertical(g, text.tail, size, x, y, font, i + 1)
        }


        else if (translationChars(char)) {
          val transform = AffineTransform.getTranslateInstance(size / 4 * 3 , -size / 4 * 3)
          g.setFont(font.deriveFont(transform))
          g.drawString(char.toString, x, y + (i + 1) * size)
          writeVertical(g, text.tail, size, x, y - size / 4 * 3, font, i + 1)
        }

        else if (translationSmallChars(char)) {
          val transform = AffineTransform.getTranslateInstance(size / 5 , 0)
          g.setFont(font.deriveFont(transform))
          g.drawString(char.toString, x, y + (i + 1) * size)
          writeVertical(g, text.tail, size, x, y, font, i + 1)
        }

        else if (halfwidthAlphabet(char)){
          val transform = AffineTransform.getTranslateInstance(size / 4 , 0)
          g.setFont(font.deriveFont(transform))
          g.drawString(char.toString, x, y + (i + 1) * size)
          writeVertical(g, text.tail, size, x, y, font, i + 1)
        }

        else {
          g.setFont(font)
          if(r == 2){
            g.drawString(char.toString, x + size/4, y + (i + 1) * size)
          }
          else {
            g.drawString(char.toString, x, y + (i + 1) * size)
          }
          writeVertical(g, text.tail, size, x, y, font, i + 1)
        }
      }
    }

    /** write strings by horizontal */
    def writeHorizontal(g: Graphics2D, text: String, size: Int, x: Int, y: Int) {
      g.setFont(g.getFont.deriveFont(size.toFloat))
      g.drawString(text, x, y + size)
    }

   /** write strings by vertical type and convert to image file
   *  for Vertical write
   *  @param alignHorizontal  value range --> ("left", "center-left", "center-right", "right") center-* determines 2nd line starts from which side
   *  @param alignVertical    value range --> ("top", "center", "bottom") "center-*" is dealt as "center"
   */

  def immutableVerticalWrite(g: Graphics2D, source: String, targetAreaInfo: TargetAreaInfo, alignVertical: Vertical, alignHorizontal: Horizontal){

    /** Exception: If font size is not right number */
    val font_size = if(targetAreaInfo.size == 0){
      Console.out.println( Console.RED + "Warning: FONT size is NULL. We set the FONT size to " + defaultFontSize +" by default" + Console.RESET )
      defaultFontSize
    }else{targetAreaInfo.size}


    /** Exception: If x_top == x_bottom */
    val x_bottom = if(targetAreaInfo.xt == targetAreaInfo.xb || targetAreaInfo.xb - targetAreaInfo.xt < 2){
      Console.out.println( Console.RED + "Warning: WIDTH size is NULL. We set the WIDTH size to " + WidthDefault +" by default" + Console.RESET )
      WidthDefault + targetAreaInfo.xt}else{targetAreaInfo.xb}

    /** Exception: If y_top == y_bottom */
    val y_bottom = if(targetAreaInfo.yt == targetAreaInfo.yb || targetAreaInfo.yb - targetAreaInfo.yt < 2){
      Console.out.println( Console.RED + "Warning: HEIGHT size is NULL. We set the HEIGHT size to " + HeightDefault +" by default" + Console.RESET )
      WidthDefault+ targetAreaInfo.yt
    }else{targetAreaInfo.yb}

    // tentatively commented out to avoid circular referrence with immutable* and mutable*
    //** If width or height is shorter than Font size, we call MUTABLE function */
    // if(x_bottom - targetAreaInfo.xt < font_size || y_bottom - targetAreaInfo.yt < font_size){
    //   Console.out.println( Console.RED + "Warning: HEIGHT or WIDTH size is shorter than FONT size. We set FONT to appropriate size " + Console.RESET )
    //   mutableVerticalWrite(g, source,
    //     targetAreaInfo.copy(size = font_size, xb = x_bottom, yb = y_bottom),
    //     alignVertical, alignHorizontal)
    //   return;
    // }


    val width = targetAreaInfo.xb - targetAreaInfo.xt
    val height = targetAreaInfo.yb - targetAreaInfo.yt

    val letter_size = source.length()
    val text_length = font_size * letter_size


    // tentatively commented out to avoid circular referrence with immutable* and mutable*
    //* if source is not fit rectangle, we call mutable function */
    // if(letter_size > (width / font_size).toInt * (height / font_size).toInt){
    //   Console.out.println( Console.RED + "Warning: Your text is not fit into rectangle. We set FONT to appropriate size. " + Console.RESET )
    //   Console.out.println( Console.RED + "letter_size > (width / font_size).toInt * (height / font_size).toInt" + Console.RESET )

    //   mutableVerticalWrite(g, source,
    //     targetAreaInfo.copy(size = font_size, xb = x_bottom, yb = y_bottom),
    //     alignVertical, alignHorizontal)
    //   return;
    // }

    // line_size_height
    val one_line_size_height = height / font_size

    val line_size_height = if(letter_size % one_line_size_height == 0)
      letter_size / one_line_size_height
      else
      letter_size / one_line_size_height + 1

    // mid_width
    val mid_width = if(line_size_height % 2 == 0)
      (targetAreaInfo.xt + targetAreaInfo.xb) / 2
      else
      (targetAreaInfo.xt + targetAreaInfo.xb - font_size) / 2

    // mid_height
    val mid_height = if(one_line_size_height % 2 == 0)
      (targetAreaInfo.yt + targetAreaInfo.yb) / 2
      else
      (targetAreaInfo.yt + targetAreaInfo.yb - font_size) / 2

    //println(text_length, width, height)

    alignVertical match {
      case Vertical.Top =>
      val line_letter_size = height / font_size
      val text_grouped_list = source.grouped(line_letter_size).toList

      alignHorizontal match {
        case Horizontal.Left =>
        if(text_length <= height){

          writeVertical(g, source, font_size, targetAreaInfo.xt, targetAreaInfo.yt)
        }
        else{
          for(i <- 0 to text_grouped_list.size - 1)
          writeVertical(g, text_grouped_list(i), font_size, targetAreaInfo.xt + font_size * i, targetAreaInfo.yt)
        }
        case Horizontal.Right =>
        if(text_length <= height){
          writeVertical(g, source, font_size, targetAreaInfo.xb - font_size, targetAreaInfo.yt)
        }
        else{
          for(i <- 0 to text_grouped_list.size - 1)
          writeVertical(g, text_grouped_list(i), font_size, targetAreaInfo.xb - font_size * (i + 1), targetAreaInfo.yt)
        }
        case Horizontal.CenterLeft =>
        if(text_length <= height){
          writeVertical(g, source, font_size, mid_width, targetAreaInfo.yt)
        }
        else{
          val gap = text_grouped_list.size / 2 * font_size
          for(i <- 0 to text_grouped_list.size - 1)
          writeVertical(g, text_grouped_list(i), font_size,
            mid_width - gap + font_size * i, targetAreaInfo.yt)
        }
        case Horizontal.CenterRight =>
        if(text_length <= height){
          writeVertical(g, source, font_size, mid_width, targetAreaInfo.yt)
        }
        else{
          val gap = text_grouped_list.size / 2 * font_size
          for(i <- 0 to text_grouped_list.size - 1)
            if(line_size_height % 2 == 0)
              writeVertical(g, text_grouped_list(i), font_size,
                mid_width + gap - font_size * (i+1), targetAreaInfo.yt)
            else
              writeVertical(g, text_grouped_list(i), font_size,
                mid_width + gap - font_size * i, targetAreaInfo.yt)
        }
      }
      case Vertical.Bottom =>
      val line_letter_size = height / font_size

      alignHorizontal match {
        case Horizontal.Left =>
        val reversed_text = source.reverse
        val text_grouped_list = reversed_text.grouped(line_letter_size).toList

        if(text_length <= height){
          writeVertical(g, source, font_size, targetAreaInfo.xt, targetAreaInfo.yb - text_length)
        }
        else{
          for(i <- 0 to text_grouped_list.size - 1)
          writeVertical(g, text_grouped_list(i).reverse, font_size, targetAreaInfo.xt + font_size * i,
            targetAreaInfo.yb - text_grouped_list(i).size * font_size)
        }
        case Horizontal.Right =>
        val text_grouped_list = source.grouped(line_letter_size).toList

        if(text_length <= height){
          writeVertical(g, source, font_size, targetAreaInfo.xb - font_size, targetAreaInfo.yb - text_length)
        }
        else{
          for(i <- 0 to text_grouped_list.size - 1)
          writeVertical(g, text_grouped_list(i), font_size, targetAreaInfo.xb - font_size * (i + 1),
            targetAreaInfo.yb - text_grouped_list(i).size * font_size)
        }
        case Horizontal.CenterLeft =>
        val reversed_text = source.reverse
        val text_grouped_list = reversed_text.grouped(line_letter_size).toList
        if(text_length <= height){
          writeVertical(g, source, font_size, mid_width, targetAreaInfo.yb - text_length)
        }
        else{
          val gap = text_grouped_list.size / 2 * font_size

          for(i <- 0 to text_grouped_list.size - 1)
          writeVertical(g, text_grouped_list(i).reverse, font_size,
            mid_width - gap + font_size * i ,
            targetAreaInfo.yb - text_grouped_list(i).size * font_size)
        }
        case Horizontal.CenterRight =>
        val text_grouped_list = source.grouped(line_letter_size).toList
        if(text_length <= height){
          writeVertical(g, source, font_size, mid_width, targetAreaInfo.yb - text_length)
        }
        else{
          val gap = text_grouped_list.size / 2 * font_size

          for(i <- 0 to text_grouped_list.size - 1)
            if(line_size_height % 2 == 0)
              writeVertical(g, text_grouped_list(i), font_size,
                mid_width + gap - font_size * (i + 1),
                targetAreaInfo.yb - text_grouped_list(i).size * font_size)
            else
              writeVertical(g, text_grouped_list(i), font_size,
                mid_width + gap - font_size * (i),
                targetAreaInfo.yb - text_grouped_list(i).size * font_size)
        }
      }
      case Vertical.CenterTop | Vertical.CenterBottom =>
      val line_letter_size = height / font_size
      val text_grouped_list = source.grouped(line_letter_size).toList
      alignHorizontal match {
        case Horizontal.Left =>
        if(text_length <= height){
          val gap = source.size / 2 * font_size
          writeVertical(g, source, font_size, targetAreaInfo.xt,
            mid_height - gap)
        }
        else{
          for(i <- 0 to text_grouped_list.size - 1){
            val gap = text_grouped_list(i).size / 2 * font_size
            writeVertical(g, text_grouped_list(i), font_size, targetAreaInfo.xt + font_size * i,
              mid_height - gap )
          }
        }
        case Horizontal.Right =>
        if(text_length <= height){
          val gap = source.size / 2 * font_size
          writeVertical(g, source, font_size, targetAreaInfo.xb - font_size,
            mid_height - gap)
        }
        else{
          for(i <- 0 to text_grouped_list.size - 1){
            val gap = text_grouped_list(i).size / 2 * font_size
            writeVertical(g, text_grouped_list(i), font_size, targetAreaInfo.xb - font_size * (i + 1),
              mid_height - gap )
          }
        }
        case Horizontal.CenterLeft =>
        if(text_length <= height){
          val gap = source.size / 2 * font_size
          writeVertical(g, source, font_size, mid_width,
            mid_height - gap)
        }
        else{
          val _gap = text_grouped_list.size / 2 * font_size
          for(i <- 0 to text_grouped_list.size - 1){
            val gap = text_grouped_list(i).size / 2 * font_size
            writeVertical(g, text_grouped_list(i), font_size,
              mid_width - _gap + font_size * i,
              mid_height - gap )
          }
        }
        case Horizontal.CenterRight =>
        if(text_length <= height){
          val gap = source.size / 2 * font_size
          writeVertical(g, source, font_size, mid_width,
            mid_height - gap)
        }
        else{
          val _gap = text_grouped_list.size / 2 * font_size
          for(i <- 0 to text_grouped_list.size - 1){
            val gap = text_grouped_list(i).size / 2 * font_size
            if(line_size_height % 2 == 0)
              writeVertical(g, text_grouped_list(i), font_size,
                mid_width + _gap - font_size * (i+1),
                mid_height - gap )
            else
              writeVertical(g, text_grouped_list(i), font_size,
                mid_width + _gap - font_size * (i),
                mid_height - gap )
          }
        }
      }

    }

  }

  /**
    *  for Horizontal write
    *  @param alignHorizontal  value range --> ("left", "center", "right")  center-* is dealt as center
    *  @param alignVertical    value range --> ("top", "center-top", "center-bottom", "bottom") center-* determines 2nd line starts from which side
  */

  def immutableHorizontalWrite(g: Graphics2D, source: String, targetAreaInfo: TargetAreaInfo, alignVertical: Vertical, alignHorizontal: Horizontal){

    /** Exception: If font size is not right number */
    if(targetAreaInfo.size == 0){
      Console.out.println( Console.RED + "Warning: FONT size is 0. We set the FONT size to " + defaultFontSize +" by default" + Console.RESET )
    }

    val font_size = if(targetAreaInfo.size == 0){defaultFontSize}else{targetAreaInfo.size}

    /** Exception: If x_top == x_bottom */
    if(targetAreaInfo.xt == targetAreaInfo.xb || targetAreaInfo.xb - targetAreaInfo.xt < 2){
      Console.out.println( Console.RED + "Warning: WIDTH size is NULL or 1. We set the WIDTH size to " + WidthDefault +" by default" + Console.RESET )
    }

    val x_bottom = if(targetAreaInfo.xt == targetAreaInfo.xb || targetAreaInfo.xb - targetAreaInfo.xt < 2){WidthDefault + targetAreaInfo.xt}else{targetAreaInfo.xb}
    //println(x_bottom)

    /** Exception: If y_top == y_bottom */
    if(targetAreaInfo.yt == targetAreaInfo.yb || targetAreaInfo.yb - targetAreaInfo.yt < 2){
      Console.out.println( Console.RED + "Warning: HEIGHT size is NULL. We set the HEIGHT size to " + HeightDefault +" by default" + Console.RESET )
    }

    val y_bottom = if(targetAreaInfo.yt == targetAreaInfo.yb || targetAreaInfo.yb - targetAreaInfo.yt < 2){WidthDefault + targetAreaInfo.yt}else{targetAreaInfo.yb}

    // tentatively commented out to avoid circular referrence with immutable* and mutable*
    //** If width or height is shorter than Font size, we call MUTABLE function */
    // if(x_bottom - targetAreaInfo.xt < font_size || y_bottom - targetAreaInfo.yt < font_size){
    //   Console.out.println( Console.RED + "Warning: HEIGHT or WIDTH size is shorter than FONT size. We set FONT to appropriate size " + Console.RESET )
    //   mutableHorizontalWrite(g, source,
    //     targetAreaInfo.copy(size = font_size, xb = x_bottom, yb = y_bottom),
    //     alignVertical, alignHorizontal)
    //   return;
    // }

    val width = targetAreaInfo.xb - targetAreaInfo.xt
    val height = targetAreaInfo.yb - targetAreaInfo.yt

    val letter_size = source.length()
    val text_length = font_size * letter_size

    // tentatively commented out to avoid circular referrence with immutable* and mutable*
    //* if source is not fit rectangle, we call mutable function */
    // if(letter_size > (width / font_size).toInt * (height / font_size).toInt){
    //   Console.out.println( Console.RED + "Warning: Your text is not fit into rectangle. We set FONT to appropriate size " + Console.RESET )
    //   Console.out.println( Console.RED + "letter_size > (width / font_size).toInt * (height / font_size).toInt" + Console.RESET )

    //   mutableHorizontalWrite(g, source,
    //     targetAreaInfo.copy(size = font_size, xb = x_bottom, yb = y_bottom),
    //     alignVertical, alignHorizontal)
    //   return;
    // }

    // line_size_width
    val one_line_size_width = width / font_size

    val line_size_width = if(letter_size % one_line_size_width == 0)
      letter_size / one_line_size_width
      else
      letter_size / one_line_size_width + 1

    val mid_height = if(line_size_width % 2 == 0)
      (targetAreaInfo.yt + targetAreaInfo.yb) / 2
      else
      (targetAreaInfo.yt + targetAreaInfo.yb - font_size) / 2

    // line_size_height
    val mid_width = if(one_line_size_width % 2 == 0)
      (targetAreaInfo.xt + targetAreaInfo.xb) / 2
      else
      (targetAreaInfo.xt + targetAreaInfo.xb - font_size) / 2

    //println(text_length, width, height)


    alignHorizontal match {
      case Horizontal.Left =>
        val line_letter_size = width / font_size
        val text_grouped_list = source.grouped(line_letter_size).toList

        /** start write top side */
        alignVertical match {
          case Vertical.Top =>
          if(text_length <= width){

            writeHorizontal(g, source, font_size, targetAreaInfo.xt, targetAreaInfo.yt)
          }
          else{
            for(i <- 0 to text_grouped_list.size - 1)
            writeHorizontal(g, text_grouped_list(i), font_size, targetAreaInfo.xt, targetAreaInfo.yt + font_size * i)
          }
          case Vertical.Bottom =>
          if(text_length <= width){
            writeHorizontal(g, source, font_size, targetAreaInfo.xt, targetAreaInfo.yb - font_size)
          }
          else{
            val gap = text_grouped_list.size * font_size
            for(i <- 0 to text_grouped_list.size - 1)
                  //writeHorizontal(g, text_grouped_list(i), font_size, targetAreaInfo.xt, targetAreaInfo.yb - font_size * (i + 1))
                  writeHorizontal(g, text_grouped_list(i), font_size, targetAreaInfo.xt, targetAreaInfo.yb - gap + font_size * i)
                }
                case Vertical.CenterBottom =>
                val reversed_text = source.reverse
                val text_grouped_list_ = reversed_text.grouped(line_letter_size).toList

                if(text_length <= width){
                  writeHorizontal(g, source, font_size, targetAreaInfo.xt, mid_height)
                }
                else{
                  val gap = text_grouped_list.size / 2 * font_size
                  for(i <- 0 to text_grouped_list_.size - 1)
                    if(line_size_width % 2 == 0)
                      writeHorizontal(g, text_grouped_list_(i).reverse, font_size,
                        targetAreaInfo.xt, mid_height + gap - font_size * (i + 1))
                    else
                        writeHorizontal(g, text_grouped_list_(i).reverse, font_size,
                        targetAreaInfo.xt, mid_height + gap - font_size * (i))
                }
                case Vertical.CenterTop =>
                if(text_length <= width){
                  writeHorizontal(g, source, font_size, targetAreaInfo.xt, mid_height)
                }
                else{
                  val gap = text_grouped_list.size / 2 * font_size
                  for(i <- 0 to text_grouped_list.size - 1)
                  writeHorizontal(g, text_grouped_list(i), font_size, targetAreaInfo.xt, mid_height - gap + font_size * i)
                }
              }
      case Horizontal.Right =>
        val line_letter_size = width / font_size
        val text_grouped_list = source.grouped(line_letter_size).toList

        /** start write top side */
        alignVertical match {
          case Vertical.Top =>
          if(text_length <= width){

            writeHorizontal(g, source, font_size, targetAreaInfo.xb - text_length, targetAreaInfo.yt)
          }
          else{
            for(i <- 0 to text_grouped_list.size - 1)
            writeHorizontal(g, text_grouped_list(i), font_size,
              targetAreaInfo.xb - text_grouped_list(i).size * font_size, targetAreaInfo.yt + font_size * i)
          }
          case Vertical.Bottom =>
          if(text_length <= width){
            writeHorizontal(g, source, font_size, targetAreaInfo.xb - text_length, targetAreaInfo.yb - font_size)
          }
          else{
            val gap = text_grouped_list.size * font_size
            for(i <- 0 to text_grouped_list.size - 1)
            writeHorizontal(g, text_grouped_list(i), font_size,
              targetAreaInfo.xb - text_grouped_list(i).size * font_size, targetAreaInfo.yb - gap + font_size * i)
          }
          case Vertical.CenterBottom =>
          val reversed_text = source.reverse
          val text_grouped_list_ = reversed_text.grouped(line_letter_size).toList
          if(text_length <= width){
            writeHorizontal(g, source, font_size, targetAreaInfo.xb - text_length, mid_height)
          }
          else{
            val gap = text_grouped_list.size / 2 * font_size
            for(i <- 0 to text_grouped_list.size - 1)
            if(line_size_width % 2 == 0)
              writeHorizontal(g, text_grouped_list_(i).reverse, font_size,
                targetAreaInfo.xb - text_grouped_list_(i).size * font_size, mid_height + gap - font_size * (i+1))
            else
              writeHorizontal(g, text_grouped_list_(i).reverse, font_size,
                targetAreaInfo.xb - text_grouped_list_(i).size * font_size, mid_height + gap - font_size * (i))
          }
          case Vertical.CenterTop =>
          if(text_length <= width){
            writeHorizontal(g, source, font_size, targetAreaInfo.xb - text_length, mid_height)
          }
          else{
            val gap = text_grouped_list.size / 2 * font_size
            for(i <- 0 to text_grouped_list.size - 1)
            writeHorizontal(g, text_grouped_list(i), font_size,
              targetAreaInfo.xb - text_grouped_list(i).size * font_size, mid_height - gap + font_size * i)
          }
        }
      case Horizontal.CenterLeft | Horizontal.CenterRight =>
        val line_letter_size = width / font_size
        val text_grouped_list = source.grouped(line_letter_size).toList
        alignVertical match {
          case Vertical.Top =>
          if(text_length <= width){

            writeHorizontal(g, source, font_size, mid_width - text_length / 2, targetAreaInfo.yt)
          }
          else{
            for(i <- 0 to text_grouped_list.size - 1)
            writeHorizontal(g, text_grouped_list(i), font_size,
              mid_width - text_grouped_list(i).size / 2 * font_size, targetAreaInfo.yt + font_size * i)
          }
          case Vertical.Bottom =>
          if(text_length <= width){
            writeHorizontal(g, source, font_size, mid_width - text_length / 2, targetAreaInfo.yb - font_size)
          }
          else{
            val gap = text_grouped_list.size * font_size
            for(i <- 0 to text_grouped_list.size - 1)
            writeHorizontal(g, text_grouped_list(i), font_size,
              mid_width - text_grouped_list(i).size / 2 * font_size, targetAreaInfo.yb - gap + font_size * i)
          }
          case Vertical.CenterBottom =>
          val reversed_text = source.reverse
          val text_grouped_list_ = reversed_text.grouped(line_letter_size).toList

          if(text_length <= width){
            writeHorizontal(g, source, font_size, mid_width - text_length / 2, mid_height)
          }
          else{
            val gap = text_grouped_list.size / 2 * font_size
            for(i <- 0 to text_grouped_list.size - 1)
              if(line_size_width % 2 == 0)
                writeHorizontal(g, text_grouped_list_(i).reverse, font_size,
                  mid_width - text_grouped_list_(i).size / 2 * font_size,
                  mid_height + gap - font_size * (i+1))
              else
                  writeHorizontal(g, text_grouped_list_(i).reverse, font_size,
                  mid_width - text_grouped_list_(i).size / 2 * font_size,
                  mid_height + gap - font_size * (i))
          }
          case Vertical.CenterTop =>
          if(text_length <= width){
            writeHorizontal(g, source, font_size, mid_width - text_length / 2, mid_height)
          }
          else{
            val gap = text_grouped_list.size / 2 * font_size
            for(i <- 0 to text_grouped_list.size - 1)
            writeHorizontal(g, text_grouped_list(i), font_size,
              mid_width - text_grouped_list(i).size / 2 * font_size,
              mid_height - gap + font_size * i)
          }
        }
      }
    }

  /** debug rendering a vertical text */
  def showVerticalText(text: String, fontsize: Int) {
    val frame = new javax.swing.JFrame
    frame.setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE)
    val panel = new javax.swing.JPanel {
      override def paintComponent(g: java.awt.Graphics) {
        writeVertical(g.asInstanceOf[Graphics2D], text, fontsize, fontsize, fontsize)
      }
    }
    frame.add(panel)
    frame.setSize(300, 800)
    frame.setVisible(true)
  }

  /** Write mutable string on plural lines, sub function of mutableVerticalWrite() */
  def mutableVerticalWritePluralLines(g: Graphics2D, source: String, targetAreaInfo: TargetAreaInfo, alignVertical: Vertical, alignHorizontal: Horizontal,
    text_length:Int, i:Int = 1){
    val height = targetAreaInfo.yb - targetAreaInfo.yt
    val width = targetAreaInfo.xb - targetAreaInfo.xt

    if(width / i < 1){
      Console.out.println( Console.RED + "Warning: Your WIDTH size is too small to write." + Console.RESET )
      mutableVerticalWrite(g, " ", targetAreaInfo,
        alignVertical, alignHorizontal)
      return
    }

    val text_length_max = height / (width / i)

    //println(width/i, text_length_max)

    if(text_length <= text_length_max * i){
        val font_size = width / i
        //println(text_length, text_length_max, font_size)
        immutableVerticalWrite(g, source, targetAreaInfo.copy(size = font_size), alignVertical, alignHorizontal)
      }
      else{
        mutableVerticalWritePluralLines(g, source, targetAreaInfo, alignVertical, alignHorizontal, text_length, i + 1)
      }
  }

  /** Mutable Vertical write */
  def mutableVerticalWrite(g: Graphics2D, source: String, targetAreaInfo: TargetAreaInfo,
    alignVertical: Vertical, alignHorizontal: Horizontal){

    /** Exception: If x_top == x_bottom */
    if(targetAreaInfo.xt == targetAreaInfo.xb || targetAreaInfo.xb - targetAreaInfo.xt < 2){
      Console.out.println( Console.RED + "Warning: WIDTH size is NULL. We set the WIDTH size to " + WidthDefault +" by default" + Console.RESET )
    }

    val x_bottom = if(targetAreaInfo.xt == targetAreaInfo.xb || targetAreaInfo.xb - targetAreaInfo.xt < 2){WidthDefault + targetAreaInfo.xt}else{targetAreaInfo.xb}

    /** Exception: If y_top == y_bottom */
    if(targetAreaInfo.yt == targetAreaInfo.yb || targetAreaInfo.yb - targetAreaInfo.yt < 2){
      Console.out.println( Console.RED + "Warning: HEIGHT size is NULL. We set the HEIGHT size to " + HeightDefault +" by default" + Console.RESET )
    }

    /** Exception: if source is NULL we call immutableVerticalWrite function **/
    if(source.length() == 0){
      Console.out.println( Console.RED + "Warning: your text is NULL"+ Console.RESET )
      immutableVerticalWrite(g, source, targetAreaInfo, alignVertical, alignHorizontal)
      return
    }

    val y_bottom = if(targetAreaInfo.yt == targetAreaInfo.yb || targetAreaInfo.yb - targetAreaInfo.yt < 2){WidthDefault + targetAreaInfo.yt}else{targetAreaInfo.yb}


    val height = (y_bottom - targetAreaInfo.yt).abs
    val width = if((x_bottom - targetAreaInfo.xt).abs != 0) (x_bottom - targetAreaInfo.xt).abs else 2

    // Vertical writing
    val text_length_max = height / (width / 2)
    val text_length = if(source.length != 0) source.length else 2
    //println(text_length, text_length_max)
    // write on one line
    if(text_length <= text_length_max){
      val font_size = height / text_length
      //println(font_size, width)
      if(font_size > width){
        immutableVerticalWrite(g, source, targetAreaInfo.copy(size = width, xb = x_bottom, yb = y_bottom), alignVertical, alignHorizontal)
      }
      else{
        immutableVerticalWrite(g, source, targetAreaInfo.copy(size = font_size, xb = x_bottom, yb = y_bottom), alignVertical, alignHorizontal)
        //println(font_size)
      }
    }

    else{
    mutableVerticalWritePluralLines(g, source, targetAreaInfo.copy(xb = x_bottom, yb = y_bottom), alignVertical, alignHorizontal, text_length)
      }
    }


/** Write mutable string on plural lines, sub function of mutableHorizontalWrite() */
  def mutableHorizontalWritePluralLines(g: Graphics2D, source: String, targetAreaInfo: TargetAreaInfo, alignVertical: Vertical, alignHorizontal: Horizontal,
    text_length:Int, i:Int = 1){
    val height = (targetAreaInfo.yb - targetAreaInfo.yt).abs
    val width = (targetAreaInfo.xb - targetAreaInfo.xt).abs

    if(height / i < 1){
      Console.out.println( Console.RED + "Warning: Your HEIGHT size is too small to write." + Console.RESET )
      mutableHorizontalWrite(g, " ", targetAreaInfo,
        alignVertical, alignHorizontal)
      return
    }

    val text_length_max = width / (height / i)

    //println(height/i, text_length_max)

    if(text_length <= text_length_max * i){
        val font_size = height / i
        //println(text_length, text_length_max, font_size)
        immutableHorizontalWrite(g, source, targetAreaInfo.copy(size = font_size), alignVertical, alignHorizontal)
      }
      else{
        mutableHorizontalWritePluralLines(g, source, targetAreaInfo, alignVertical, alignHorizontal, text_length, i + 1)
      }
  }

    /** Mutable Horizontal write */
  def mutableHorizontalWrite(g: Graphics2D, source: String, targetAreaInfo: TargetAreaInfo,
    alignVertical: Vertical, alignHorizontal: Horizontal){

    /** Exception: If x_top == x_bottom */
    if(targetAreaInfo.xt == targetAreaInfo.xb || targetAreaInfo.xb - targetAreaInfo.xt < 2){
      Console.out.println( Console.RED + "Warning: WIDTH size is NULL or 1. We set the WIDTH size to " + WidthDefault +" by default" + Console.RESET )
    }

    val x_bottom = if(targetAreaInfo.xt == targetAreaInfo.xb || targetAreaInfo.xb - targetAreaInfo.xt < 2){WidthDefault + targetAreaInfo.xt}else{targetAreaInfo.xb}
    //println(x_bottom)

    /** Exception: If y_top == y_bottom */
    if(targetAreaInfo.yt == targetAreaInfo.yb || targetAreaInfo.yb - targetAreaInfo.yt < 2){
      Console.out.println( Console.RED + "Warning: HEIGHT size is NULL. We set the HEIGHT size to " + HeightDefault +" by default" + Console.RESET )
    }

    /** Exception: if source is NULL we call immutableHorizontalWrite function **/
    if(source.length() == 0){
      Console.out.println( Console.RED + "Warning: your text is NULL"+ Console.RESET )
      immutableHorizontalWrite(g, source, targetAreaInfo, alignVertical, alignHorizontal)
      return
    }

    val y_bottom = if(targetAreaInfo.yt == targetAreaInfo.yb || targetAreaInfo.yb - targetAreaInfo.yt < 2){WidthDefault + targetAreaInfo.yt}else{targetAreaInfo.yb}


    val height = (y_bottom - targetAreaInfo.yt).abs
    val width = (x_bottom - targetAreaInfo.xt).abs

    // Horizontal writing
    val text_length_max = width / (height / 2)
    val text_length = source.length()
    //println(text_length, text_length_max)
    // write on one line
    if(text_length <= text_length_max){
      val font_size = width / text_length
      //println(font_size, height)
      if(font_size > height){
        immutableHorizontalWrite(g, source, targetAreaInfo.copy(size = height, xb = x_bottom, yb = y_bottom), alignVertical, alignHorizontal)
      }
      else{
        immutableHorizontalWrite(g, source, targetAreaInfo.copy(size = font_size, xb = x_bottom, yb = y_bottom), alignVertical, alignHorizontal)
        //println(font_size)
      }
    }

    else{
    mutableHorizontalWritePluralLines(g, source, targetAreaInfo.copy(xb = x_bottom, yb = y_bottom), alignVertical, alignHorizontal, text_length)
      }
  }


  def writeVerticalNextLine(g: Graphics2D, source: String, targetAreaInfo: TargetAreaInfo,
    alignVertical: Vertical, alignHorizontal: Horizontal): Int = {

      val font_size = if(targetAreaInfo.size == 0){
        defaultFontSize
      } else{
        targetAreaInfo.size
      }

      val width = (targetAreaInfo.xb - targetAreaInfo.xt).abs
      val heightp = (targetAreaInfo.yb - targetAreaInfo.yt).abs
      val height = if(heightp != 0) heightp else 1

      val letter_size = source.length
      val text_length = font_size * letter_size

      //println(width, height, font_size, letter_size, text_length)



      alignVertical match {
        case Vertical.Top =>
        val line_letter_size = height / font_size
        val text_grouped_list = source.grouped(line_letter_size).toList

        alignHorizontal match {

          case Horizontal.Right =>
          if(text_length <= height){
            writeVertical(g, source, font_size, targetAreaInfo.xt, targetAreaInfo.yt)
          }
          else{
            for(i <- 0 to text_grouped_list.size - 1)
            writeVertical(g, text_grouped_list(i), font_size, targetAreaInfo.xt - font_size * i, targetAreaInfo.yt)
          }
        }

        case Vertical.Bottom =>
        val line_letter_size = height / font_size

        alignHorizontal match {

          case Horizontal.Right =>
          val text_grouped_list = source.grouped(line_letter_size).toList

          if(text_length <= height){
            writeVertical(g, source, font_size, targetAreaInfo.xt, targetAreaInfo.yb - text_length)
          }
          else{
            for(i <- 0 to text_grouped_list.size - 1)
            writeVertical(g, text_grouped_list(i), font_size, targetAreaInfo.xt - font_size * i,
              targetAreaInfo.yb - text_grouped_list(i).size * font_size)
          }

          case _ => //todo

        }

      }

      /** calculate how many lines needed to write */
      val lineNumbers = if (text_length == 0) 0 else if(text_length <= height) 1 else (if(text_length % height == 0) text_length / height else (text_length / height) + 1)
      return lineNumbers
    }

  def writeHorizontalNextLine(g: Graphics2D, source: String, targetAreaInfo: TargetAreaInfo,
    alignVertical: Vertical, alignHorizontal: Horizontal): Int = {
      val font_size = if(targetAreaInfo.size == 0){
        defaultFontSize
      }else{
        targetAreaInfo.size
      }

      val widthp = (targetAreaInfo.xb - targetAreaInfo.xt).abs
      val width = if(widthp != 0) widthp else 1

      val height = (targetAreaInfo.yb - targetAreaInfo.yt).abs

      val letter_size = source.length()
      val text_length = font_size * letter_size

      //println(width, height, font_size, letter_size, text_length)

      alignHorizontal match {
        case Horizontal.Left =>
        val line_letter_size = width / font_size
        val text_grouped_list = source.grouped(line_letter_size).toList

        alignVertical match {

          case Vertical.Top =>
          if(text_length <= height){
            writeHorizontal(g, source, font_size, targetAreaInfo.xt, targetAreaInfo.yt)
          }
          else{
            for(i <- 0 to text_grouped_list.size - 1)
            writeHorizontal(g, text_grouped_list(i), font_size, targetAreaInfo.xt, targetAreaInfo.yt + font_size * i)
          }

        }

        case Horizontal.Right =>
        val line_letter_size = width / font_size

        alignVertical match {

          case Vertical.Top =>
          val text_grouped_list = source.grouped(line_letter_size).toList

          if(text_length <= height){
            writeHorizontal(g, source, font_size, targetAreaInfo.xb - text_length, targetAreaInfo.yt)
          }
          else{
            for(i <- 0 to text_grouped_list.size - 1)
            writeHorizontal(g, text_grouped_list(i), font_size, targetAreaInfo.xb - text_grouped_list(i).size * font_size,
              targetAreaInfo.yt + font_size * i)
          }
        }

      }

      /** calculate how many lines needed to write */
      val lineNumbers = if(text_length==0) 0 else (if(text_length <= width) 1 else {if(text_length % width == 0) text_length / width else text_length / width + 1})
      return lineNumbers
    }

  def countVerticalNextLine(source: String, targetAreaInfo: TargetAreaInfo): Int = {

  val font_size = if(targetAreaInfo.size == 0){
    defaultFontSize
  }else{
    targetAreaInfo.size
  }

  val width = (targetAreaInfo.xb - targetAreaInfo.xt).abs
  val heightp = (targetAreaInfo.yb - targetAreaInfo.yt).abs
  val height = if(heightp != 0) heightp else 1
  val letter_size = source.length
  val text_length = font_size * letter_size

  /** calculate how many lines needed to write */
  val lineNumbers = if(letter_size==0) 0 else (if(text_length <= height){1} else{if(text_length % height == 0) text_length / height else text_length / height + 1})
  return lineNumbers
}

def countHorizontalNextLine(source: String, targetAreaInfo: TargetAreaInfo): Int = {

  val font_size = if(targetAreaInfo.size == 0){defaultFontSize}
                  else{targetAreaInfo.size}
  val widthp = (targetAreaInfo.xb - targetAreaInfo.xt).abs
  val width = if(widthp != 0) widthp else 1
  val height = (targetAreaInfo.yb - targetAreaInfo.yt).abs
  val letter_size = source.length
  val text_length = font_size * letter_size
  /** calculate how many lines needed to write */
  val lineNumbers = if(letter_size==0) 0 else ( if(text_length <= width){1} else{ if(text_length % width == 0) text_length / width else text_length / width + 1})
  return lineNumbers
  }

}
