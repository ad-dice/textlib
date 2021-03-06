package com.ad_dice.textlib

import java.awt.{Image, Color, Font, Dimension, Graphics2D, RenderingHints}
import java.awt.geom.AffineTransform
import java.awt.image.BufferedImage
import java.awt.font.FontRenderContext
import java.io.File
import javax.imageio.ImageIO

case class TargetAreaInfo(xt: Int, yt: Int, xb: Int, yb: Int, size: Int)

object TextLib extends App {

    val defaultSize = 100.0
    val WidthDefault = 2
    val HeightDefault = 2
    def defaultFontSize: Int = 10 //FontDefault

    /**  definitions for vertical writing */
    /**  no need to rotate, case match */
    def rotationAlignUpChars = Set('(', '（', '「', '『', '【', '［', '[',
      '｛', '{', '〔', '＜', '<', '［', '《', '〈')
    def rotationAlignDownChars = Set(')', '）', '」', '』','】', '］', ']',
    '｝', '}', '〕', '＞', '>', '］', '》', '〉')
    def horizontalUpChars = Set('」', '』')
    def rotationAlign = Set('ー', '-', '‐', '‑', '‒', '–', '―','ｰ', '−')
    def rotationAlignRightChars = Set('＝', '=')
    def kutouTen = Set('、', '。')

    /** need to rotate */
    def rotationChars = Set('~')
    def rotationTranslationUpDown = Set('〜')

    /** need to translate */
    def translationChars = Set(',', '.', '，', '.')
    def translationSmallChars = Set('ゃ', 'ょ', 'ゅ', 'っ', 'ぁ', 'ぇ', 'ぃ', 'ぉ', 'ぅ',
      'ャ', 'ョ', 'ュ', 'ッ', 'ァ', 'ェ', 'ィ', 'ォ', 'ゥ')
    def halfwidthAlphabetBig = Set(
      'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
      'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
      '0', '1', '2', '3', '4', '5', '6', '7', '8', '9','%', '$', '¥')
    def halfwidthAlphabetSmallUpResize = Set('a', 'c', 'e', 'm',
      'n', 'o', 'r', 's', 't', 'u', 'v', 'w', 'x', 'z')
    def halfwidthAlphabetSmall = Set('b', 'd', 'f', 'h', 'i', 'l', 'k')
    def halfwidthAlphabetSmallUp = Set('y', 'q', 'g', 'j', 'p')
    def rotateAlign(a: Char) = a match {
      case '「' => '﹁'
      case '」' => '﹂'
      case '『' => '﹃'
      case '』' => '﹄'

      case '（' => '︵'
      case '）' => '︶'
      case '(' => '︵'
      case ')' => '︶'

      case '［' => '﹇'
      case '］' => '﹈'
      case '[' => '﹇'
      case ']' => '﹈'

      case '【' => '︻'
      case '】' => '︼'

      case '｛' => '︷'
      case '｝' => '︸'
      case '{' => '︷'
      case '}' => '︸'

      case '〔' => '︹'
      case '〕' => '︺'

      case '〈' => '︿'
      case '〉' => '﹀'
      case '＜' => '︿'
      case '＞' => '﹀'
      case '<' => '︿'
      case '>' => '﹀'

      case '《' => '︽'
      case '》' => '︾'

      case 'ー' => '｜'
      case '-' => '｜'
      case '‐' => '｜'
      case '‑' => '｜'
      case '‒' => '｜'
      case '–' => '｜'
      case '―' => '｜'
      case 'ｰ' => '｜'
      case '−' => '｜'

      case '＝' => '‖'
      case '=' => '‖'

      case '、' => '︑'
      case '。' => '︒'
    }

    def halfWidthNumber = Set('0', '1', '2', '3', '4', '5', '6', '7', '8', '9')

    def verticalRatio(letter: Char): Double = {
      var count_letter = 0.0
      if(rotationAlignUpChars(letter)){
        count_letter += 2 / 4.0
      }
      else if(rotationAlignDownChars(letter)){
        count_letter += 3 / 4.0
      }
      else if(rotationChars(letter)){
        count_letter += 3 / 4.0
      }
      else if(translationChars(letter)){
        count_letter += 1 / 4.0
      }
      else if(halfwidthAlphabetSmallUpResize(letter)){
        count_letter += 3 / 4.0
      }
      else{
        count_letter += 1
      }

      return count_letter
    }

    def horizontalRatio(g: Graphics2D, letter: Char): Double = {
      val font_init = g.getFont.deriveFont(100.toFloat)
      g.setFont(font_init)

      return g.getFontMetrics(font_init).stringWidth(letter.toString) / 100.0
    }


    def countVerticalSourceSize(source: String): Double = {
      var count_letter = 0.0
      for(letter <- source) {
        count_letter += verticalRatio(letter)
      }

      return count_letter
    }

    def countHorizontalSourceSize(g: Graphics2D, source: String): Double = {
      var count_letter = 0.0
      for(letter <- source) {
        count_letter += horizontalRatio(g, letter)
      }

      return count_letter
    }

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
        //println(r, char, g.getFontMetrics(font).stringWidth(char.toString))
        if (rotationAlignUpChars(char)) {
          val transform = AffineTransform.getRotateInstance(Math.toRadians(0), 0, 0)
          g.setFont(font.deriveFont(transform))

          g.drawString(rotateAlign(char).toString, x, y + (i + 1) * size - size * 2 / 4)
          writeVertical(g, text.tail, size, x, y - size * 2 / 4, font, i + 1)
        }
        else if (rotationAlignDownChars(char)) {
          val transform = AffineTransform.getRotateInstance(Math.toRadians(0), 0, 0)
          g.setFont(font.deriveFont(transform))

          g.drawString(rotateAlign(char).toString, x, y + (i + 1) * size)
          writeVertical(g, text.tail, size, x, y - size / 4, font, i + 1)
        }

        else if (rotationAlignRightChars(char)){
          val transform = AffineTransform.getRotateInstance(Math.toRadians(0), 0, 0)
          g.setFont(font.deriveFont(transform))

          g.drawString(rotateAlign(char).toString, x + size / 4, y + (i + 1) * size)
          writeVertical(g, text.tail, size, x, y, font, i + 1)
        }

        else if (rotationAlign(char)){
          val transform = AffineTransform.getRotateInstance(Math.toRadians(0), 0, 0)
          g.setFont(font.deriveFont(transform))

          g.drawString(rotateAlign(char).toString, x, y + (i + 1) * size)
          writeVertical(g, text.tail, size, x, y, font, i + 1)
        }


        else if (rotationChars(char)) {
          val transform = AffineTransform.getRotateInstance(Math.toRadians(0), 0, 0)
          g.setFont(font.deriveFont(transform))

          transform.rotate(Math.toRadians(90))
          g.setFont(font.deriveFont(transform))

          g.drawString(char.toString,
            //x + size / 2 - g.getFontMetrics(font).stringWidth(char.toString) / 2 * 14 / 16,*/
            x + size / 4 ,
            y + (i + 1) * size - size * 3 / 4)

          transform.setToTranslation(0, 0)
          transform.rotate(Math.toRadians(0))
          g.setTransform(transform)

          writeVertical(g, text.tail, size, x, y - size * 1 / 4, font, i + 1)
        }

        else if (rotationTranslationUpDown(char)) {
          val transform = AffineTransform.getRotateInstance(Math.toRadians(0), 0, 0)
          g.setFont(font.deriveFont(transform))

          transform.rotate(Math.toRadians(90))
          g.setFont(font.deriveFont(transform))

          g.drawString(char.toString, x + size / 2 - g.getFontMetrics(font).stringWidth(char.toString) / 2 * 14 / 16
            , y + (i + 1) * size - size * 7 / 8)

          transform.setToTranslation(0, 0)
          transform.rotate(Math.toRadians(0))
          g.setTransform(transform)

          writeVertical(g, text.tail, size, x, y, font, i + 1)
        }

        else if (kutouTen(char)) {
          g.drawString(rotateAlign(char).toString, x, y + (i + 1) * size)
          writeVertical(g, text.tail, size, x, y, font, i + 1)
        }

        else if (translationChars(char)) {
          g.drawString(char.toString, x + size / 4 * 3, y + (i + 1) * size -size / 4 * 3)
          writeVertical(g, text.tail, size, x, y - size / 4 * 3, font, i + 1)
        }

        else if (translationSmallChars(char)) {
          g.drawString(char.toString, x + size / 5, y + (i + 1) * size)
          writeVertical(g, text.tail, size, x, y, font, i + 1)
        }

        else if (halfwidthAlphabetBig(char)){
          val transform = AffineTransform.getRotateInstance(Math.toRadians(0), 0, 0)
          g.setFont(font.deriveFont(transform))

          g.drawString(char.toString,
            x + size / 2 - g.getFontMetrics(font).stringWidth(char.toString) / 2,
            y + (i + 1) * size)
          writeVertical(g, text.tail, size, x, y, font, i + 1)
        }

        else if (halfwidthAlphabetSmall(char)){
          val transform = AffineTransform.getRotateInstance(Math.toRadians(0), 0, 0)
          g.setFont(font.deriveFont(transform))

          g.drawString(char.toString,
            x + size / 2 - g.getFontMetrics(font).stringWidth(char.toString) / 2,
            y + (i + 1) * size)
          writeVertical(g, text.tail, size, x, y, font, i + 1)
        }

        else if (halfwidthAlphabetSmallUp(char)){
          val transform = AffineTransform.getRotateInstance(Math.toRadians(0), 0, 0)
          g.setFont(font.deriveFont(transform))

          g.drawString(char.toString,
            x + size / 2 - g.getFontMetrics(font).stringWidth(char.toString) / 2,
            y + (i + 1) * size - size / 4)
          writeVertical(g, text.tail, size, x, y, font, i + 1)
        }

        else if (halfwidthAlphabetSmallUpResize(char)){
          val transform = AffineTransform.getRotateInstance(Math.toRadians(0), 0, 0)
          g.setFont(font.deriveFont(transform))

          g.drawString(char.toString,
            x + size / 2 - g.getFontMetrics(font).stringWidth(char.toString) / 2,
            y + (i + 1) * size - size / 4)
          writeVertical(g, text.tail, size, x, y - size / 4, font, i + 1)
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


    def writeHorizontal(g: Graphics2D, text: String, size: Int, x: Int, y: Int) {
      val font = g.getFont.deriveFont(size.toFloat)
      g.setFont(font)
      //g.setFont(g.getFont.deriveFont(size.toFloat))
      //for(i <- 0 to text.length - 1)
        //println(text(i), g.getFontMetrics(font).stringWidth(text(i).toString), g.getFontMetrics.getHeight())
      g.drawString(text, x, y + size * 13 / 16)
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

    val height = (y_bottom - targetAreaInfo.yt).abs
    val width = (x_bottom - targetAreaInfo.xt).abs

    var count_letter = countVerticalSourceSize(source)
  
    val line_numbers = if(count_letter.ceil.toInt <= (width / font_size - 1) * (height / font_size)) (width / font_size - 1) else (width / font_size)
    // println(width / font_size, line_numbers)
    val font = g.getFont.deriveFont(font_size.toFloat)
    g.setFont(font)

    val marginErr = font_size / 8
    var source_heightS = 0.0
    var j, k = 0

    for(i <- 0 to source.length - 1){
      source_heightS += verticalRatio(source(i)) * font_size
      if(source_heightS > height){
        val source_height = (source_heightS - verticalRatio(source(i)) * font_size).toInt
        val sub_source = source.substring(j,i)
        alignVertical match {
          case Vertical.Top =>
            alignHorizontal match {
              case Horizontal.Left =>
                writeVertical(g, sub_source, font_size, targetAreaInfo.xt + k * font_size , targetAreaInfo.yt )
              case Horizontal.Right =>
                writeVertical(g, sub_source, font_size, targetAreaInfo.xb - font_size - k * font_size, targetAreaInfo.yt)
              case Horizontal.CenterLeft =>
                if(line_numbers % 2 == 0)
                  writeVertical(g, sub_source, font_size, targetAreaInfo.xt + width / 2 - (line_numbers / 2 - k) * font_size, targetAreaInfo.yt)
                else
                  writeVertical(g, sub_source, font_size, targetAreaInfo.xt + width / 2 - font_size / 2 - (line_numbers / 2 - k) * font_size, targetAreaInfo.yt)
              case Horizontal.CenterRight =>
                if(line_numbers % 2 == 0)
                  writeVertical(g, sub_source, font_size, targetAreaInfo.xt + width / 2 + (line_numbers / 2 - k - 1) * font_size, targetAreaInfo.yt)
                else
                  writeVertical(g, sub_source, font_size, targetAreaInfo.xt + width / 2 - font_size / 2 + (line_numbers / 2 - k) * font_size, targetAreaInfo.yt)
              }
            case Vertical.Bottom =>
            alignHorizontal match {
              case Horizontal.Left =>
                writeVertical(g, sub_source, font_size, targetAreaInfo.xt + k * font_size , targetAreaInfo.yb - source_height - marginErr)
              case Horizontal.Right =>
                writeVertical(g, sub_source, font_size, targetAreaInfo.xb - font_size - k * font_size, targetAreaInfo.yb - source_height - marginErr)
              case Horizontal.CenterLeft =>
                if(line_numbers % 2 == 0)
                  writeVertical(g, sub_source, font_size, targetAreaInfo.xt + width / 2 - (line_numbers / 2 - k) * font_size, targetAreaInfo.yb - source_height - marginErr)
                else
                  writeVertical(g, sub_source, font_size, targetAreaInfo.xt + width / 2 - font_size / 2 - (line_numbers / 2 - k) * font_size, targetAreaInfo.yb - source_height - marginErr)
              case Horizontal.CenterRight =>
                if(line_numbers % 2 == 0)
                  writeVertical(g, sub_source, font_size, targetAreaInfo.xt + width / 2 + (line_numbers / 2 - k - 1) * font_size, targetAreaInfo.yb - source_height - marginErr)
                else
                  writeVertical(g, sub_source, font_size, targetAreaInfo.xt + width / 2 - font_size / 2 + (line_numbers / 2 - k) * font_size, targetAreaInfo.yb - source_height - marginErr)
              }
            case Vertical.CenterTop| Vertical.CenterBottom =>
            alignHorizontal match {
              case Horizontal.Left =>
                writeVertical(g, sub_source, font_size, targetAreaInfo.xt + k * font_size , targetAreaInfo.yt + height / 2 - source_height / 2)
              case Horizontal.Right =>
                writeVertical(g, sub_source, font_size, targetAreaInfo.xb - font_size - k * font_size, targetAreaInfo.yt + height / 2 - source_height / 2)
              case Horizontal.CenterLeft =>
                if(line_numbers % 2 == 0)
                  writeVertical(g, sub_source, font_size, targetAreaInfo.xt + width / 2 - (line_numbers / 2 - k) * font_size, targetAreaInfo.yt + height / 2 - source_height / 2)
                else
                  writeVertical(g, sub_source, font_size, targetAreaInfo.xt + width / 2 - font_size / 2 - (line_numbers / 2 - k) * font_size, targetAreaInfo.yt + height / 2 - source_height / 2)
              case Horizontal.CenterRight =>
                if(line_numbers % 2 == 0)
                  writeVertical(g, sub_source, font_size, targetAreaInfo.xt + width / 2 + (line_numbers / 2 - k - 1) * font_size, targetAreaInfo.yt + height / 2 - source_height / 2)
                else
                  writeVertical(g, sub_source, font_size, targetAreaInfo.xt + width / 2 - font_size / 2 + (line_numbers / 2 - k) * font_size, targetAreaInfo.yt + height / 2 - source_height / 2)
              }
            }
        source_heightS = verticalRatio(source(i)) * font_size
        j = i
        k += 1
      }
    }
    val source_height = source_heightS.toInt
    val sub_source = source.substring(j, source.length)
         alignVertical match {
          case Vertical.Top =>
            alignHorizontal match {
              case Horizontal.Left =>
                writeVertical(g, sub_source, font_size, targetAreaInfo.xt + k * font_size , targetAreaInfo.yt )
              case Horizontal.Right =>
                writeVertical(g, sub_source, font_size, targetAreaInfo.xb - font_size - k * font_size, targetAreaInfo.yt)
              case Horizontal.CenterLeft =>
                if(line_numbers % 2 == 0)
                  writeVertical(g, sub_source, font_size, targetAreaInfo.xt + width / 2 - (line_numbers / 2 - k) * font_size, targetAreaInfo.yt)
                else
                  writeVertical(g, sub_source, font_size, targetAreaInfo.xt + width / 2 - font_size / 2 - (line_numbers / 2 - k) * font_size, targetAreaInfo.yt)
              case Horizontal.CenterRight =>
                if(line_numbers % 2 == 0)
                  writeVertical(g, sub_source, font_size, targetAreaInfo.xt + width / 2 + (line_numbers / 2 - k - 1) * font_size, targetAreaInfo.yt)
                else
                  writeVertical(g, sub_source, font_size, targetAreaInfo.xt + width / 2 - font_size / 2 + (line_numbers / 2 - k) * font_size, targetAreaInfo.yt)
              }
          case Vertical.Bottom =>
            alignHorizontal match {
              case Horizontal.Left =>
                writeVertical(g, sub_source, font_size, targetAreaInfo.xt + k * font_size , targetAreaInfo.yb - source_height - marginErr)
              case Horizontal.Right =>
                writeVertical(g, sub_source, font_size, targetAreaInfo.xb - font_size - k * font_size, targetAreaInfo.yb - source_height - marginErr)
              case Horizontal.CenterLeft =>
                if(line_numbers % 2 == 0)
                  writeVertical(g, sub_source, font_size, targetAreaInfo.xt + width / 2 - (line_numbers / 2 - k) * font_size, targetAreaInfo.yb - source_height - marginErr)
                else
                  writeVertical(g, sub_source, font_size, targetAreaInfo.xt + width / 2 - font_size / 2 - (line_numbers / 2 - k) * font_size, targetAreaInfo.yb - source_height - marginErr)
              case Horizontal.CenterRight =>
                if(line_numbers % 2 == 0)
                  writeVertical(g, sub_source, font_size, targetAreaInfo.xt + width / 2 + (line_numbers / 2 - k - 1) * font_size, targetAreaInfo.yb - source_height - marginErr)
                else
                  writeVertical(g, sub_source, font_size, targetAreaInfo.xt + width / 2 - font_size / 2 + (line_numbers / 2 - k) * font_size, targetAreaInfo.yb - source_height - marginErr)
              }
          case Vertical.CenterTop| Vertical.CenterBottom =>
            alignHorizontal match {
              case Horizontal.Left =>
                writeVertical(g, sub_source, font_size, targetAreaInfo.xt + k * font_size , targetAreaInfo.yt + height / 2 - source_height / 2)
              case Horizontal.Right =>
                writeVertical(g, sub_source, font_size, targetAreaInfo.xb - font_size - k * font_size, targetAreaInfo.yt + height / 2 - source_height / 2)
              case Horizontal.CenterLeft =>
                if(line_numbers % 2 == 0)
                  writeVertical(g, sub_source, font_size, targetAreaInfo.xt + width / 2 - (line_numbers / 2 - k) * font_size, targetAreaInfo.yt + height / 2 - source_height / 2)
                else
                  writeVertical(g, sub_source, font_size, targetAreaInfo.xt + width / 2 - font_size / 2 - (line_numbers / 2 - k) * font_size, targetAreaInfo.yt + height / 2 - source_height / 2)
              case Horizontal.CenterRight =>
                if(line_numbers % 2 == 0)
                  writeVertical(g, sub_source, font_size, targetAreaInfo.xt + width / 2 + (line_numbers / 2 - k - 1) * font_size, targetAreaInfo.yt + height / 2 - source_height / 2)
                else
                  writeVertical(g, sub_source, font_size, targetAreaInfo.xt + width / 2 - font_size / 2 + (line_numbers / 2 - k) * font_size, targetAreaInfo.yt + height / 2 - source_height / 2)
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

    val height = (y_bottom - targetAreaInfo.yt).abs
    val width = (x_bottom - targetAreaInfo.xt).abs

    var count_letter = countHorizontalSourceSize(g, source)

    val line_numbers = if(count_letter.ceil.toInt <= (height / font_size - 1) * (width / font_size)) (height / font_size - 1) else (height / font_size)
   
    val font = g.getFont.deriveFont(font_size.toFloat)
    g.setFont(font)
    var source_width = 0
    var j, k = 0

    for(i <- 0 to source.length - 1){
      source_width += g.getFontMetrics(font).stringWidth(source(i).toString)
      if(source_width > width){
        source_width = source_width - g.getFontMetrics(font).stringWidth(source(i).toString)
        val sub_source = source.substring(j,i)
        alignHorizontal match {
          case Horizontal.Left =>
            alignVertical match {
              case Vertical.Top =>
                writeHorizontal(g, sub_source, font_size, targetAreaInfo.xt, targetAreaInfo.yt + k * font_size)
              case Vertical.Bottom =>
                writeHorizontal(g, sub_source, font_size, targetAreaInfo.xt, targetAreaInfo.yb - font_size - k * font_size)
              case Vertical.CenterTop =>
                if(line_numbers % 2 == 0)
                  writeHorizontal(g, sub_source, font_size, targetAreaInfo.xt, targetAreaInfo.yt + height / 2 - (line_numbers / 2 - k) * font_size)
                else
                  writeHorizontal(g, sub_source, font_size, targetAreaInfo.xt, targetAreaInfo.yt + height / 2 - font_size / 2 - (line_numbers / 2 - k) * font_size)
              case Vertical.CenterBottom =>
                if(line_numbers % 2 == 0)
                  writeHorizontal(g, sub_source, font_size, targetAreaInfo.xt, targetAreaInfo.yt + height / 2 + (line_numbers / 2 - k - 1) * font_size)
                else
                  writeHorizontal(g, sub_source, font_size, targetAreaInfo.xt, targetAreaInfo.yt + height / 2 - font_size / 2 + (line_numbers / 2 - k) * font_size)
              }
          case Horizontal.Right =>
            alignVertical match {
              case Vertical.Top =>
                writeHorizontal(g, sub_source, font_size, targetAreaInfo.xb - source_width, targetAreaInfo.yt + k * font_size)
              case Vertical.Bottom =>
                writeHorizontal(g, sub_source, font_size, targetAreaInfo.xb - source_width, targetAreaInfo.yb - font_size - k * font_size)
              case Vertical.CenterTop =>
                if(line_numbers % 2 == 0)
                  writeHorizontal(g, sub_source, font_size, targetAreaInfo.xb - source_width, targetAreaInfo.yt + height / 2 - (line_numbers / 2 - k) * font_size)
                else
                  writeHorizontal(g, sub_source, font_size, targetAreaInfo.xb - source_width, targetAreaInfo.yt + height / 2 - font_size / 2 - (line_numbers / 2 - k) * font_size)
              case Vertical.CenterBottom =>
                if(line_numbers % 2 == 0)
                  writeHorizontal(g, sub_source, font_size, targetAreaInfo.xb - source_width, targetAreaInfo.yt + height / 2 + (line_numbers / 2 - k - 1) * font_size)
                else
                  writeHorizontal(g, sub_source, font_size, targetAreaInfo.xb - source_width, targetAreaInfo.yt + height / 2 - font_size / 2 + (line_numbers / 2 - k) * font_size)
              }
          case Horizontal.CenterLeft| Horizontal.CenterRight =>
            alignVertical match {
              case Vertical.Top =>
                writeHorizontal(g, sub_source, font_size, targetAreaInfo.xt + width / 2 - source_width / 2, targetAreaInfo.yt + k * font_size)
              case Vertical.Bottom =>
                writeHorizontal(g, sub_source, font_size, targetAreaInfo.xt + width / 2 - source_width / 2, targetAreaInfo.yb - font_size - k * font_size)
              case Vertical.CenterTop =>
                if(line_numbers % 2 == 0)
                  writeHorizontal(g, sub_source, font_size, targetAreaInfo.xt + width / 2 - source_width / 2, targetAreaInfo.yt + height / 2 - (line_numbers / 2 - k) * font_size)
                else
                  writeHorizontal(g, sub_source, font_size, targetAreaInfo.xt + width / 2 - source_width / 2, targetAreaInfo.yt + height / 2 - font_size / 2 - (line_numbers / 2 - k) * font_size)
              case Vertical.CenterBottom =>
                if(line_numbers % 2 == 0)
                  writeHorizontal(g, sub_source, font_size, targetAreaInfo.xt + width / 2 - source_width / 2, targetAreaInfo.yt + height / 2 + (line_numbers / 2 - k - 1) * font_size)
                else
                  writeHorizontal(g, sub_source, font_size, targetAreaInfo.xt + width / 2 - source_width / 2, targetAreaInfo.yt + height / 2 - font_size / 2 + (line_numbers / 2 - k) * font_size)
              }
        }
        source_width = g.getFontMetrics(font).stringWidth(source(i).toString)
        j = i
        k += 1
      }
    }

    val sub_source = source.substring(j, source.length)
        alignHorizontal match {
          case Horizontal.Left =>
            alignVertical match {
              case Vertical.Top =>
                writeHorizontal(g, sub_source, font_size, targetAreaInfo.xt, targetAreaInfo.yt + k * font_size)
              case Vertical.Bottom =>
                writeHorizontal(g, sub_source, font_size, targetAreaInfo.xt, targetAreaInfo.yb - font_size - k * font_size)
              case Vertical.CenterTop =>
                if(line_numbers % 2 == 0)
                  writeHorizontal(g, sub_source, font_size, targetAreaInfo.xt, targetAreaInfo.yt + height / 2 - (line_numbers / 2 - k) * font_size)
                else
                  writeHorizontal(g, sub_source, font_size, targetAreaInfo.xt, targetAreaInfo.yt + height / 2 - font_size / 2 - (line_numbers / 2 - k) * font_size)
              case Vertical.CenterBottom =>
                if(line_numbers % 2 == 0)
                  writeHorizontal(g, sub_source, font_size, targetAreaInfo.xt, targetAreaInfo.yt + height / 2 + (line_numbers / 2 - k - 1) * font_size)
                else
                  writeHorizontal(g, sub_source, font_size, targetAreaInfo.xt, targetAreaInfo.yt + height / 2 - font_size / 2 + (line_numbers / 2 - k) * font_size)
              }
          case Horizontal.Right =>
            alignVertical match {
              case Vertical.Top =>
                writeHorizontal(g, sub_source, font_size, targetAreaInfo.xb - source_width, targetAreaInfo.yt + k * font_size)
              case Vertical.Bottom =>
                writeHorizontal(g, sub_source, font_size, targetAreaInfo.xb - source_width, targetAreaInfo.yb - font_size - k * font_size)
              case Vertical.CenterTop =>
                if(line_numbers % 2 == 0)
                  writeHorizontal(g, sub_source, font_size, targetAreaInfo.xb - source_width, targetAreaInfo.yt + height / 2 - (line_numbers / 2 - k) * font_size)
                else
                  writeHorizontal(g, sub_source, font_size, targetAreaInfo.xb - source_width, targetAreaInfo.yt + height / 2 - font_size / 2 - (line_numbers / 2 - k) * font_size)
              case Vertical.CenterBottom =>
                if(line_numbers % 2 == 0)
                  writeHorizontal(g, sub_source, font_size, targetAreaInfo.xb - source_width, targetAreaInfo.yt + height / 2 + (line_numbers / 2 - k - 1) * font_size)
                else
                  writeHorizontal(g, sub_source, font_size, targetAreaInfo.xb - source_width, targetAreaInfo.yt + height / 2 - font_size / 2 + (line_numbers / 2 - k) * font_size)
              }
          case Horizontal.CenterLeft| Horizontal.CenterRight =>
            alignVertical match {
              case Vertical.Top =>
                writeHorizontal(g, sub_source, font_size, targetAreaInfo.xt + width / 2 - source_width / 2, targetAreaInfo.yt + k * font_size)
              case Vertical.Bottom =>
                writeHorizontal(g, sub_source, font_size, targetAreaInfo.xt + width / 2 - source_width / 2, targetAreaInfo.yb - font_size - k * font_size)
              case Vertical.CenterTop =>
                if(line_numbers % 2 == 0)
                  writeHorizontal(g, sub_source, font_size, targetAreaInfo.xt + width / 2 - source_width / 2, targetAreaInfo.yt + height / 2 - (line_numbers / 2 - k) * font_size)
                else
                  writeHorizontal(g, sub_source, font_size, targetAreaInfo.xt + width / 2 - source_width / 2, targetAreaInfo.yt + height / 2 - font_size / 2 - (line_numbers / 2 - k) * font_size)
              case Vertical.CenterBottom =>
                if(line_numbers % 2 == 0)
                  writeHorizontal(g, sub_source, font_size, targetAreaInfo.xt + width / 2 - source_width / 2, targetAreaInfo.yt + height / 2 + (line_numbers / 2 - k - 1) * font_size)
                else
                  writeHorizontal(g, sub_source, font_size, targetAreaInfo.xt + width / 2 - source_width / 2, targetAreaInfo.yt + height / 2 - font_size / 2 + (line_numbers / 2 - k) * font_size)
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

    if(text_length <= text_length_max * i){
        val font_size = width / i
        //immutableVerticalWrite(g, source, targetAreaInfo.copy(size = if(font_size>100) 100 else font_size), alignVertical, alignHorizontal)
        immutableVerticalWrite(g, source, targetAreaInfo.copy(size = font_size), alignVertical, alignHorizontal)
      }
    else{
        mutableVerticalWritePluralLines(g, source, targetAreaInfo, alignVertical, alignHorizontal, text_length, i + 1)
      }
  }

  /** Mutable Vertical write */
  def mutableVerticalWrite(g: Graphics2D, source: String, targetAreaInfo: TargetAreaInfo, alignVertical: Vertical, alignHorizontal: Horizontal){
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

    /** Counting letter */
    var count_letter = 0.0
    for(letter <- source){
      count_letter += verticalRatio(letter)
    }

    val letter_numbers = if(count_letter.ceil.toInt != 0) count_letter.ceil.toInt else 2
    val letter_numbers_max = height / (width / 2)
    println(count_letter, source.length, width)
    // write on one line
    if(letter_numbers <= letter_numbers_max){
      val font_size = height / letter_numbers
      if(font_size > width){
        // immutableVerticalWrite(g, source, targetAreaInfo.copy(size = if(width>100) 100 else width, xb = x_bottom, yb = y_bottom), alignVertical, alignHorizontal)
        immutableVerticalWrite(g, source, targetAreaInfo.copy(size = width, xb = x_bottom, yb = y_bottom), alignVertical, alignHorizontal)
      }
      else{
        // immutableVerticalWrite(g, source, targetAreaInfo.copy(size = if(width>100) 100 else width, xb = x_bottom, yb = y_bottom), alignVertical, alignHorizontal)
        immutableVerticalWrite(g, source, targetAreaInfo.copy(size = font_size, xb = x_bottom, yb = y_bottom), alignVertical, alignHorizontal)
      }
    }
    else{
    mutableVerticalWritePluralLines(g, source, targetAreaInfo.copy(xb = x_bottom, yb = y_bottom), alignVertical, alignHorizontal, letter_numbers)
      }
    }

  /** Write mutable string on plural lines, sub function of mutableHorizontalWrite() */
  def mutableHorizontalWritePluralLines(g: Graphics2D, source: String, targetAreaInfo: TargetAreaInfo, alignVertical: Vertical, alignHorizontal: Horizontal, text_length:Int, i:Int = 1){
    val height = (targetAreaInfo.yb - targetAreaInfo.yt).abs
    val width = (targetAreaInfo.xb - targetAreaInfo.xt).abs

    if(height / i < 1){
      Console.out.println( Console.RED + "Warning: Your HEIGHT size is too small to write." + Console.RESET )
      mutableHorizontalWrite(g, " ", targetAreaInfo, alignVertical, alignHorizontal)
      return
    }

    val text_length_max = width / (height / i)

    if(text_length <= text_length_max * i){
        val font_size = height / i
        immutableHorizontalWrite(g, source, targetAreaInfo.copy(size = font_size), alignVertical, alignHorizontal)
      }
    else{
        mutableHorizontalWritePluralLines(g, source, targetAreaInfo, alignVertical, alignHorizontal, text_length, i + 1)
      }
  }

  /** Mutable Horizontal write */
  def mutableHorizontalWrite(g: Graphics2D, source: String, targetAreaInfo: TargetAreaInfo, alignVertical: Vertical, alignHorizontal: Horizontal){
    /** Exception: If x_top == x_bottom */
    if(targetAreaInfo.xt == targetAreaInfo.xb || targetAreaInfo.xb - targetAreaInfo.xt < 2){
      Console.out.println( Console.RED + "Warning: WIDTH size is NULL or 1. We set the WIDTH size to " + WidthDefault +" by default" + Console.RESET )
    }

    val x_bottom = if(targetAreaInfo.xt == targetAreaInfo.xb || targetAreaInfo.xb - targetAreaInfo.xt < 2){WidthDefault + targetAreaInfo.xt}else{targetAreaInfo.xb}

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

    var count_letter = countHorizontalSourceSize(g, source)
    val letter_numbers = if(count_letter.ceil.toInt != 0) count_letter.ceil.toInt else 2
    // println(letter_numbers)

    // Horizontal writing
    val letter_numbers_max = width / (height / 2)
    if(letter_numbers <= letter_numbers_max){
      val font_size = width / letter_numbers
      if(font_size > height){
        immutableHorizontalWrite(g, source, targetAreaInfo.copy(size = height, xb = x_bottom, yb = y_bottom), alignVertical, alignHorizontal)
      }
      else{
        immutableHorizontalWrite(g, source, targetAreaInfo.copy(size = font_size, xb = x_bottom, yb = y_bottom), alignVertical, alignHorizontal)
      }
    }
    else{
    mutableHorizontalWritePluralLines(g, source, targetAreaInfo.copy(xb = x_bottom, yb = y_bottom), alignVertical, alignHorizontal, letter_numbers)
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

  /** Mutable Horizontal Write One Line */
  def mutableHorizontalWriteOneLine(g: Graphics2D, source: String, targetAreaInfo: TargetAreaInfo, alignVertical: Vertical, alignHorizontal: Horizontal){
    /** Exception: If x_top == x_bottom */
    if(targetAreaInfo.xt == targetAreaInfo.xb || targetAreaInfo.xb - targetAreaInfo.xt < 2){
      Console.out.println( Console.RED + "Warning: WIDTH size is NULL or 1. We set the WIDTH size to " + WidthDefault +" by default" + Console.RESET )
    }

    val x_bottom = if(targetAreaInfo.xt == targetAreaInfo.xb || targetAreaInfo.xb - targetAreaInfo.xt < 2){WidthDefault + targetAreaInfo.xt}else{targetAreaInfo.xb}

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

    var count_letter = countHorizontalSourceSize(g, source)
    // println(count_letter)
    val font_size = if(width / count_letter.ceil.toInt < height) width / count_letter.ceil.toInt else height

    val font = g.getFont.deriveFont(font_size.toFloat)
    g.setFont(font)
    var source_width = 0
    for(letter <- source){
      source_width += g.getFontMetrics(font).stringWidth(letter.toString)
    }

    //println(source_width, source.length * font_size, height)
    //println(count_letter, width / count_letter.ceil.toInt)

    alignHorizontal match {
      case Horizontal.Left =>
        alignVertical match {
          case Vertical.Top =>
            writeHorizontal(g, source, font_size, targetAreaInfo.xt, targetAreaInfo.yt)
          case Vertical.Bottom =>
            writeHorizontal(g, source, font_size, targetAreaInfo.xt, targetAreaInfo.yb - font_size)
          case Vertical.CenterBottom | Vertical.CenterTop =>
            writeHorizontal(g, source, font_size, targetAreaInfo.xt, targetAreaInfo.yt + height / 2 - font_size / 2)
        }
      case Horizontal.Right =>
        alignVertical match {
          case Vertical.Top =>
            writeHorizontal(g, source, font_size, targetAreaInfo.xb - source_width, targetAreaInfo.yt)
          case Vertical.Bottom =>
            writeHorizontal(g, source, font_size, targetAreaInfo.xb - source_width, targetAreaInfo.yb - font_size)
          case Vertical.CenterBottom | Vertical.CenterTop =>
            writeHorizontal(g, source, font_size, targetAreaInfo.xb - source_width, targetAreaInfo.yt + height / 2 - font_size / 2)
        }
      case Horizontal.CenterLeft| Horizontal.CenterRight =>
        alignVertical match {
          case Vertical.Top =>
            writeHorizontal(g, source, font_size, targetAreaInfo.xt + width / 2 - source_width / 2, targetAreaInfo.yt)
          case Vertical.Bottom =>
            writeHorizontal(g, source, font_size, targetAreaInfo.xt + width / 2 - source_width / 2, targetAreaInfo.yb - font_size)
          case Vertical.CenterBottom | Vertical.CenterTop =>
            writeHorizontal(g, source, font_size, targetAreaInfo.xt + width / 2 - source_width / 2, targetAreaInfo.yt + height / 2 - font_size / 2)
        }
    }
  }

  /** Mutable Vertical Write One Line */
  def mutableVerticalWriteOneLine(g: Graphics2D, source: String, targetAreaInfo: TargetAreaInfo, alignVertical: Vertical, alignHorizontal: Horizontal){
    /** Exception: If x_top == x_bottom */
    if(targetAreaInfo.xt == targetAreaInfo.xb || targetAreaInfo.xb - targetAreaInfo.xt < 2){
      Console.out.println( Console.RED + "Warning: WIDTH size is NULL or 1. We set the WIDTH size to " + WidthDefault +" by default" + Console.RESET )
    }

    val x_bottom = if(targetAreaInfo.xt == targetAreaInfo.xb || targetAreaInfo.xb - targetAreaInfo.xt < 2){WidthDefault + targetAreaInfo.xt}else{targetAreaInfo.xb}

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

    //println(source, height, width)

    var count_letter = countVerticalSourceSize(source)
    
    //println(source.length, height / source.length)
    //println(count_letter.ceil.toInt, height / count_letter.ceil.toInt)
    val font_size = if(height / count_letter.ceil.toInt < width) height / count_letter.ceil.toInt else width
    //println("size = ", font_size)

    val source_height = (count_letter * font_size).ceil.toInt
    var marginErr = font_size / 8

    alignVertical match {
      case Vertical.Top =>
        alignHorizontal match {
          case Horizontal.Left =>
            writeVertical(g, source, font_size, targetAreaInfo.xt, targetAreaInfo.yt)
          case Horizontal.Right =>
            writeVertical(g, source, font_size, targetAreaInfo.xb - font_size, targetAreaInfo.yt)
          case Horizontal.CenterLeft | Horizontal.CenterRight =>
            writeVertical(g, source, font_size, targetAreaInfo.xt + width / 2 - font_size / 2, targetAreaInfo.yt)
        }
      case Vertical.Bottom =>
        alignHorizontal match {
          case Horizontal.Left =>
            writeVertical(g, source, font_size, targetAreaInfo.xt, targetAreaInfo.yb - source_height - marginErr)
          case Horizontal.Right =>
            writeVertical(g, source, font_size, targetAreaInfo.xb - font_size, targetAreaInfo.yb - source_height - marginErr)
          case Horizontal.CenterLeft | Horizontal.CenterRight =>
            writeVertical(g, source, font_size, targetAreaInfo.xt + width / 2 - font_size / 2, targetAreaInfo.yb - source_height - marginErr)
        }
      case Vertical.CenterTop | Vertical.CenterBottom =>
        alignHorizontal match {
          case Horizontal.Left =>
            writeVertical(g, source, font_size, targetAreaInfo.xt, targetAreaInfo.yt + height / 2 - source_height / 2)
          case Horizontal.Right =>
            writeVertical(g, source, font_size, targetAreaInfo.xb - font_size, targetAreaInfo.yt + height / 2 - source_height / 2)
          case Horizontal.CenterLeft | Horizontal.CenterRight =>
            writeVertical(g, source, font_size, targetAreaInfo.xt + width / 2 - font_size / 2, targetAreaInfo.yt + height / 2 - source_height / 2)
        }
    }
  }

  def countVerticalLinesRecursion(source: String, targetAreaInfo: TargetAreaInfo, i: Int = 1): Int = {
    val height = targetAreaInfo.yb - targetAreaInfo.yt
    val width = targetAreaInfo.xb - targetAreaInfo.xt

    val text_length_max = height / (width / i)
    val count_letter = countVerticalSourceSize(source)
    if(count_letter <= text_length_max * i){
        return i
      }
    else{
        countVerticalLinesRecursion(source, targetAreaInfo, i + 1)
      }
  }
  /** Count how many lines needed for Mutable Vertical Write */
  def countVerticalLines(source: String, targetAreaInfo: TargetAreaInfo): Int = {
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
      return 1
    }
    val y_bottom = if(targetAreaInfo.yt == targetAreaInfo.yb || targetAreaInfo.yb - targetAreaInfo.yt < 2){WidthDefault + targetAreaInfo.yt}else{targetAreaInfo.yb}

    var lines = 0
    val height = (y_bottom - targetAreaInfo.yt).abs
    val width = if((x_bottom - targetAreaInfo.xt).abs != 0) (x_bottom - targetAreaInfo.xt).abs else 2

    /** Counting letter */
    var count_letter = countVerticalSourceSize(source)
    
    val letter_numbers = if(count_letter.ceil.toInt != 0) count_letter.ceil.toInt else 2
    val letter_numbers_max = height / (width / 2)
    // println(count_letter, source.length, width)
    
    // write on one line
    if(letter_numbers <= letter_numbers_max){
      lines = 1
    }
    else{
      lines = countVerticalLinesRecursion(source, targetAreaInfo)
      }

    val line_numbers = if(count_letter.ceil.toInt <= (lines - 1) * (height / (width / lines))) lines - 1 else lines
    return line_numbers
  }

  /** Count how many lines needed for Mutable Horizontal Write */
  def countHorizontalLinesRecursion(g: Graphics2D, source: String, targetAreaInfo: TargetAreaInfo, i: Int = 1): Int = {
    val height = targetAreaInfo.yb - targetAreaInfo.yt
    val width = targetAreaInfo.xb - targetAreaInfo.xt

    val text_length_max = width / (height / i)
    val count_letter = countHorizontalSourceSize(g, source)
    if(count_letter <= text_length_max * i){
        return i
      }
    else{
        countHorizontalLinesRecursion(g, source, targetAreaInfo, i + 1)
      }
  }
  /** Count how many lines needed for Mutable Vertical Write */
  def countHorizontalLines(g: Graphics2D,source: String, targetAreaInfo: TargetAreaInfo): Int = {
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
      return 1
    }
    val y_bottom = if(targetAreaInfo.yt == targetAreaInfo.yb || targetAreaInfo.yb - targetAreaInfo.yt < 2){WidthDefault + targetAreaInfo.yt}else{targetAreaInfo.yb}

    var lines = 0
    val height = if((y_bottom - targetAreaInfo.yt).abs != 0) (y_bottom - targetAreaInfo.yt).abs else 2
    val width = if((x_bottom - targetAreaInfo.xt).abs != 0) (x_bottom - targetAreaInfo.xt).abs else 2

    /** Counting letter */
    var count_letter = countHorizontalSourceSize(g, source)
    
    val letter_numbers = if(count_letter.ceil.toInt != 0) count_letter.ceil.toInt else 2
    val letter_numbers_max = width / (height / 2)
    // println(count_letter, source.length, width)
    
    // write on one line
    if(letter_numbers <= letter_numbers_max){
      lines = 1
    }
    else{
      lines = countHorizontalLinesRecursion(g, source, targetAreaInfo)
      }

    val line_numbers = if(count_letter.ceil.toInt <= (lines - 1) * (width / (height / lines))) lines - 1 else lines
    return line_numbers
  }
}
