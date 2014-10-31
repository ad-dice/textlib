package com.ad_dice.textlib

import java.awt.{Image, Color, Font, Dimension, Graphics2D, RenderingHints}
import java.awt.geom.AffineTransform
import java.awt.image.BufferedImage
import java.awt.font.FontRenderContext
import java.io.File
import javax.imageio.ImageIO

object TextLib extends App {

  def fontPath = "src/main/resources/mincho.ttf"

  def createFont(size: Float) = Font.createFont(Font.TRUETYPE_FONT, new File(fontPath)).deriveFont(size)

    /**  definitions for vertical writing */
  def rotationTranslationUpChars = Set('（')
  def rotationTranslationDownChars = Set('）')
  def rotationChars = Set('(', ')', '[',  ']', '「', '」', 'ー')
  def translationChars = Set(',', '.', '，', '。', '.')
  def translationSmallChars = Set('ゃ', 'ょ', 'ゅ', 'っ', 'ぁ', 'ぇ', 'ぃ', 'ぉ', 'ぅ',
                                  'ャ', 'ョ', 'ュ', 'ッ', 'ァ', 'ェ', 'ィ', 'ォ', 'ゥ')
  def halfwidthAlphabet = Set('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 
    'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', 
    'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
    'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', 
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9','%', '$', '¥')
  
    /** write strings by vertical */
  def writeVertical(g: Graphics2D, text: String, size: Int, x: Int, y: Int, i: Int = 0) {
    val font = createFont(size)
    if (text.nonEmpty) {
      val char = text.head
      val r = size / g.getFontMetrics(font).stringWidth(char.toString)

      if (rotationTranslationUpChars(char)) {
        val transform = AffineTransform.getRotateInstance(Math.toRadians(90), 0, 0)
        g.setFont(font.deriveFont(transform))
        val gap_y = -size / r - size/2
        val gap_x = size / 8
        g.drawString(char.toString, x + gap_x, y + (i + 1) * size + gap_y)
        writeVertical(g, text.tail, size, x, y - 3 * size / 4, i + 1) 
      }
      else if (rotationTranslationDownChars(char)) {
        val transform = AffineTransform.getRotateInstance(Math.toRadians(90), 0, 0)
        g.setFont(font.deriveFont(transform))
        val gap_y = -size / r
        val gap_x = size / 8
        g.drawString(char.toString, x + gap_x, y + (i + 1) * size + gap_y)
        writeVertical(g, text.tail, size, x, y - 2 * size / 4, i + 1)
      }

      else if (rotationChars(char)) {
        val transform = AffineTransform.getRotateInstance(Math.toRadians(90), 0, 0)
        g.setFont(font.deriveFont(transform))
        val gap_y = -size / r
        val gap_x = size / 8
        g.drawString(char.toString, x + gap_x, y + (i + 1) * size + gap_y)
        writeVertical(g, text.tail, size, x, y, i + 1)
      }


      else if (translationChars(char)) {
        val transform = AffineTransform.getTranslateInstance(size / 4 * 3 , -size / 4 * 3)
        g.setFont(font.deriveFont(transform))
        g.drawString(char.toString, x, y + (i + 1) * size)
        writeVertical(g, text.tail, size, x, y - size / 4 * 3, i + 1)
      }

      else if (translationSmallChars(char)) {
        val transform = AffineTransform.getTranslateInstance(size / 5 , 0)
        g.setFont(font.deriveFont(transform))
        g.drawString(char.toString, x, y + (i + 1) * size)
        writeVertical(g, text.tail, size, x, y, i + 1)
      }

      else if (halfwidthAlphabet(char)){
        val transform = AffineTransform.getTranslateInstance(size / 4 , 0)
        g.setFont(font.deriveFont(transform))
        g.drawString(char.toString, x, y + (i + 1) * size)
        writeVertical(g, text.tail, size, x, y, i + 1) 
      }  

      else {
        g.setFont(font)
        if(r == 2){
          g.drawString(char.toString, x + size/4, y + (i + 1) * size)
        }
        else {
          g.drawString(char.toString, x, y + (i + 1) * size)
        }
        writeVertical(g, text.tail, size, x, y, i + 1)
      }
    }
  }

  /** write strings by vertical type and convert to image file
   *  for Vertical write
   *  @param allignHorizontal  value range --> "left", "center-left", "center-right", "right") center-* determines 2nd line starts from which side 
   *  @param allignVertical    value range --> "top", "center", "bottom") "center-*" is dealt as "center"
   */

  def immutable(g: Graphics2D, source: Source, targetAreaInfo: TargetAreaInfo, allignVertical: String, allignHorizontal: String){
    val letter_size = source.text.length()
    val text_length = targetAreaInfo.size * letter_size
    val width = targetAreaInfo.xb - targetAreaInfo.xt
    val height = targetAreaInfo.yb - targetAreaInfo.yt

    println(text_length, width, height)

    if(allignVertical == "top"){
      val line_letter_size = height / targetAreaInfo.size
      val text_grouped_list = source.text.grouped(line_letter_size).toList
           
      /** start write left side */
      if(allignHorizontal == "left"){
        if(text_length <= height){

          writeVertical(g, source.text, targetAreaInfo.size, targetAreaInfo.xt, targetAreaInfo.yt)
        }
        else{
          for(i <- 0 to text_grouped_list.size - 1)
              writeVertical(g, text_grouped_list(i), targetAreaInfo.size, targetAreaInfo.xt + targetAreaInfo.size * i, targetAreaInfo.yt)
        }
      }

      /** start write right side */
      if(allignHorizontal == "right"){
        if(text_length <= height){
          writeVertical(g, source.text, targetAreaInfo.size, targetAreaInfo.xb - targetAreaInfo.size, targetAreaInfo.yt)
        }
        else{
          for(i <- 0 to text_grouped_list.size - 1)
              writeVertical(g, text_grouped_list(i), targetAreaInfo.size, targetAreaInfo.xb - targetAreaInfo.size * (i + 1), targetAreaInfo.yt)
        }
      }

      /** start write center-left side */
      if(allignHorizontal == "center-left"){
        val mid = (targetAreaInfo.xt + targetAreaInfo.xb) / 2
        if(text_length <= height){
          writeVertical(g, source.text, targetAreaInfo.size, mid, targetAreaInfo.yt)
        }
        else{
          val gap = text_grouped_list.size / 2 * targetAreaInfo.size
          for(i <- 0 to text_grouped_list.size - 1)
              writeVertical(g, text_grouped_list(i), targetAreaInfo.size, mid - gap + targetAreaInfo.size * i, targetAreaInfo.yt)
        }
      }

      /** start write center-left side */
      if(allignHorizontal == "center-right"){
        val mid = (targetAreaInfo.xt + targetAreaInfo.xb) / 2
        if(text_length <= height){
          writeVertical(g, source.text, targetAreaInfo.size, mid, targetAreaInfo.yt)
        }
        else{
          val gap = text_grouped_list.size / 2 * targetAreaInfo.size
          for(i <- 0 to text_grouped_list.size - 1)
              writeVertical(g, text_grouped_list(i), targetAreaInfo.size, mid + gap - targetAreaInfo.size * i, targetAreaInfo.yt)
        }
      }
    }


    if(allignVertical == "bottom"){
      val line_letter_size = height / targetAreaInfo.size
      val reversed_text = source.text.reverse
      val text_grouped_list = reversed_text.grouped(line_letter_size).toList

      if(allignHorizontal == "left"){
        if(text_length <= height){
          writeVertical(g, source.text, targetAreaInfo.size, targetAreaInfo.xt, targetAreaInfo.yb - text_length)
        }
        else{
          for(i <- 0 to text_grouped_list.size - 1)
              writeVertical(g, text_grouped_list(i).reverse, targetAreaInfo.size, targetAreaInfo.xt + targetAreaInfo.size * i,
                targetAreaInfo.yb - text_grouped_list(i).size * targetAreaInfo.size)
        }
      }

      else if(allignHorizontal == "right"){
        if(text_length <= height){
          writeVertical(g, source.text, targetAreaInfo.size, targetAreaInfo.xb - targetAreaInfo.size, targetAreaInfo.yb - text_length)
        }
        else{
          for(i <- 0 to text_grouped_list.size - 1)
              writeVertical(g, text_grouped_list(i).reverse, targetAreaInfo.size, targetAreaInfo.xb - targetAreaInfo.size * (i + 1),
                targetAreaInfo.yb - text_grouped_list(i).size * targetAreaInfo.size)
        }
      }

      else if(allignHorizontal == "center-left"){
        if(text_length <= height){
          writeVertical(g, source.text, targetAreaInfo.size, (targetAreaInfo.xt + targetAreaInfo.xb) / 2, targetAreaInfo.yb - text_length)
        }
        else{
          val gap = text_grouped_list.size / 2 * targetAreaInfo.size

          for(i <- 0 to text_grouped_list.size - 1)
              writeVertical(g, text_grouped_list(i).reverse, targetAreaInfo.size, 
                (targetAreaInfo.xt + targetAreaInfo.xb) / 2 - gap + targetAreaInfo.size * (i + 1),
                targetAreaInfo.yb - text_grouped_list(i).size * targetAreaInfo.size)
        }
      }

      else if(allignHorizontal == "center-right"){
        if(text_length <= height){
          writeVertical(g, source.text, targetAreaInfo.size, (targetAreaInfo.xt + targetAreaInfo.xb) / 2, targetAreaInfo.yb - text_length)
        }
        else{
          val gap = text_grouped_list.size / 2 * targetAreaInfo.size

          for(i <- 0 to text_grouped_list.size - 1)
              writeVertical(g, text_grouped_list(i).reverse, targetAreaInfo.size, 
                (targetAreaInfo.xt + targetAreaInfo.xb) / 2 + gap - targetAreaInfo.size * (i + 1),
                targetAreaInfo.yb - text_grouped_list(i).size * targetAreaInfo.size)
        }
      }

    }

    if(allignVertical == "center"){
      val line_letter_size = height / targetAreaInfo.size
      val text_grouped_list = source.text.grouped(line_letter_size).toList

      if(allignHorizontal == "left"){
        if(text_length <= height){
          val gap = source.text.size / 2 * targetAreaInfo.size
          writeVertical(g, source.text, targetAreaInfo.size, targetAreaInfo.xt, 
            (targetAreaInfo.yt + targetAreaInfo.yb) / 2 - gap)
        }
        else{
          for(i <- 0 to text_grouped_list.size - 1){
              val gap = text_grouped_list(i).size / 2 * targetAreaInfo.size
              writeVertical(g, text_grouped_list(i), targetAreaInfo.size, targetAreaInfo.xt + targetAreaInfo.size * i, 
                (targetAreaInfo.yt + targetAreaInfo.yb) / 2 - gap )
            }
        }
      }

      else if(allignHorizontal == "right"){
        if(text_length <= height){
          val gap = source.text.size / 2 * targetAreaInfo.size
          writeVertical(g, source.text, targetAreaInfo.size, targetAreaInfo.xb - targetAreaInfo.size, 
            (targetAreaInfo.yt + targetAreaInfo.yb) / 2 - gap)
        }
        else{
          for(i <- 0 to text_grouped_list.size - 1){
              val gap = text_grouped_list(i).size / 2 * targetAreaInfo.size
              writeVertical(g, text_grouped_list(i), targetAreaInfo.size, targetAreaInfo.xb - targetAreaInfo.size * (i + 1), 
                (targetAreaInfo.yt + targetAreaInfo.yb) / 2 - gap )
            }
        }
      }

      else if(allignHorizontal == "center-left"){
        if(text_length <= height){
          val gap = source.text.size / 2 * targetAreaInfo.size
          writeVertical(g, source.text, targetAreaInfo.size, (targetAreaInfo.xt + targetAreaInfo.xb)/2, 
            (targetAreaInfo.yt + targetAreaInfo.yb) / 2 - gap)
        }
        else{
          val _gap = text_grouped_list.size / 2 * targetAreaInfo.size
          for(i <- 0 to text_grouped_list.size - 1){
              val gap = text_grouped_list(i).size / 2 * targetAreaInfo.size
              writeVertical(g, text_grouped_list(i), targetAreaInfo.size, 
                (targetAreaInfo.xt + targetAreaInfo.xb)/2 - _gap + targetAreaInfo.size * i, 
                (targetAreaInfo.yt + targetAreaInfo.yb) / 2 - gap )
            }
        }
      }

      else if(allignHorizontal == "center-right"){
        if(text_length <= height){
          val gap = source.text.size / 2 * targetAreaInfo.size
          writeVertical(g, source.text, targetAreaInfo.size, (targetAreaInfo.xt + targetAreaInfo.xb)/2, 
            (targetAreaInfo.yt + targetAreaInfo.yb) / 2 - gap)
        }
        else{
          val _gap = text_grouped_list.size / 2 * targetAreaInfo.size
          for(i <- 0 to text_grouped_list.size - 1){
              val gap = text_grouped_list(i).size / 2 * targetAreaInfo.size
              writeVertical(g, text_grouped_list(i), targetAreaInfo.size, 
                (targetAreaInfo.xt + targetAreaInfo.xb)/2 + _gap - targetAreaInfo.size * i, 
                (targetAreaInfo.yt + targetAreaInfo.yb) / 2 - gap )
            }
        }
      }

    }

  }
  /**
    *  for Horizontal write (under construction)
    *  @param allignHorizontal  value range --> "left", "center", "right")  center-* is dealt as center
    *  @param allignVertical    value range --> "top", center-, bottom) center-* determines 2nd line starts from which side 
  */



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

  val text = "ここに縦書きの文章を書いてください。"

  showVerticalText(text, 12)
}
