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

  def rotationTranslationUpChars = Set('（')
  def rotationTranslationDownChars = Set('）')
  def rotationChars = Set('(', ')', '[',  ']', '「', '」', 'ー')
  def translationChars = Set(',', '.', '，', '。', '.')
  def translationSmallChars = Set('ゃ', 'ょ', 'ゅ', 'っ', 'ぁ', 'ぇ', 'ぃ', 'ぉ', 'ぅ',
                                  'ャ', 'ョ', 'ュ', 'ッ', 'ァ', 'ェ', 'ィ', 'ォ', 'ゥ')

  
  def writeVertical(g: Graphics2D, text: String, size: Int, x: Int, y: Int, i: Int = 0) {
    val font = createFont(size)
    if (text.nonEmpty) {
      val char = text.head
      // aspect ratio of a character
      val r = size / g.getFontMetrics(font).stringWidth(char.toString)

      if (rotationTranslationUpChars(char)) {
        val transform = AffineTransform.getRotateInstance(Math.toRadians(90), 0, 0)
        g.setFont(font.deriveFont(transform))
        val gap_y = -size / r - size/2
        val gap_x = size / 8
        g.drawString(char.toString, x + gap_x, y + (i + 1) * size + gap_y)
        writeVertical(g, text.tail, size, x, y - 3 * size / 4, i + 1) //見栄えが悪い隙間ができるので上にずらす 
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
        // single-byte characters move than the double-byte characters.
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
