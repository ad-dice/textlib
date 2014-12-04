textlib
=======

Introduction
------

Add the folloing one line to build definition.
```
libraryDependencies ++= Seq(
  "com.ad_dice.textlib" %% "textlib" % "0.2.6"
)
```

How to use this package
------
```
def generate(background: Background, source: String, x1: Int, y1: Int, x2: Int, y2: Int, fontSize: Int, 
  alignVertical:Vertical, alignHorizontal: Horizontal, setFontInfo: String) {
  
  val backImage = ImageIO.read(new File(backImagePath(background)))
  val image = new BufferedImage(backImage.getWidth, backImage.getHeight, BufferedImage.TYPE_INT_ARGB)
  val g = image.createGraphics.asInstanceOf[Graphics2D] 
  
    // antialiasing
    g.setRenderingHint(
      RenderingHints.KEY_TEXT_ANTIALIASING,
      RenderingHints.VALUE_TEXT_ANTIALIAS_ON)

    g.drawImage(backImage, 0, 0, null)
    
    g.setColor(Color.WHITE)
    g.fillRect(x1, y1, x2 - x1, y2 - y1)

    g.setColor(classOf[Color].getField(setFontInfo.toUpperCase).get(null).asInstanceOf[Color])


    /** For immutable VERTICAL */
    //println(alignVertical, alignHorizontal)
    //TextLib.immutableVerticalWrite(g, source, TargetAreaInfo(x1, y1, x2, y2, fontSize), alignVertical, alignHorizontal)

    /** For immutable HORIZONTAL */
    TextLib.immutableHorizontalWrite(g, source, TargetAreaInfo(x1, y1, x2, y2, fontSize), alignVertical, alignHorizontal)

    /** For mutable Vertical */
    //TextLib.mutableVerticalWrite(g, source, TargetAreaInfo(x1, y1, x2, y2, fontSize), alignVertical, alignHorizontal)

    /** For mutable Horizontal */
    //TextLib.mutableHorizontalWrite(g, source, TargetAreaInfo(x1, y1, x2, y2, fontSize), alignVertical, alignHorizontal)

    ImageIO.write(image, "PNG", new File(output))
    g.dispose()
  }

generate(
  Background("A"),
  "ビャムバスレン",
  100, 215, 290, 270, 52,
  Vertical.CenterTop,
  Horizontal.Left,
  setFontInfo
  )
```
