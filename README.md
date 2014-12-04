textlib
=======

Introduction
------

Add the folloing one line to build definition.
```
libraryDependencies ++= Seq(
  "com.ad_dice.textlib" %% "textlib" % "0.2.4"
)
```

How to use this package
------
```
def generate(background: Background, source: String, targetAreaInfo: TargetAreaInfo, alignVertical:Vertical, alignHorizontal: Horizontal, setFontInfo: String) {
  val backImage = ImageIO.read(new File(backImagePath(background)))
  val image = new BufferedImage(backImage.getWidth, backImage.getHeight, BufferedImage.TYPE_INT_ARGB)
  val g = image.createGraphics.asInstanceOf[Graphics2D] 
  
    // antialiasing
    g.setRenderingHint(
      RenderingHints.KEY_TEXT_ANTIALIASING,
      RenderingHints.VALUE_TEXT_ANTIALIAS_ON)

    g.drawImage(backImage, 0, 0, null)
    
    g.setColor(Color.WHITE)
    g.fillRect(targetAreaInfo.xt, targetAreaInfo.yt, targetAreaInfo.xb - targetAreaInfo.xt, targetAreaInfo.yb - targetAreaInfo.yt)

    g.setColor(classOf[Color].getField(setFontInfo.toUpperCase).get(null).asInstanceOf[Color])


    /** For immutable VERTICAL */
    immutableVerticalWrite(g, source, targetAreaInfo, alignVertical, alignHorizontal)

    /** For immutable HORIZONTAL */
    //immutableHorizontalWrite(g, source, targetAreaInfo, alignVertical, alignHorizontal)

    
    /** For mutable Vertical */
    //mutableVerticalWrite(g, source, targetAreaInfo, alignVertical, alignHorizontal)

    /** For mutable Horizontal */
    //mutableHorizontalWrite(g, source, targetAreaInfo, alignVertical, alignHorizontal)

    ImageIO.write(image, "PNG", new File(output))
    g.dispose()
  }

  generate(
    Background("B"),
    "パターン認識ビャムバスレン教科書情報制御システム",
    //TargetAreaInfo(1415, 166, 1435, 186, 20),
    TargetAreaInfo(1415, 166, 1415, 553, 20),
    //TargetAreaInfo(300, 400, 600, 410, 0),
    // TargetAreaInfo(400, 300, 600, 900, 24),
    //TargetAreaInfo(300, 300, 900, 500, 24),
    Vertical.CenterBottom,
    Horizontal.CenterRight,
    setFontInfo
    )
```
