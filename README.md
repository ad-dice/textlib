textlib
=======

  /** example of imMutable */
  generate(
    Background("B"),
    
    //Source("Byambasuren"),
    Source("Byambasuren Ganbaatar UEC "),

    TargetAreaInfo(300, 300, 900, 500, 24),

    /** vertical-TOP write */
    //DrawFromKB("top", "left"),
    //DrawFromKB("top", "right"),
    //DrawFromKB("top", "center-left"),
    //DrawFromKB("top", "center-right"),

    /** vertical-BOTTOM write */
    //DrawFromKB("bottom", "left"),
    //DrawFromKB("bottom", "right"),
    //DrawFromKB("bottom", "center-left"),
    //DrawFromKB("bottom", "center-right"),

    /** vertical-CENTER write */
    //DrawFromKB("center", "left"),
    //DrawFromKB("center", "right"),
    //DrawFromKB("center", "center-left"),
    DrawFromKB("center", "center-right"),

    setFontInfo
  )
