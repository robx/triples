module Graphics.Colors exposing
    ( Scheme
    , classic
    , play
    , playv
    )


colors =
    -- classic set card colors
    { classicRed = "rgb(229,46,37)"
    , classicGreen = "rgb(72,128,52)"
    , classicPurple = "rgb(116,44,177)"

    -- playroom palette from colourlovers.com
    , playGreen = "rgb(5,135,137)"
    , playDark = "rgb(80,61,46)"
    , playRed = "rgb(200,64,34)"
    , playYellow = "rgb(227,167,47)"
    , playPale = "rgb(240,236,201)"

    -- playroom variants
    , lighterYellow = "rgb(233,167,67)"
    , lighterPale = "rgb(246,242,231)"

    -- standard colors
    , black = "black"
    , white = "white"
    , orange = "orange"
    , slategray = "slategray"

    -- a palette from lolcolors
    , lolBlue = "#30a9de"
    , lolYellow = "#efdc05"
    , lolRed = "#e53a40"
    , lolBlack = "#090707"

    -- from yeun/open-color
    , openTeal8 = "rgb(9,146,10)"
    , openYellow8 = "rgb(232,89,12)"
    , openGrape8 = "rgb(156,54,181)"
    , openGray8 = "rgb(52,58,64)"
    , openGray5 = "rgb(173,181,189)"
    , openGray2 = "rgb(233,236,239)"
    , openGray1 = "rgb(241,243,245)"
    , openLime5 = "rgb(148,216,45)"
    , openPink7 = "#d6336c"
    }


type alias Scheme =
    { symbols : ( String, String, String ) -- game colors
    , foreground : String -- card foreground (border, symbols)
    , background : String -- card background
    , select : String -- card selection highlight
    , table : String -- table color
    }


classic : Scheme
classic =
    { symbols = ( colors.classicRed, colors.classicGreen, colors.classicPurple )
    , foreground = colors.black
    , background = colors.white
    , table = colors.white
    , select = colors.orange
    }


play : Scheme
play =
    { symbols = ( colors.playGreen, colors.playRed, colors.lighterYellow )
    , foreground = colors.playDark
    , background = colors.lighterPale
    , table = colors.slategray
    , select = colors.orange
    }


playv : Scheme
playv =
    { symbols = ( colors.playGreen, colors.openPink7, colors.lighterYellow )
    , foreground = colors.playDark
    , background = colors.lighterPale
    , table = colors.slategray
    , select = colors.orange
    }


lol : Scheme
lol =
    { symbols = ( colors.lolBlue, colors.lolRed, colors.lolYellow )
    , foreground = colors.lolBlack
    , background = colors.white
    , table = colors.playPale
    , select = colors.orange
    }


open : Scheme
open =
    { symbols = ( colors.openTeal8, colors.openYellow8, colors.openGrape8 )
    , foreground = colors.openGray8
    , background = colors.openGray1
    , table = colors.openGray5
    , select = colors.openGray8
    }
