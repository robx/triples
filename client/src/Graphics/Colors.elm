module Graphics.Colors exposing (..)


colors =
    -- classic set card colors
    { classicRed = "rgb(229,46,37)"
    , classicGreen = "rgb(72,128,52)"
    , classicPurple = "rgb(116,44,177)"

    -- playroom palette from colourlovers.com
    , playGreen = "rgb(5,135,137"
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
