module Graphics.Style exposing
    ( Style
    , classic
    , modified
    , square
    )

import Graphics.Colors as Colors
import Graphics.Layout as Layout
import Graphics.Shapes as Shapes


type alias Style =
    { colors : Colors.Scheme
    , shapes : Shapes.Shapes
    , layout : Layout.Layout
    }


classic : Style
classic =
    { colors = Colors.classic
    , shapes = Shapes.classic
    , layout = Layout.card
    }


modified : Style
modified =
    { colors = Colors.play
    , shapes = Shapes.variant
    , layout = Layout.card
    }


square : Style
square =
    { colors = Colors.playv
    , shapes = Shapes.squared
    , layout = Layout.square
    }
