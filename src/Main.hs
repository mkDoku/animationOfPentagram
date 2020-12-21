#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE OverloadedStrings #-}
module Main ( main ) where

import           Reanimate
import           Reanimate.Builtin.Documentation
import           Reanimate.Transition

import           Geom2D.CubicBezier.Linear
import           Linear.Metric
import           Linear.V2
import           Linear.Vector

import           Data.Text                       (Text)
import qualified Data.Text                       as T
import           Text.Printf

import           Graphics.SvgTree                hiding (Text, height)


ratio = 16/9

newWidth, newHeight :: Double
newWidth = 8
newHeight = 8

main :: IO ()
main = reanimate $ docEnv
 $ mapA (withViewBox (-1.5, -1.5, 3.0, 3.0)) $ scene $ do
--  newSpriteSVG_ $ mkBackgroundPixel rtfdBackgroundColor
--  newSpriteSVG_ static
  dotPath  <- newVar (QuadBezier coord1 coord1 coord1)
  dotParam <- newVar 0
  newSprite_ $ redDot <$> (evalBezier <$> unVar dotPath <*> unVar dotParam)
  let moveDot a b = do
        pos <- evalBezier <$> readVar dotPath <*> pure 1
        writeVar dotPath $ QuadBezier pos a b
        writeVar dotParam 0
        tweenVar dotParam 5 $ \v -> fromToS v 1 . curveS 2
        wait 1
  wait 1
  moveDot coord1 coord2
  moveDot coord2 coord3
  moveDot coord3 coord4
  moveDot coord4 coord5
  moveDot coord5 coord1

coord1 = V2 1 0
coord2 = V2 x y
  where (x, y) = fromPolarU 72
coord3 = V2 x y
  where (x, y) = fromPolarU 144
coord4 = V2 x y
  where (x, y) = fromPolarU 216
coord5 = V2 x y
  where (x, y) = fromPolarU 288

--main :: IO ()
--main = reanimate $ mapA squareViewBox $ scene $ do
--  newSpriteSVG_ $ mkBackgroundPixel rtfdBackgroundColor
--  newSpriteSVG_ static
--  dotPath <- newVar (QuadBezier (V2 0 0) (V2 0 0) (V2 0 0))
--  dotParam <- newVar 0
--  newSprite_ $ redDot <$> (evalBezier <$> unVar dotPath <*> unVar dotParam)

--  let moveDot a b = do
--      pos <- evalBezier <$> readVar dotPath <*> pure 1
--      writeVar dotPath $ QuadBezier pos a b
--      writeVar dotParam 0
--      tweenVar dotParam 5 $ \v -> fromToS v 1 . curveS 2
--      wait 1
--
--  moveDot (V2 0 1)     (V2 2 1)
--  moveDot (V2 (-1) 0)  (V2 (-1) 1.5)
--  moveDot (V2 0 0)   (V2 (-2) (-3))
--  moveDot (V2 0 0)   (V2 2.5 (-1))
--  moveDot (V2 3 1.5) (V2 1.5 1)
--  moveDot (V2 0 0)   (V2 0 0)

-- reanimate
--  $ docEnv
-- $ mapA ( withViewBox (-1.5*ratio)
--                     , -1.5
--                     , (3*ratio)
--                     , 3 )

--squareViewBox :: SVG -> SVG
--squareViewBox = withViewBox (-4, -4, 8, 8)
--
--static :: SVG
--static = mkGroup
--  [ grid
--  , withStrokeColor "grey" $ mkLine (-screenWidth, 0) (screenWidth, 0)
--  , withStrokeColor "grey" $ mkLine (0, -screenHeight) (0, screenHeight)
--  , curlyBracket (V2 (-newWidth / 2 + defaultStrokeWidth) (newHeight / 2))
--                 (V2 (newWidth / 2 - defaultStrokeWidth) (newHeight / 2))
--                 1
--  , translate 0 2.5 $ outlinedText "8 units"
--  , curlyBracket (V2 (-newWidth / 2) (-newHeight / 2 + defaultStrokeWidth))
--                 (V2 (-newWidth / 2) (newHeight / 2 - defaultStrokeWidth))
--                 1
--  , translate (-2.5) 0 $ rotate 90 $ outlinedText "8 units"
--  ]
--
--curlyBracket :: RPoint -> RPoint -> Double -> SVG
--curlyBracket from to height =
--  withStrokeColor "black"
--    $ withFillOpacity 0
--    $ withStrokeWidth (defaultStrokeWidth * 2)
--    $ mkPath
--        [ MoveTo OriginAbsolute [from]
--        , CurveTo OriginAbsolute [(from + outwards, halfway, halfway + outwards)]
--        , CurveTo OriginAbsolute [(halfway, to + outwards, to)]
--        ]
-- where
--  outwards = case normalize (from - to) ^* height of
--    V2 x y -> V2 (-y) x
--  halfway = lerp 0.5 from to
--
--grid :: SVG
--grid = withStrokeColor "grey" $ withStrokeWidth (defaultStrokeWidth * 0.5) $ mkGroup
--  [ mkGroup
--    [ translate
--          0
--          (i / screenHeight * screenHeight - screenHeight / 2 - screenHeight / 18)
--        $ mkLine (-screenWidth, 0) (screenWidth, 0)
--    | i <- [0 .. screenHeight]
--    ]
--  , mkGroup
--    [ translate (i / screenWidth * screenWidth - screenWidth / 2) 0
--        $ mkLine (0, -screenHeight) (0, screenHeight)
--    | i <- [0 .. screenWidth]
--    ]
--  ]

redDot :: V2 Double -> SVG
redDot (V2 x y) = translate x y $ mkGroup
  [ translate 0 (-0.5) $ scale 0.2 $ outlinedText $ T.pack $ printf "%.3f,%.3f" x y
  , withFillColor "red" $ mkCircle 0.01
  ]

outlinedText :: Text -> SVG
outlinedText txt = mkGroup
  [ center
  $ withStrokeColorPixel rtfdBackgroundColor
  $ withStrokeWidth (defaultStrokeWidth * 8)
  $ withFillOpacity 0
  $ latex txt
  , center $ latex txt
  ]


circleWithDots =
    mapA (withViewBox ((-1.5*ratio),-1.5,(3*ratio),3))
  $ chainT andThen [
        drawDotAni 0
      ,  aniCircle 0
      ,  drawDotAni 72
      , aniCircle 72
      ,  drawDotAni 144
      , aniCircle 144
      ,  drawDotAni 216
      , aniCircle 216
      ,  drawDotAni 288
      , aniCircle 288
                     ]

drawDotAni :: Double -> Animation
drawDotAni r = staticFrame 1 translatedCircle
  where
    translatedCircle = translate coordX coordY (mkCircle 0.01)
    (coordX, coordY) = fromPolarU r

aniCircle :: Double -> Animation
aniCircle r = signalA (fromToS 0.0 0.2) $ rotatedUnitCircle r

scence01 :: Scene s ()
scence01 = do
  play $ (signalA (fromToS 0.0 0.2) $ rotatedUnitCircle 0)
  wait 1

scence02 :: Scene s ()
scence02 = do
  play $ (signalA (fromToS 0.0 0.2) $ rotatedUnitCircle (-72))
  wait 1

meinScene :: Scene s ()
meinScene = do
  newSpriteSVG $ staticStar
  -- fork $ play (staticFrame 3 staticStar)
  fork $ play $
    andThen
      (signalA (fromToS 0.0 0.2) $ rotatedUnitCircle 0)
      (signalA (fromToS 0.0 0.2) $ rotatedUnitCircle 72)
  wait 1
  -- draw point
  wait 1
  -- continue drawing


--  $ staticFrame 1
--  $ staticStar
--  $ pauseAtEnd 1
--  $ rotatingStar

customScaling = 1

unitCircle :: Animation
unitCircle =
  mkAnimation 3
--    staticFrame 3
  $ \t ->
    partialSvg t
    $ pathify
--    $ scaleToHeight 1
      $ scaleToHeight (customScaling * 1.05)
--    $ center
      $ withStrokeWidth 0.001
      $ rotate 0
      $ flipXAxis
      $ mkCircle 1

rotatedUnitCircle :: Double -> Animation
rotatedUnitCircle r =
  mkAnimation 3
--    staticFrame 3
  $ \t ->
    partialSvg t
    $ pathify
  --    $ scaleToHeight (customScaling * 1.05)
      $ withStrokeWidth 0.01
      $ rotate r
      $ flipXAxis
      $ mkCircle 1

rotatingStar :: Animation
rotatingStar =
  mkAnimation 3
  $ \t ->
      rotate (360 * t)
--    $ center
--    $ scaleToHeight 1
--    $ scaleToHeight customScaling
    $ staticStar

-- A static 'SVG' by using 'mkLinePathClosed'
--
-- with 'mkLinePathClosed' the last point will
-- be connected to the first point
staticStar :: SVG
staticStar =
  withStrokeWidth 0.001
  $ scaleToHeight customScaling
  $ mkLinePathClosed coordsInCartesianU

fromDegrees :: Floating a => a -> a
fromDegrees deg = deg * pi / 180

fromPolar :: Floating a => a -> a -> (a, a)
fromPolar r phi = (x, y)
  where
    x = r * cos( fromDegrees phi )
    y = r * sin( fromDegrees phi )

fromPolarU :: Floating a => a -> (a, a)
fromPolarU = fromPolar 1

coordsInPolar = [0, 144, 288, 72, 216]

coordsInCartesianU = fmap fromPolarU coordsInPolar
