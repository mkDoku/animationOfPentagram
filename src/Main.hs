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

main :: IO ()
main = reanimate 
  $ docEnv
  -- change viewBox (ranges) 
  --  to 
  --    x=[-1,5,..,1.5]
  --    y=[-1,5,..,1.5]
  --
  -- use ratio to stretch result to fit the 16/9 ratio of my display
  $ mapA (withViewBox (-1.5*ratio, -1.5, 3.0*ratio, 3.0)) 
  $ scene 
  $ do
    newSpriteSVG_ (withStrokeWidth 0.001 $ mkCircle 1)
    dotAngle  <- newVar $ coordEval 0
    newSprite_ $ redDot <$> unVar dotAngle

    let rotateDot a b = do
        -- writeVar dotAngle $ coordEval a
        tweenVar dotAngle 5 $ \_ t-> coordEval ( ( t*(b-a) )+a) 
        wait 1
    
    rotateDot 0 72
    rotateDot 72 144
    rotateDot 144 216
    rotateDot 216 288
    rotateDot 288 360

coordEval val = V2 x y
  where (x, y) = fromPolarU val

coord1 = coordEval 0
coord2 = coordEval 72
coord3 = coordEval 144
coord4 = coordEval 216
coord5 = coordEval 288

redDot :: V2 Double -> SVG
redDot (V2 x y) = translate x y $ mkGroup
  [ translate 0 0.2 $ scale 0.2 $ outlinedText $ T.pack $ printf "%.1f,%.1f" x y
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
