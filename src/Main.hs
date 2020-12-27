#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE OverloadedStrings #-}
module Main ( main ) where

import           Reanimate
import           Reanimate.Builtin.Documentation
import           Reanimate.Transition
import Reanimate.Animation ( Sync(..) )

import           Geom2D.CubicBezier.Linear
import           Linear.Metric
import           Linear.V2
import           Linear.Vector

import           Data.Text                       (Text)
import qualified Data.Text                       as T
import           Text.Printf

import           Graphics.SvgTree                hiding (Text, height)


ratio = 16/9

xmin = 2.0
ymin = 2.0

main :: IO ()
main = reanimate
  $ docEnv
  -- change viewBox (ranges)
  --  to
  --    x=[-1,5,..,1.5]
  --    y=[-1,5,..,1.5]
  --
  -- use ratio to stretch result to fit the 16/9 ratio of my display
  $ mapA (withViewBox (-xmin*ratio, -ymin, xmin*2*ratio, ymin*2))
  $ scene
  $ do
    newSpriteSVG_ $ mkBackgroundPixel rtfdBackgroundColor
    
    -- Create unit circle (r = 1.0)
    newSpriteSVG_ (withStrokeWidth 0.01 $ withStrokeColor "blue" $ mkCircle 1)
    
    -- Create variable for angle in polar coordinates
    dotAngle  <- newVar 0
    
    -- Create dot, line and text for constant angle of 0
    newSpriteSVG_ $ angleLine $ coordEval 0
   -- newSpriteSVG_ $ mkDot "black" $ coordEval 0
   -- newSpriteSVG_ $ coordText $ coordEval 0
    
    -- Create dot, line and text for variable angle
    newSprite_ $ angleLine . coordEval <$> unVar dotAngle
    redDot <- newSprite $ mkDot "red" . coordEval <$> unVar dotAngle
    spriteZ redDot 1
    newSprite_ $ angleText <$> unVar dotAngle
    newSprite_ $ coordText . coordEval <$> unVar dotAngle

--    newSprite_ $ undefined . coordEval <$> unVar dotAngle

    wait 1

    let rotateDot a b = do
        -- writeVar dotAngle $ coordEval a
        -- tweenVar dotAngle 5 $ \_ t-> coordEval ( ( t*(b-a) )+a)
        fork $ tweenVar dotAngle 5 $ \_ t-> t*(b-a)+a
        -- newSpriteA and SyncStretch would stretch the circle to the
        -- life time of the whole scene
        newSpriteA' SyncFreeze $ dymCircle a 0 (72/360)
        
        newDot <- newVar $ coordEval b
        newSprite_ $ mkDot "black" <$> unVar newDot
        newSprite_ $ coordText <$> unVar newDot
        
        wait 1

    rotateDot 0 72
    rotateDot 72 144
    rotateDot 144 216
    rotateDot 216 288
    -- destroySprite dymCoordText
    rotateDot 288 360
    destroySprite redDot
    wait 2

data LatexTable = LatexTable [LatexEntry]

data LatexEntry = LatexEntry Double (Double, Double)

-- dymCircle :: Double -> (Frame s Double) -> Animation
dymCircle r t1 t2 = signalA (fromToS t1 t2) $ rotatedCircle r 0.4 5.0

rotatedCircle :: Double -> Double -> Time -> Animation
rotatedCircle r size dur =
  mkAnimation dur
--    staticFrame 3
  $ \t ->
    partialSvg t
    $ pathify
  --    $ scaleToHeight (customScaling * 1.05)
      $ withStrokeWidth 0.01
      $ rotate r
      $ flipXAxis
      $ mkCircle size

coordEval val = V2 x y
  where (x, y) = fromPolarU val

mkDot :: String -> V2 Double -> SVG
mkDot color (V2 x y) = translate x y 
  $   withStrokeWidth 0.01 $ withFillOpacity 1 $ withStrokeColor "blue" $ withFillColor color $ mkCircle 0.03

coordText (V2 _ 360) = outlinedText $ T.pack $ printf ""
coordText (V2 x y) = translate (x*1.5) (y*1.3) $ center $ scale 0.15 $ outlinedText $ T.pack $ printf "(%.3f,%.3f)" x y

angleLine :: V2 Double -> SVG
angleLine (V2 x y) = translate x y $ mkGroup
  [ withStrokeWidth 0.01 $ mkLine (0,0) (-x,-y)
--  , translate (-x*0.75) (-y*0.75) $ withStrokeWidth 0.01 $ mkCircle 0.01 
  ]
outlinedText :: Text -> SVG
outlinedText txt = mkGroup
  [ center
   $ withFillColor "black"
   $ withFillOpacity 1.0
  $ latex txt
  , center $ latex txt
  ]

angleText :: Double -> SVG
angleText angle = translate 0.2 0.1 $ scale 0.15 
  $ center 
  $ withFillColor "black"
  $ withFillOpacity 1.0
  $ latex 
  $ T.pack
  $ printf "%3.0f\\degree" angle


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

fromDegrees :: Floating a => a -> a
fromDegrees deg = deg * pi / 180

fromPolar :: Floating a => a -> a -> (a, a)
fromPolar r phi = (x, y)
  where
    x = r * cos( fromDegrees phi )
    y = r * sin( fromDegrees phi )

fromPolarU :: Floating a => a -> (a, a)
fromPolarU = fromPolar 1
