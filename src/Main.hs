#!/usr/bin/env stack
-- stack runghc --package reanimate
module Main ( main ) where

import           Reanimate
import           Reanimate.Builtin.Documentation


main :: IO ()
main = reanimate
  $ docEnv
--  $ staticFrame 1
--  $ staticStar
  $ pauseAtEnd 1
  $ rotatingStar


rotatingStar :: Animation
rotatingStar =
  mkAnimation 3
  $ \t ->
      rotate (360 * t)
    $ center
    $ scaleToHeight 4 staticStar

-- A static 'SVG' by using 'mkLinePathClosed'
--
-- with 'mkLinePathClosed' the last point will
-- be connected to the first point
staticStar :: SVG
staticStar =
  mkLinePathClosed coordsInCartesianU

fromDegrees :: Floating a => a -> a
fromDegrees deg = deg * pi / 180

fromPolar :: Floating a => a -> a -> (a, a)
fromPolar r phi = (x, y)
  where
    x = r * sin( fromDegrees phi )
    y = r * cos( fromDegrees phi )

fromPolarU :: Floating a => a -> (a, a)
fromPolarU = fromPolar 1

coordsInPolar = [0, 144, 288, 72, 216]

coordsInCartesianU = fmap fromPolarU coordsInPolar
