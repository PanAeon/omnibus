{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Main where

import Lib
import Codec.Picture
import Codec.Picture.Types
import qualified Data.ByteString.Lazy as L
import qualified Data.Vector.Storable as V
import Control.Monad(void)
import System.Process

greyScaleWitness :: Int -> Image Pixel8
greyScaleWitness offset = img 232 241
    where img w h = Image w h $ V.fromListN (w * h) $ pixels w h
          pixels w h = [pixel (x + offset) (y + offset)
                                | y <- [0 .. h-1], x <- [0 .. w-1] ]
          pixel x y =  truncate $ sqrt dist
                where xf = fromIntegral $ x - 100 :: Int
                      yf = fromIntegral $ y - 100
                      dist = (fromIntegral $ xf * xf + yf * yf) :: Double

imageCreator :: Image PixelRGB8
imageCreator  =  generateImage pixelRenderer 250 300
   where pixelRenderer x y = PixelRGB8 (fromIntegral x) (fromIntegral y) 128

wallpaper :: Image PixelRGB8
wallpaper  =  generateImage pixelRenderer 1027 1027
   where
     corna = 0 :: Float
     cornb = 0 :: Float
     side  = 32.0 :: Float
     pixelRenderer x y = let
                           x' = corna + (fromIntegral x) * (side / 1027.0)
                           y' = cornb + (fromIntegral y) * (side / 1027.0)
                           c  = round (x'*x' + y'*y')
                         in   colorIn3 c

colorInBlackAndWhite c = if even c
     then PixelRGB8 220 220 220
     else PixelRGB8 64   64   64

-- if even c
-- then PixelRGB8 8 255 255
-- else PixelRGB8 0   0   0

colorIn5 c = case c `mod` 5 of
     0 -> PixelRGB8 90 148 90
     1 -> PixelRGB8 148 90 90
     2 -> PixelRGB8 0 0 0
     3 -> PixelRGB8 255 255 255
     4 -> PixelRGB8 90 90 148

colorIn3 c =  case c `mod` 3 of
     0 -> PixelRGB8 0 255 0
     1 -> PixelRGB8 255 0 0
     2 -> PixelRGB8 0 0 255


-- exportBmpWitness :: IO ()
-- exportBmpWitness = writeBitmap "wintess.bmp" $ img 232 241
--     where img w h = array ((0,0), (w - 1, h - 1)) $ pixels w h
--           pixels w h = [((x,y), pixel x y) | y <- [0 .. h-1], x <- [0 .. w-1] ]
--           pixel x y = PixelRGBA8 128 (fromIntegral x) (fromIntegral y) 255
main :: IO ()
main = do
         writeBitmap "foo.bmp" $ (wallpaper)
         openViewer "foo.bmp"

openViewer :: String -> IO ()
openViewer path = void $ spawnCommand ("xdg-open " ++  path)
