module Main where

import qualified Data.ByteString as B

import Graphics.GD

white :: Color
white = rgb 255 255 255

black :: Color
black = rgb 0 0 0

baseQuad = 128
baseDim = baseQuad * 2
colorScale = floor $ 255 / (fromIntegral $ baseDim - 1)

baseImage :: IO Image
baseImage = do png <- newImage (baseDim, baseDim)
               mapM_ (\x -> drawLine (2*x, 0) (2*x, baseQuad - 1) white png) [0..baseQuad `div` 2]
               mapM_ (\x -> drawLine (baseQuad + 2*x, 0) (baseQuad + x, baseQuad - 1) white png) [0..baseQuad `div` 2]
               mapM_ (\r -> drawFilledEllipse (0, baseDim - 1) (4 * r, 4 * r) white png 
                            >> drawFilledEllipse (0, baseDim - 1) (4 * r - 2, 4 * r - 2) black png ) $ reverse [1..baseQuad `div` 2]
               mapM_ (\(x, y) -> setPixel (baseQuad + x, baseQuad + y) 
                                 (rgb (x * colorScale) (y * colorScale) 128) png) 
                         [(x, y) | x <- [0..baseDim - 1], y <- [0..baseDim - 1]]
               return png

main = do img <- baseImage
          savePngFile "base.png" img
          jpeg <- saveJpegByteString 70 img >>= loadJpegByteString
          png <- savePngByteString img >>= loadPngByteString
          gif <- saveGifByteString img >>= loadGifByteString
          comp <- newImage (baseDim * 2, baseDim * 2)
          copyRegion (0, 0) (baseDim, baseDim) img  (      0,       0) comp
          copyRegion (0, 0) (baseDim, baseDim) jpeg (      0, baseDim) comp
          copyRegion (0, 0) (baseDim, baseDim) gif  (baseDim,       0) comp
          copyRegion (0, 0) (baseDim, baseDim) png  (baseDim, baseDim) comp
          savePngFile "bytesDemo.png" comp
