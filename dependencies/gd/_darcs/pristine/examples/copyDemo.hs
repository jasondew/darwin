module Main where

import Graphics.GD

uglyFilename = "ugly.png"

main = do ugly <- uglyImage
          png <- newImage (200, 200)
          copyRegion ( 0,  0) (10, 10) ugly ( 10,  10) png
          copyRegion ( 1,  1) ( 8,  8) ugly ( 11,  31) png
          copyRegion ( 0,  0) ( 8,  8) ugly ( 11,  51) png
          copyRegionScaled ( 0,  0) (10, 10) ugly ( 30, 10) (20, 20) png
          copyRegionScaled ( 0,  0) ( 5,  5) ugly ( 30, 50) (20, 20) png
          copyRegionScaled ( 0,  0) ( 8,  8) ugly ( 30, 90) (20, 20) png
          copyRegionScaled ( 0,  0) (10, 10) ugly ( 70, 10) (19, 19) png
          copyRegionScaled ( 0,  0) (10, 10) ugly ( 70, 50) (21, 21) png
          savePngFile "copyDemo.png" png

uglyImage :: IO Image
uglyImage = do img <- newImage (10, 10)
               drawFilledRectangle (0, 0) (9, 9) magenta img
               drawFilledRectangle (1, 1) (8, 8) green   img
               drawFilledRectangle (2, 2) (7, 7) magenta img
               drawFilledRectangle (3, 3) (6, 6) green   img
               drawFilledRectangle (4, 4) (5, 5) magenta img
               return img 

magenta :: Color
magenta = rgb 255 0 255

green :: Color
green = rgb 0 255 0


