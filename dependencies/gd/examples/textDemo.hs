import Graphics.GD

green :: Color
green = rgb 0 255 0

main :: IO ()
main = do useFontConfig True
          (_, (lrx, lry), _, (ulx, uly)) <- measureString "sans" 12.0 0.0 (0, 0) "Test" green
          let smallSize@(smallWidth, smallHeight) = (1 + lrx - ulx, 1 + lry - uly)
          smallPng <- newImage smallSize
          drawString "sans" 12.0 0.0 (negate ulx, negate uly) "Test" green smallPng
          fullPng <- newImage (smallWidth * 2, smallHeight * 4)
          copyRegionScaled (0, 0) smallSize smallPng (0, 0) (2 * smallWidth, 2 * smallHeight) fullPng
          drawString "sans" 24.0 0.0 (0, 4 * smallHeight) "Test" green fullPng
          savePngFile "textDemo.png" fullPng
          itxt <- readFile "textUTF8.txt"
          png <- newImage (400, 80)
          drawString "sans" 18.0 0.0 (10, 70) itxt green png
          savePngFile "textDemo2.png" png