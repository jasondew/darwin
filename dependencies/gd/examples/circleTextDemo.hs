import Graphics.GD

green :: Color
green = rgb 0 255 0

red :: Color
red = rgb 255 0 0

blue :: Color
blue = rgb 0 0 255

main :: IO ()
main = do useFontConfig True
          png <- newImage (200, 200)
          drawStringCircle (100, 100) 100.0 50.0 0.4 "sans" 12.0 "top" "bottom" green png
          drawStringCircle (100, 100)  50.0 25.0 0.6 "sans" 12.0 "top" "bottom" red   png
          drawStringCircle (100, 100)  25.0 12.5 0.8 "sans" 12.0 "top" "bottom" blue  png
          savePngFile "circleTextDemo.png" png
