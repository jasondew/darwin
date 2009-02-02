import Graphics.GD

red :: Color
red = rgb 255 0 0

white :: Color
white = rgb 255 255 255

main :: IO ()
main = do let w = 200
          let h = 200
          pic <- newImage (w, h)
          fillImage white pic
          drawLine (0,0) (w,h) red pic
          drawLine (w,0) (0,h) red pic
          saveJpegFile 90 "drawLineDemo.jpg" pic