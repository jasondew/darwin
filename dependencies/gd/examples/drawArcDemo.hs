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
          drawArc (100,100) (w,h) 40 210 red pic
          saveJpegFile 90 "drawArcDemo.jpg" pic