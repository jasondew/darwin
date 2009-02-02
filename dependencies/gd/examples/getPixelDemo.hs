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
          mapM_ (\t -> setPixel (mod (t*4) w, mod (t*3) w) red pic) [0..w*3]
          color <- getPixel (100, 100)
          return color
