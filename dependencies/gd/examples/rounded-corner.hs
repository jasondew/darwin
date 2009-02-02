import Graphics.GD

import Numeric (readHex)
import System.Environment
import System.IO


makeImage :: Size -> Color -> Color -> IO Image
makeImage sz@(w,h) bg fg = 
    do i <- newImage sz
       fillImage bg i
       antiAliased (drawFilledEllipse (w-1,h-1) (2*w,2*h)) fg i
       return i

main :: IO ()
main = do args <- getArgs
          case args of
              [f,w,h,fg,bg] -> 
                  do i <- makeImage (read w,read h) (readColor fg) (readColor bg)
                     savePngFile f i
              _ -> do hPutStrLn stderr "Usage: rounded-corner <file> <width> <height> <fg color> <bgcolor>"
                      hPutStrLn stderr ""
                      hPutStrLn stderr "Example: rounded-corner corner.png 20 20 d3d7cf 4e9a06"

readColor :: String -> Color
readColor [r1,r2,g1,g2,b1,b2] = rgb (rb r1 r2) (rb g1 g2) (rb b1 b2)
  where rb x y = case readHex [x,y] of
                   [(i,"")] -> i
                   s        -> error $ "Bad color: " ++ show s
readColor s = error $ "Bad color: " ++ show s