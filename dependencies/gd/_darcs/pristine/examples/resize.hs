import qualified Graphics.GD as GD

import Data.Ratio
import System.Environment
import System.Exit
import System.IO

type Size = (Int,Int)

calcSize :: Size -> Size -> Size
calcSize (inW,inH) (outW,outH)
    | inAspect >  outAspect = (outW, round (fromIntegral outW / inAspect))
    | inAspect <  outAspect = (round (fromIntegral outH * inAspect), outH)
    | otherwise             = (outW, outH)
    where inAspect = inW % inH
          outAspect = outW % outH

-- | Resizes an image file and saves the result to a new file.
resizeImage :: FilePath -- ^ Source image file
            -> FilePath -- ^ Destination image file
            -> Size     -- ^ The maximum dimensions of the output file
            -> IO (Size,Size) -- ^ The sizes of the input and output files
resizeImage from to maxSz = 
    do img  <- GD.loadJpegFile from
       inSz <- GD.imageSize img
       let outSz@(w,h) = calcSize inSz maxSz                  
       img' <- GD.resizeImage w h img
       GD.saveJpegFile (-1) to img'
       return (inSz,outSz)

readM :: (Monad m, Read a) => String -> m a
readM s = case reads s of
                     [] -> fail "No read"
                     [(x,"")] -> return x
                     [_] -> fail "Incomplete read"
                     _ -> fail "Ambiguous read"

parseArgs :: [String] -> Maybe (String,String,Int,Int)
parseArgs [a1,a2,a3,a4] = do
                          w <- readM a3
                          h <- readM a4
                          return (a1,a2,w,h)
parseArgs _ = Nothing

main :: IO ()
main = do
       args <- getArgs
       case parseArgs args of
            Nothing -> do
                       hPutStrLn stderr "Usage: resize <src file> <dest file> <width> <height>"
                       exitFailure
            Just (from,to,w,h) -> do
                                  (inSz,outSz) <- resizeImage from to (w,h)
                                  putStrLn $ "Resized " ++ from ++ " " ++ show inSz
                                               ++ " to " ++ to ++ " " ++ show outSz
