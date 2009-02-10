import Prelude hiding (lookup)
import Graphics.GD
import System.Random
import Control.Monad
import Foreign.C.Types
import Data.Bits
import Data.Map hiding (map)
import Data.Time
import Text.Printf

type Rectangle = (Point, Point, Color)
type DNA = [Rectangle]
type RGB = (Int, Int, Int)

numberOfObjects :: Int
numberOfObjects = 10

numberOfAdditions :: Int
numberOfAdditions = 2

additionProbability :: Int
additionProbability = 1

mutationProbability :: Int
mutationProbability = 20

numberOfIterations :: Int
numberOfIterations = 1000000

snapshotEvery :: Int
snapshotEvery = 10

targetPath :: String
targetPath = "monalisa.jpg"

-- =======================================================================================================================

targetImage :: IO Image
targetImage = loadJpegFile targetPath

pixelColors :: IO Image -> Int -> Int -> IO [(Point, RGB)]
pixelColors io_image width height = do image <- io_image
                                       mapM (\point -> getRGB image point) [(x, y) | x <- [1..width], y <- [1..height]]
                                    where getRGB image point = do color <- getPixel point image
                                                                  return (point, (red color, green color, blue color))

randomNumberGenerator = randomR (0, 100)
randomRGBGenerator    = randomR (0, 255)
randomAlphaGenerator  = randomR (0, 127)

randomColor :: IO Color
randomColor = do red   <- getStdRandom randomRGBGenerator
                 green <- getStdRandom randomRGBGenerator
                 blue  <- getStdRandom randomRGBGenerator
                 alpha <- getStdRandom randomAlphaGenerator
                 return $ rgba red green blue alpha

alpha :: Num a => Color -> a
alpha color = fromIntegral $ color `shiftR` 24

red :: Num a => Color -> a
red color = fromIntegral $ (color .&. 16711680) `shiftR` 16

green :: Num a => Color -> a
green color = fromIntegral $ (color .&. 65280) `shiftR` 8

blue :: Num a => Color -> a
blue color = fromIntegral $ color .&. 255

randomPoint :: Int -> Int -> IO Point
randomPoint maxX maxY = do x <- getStdRandom $ randomR (0, maxX)
                           y <- getStdRandom $ randomR (0, maxY)
                           return (x, y)

randomRectangle :: Int -> Int -> IO Rectangle
randomRectangle maxX maxY = do start <- randomPoint maxX maxY
                               end   <- randomPoint maxX maxY
                               color <- randomColor
                               return (start, end, color)

drawRectangle :: Rectangle -> Image -> IO ()
drawRectangle (start, end, color) = drawFilledRectangle start end color

initialDNA :: Int -> Int -> Int -> IO DNA
initialDNA objects maxX maxY = sequence [randomRectangle maxX maxY | _ <- [1..objects]]

drawDNAImage :: Int -> Int -> IO DNA -> IO Image
drawDNAImage width height ioDNA = do image <- newImage (width, height)
                                     dna   <- ioDNA
                                     mapM_ (\rectangle -> drawRectangle rectangle image) dna
                                     return image

mutatedValue :: Int -> Int -> IO Int
mutatedValue original max = do offset <- getStdRandom $ randomR (0, max)
                               return $ (original + offset) `mod` max

maybeMutateValue :: Int -> Int -> IO Int
maybeMutateValue original max = do randomNumber <- getStdRandom randomNumberGenerator
                                   case randomNumber < mutationProbability of
                                     True  -> mutatedValue original max
                                     False -> return original

maybeMutatePoint :: Point -> Int -> Int -> IO Point
maybeMutatePoint (x, y) maxX maxY = do newX <- maybeMutateValue x maxX
                                       newY <- maybeMutateValue y maxY
                                       return (newX, newY)

maybeMutateColor :: Color -> IO Color
maybeMutateColor original = do possiblyMutatedRed   <- maybeMutateValue (red original)   255
                               possiblyMutatedGreen <- maybeMutateValue (green original) 255
                               possiblyMutatedBlue  <- maybeMutateValue (blue original)  255
                               possiblyMutatedAlpha <- maybeMutateValue (alpha original) 127
                               return $ rgba possiblyMutatedRed possiblyMutatedGreen possiblyMutatedBlue possiblyMutatedAlpha

maybeMutateRectangle :: Rectangle -> Int -> Int -> IO Rectangle
maybeMutateRectangle rectangle maxX maxY = do randomNumber <- getStdRandom randomNumberGenerator
                                              case randomNumber < mutationProbability of
                                                True  -> mutateRectangle rectangle maxX maxY
                                                False -> return rectangle

mutateRectangle :: Rectangle -> Int -> Int -> IO Rectangle
mutateRectangle (start, end, color) maxX maxY = do possiblyMutatedStart <- maybeMutatePoint start maxX maxY
                                                   possiblyMutatedEnd   <- maybeMutatePoint end maxX maxY
                                                   possiblyMutatedColor <- maybeMutateColor color
                                                   return (possiblyMutatedStart, possiblyMutatedEnd, possiblyMutatedColor)

maybeNewRectangles :: Int -> Int -> IO [Rectangle]
maybeNewRectangles maxX maxY = do randomNumber <- getStdRandom randomNumberGenerator
                                  case randomNumber < additionProbability of
                                    True  -> mapM (\i -> randomRectangle maxX maxY) [1..numberOfAdditions]
                                    False -> return []
mutateDNA :: DNA -> Int -> Int -> IO DNA
mutateDNA (rectangle:rectangles) maxX maxY = do possiblyMutatedRectangle  <- maybeMutateRectangle rectangle maxX maxY
                                                possiblyMutatedRectangles <- mutateDNA rectangles maxX maxY
                                                possiblyNewRectangles     <- maybeNewRectangles maxX maxY
                                                return $ (possiblyMutatedRectangle : possiblyMutatedRectangles) ++ possiblyNewRectangles
mutateDNA _ _ _                            = return []

rgbDifference :: RGB -> RGB -> Double
rgbDifference (r1, g1, b1) (r2, g2, b2) = sum $ map (\d -> (fromIntegral d) ** 2) [r1 - r2, g1 - g2, b1 - b2]

comparePixel :: Point -> Map Point RGB -> Image -> IO Double
comparePixel point target image = case lookup point target of
                                       Just rgb -> do candidateColor <- getPixel point image
                                                      return $ rgbDifference rgb (red candidateColor, green candidateColor, blue candidateColor)
                                       Nothing  -> return $ 0.0

fitness :: Map Point RGB -> Image -> Int -> Int -> IO Double
fitness target image width height = foldl pixelFitness (return 0.0) [(x, y) | x <- [1..width], y <- [1..height]]
                                    where pixelFitness total point = do t <- total
                                                                        f <- comparePixel point target image
                                                                        return $ f + t

printStatus :: Int -> Double -> [Double] -> Int -> IO ()
printStatus iteration fit previousFits objects = do currentTime <- getCurrentTime
                                                    printf "%30s: iteration: %8d, objects: %5d, fit: %14.0f, fit delta: %12.0f, percent improvement: %5.1f%%\n"
                                                           (show currentTime) iteration objects fit delta percentImprovement

                                                 where findLastSeenFit []   = 0.0
                                                       findLastSeenFit fits = fits !! (snapshotEvery `mod` length fits)

                                                       lastSeenFit        = findLastSeenFit previousFits
                                                       delta              = lastSeenFit - fit
                                                       percentImprovement = 100.0 * (delta / lastSeenFit)

nextGeneration :: Map Point RGB -> Int -> Int -> DNA -> Double -> Int -> IO (DNA, Double)
nextGeneration target width height currentDNA currentFit iteration =
  do case iteration `mod` snapshotEvery of
       0         -> do image <- drawDNAImage width height $ return currentDNA
                       savePngFile ("iteration" ++ show iteration ++ ".png") image
       otherwise -> return ()

     mutatedDNA   <- mutateDNA currentDNA width height
     mutatedImage <- drawDNAImage width height $ return mutatedDNA
     mutatedFit   <- fitness target mutatedImage width height

     case (mutatedFit < currentFit) of
       True  -> return (mutatedDNA, mutatedFit)
       False -> return (currentDNA, currentFit)

simulationStep :: Map Point RGB -> Int -> Int -> DNA -> Int -> Int -> [Double] -> IO [Double]
simulationStep target width height currentDNA iteration totalIterations fits@(currentFit:_)
  | iteration == totalIterations = return fits
  | otherwise                    = do (nextDNA, nextFit) <- nextGeneration target width height currentDNA currentFit iteration

                                      case iteration `mod` snapshotEvery of
                                        0         -> printStatus iteration currentFit fits $ length currentDNA
                                        otherwise -> return ()

                                      simulationStep target width height nextDNA (iteration + 1) totalIterations (nextFit:fits)

runSimulation :: Map Point RGB -> Int -> Int -> DNA -> Int -> IO [Double]
runSimulation target width height ivDNA iterations = simulationStep target width height ivDNA 0 iterations [1e12]

main :: IO ()
main = do startTime <- getCurrentTime
          putStrLn $ show startTime ++ ": processing started; initial objects = " ++ show numberOfObjects ++ 
                     ", increment = " ++ show numberOfAdditions ++ 
                     ", increment probability = " ++ show additionProbability ++
                     ", mutation probability = " ++ show mutationProbability

          (width, height) <- withImage targetImage imageSize
          target          <- pixelColors targetImage width height
          dna             <- initialDNA numberOfObjects width height
          fits            <- runSimulation (fromList target) width height dna numberOfIterations

          endTime <- getCurrentTime
          putStrLn $ show endTime ++ ": processing ended"
