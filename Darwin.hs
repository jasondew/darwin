--TODO keep and analyze convergence for optimal parameters
--TODO have generation size > 1, possibly different types

import Prelude hiding (lookup)
import Control.Monad
import Data.Bits
import Data.Map hiding (map)
import Data.Maybe (fromMaybe)
import Data.Time
import Foreign.C.Types
import Graphics.GD
import System.Console.GetOpt
import System.Environment
import System.Random
import Text.Printf

type Rectangle = (Point, Point, Color)
type DNA = [Rectangle]

data Flag = TargetPath String | StartingObjects Int | MutationProbability Int | AdditionProbability Int |
            NumberOfAdditions Int | NumberOfIterations Int | SnapshotFrequency Int
            deriving Show

allowedOptions :: [OptDescr Flag]
allowedOptions = [Option ['t'] ["target"]                (ReqArg TargetPath                   "JPEG only")      "target image",
                  Option ['i'] ["initial"]               (ReqArg (StartingObjects . read)     "n > 0")          "initial number of objects",
                  Option ['m'] ["mutation_probability"]  (ReqArg (MutationProbability . read) "0 <= p <= 100")  "mutation probability",
                  Option ['a'] ["addition_probability"]  (ReqArg (AdditionProbability . read) "0 <= p <= 100")  "addition probability",
                  Option ['d'] ["addition_number"]       (ReqArg (NumberOfAdditions . read)   "n >= 0")         "number of additions",
                  Option ['n'] ["iterations"]            (ReqArg (NumberOfIterations . read)  "n > 0")          "number of iterations",
                  Option ['s'] ["snapshot_frequency"]    (ReqArg (SnapshotFrequency . read)   "n > 0")          "snapshot frequency n iterations"]

header :: String
header = "Usage: Darwin [OPTIONS...] image"

getOptions :: [String] -> IO ([Flag], String)
getOptions argv = case getOpt Permute allowedOptions argv of
                       (options, (path:[]),   []) -> return (options, path)
                       (      _,        [],    _) -> ioError (userError ("Target must be specified" ++ usageInfo header allowedOptions))
                       (      _,         _, errs) -> ioError (userError (concat errs ++ usageInfo header allowedOptions))

startingObjects :: [Flag] -> Int
startingObjects ((StartingObjects i):_) = i
startingObjects (_:fs)                  = startingObjects fs
startingObjects []                      = 5

mutationProbability :: [Flag] -> Int
mutationProbability ((MutationProbability i):_) = i
mutationProbability (_:fs)                      = mutationProbability fs
mutationProbability []                          = 25

additionProbability :: [Flag] -> Int
additionProbability ((AdditionProbability i):_) = i
additionProbability (_:fs)                      = additionProbability fs
additionProbability []                          = 2

numberOfAdditions :: [Flag] -> Int
numberOfAdditions ((NumberOfAdditions i):_) = i
numberOfAdditions (_:fs)                    = numberOfAdditions fs
numberOfAdditions []                        = 2

numberOfIterations :: [Flag] -> Int
numberOfIterations ((NumberOfIterations i):_) = i
numberOfIterations (_:fs)                     = numberOfIterations fs
numberOfIterations []                         = 1000000

snapshotFrequency :: [Flag] -> Int
snapshotFrequency ((SnapshotFrequency i):_) = i
snapshotFrequency (_:fs)                    = snapshotFrequency fs
snapshotFrequency []                        = 1000

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

mutate :: IO Bool
mutate = do randomNumber <- getStdRandom randomNumberGenerator
            args         <- getArgs
            (options, _) <- getOptions args

            let probability = mutationProbability options

            return $ randomNumber < probability

additions :: IO Int
additions = do randomNumber <- getStdRandom randomNumberGenerator
               args         <- getArgs
               (options, _) <- getOptions args

               let probability = additionProbability options
                   number      = numberOfAdditions options

               case randomNumber < probability of
                 True  -> return number
                 False -> return 0

mutatedValue :: Int -> Int -> IO Int
mutatedValue original max = do offset <- getStdRandom $ randomR (0, max)
                               return $ (original + offset) `mod` max

maybeMutateValue :: Int -> Int -> IO Int
maybeMutateValue original max = do perform <- mutate
                                   case perform of
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
maybeMutateRectangle rectangle maxX maxY = do perform <- mutate
                                              case perform of
                                                True  -> mutateRectangle rectangle maxX maxY
                                                False -> return rectangle

mutateRectangle :: Rectangle -> Int -> Int -> IO Rectangle
mutateRectangle (start, end, color) maxX maxY = do possiblyMutatedStart <- maybeMutatePoint start maxX maxY
                                                   possiblyMutatedEnd   <- maybeMutatePoint end maxX maxY
                                                   possiblyMutatedColor <- maybeMutateColor color
                                                   return (possiblyMutatedStart, possiblyMutatedEnd, possiblyMutatedColor)

maybeNewRectangles :: Int -> Int -> IO [Rectangle]
maybeNewRectangles maxX maxY = do adds <- additions
                                  case adds > 0 of 
                                    True  -> mapM (\i -> randomRectangle maxX maxY) [1..adds]
                                    False -> return []

mutateDNA :: DNA -> Int -> Int -> IO DNA
mutateDNA (rectangle:rectangles) maxX maxY = do possiblyMutatedRectangle  <- maybeMutateRectangle rectangle maxX maxY
                                                possiblyMutatedRectangles <- mutateDNA rectangles maxX maxY
                                                possiblyNewRectangles     <- maybeNewRectangles maxX maxY
                                                return $ (possiblyMutatedRectangle : possiblyMutatedRectangles) ++ possiblyNewRectangles
mutateDNA _ _ _                            = return []

colorDifference :: Color -> Color -> Double
colorDifference c1 c2 = sum $ map (\d -> (fromIntegral d) ** 2) [red c1 - red c2, green c1 - green c2, blue c1 - blue c2]

fitness :: [[Color]] -> [[Color]] -> Double
fitness as bs = sum $ zipWith columnZip as bs
          where columnZip cs ds = sum $ zipWith colorDifference cs ds

printStatus :: Int -> Double -> [Double] -> Int -> Int -> IO ()
printStatus iteration fit previousFits objects snapshotFreq =
  do currentTime <- getCurrentTime
     printf "%30s: iteration: %8d, objects: %5d, fit: %14.0f, fit delta: %12.0f, percent improvement: %5.1f%%\n"
            (show currentTime) iteration objects fit delta percentImprovement

  where findLastSeenFit []   = 0.0
        findLastSeenFit fits = fits !! (snapshotFreq `mod` length fits)

        lastSeenFit        = findLastSeenFit previousFits
        delta              = lastSeenFit - fit
        percentImprovement = 100.0 * (delta / lastSeenFit)

nextGeneration :: [[Color]] -> Int -> Int -> DNA -> Double -> Int -> Int -> IO (DNA, Double)
nextGeneration target width height currentDNA currentFit iteration snapshotFreq =
  do case iteration `mod` snapshotFreq of
       0         -> do image <- drawDNAImage width height $ return currentDNA
                       savePngFile ("iteration" ++ show iteration ++ ".png") image
       otherwise -> return ()

     mutatedDNA    <- mutateDNA currentDNA width height
     mutatedImage  <- drawDNAImage width height $ return mutatedDNA
     mutatedPixels <- getPixels mutatedImage
     mutatedFit    <- return $ fitness target mutatedPixels

     case (mutatedFit < currentFit) of
       True  -> return (mutatedDNA, mutatedFit)
       False -> return (currentDNA, currentFit)

simulationStep :: [[Color]] -> Int -> Int -> DNA -> Int -> Int -> Int -> [Double] -> IO [Double]
simulationStep target width height currentDNA iteration iterations snapshotFreq fits@(currentFit:_)
  | iteration == iterations = return fits
  | otherwise               = do (nextDNA, nextFit) <- nextGeneration target width height currentDNA currentFit iteration snapshotFreq
                                 case iteration `mod` snapshotFreq of
                                   0         -> printStatus iteration currentFit fits (length currentDNA) snapshotFreq
                                   otherwise -> return ()

                                 simulationStep target width height nextDNA (iteration + 1) iterations snapshotFreq (nextFit:fits)

runSimulation :: [[Color]] -> Int -> Int -> DNA -> Int -> Int -> IO [Double]
runSimulation target width height ivDNA iterations snapshotFreq =
  simulationStep target width height ivDNA 0 iterations snapshotFreq [maxError]
  where maxError = (255 ** 3) * (fromIntegral width) * (fromIntegral height)

main :: IO ()
main = do argv            <- getArgs
          (options, path) <- getOptions argv

          let
            objects      = startingObjects options
            mutationProb = mutationProbability options
            additionProb = additionProbability options
            additions    = numberOfAdditions options
            iterations   = numberOfIterations options
            snapshotFreq = snapshotFrequency options

          target          <- loadJpegFile path
          (width, height) <- withImage (loadJpegFile path) imageSize
          targetPixels    <- getPixels target
          dna             <- initialDNA objects width height

          startTime <- getCurrentTime
          putStrLn $ show startTime ++ ": processing started; " ++ ", iterations = " ++ show iterations ++ 
                     ", initial objects = " ++ show objects ++ ", mutation probability = " ++ show mutationProb ++
                     ", increment = " ++ show additions ++ ", increment probability = " ++ show additionProb ++
                     ", snapshot frequency = " ++ show snapshotFreq

          fits            <- runSimulation targetPixels width height dna iterations snapshotFreq

          endTime <- getCurrentTime
          putStrLn $ show endTime ++ ": processing ended"
