module Main where

import Control.Monad.Reader
import Data.Complex
import Graphics.Image hiding (magnitude, map, scale)

type CompDbl = Complex Double
type Point = (Int, Int)
type RCfg = Reader Config

data Config = Config { width :: Int
                     , height :: Int
                     , xMax :: Double
                     , yMax :: Double
                     , numIterations :: Int
                     , hueOffset :: Double
                     }

defaultConfig = Config { width = 5000
                       , height = 5000
                       , xMax = 2
                       , yMax = 2
                       , numIterations = 100
                       , hueOffset = 0.65
                       }

julia :: CompDbl -> CompDbl -> CompDbl
julia c x = (x ** 2) + c

iteratedJulia :: CompDbl -> RCfg [CompDbl]
iteratedJulia c = do
    cfg <- ask
    return $ take (numIterations cfg) $ iterate (julia c) 0

scale :: RCfg (Double, Double)
scale = do
    cfg <- ask
    let widthF = fromIntegral (width cfg)
        heightF = fromIntegral (height cfg)
    return ((xMax cfg * 2) / widthF, (yMax cfg * 2) / heightF)

screenToComp :: Point -> RCfg CompDbl
screenToComp (y, x) = do
    (xScale, yScale) <- scale
    cfg <- ask
    let plotX = (fromIntegral x * xScale) - (xMax cfg)
        plotY = (yMax cfg) - (fromIntegral y * yScale)
    return $ plotX :+ plotY

rotateHue :: Double -> Double
rotateHue h
    | h < 0 = h + 1
    | h >= 1 = h - 1
    | otherwise = h

pixelColor :: Point -> RCfg (Pixel HSI Double)
pixelColor pixel = do
    num <- screenToComp pixel
    cfg <- ask
    iterations <- map magnitude <$> iteratedJulia num
    let numBounded = length $ takeWhile (not . isInfinite) iterations
        hsi =
            if numBounded == numIterations cfg
                then PixelHSI 0.0 0.0 0.0
                else PixelHSI hue 1 int
          where
            hue = (rotateHue $ hueOffset cfg + (fromIntegral numBounded / 200))
            int = max 0 (min 1 (fromIntegral numBounded / 50))
    return hsi

pixelColor' :: Config -> Point -> Pixel HSI Double
pixelColor' cfg pixel = runReader (pixelColor pixel) cfg

draw :: RCfg (Image VU RGB Double)
draw = do
    cfg <- ask
    return $ toImageRGB $ makeImageR VU (width cfg, height cfg) (pixelColor' cfg)

main :: IO ()
main = do
    let image = runReader draw defaultConfig
    writeImage "output.png" image
