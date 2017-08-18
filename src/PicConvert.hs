module PicConvert (
    fetchImage,
    scaleImage,
    renderImage
) where

import Codec.Picture
import System.Console.ANSI (setSGR, Color(..), ColorIntensity(..), SGR(..), ConsoleLayer(..))
import Data.List (minimumBy)
import Data.Foldable (traverse_)

-- give it the image path and return either an error or the image data (colours squashed)
fetchImage :: FilePath -> IO (Either String (Image PixelRGB8))
fetchImage = fmap (fmap convertRGB8) . readImage

-- new width -> image data -> scaled image
scaleImage :: Int -> Image PixelRGB8 -> Image PixelRGB8
scaleImage newWidth oldImg@(Image oldWidth oldHeight _) =
    if newWidth >= oldWidth
        then oldImg
        else generateImage genFunc newWidth newHeight
    where
        newHeight = (oldHeight * newWidth) `div` oldWidth
        genFunc px py = pixelAt oldImg ((px * oldWidth) `div` newWidth) ((py * oldHeight) `div` newHeight)

colourMap :: [((Int, Int, Int), (Color, ColorIntensity))]
colourMap = [
    ((0, 0, 0), (Black, Dull)), -- black
    ((128, 0, 0), (Red, Dull)), -- red
    ((0, 128, 0), (Green, Dull)), -- green
    ((128, 128, 0), (Yellow, Dull)), -- yellow
    ((0, 0, 128), (Blue, Dull)), -- blue
    ((128, 0, 128), (Magenta, Dull)), -- magenta
    ((0, 128, 128), (Cyan, Dull)), -- cyan
    ((192, 192, 192), (White, Dull)), -- light gray
    ((128, 128, 128), (Black, Vivid)), -- dark gray
    ((255, 0, 0), (Red, Vivid)), -- light red
    ((0, 255, 0), (Green, Vivid)), -- light green
    ((255, 255, 0), (Yellow, Vivid)), -- light yellow
    ((0, 0, 255), (Blue, Vivid)), -- light blue
    ((255, 0, 255), (Magenta, Vivid)), -- light magenta
    ((0, 255, 255), (Cyan, Vivid)), -- light cyan
    ((255, 255, 255), (White, Vivid))-- white
    ]

-- convert RGB8 to console colours
colourConvert :: PixelRGB8 -> (Color, ColorIntensity)
colourConvert (PixelRGB8 r g b) = snd $ minimumBy minFunc colourMap
    where
        rInt = fromIntegral r
        gInt = fromIntegral g
        bInt = fromIntegral b

        dist (cr, cg, cb) = abs (cr - rInt) + abs (cg - gInt) + abs (cb - bInt)

        minFunc (c0, _) (c1, _) = dist c0 `compare` dist c1

-- takes image data to render, and renders it
renderImage :: Image PixelRGB8 -> IO ()
renderImage img@(Image imgWidth imgHeight _) = traverse_ renderRow [0 .. imgHeight - 1] >> setSGR []
    where
        renderRow j = traverse_ (flip renderPixel j) [0 .. imgWidth - 1] >> putChar '\n'
        renderPixel i j =
            let (color, colorIntensity) = colourConvert (pixelAt img i j)
            in setSGR [SetColor Background colorIntensity color] >> putChar ' '
