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
        pixAccFunc (PixelRGB8 r g b) ((pr, pg, pb), plen) = ((pr + fromIntegral r, pg + fromIntegral g, pb + fromIntegral b), plen + 1)
        genFunc px py =
            let scaleFactor = oldWidth `div` newWidth
                xstart = px * scaleFactor
                xend = (min oldWidth $ xstart + scaleFactor) - 1
                ystart = py * scaleFactor
                yend = (min oldHeight $ ystart + scaleFactor) - 1
                ((pr, pg, pb), plen) = foldr pixAccFunc ((0, 0, 0), 0) $ (flip (pixelAt oldImg)) <$> [ystart .. yend] <*> [xstart .. xend]
            in PixelRGB8 (fromIntegral (pr `div` plen)) (fromIntegral (pg `div` plen)) (fromIntegral (pb `div` plen))

colourMap :: [((Int, Int, Int), (ColorIntensity, Color))]
colourMap = [
    ((0, 0, 0), (Dull, Black)), -- black
    ((128, 0, 0), (Dull, Red)), -- red
    ((0, 128, 0), (Dull, Green)), -- green
    ((128, 128, 0), (Dull, Yellow)), -- yellow
    ((0, 0, 128), (Dull, Blue)), -- blue
    ((128, 0, 128), (Dull, Magenta)), -- magenta
    ((0, 128, 128), (Dull, Cyan)), -- cyan
    ((192, 192, 192), (Dull, White)), -- light gray
    ((128, 128, 128), (Vivid, Black)), -- dark gray
    ((255, 0, 0), (Vivid, Red)), -- light red
    ((0, 255, 0), (Vivid, Green)), -- light green
    ((255, 255, 0), (Vivid, Yellow)), -- light yellow
    ((0, 0, 255), (Vivid, Blue)), -- light blue
    ((255, 0, 255), (Vivid, Magenta)), -- light magenta
    ((0, 255, 255), (Vivid, Cyan)), -- light cyan
    ((255, 255, 255), (Vivid, White))-- white
    ]

-- convert RGB8 to console colours
colourConvert :: PixelRGB8 -> (ColorIntensity, Color)
colourConvert (PixelRGB8 r g b) = snd $ minimumBy minFunc colourMap
    where
        rInt = fromIntegral r
        gInt = fromIntegral g
        bInt = fromIntegral b

        dist (cr, cg, cb) = abs (cr - rInt) + abs (cg - gInt) + abs (cb - bInt)

        minFunc (c0, _) (c1, _) = dist c0 `compare` dist c1

-- takes image data to render, and renders it
renderImage :: Image PixelRGB8 -> IO ()
renderImage img@(Image imgWidth imgHeight _) = renderRows 0
    where
        renderRows j =
            if j >= imgHeight
                then return ()
                else renderPixels j 0 >> setSGR [Reset] >> putChar '\n' >> renderRows (j + 1)

        renderPixels j i =
            if i >= imgWidth
                then return ()
                else
                    let (colorIntensity, color) = colourConvert (pixelAt img i j)
                    in setSGR [SetColor Background colorIntensity color] >> putChar ' ' >> renderPixels j (i + 1)
