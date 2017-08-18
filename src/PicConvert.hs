module PicConvert (
    fetchImage,
    renderImage
) where

import Codec.Picture(convertRGB8, readImage, Image(..), PixelRGB8(..))
import System.Console.ANSI

-- give it the image path and return either an error or the image data (squashed)
fetchImage :: FilePath -> IO (Either String (Image PixelRGB8))
fetchImage = fmap (fmap convertRGB8) . readImage

-- takes output length and width, followed by the image data
renderImage :: Int -> Int -> Image PixelRGB8 -> IO ()
