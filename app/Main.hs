module Main where

import System.Environment(getArgs)
import System.Console.Terminal.Size

import PicConvert

main :: IO ()
main = do
    args <- getArgs
    if null args
        then putStrLn "no filename given"
        else do
            result <- fetchImage (head args)
            case result of
                Left err -> putStrLn err
                Right img -> do
                    maybeSize <- size
                    case maybeSize of
                        Nothing -> putStrLn "could not get terminal size"
                        Just (Window _ width) -> putStrLn "" >> renderImage (scaleImage width img) >> putStrLn ""
