module Main where

import System.Environment (getArgs)
import System.Console.Terminal.Size (size)

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
                        Just (Window _ width) -> putChar '\n' >> renderImage (scaleImage width img) >> putChar '\n'
