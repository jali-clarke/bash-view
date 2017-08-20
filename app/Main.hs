module Main where

import System.Environment (getArgs)
import System.Console.Terminal.Size (Window(..), size)
import Control.Monad.Except
import Control.Monad (when)

import PicConvert

main :: IO ()
main = do
    actionResult <- runExceptT $ do
        args <- liftIO getArgs
        when (null args) (throwError "no filename given")

        result <- liftIO (fetchImage (head args))
        img <- ExceptT (return result)
        maybeSize <- liftIO size
        maybe (throwError "could not get terminal size") (liftIO . renderAction img) maybeSize

    case actionResult of
        Left err -> putStrLn err
        Right _ -> return ()

    where
        renderAction img (Window _ width) = putChar '\n' >> renderImage (scaleImage width img) >> putChar '\n'
