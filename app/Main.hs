{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Import
import Run
-- import RIO.Process
import Options.Applicative.Simple
import qualified Paths_haradict

main :: IO ()
main = do
  (options, ()) <- simpleOptions
    $(simpleVersion Paths_haradict.version)
    "Header for command line arguments"
    "Program description, also for command line arguments"
    (Options
       <$> switch ( long "verbose"
                 <> short 'v'
                 <> help "Verbose output?"
                  )
    )
    empty
  -- lo <- logOptionsHandle stderr (optionsVerbose options)
  -- pc <- mkDefaultProcessContext

  run options
  -- withLogFunc lo $ \lf ->
  --   let app = App
  --         { appLogFunc = lf
  --         , appProcessContext = pc
  --         , appOptions = options
  --         }
  --    in runRIO app run
