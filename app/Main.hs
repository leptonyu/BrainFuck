module Main where

import Language.BrainFuck
import Options.Applicative
import Control.Monad(when)
import System.CPUTime
import System.Directory(doesFileExist)
import              Text.Printf(printf)
import System.IO(readFile,hFlush,stdout,hSetBuffering,BufferMode( NoBuffering ) )

pflag :: Parser BFConfig
pflag = BFConfig 
  <$> switch    ( long "bfVerbose" <> short 'v' <> help "verbose log")
  <*> switch    ( long "bfDebugs" <> short 'd' <> help "debug log")
  <*> switch    ( long "bfShow" <> short 'p' <> help "show ast")
  <*> switch    ( long "bfTime" <> short 'c' <> help "show time")
  <*> switch    ( long "bfOptimize" <> short 'o' <> help "shutdown optimize")
  <*> switch    ( long "bfDryrun" <> short 'r' <> help "dryrun")
  <*> option auto ( long  "bfSize" <> short 's' <> value 512 <> showDefault  <> metavar "SIZE" <> help "memory size")
  <*> strOption ( long "bfExpress" <> short 'e' <> help "express lang" <> value "" <> metavar "EXPRESS")
  <*> strOption ( long "bfFile" <> short 'f' <> help "input file"   <> value "-" <> showDefault  <> metavar "INPUT" )

time :: IO t -> IO t
time a = do
    start <- getCPUTime
    v <- a
    end   <- getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)
    printf "\nComputation time: %0.3f sec\n" (diff :: Double)
    return v


main :: IO ()
main = execParser opts >>= run
  where opts        = info (helper <*> pflag) fullDesc
        run  config = if (bfTime config) then time $ run' config else run' config
        run' config = do
          hSetBuffering stdout NoBuffering
          s <- go config (bfExpress config) (bfFile config)
          when (bfVerbose config) (print s)
        go config ex file 
             | ex   /= ""  = exec config ex
             | file == "-" = getContents >>= exec config
             | otherwise   = do 
                  exists <- doesFileExist file
                  if exists then readFile file >>= exec config
                            else error "File not found!"