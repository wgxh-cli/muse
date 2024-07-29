module Muse.Output
( outputDir
, trackPath
, makeTrackRaw
, makeTrackWav
, mergeTracks
, setup
, cleanup
, playOutput
) where

import System.FilePath
import Muse.Raw (Wave, sampleRate)
import Data.ByteString.Builder (floatLE, toLazyByteString)
import qualified Data.ByteString.Lazy as B
import Muse.FFmpeg (task, def, (|>), (|-), withFormat, FFmpegBuilder, withAudioSRate, withInputs, withOutput, withComplexFilter, withOverwrite, syscall)
import Control.Monad (void, when, unless)
import Control.Applicative (Alternative(empty))
import System.Directory

outputDir :: String
outputDir = "out"

output :: FilePath -> FilePath
output p = outputDir </> p

trackPath :: String -> Int -> FilePath
trackPath suf i = output $ "track" <> show i <.> suf

-- output flow: tracks -> tracks raw -> trakcs wav -> output wav

setup :: IO ()
setup = do
  e <- doesDirectoryExist outputDir
  unless e $ createDirectory outputDir

cleanup :: IO ()
cleanup = do
  e <- doesDirectoryExist outputDir
  when e $ removeDirectoryRecursive outputDir

baseFFmpeg :: FFmpegBuilder
baseFFmpeg = id |> withOverwrite

rawFFmpeg :: FFmpegBuilder
rawFFmpeg = baseFFmpeg
  |> withFormat "f32le"
  |> withAudioSRate sampleRate

makeTrackRaw :: Int -> Wave -> IO ()
makeTrackRaw i w = B.writeFile (trackPath "raw" i) $ toLazyByteString $ foldMap floatLE w

makeTrackWav :: Int -> IO ()
makeTrackWav i = void . task $ rawFFmpeg
  |> withInputs [trackPath "raw" i]
  |> withOutput (trackPath "wav" i)
  |- def

mergeTracks :: Int -> IO ()
mergeTracks 0 = empty
mergeTracks n = void . task $ baseFFmpeg
  |> withInputs (trackPath "wav" <$> [1..n])
  |> withComplexFilter "amerge" [("inputs", show n)]
  |> withOutput (output "output.wav")
  |- def

playOutput :: IO ()
playOutput = void $ syscall ["ffplay", "-showmode 1" , output "output.wav"]
