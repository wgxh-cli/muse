module Muse.FFmpeg
( FFmpeg (..)
, FFmpegArg
, FFmpegInput
, FFmpegBuilder

, withArgs
, withInputs
, withAudioFilter
, withVideoFilter
, withComplexFilter
, withFormat
, withAudioSRate
, withOverwrite
, syscall
, task
, def
, withOutput
, (|>)
, (|-)
) where

import Data.List (intercalate)
import System.Exit (ExitCode)
import System.Process (system)

type FFmpegArg = String

newtype FFmpeg
  = FFmpeg
  { args :: [FFmpegArg]
  }

instance Semigroup FFmpeg where
  f1 <> f2 = FFmpeg $ args f1 <> args f2

type FFmpegInput = FilePath
type FFmpegBuilder = FFmpeg -> FFmpeg

withArgs :: [String] -> FFmpegBuilder
withArgs nargs f = f { args = args f <> nargs }

withInputs :: [FFmpegInput] -> FFmpegBuilder
withInputs inputs = withArgs $ concatMap (\i -> ["-i", i]) inputs

type FilterName = String


withFilter :: String -> FilterName -> [(String, String)] -> FFmpegBuilder
withFilter f name options = withArgs [f, name <> "=" <> intercalate ":" formatedArgs]
  where formatedArgs = (\(k, v) -> k <> "=" <> v) <$> options

withAudioFilter :: FilterName -> [(String, String)] -> FFmpegBuilder
withAudioFilter = withFilter "-af"

withVideoFilter :: FilterName -> [(String, String)] -> FFmpegBuilder
withVideoFilter = withFilter "-vf"

withComplexFilter :: FilterName -> [(String, String)] -> FFmpegBuilder
withComplexFilter = withFilter "-filter_complex"

withFormat :: String -> FFmpegBuilder
withFormat f = withArgs ["-f", f]

withAudioSRate :: Int -> FFmpegBuilder
withAudioSRate rate = withArgs ["-ar", show rate]

withOutput :: FilePath -> FFmpegBuilder
withOutput f = withArgs [f]

withOverwrite :: FFmpegBuilder
withOverwrite = withArgs ["-y"]

(|>) :: FFmpegBuilder -> FFmpegBuilder -> FFmpegBuilder
b1 |> b2 = b2 . b1

(|-) :: FFmpegBuilder -> FFmpeg -> FFmpeg
b |- f = b f

syscall :: [String] -> IO ExitCode
syscall ss = system $ unwords ss

def :: FFmpeg
def = FFmpeg ["ffmpeg"]

task :: FFmpeg -> IO ExitCode
task f = syscall $ args f
