module Main
( main
) where

import Muse.Output (makeTrackRaw, makeTrackWav, mergeTracks, setup, playOutput)
import Muse.Raw (Wave, duration, t12note, volume, squareWave, sineWave, delay, ax)
import Data.List (intercalate)
import Control.Monad (liftM2)
import Data.Coerce (coerce)

bpm :: Float
bpm = 9

beat :: Float
beat = 1 / bpm

delayIn :: Float -> [Wave] -> Wave
delayIn d = concatMap (++ delay d)

discipline1 :: Float -> Wave
discipline1 base = delayIn beat
  [ duration beat (t12note (base + 0) sineWave)
  , duration beat (t12note (base - 10) sineWave)
  , duration beat (t12note (base - 3) sineWave) , duration beat (t12note (base - 12) sineWave)
  , duration beat (t12note (base - 15) sineWave)
  ]

discipline2 :: Float -> Wave
discipline2 base = delayIn beat
  [ duration beat (t12note (base - 3) sineWave)
  , duration beat (t12note (base - 12) sineWave)
  , duration beat (t12note (base - 15) sineWave)
  , duration beat (t12note (base + 0) sineWave)
  , duration beat (t12note (base - 10) sineWave)
  ]

discipline3 :: Float -> Wave
discipline3 base = delayIn (beat / 2)
  [ duration (beat / 2) (t12note (base + 3) sineWave)
  , duration (beat / 2) (t12note (base - 4) sineWave)
  , duration (beat / 2) (t12note (base - 11) sineWave)
  , duration (beat / 2) (t12note (base + 1) sineWave)
  , duration (beat / 2) (t12note (base - 4) sineWave)
  , duration (beat / 2) (t12note (base - 1) sineWave)
  , duration (beat / 2) (t12note (base - 4) sineWave)
  , duration (beat / 2) (t12note (base - 11) sineWave)

  , duration (beat / 2) (t12note (base + 3) sineWave)
  , duration (beat / 2) (t12note (base - 4) sineWave)
  , duration (beat / 2) (t12note (base + 1) sineWave)
  , duration (beat / 2) (t12note (base - 4) sineWave)
  , duration (beat / 2) (t12note (base - 1) sineWave)
  , duration (beat / 2) (t12note (base - 4) sineWave)
  , duration (beat / 2) (t12note (base - 11) sineWave)
  , duration (beat / 2) (t12note (base - 4) sineWave)
  ]

chicken :: Float -> Wave
chicken base = duration beat (t12note (base + 0) sineWave)
  <> delay beat
  <> duration beat (t12note (base +0) sineWave)
  <> delay beat
  <> duration beat (t12note (base + (-2)) sineWave)
  <> delay beat
  <> duration beat (t12note (base + 0) sineWave)

  <> delay (beat * 3)
  <> duration (beat * 2) (t12note (base + (-5)) sineWave)
  <> delay (beat * 2)
  <> duration beat (t12note (base + (-5)) sineWave)
  <> delay beat
  <> duration beat (t12note (base + 0) sineWave)
  <> delay beat
  <> duration beat (t12note (base + 5) sineWave)
  <> delay beat
  <> duration beat (t12note (base + 4) sineWave)
  <> delay beat
  <> duration (beat * 4) (t12note (base + 0) sineWave)

squareLevel :: Int -> Float -> Float -> Float
squareLevel n r rx = sum $ flip homo x . fromIntegral <$> take n (filter odd [1..]) where x = r * rx

guitarWave' :: Float -> Float -> Float
guitarWave' r rx = sin x + cos x + cos (3 * x) + squareLevel 7 r rx
 where
  x = r * rx

guitarWave :: Float -> Wave
guitarWave r = guitarWave' r <$> ax

hbmql1 :: Float -> Wave
hbmql1 base = delayIn beat
  [ duration beat (t12note (base + 3) guitarWave)
  , duration beat (t12note (base - 6) guitarWave)
  , duration beat (t12note (base + 1) guitarWave)
  , duration beat (t12note (base - 8) guitarWave)
  , duration beat (t12note (base - 1) guitarWave)
  , duration beat (t12note (base - 11) guitarWave)
  , duration beat (t12note (base - 2) guitarWave)
  , duration beat (t12note (base - 13) guitarWave)
  , duration beat (t12note (base - 4) guitarWave)
  , duration beat (t12note (base - 14) guitarWave)
  ]

hbmql2 :: Float -> Wave
hbmql2 base = delayIn beat
  [ duration beat (t12note (base - 1) guitarWave)
  , duration beat (t12note (base - 11) guitarWave)
  , duration beat (t12note (base - 2) guitarWave)
  , duration beat (t12note (base - 13) guitarWave)
  , duration beat (t12note (base - 4) guitarWave)
  , duration beat (t12note (base - 14) guitarWave)
  , duration beat (t12note (base + 3) guitarWave)
  , duration beat (t12note (base - 6) guitarWave)
  , duration beat (t12note (base + 1) guitarWave)
  , duration beat (t12note (base - 8) guitarWave)
  ]

homo :: Float -> Float -> Float
homo n x = 1 / n * sin (n * x)

bassWave' :: Float -> Float -> Float
bassWave' r rx = sum $ flip homo x . fromIntegral  <$> take 20 (filter odd listInt)
 where
  listInt :: Integral a => [a]
  listInt = [1..]
  x = r * rx

bassWave :: Float -> Wave
bassWave r = bassWave' r <$> ax

hbmql3 :: Float -> Wave
hbmql3 base = duration (beat * 4 * 7 / 8) (t12note (bassC + 1) bassWave)
  <> delay (beat * 4 / 8)
  <> duration (beat * 4 * 7 / 8) (t12note (bassC + 1) bassWave)

  <> delay (beat * 6.5)

  <> duration (beat * 4 * 5 / 8) (t12note (bassC + 4) bassWave)
  <> delay (beat * 4 * 3 / 8)
  <> duration (beat * 2 * 7 / 8) (t12note (bassC + 6) bassWave)
  <> delay (beat * 2 / 8)
 where
  bassC = base - 36

hbmql3_1 :: Float -> Wave
hbmql3_1 base = duration (beat * 4 * 7 / 8) (t12note (bassC + 1) bassWave)
  <> delay (beat * 4 / 8)
  <> duration (beat * 4 * 7 / 8) (t12note (bassC + 1) bassWave)

  <> delay (beat * 2.5)

  <> duration (beat * 4 * 5 / 8) (t12note (bassC + 4) bassWave)
  <> delay (beat * 4 * 3 / 8)
  <> duration (beat * 2 * 7 / 8) (t12note (bassC + 8) bassWave)
  <> delay (beat * 2 / 8)
  <> duration (beat * 2 * 7 / 8) (t12note (bassC + 4) bassWave)
  <> delay (beat * 2 / 8)
  <> duration (beat * 2 * 7 / 8) (t12note (bassC + 11) bassWave)
  <> delay (beat * 2 / 8)
 where
  bassC = base - 36

hbmql3_2 :: Float -> Wave
hbmql3_2 base = duration (beat * 4 * 7 / 8) (t12note (bassC + 1) bassWave)
  <> delay (beat * 4 / 8)
  <> duration (beat * 4 * 7 / 8) (t12note (bassC + 1) bassWave)

  <> delay (beat * 2.5)

  <> duration (beat * 4 * 5 / 8) (t12note (bassC + 11) bassWave)
  <> delay (beat * 4 * 3 / 8)
  <> duration (beat * 2 * 7 / 8) (t12note (bassC + 10) bassWave)
  <> delay (beat * 2 / 8)
  <> duration (beat * 2 * 7 / 8) (t12note (bassC + 8) bassWave)
  <> delay (beat * 2 / 8)
  <> duration (beat * 2 * 7 / 8) (t12note (bassC + 4) bassWave)
  <> delay (beat * 2 / 8)
 where
  bassC = base - 36

hbmql3_3 :: Float -> Wave
hbmql3_3 base = duration (beat * 4 * 7 / 8) (t12note (bassC + 1) bassWave)
  <> delay (beat * 4 / 8)
  <> duration (beat * 4 * 7 / 8) (t12note (bassC + 1) bassWave)

  <> delay (beat * 2.5)

  <> duration (beat * 4 * 5 / 8) (t12note (bassC - 10) bassWave)
  <> delay (beat * 4 * 3 / 8)
  <> duration (beat * 2 * 7 / 8) (t12note (bassC - 9) bassWave)
  <> delay (beat * 2 / 8)
  <> duration (beat * 2 * 7 / 8) (t12note (bassC + 4) bassWave)
  <> delay (beat * 2 / 8)
  <> duration (beat * 2 * 7 / 8) (t12note (bassC - 1) bassWave)
  <> delay (beat * 2 / 8)
 where
  bassC = base - 36

rew :: Int -> Wave -> Wave
rew n = concat . replicate n

main :: IO ()
main = let
  ws :: [Wave]
  base = 0
  ws = volume 0.1 <$>
    -- [ rew 8 (discipline1 base) <> rew 18 (discipline1 (base - 1)) <> rew 8 (discipline1 base)
    -- , rew 8 (discipline2 base) <> rew 5 (discipline3 base)        <> rew 8 (discipline2 base)
    -- ]
    [ rew 24 (hbmql1 base)
    , rew 24 (hbmql2 base)
    , rew 4 (rew 3 (hbmql3 base) <> hbmql3_3 base <> rew 3 (hbmql3 base) <> hbmql3_2 base <> rew 3 (hbmql3 base) <> hbmql3_1 base)
    ]
  len = length ws
  in do
    -- create 'out' directory
    setup
    -- make raw tracks
    mapM_ (uncurry makeTrackRaw) $ zip [1..] ws
    -- convert them to `.wav` format
    mapM_ makeTrackWav [1..len]
    -- merge them together into `output.wav`
    mergeTracks len
    -- play it
    playOutput

    return ()
