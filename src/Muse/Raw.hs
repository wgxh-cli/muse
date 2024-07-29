module Muse.Raw
( Frequency
, Duration
, Wave
, Volume

, ax
, freq
, duration
, volume
, sampleRate
, delay
, t12note
, sineWave
, squareWave
, triangleWave
) where
import Data.Fixed (mod')

type Frequency = Float -- unit: Hz
type Duration = Float -- unit: beats
type Wave = [Float]
type Volume = Float -- ratio: gaining value

-- | the x axis of the wave
ax :: [Float]
ax = [0..]

sampleRate :: Num a => a
sampleRate = 48000

sineWave :: Float -> Wave
sineWave r = sin . (* r) <$> ax

squareWave :: Float -> Wave
squareWave r = signum <$> sineWave r

triangleWave :: Float -> Wave
triangleWave r = (\x -> (4 / p) * abs ((x - p / 4) `mod'` p) - p / 2) <$> ax where p = 2 * pi

freq :: Frequency -> (Float -> Wave) -> Wave
freq f wave = wave (2 * pi * f / sampleRate)

t12note :: Float -> (Float -> Wave) -> Wave
t12note n = freq (440 * (2 ** ((3 + n) / 12)))

duration :: Duration -> Wave -> Wave
duration d = take $ floor (d * sampleRate)

volume :: Volume -> Wave -> Wave
volume v w = (* v) <$> w

delay :: Duration -> Wave
delay d = duration d $ repeat 0
