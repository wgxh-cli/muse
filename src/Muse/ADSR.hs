module Muse.ADSR
( ADSREnvelope (..)
, ADSRBuilder (..)
, Ratio
, (|>)
) where

import Muse.Raw (Duration)

type Ratio = Float

-- | `ADSREnvelope` describes how a value changes as the time change
newtype ADSREnvelope = ADSREnvelope
  { runAdsr :: Float -> Float
  }

data ADSRBuilder = ADSRBuilder
  { attack :: Float -> Maybe Float
  , decay :: Float -> Maybe Float
  , sustain :: Duration
  , release :: Float -> Maybe Float
  }

(|>) :: ADSRBuilder -> (ADSRBuilder -> ADSRBuilder) -> ADSRBuilder
adsr |> b = b adsr

withAttack :: (Ratio -> Float) -> Duration -> ADSRBuilder -> ADSRBuilder
withAttack f d adsr = adsr { attack = attacker }
 where
  attacker x = if x < d then Just . f $ x / d else Nothing

withLinearAttack :: Float -> Duration -> ADSRBuilder -> ADSRBuilder
withLinearAttack h = withAttack (* h)
