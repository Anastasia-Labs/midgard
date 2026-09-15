{- |
Module      : DesignPatterns.ValidityRangeNormalization
Description : Plutarch port of
              @aiken-design-patterns/validity-range-normalization.ak@ (v1.7.0).

Cardano's validity range admits meaningless and redundant values: the
inclusivity flags mean a single interval has several representations, and some
combinations are nonsense. This narrows it to a normalised form in which every
bound is inclusive.
-}
module DesignPatterns.ValidityRangeNormalization (
  PNormalizedTimeRange (..),
  pnormalizeTimeRange,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.LedgerApi.Interval (PExtended (..), PInterval (..), PLowerBound (..), PUpperBound (..))
import Plutarch.LedgerApi.V3 (PPosixTime)
import Plutarch.Monadic qualified as P
import Plutarch.Prelude

{- | Aiken @NormalizedTimeRange@.

__Data-encoded__, and it has to be. Most uses of this type are internal — the
validity helpers in "Midgard.Common.Utils" produce and consume it inside one
script — but @invalid_range/step_02.State@ carries one in a /datum/, so its
@Constr@ tags are wire format and Scott encoding would put bytes on chain that no
SDK could build. The tags are the declaration order: @ClosedRange@ 0 (two
fields), @FromNegInf@ 1, @ToPosInf@ 2, @Always@ 3, @InvalidRange@ 4.

@InvalidRange@ is not "no range": it is a range whose lower bound exceeds its
upper, which nothing can satisfy. That is why @invalid-range@'s step-02 convicts
on it unconditionally while @Always@ makes the step abort — an unbounded range is
not a fault, and a step reaching it was built on a false premise.
-}
data PNormalizedTimeRange (s :: S)
  = PClosedRange
      { pntr'lower :: Term s (PAsData PInteger)
      , pntr'upper :: Term s (PAsData PInteger)
      }
  | PFromNegInf {pntr'upperOnly :: Term s (PAsData PInteger)}
  | PToPosInf {pntr'lowerOnly :: Term s (PAsData PInteger)}
  | PAlways
  | PInvalidRange
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PNormalizedTimeRange)

{- | Aiken @normalize_time_range@.

Every bound comes back inclusive: an exclusive lower bound becomes @lower + 1@
and an exclusive upper becomes @upper - 1@.

The @lower >= upper@ check is redundant on-chain — Cardano's phase-1 validation
already rejects such a transaction — but the Aiken original re-performs it so
the helper stays correct for a @ValidityRange@ from any source, and the port
keeps it for the same reason.
-}
pnormalizeTimeRange ::
  forall (s :: S).
  Term s (PInterval PPosixTime :--> PNormalizedTimeRange)
pnormalizeTimeRange = phoistAcyclic $
  plam $ \validityRange -> P.do
    PInterval {pinterval'from, pinterval'to} <- pmatch validityRange
    PLowerBound lowerBound lowerClosed <- pmatch pinterval'from
    PUpperBound upperBound upperClosed <- pmatch pinterval'to
    let resolveLower x = pif (pfromData lowerClosed) x (x + 1)
        resolveUpper x = pif (pfromData upperClosed) x (x - 1)
    pmatch lowerBound $ \case
      PNegInf ->
        pmatch upperBound $ \case
          PPosInf -> pcon PAlways
          PFinite u -> pcon (PFromNegInf (pdata (resolveUpper (pto (pfromData u)))))
          PNegInf -> pcon PInvalidRange
      PFinite l -> P.do
        lower <- plet $ resolveLower (pto (pfromData l))
        pmatch upperBound $ \case
          PPosInf -> pcon (PToPosInf (pdata lower))
          PFinite u -> P.do
            upper <- plet $ resolveUpper (pto (pfromData u))
            pif (lower #>= upper) (pcon PInvalidRange) (pcon (PClosedRange (pdata lower) (pdata upper)))
          PNegInf -> pcon PInvalidRange
      PPosInf -> pcon PInvalidRange
