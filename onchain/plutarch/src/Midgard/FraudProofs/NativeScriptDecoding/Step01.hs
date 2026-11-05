module Midgard.FraudProofs.NativeScriptDecoding.Step01 (PStep01Args (..)) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Prelude

import Midgard.FraudProofs.Common (PNativeTxInclusionCarriage)

data PStep01Args s
  = PBindNormalTransaction (Term s (PAsData PNativeTxInclusionCarriage))
  | PRecordForcedSource
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep01Args)
