{- |
Module      : Midgard.Validators.FraudProofs.ValidationTrace.Cek
Description : CEK validation-trace preparation validator.
-}
module Midgard.Validators.FraudProofs.ValidationTrace.Cek (
  cekV1Validator,
) where

import Plutarch.LedgerApi.V3 (
  PCurrencySymbol,
  PScriptContext,
  PScriptHash,
 )
import Plutarch.Prelude

import Midgard.ValidationTrace (PValidationPhase (PCek))
import Midgard.Validators.FraudProofs.ValidationTrace.Preparation (
  pprepareSelectedValidator,
 )

cekV1Validator ::
  forall (s :: S).
  Term
    s
    ( PAsData (PBuiltinList (PAsData PScriptHash))
        :--> PAsData PCurrencySymbol
        :--> PScriptContext
        :--> PUnit
    )
cekV1Validator = pprepareSelectedValidator (pcon PCek) 4
