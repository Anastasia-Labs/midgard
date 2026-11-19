{- |
Module      : Midgard.Validators.FraudProofs.ValidationTrace.ValueAndMint
Description : ValueAndMint validation-trace preparation validator.
-}
module Midgard.Validators.FraudProofs.ValidationTrace.ValueAndMint (
  valueAndMintV1Validator,
) where

import Plutarch.LedgerApi.V3 (PCurrencySymbol, PScriptContext, PScriptHash)
import Plutarch.Prelude

import Midgard.ValidationTrace (PValidationPhase (PValueAndMint))
import Midgard.Validators.FraudProofs.ValidationTrace.Preparation (
  pprepareSelectedValidator,
 )

valueAndMintV1Validator ::
  forall s.
  Term
    s
    ( PAsData (PBuiltinList (PAsData PScriptHash))
        :--> PAsData PCurrencySymbol
        :--> PScriptContext
        :--> PUnit
    )
valueAndMintV1Validator = pprepareSelectedValidator (pcon PValueAndMint) 11
