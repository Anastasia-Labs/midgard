{- |
Module      : Midgard.Validators.FraudProofs.ValidationTrace.ValueAndMint
Description : ValueAndMint aggregate validation-trace validator.
-}
module Midgard.Validators.FraudProofs.ValidationTrace.ValueAndMint (
  valueAndMintV1Validator,
) where

import Plutarch.LedgerApi.V3 (PAddress, PCurrencySymbol, PScriptContext)
import Plutarch.Prelude

import Midgard.ValidationMachine (pverifyValueAndMintOneStepV1)
import Midgard.ValidationTrace (PValidationPhase (PValueAndMint))
import Midgard.Validators.FraudProofs.ValidationTrace.Resolution (
  pvalidateResolutionValidator,
 )

valueAndMintV1Validator :: forall s.
  Term s
    ( PAsData PCurrencySymbol
        :--> PAsData PCurrencySymbol
        :--> PAsData PAddress
        :--> PScriptContext
        :--> PUnit
    )
valueAndMintV1Validator =
  pvalidateResolutionValidator (pcon PValueAndMint) pverifyValueAndMintOneStepV1
