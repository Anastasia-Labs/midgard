{- |
Module      : Midgard.Validators.FraudProofs.ValidationTrace.AwardTimeout
Description : Validation-game award and challenger-timeout validators.
-}
module Midgard.Validators.FraudProofs.ValidationTrace.AwardTimeout (
  PFinalizeActionV1 (..),
  awardV1Validator,
  timeoutV1Validator,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP

import Plutarch.LedgerApi.V3 (
  PAddress,
  PCurrencySymbol,
  PScriptContext,
  PTxInfo (..),
 )
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.Common.Utils (pgetInclusiveBoundsOfAShortValidityRange)
import Midgard.FraudProofs.Common (pfinalize)
import Midgard.ValidationDispute (PDisputeWinner (PChallengerWins), ptimeoutWinner)
import Midgard.ValidationGame (PValidationGameStateV1 (..))
import Midgard.ValidationResolution (
  PWinningValidationResolutionStateV1,
  pwinningResolutionIsWellFormed,
 )
import Midgard.Validators.FraudProofs.Step (
  pdispatch,
  pexpectDatum,
  pexpectStateAs,
  pstep,
 )

data PFinalizeActionV1 (s :: S)
  = PFinalize
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PFinalizeActionV1)

awardV1Validator :: forall s.
  Term s
    ( PAsData PCurrencySymbol
        :--> PAsData PCurrencySymbol
        :--> PAsData PAddress
        :--> PScriptContext
        :--> PUnit
    )
awardV1Validator = plam $ \computationThreadPolicyId fraudProofPolicyId fraudProofAddress ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
  pdispatch @_ @PFinalizeActionV1 computationThreadPolicyId datum redeemer ownOutRef txInfo $
    \action -> pmatch action $ \(PFinalize inputIndex outputIndex mintRedeemerIndex) ->
      pmatch txInfo $ \PTxInfo {ptxInfo'inputs, ptxInfo'outputs, ptxInfo'redeemers} ->
        pfinalize
          computationThreadPolicyId fraudProofPolicyId fraudProofAddress
          (pexpectDatum datum)
          (pfromData inputIndex) (pfromData outputIndex) (pfromData mintRedeemerIndex)
          ownOutRef
          (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs)
          (pto $ pto $ pfromData ptxInfo'redeemers)
          $ \_inputScriptHash _assetName _fraudProver inputState ->
            pwinningResolutionIsWellFormed
              # pexpectStateAs @PWinningValidationResolutionStateV1 inputState

timeoutV1Validator :: forall s.
  Term s
    ( PAsData PCurrencySymbol
        :--> PAsData PCurrencySymbol
        :--> PAsData PAddress
        :--> PScriptContext
        :--> PUnit
    )
timeoutV1Validator = plam $ \computationThreadPolicyId fraudProofPolicyId fraudProofAddress ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
  pdispatch @_ @PFinalizeActionV1 computationThreadPolicyId datum redeemer ownOutRef txInfo $
    \action -> pmatch action $ \(PFinalize inputIndex outputIndex mintRedeemerIndex) ->
      pmatch txInfo $ \PTxInfo {ptxInfo'inputs, ptxInfo'outputs, ptxInfo'redeemers, ptxInfo'validRange} ->
      let (currentTimeLower, _) =
            pgetInclusiveBoundsOfAShortValidityRange (punsafeCoerce ptxInfo'validRange)
       in pfinalize
            computationThreadPolicyId fraudProofPolicyId fraudProofAddress
            (pexpectDatum datum)
            (pfromData inputIndex) (pfromData outputIndex) (pfromData mintRedeemerIndex)
            ownOutRef
            (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs)
            (pto $ pto $ pfromData ptxInfo'redeemers)
            $ \_inputScriptHash _assetName _fraudProver inputState ->
              pmatch (pexpectStateAs @PValidationGameStateV1 inputState) $ \state ->
                ptimeoutWinner
                  # pfromData (pvalidationGame'dispute state)
                  # currentTimeLower
                  #== pcon PChallengerWins
