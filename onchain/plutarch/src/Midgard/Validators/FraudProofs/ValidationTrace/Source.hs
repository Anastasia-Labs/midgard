{- |
Module      : Midgard.Validators.FraudProofs.ValidationTrace.Source
Description : Validation-claim source verification validator.
-}
module Midgard.Validators.FraudProofs.ValidationTrace.Source (
  PVerifySourceActionV1 (..),
  sourceV1Validator,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP

import Plutarch.LedgerApi.V3 (
  PCurrencySymbol,
  PScriptContext,
  PScriptHash,
  PTxInfo (..),
 )
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.Common.Utils (pgetInclusiveBoundsOfAShortValidityRange)
import Midgard.FraudProofs.Common (pcontinue)
import Midgard.LedgerState (PHeaderV1 (..), pblockMaturityDurationV1)
import Midgard.TransitionTrace (PRootMembershipProof (..))
import Midgard.ValidationClaim (
  PValidationClaimWitnessV1 (..),
  pcommittedClaimEndpointsAndSourceAreValid,
  pcommittedClaimSourceIsAuthenticated,
 )
import Midgard.ValidationDispute (popenAfterSourceVerification)
import Midgard.ValidationGame (
  PPendingValidationClaimV1 (..),
  PValidationGameStateV1 (..),
 )
import Midgard.ValidationResolution (pwinningResolution)
import Midgard.ValidationTrace (PValidationTraceDescriptorV1)
import Midgard.Validators.FraudProofs.Step (
  pdispatch,
  pexpectDatum,
  pexpectStateAs,
  pstep,
 )

data PVerifySourceActionV1 (s :: S)
  = PVerifySource
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PVerifySourceActionV1)

sourceV1Validator :: forall s.
  Term s
    ( PAsData PScriptHash
        :--> PAsData PScriptHash
        :--> PAsData PCurrencySymbol
        :--> PScriptContext
        :--> PUnit
    )
sourceV1Validator = plam $ \gameScriptHash awardScriptHash computationThreadPolicyId ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
  pdispatch @_ @PVerifySourceActionV1 computationThreadPolicyId datum redeemer ownOutRef txInfo $
    \action -> pmatch action $ \(PVerifySource inputIndex outputIndex) ->
      pmatch txInfo $ \PTxInfo {ptxInfo'inputs, ptxInfo'outputs, ptxInfo'validRange} ->
      let (_, sourceTimeUpper) =
            pgetInclusiveBoundsOfAShortValidityRange (punsafeCoerce ptxInfo'validRange)
       in pcontinue
            computationThreadPolicyId
            (pexpectDatum datum)
            (pfromData inputIndex) (pfromData outputIndex)
            ownOutRef
            (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs)
            $ \_inputScriptHash _assetName _fraudProver inputState outputScriptHash outputState ->
              pmatch (pexpectStateAs @PPendingValidationClaimV1 inputState) $ \state ->
              plet (pfromData $ ppendingClaim'challengedHeader state) $ \header ->
              plet (pfromData $ ppendingClaim'claim state) $ \claim ->
              pcommittedClaimSourceIsAuthenticated header claim
                #&& pif
                  (pcommittedClaimEndpointsAndSourceAreValid header claim)
                  ( pmatch header $ \headerFields ->
                    pmatch claim $ \claimFields ->
                    pmatch (pfromData $ pclaim'descriptorMembership claimFields) $ \membership ->
                    plet
                      ( pfromData
                          (punsafeCoerce @(PAsData PValidationTraceDescriptorV1) (prootMembership'value membership))
                      )
                      $ \operatorDescriptor ->
                      plet
                        ( popenAfterSourceVerification
                            # operatorDescriptor
                            # pfromData (ppendingClaim'challengerDescriptor state)
                            # pfromData (ppendingClaim'openTimeUpper state)
                            # sourceTimeUpper
                            # pfromData (pheader'endTime headerFields)
                            # pblockMaturityDurationV1
                        )
                        $ \expectedDispute ->
                        plet
                          ( pcon $ PValidationGameStateV1
                              (ppendingClaim'challengedHeaderHash state)
                              (pheader'operatorVkey headerFields)
                              (pdata expectedDispute)
                          )
                          $ \expectedState ->
                            pfromData outputScriptHash #== pfromData gameScriptHash
                              #&& outputState #== pforgetData (pdata expectedState)
                  )
                  ( pfromData outputScriptHash #== pfromData awardScriptHash
                      #&& outputState #== pforgetData (pdata pwinningResolution)
                  )
