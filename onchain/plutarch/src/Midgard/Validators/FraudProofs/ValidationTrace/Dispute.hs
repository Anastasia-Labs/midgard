{- |
Module      : Midgard.Validators.FraudProofs.ValidationTrace.Dispute
Description : Validation dispute opening validator.
-}
module Midgard.Validators.FraudProofs.ValidationTrace.Dispute (
  POpenActionV1 (..),
  disputeV1Validator,
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

import Midgard.Common.Utils (pgetInclusiveBoundsOfAShortValidityRange, phasSigned)
import Midgard.FraudProofCatalogue (pidByteCount)
import Midgard.FraudProofs.Common (pcontinue)
import Midgard.HubOracle (PHubOracleDatum (..), pgetDatum)
import Midgard.LedgerState (PHeaderV1 (..), pblockMaturityDurationV1)
import Midgard.StateQueue (pgetBlockDatumV1)
import Midgard.ValidationClaim (
  PValidationClaimWitnessV1,
  pcommittedClaimStructureIsValid,
 )
import Midgard.ValidationDispute (pcanOpenBeforeMaturity)
import Midgard.ValidationGame (PPendingValidationClaimV1 (..))
import Midgard.ValidationTrace (PValidationTraceDescriptorV1)
import Midgard.Validators.FraudProofs.Step (
  pdispatch,
  pexpectDatum,
  pstateIsAbsent,
  pstep,
 )

data POpenActionV1 (s :: S)
  = POpen
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PValidationClaimWitnessV1))
      (Term s (PAsData PValidationTraceDescriptorV1))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct POpenActionV1)

disputeV1Validator :: forall s.
  Term s
    ( PAsData PScriptHash
        :--> PAsData PCurrencySymbol
        :--> PAsData PScriptHash
        :--> PScriptContext
        :--> PUnit
    )
disputeV1Validator = plam $ \sourceScriptHash computationThreadPolicyId hubOracle ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
  pdispatch @_ @POpenActionV1 computationThreadPolicyId datum redeemer ownOutRef txInfo $
    \action -> pmatch action $ \(POpen inputIndex outputIndex hubRefInputIndex stateQueueRefInputIndex claimD challengerDescriptorD) ->
      pmatch txInfo $ \PTxInfo {ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs, ptxInfo'signatories, ptxInfo'validRange} ->
      let (_, currentTimeUpper) =
            pgetInclusiveBoundsOfAShortValidityRange (punsafeCoerce ptxInfo'validRange)
          referenceInputs = pfromData ptxInfo'referenceInputs
       in pmatch
            (pgetDatum # referenceInputs # hubOracle # pfromData hubRefInputIndex)
            $ \hubDatum ->
            pcontinue
              computationThreadPolicyId
              (pexpectDatum datum)
              (pfromData inputIndex) (pfromData outputIndex)
              ownOutRef
              (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs)
              $ \_inputScriptHash assetName fraudProver inputState outputScriptHash outputState ->
                pif
                  (pstateIsAbsent inputState #&& phasSigned # fraudProver # pfromData ptxInfo'signatories)
                  ( pgetBlockDatumV1
                      referenceInputs
                      (phubOracle'stateQueue hubDatum)
                      (pfromData stateQueueRefInputIndex)
                      $ \headerD headerHash ->
                        plet (pfromData headerD) $ \header ->
                        plet (pfromData claimD) $ \claim ->
                        plet (pto $ pfromData assetName) $ \assetNameBytes ->
                        plet
                          ( pcon $ PPendingValidationClaimV1
                              (pdata headerHash)
                              headerD
                              claimD
                              challengerDescriptorD
                              (pdata currentTimeUpper)
                          )
                          $ \expectedState ->
                            pfromData outputScriptHash #== pfromData sourceScriptHash
                              #&& headerHash
                                #== psliceBS
                                  # pidByteCount
                                  # (plengthBS # assetNameBytes - pidByteCount)
                                  # assetNameBytes
                              #&& pcommittedClaimStructureIsValid header claim
                              #&& ( pmatch header $ \headerFields ->
                                      pcanOpenBeforeMaturity
                                        # currentTimeUpper
                                        # pfromData (pheader'endTime headerFields)
                                        # pblockMaturityDurationV1
                                  )
                              #&& outputState #== pforgetData (pdata expectedState)
                  )
                  perror
