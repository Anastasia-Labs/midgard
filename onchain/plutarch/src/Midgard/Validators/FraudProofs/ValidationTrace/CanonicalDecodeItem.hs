{- |
Module      : Midgard.Validators.FraudProofs.ValidationTrace.CanonicalDecodeItem
Description : Staged CanonicalDecode complete-item fraud-proof validators.

Ports @canonical-decode-item-source-v1.ak@,
@canonical-decode-item-proof-v1.ak@, and
@canonical-decode-item-settlement-v1.ak@.
-}
module Midgard.Validators.FraudProofs.ValidationTrace.CanonicalDecodeItem (
  PCanonicalDecodeItemBindActionV1 (..),
  PCanonicalDecodeItemVerifyActionV1 (..),
  PCanonicalDecodeItemSettleActionV1 (..),
  canonicalDecodeItemSourceV1Validator,
  canonicalDecodeItemProofV1Validator,
  canonicalDecodeItemSettlementV1Validator,
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

import Midgard.CanonicalDecodeItemStaging (
  PAuthenticatedCanonicalDecodeItemV1 (..),
  PObservedCanonicalDecodeItemV1 (..),
  PPreparedCanonicalDecodeItemV1 (..),
  PVerifiedCanonicalDecodeItemV1 (..),
  pauthenticatedCanonicalDecodeItemIsWellFormed,
  pobservedCanonicalDecodeItemIsWellFormed,
  pprepareCanonicalDecodeItem,
  pverifiedCanonicalDecodeItemIsWellFormed,
  pverifyCanonicalDecodeItem,
 )
import Midgard.FraudProofs.Common (pcontinue)
import Midgard.ValidationMachine (
  pbindCanonicalDecodeItemSourceV1,
  pverifyCanonicalDecodeItemObservationV1,
  pverifyCanonicalDecodeItemSuccessorV1,
 )
import Midgard.ValidationResolution (
  PPreparedValidationResolutionStateV1 (..),
  PValidationResolutionStateV1 (..),
  pwinningResolution,
 )
import Midgard.Validators.FraudProofs.Step (
  pdispatch,
  pexpectDatum,
  pexpectStateAs,
  pstep,
 )

data PCanonicalDecodeItemBindActionV1 (s :: S)
  = PBind
      { pbind'inputIndex :: Term s (PAsData PInteger)
      , pbind'outputIndex :: Term s (PAsData PInteger)
      }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PCanonicalDecodeItemBindActionV1)

data PCanonicalDecodeItemVerifyActionV1 (s :: S)
  = PVerify
      { pverify'inputIndex :: Term s (PAsData PInteger)
      , pverify'outputIndex :: Term s (PAsData PInteger)
      }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PCanonicalDecodeItemVerifyActionV1)

data PCanonicalDecodeItemSettleActionV1 (s :: S)
  = PSettle
      { psettle'inputIndex :: Term s (PAsData PInteger)
      , psettle'outputIndex :: Term s (PAsData PInteger)
      }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PCanonicalDecodeItemSettleActionV1)

canonicalDecodeItemSourceV1Validator :: forall s.
  Term s
    ( PAsData PScriptHash :--> PAsData PCurrencySymbol
        :--> PScriptContext :--> PUnit
    )
canonicalDecodeItemSourceV1Validator = plam $ \observerScriptHash policyId ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
  pdispatch @_ @PCanonicalDecodeItemBindActionV1 policyId datum redeemer ownOutRef txInfo $
    \action -> pmatch action $ \(PBind inputIndex outputIndex) ->
      pmatch txInfo $ \PTxInfo {ptxInfo'inputs, ptxInfo'outputs} ->
      pcontinue
        policyId
        (pexpectDatum datum)
        (pfromData inputIndex)
        (pfromData outputIndex)
        ownOutRef
        (pfromData ptxInfo'inputs)
        (pfromData ptxInfo'outputs)
        $ \_inputScriptHash _assetName _fraudProver inputState outputScriptHash outputState ->
          plet (pexpectStateAs @PAuthenticatedCanonicalDecodeItemV1 inputState) $ \authenticated ->
          pmatch authenticated $ \authenticatedFields ->
          plet (pfromData $ pauthenticatedCanonical'base authenticatedFields) $ \base ->
          plet (pfromData $ pauthenticatedCanonical'transition authenticatedFields) $ \transition ->
          pmatch base $ \baseFields ->
          pmatch (pfromData $ pprepared'resolution baseFields) $ \resolution ->
          plet (pfromData $ presolution'preState resolution) $ \pre ->
          plet (pbindCanonicalDecodeItemSourceV1 # pre # transition) $ \source ->
          plet
            (pforgetData $ pdata $ pprepareCanonicalDecodeItem # authenticated # source)
            $ \expectedOutputState ->
              pauthenticatedCanonicalDecodeItemIsWellFormed # authenticated
                #&& outputScriptHash #== observerScriptHash
                #&& outputState #== expectedOutputState

canonicalDecodeItemProofV1Validator :: forall s.
  Term s
    ( PAsData PScriptHash :--> PAsData PCurrencySymbol
        :--> PScriptContext :--> PUnit
    )
canonicalDecodeItemProofV1Validator = plam $ \successorScriptHash policyId ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
  pdispatch @_ @PCanonicalDecodeItemVerifyActionV1 policyId datum redeemer ownOutRef txInfo $
    \action -> pmatch action $ \(PVerify inputIndex outputIndex) ->
      pmatch txInfo $ \PTxInfo {ptxInfo'inputs, ptxInfo'outputs} ->
      pcontinue
        policyId
        (pexpectDatum datum)
        (pfromData inputIndex)
        (pfromData outputIndex)
        ownOutRef
        (pfromData ptxInfo'inputs)
        (pfromData ptxInfo'outputs)
        $ \_inputScriptHash _assetName _fraudProver inputState outputScriptHash outputState ->
          plet (pexpectStateAs @PObservedCanonicalDecodeItemV1 inputState) $ \observed ->
          pmatch observed $ \observedFields ->
          plet (pfromData $ pobservedCanonical'prepared observedFields) $ \prepared ->
          pmatch prepared $ \preparedFields ->
          plet (pfromData $ ppreparedCanonical'authenticated preparedFields) $ \authenticated ->
          pmatch authenticated $ \authenticatedFields ->
          plet (pfromData $ pauthenticatedCanonical'transition authenticatedFields) $ \transition ->
          plet (pfromData $ ppreparedCanonical'source preparedFields) $ \source ->
          plet (pfromData $ pobservedCanonical'observation observedFields) $ \observation ->
          plet
            ( pverifyCanonicalDecodeItemObservationV1
                # transition # source # observation
            )
            $ \proof ->
          plet
            (pforgetData $ pdata $ pverifyCanonicalDecodeItem # observed # proof)
            $ \expectedOutputState ->
              pobservedCanonicalDecodeItemIsWellFormed # observed
                #&& outputScriptHash #== successorScriptHash
                #&& outputState #== expectedOutputState

canonicalDecodeItemSettlementV1Validator :: forall s.
  Term s
    ( PAsData PScriptHash :--> PAsData PCurrencySymbol
        :--> PScriptContext :--> PUnit
    )
canonicalDecodeItemSettlementV1Validator = plam $ \awardScriptHash policyId ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
  pdispatch @_ @PCanonicalDecodeItemSettleActionV1 policyId datum redeemer ownOutRef txInfo $
    \action -> pmatch action $ \(PSettle inputIndex outputIndex) ->
      pmatch txInfo $ \PTxInfo {ptxInfo'inputs, ptxInfo'outputs} ->
      pcontinue
        policyId
        (pexpectDatum datum)
        (pfromData inputIndex)
        (pfromData outputIndex)
        ownOutRef
        (pfromData ptxInfo'inputs)
        (pfromData ptxInfo'outputs)
        $ \_inputScriptHash _assetName _fraudProver inputState outputScriptHash outputState ->
          plet (pexpectStateAs @PVerifiedCanonicalDecodeItemV1 inputState) $ \verified ->
          pmatch verified $ \verifiedFields ->
          plet (pfromData $ pverifiedCanonical'observed verifiedFields) $ \observed ->
          pmatch observed $ \observedFields ->
          plet (pfromData $ pobservedCanonical'prepared observedFields) $ \prepared ->
          pmatch prepared $ \preparedFields ->
          plet (pfromData $ ppreparedCanonical'authenticated preparedFields) $ \authenticated ->
          pmatch authenticated $ \authenticatedFields ->
          plet (pfromData $ pauthenticatedCanonical'base authenticatedFields) $ \base ->
          pmatch base $ \baseFields ->
          pmatch (pfromData $ pprepared'resolution baseFields) $ \resolution ->
          plet (pfromData $ presolution'preState resolution) $ \pre ->
          plet (pfromData $ pauthenticatedCanonical'transition authenticatedFields) $ \transition ->
          plet (pfromData $ ppreparedCanonical'source preparedFields) $ \source ->
          plet (pfromData $ pverifiedCanonical'proof verifiedFields) $ \proof ->
            pverifiedCanonicalDecodeItemIsWellFormed # verified
              #&& pverifyCanonicalDecodeItemSuccessorV1
                # pre # transition # source # proof
              #&& outputScriptHash #== awardScriptHash
              #&& outputState #== pforgetData (pdata pwinningResolution)
