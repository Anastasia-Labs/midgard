{- |
Module      : Midgard.Validators.FraudProofs.ValidationTrace.CanonicalDecodeItemEvidence
Description : CanonicalDecode item evidence staging validators.

Ports @canonical-decode-item-observe-v1.ak@,
@canonical-decode-item-semantic-v1.ak@, and @proof-item-v1.ak@.
-}
module Midgard.Validators.FraudProofs.ValidationTrace.CanonicalDecodeItemEvidence (
  PCanonicalDecodeItemObserveActionV1 (..),
  PCanonicalDecodeItemSemanticActionV1 (..),
  canonicalDecodeItemObserveV1Validator,
  canonicalDecodeItemSemanticV1Validator,
  canonicalDecodeProofItemV1Validator,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP

import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.LedgerApi.V3 (
  PAddress (..),
  PCredential (..),
  PCurrencySymbol,
  POutputDatum (..),
  PScriptContext,
  PScriptHash,
  PTxInInfo (..),
  PTxInfo (..),
  PTxOut (..),
  PTxOutRef,
 )
import Plutarch.Prelude
import Plutarch.Monadic qualified as P
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.BoundedCollection (PItemProofV1)
import Midgard.CanonicalDecodeItemStaging (
  PAuthenticatedCanonicalDecodeItemV1 (..),
  PPreparedCanonicalDecodeItemV1 (..),
  pauthenticateCanonicalDecodeItem,
  pobserveCanonicalDecodeItem,
  ppreparedCanonicalDecodeItemIsWellFormed,
 )
import Midgard.ComputationThread (PStepDatum (..))
import Midgard.FraudProofs.Common (pcontinue)
import Midgard.ValidationMachine (
  PValidationAuxiliaryWitnessV1 (..),
  PValidationOneStepWitnessV1,
  PValidationProofItemDatumV1 (..),
  pobserveCanonicalDecodeItemV1,
 )
import Midgard.ValidationResolution (
  PPreparedValidationResolutionStateV1 (..),
  PValidationResolutionStateV1 (..),
  phashOneStepEvidence,
 )
import Midgard.ValidationTrace (PValidationMachineStateV1 (..))
import Midgard.Validators.FraudProofs.Step (
  pdispatch,
  pexpectDatum,
  pexpectStateAs,
  pstep,
 )

data PCanonicalDecodeItemObserveActionV1 (s :: S)
  = PObserve
      { pobserve'inputIndex :: Term s (PAsData PInteger)
      , pobserve'outputIndex :: Term s (PAsData PInteger)
      , pobserve'collectionProof :: Term s (PAsData PItemProofV1)
      , pobserve'itemCbor :: Term s (PAsData PByteString)
      }
  | PObserveReference
      { pobserveReference'inputIndex :: Term s (PAsData PInteger)
      , pobserveReference'outputIndex :: Term s (PAsData PInteger)
      , pobserveReference'referenceInputIndex :: Term s (PAsData PInteger)
      }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PCanonicalDecodeItemObserveActionV1)

data PCanonicalDecodeItemSemanticActionV1 (s :: S)
  = PVerify
      { pverify'inputIndex :: Term s (PAsData PInteger)
      , pverify'outputIndex :: Term s (PAsData PInteger)
      , pverify'transition :: Term s (PAsData PValidationOneStepWitnessV1)
      , pverify'collectionProof :: Term s (PAsData PItemProofV1)
      , pverify'itemCbor :: Term s (PAsData PByteString)
      }
  | PVerifyReference
      { pverifyReference'inputIndex :: Term s (PAsData PInteger)
      , pverifyReference'outputIndex :: Term s (PAsData PInteger)
      , pverifyReference'transition :: Term s (PAsData PValidationOneStepWitnessV1)
      , pverifyReference'referenceInputIndex :: Term s (PAsData PInteger)
      }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PCanonicalDecodeItemSemanticActionV1)

pproofItemFromReference :: forall s.
  Term s (PAsData PScriptHash) ->
  Term s PValidationMachineStateV1 ->
  Term s PInteger ->
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  Term s PValidationAuxiliaryWitnessV1
pproofItemFromReference proofItemScriptHash pre referenceInputIndex referenceInputs =
  pif (referenceInputIndex #>= 0) (P.do
    PTxInInfo {ptxInInfo'resolved} <-
      pmatch (pfromData $ pelemAt # referenceInputIndex # referenceInputs)
    PTxOut {ptxOut'address, ptxOut'datum, ptxOut'referenceScript} <-
      pmatch ptxInInfo'resolved
    PAddress {paddress'credential} <- pmatch ptxOut'address
    actualScriptHash <- plet $ pmatch paddress'credential $ \case
      PScriptCredential scriptHash -> pfromData scriptHash
      PPubKeyCredential _ -> perror
    itemData <- plet $ pmatch ptxOut'datum $ \case
      POutputDatum {poutputDatum'outputDatum} -> pto poutputDatum'outputDatum
      _ -> perror
    pif
      ( pmatch ptxOut'referenceScript $ \case
          PDNothing -> pconstant True
          PDJust _ -> pconstant False
      )
      (P.do
        proofItem <-
          plet $ pfromData $ punsafeCoerce @(PAsData PValidationProofItemDatumV1) itemData
        pmatch pre $ \preFields ->
          pmatch proofItem $ \proofItemFields ->
          pif
            ( actualScriptHash #== pfromData proofItemScriptHash
                #&& pfromData (pproofItem'version proofItemFields) #== 1
                #&& pfromData (pproofItem'transactionId proofItemFields)
                  #== pfromData (pmachineState'transactionId preFields)
                #&& pfromData (pproofItem'transactionCommitment proofItemFields)
                  #== pfromData (pmachineState'transactionCommitment preFields)
            )
            ( pcon $ PTransactionFieldItemWitness
                (pproofItem'collectionProof proofItemFields)
                (pproofItem'itemCbor proofItemFields)
            )
            perror)
      perror)
    perror

pobserveSemanticPreState :: forall s.
  Term s (PMaybeData PStepDatum) ->
  Term s PValidationMachineStateV1
pobserveSemanticPreState datum =
  pmatch (pexpectDatum datum) $ \stepDatum ->
  plet (pexpectStateAs @PPreparedCanonicalDecodeItemV1 $ pstep'data stepDatum) $ \prepared ->
  pmatch prepared $ \preparedFields ->
  pmatch (pfromData $ ppreparedCanonical'authenticated preparedFields) $ \authenticated ->
  pmatch (pfromData $ pauthenticatedCanonical'base authenticated) $ \base ->
  pmatch (pfromData $ pprepared'resolution base) $ \resolution ->
    pfromData $ presolution'preState resolution

psemanticPreState :: forall s.
  Term s (PMaybeData PStepDatum) ->
  Term s PValidationMachineStateV1
psemanticPreState datum =
  pmatch (pexpectDatum datum) $ \stepDatum ->
  plet (pexpectStateAs @PPreparedValidationResolutionStateV1 $ pstep'data stepDatum) $ \base ->
  pmatch base $ \baseFields ->
  pmatch (pfromData $ pprepared'resolution baseFields) $ \resolution ->
    pfromData $ presolution'preState resolution

pobserveItem :: forall s.
  Term s (PAsData PScriptHash) ->
  Term s (PAsData PCurrencySymbol) ->
  Term s (PMaybeData PStepDatum) ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PValidationAuxiliaryWitnessV1 ->
  Term s PTxOutRef ->
  Term s PTxInfo ->
  Term s PBool
pobserveItem proofVerifierScriptHash policyId datum inputIndex outputIndex auxiliary ownOutRef txInfo =
  pmatch auxiliary $ \case
    PTransactionFieldItemWitness collectionProof itemCbor ->
      pmatch txInfo $ \PTxInfo {ptxInfo'inputs, ptxInfo'outputs} ->
      pcontinue
        policyId
        (pexpectDatum datum)
        inputIndex
        outputIndex
        ownOutRef
        (pfromData ptxInfo'inputs)
        (pfromData ptxInfo'outputs)
        $ \_inputScriptHash _assetName _fraudProver inputState outputScriptHash outputState ->
          plet (pexpectStateAs @PPreparedCanonicalDecodeItemV1 inputState) $ \prepared ->
          pmatch prepared $ \preparedFields ->
          plet (pfromData $ ppreparedCanonical'authenticated preparedFields) $ \authenticated ->
          pmatch authenticated $ \authenticatedFields ->
          plet (pfromData $ pauthenticatedCanonical'base authenticatedFields) $ \base ->
          pmatch base $ \baseFields ->
          plet (pfromData $ pauthenticatedCanonical'transition authenticatedFields) $ \transition ->
          plet
            ( pobserveCanonicalDecodeItemV1
                # transition
                # pfromData collectionProof
                # pfromData itemCbor
            )
            $ \observation ->
          plet
            (pforgetData $ pdata $ pobserveCanonicalDecodeItem # prepared # observation)
            $ \expectedOutputState ->
              ppreparedCanonicalDecodeItemIsWellFormed # prepared
                #&& phashOneStepEvidence
                  # pforgetData (pauthenticatedCanonical'transition authenticatedFields)
                  # pforgetData (pdata auxiliary)
                  #== pfromData (pprepared'evidenceHash baseFields)
                #&& outputScriptHash #== proofVerifierScriptHash
                #&& outputState #== expectedOutputState
    _ -> perror

pbindItemSource :: forall s.
  Term s (PAsData PScriptHash) ->
  Term s (PAsData PCurrencySymbol) ->
  Term s (PMaybeData PStepDatum) ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PValidationOneStepWitnessV1 ->
  Term s PValidationAuxiliaryWitnessV1 ->
  Term s PTxOutRef ->
  Term s PTxInfo ->
  Term s PBool
pbindItemSource sourceBinderScriptHash policyId datum inputIndex outputIndex transition auxiliary ownOutRef txInfo =
  pmatch auxiliary $ \case
    PTransactionFieldItemWitness _ _ ->
      pmatch txInfo $ \PTxInfo {ptxInfo'inputs, ptxInfo'outputs} ->
      pcontinue
        policyId
        (pexpectDatum datum)
        inputIndex
        outputIndex
        ownOutRef
        (pfromData ptxInfo'inputs)
        (pfromData ptxInfo'outputs)
        $ \_inputScriptHash _assetName _fraudProver inputState outputScriptHash outputState ->
          plet (pexpectStateAs @PPreparedValidationResolutionStateV1 inputState) $ \base ->
          pmatch base $ \baseFields ->
          plet
            (pforgetData $ pdata $ pauthenticateCanonicalDecodeItem # base # transition)
            $ \expectedOutputState ->
              phashOneStepEvidence
                # pforgetData (pdata transition)
                # pforgetData (pdata auxiliary)
                #== pfromData (pprepared'evidenceHash baseFields)
                #&& outputScriptHash #== sourceBinderScriptHash
                #&& outputState #== expectedOutputState
    _ -> perror

canonicalDecodeItemObserveV1Validator :: forall s.
  Term s
    ( PAsData PScriptHash :--> PAsData PCurrencySymbol
        :--> PAsData PScriptHash :--> PScriptContext :--> PUnit
    )
canonicalDecodeItemObserveV1Validator = plam $ \proofVerifierScriptHash policyId proofItemScriptHash ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
  pdispatch @_ @PCanonicalDecodeItemObserveActionV1 policyId datum redeemer ownOutRef txInfo $
    \action -> pmatch action $ \case
      PObserve inputIndex outputIndex collectionProof itemCbor ->
        pobserveItem
          proofVerifierScriptHash policyId datum
          (pfromData inputIndex) (pfromData outputIndex)
          (pcon $ PTransactionFieldItemWitness collectionProof itemCbor)
          ownOutRef txInfo
      PObserveReference inputIndex outputIndex referenceInputIndex ->
        pmatch txInfo $ \PTxInfo {ptxInfo'referenceInputs} ->
        plet
          ( pproofItemFromReference
              proofItemScriptHash
              (pobserveSemanticPreState datum)
              (pfromData referenceInputIndex)
              (pfromData ptxInfo'referenceInputs)
          )
          $ \auxiliary ->
            pobserveItem
              proofVerifierScriptHash policyId datum
              (pfromData inputIndex) (pfromData outputIndex)
              auxiliary ownOutRef txInfo

canonicalDecodeItemSemanticV1Validator :: forall s.
  Term s
    ( PAsData PScriptHash :--> PAsData PCurrencySymbol
        :--> PAsData PScriptHash :--> PScriptContext :--> PUnit
    )
canonicalDecodeItemSemanticV1Validator = plam $ \sourceBinderScriptHash policyId proofItemScriptHash ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
  pdispatch @_ @PCanonicalDecodeItemSemanticActionV1 policyId datum redeemer ownOutRef txInfo $
    \action -> pmatch action $ \case
      PVerify inputIndex outputIndex transition collectionProof itemCbor ->
        pbindItemSource
          sourceBinderScriptHash policyId datum
          (pfromData inputIndex) (pfromData outputIndex)
          (pfromData transition)
          (pcon $ PTransactionFieldItemWitness collectionProof itemCbor)
          ownOutRef txInfo
      PVerifyReference inputIndex outputIndex transition referenceInputIndex ->
        pmatch txInfo $ \PTxInfo {ptxInfo'referenceInputs} ->
        plet
          ( pproofItemFromReference
              proofItemScriptHash
              (psemanticPreState datum)
              (pfromData referenceInputIndex)
              (pfromData ptxInfo'referenceInputs)
          )
          $ \auxiliary ->
            pbindItemSource
              sourceBinderScriptHash policyId datum
              (pfromData inputIndex) (pfromData outputIndex)
              (pfromData transition) auxiliary ownOutRef txInfo

canonicalDecodeProofItemV1Validator :: forall s. Term s (PScriptContext :--> PUnit)
canonicalDecodeProofItemV1Validator = plam $ \_ -> perror
