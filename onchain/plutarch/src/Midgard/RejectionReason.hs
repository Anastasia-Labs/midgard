{-# LANGUAGE OverloadedStrings #-}

module Midgard.RejectionReason (
  POperatorVerdictV1 (..),
  PRejectionReasonV1 (..),
  prejectionCodeOf,
  prejectFieldPreimageSize,
  prejectAssetCount,
  prejectInvalidFieldType,
  prejectNativeScriptDepth,
  prejectNativeScriptNodeCount,
  prejectMinAda,
  prejectEmptyInputs,
  prejectDuplicateInput,
  prejectNetworkIdMismatch,
  prejectMinFee,
  prejectInvalidValidityIntervalFormat,
  prejectMissingRequiredWitness,
  prejectInvalidSignature,
  prejectNativeScriptInvalid,
  prejectPlutusScriptInvalid,
  prejectValidityIntervalMismatch,
  prejectInputNotFound,
  prejectInvalidOutput,
  prejectValueNotPreserved,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Prelude

-- | Aiken @rejection_reason_v1.OperatorVerdictV1@.
data POperatorVerdictV1 (s :: S)
  = PForcedTxValid
  | PForcedTxInvalid (Term s PData)
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct POperatorVerdictV1)

-- | Aiken's wire-normative rejection catalogue, in declaration order.
data PRejectionReasonV1 (s :: S)
  = PFieldPreimageLengthMismatch (Term s (PAsData PInteger))
  | PFieldItemWidthIllegal (Term s (PAsData PInteger)) (Term s (PAsData PInteger))
  | PEmptyInputs
  | PDuplicateInput
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
  | PValidityIntervalMalformed
  | PNetworkIdMismatch
  | PFeeBelowMinimum
  | PAddressWitnessSignatureInvalid (Term s (PAsData PInteger))
  | PRequiredSignerUnsigned (Term s (PAsData PInteger))
  | PWitnessScriptHeaderMalformed (Term s (PAsData PInteger))
  | PWitnessNativeScriptMalformed (Term s (PAsData PInteger))
  | PWitnessNativeScriptNodeLimit (Term s (PAsData PInteger))
  | PWitnessNativeScriptDepthLimit (Term s (PAsData PInteger))
  | PWitnessNativeScriptFalse (Term s (PAsData PInteger))
  | PScriptIntegrityHashMissing
  | PObserversForbiddenOnUntaggedNetwork
  | PObserverOrderInvalid (Term s (PAsData PInteger))
  | PValidityIntervalExcludesBlockSlot
  | PInputNotFound (Term s (PAsData PInteger)) (Term s (PAsData PInteger))
  | PInputSpentOutputNonCanonical (Term s (PAsData PInteger)) (Term s (PAsData PInteger))
  | PResolvedReferenceScriptMalformed (Term s (PAsData PInteger)) (Term s (PAsData PInteger))
  | PResolvedReferenceScriptNodeLimit (Term s (PAsData PInteger)) (Term s (PAsData PInteger))
  | PResolvedReferenceScriptDepthLimit (Term s (PAsData PInteger)) (Term s (PAsData PInteger))
  | PSpendInputSignerMissing (Term s (PAsData PInteger))
  | PRedeemerMalformed (Term s (PAsData PInteger))
  | POutputNonCanonical (Term s (PAsData PInteger))
  | POutputReferenceScriptMalformed (Term s (PAsData PInteger))
  | POutputReferenceScriptNodeLimit (Term s (PAsData PInteger))
  | POutputReferenceScriptDepthLimit (Term s (PAsData PInteger))
  | PProtectedOutputSignerMissing (Term s (PAsData PInteger))
  | PMintDeclaredAssetLimit (Term s (PAsData PInteger))
  | PScriptSourceMissing (Term s (PAsData PInteger)) (Term s (PAsData PInteger))
  | PRedeemerMissing (Term s (PAsData PInteger)) (Term s (PAsData PInteger))
  | PUnusedScriptWitness (Term s (PAsData PInteger))
  | PUnusedRedeemer (Term s (PAsData PInteger))
  | PExecutionNativeScriptMalformed (Term s (PAsData PInteger))
  | PExecutionNativeScriptNodeLimit (Term s (PAsData PInteger))
  | PExecutionNativeScriptDepthLimit (Term s (PAsData PInteger))
  | PExecutionNativeScriptFalse (Term s (PAsData PInteger))
  | PScriptIntegrityHashMismatch
  | PReceivePurposePlutusV3Forbidden (Term s (PAsData PInteger))
  | PPlutusExecutionFailed (Term s (PAsData PInteger))
  | PInputAssetAccumulationLimit (Term s (PAsData PInteger)) (Term s (PAsData PInteger))
  | POutputAssetAccumulationLimit (Term s (PAsData PInteger)) (Term s (PAsData PInteger))
  | PMintAssetAccumulationLimit (Term s (PAsData PInteger))
  | POutputBelowMinAda (Term s (PAsData PInteger))
  | PValueNotPreserved
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PRejectionReasonV1)

prejectFieldPreimageSize, prejectAssetCount, prejectInvalidFieldType :: forall s. Term s PByteString
prejectFieldPreimageSize = pconstant "E_FIELD_PREIMAGE_SIZE"
prejectAssetCount = pconstant "E_ASSET_COUNT"
prejectInvalidFieldType = pconstant "E_INVALID_FIELD_TYPE"

prejectNativeScriptDepth, prejectNativeScriptNodeCount, prejectMinAda :: forall s. Term s PByteString
prejectNativeScriptDepth = pconstant "E_NATIVE_SCRIPT_DEPTH"
prejectNativeScriptNodeCount = pconstant "E_NATIVE_SCRIPT_NODE_COUNT"
prejectMinAda = pconstant "E_MIN_ADA"

prejectEmptyInputs, prejectDuplicateInput, prejectNetworkIdMismatch :: forall s. Term s PByteString
prejectEmptyInputs = pconstant "E_EMPTY_INPUTS"
prejectDuplicateInput = pconstant "E_DUPLICATE_INPUT_IN_TX"
prejectNetworkIdMismatch = pconstant "E_NETWORK_ID_MISMATCH"

prejectMinFee, prejectInvalidValidityIntervalFormat, prejectMissingRequiredWitness :: forall s. Term s PByteString
prejectMinFee = pconstant "E_MIN_FEE"
prejectInvalidValidityIntervalFormat = pconstant "E_INVALID_VALIDITY_INTERVAL_FORMAT"
prejectMissingRequiredWitness = pconstant "E_MISSING_REQUIRED_WITNESS"

prejectInvalidSignature, prejectNativeScriptInvalid, prejectPlutusScriptInvalid :: forall s. Term s PByteString
prejectInvalidSignature = pconstant "E_INVALID_SIGNATURE"
prejectNativeScriptInvalid = pconstant "E_NATIVE_SCRIPT_INVALID"
prejectPlutusScriptInvalid = pconstant "E_PLUTUS_SCRIPT_INVALID"

prejectValidityIntervalMismatch, prejectInputNotFound, prejectInvalidOutput :: forall s. Term s PByteString
prejectValidityIntervalMismatch = pconstant "E_VALIDITY_INTERVAL_MISMATCH"
prejectInputNotFound = pconstant "E_INPUT_NOT_FOUND"
prejectInvalidOutput = pconstant "E_INVALID_OUTPUT"

prejectValueNotPreserved :: forall s. Term s PByteString
prejectValueNotPreserved = pconstant "E_VALUE_NOT_PRESERVED"

-- | Total 47-to-19 bridge back to the frozen descriptor code space.
prejectionCodeOf :: forall s. Term s (PData :--> PByteString)
prejectionCodeOf = phoistAcyclic $
  plam $ \reason ->
    pmatch (pasConstr # reason) $ \(PBuiltinPair tag fields) ->
      pif
        ( plength # fields #== prejectionArity tag
            #&& pall # plam (\field -> pabs # (pasInt # field) #>= 0) # fields
        )
        (pif (tag #== 0) prejectFieldPreimageSize $ pif (tag #== 1) prejectInvalidFieldType $ pif (tag #== 2) prejectEmptyInputs $ pif (tag #== 3) prejectDuplicateInput $ pif (tag #== 4) prejectInvalidValidityIntervalFormat $ pif (tag #== 5) prejectNetworkIdMismatch $ pif (tag #== 6) prejectMinFee $ pif (tag #== 7) prejectInvalidSignature $ pif (tag #== 8) prejectMissingRequiredWitness $ pif (tag #== 9) prejectInvalidFieldType $ pif (tag #== 10) prejectInvalidFieldType $ pif (tag #== 11) prejectNativeScriptNodeCount $ pif (tag #== 12) prejectNativeScriptDepth $ pif (tag #== 13) prejectNativeScriptInvalid $ pif (tag #== 14) prejectInvalidFieldType $ pif (tag #== 15) prejectInvalidFieldType $ pif (tag #== 16) prejectInvalidFieldType $ pif (tag #== 17) prejectValidityIntervalMismatch $ pif (tag #== 18) prejectInputNotFound $ pif (tag #== 19) prejectInvalidOutput $ pif (tag #== 20) prejectInvalidFieldType $ pif (tag #== 21) prejectNativeScriptNodeCount $ pif (tag #== 22) prejectNativeScriptDepth $ pif (tag #== 23) prejectMissingRequiredWitness $ pif (tag #== 24) prejectInvalidFieldType $ pif (tag #== 25) prejectInvalidOutput $ pif (tag #== 26) prejectInvalidFieldType $ pif (tag #== 27) prejectNativeScriptNodeCount $ pif (tag #== 28) prejectNativeScriptDepth $ pif (tag #== 29) prejectMissingRequiredWitness $ pif (tag #== 30) prejectAssetCount $ pif (tag #== 31) prejectMissingRequiredWitness $ pif (tag #== 32) prejectMissingRequiredWitness $ pif (tag #== 33) prejectInvalidFieldType $ pif (tag #== 34) prejectInvalidFieldType $ pif (tag #== 35) prejectInvalidFieldType $ pif (tag #== 36) prejectNativeScriptNodeCount $ pif (tag #== 37) prejectNativeScriptDepth $ pif (tag #== 38) prejectNativeScriptInvalid $ pif (tag #== 39) prejectInvalidFieldType $ pif (tag #== 40) prejectPlutusScriptInvalid $ pif (tag #== 41) prejectPlutusScriptInvalid $ pif (tag #== 42) prejectAssetCount $ pif (tag #== 43) prejectAssetCount $ pif (tag #== 44) prejectAssetCount $ pif (tag #== 45) prejectMinAda $ pif (tag #== 46) prejectValueNotPreserved $ perror)
        perror

prejectionArity :: forall s. Term s PInteger -> Term s PInteger
prejectionArity tag =
  pif (tag #== 0) 1 $ pif (tag #== 1) 2 $ pif (tag #== 2) 0 $ pif (tag #== 3) 4 $ pif (tag #== 4) 0 $ pif (tag #== 5) 0 $ pif (tag #== 6) 0 $ pif (tag #== 7) 1 $ pif (tag #== 8) 1 $ pif (tag #== 9) 1 $ pif (tag #== 10) 1 $ pif (tag #== 11) 1 $ pif (tag #== 12) 1 $ pif (tag #== 13) 1 $ pif (tag #== 14) 0 $ pif (tag #== 15) 0 $ pif (tag #== 16) 1 $ pif (tag #== 17) 0 $ pif (tag #== 18) 2 $ pif (tag #== 19) 2 $ pif (tag #== 20) 2 $ pif (tag #== 21) 2 $ pif (tag #== 22) 2 $ pif (tag #== 23) 1 $ pif (tag #== 24) 1 $ pif (tag #== 25) 1 $ pif (tag #== 26) 1 $ pif (tag #== 27) 1 $ pif (tag #== 28) 1 $ pif (tag #== 29) 1 $ pif (tag #== 30) 1 $ pif (tag #== 31) 2 $ pif (tag #== 32) 2 $ pif (tag #== 33) 1 $ pif (tag #== 34) 1 $ pif (tag #== 35) 1 $ pif (tag #== 36) 1 $ pif (tag #== 37) 1 $ pif (tag #== 38) 1 $ pif (tag #== 39) 0 $ pif (tag #== 40) 1 $ pif (tag #== 41) 1 $ pif (tag #== 42) 2 $ pif (tag #== 43) 2 $ pif (tag #== 44) 1 $ pif (tag #== 45) 1 $ pif (tag #== 46) 0 perror
