{- |
Module      : Midgard.DaAttestation
Description : Partial Plutarch port of @lib/midgard/da-attestation-types.ak@.

Ported so far: the minting redeemer the state queue reads, and the governed
parameters datum the DA params governor maintains. The attestation's own
validator and signature machinery are a separate slice.
-}
module Midgard.DaAttestation (
  PMintRedeemer (..),
  PDaParamsDatum (..),
  pdaParamsAssetName,
  pverificationKeyByteCount,
  pmaxIndexedSignerCount,
  pattestationAssetNamePrefix,
  psignatureByteCount,
  psignatureWitnessByteCount,
  pattestedSignerBitmapByteCount,
  pemptyAttestedSignerBitmap,
  PDaAttestationDatum (..),
  pstateQueueMintReferenceScriptAssetName,
  PSpendRedeemer (..),
) where

import Data.ByteString qualified as BS
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.LedgerApi.V3 (PPubKeyHash, PTokenName (..))
import Plutarch.Prelude

{- | Aiken @da_attestation.MintRedeemer@.

Tags: @Init@ 0, @ApplyToStateQueue@ 1, @RescueStrandedAttestation@ 2. Every field is typed: the attestation's own validator reads all of them, so the
placeholder 'PData's that stood in while only the state queue consumed this
redeemer are gone.
-}
data PMintRedeemer (s :: S)
  = PInit
      { pdaInit'outputIndex :: Term s (PAsData PInteger)
      , pdaInit'daParamsRefInputIndex :: Term s (PAsData PInteger)
      , pdaInit'stateQueueRefInputIndex :: Term s (PAsData PInteger)
      , pdaInit'stateQueueMintRefScriptInputIndex :: Term s (PAsData PInteger)
      }
  | PApplyToStateQueue
      { papply'daAttestationInputIndex :: Term s (PAsData PInteger)
      , papply'daParamsRefInputIndex :: Term s (PAsData PInteger)
      , papply'stateQueueInputIndex :: Term s (PAsData PInteger)
      , papply'stateQueueOutputIndex :: Term s (PAsData PInteger)
      , papply'stateQueueMintRefScriptInputIndex :: Term s (PAsData PInteger)
      }
  | PRescueStrandedAttestation
      { prescue'daAttestationInputIndex :: Term s (PAsData PInteger)
      , prescue'daParamsRefInputIndex :: Term s (PAsData PInteger)
      , prescue'refundOutputIndex :: Term s (PAsData PInteger)
      }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PMintRedeemer)

{- | Aiken @da_attestation.da_params_asset_name@.

The governed parameters live at one UTxO under one name, so "the DA params" is a
singular thing the whole protocol can point at.
-}
pdaParamsAssetName :: forall (s :: S). Term s (PAsData PTokenName)
pdaParamsAssetName = pdata (pcon (PTokenName (pconstant "MIDGARD_DA_PARAMS")))

-- | Aiken @da_attestation.verification_key_byte_count@.
pverificationKeyByteCount :: forall (s :: S). Term s PInteger
pverificationKeyByteCount = 32

{- | Aiken @da_attestation.max_indexed_signer_count@.

The committee is indexed by a 32-byte bitmap, so 256 signers is the ceiling the
encoding itself imposes.
-}
pmaxIndexedSignerCount :: forall (s :: S). Term s PInteger
pmaxIndexedSignerCount = 256

{- | Aiken @da_attestation.DaParamsDatum@.

The governed data-availability parameters: who may attest, how many of them are
needed, and who may change that.

@committee@ is /packed/ — verification keys concatenated as 32-byte chunks
rather than held as a list — while @owners@ is an ordinary list of key hashes.
The asymmetry is deliberate: attestation signatures are indexed positionally
into the committee, so it needs a fixed stride; governance uses transaction
signatories, which are hashes.
-}
data PDaParamsDatum (s :: S) = PDaParamsDatum
  { pdaParams'committee :: Term s (PAsData PByteString)
  , pdaParams'committeeSignersHash :: Term s (PAsData PByteString)
  , pdaParams'daThreshold :: Term s (PAsData PInteger)
  , pdaParams'owners :: Term s (PAsData (PBuiltinList (PAsData PPubKeyHash)))
  , pdaParams'updateThreshold :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PDaParamsDatum)

-- | Aiken @da_attestation.attestation_asset_name_prefix@ — "DAAT".
pattestationAssetNamePrefix :: forall (s :: S). Term s PByteString
pattestationAssetNamePrefix = pconstant "DAAT"

-- | Aiken @da_attestation.signature_byte_count@ — Ed25519.
psignatureByteCount :: forall (s :: S). Term s PInteger
psignatureByteCount = 64

{- | Aiken @da_attestation.signature_witness_byte_count@.

One index byte plus a 64-byte signature. The witness sequence is these packed
end to end, so this stride is what lets the cursor walk it.
-}
psignatureWitnessByteCount :: forall (s :: S). Term s PInteger
psignatureWitnessByteCount = 65

{- | Aiken @da_attestation.attested_signer_bitmap_byte_count@.

Thirty-two bytes — 256 bits, one per indexable signer, matching
'pmaxIndexedSignerCount'.
-}
pattestedSignerBitmapByteCount :: forall (s :: S). Term s PInteger
pattestedSignerBitmapByteCount = 32

-- | Aiken @da_attestation.empty_attested_signer_bitmap@ — 32 zero bytes.
pemptyAttestedSignerBitmap :: forall (s :: S). Term s PByteString
pemptyAttestedSignerBitmap = pconstant (BS.replicate 32 0x00)

{- | Aiken @da_attestation.DaAttestationDatum@.

One in-progress attestation for one block.

@da_threshold@ and @committee_signers_hash@ are /frozen/ here at creation, and
that freezing is what the apply path later has to reconcile against the current
governed parameters: an attestation gathered under a committee that has since
been rotated out must no longer apply, however many signatures it holds.

@attested_signers@ is the 256-bit MSB-first bitmap; @attestation_count@ is its
population, carried separately so a consumer need not count bits.
-}
data PDaAttestationDatum (s :: S) = PDaAttestationDatum
  { pdaAttestation'headerHash :: Term s (PAsData PByteString)
  , pdaAttestation'daThreshold :: Term s (PAsData PInteger)
  , pdaAttestation'committeeSignersHash :: Term s (PAsData PByteString)
  , pdaAttestation'attestedSigners :: Term s (PAsData PByteString)
  , pdaAttestation'attestationCount :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PDaAttestationDatum)

-- | Aiken @da_attestation.state_queue_mint_reference_script_asset_name@.
pstateQueueMintReferenceScriptAssetName :: forall (s :: S). Term s (PAsData PTokenName)
pstateQueueMintReferenceScriptAssetName =
  pdata (pcon (PTokenName (pconstant "StateQueueMint")))

{- | Aiken @da_attestation.SpendRedeemer@.

Tags: @AddSignatures@ 0, @BurnForStateQueue@ 1, @BurnForRescue@ 2.

The two burn variants carry the same single field but must not be
interchangeable: each defers to a different mint redeemer, and the whole point
of having two is that a rescue authorisation cannot satisfy an apply, nor the
reverse.
-}
data PSpendRedeemer (s :: S)
  = PAddSignatures
      { paddSigs'outputIndex :: Term s (PAsData PInteger)
      , paddSigs'daParamsRefInputIndex :: Term s (PAsData PInteger)
      , paddSigs'signatures :: Term s (PAsData PByteString)
      }
  | PBurnForStateQueue {pburnSq'mintRedeemerIndex :: Term s (PAsData PInteger)}
  | PBurnForRescue {pburnRescue'mintRedeemerIndex :: Term s (PAsData PInteger)}
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PSpendRedeemer)
