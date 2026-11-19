{- |
Module      : Midgard.FraudProof
Description : Plutarch port of @lib/midgard/fraud-proof.ak@.

A fraud proof token is minted when a computation thread proves a block
fraudulent, and never burned — Midgard fraud proof tokens last forever. The
token's asset name encodes which block it convicts.
-}
module Midgard.FraudProof (
  PFraudProofDatum (..),
  PMintRedeemer (..),
  passetNameToHeaderHash,
  pgetProvenFraudRecord,
  pgetProvenFraudRecordWithIdentity,
) where

import Data.Kind (Type)
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.LedgerApi.V3 (PCurrencySymbol, PTokenName (..), PTxInInfo)
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.Common.Utils (
  pgetAuthenticInputDatumAndAssetNameWithPolicyAt,
 )
import Midgard.FraudProofCatalogue (pidByteCount)
import Midgard.LedgerState (PHeaderHash)

{- | Aiken @fraud_proof.Datum@.

'DeriveAsDataStruct', not 'DeriveAsDataRec': Aiken records are @Constr 0@, and
the latter would encode this as a bare CBOR list. That distinction is
load-bearing here because 'Midgard.FraudProofs.Common.pfinalize' builds this
datum and compares it to the produced output's datum byte-for-byte, so a
list-encoded value would reject every genuine finalisation.
-}
newtype PFraudProofDatum (s :: S) = PFraudProofDatum
  {pfraudProof'fraudProver :: Term s (PAsData PByteString)}
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PFraudProofDatum)

-- | Aiken @fraud_proof.MintRedeemer@.
data PMintRedeemer (s :: S) = PMintRedeemer
  { pfpMint'computationThreadTokenAssetName :: Term s (PAsData PTokenName)
  , pfpMint'computationThreadMintRedeemerIndex :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PMintRedeemer)

{- | Aiken @fraud_proof.asset_name_to_header_hash@.

@
fraud_proof_asset_name |> bytearray.drop(fraud_proof_catalogue.id_byte_count)
@

A fraud proof's asset name is the catalogue id of the fraud category (4 bytes)
followed by the header hash of the block it convicts. Dropping the id prefix
leaves the header hash.
-}
passetNameToHeaderHash ::
  forall (s :: S). Term s (PAsData PTokenName :--> PHeaderHash)
passetNameToHeaderHash = phoistAcyclic $
  plam $ \assetName ->
    plet (pto (pfromData assetName)) $ \bytes ->
      psliceBS # pidByteCount # (plengthBS # bytes - pidByteCount) # bytes

{- | Aiken @fraud_proof.get_proven_fraud_record@.

Returns the authenticated header-hash suffix and the immutable fraud prover
stored in the proof UTxO's inline datum.
-}
pgetProvenFraudRecord ::
  forall (s :: S) (r :: S -> Type).
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  Term s (PAsData PCurrencySymbol) ->
  Term s PInteger ->
  (Term s PHeaderHash -> Term s PByteString -> Term s r) ->
  Term s r
pgetProvenFraudRecord referenceInputs fraudProofPolicyId fraudProofRefInputIndex k =
  pgetProvenFraudRecordWithIdentity
    referenceInputs
    fraudProofPolicyId
    fraudProofRefInputIndex
    (\headerHash fraudProver _assetName -> k headerHash fraudProver)

{- | Aiken @fraud_proof.get_proven_fraud_record_with_identity@.

The complete proof asset name is preserved because its category prefix is part
of the correction identity; two categories may convict the same header but may
not resume one another's in-flight correction.
-}
pgetProvenFraudRecordWithIdentity ::
  forall (s :: S) (r :: S -> Type).
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  Term s (PAsData PCurrencySymbol) ->
  Term s PInteger ->
  (Term s PHeaderHash -> Term s PByteString -> Term s (PAsData PTokenName) -> Term s r) ->
  Term s r
pgetProvenFraudRecordWithIdentity referenceInputs fraudProofPolicyId fraudProofRefInputIndex k =
  pgetAuthenticInputDatumAndAssetNameWithPolicyAt
    referenceInputs
    fraudProofPolicyId
    fraudProofRefInputIndex
    ( \assetName datumData ->
        pmatch
          (pfromData $ punsafeCoerce @(PAsData PFraudProofDatum) datumData)
          $ \PFraudProofDatum {pfraudProof'fraudProver} ->
            k
              (passetNameToHeaderHash # assetName)
              (pfromData pfraudProof'fraudProver)
              assetName
    )
