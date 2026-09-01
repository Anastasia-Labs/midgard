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
  pgetProvenFraudulentBlocksHeaderHash,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.LedgerApi.V3 (PCurrencySymbol, PTokenName (..), PTxInInfo)
import Plutarch.Prelude

import Midgard.Common.Utils (pgetAuthenticInputAssetNameWithPolicyAt)
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

{- | Aiken @fraud_proof.get_proven_fraudulent_blocks_header_hash@.

Reads the fraud proof token held by the reference input at the given index and
returns the header hash of the block it convicts.

Authenticity here is by policy id alone — the asset name is the payload being
read out, so it cannot also be a constraint.
-}
pgetProvenFraudulentBlocksHeaderHash ::
  forall (s :: S).
  Term
    s
    ( PBuiltinList (PAsData PTxInInfo)
        :--> PAsData PCurrencySymbol
        :--> PInteger
        :--> PHeaderHash
    )
pgetProvenFraudulentBlocksHeaderHash = phoistAcyclic $
  plam $ \referenceInputs fraudProofPolicyId fraudProofRefInputIndex ->
    passetNameToHeaderHash
      #$ pgetAuthenticInputAssetNameWithPolicyAt
      # referenceInputs
      # fraudProofPolicyId
      # fraudProofRefInputIndex
