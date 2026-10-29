{- |
Module      : Midgard.FraudProofCatalogue
Description : Plutarch port of @lib/midgard/fraud-proof-catalogue.ak@.

The fraud proof catalogue is a single authenticated UTxO whose inline datum is
the Merkle root of the set of admissible fraud proofs, keyed by integer index.
Consumers read it as a reference input.
-}
module Midgard.FraudProofCatalogue (
  passetName,
  pidByteCount,
  PCatalogueDatum,
  pgetDatum,
) where

import Plutarch.LedgerApi.V3 (
  PCurrencySymbol,
  POutputDatum (..),
  PTokenName (..),
  PTxInInfo (..),
  PTxOut (..),
 )
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.Common.Types (PMerkleRoot)
import Midgard.Common.Utils (pgetAuthenticInputWithNftAt)

-- | Aiken @fraud_proof_catalogue.asset_name@.
passetName :: forall (s :: S). Term s (PAsData PTokenName)
passetName = pdata (pcon (PTokenName (pconstant "MIDGARD_FRAUD_PROOF_CATALOGUE")))

{- | Aiken @fraud_proof_catalogue.id_byte_count@.

Catalogue keys are integer indices, each represented in this many bytes.
-}
pidByteCount :: forall (s :: S). Term s PInteger
pidByteCount = 4

{- | Aiken @fraud_proof_catalogue.Datum = MerkleRoot<Int, ByteArray>@.

An alias for the Merkle root bytes, not a wrapper — see 'PMerkleRoot'.
-}
type PCatalogueDatum = PMerkleRoot

{- | Aiken @fraud_proof_catalogue.get_datum@.

Resolves the authentic catalogue UTxO from @reference_inputs@ at the supplied
index and returns its inline datum.

Note this uses 'pgetAuthenticInputWithNftAt', which checks the NFT but not the
address — matching the original, which calls
@utils.get_authentic_input_with_nft_at@ and passes the catalogue script hash as
the /policy id/, not as an address constraint.

The Aiken original writes @expect datum: Datum = datum_data@. Because @Datum@ is
an alias for @ByteArray@, that @expect@ compiles to a bytestring cast, which is
what the coercion below performs.
-}
pgetDatum ::
  forall (s :: S).
  Term
    s
    ( PBuiltinList (PAsData PTxInInfo)
        :--> PAsData PCurrencySymbol
        :--> PInteger
        :--> PCatalogueDatum
    )
pgetDatum = phoistAcyclic $
  plam $ \referenceInputs catalogueScriptHash catalogueRefInputIndex -> P.do
    catalogueInput <-
      plet $
        pgetAuthenticInputWithNftAt
          # referenceInputs
          # catalogueScriptHash
          # passetName
          # catalogueRefInputIndex
    PTxInInfo {ptxInInfo'resolved} <- pmatch $ pfromData catalogueInput
    PTxOut {ptxOut'datum} <- pmatch ptxInInfo'resolved
    pmatch ptxOut'datum $ \case
      POutputDatum {poutputDatum'outputDatum} ->
        pfromData (punsafeCoerce @(PAsData PCatalogueDatum) (pto poutputDatum'outputDatum))
      _ -> perror
