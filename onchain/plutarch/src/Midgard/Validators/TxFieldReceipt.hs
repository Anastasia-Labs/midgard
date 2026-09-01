{- |
Module      : Midgard.Validators.TxFieldReceipt
Description : Plutarch port of @validators/user-events/tx-field-receipt-v1.ak@.

The minting policy for field receipts — the tokens that acknowledge a published
chunk of a forced transaction's material.

The validator itself is a dispatch: two parameters, two redeemer arms, and
@else(_) { fail }@. What is worth reading is which of the two arms can succeed.

=== @BurnReceipts@ is ported in full

Burning is the direction that matters for safety. A receipt is the record that a
chunk was published, so destroying one has to be tied to destroying the order it
belongs to — otherwise an operator could quietly retire the acknowledgements
while the material was still in play.
'Midgard.UserEvents.TxFieldReceipt.pvalidateBurnReceipts' is the whole branch and
is tested in "Testing.TxFieldReceipt".

=== @PublishField@ retains the retired counted gate

Its final conjunct is
@native_transaction.verify_midgard_transaction_field_chunk_v1@, which binds
@bounded_collection_v1.verify_item@ — surface the Aiken tree documents as
unsatisfiable. Under §4's plain hashing a field commitment is a single hash over
the whole preimage, so there is nothing for a per-item Merkle opening to be
checked against, and the function cannot return @True@ for any input. Every
guard in front of it is therefore dead weight: no transaction reaches the end of
that branch.

The port now retains all publication guards and both final proof predicates
literally. Honest transactions still fail at the counted-root binding, but the
standalone counted-source fixtures and every preceding rejection boundary match
Aiken rather than being collapsed into an immediate error.

The branch comes back when §8.6's replacement lands and the receipt chain is
rebuilt on the carriage layer that replaced the counted scheme, at which point
the guards above the conjunct are worth writing against a check that can hold.
-}
module Midgard.Validators.TxFieldReceipt (txFieldReceiptMintValidator) where

import Plutarch.LedgerApi.V3 (
  PScriptContext (..),
  PScriptHash,
  PScriptInfo (..),
  PTxInfo (..),
 )
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.UserEvents.TxFieldReceipt (
  PMintRedeemer (..),
  pvalidateBurnReceipts,
  pvalidatePublication,
 )

{- | Aiken @validators/user-events/tx-field-receipt-v1.ak@ — @mint@, with
@else(_) { fail }@.

Parameterised by the two addresses the publication branch reads: the
field-preimage script and the receipt script. The burn branch only reads the
second, but both remain part of the policy's applied script identity.
-}
txFieldReceiptMintValidator ::
  forall (s :: S).
  Term
    s
    ( PAsData PScriptHash
        :--> PAsData PScriptHash
        :--> PScriptContext
        :--> PUnit
    )
txFieldReceiptMintValidator = plam $ \fieldPreimageScriptHash receiptScriptHash ctx -> P.do
  PScriptContext {pscriptContext'txInfo, pscriptContext'redeemer, pscriptContext'scriptInfo} <-
    pmatch ctx
  -- `else(_) { fail }`: no purpose but `mint` reaches the dispatch.
  ownPolicy <-
    plet $ pmatch pscriptContext'scriptInfo $ \case
      PMintingScript policy -> policy
      _ -> perror
  PTxInfo
    { ptxInfo'inputs
    , ptxInfo'outputs
    , ptxInfo'referenceInputs
    , ptxInfo'mint
    } <-
    pmatch pscriptContext'txInfo
  redeemer <-
    plet $
      pfromData $
        punsafeCoerce @(PAsData PMintRedeemer) (pto pscriptContext'redeemer)
  pmatch redeemer $ \case
    PPublishField
      { ppublishField'fieldReferenceInputIndex
      , ppublishField'predecessorReceiptReferenceInputIndex
      , ppublishField'receiptOutputIndex
      , ppublishField'transactionId
      , ppublishField'source
      } ->
        pif
          ( pvalidatePublication
              # fieldPreimageScriptHash
              # receiptScriptHash
              # ownPolicy
              # pfromData ptxInfo'referenceInputs
              # pfromData ptxInfo'outputs
              # pfromData ptxInfo'mint
              # pfromData ppublishField'fieldReferenceInputIndex
              # pfromData ppublishField'predecessorReceiptReferenceInputIndex
              # pfromData ppublishField'receiptOutputIndex
              # pfromData ppublishField'transactionId
              # pfromData ppublishField'source
          )
          (pconstant ())
          perror
    PBurnReceipts {pburnReceipts'receiptInputIndices} ->
      pif
        ( pvalidateBurnReceipts
            # receiptScriptHash
            # ownPolicy
            # pfromData ptxInfo'inputs
            # pfromData ptxInfo'mint
            # pfromData pburnReceipts'receiptInputIndices
        )
        (pconstant ())
        perror
