{- |
Module      : Midgard.Validators.TxOrderFields
Description : Plutarch port of @validators/user-events/tx-field-preimage-v1.ak@
              and @validators/user-events/tx-field-receipt-spend-v1.ak@.

Two sibling validators, three lines of logic each, holding up the same property
from opposite ends.

A forced transaction's material goes up on L1 piecewise: each field is published
in chunks, each chunk as a *preimage* UTxO, each preimage acknowledged by a
*receipt* UTxO. Both are addressed by these validators, and neither may be
released on its own. A spend is permitted only when the transaction burns both
the order's event NFT and the chunk's receipt NFT — that is, only as part of
dismantling the order the material belongs to.

Why that matters: were a preimage independently spendable, an operator could
retire the evidence for a forced transaction while the order was still live and
still awaiting classification or challenge. Tying the spend to the order's own
burn means the material outlives every use anyone could have for it.

Both are unparameterised and take no redeemer. Everything they need is in the
datum, which names its own order and its own coordinates.
-}
module Midgard.Validators.TxOrderFields (
  txFieldPreimageSpendValidator,
  txFieldReceiptSpendValidator,
) where

import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.LedgerApi.V3 (
  PMintValue,
  PScriptContext (..),
  PScriptInfo (..),
  PTxInfo (..),
 )
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.LedgerState (PTxFieldPreimageV1, PTxFieldReceiptV1)
import Midgard.UserEvents.TxOrder (
  pfieldFragmentBurnAuthorized,
  pfieldReceiptBurnAuthorized,
 )

-- | Aiken @validators/user-events/tx-field-preimage-v1.ak@ — @spend@.
txFieldPreimageSpendValidator ::
  forall (s :: S). Term s (PScriptContext :--> PUnit)
txFieldPreimageSpendValidator = plam $ \ctx ->
  pspendWithDatum ctx $ \datum mint ->
    pfieldFragmentBurnAuthorized # punsafeCoerce @(PAsData PTxFieldPreimageV1) datum # mint

-- | Aiken @validators/user-events/tx-field-receipt-spend-v1.ak@ — @spend@.
txFieldReceiptSpendValidator ::
  forall (s :: S). Term s (PScriptContext :--> PUnit)
txFieldReceiptSpendValidator = plam $ \ctx ->
  pspendWithDatum ctx $ \datum mint ->
    pfieldReceiptBurnAuthorized # punsafeCoerce @(PAsData PTxFieldReceiptV1) datum # mint

{- | The shape both validators share: @expect Some(datum)@, then a predicate over
the datum and the transaction's mint.

A datum-less UTxO at either address is unspendable, which is Aiken's
@expect Some(..)@ and not a special case worth naming — an unparsed datum is the
same rejection.
-}
pspendWithDatum ::
  forall (s :: S).
  Term s PScriptContext ->
  (Term s PData -> Term s PMintValue -> Term s PBool) ->
  Term s PUnit
pspendWithDatum ctx k = P.do
  PScriptContext {pscriptContext'txInfo, pscriptContext'scriptInfo} <- pmatch ctx
  datum <-
    plet $ pmatch pscriptContext'scriptInfo $ \case
      PSpendingScript _ mDatum ->
        pmatch mDatum $ \case
          PDJust d -> pto (pfromData d)
          PDNothing -> perror
      _ -> perror
  PTxInfo {ptxInfo'mint} <- pmatch pscriptContext'txInfo
  pif (k datum (pfromData ptxInfo'mint)) (pconstant ()) perror
