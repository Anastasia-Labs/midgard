{- |
Module      : Midgard.Validators.FraudProofCatalogue
Description : Plutarch port of @validators/fraud-proof-catalogue.ak@.

Two validators. The minting policy couples the catalogue NFT one-to-one with the
hub oracle NFT, so the catalogue can only come into existence at genesis. The
spending validator always fails, so the deployed catalogue UTxO is unspendable
and its Merkle root can never be mutated.
-}
module Midgard.Validators.FraudProofCatalogue (
  fraudProofCatalogueMintValidator,
  fraudProofCatalogueSpendValidator,
) where

import Plutarch.LedgerApi.V3 (
  PCurrencySymbol,
  PScriptContext (..),
  PScriptInfo (..),
  PTxInfo (..),
 )
import Plutarch.Monadic qualified as P
import Plutarch.Prelude

import Midgard.Common.Utils (pgetSingletonAssetWithPolicy)
import Midgard.FraudProofCatalogue qualified as Catalogue
import Midgard.HubOracle qualified as Hub

{- | Aiken @validators/fraud-proof-catalogue.ak@ — the @mint@ handler.

@
mint(_redeemer: Redeemer, own_policy_id: PolicyId, self: Transaction) {
  let Transaction { mint, .. } = self
  and {
    Pair(hub.asset_name, 1) == get_singleton_asset_with_policy(mint, hub_oracle_script_hash),
    Pair(asset_name, 1) == get_singleton_asset_with_policy(mint, own_policy_id),
  }
}
else(_) { fail }
@

The immutability property rests on the first conjunct: because the hub oracle
NFT is itself one-shot, requiring it in the same mint means the catalogue NFT
can only ever be minted at genesis. A standalone re-mint later — which would let
a deployed catalogue reference be substituted — has no hub oracle NFT to offer
and so fails.

@get_singleton_asset_with_policy@ fails outright if a policy contributes more
than one token name, and the quantity comparison rejects anything other than
exactly one token.

The Aiken validator parameter becomes a leading argument.
-}
fraudProofCatalogueMintValidator ::
  forall (s :: S).
  Term
    s
    ( PAsData PCurrencySymbol
        :--> PScriptContext
        :--> PUnit
    )
fraudProofCatalogueMintValidator = plam $ \hubOracleScriptHash ctx -> P.do
  PScriptContext {pscriptContext'txInfo, pscriptContext'scriptInfo} <- pmatch ctx
  ownPolicyId <-
    plet $ pmatch pscriptContext'scriptInfo $ \case
      PMintingScript cs -> cs
      _ -> perror
  PTxInfo {ptxInfo'mint} <- pmatch pscriptContext'txInfo
  mint <- plet $ pfromData ptxInfo'mint
  pif
    ( pand'
        # ( (pgetSingletonAssetWithPolicy # mint # hubOracleScriptHash)
              #== (ppairDataBuiltin # Hub.passetName # pdata 1)
          )
        # ( (pgetSingletonAssetWithPolicy # mint # ownPolicyId)
              #== (ppairDataBuiltin # Catalogue.passetName # pdata 1)
          )
    )
    (pconstant ())
    perror

{- | Aiken @validators/fraud-proof-catalogue.ak@ — the @spend@ validator.

@
validator spend {
  else(_) {
    trace @"Midgard - Fraud Proof Catalogue Spending Validator"
    fail
  }
}
@

The catalogue has no @spend@ handler at all, only a failing @else@, so every
purpose fails. That unspendability is the immutability guarantee for the
catalogue's Merkle root.
-}
fraudProofCatalogueSpendValidator ::
  forall (s :: S). Term s (PScriptContext :--> PUnit)
fraudProofCatalogueSpendValidator =
  plam $ \_ctx ->
    ptraceInfoError "Midgard - Fraud Proof Catalogue Spending Validator"
