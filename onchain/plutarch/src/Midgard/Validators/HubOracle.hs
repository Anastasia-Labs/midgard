{- |
Module      : Midgard.Validators.HubOracle
Description : Plutarch port of @validators/hub-oracle.ak@.

A one-shot NFT minting policy. Minting is permitted only in the transaction that
spends a specific initialisation UTxO, which is what makes the resulting token
unique for the lifetime of the chain; burning is permitted unconditionally.
-}
module Midgard.Validators.HubOracle (hubOracleMintValidator) where

import Plutarch.Core.Utils (phasUTxO)
import Plutarch.LedgerApi.V3 (
  PCurrencySymbol,
  PMintValue,
  PScriptContext (..),
  PScriptInfo (..),
  PTokenName,
  PTxInfo (..),
  PTxOutRef,
 )
import Plutarch.LedgerApi.Value qualified as Value
import Plutarch.Monadic qualified as P
import Plutarch.Prelude

import Midgard.Common.Utils (pvalidateMint)

{- | Aiken @validators/hub-oracle.ak@ — the @mint@ handler, and @else(_) { fail }@.

@
validator mint(init_utxo: OutputReference, hub_oracle_asset_name: AssetName) {
  mint(_redeemer: Data, policy_id: PolicyId, self: Transaction) {
    let qty = quantity_of(self.mint, policy_id, hub_oracle_asset_name)
    expect utils.validate_mint(self.mint, policy_id, hub_oracle_asset_name, qty)
    if qty == 1 {
      expect Some(_) = list.find(self.inputs, fn(input) { input.output_reference == init_utxo })
      True
    } else {
      expect qty == -1
      True
    }
  }
  else(_) { fail }
}
@

Three details of the original are preserved deliberately:

  * @validate_mint@ is called with the /observed/ quantity, so it does not
    constrain the amount — its job is to reject any transaction that touches
    more than this one token name under this policy. The amount is then
    constrained separately to exactly @1@ or @-1@, which is what rules out
    minting two NFTs at once.
  * The @else@ branch fails, so this script is valid only at a minting purpose.
  * Burning (@-1@) requires no witness beyond the amount check.

The two Aiken validator parameters become leading arguments; apply them with
'Plutarch.Evaluate.applyArguments' to obtain the deployable script.
-}
hubOracleMintValidator ::
  forall (s :: S).
  Term
    s
    ( PAsData PTxOutRef
        :--> PAsData PTokenName
        :--> PScriptContext
        :--> PUnit
    )
hubOracleMintValidator = plam $ \initUtxo hubOracleAssetName ctx -> P.do
  PScriptContext {pscriptContext'txInfo, pscriptContext'scriptInfo} <- pmatch ctx
  policyId <-
    plet $ pmatch pscriptContext'scriptInfo $ \case
      PMintingScript cs -> cs
      _ -> perror
  PTxInfo {ptxInfo'inputs, ptxInfo'mint} <- pmatch pscriptContext'txInfo
  mint <- plet $ pfromData ptxInfo'mint
  qty <-
    plet $
      pquantityOf # mint # policyId # hubOracleAssetName
  pif
    (pvalidateMint # mint # policyId # hubOracleAssetName # pdata qty)
    ( pif
        (qty #== 1)
        ( pif
            (phasUTxO # pfromData initUtxo # pfromData ptxInfo'inputs)
            (pconstant ())
            perror
        )
        ( pif
            (qty #== -1)
            (pconstant ())
            perror
        )
    )
    perror

{- | Aiken @assets.quantity_of@ specialised to the mint field.

Returns @0@ for an absent policy or token name, matching Aiken.
-}
pquantityOf ::
  forall (s :: S).
  Term
    s
    ( PMintValue
        :--> PAsData PCurrencySymbol
        :--> PAsData PTokenName
        :--> PInteger
    )
pquantityOf = phoistAcyclic $
  plam $ \mint policy name ->
    Value.pvalueOf # pto mint # pfromData policy # pfromData name
