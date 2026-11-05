{- |
Module      : Midgard.Validators.HubOracle
Description : Plutarch port of @validators/hub-oracle.ak@.

A one-shot policy for the hub NFT and correction-lock singleton. They are
created and destroyed together at equal quantities. Minting is permitted only
in the transaction that spends a specific initialisation UTxO, which makes both
tokens unique for the lifetime of the chain; burning is otherwise unconditional.
-}
module Midgard.Validators.HubOracle (
    phubMintSetIsExact,
    hubOracleMintValidator,
) where

import Plutarch.Core.Utils (phasUTxO)
import Plutarch.LedgerApi.AssocMap qualified as AssocMap
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

import Midgard.CorrectionLock qualified as CorrectionLock

{- | Aiken @validators/hub-oracle.ak@ — the @mint@ handler, and
@else(_) { fail }@.

Three details of the original are preserved deliberately:

  * The exact mint set contains the correction-lock name followed by the hub
    name, both at the observed hub quantity. The amount is then constrained to
    exactly @1@ or @-1@.
  * The @else@ branch fails, so this script is valid only at a minting purpose.
  * Burning (@-1@) requires no witness beyond the amount check.

The two Aiken validator parameters become leading arguments; apply them with
'Plutarch.Evaluate.applyArguments' to obtain the deployable script.
-}
phubMintSetIsExact ::
    forall (s :: S).
    Term s PMintValue ->
    Term s (PAsData PCurrencySymbol) ->
    Term s (PAsData PTokenName) ->
    Term s PBool
phubMintSetIsExact mint policyId hubOracleAssetName =
    plet (pquantityOf # mint # policyId # hubOracleAssetName) $ \qty ->
        pmatch (AssocMap.plookup # pfromData policyId # pto (pto mint)) $ \case
            PNothing -> pconstant False
            PJust tokenMap ->
                plet (pto (pto tokenMap)) $ \entries ->
                    plength
                        # entries
                        #== 2
                        #&& Value.pvalueOf
                        # pto mint
                        # pfromData policyId
                        # pfromData CorrectionLock.passetName
                        #== qty
                        #&& Value.pvalueOf
                        # pto mint
                        # pfromData policyId
                        # pfromData hubOracleAssetName
                        #== qty

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
    PScriptContext{pscriptContext'txInfo, pscriptContext'scriptInfo} <- pmatch ctx
    policyId <-
        plet $ pmatch pscriptContext'scriptInfo $ \case
            PMintingScript cs -> cs
            _ -> perror
    PTxInfo{ptxInfo'inputs, ptxInfo'mint} <- pmatch pscriptContext'txInfo
    mint <- plet $ pfromData ptxInfo'mint
    qty <-
        plet $
            pquantityOf # mint # policyId # hubOracleAssetName
    pif
        (phubMintSetIsExact mint policyId hubOracleAssetName)
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
