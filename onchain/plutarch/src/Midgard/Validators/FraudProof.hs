{- |
Module      : Midgard.Validators.FraudProof
Description : Plutarch port of @validators/fraud-proof.ak@.

Two validators. The minting policy converts a completed computation thread token
into a permanent fraud proof token of the same asset name. The spending
validator always fails: Midgard fraud proof tokens last forever.
-}
module Midgard.Validators.FraudProof (
  fraudProofMintValidator,
  fraudProofSpendValidator,
) where

import Plutarch.Core.Utils (pand'List)
import Plutarch.LedgerApi.AssocMap qualified as AssocMap
import Plutarch.LedgerApi.V3 (
  PCurrencySymbol,
  PMintValue,
  PScriptContext (..),
  PScriptInfo (..),
  PScriptPurpose (..),
  PTokenName,
  PTxInfo (..),
 )
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.ComputationThread qualified as CT
import Midgard.Common.Utils (pgetRedeemerAt)
import Midgard.FraudProof (PMintRedeemer (..))

{- | Aiken @validators/fraud-proof.ak@ — the @mint@ handler.

The three numbered checks of the original, in order:

  1. The computation thread's own mint redeemer, at the caller-supplied index,
     must be @Success@. 'pgetRedeemerAt' pins the redeemer to the computation
     thread's minting purpose, so the index hint cannot be aimed elsewhere.
  2. The asset name in that @Success@ redeemer must equal the one in this
     redeemer.
  3. The transaction's whole mint field must be exactly: burn one computation
     thread token, mint one fraud proof token, both under that asset name.

Check 3 is an equality against the entire mint field, not a containment check.
That is what stops a transaction from minting a fraud proof while also minting
anything else, and it is why this is compared as a whole value rather than
looked up policy by policy.

The Aiken validator parameter becomes a leading argument.
-}
fraudProofMintValidator ::
  forall (s :: S).
  Term
    s
    ( PAsData PCurrencySymbol
        :--> PScriptContext
        :--> PUnit
    )
fraudProofMintValidator = plam $ \computationThreadScriptHash ctx -> P.do
  PScriptContext {pscriptContext'txInfo, pscriptContext'redeemer, pscriptContext'scriptInfo} <-
    pmatch ctx
  policyId <-
    plet $ pmatch pscriptContext'scriptInfo $ \case
      PMintingScript cs -> cs
      _ -> perror
  PTxInfo {ptxInfo'mint, ptxInfo'redeemers} <- pmatch pscriptContext'txInfo
  PMintRedeemer
    { pfpMint'computationThreadTokenAssetName
    , pfpMint'computationThreadMintRedeemerIndex
    } <-
    pmatch $
      pfromData $
        punsafeCoerce @(PAsData PMintRedeemer) (pto pscriptContext'redeemer)

  -- 1. `Success` redeemer of the computation thread's token policy must be
  --    invoked.
  mintRedeemerData <-
    plet $
      pgetRedeemerAt
        # pto (pto (pfromData ptxInfo'redeemers))
        # pdata (pcon (PMinting computationThreadScriptHash))
        # pfromData pfpMint'computationThreadMintRedeemerIndex
  successAssetName <-
    plet $
      pmatch
        ( pfromData
            (punsafeCoerce @(PAsData CT.PMintRedeemer) (pto (pfromData mintRedeemerData)))
        )
        $ \case
          CT.PSuccess {pctSuccess'burningTokenAssetName} -> pctSuccess'burningTokenAssetName
          _ -> perror

  pif
    ( pand'List
        [ -- 2. Asset names must agree.
          successAssetName #== pfpMint'computationThreadTokenAssetName
        , -- 3. The mint field must be exactly the burn plus the mint.
          pfromData ptxInfo'mint
            #== pexpectedMint
            # computationThreadScriptHash
            # policyId
            # pfpMint'computationThreadTokenAssetName
        ]
    )
    (pconstant ())
    perror

{- | The exact mint field the fraud proof policy requires:
@-1@ computation thread token and @+1@ fraud proof token, same asset name.

Built as a sorted two-entry map. The currency symbols are distinct by
construction (a script cannot be its own computation thread), so the ordering is
total, but it still has to be emitted in ascending order for the equality
against the ledger's own sorted mint field to hold.
-}
pexpectedMint ::
  forall (s :: S).
  Term
    s
    ( PAsData PCurrencySymbol
        :--> PAsData PCurrencySymbol
        :--> PAsData PTokenName
        :--> PMintValue
    )
pexpectedMint = phoistAcyclic $
  plam $ \computationThreadPolicy fraudProofPolicy assetName ->
    plet (pentry # computationThreadPolicy # assetName # (-1)) $ \burnEntry ->
      plet (pentry # fraudProofPolicy # assetName # 1) $ \mintEntry ->
        punsafeCoerce $
          pif
            (pfromData computationThreadPolicy #< pfromData fraudProofPolicy)
            (pcons @PBuiltinList # burnEntry #$ pcons # mintEntry # pnil)
            (pcons @PBuiltinList # mintEntry #$ pcons # burnEntry # pnil)

-- | A single currency-symbol entry of a mint field: one token name, one amount.
pentry ::
  forall (s :: S).
  Term
    s
    ( PAsData PCurrencySymbol
        :--> PAsData PTokenName
        :--> PInteger
        :--> PBuiltinPair
              (PAsData PCurrencySymbol)
              (PAsData (AssocMap.PSortedMap PTokenName PInteger))
    )
pentry = phoistAcyclic $
  plam $ \policy assetName amount ->
    ppairDataBuiltin
      # policy
      # pdata
        ( punsafeCoerce
            (pcons @PBuiltinList # (ppairDataBuiltin # assetName # pdata amount) # pnil)
        )

{- | Aiken @validators/fraud-proof.ak@ — the @spend@ validator.

@
validator spend {
  else(_) {
    trace @"Midgard - Fraud Proof Spending Validator"
    fail
  }
}
@

No handler at all, so every purpose fails. A minted fraud proof token can never
be spent, which is how the conviction is made permanent.
-}
fraudProofSpendValidator :: forall (s :: S). Term s (PScriptContext :--> PUnit)
fraudProofSpendValidator =
  plam $ \_ctx -> ptraceInfoError "Midgard - Fraud Proof Spending Validator"
