{- |
Module      : Midgard.Validators.Reserve
Description : Plutarch port of @validators/reserve.ak@.

The reserve holds protocol funds. Its spending validator does not decide how
much moves — it delegates that entirely to the payout validator, and confines
itself to checking that the two scripts are looking at the same pair of inputs
and that nothing is minted along the way.
-}
module Midgard.Validators.Reserve (
  reserveSpendValidator,
  reserveWithdrawValidator,
) where

import Plutarch.Core.Utils (pand'List)
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.LedgerApi.V3 (
  POutputDatum (..),
  PScriptContext (..),
  PScriptHash,
  PScriptInfo (..),
  PScriptPurpose (..),
  PTxInInfo (..),
  PTxInfo (..),
  PTxOut (..),
  PTxOutRef,
 )
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.Common.Utils (pgetRedeemerAt, pquantityOfPolicyId)
import Midgard.HubOracle (PHubOracleDatum (..), pgetDatum)
import Midgard.Payout qualified as Payout
import Midgard.Reserve (PSpendRedeemer (..))

{- | Aiken @validators/reserve.ak@ — the @spend@ handler.

Working through the original in order:

  * the datum must be absent — a reserve UTxO is pure value, and a datum on it
    would be a UTxO this validator does not understand;
  * the hub oracle supplies the payout policy id and the payout and reserve
    addresses, so none of them come from the redeemer;
  * the input at @reserve_input_index@ must be the one actually being spent
    (@own_out_ref@), sit at the hub's reserve address, and carry no datum and no
    reference script;
  * the input at @payout_input_index@ must sit at the hub's payout address and
    hold exactly one token of the payout policy;
  * the payout's own spend redeemer must be @AddFunds@ and must agree with this
    redeemer on all four indices;
  * the reserve redeemer the payout points back at must be this one;
  * and the transaction must mint nothing.

The cross-checking of indices in both directions is the substance here. Each
script is handed indices by an untrusted caller; requiring that the two scripts
were given consistent indices, and that each points back at the other, is what
stops a transaction from showing the payout validator one pairing and the
reserve validator another.
-}
reserveSpendValidator ::
  forall (s :: S).
  Term
    s
    ( PAsData PScriptHash
        :--> PScriptContext
        :--> PUnit
    )
reserveSpendValidator = plam $ \hubOracle ctx -> P.do
  PScriptContext {pscriptContext'txInfo, pscriptContext'redeemer, pscriptContext'scriptInfo} <-
    pmatch ctx
  ownOutRef <-
    plet $ pmatch pscriptContext'scriptInfo $ \case
      PSpendingScript outRef mDatum ->
        -- `expect datum == None`
        pmatch mDatum $ \case
          PDNothing -> outRef
          _ -> perror
      _ -> perror
  PTxInfo {ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'redeemers, ptxInfo'mint} <-
    pmatch pscriptContext'txInfo
  inputs <- plet $ pfromData ptxInfo'inputs

  PSpend
    { preserveSpend'reserveInputIndex
    , preserveSpend'payoutInputIndex
    , preserveSpend'payoutSpendRedeemerIndex
    , preserveSpend'hubRefInputIndex
    } <-
    pmatch $
      pfromData $
        punsafeCoerce @(PAsData PSpendRedeemer) (pto pscriptContext'redeemer)

  PHubOracleDatum {phubOracle'payout, phubOracle'payoutAddr, phubOracle'reserveAddr} <-
    pmatch $
      pgetDatum
        # pfromData ptxInfo'referenceInputs
        # hubOracle
        # pfromData preserveSpend'hubRefInputIndex

  -- The reserve input: identity, address, and emptiness of datum/ref-script.
  reserveInput <-
    plet $ pelemAt # pfromData preserveSpend'reserveInputIndex # inputs
  PTxInInfo {ptxInInfo'outRef = reserveOutRef, ptxInInfo'resolved = reserveResolved} <-
    pmatch $ pfromData reserveInput
  PTxOut
    { ptxOut'address = reserveAddress
    , ptxOut'datum = reserveDatum
    , ptxOut'referenceScript = reserveRefScript
    } <-
    pmatch reserveResolved

  -- The payout input: address and the single payout-policy token.
  payoutInput <-
    plet $ pelemAt # pfromData preserveSpend'payoutInputIndex # inputs
  PTxInInfo {ptxInInfo'outRef = payoutOutRef, ptxInInfo'resolved = payoutResolved} <-
    pmatch $ pfromData payoutInput
  PTxOut
    { ptxOut'address = payoutAddress
    , ptxOut'value = payoutValue
    , ptxOut'datum = payoutDatum
    } <-
    pmatch payoutResolved

  -- The payout's own redeemer, pinned to the payout input's spending purpose.
  redeemerList <- plet $ pto (pto (pfromData ptxInfo'redeemers))
  Payout.PAddFunds
    { ppayoutAddFunds'payoutInputIndex
    , ppayoutAddFunds'reserveInputIndex
    , ppayoutAddFunds'reserveSpendRedeemerIndex
    , ppayoutAddFunds'payoutSpendRedeemerIndex
    , ppayoutAddFunds'hubRefInputIndex
    } <-
    pmatch $
      pfromData $
        punsafeCoerce @(PAsData Payout.PSpendRedeemer) $
          pto $
            pfromData $
              pgetRedeemerAt
                # redeemerList
                # pdata (pcon (PSpending payoutOutRef))
                # pfromData preserveSpend'payoutSpendRedeemerIndex

  -- And the reserve redeemer the payout points back at must be this one.
  PSpend {preserveSpend'reserveInputIndex = selfReserveInputIndex} <-
    pmatch $
      pfromData $
        punsafeCoerce @(PAsData PSpendRedeemer) $
          pto $
            pfromData $
              pgetRedeemerAt
                # redeemerList
                # pdata (pcon (PSpending ownOutRef))
                # pfromData ppayoutAddFunds'reserveSpendRedeemerIndex

  pif
    ( pand'List
        [ reserveOutRef #== ownOutRef
        , reserveAddress #== pfromData phubOracle'reserveAddr
        , pisNoDatum # reserveDatum
        , pmatch reserveRefScript $ \case
            PDNothing -> pconstant True
            _ -> pconstant False
        , payoutAddress #== pfromData phubOracle'payoutAddr
        , pisInlineDatum # payoutDatum
        , pquantityOfPolicyId # pfromData payoutValue # phubOracle'payout #== 1
        , ppayoutAddFunds'reserveInputIndex #== preserveSpend'reserveInputIndex
        , ppayoutAddFunds'payoutInputIndex #== preserveSpend'payoutInputIndex
        , ppayoutAddFunds'hubRefInputIndex #== preserveSpend'hubRefInputIndex
        , ppayoutAddFunds'payoutSpendRedeemerIndex #== preserveSpend'payoutSpendRedeemerIndex
        , selfReserveInputIndex #== preserveSpend'reserveInputIndex
        , -- `expect mint == assets.zero`: PMintValue carries no Ada entry, so an
          -- empty entry list is exactly a zero mint.
          pnull # pto (pto (pto (pto (pfromData ptxInfo'mint))))
        ]
    )
    (pconstant ())
    perror

-- | Aiken's @datum: NoDatum@ pattern on the reserve input.
pisNoDatum :: forall (s :: S). Term s (POutputDatum :--> PBool)
pisNoDatum = phoistAcyclic $
  plam $ \d -> pmatch d $ \case
    PNoOutputDatum -> pconstant True
    _ -> pconstant False

-- | Aiken's @datum: InlineDatum(_)@ pattern on the payout input.
pisInlineDatum :: forall (s :: S). Term s (POutputDatum :--> PBool)
pisInlineDatum = phoistAcyclic $
  plam $ \d -> pmatch d $ \case
    POutputDatum _ -> pconstant True
    _ -> pconstant False

{- | Aiken @validators/reserve.ak@ — the @withdraw@ validator.

@
validator withdraw {
  else(_) {
    // `reserve_observer` is intentionally deferred. Keep this fail-closed until
    // the observer protocol is specified and reviewed.
    fail
  }
}
@

Deliberately fail-closed, and ported as such. The hub oracle datum already
carries a @reserve_observer@ field, so the wiring exists ahead of the protocol;
this must stay failing until that protocol is specified and reviewed.
-}
reserveWithdrawValidator :: forall (s :: S). Term s (PScriptContext :--> PUnit)
reserveWithdrawValidator = plam $ \_ctx -> perror
