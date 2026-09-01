{- |
Module      : Midgard.Validators.Payout
Description : Plutarch port of @validators/payout.ak@ — the minting policy.

A payout is an L2 withdrawal being paid out on L1. The withdrawal validator
decides that a withdrawal is due; this policy turns that decision into a token,
and destroys the token once the funds have gone to the user.

The two branches are the token's whole life, and each is a hinge between two
other scripts rather than a decision of its own.

@MintPayout@ is a /conversion/: the payout token comes into existence in the
same transaction, and under the same asset name, as the withdrawal token going
out of existence. That shared name is what carries the withdrawal's identity
across; and requiring the mint field to be exactly those two entries is what
stops a payout being minted for a withdrawal that is not being consumed, or a
second payout riding along on one withdrawal's authority.

@BurnPayout@ is the mirror, and defers to the payout's own spend handler for
whether the funds actually reached the user. What it enforces is that the two
scripts are talking about the same payout: same input index, same hub reference.

Both branches re-derive the payout policy id from the hub oracle and require it
to equal the policy actually running. That is not redundant — it is what ties
this script to one protocol instance, so a payout token minted under a
look-alike deployment cannot be spent against this one's reserve.

The spend handler is the other half. @AddFunds@ collects funds from the reserve
into the payout, possibly over several transactions; @ConcludeWithdrawal@ pays
the collected value to the user and ends the payout. Both are value arithmetic,
and the two properties they defend are that the payout can never hold more than
its target, and that the reserve's change can never contain something the payout
still needs.
-}
module Midgard.Validators.Payout (
  payoutMintValidator,
  payoutSpendValidator,
) where

import Data.Kind (Type)
import Plutarch.Core.Utils (pand'List)
import Plutarch.LedgerApi.AssocMap qualified as AssocMap
import Plutarch.LedgerApi.Value qualified as Value
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.LedgerApi.V3 (
  PAddress,
  PCurrencySymbol,
  POutputDatum (..),
  PRedeemer,
  PScriptContext (..),
  PScriptHash,
  PScriptInfo (..),
  PScriptPurpose (..),
  PTokenName,
  PTxInInfo (..),
  PTxInfo (..),
  PTxOut (..),
  PTxOutRef,
  PLedgerValue,
 )
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce, punsafeDowncast)

import Midgard.Common.Utils (pgetRedeemerAt, pquantityOfPolicyId)
import Midgard.Common.Value (
  pfromAssetList,
  pmergeValues,
  pnegateValue,
  pnoChangeForStillNeededAssets,
  pquantityOfValue,
  pvalueIsNonNegative,
  pvalueIsPositive,
  pvalueWithoutNft,
 )
import Midgard.HubOracle (PHubOracleDatum (..))
import Midgard.HubOracle qualified as Hub
import Midgard.Payout (PMintRedeemer (..), PPayoutDatum (..), PSpendRedeemer (..))
import Midgard.Reserve qualified as Reserve
import Midgard.UserEvents.Withdrawal (PSpendPurpose (..))
import Midgard.UserEvents.Withdrawal qualified as Withdrawal

punsafeCoerceRedeemer ::
  forall (a :: S -> Type) (s :: S). Term s (PAsData PRedeemer) -> Term s (PAsData a)
punsafeCoerceRedeemer r = punsafeCoerce (pto (pfromData r))

punsafeCoerceOwnRedeemer ::
  forall (a :: S -> Type) (s :: S). Term s PRedeemer -> Term s (PAsData a)
punsafeCoerceOwnRedeemer r = punsafeCoerce (pto r)

pscriptHashOf ::
  forall (s :: S). Term s (PAsData PCurrencySymbol) -> Term s (PAsData PScriptHash)
pscriptHashOf = punsafeCoerce

punsafeCoerceData ::
  forall (a :: S -> Type) (s :: S). (PIsData a) => Term s PData -> Term s a
punsafeCoerceData d = pfromData (punsafeCoerce @(PAsData a) d)

{- | A @TxOut@'s value as a sorted value.

@PLedgerValue@ sits one wrapper further out than @PSortedValue@, and every value
helper in "Midgard.Common.Value" is typed on the latter.
-}
psorted :: forall (s :: S). Term s (PAsData PLedgerValue) -> Term s Value.PSortedValue
psorted v = pto (pfromData v)

{- | Aiken @value |> assets.tokens(policy) |> dict.to_pairs@ as a singleton.

@expect [Pair(name, qty)] = ...@ — exactly one name under the policy, with its
quantity. Failing on two names is the point: a check phrased as "how much of
this one name" says nothing about the names it was not asked about, so a second
name of the same policy could ride along unexamined.
-}
psingletonTokenOfPolicy ::
  forall (s :: S).
  Term s (AssocMap.PSortedMap PCurrencySymbol (AssocMap.PSortedMap PTokenName PInteger)) ->
  Term s (PAsData PCurrencySymbol) ->
  Term s (PBuiltinPair (PAsData PTokenName) (PAsData PInteger))
psingletonTokenOfPolicy valueMap policyId =
  pmatch (AssocMap.plookup # pfromData policyId # valueMap) $ \case
    PNothing -> perror
    PJust tokenMap -> psingletonPair (pto (pto tokenMap))

-- | @expect [p] = xs; p@ — the sole element, failing on none or more than one.
psingletonPair ::
  forall (s :: S) (a :: S -> Type).
  (PIsListLike PBuiltinList a) =>
  Term s (PBuiltinList a) ->
  Term s a
psingletonPair xs =
  pif (pnull # (ptail # xs)) (phead # xs) perror

{- | Aiken @validators/payout.ak@ — @mint@.

One parameter: the hub oracle's script hash, which is what makes every other
identity in the transaction derivable rather than asserted.
-}
payoutMintValidator ::
  forall (s :: S).
  Term s (PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
payoutMintValidator = plam $ \hubOracle ctx -> P.do
  PScriptContext {pscriptContext'txInfo, pscriptContext'redeemer, pscriptContext'scriptInfo} <-
    pmatch ctx
  ownPolicy <-
    plet $ pmatch pscriptContext'scriptInfo $ \case
      PMintingScript cs -> cs
      _ -> perror
  PTxInfo {ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'redeemers, ptxInfo'mint} <-
    pmatch pscriptContext'txInfo
  inputs <- plet $ pfromData ptxInfo'inputs
  referenceInputs <- plet $ pfromData ptxInfo'referenceInputs
  redeemers <- plet $ pto (pto (pfromData ptxInfo'redeemers))
  -- The mint field's policy map, one unwrapping down from `PMintValue`.
  mintMap <- plet $ pto (pto (pfromData ptxInfo'mint))
  redeemer <-
    plet $ pfromData (punsafeCoerceOwnRedeemer @PMintRedeemer pscriptContext'redeemer)

  pif
    ( pmatch redeemer $ \case
        --------------------------------------------------------------------
        PMintPayout
          { pmintPayout'withdrawalUtxoOutRef
          , pmintPayout'withdrawalInputIndex
          , pmintPayout'withdrawalSpendRedeemerIndex
          , pmintPayout'hubRefInputIndex
          } -> P.do
            PHubOracleDatum
              { phubOracle'payout
              , phubOracle'withdrawal
              , phubOracle'withdrawalAddr
              } <-
              pmatch
                ( Hub.pgetDatum
                    # referenceInputs
                    # pscriptHashOf hubOracle
                    # pfromData pmintPayout'hubRefInputIndex
                )

            payoutEntry <- plet $ psingletonTokenOfPolicy mintMap ownPolicy
            withdrawalEntry <- plet $ psingletonTokenOfPolicy mintMap phubOracle'withdrawal
            payoutAssetName <- plet $ pfstBuiltin # payoutEntry
            withdrawalAssetName <- plet $ pfstBuiltin # withdrawalEntry

            withdrawalInput <-
              plet $ pfromData (pelemAt # pfromData pmintPayout'withdrawalInputIndex # inputs)
            PTxInInfo {ptxInInfo'outRef, ptxInInfo'resolved} <- pmatch withdrawalInput
            PTxOut {ptxOut'address, ptxOut'value} <- pmatch ptxInInfo'resolved

            pand'List
              [ -- One protocol instance: the policy running must be the one the
                -- hub oracle names.
                ownPolicy #== phubOracle'payout
              , -- The conversion. Same name on both sides is what carries the
                -- withdrawal's identity into the payout.
                payoutAssetName #== withdrawalAssetName
              , pfromData (psndBuiltin # payoutEntry) #== 1
              , pfromData (psndBuiltin # withdrawalEntry) #== (-1)
              , -- Exactly those two entries and nothing else, so no second
                -- payout can ride along on one withdrawal's authority. The two
                -- singleton reads above already pin one name per policy; this
                -- pins the set of policies.
                plength # pto (pto mintMap) #== 2
              , pdata ptxInInfo'outRef #== pmintPayout'withdrawalUtxoOutRef
              , pdata ptxOut'address #== phubOracle'withdrawalAddr
              , -- The withdrawal being burnt is the one actually being spent.
                pquantityOf ptxOut'value phubOracle'withdrawal withdrawalAssetName #== 1
              , -- And it is being spent to start a payout, not refunded.
                pmatch
                  ( pfromData
                      ( punsafeCoerceRedeemer @Withdrawal.PSpendRedeemer $
                          pgetRedeemerAt
                            # redeemers
                            # pdata (pcon (PSpending ptxInInfo'outRef))
                            # pfromData pmintPayout'withdrawalSpendRedeemerIndex
                      )
                  )
                  $ \Withdrawal.PSpendRedeemer {Withdrawal.pwithdrawalSpend'purpose} ->
                    pmatch (pfromData pwithdrawalSpend'purpose) $ \case
                      PInitializePayout -> pconstant True
                      PRefund _ -> pconstant False
              ]
        --------------------------------------------------------------------
        PBurnPayout
          { pburnPayout'payoutInputIndex
          , pburnPayout'payoutAssetName
          , pburnPayout'payoutSpendRedeemerIndex
          , pburnPayout'hubRefInputIndex
          } -> P.do
            PHubOracleDatum {phubOracle'payout} <-
              pmatch
                ( Hub.pgetDatum
                    # referenceInputs
                    # pscriptHashOf hubOracle
                    # pfromData pburnPayout'hubRefInputIndex
                )
            payoutInputIndex <- plet $ pfromData pburnPayout'payoutInputIndex
            payoutInput <- plet $ pfromData (pelemAt # payoutInputIndex # inputs)
            PTxInInfo {ptxInInfo'outRef, ptxInInfo'resolved} <- pmatch payoutInput
            PTxOut {ptxOut'value} <- pmatch ptxInInfo'resolved

            mintEntry <- plet $ psingletonTokenOfPolicy mintMap ownPolicy

            pand'List
              [ ownPolicy #== phubOracle'payout
              , -- The whole mint field is this one burn: a burn redeemer must
                -- not be able to bring anything into existence.
                plength # pto (pto mintMap) #== 1
              , pfstBuiltin # mintEntry #== pburnPayout'payoutAssetName
              , pfromData (psndBuiltin # mintEntry) #== (-1)
              , -- The token being burnt is the one actually being spent.
                pquantityOf ptxOut'value ownPolicy pburnPayout'payoutAssetName #== 1
              , -- Both scripts must mean the same payout and the same hub.
                pmatch
                  ( pfromData
                      ( punsafeCoerceRedeemer @PSpendRedeemer $
                          pgetRedeemerAt
                            # redeemers
                            # pdata (pcon (PSpending ptxInInfo'outRef))
                            # pfromData pburnPayout'payoutSpendRedeemerIndex
                      )
                  )
                  $ \case
                    PConcludeWithdrawal
                      { ppayoutConclude'payoutInputIndex
                      , ppayoutConclude'hubRefInputIndex
                      } ->
                        pand'List
                          [ ppayoutConclude'payoutInputIndex #== pburnPayout'payoutInputIndex
                          , ppayoutConclude'hubRefInputIndex #== pburnPayout'hubRefInputIndex
                          ]
                    PAddFunds {} -> perror
              ]
    )
    (pconstant ())
    perror
  where
    -- Aiken @assets.quantity_of@ over an output's value; zero when absent.
    pquantityOf value policyId tokenName =
      pmatch (AssocMap.plookup # pfromData policyId # pto (pto (pfromData value))) $ \case
        PNothing -> 0
        PJust tokenMap ->
          pmatch (AssocMap.plookup # pfromData tokenName # tokenMap) $ \case
            PNothing -> 0
            PJust quantity -> quantity

--------------------------------------------------------------------------------
-- Spend
--------------------------------------------------------------------------------

{- | Aiken @validators/payout.ak@ — @spend@.

@AddFunds@ moves value from the reserve into the payout, and may run several
times: a payout whose target exceeds what one reserve UTxO holds is filled
incrementally. @ConcludeWithdrawal@ pays the collected value out and ends it.

The invariant the collection branch defends is that the payout can never hold
more than its target — checked from both ends, before and after the collection —
so a filled payout is exactly the target and a partially filled one is strictly
under it. Overshooting would strand reserve funds in a payout no conclusion can
release, because the conclusion requires the payout's contents to /equal/ the
target.
-}
payoutSpendValidator ::
  forall (s :: S).
  Term s (PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
payoutSpendValidator = plam $ \hubOracle ctx -> P.do
  PScriptContext {pscriptContext'txInfo, pscriptContext'redeemer, pscriptContext'scriptInfo} <-
    pmatch ctx
  ownOutRef <-
    plet $ pmatch pscriptContext'scriptInfo $ \case
      PSpendingScript outRef _ -> outRef
      _ -> perror
  payoutDatumData <-
    plet $ pmatch pscriptContext'scriptInfo $ \case
      PSpendingScript _ mDatum ->
        pmatch mDatum $ \case
          PDJust d -> pto (pfromData d)
          PDNothing -> perror
      _ -> perror
  PPayoutDatum {ppayoutDatum'l2Value, ppayoutDatum'l1Address, ppayoutDatum'l1Datum} <-
    pmatch (punsafeCoerceData @PPayoutDatum payoutDatumData)
  targetValue <- plet $ pfromAssetList # pfromData ppayoutDatum'l2Value

  PTxInfo {ptxInfo'inputs, ptxInfo'outputs, ptxInfo'referenceInputs, ptxInfo'redeemers, ptxInfo'mint} <-
    pmatch pscriptContext'txInfo
  inputs <- plet $ pfromData ptxInfo'inputs
  outputs <- plet $ pfromData ptxInfo'outputs
  referenceInputs <- plet $ pfromData ptxInfo'referenceInputs
  redeemers <- plet $ pto (pto (pfromData ptxInfo'redeemers))
  mintMap <- plet $ pto (pto (pfromData ptxInfo'mint))

  redeemer <-
    plet $ pfromData (punsafeCoerceOwnRedeemer @PSpendRedeemer pscriptContext'redeemer)

  pif
    ( pmatch redeemer $ \case
        --------------------------------------------------------------------
        PAddFunds
          { ppayoutAddFunds'payoutInputIndex
          , ppayoutAddFunds'payoutOutputIndex
          , ppayoutAddFunds'reserveInputIndex
          , ppayoutAddFunds'reserveChangeOutputIndex
          , ppayoutAddFunds'reserveSpendRedeemerIndex
          , ppayoutAddFunds'payoutSpendRedeemerIndex
          , ppayoutAddFunds'hubRefInputIndex
          } -> P.do
            hubRefInputIndex <- plet $ pfromData ppayoutAddFunds'hubRefInputIndex
            payoutInputIndex <- plet $ pfromData ppayoutAddFunds'payoutInputIndex
            reserveInputIndex <- plet $ pfromData ppayoutAddFunds'reserveInputIndex
            PHubOracleDatum {phubOracle'payout, phubOracle'payoutAddr, phubOracle'reserveAddr} <-
              pmatch (Hub.pgetDatum # referenceInputs # pscriptHashOf hubOracle # hubRefInputIndex)

            payoutInput <- plet $ pfromData (pelemAt # payoutInputIndex # inputs)
            PTxInInfo {ptxInInfo'outRef = payoutOutRef, ptxInInfo'resolved = payoutResolved} <-
              pmatch payoutInput
            PTxOut
              { ptxOut'address = payoutInAddress
              , ptxOut'value = payoutInValue
              , ptxOut'datum = payoutInDatum
              } <-
              pmatch payoutResolved
            payoutAssetName <-
              plet $ psingletonNameOfPolicy (psorted payoutInValue) phubOracle'payout

            reserveInput <- plet $ pfromData (pelemAt # reserveInputIndex # inputs)
            PTxInInfo {ptxInInfo'outRef = reserveOutRef, ptxInInfo'resolved = reserveResolved} <-
              pmatch reserveInput
            PTxOut
              { ptxOut'address = reserveInAddress
              , ptxOut'value = reserveInValue
              , ptxOut'datum = reserveInDatum
              , ptxOut'referenceScript = reserveInRefScript
              } <-
              pmatch reserveResolved

            payoutOutput <-
              plet $ pfromData (pelemAt # pfromData ppayoutAddFunds'payoutOutputIndex # outputs)
            PTxOut
              { ptxOut'address = payoutOutAddress
              , ptxOut'value = payoutOutValue
              , ptxOut'datum = payoutOutDatum
              , ptxOut'referenceScript = payoutOutRefScript
              } <-
              pmatch payoutOutput

            currentValue <-
              plet $ pvalueWithoutNft # psorted payoutInValue # phubOracle'payout # payoutAssetName
            payoutOutWithoutNft <-
              plet $ pvalueWithoutNft # psorted payoutOutValue # phubOracle'payout # payoutAssetName
            collectedValue <-
              plet $ pmergeValues # payoutOutWithoutNft #$ pnegateValue # currentValue
            remainingNeeded <-
              plet $ pmergeValues # targetValue #$ pnegateValue # payoutOutWithoutNft
            changeValue <-
              plet $
                preserveChangeValue
                  outputs
                  phubOracle'reserveAddr
                  ppayoutAddFunds'reserveChangeOutputIndex

            pand'List
              [ -- The payout being spent is this one, at the address the hub
                -- names, with its datum unchanged on both sides.
                payoutOutRef #== ownOutRef
              , pdata payoutInAddress #== phubOracle'payoutAddr
              , pinlineDatumData payoutInDatum #== payoutDatumData
              , pdata payoutOutAddress #== phubOracle'payoutAddr
              , pinlineDatumData payoutOutDatum #== payoutDatumData
              , pmatch payoutOutRefScript $ \case
                  PDNothing -> pconstant True
                  PDJust _ -> pconstant False
              , -- Exactly one payout in and one out, and no second token of the
                -- payout policy anywhere: two payouts in one transaction could
                -- each count the same reserve input as their collection.
                pcountInputsWithNft inputs phubOracle'payout payoutAssetName #== 1
              , pcountInputsWithPolicy inputs phubOracle'payout #== 1
              , psingletonNameOfPolicy (psorted payoutOutValue) phubOracle'payout
                  #== payoutAssetName
              , pcountOutputsWithNft outputs phubOracle'payout payoutAssetName #== 1
              , pcountOutputsWithPolicy outputs phubOracle'payout #== 1
              , -- The reserve input is a bare, datumless UTxO at the reserve
                -- address, and it is the only one.
                pdata reserveInAddress #== phubOracle'reserveAddr
              , pmatch reserveInDatum $ \case
                  PNoOutputDatum -> pconstant True
                  _ -> pconstant False
              , pmatch reserveInRefScript $ \case
                  PDNothing -> pconstant True
                  PDJust _ -> pconstant False
              , pcountInputsAtAddress inputs phubOracle'reserveAddr #== 1
              , -- The reserve's own redeemer must agree about every index, so
                -- neither script can be pointed at a different pairing.
                pmatch
                  ( pfromData
                      ( punsafeCoerceRedeemer @Reserve.PSpendRedeemer $
                          pgetRedeemerAt
                            # redeemers
                            # pdata (pcon (PSpending reserveOutRef))
                            # pfromData ppayoutAddFunds'reserveSpendRedeemerIndex
                      )
                  )
                  $ \Reserve.PSpend
                      { Reserve.preserveSpend'reserveInputIndex
                      , Reserve.preserveSpend'payoutInputIndex
                      , Reserve.preserveSpend'payoutSpendRedeemerIndex
                      , Reserve.preserveSpend'hubRefInputIndex
                      } ->
                      pand'List
                        [ preserveSpend'reserveInputIndex #== ppayoutAddFunds'reserveInputIndex
                        , preserveSpend'payoutInputIndex #== ppayoutAddFunds'payoutInputIndex
                        , preserveSpend'payoutSpendRedeemerIndex
                            #== ppayoutAddFunds'payoutSpendRedeemerIndex
                        , preserveSpend'hubRefInputIndex #== ppayoutAddFunds'hubRefInputIndex
                        ]
              , -- And this redeemer must be the one at the index it claims.
                pmatch
                  ( pfromData
                      ( punsafeCoerceRedeemer @PSpendRedeemer $
                          pgetRedeemerAt
                            # redeemers
                            # pdata (pcon (PSpending ownOutRef))
                            # pfromData ppayoutAddFunds'payoutSpendRedeemerIndex
                      )
                  )
                  $ \case
                    PAddFunds {ppayoutAddFunds'reserveInputIndex = selfReserveInputIndex} ->
                      selfReserveInputIndex #== ppayoutAddFunds'reserveInputIndex
                    PConcludeWithdrawal {} -> perror
              , -- The payout never exceeds its target, checked before and after.
                pvalueIsNonNegative #$ pmergeValues # targetValue #$ pnegateValue # currentValue
              , pvalueIsNonNegative # remainingNeeded
              , -- The collection moved value in, and moved nothing out.
                pvalueIsNonNegative # collectedValue
              , pvalueIsPositive # collectedValue
              , -- Everything the reserve gave up either landed in the payout or
                -- came back as change; nothing may go anywhere else.
                (pmergeValues # collectedValue # changeValue) #== psorted reserveInValue
              , pnoChangeForStillNeededAssets # changeValue # remainingNeeded
              , -- Collection mints nothing: a payout's identity is fixed when
                -- its token is minted, and this branch must not touch it.
                pnull # pto (pto mintMap)
              ]
        --------------------------------------------------------------------
        PConcludeWithdrawal
          { ppayoutConclude'payoutInputIndex
          , ppayoutConclude'l1OutputIndex
          , ppayoutConclude'burnRedeemerIndex
          , ppayoutConclude'hubRefInputIndex
          } -> P.do
            hubRefInputIndex <- plet $ pfromData ppayoutConclude'hubRefInputIndex
            payoutInputIndex <- plet $ pfromData ppayoutConclude'payoutInputIndex
            PHubOracleDatum {phubOracle'payout, phubOracle'payoutAddr} <-
              pmatch (Hub.pgetDatum # referenceInputs # pscriptHashOf hubOracle # hubRefInputIndex)

            payoutInput <- plet $ pfromData (pelemAt # payoutInputIndex # inputs)
            PTxInInfo {ptxInInfo'outRef = payoutOutRef, ptxInInfo'resolved = payoutResolved} <-
              pmatch payoutInput
            PTxOut
              { ptxOut'address = payoutInAddress
              , ptxOut'value = payoutInValue
              , ptxOut'datum = payoutInDatum
              } <-
              pmatch payoutResolved
            payoutAssetName <-
              plet $ psingletonNameOfPolicy (psorted payoutInValue) phubOracle'payout

            l1Output <-
              plet $ pfromData (pelemAt # pfromData ppayoutConclude'l1OutputIndex # outputs)
            PTxOut
              { ptxOut'address = l1Address
              , ptxOut'value = l1Value
              , ptxOut'datum = l1Datum
              , ptxOut'referenceScript = l1RefScript
              } <-
              pmatch l1Output

            pand'List
              [ payoutOutRef #== ownOutRef
              , pdata payoutInAddress #== phubOracle'payoutAddr
              , pinlineDatumData payoutInDatum #== payoutDatumData
              , pcountInputsWithPolicy inputs phubOracle'payout #== 1
              , -- The payout must be exactly full: concluding an underfunded
                -- payout would short the user, and the collection branch's
                -- ceiling means it can never be over.
                (pvalueWithoutNft # psorted payoutInValue # phubOracle'payout # payoutAssetName
                  #== targetValue)
              , -- The user gets the target value, at the address and with the
                -- datum the payout named when it was created.
                pdata l1Address #== ppayoutDatum'l1Address
              , l1Datum #== ppayoutDatum'l1Datum
              , psorted l1Value #== targetValue
              , pmatch l1RefScript $ \case
                  PDNothing -> pconstant True
                  PDJust _ -> pconstant False
              , -- The payout token does not survive, and no payout-policy token
                -- of any name does.
                pcountOutputsWithNft outputs phubOracle'payout payoutAssetName #== 0
              , pcountOutputsWithPolicy outputs phubOracle'payout #== 0
              , -- And no output recreates this payout's datum at the payout
                -- address, which would be a payout continuing without its token.
                pnot
                  #$ pany
                  # plam
                    ( \out ->
                        pmatch (pfromData out) $ \PTxOut {ptxOut'address, ptxOut'datum} ->
                          (pdata ptxOut'address #== phubOracle'payoutAddr)
                            #&& (pinlineDatumData' ptxOut'datum #== pcon (PDJust (pdata payoutDatumData)))
                    )
                  # outputs
              , -- The burn and this spend must agree about which payout is
                -- ending, and about the hub they read it from.
                pmatch
                  ( pfromData
                      ( punsafeCoerceRedeemer @PMintRedeemer $
                          pgetRedeemerAt
                            # redeemers
                            # pdata (pcon (PMinting phubOracle'payout))
                            # pfromData ppayoutConclude'burnRedeemerIndex
                      )
                  )
                  $ \case
                    PBurnPayout
                      { pburnPayout'payoutInputIndex
                      , pburnPayout'payoutAssetName
                      , pburnPayout'payoutSpendRedeemerIndex
                      , pburnPayout'hubRefInputIndex
                      } ->
                        pand'List
                          [ pburnPayout'payoutInputIndex #== ppayoutConclude'payoutInputIndex
                          , pburnPayout'payoutAssetName #== payoutAssetName
                          , pburnPayout'hubRefInputIndex #== ppayoutConclude'hubRefInputIndex
                          , -- And the redeemer the burn points back at is this one.
                            pmatch
                              ( pfromData
                                  ( punsafeCoerceRedeemer @PSpendRedeemer $
                                      pgetRedeemerAt
                                        # redeemers
                                        # pdata (pcon (PSpending ownOutRef))
                                        # pfromData pburnPayout'payoutSpendRedeemerIndex
                                  )
                              )
                              $ \case
                                PConcludeWithdrawal
                                  { ppayoutConclude'payoutInputIndex = selfPayoutInputIndex
                                  , ppayoutConclude'l1OutputIndex = selfL1OutputIndex
                                  , ppayoutConclude'burnRedeemerIndex = selfBurnRedeemerIndex
                                  , ppayoutConclude'hubRefInputIndex = selfHubRefInputIndex
                                  } ->
                                    pand'List
                                      [ selfPayoutInputIndex #== ppayoutConclude'payoutInputIndex
                                      , selfL1OutputIndex #== ppayoutConclude'l1OutputIndex
                                      , selfBurnRedeemerIndex #== ppayoutConclude'burnRedeemerIndex
                                      , selfHubRefInputIndex #== ppayoutConclude'hubRefInputIndex
                                      ]
                                PAddFunds {} -> perror
                          ]
                    PMintPayout {} -> perror
              ]
    )
    (pconstant ())
    perror
  where
    pinlineDatumData d =
      pmatch d $ \case
        POutputDatum {poutputDatum'outputDatum} -> pto poutputDatum'outputDatum
        _ -> perror
    -- The same read, but total: used where an output merely might carry one.
    pinlineDatumData' d =
      pmatch d $ \case
        POutputDatum {poutputDatum'outputDatum} -> pcon (PDJust (pdata (pto poutputDatum'outputDatum)))
        _ -> pcon PDNothing

{- | Aiken @singleton_asset_name_with_policy@.

@expect [Pair(asset_name, 1)] = value |> assets.tokens(policy) |> dict.to_pairs@
— one name under the policy, at quantity one.
-}
psingletonNameOfPolicy ::
  forall (s :: S).
  Term s Value.PSortedValue ->
  Term s (PAsData PCurrencySymbol) ->
  Term s (PAsData PTokenName)
psingletonNameOfPolicy value policyId =
  pmatch (AssocMap.plookup # pfromData policyId # pto value) $ \case
    PNothing -> perror
    PJust tokenMap -> P.do
      entry <- plet $ psingletonPair (pto (pto tokenMap))
      pif (pfromData (psndBuiltin # entry) #== 1) (pfstBuiltin # entry) perror

{- | Aiken @reserve_change_value@.

The reserve's change output, or the zero value when the redeemer names none.
When one is named it must be a bare UTxO at the reserve address carrying a
non-zero value — an empty change output would be a way to claim change was
returned while returning nothing.
-}
preserveChangeValue ::
  forall (s :: S).
  Term s (PBuiltinList (PAsData PTxOut)) ->
  Term s (PAsData PAddress) ->
  Term s (PMaybeData PInteger) ->
  Term s Value.PSortedValue
preserveChangeValue outputs reserveAddr mIndex =
  pmatch mIndex $ \case
    -- Aiken @assets.zero@: an empty policy map.
    PDNothing -> punsafeDowncast (punsafeDowncast (punsafeDowncast (pcon PNil)))
    PDJust index -> P.do
      PTxOut {ptxOut'address, ptxOut'value, ptxOut'datum, ptxOut'referenceScript} <-
        pmatch $ pfromData (pelemAt # pfromData index # outputs)
      pif
        ( pand'List
            [ pdata ptxOut'address #== reserveAddr
            , pmatch ptxOut'datum $ \case
                PNoOutputDatum -> pconstant True
                _ -> pconstant False
            , pmatch ptxOut'referenceScript $ \case
                PDNothing -> pconstant True
                PDJust _ -> pconstant False
            , pnot #$ pnull #$ pto (pto (pto (psorted ptxOut'value)))
            ]
        )
        (psorted ptxOut'value)
        perror

-- | Aiken @count_inputs_with_nft@ / @count_outputs_with_nft@ and their policy variants.
pcountInputsWithNft ::
  forall (s :: S).
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  Term s (PAsData PCurrencySymbol) ->
  Term s (PAsData PTokenName) ->
  Term s PInteger
pcountInputsWithNft inputs policyId assetName =
  pcountBy inputs $ \input ->
    pmatch (pfromData input) $ \PTxInInfo {ptxInInfo'resolved} ->
      pmatch ptxInInfo'resolved $ \PTxOut {ptxOut'value} ->
        pquantityOfValue # psorted ptxOut'value # policyId # assetName #== 1

pcountOutputsWithNft ::
  forall (s :: S).
  Term s (PBuiltinList (PAsData PTxOut)) ->
  Term s (PAsData PCurrencySymbol) ->
  Term s (PAsData PTokenName) ->
  Term s PInteger
pcountOutputsWithNft outputs policyId assetName =
  pcountBy outputs $ \out ->
    pmatch (pfromData out) $ \PTxOut {ptxOut'value} ->
      pquantityOfValue # psorted ptxOut'value # policyId # assetName #== 1

pcountInputsWithPolicy ::
  forall (s :: S).
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  Term s (PAsData PCurrencySymbol) ->
  Term s PInteger
pcountInputsWithPolicy inputs policyId =
  pcountBy inputs $ \input ->
    pmatch (pfromData input) $ \PTxInInfo {ptxInInfo'resolved} ->
      pmatch ptxInInfo'resolved $ \PTxOut {ptxOut'value} ->
        pnot # (pquantityOfPolicyId # pfromData ptxOut'value # policyId #== 0)

pcountOutputsWithPolicy ::
  forall (s :: S).
  Term s (PBuiltinList (PAsData PTxOut)) ->
  Term s (PAsData PCurrencySymbol) ->
  Term s PInteger
pcountOutputsWithPolicy outputs policyId =
  pcountBy outputs $ \out ->
    pmatch (pfromData out) $ \PTxOut {ptxOut'value} ->
      pnot # (pquantityOfPolicyId # pfromData ptxOut'value # policyId #== 0)

pcountInputsAtAddress ::
  forall (s :: S).
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  Term s (PAsData PAddress) ->
  Term s PInteger
pcountInputsAtAddress inputs address =
  pcountBy inputs $ \input ->
    pmatch (pfromData input) $ \PTxInInfo {ptxInInfo'resolved} ->
      pmatch ptxInInfo'resolved $ \PTxOut {ptxOut'address} ->
        pdata ptxOut'address #== address

-- | Aiken @list.count@.
pcountBy ::
  forall (s :: S) (a :: S -> Type).
  (PIsListLike PBuiltinList a) =>
  Term s (PBuiltinList a) ->
  (Term s a -> Term s PBool) ->
  Term s PInteger
pcountBy xs predicate =
  pfoldr # plam (\x acc -> pif (predicate x) (acc + 1) acc) # 0 # xs
