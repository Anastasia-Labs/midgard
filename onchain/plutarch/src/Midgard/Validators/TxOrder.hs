{- |
Module      : Midgard.Validators.TxOrder
Description : Plutarch port of
              @validators/user-events/tx-order-v1.ak@.

A transaction order is a user's request that an L2 transaction be included in a
block. Unlike a deposit or a withdrawal it moves no funds of its own — the UTxO
exists to carry the request and its authentication NFT.

Both handlers preserve the Aiken policy. Minting delegates event authentication
to the shared user-event validator and additionally authenticates the terminal
field-receipt chain named by the order payload.
-}
module Midgard.Validators.TxOrder (txOrderMintValidator, txOrderSpendValidator) where

import Plutarch.Core.Utils (pand'List)
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.LedgerApi.V3 (
  PCurrencySymbol,
  PScriptContext (..),
  PScriptHash,
  PScriptInfo (..),
  PScriptPurpose (..),
  PTokenName,
  PTxInInfo (..),
  PTxInfo (..),
  PTxOut (..),
  PTxOutRef,
 )
import Plutarch.LedgerApi.Value qualified as Value
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import DesignPatterns.SingularUtxoIndexer (poneToOne)
import Midgard.Common.Utils (PAssetTriplet (..), pgetRedeemerAt, pgetSingleAssetFromValueApartFromAda)
import Midgard.HubOracle (PHubOracleDatum (..))
import Midgard.HubOracle qualified as Hub
import Midgard.LedgerState (PTxOrderPayloadV1)
import Midgard.Settlement (PSettlementDatum (..), pvalidCountedMembership)
import Midgard.Settlement qualified as Settlement
import Midgard.TransitionTrace (PRootDomain (..))
import Midgard.UserEvents (PMintRedeemer (..), pvalidateMint)
import Midgard.UserEvents.TxOrder (
  PSpendRedeemer (..),
  PTxOrderDatum (..),
  pforcedInclusionKeyValue,
  pverifyOrderReceipts,
 )

{- | Aiken @validators/user-events/tx-order-v1.ak@ — @mint@.

The shared user-event policy authenticates the nonce, witness registration,
hub-selected address and output datum. The order-specific callback pins the
output to its single event NFT and requires either an empty material directory
or the exact terminal receipt named by the payload.
-}
txOrderMintValidator ::
  forall (s :: S).
  Term
    s
    ( PAsData PScriptHash
        :--> PAsData PScriptHash
        :--> PAsData PCurrencySymbol
        :--> PScriptContext
        :--> PUnit
    )
txOrderMintValidator =
  plam $ \hubOracle receiptScriptHash fieldReceiptPolicyId ctx -> P.do
    PScriptContext {pscriptContext'txInfo, pscriptContext'redeemer, pscriptContext'scriptInfo} <-
      pmatch ctx
    ownPolicy <-
      plet $ pmatch pscriptContext'scriptInfo $ \case
        PMintingScript policy -> policy
        _ -> perror
    PTxInfo
      { ptxInfo'inputs
      , ptxInfo'outputs
      , ptxInfo'referenceInputs
      , ptxInfo'mint
      , ptxInfo'redeemers
      , ptxInfo'validRange
      } <-
      pmatch pscriptContext'txInfo
    inputs <- plet (pfromData ptxInfo'inputs)
    referenceInputs <- plet (pfromData ptxInfo'referenceInputs)
    redeemer <-
      plet $
        pfromData (punsafeCoerce @(PAsData PMintRedeemer) (pto pscriptContext'redeemer))
    nonceInputIndex <-
      plet $
        pmatch redeemer $ \case
          PAuthenticateEvent {pauthenticate'nonceInputIndex} ->
            pfromData pauthenticate'nonceInputIndex
          _ -> perror
    PTxInInfo {ptxInInfo'outRef = txOrderId} <-
      pmatch $ pfromData (pelemAt # nonceInputIndex # inputs)
    pif
      ( pvalidateMint
          hubOracle
          ( \hubDatum ->
              pmatch hubDatum $ \PHubOracleDatum {phubOracle'txOrderAddr} ->
                phubOracle'txOrderAddr
          )
          redeemer
          ownPolicy
          inputs
          (pfromData ptxInfo'outputs)
          referenceInputs
          ptxInfo'validRange
          (pfromData ptxInfo'mint)
          (pto (pto (pfromData ptxInfo'redeemers)))
          ( ptxOrderEventValidator
              ownPolicy
              referenceInputs
              receiptScriptHash
              fieldReceiptPolicyId
              (pdata txOrderId)
          )
      )
      (pconstant ())
      perror

ptxOrderEventValidator ::
  forall (s :: S).
  Term s (PAsData PCurrencySymbol) ->
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  Term s (PAsData PScriptHash) ->
  Term s (PAsData PCurrencySymbol) ->
  Term s (PAsData PTxOutRef) ->
  Term s (PAsData PTokenName) ->
  Term s PData ->
  Term s PData ->
  Term s PBool
ptxOrderEventValidator
  ownPolicy
  referenceInputs
  receiptScriptHash
  fieldReceiptPolicyId
  txOrderId
  l1Id
  outputValueData
  txOrderPayloadData = P.do
    PAssetTriplet {passetTriplet'policy, passetTriplet'name, passetTriplet'amount} <-
      pmatch $
        pgetSingleAssetFromValueApartFromAda
          # pfromData (punsafeCoerce @(PAsData Value.PLedgerValue) outputValueData)
    payload <-
      plet $
        pfromData (punsafeCoerce @(PAsData PTxOrderPayloadV1) txOrderPayloadData)
    pand'List
      [ passetTriplet'policy #== ownPolicy
      , passetTriplet'name #== l1Id
      , pfromData passetTriplet'amount #== 1
      , pverifyOrderReceipts
          # referenceInputs
          # receiptScriptHash
          # fieldReceiptPolicyId
          # ownPolicy
          # txOrderId
          # payload
      ]

{- | Aiken @validators/user-events/tx-order-v1.ak@ — @spend@.

An order leaves once the block that included it has settled. The UTxO returns to
the user's refund address and datum, its NFT is burnt — which forces the witness
credential's unregistration, and is what the original relies on to prevent
double satisfaction — and the settlement's @forced_transactions_root@ must
contain this order under the verdict the redeemer claims.

There is only one branch, because an order has no second outcome: it is not
refunded or forwarded like a withdrawal, it is simply released once its fate is
recorded. What varies is the verdict, and that has to be corroborated by the
operator's committed root rather than asserted here.
-}
txOrderSpendValidator ::
  forall (s :: S).
  Term s (PAsData PScriptHash :--> PScriptContext :--> PUnit)
txOrderSpendValidator = plam $ \hubOracle ctx -> P.do
  PScriptContext {pscriptContext'txInfo, pscriptContext'redeemer, pscriptContext'scriptInfo} <-
    pmatch ctx
  ownOutRef <-
    plet $ pmatch pscriptContext'scriptInfo $ \case
      PSpendingScript outRef _ -> outRef
      _ -> perror
  ownDatum <-
    plet $ pmatch pscriptContext'scriptInfo $ \case
      PSpendingScript _ mDatum -> mDatum
      _ -> perror
  PTxInfo {ptxInfo'inputs, ptxInfo'outputs, ptxInfo'referenceInputs, ptxInfo'redeemers} <-
    pmatch pscriptContext'txInfo
  inputs <- plet $ pfromData ptxInfo'inputs
  outputs <- plet $ pfromData ptxInfo'outputs
  referenceInputs <- plet $ pfromData ptxInfo'referenceInputs
  redeemerList <- plet $ pto (pto (pfromData ptxInfo'redeemers))

  PSpendRedeemer
    { ptxOrderSpend'inputIndex
    , ptxOrderSpend'outputIndex
    , ptxOrderSpend'hubRefInputIndex
    , ptxOrderSpend'settlementRefInputIndex
    , ptxOrderSpend'burnRedeemerIndex
    , ptxOrderSpend'membershipProof
    , ptxOrderSpend'validityOverride
    } <-
    pmatch (pfromData (punsafeCoerce @(PAsData PSpendRedeemer) (pto pscriptContext'redeemer)))

  PHubOracleDatum {phubOracle'txOrder, phubOracle'settlement} <-
    pmatch $
      Hub.pgetDatum
        # referenceInputs
        # hubOracle
        # pfromData ptxOrderSpend'hubRefInputIndex

  PSettlementDatum {psettlement'forcedTransactionsRoot} <-
    pmatch $
      Settlement.pgetDatum
        # referenceInputs
        # phubOracle'settlement
        # pfromData ptxOrderSpend'settlementRefInputIndex

  PTxOrderDatum
    { ptxOrderDatum'event
    , ptxOrderDatum'refundAddress
    , ptxOrderDatum'refundDatum
    } <-
    pmatch $
      pmatch ownDatum $ \case
        PDJust d -> pfromData (punsafeCoerce @(PAsData PTxOrderDatum) (pto (pfromData d)))
        PDNothing -> perror

  burnAssetName <-
    plet $
      pmatch
        ( pfromData
            ( punsafeCoerce @(PAsData PMintRedeemer)
                ( pto
                    ( pfromData
                        ( pgetRedeemerAt
                            # redeemerList
                            # pdata (pcon (PMinting phubOracle'txOrder))
                            # pfromData ptxOrderSpend'burnRedeemerIndex
                        )
                    )
                )
            )
        )
        $ \case
          PBurnEventNFT {pburnEvent'nonceAssetName} -> pburnEvent'nonceAssetName
          _ -> perror

  let (txOrderId, forcedInclusionTx) =
        pforcedInclusionKeyValue
          (pforgetData ptxOrderDatum'event)
          ptxOrderSpend'validityOverride

  pif
    ( poneToOne
        (pfromData ptxOrderSpend'inputIndex)
        (pfromData ptxOrderSpend'outputIndex)
        ownOutRef
        inputs
        outputs
        -- The NFT burn forces the witness unregistration, which cannot happen
        -- twice; see the module note.
        (pconstant True)
        ( \input output -> P.do
            PTxInInfo {ptxInInfo'resolved} <- pmatch $ pfromData input
            PTxOut {ptxOut'value = ownValue} <- pmatch ptxInInfo'resolved
            PTxOut
              { ptxOut'address = outAddress
              , ptxOut'value = outValue
              , ptxOut'datum = outDatum
              , ptxOut'referenceScript = outRefScript
              } <-
              pmatch output
            pand'List
              [ pmatch outRefScript $ \case
                  PDNothing -> pconstant True
                  PDJust _ -> pconstant False
              , outAddress #== pfromData ptxOrderDatum'refundAddress
              , outDatum #== ptxOrderDatum'refundDatum
              , -- Aiken: @output == own - NFT@, stated additively so neither
                -- side needs an entry removed. See
                -- "Midgard.Validators.Deposit" for why.
                ( Value.punionWith
                    # plam (+)
                    # pto (pfromData outValue)
                    # ( Value.psingletonSortedValue
                          # pfromData phubOracle'txOrder
                          # pfromData burnAssetName
                          # 1
                      )
                )
                  #== pto (pfromData ownValue)
              , pvalidCountedMembership
                  (pdata (pcon PForcedTransactionsV1RootDomain))
                  (pfromData psettlement'forcedTransactionsRoot)
                  (pfromData ptxOrderSpend'membershipProof)
                  txOrderId
                  forcedInclusionTx
                  redeemerList
              ]
        )
    )
    (pconstant ())
    perror
