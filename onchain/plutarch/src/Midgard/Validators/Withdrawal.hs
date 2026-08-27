{- |
Module      : Midgard.Validators.Withdrawal
Description : Partial Plutarch port of @validators/user-events/withdrawal.ak@.

The withdrawal event: an L2 request to move funds back to L1. The minting policy
authenticates a new withdrawal UTxO and burns its NFT on the way out.

Both sides are ported. The @spend@ side either opens a payout accumulator for a
withdrawal the operator judged valid, or refunds one it did not.
-}
module Midgard.Validators.Withdrawal (
  withdrawalMintValidator,
  withdrawalSpendValidator,
) where

import Plutarch.Core.Utils (pand'List)
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
 )
import Plutarch.LedgerApi.Value qualified as Value
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import DesignPatterns.SingularUtxoIndexer (poneToOne)
import Midgard.Common.Utils (
  PAssetTriplet (..),
  pgetRedeemerAt,
  pgetSingleAssetFromValueApartFromAda,
 )
import Midgard.Common.Value (
  pfromAssetList,
  pmergeValues,
  pnegateValue,
  pvalueIsNonNegative,
 )
import Midgard.HubOracle (PHubOracleDatum (..))
import Midgard.HubOracle qualified as Hub
import Midgard.LedgerState (
  PWithdrawalBody (..),
  PWithdrawalInfo (..),
  PWithdrawalValidity (..),
  punsafeEventToKeyValuePair,
 )
import Midgard.Payout (PPayoutDatum (..))
import Midgard.Payout qualified as Payout
import Midgard.Settlement (PSettlementDatum (..), pvalidCountedMembership)
import Midgard.Settlement qualified as Settlement
import Midgard.TransitionTrace (PRootDomain (..), PRootMembershipProof)
import Midgard.UserEvents (PMintRedeemer (..), pvalidateMint)
import Midgard.UserEvents.Withdrawal qualified as W

{- | Aiken @validators/user-events/withdrawal.ak@ — @mint@.

Delegates to "Midgard.UserEvents".'Midgard.UserEvents.pvalidateMint' with the
hub oracle's @withdrawal_addr@, then adds two withdrawal-specific checks:

  * the produced UTxO holds Ada and the authentication NFT and nothing else —
    stricter than the deposit, which is allowed a bounded basket of assets,
    because a withdrawal request carries no funds of its own; and
  * the event is marked @WithdrawalIsValid@. Only valid withdrawals may
    initialise a payout accumulator, so an invalid one must never be minted in
    the first place.
-}
withdrawalMintValidator ::
  forall (s :: S).
  Term s (PAsData PScriptHash :--> PScriptContext :--> PUnit)
withdrawalMintValidator = plam $ \hubOracle ctx -> P.do
  PScriptContext {pscriptContext'txInfo, pscriptContext'redeemer, pscriptContext'scriptInfo} <-
    pmatch ctx
  ownPolicy <-
    plet $ pmatch pscriptContext'scriptInfo $ \case
      PMintingScript cs -> cs
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
  redeemer <-
    plet $
      pfromData (punsafeCoerce @(PAsData PMintRedeemer) (pto pscriptContext'redeemer))
  pif
    ( pvalidateMint
        hubOracle
        ( \hubDatum ->
            pmatch hubDatum $
              \(PHubOracleDatum {phubOracle'withdrawalAddr}) -> phubOracle'withdrawalAddr
        )
        redeemer
        ownPolicy
        (pfromData ptxInfo'inputs)
        (pfromData ptxInfo'outputs)
        (pfromData ptxInfo'referenceInputs)
        ptxInfo'validRange
        (pfromData ptxInfo'mint)
        (pto (pto (pfromData ptxInfo'redeemers)))
        (pwithdrawalEventValidator ownPolicy)
    )
    (pconstant ())
    perror

-- | The withdrawal-specific half of the mint check.
pwithdrawalEventValidator ::
  forall (s :: S).
  Term s (PAsData PCurrencySymbol) ->
  Term s (PAsData PTokenName) ->
  Term s PData ->
  Term s PData ->
  Term s PBool
pwithdrawalEventValidator ownPolicy l1Id outputValueData withdrawalInfoData = P.do
  PAssetTriplet {passetTriplet'policy, passetTriplet'name, passetTriplet'amount} <-
    pmatch $
      pgetSingleAssetFromValueApartFromAda
        # pfromData (punsafeCoerce @(PAsData Value.PLedgerValue) outputValueData)
  PWithdrawalInfo {pwithdrawalInfo'validity} <-
    pmatch (pfromData (punsafeCoerce @(PAsData PWithdrawalInfo) withdrawalInfoData))
  pand'List
    [ passetTriplet'policy #== ownPolicy
    , passetTriplet'name #== l1Id
    , pfromData passetTriplet'amount #== 1
    , pmatch (pfromData pwithdrawalInfo'validity) $ \case
        PWithdrawalIsValid -> pconstant True
        _ -> pconstant False
    ]

{- | Aiken @validators/user-events/withdrawal.ak@ — @spend@.

A withdrawal leaves one of two ways, and both begin the same: the event's NFT
must be burnt (which forces the witness credential's unregistration, and is what
the original relies on to prevent double satisfaction), the spend must resolve
through the redeemer's indices, and the produced output must carry no reference
script.

@InitializePayout@ moves the request forward into a payout accumulator. It is
allowed only for a withdrawal the settlement's tree records as
@WithdrawalIsValid@, the payout policy's own @MintPayout@ redeemer must agree
with this one on every shared index, the accumulator's opening value must not
already exceed the L2 target, and the payout datum must carry the target,
address and datum across unchanged.

@Refund@ returns it to the user. The verdict claimed in the redeemer must be
something other than @WithdrawalIsValid@, and the settlement's tree must record
that same verdict — so a user cannot refund a withdrawal that was in fact valid,
and cannot invent a verdict the operator never gave.
-}
withdrawalSpendValidator ::
  forall (s :: S).
  Term s (PAsData PScriptHash :--> PScriptContext :--> PUnit)
withdrawalSpendValidator = plam $ \hubOracle ctx -> P.do
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

  W.PSpendRedeemer
    { W.pwithdrawalSpend'inputIndex
    , W.pwithdrawalSpend'outputIndex
    , W.pwithdrawalSpend'hubRefInputIndex
    , W.pwithdrawalSpend'settlementRefInputIndex
    , W.pwithdrawalSpend'burnRedeemerIndex
    , W.pwithdrawalSpend'payoutMintRedeemerIndex
    , W.pwithdrawalSpend'membershipProof
    , W.pwithdrawalSpend'inclusionProofScriptWithdrawRedeemerIndex
    , W.pwithdrawalSpend'purpose
    } <-
    pmatch (pfromData (punsafeCoerce @(PAsData W.PSpendRedeemer) (pto pscriptContext'redeemer)))

  PHubOracleDatum
    { phubOracle'withdrawal
    , phubOracle'settlement
    , phubOracle'payout
    , phubOracle'payoutAddr
    } <-
    pmatch $
      Hub.pgetDatum
        # referenceInputs
        # hubOracle
        # pfromData pwithdrawalSpend'hubRefInputIndex

  PSettlementDatum {psettlement'withdrawalsRoot} <-
    pmatch $
      Settlement.pgetDatum
        # referenceInputs
        # phubOracle'settlement
        # pfromData pwithdrawalSpend'settlementRefInputIndex

  W.PWithdrawalDatum
    { W.pwithdrawalDatum'event
    , W.pwithdrawalDatum'refundAddress
    , W.pwithdrawalDatum'refundDatum
    } <-
    pmatch $
      pmatch ownDatum $ \case
        PDJust d -> pfromData (punsafeCoerce @(PAsData W.PWithdrawalDatum) (pto (pfromData d)))
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
                            # pdata (pcon (PMinting phubOracle'withdrawal))
                            # pfromData pwithdrawalSpend'burnRedeemerIndex
                        )
                    )
                )
            )
        )
        $ \case
          PBurnEventNFT {pburnEvent'nonceAssetName} -> pburnEvent'nonceAssetName
          _ -> perror

  let (withdrawalId, withdrawalInfoData) =
        punsafeEventToKeyValuePair (pforgetData pwithdrawalDatum'event)

  pif
    ( poneToOne
        (pfromData pwithdrawalSpend'inputIndex)
        (pfromData pwithdrawalSpend'outputIndex)
        ownOutRef
        inputs
        outputs
        -- See the module note: the witness unregistration forced by the burn is
        -- what makes this safe.
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
            {- The input value with the withdrawal NFT *added back*, not the
            input value with it removed. Subtracting leaves a zero entry that
            Plutarch's union does not drop, so the literal form of Aiken's
            @own_value |> assets.add(policy, name, -1)@ compares unequal against
            a well-formed output. Both branches below therefore state their
            equation with the NFTs moved to the side they are absent from. -}
            plusWithdrawalNft <-
              plet $
                plam
                  ( \v ->
                      Value.punionWith
                        # plam (+)
                        # v
                        # ( Value.psingletonSortedValue
                              # pfromData phubOracle'withdrawal
                              # pfromData burnAssetName
                              # 1
                          )
                  )
            pand'List
              [ pmatch outRefScript $ \case
                  PDNothing -> pconstant True
                  PDJust _ -> pconstant False
              , pmatch (pfromData pwithdrawalSpend'purpose) $ \case
                  W.PInitializePayout ->
                    pinitializePayout
                      phubOracle'payout
                      phubOracle'payoutAddr
                      burnAssetName
                      (pto (pfromData ownValue))
                      (plusWithdrawalNft # pto (pfromData outValue))
                      outAddress
                      outValue
                      outDatum
                      withdrawalId
                      withdrawalInfoData
                      (pfromData psettlement'withdrawalsRoot)
                      (pfromData pwithdrawalSpend'membershipProof)
                      redeemerList
                      ownOutRef
                      (pfromData pwithdrawalSpend'inputIndex)
                      (pfromData pwithdrawalSpend'hubRefInputIndex)
                      (pfromData pwithdrawalSpend'payoutMintRedeemerIndex)
                  W.PRefund {W.prefund'validityOverride} ->
                    prefundBranch
                      prefund'validityOverride
                      (pto (pfromData ownValue))
                      (plusWithdrawalNft # pto (pfromData outValue))
                      outAddress
                      outValue
                      outDatum
                      pwithdrawalDatum'refundAddress
                      pwithdrawalDatum'refundDatum
                      withdrawalId
                      withdrawalInfoData
                      (pfromData psettlement'withdrawalsRoot)
                      (pfromData pwithdrawalSpend'membershipProof)
                      redeemerList
              ]
        )
    )
    (pconstant ())
    perror

{- | The @InitializePayout@ branch.

Every field of the payout policy's @MintPayout@ redeemer is cross-checked
against this one, and this validator's own spend redeemer is re-read out of the
transaction and checked to agree with itself. That looks redundant but is not:
the payout script trusts those indices, so they have to be pinned from both
sides.
-}
pinitializePayout ::
  forall (s :: S).
  Term s (PAsData PCurrencySymbol) ->
  Term s (PAsData PAddress) ->
  Term s (PAsData PTokenName) ->
  Term s Value.PSortedValue ->
  Term s Value.PSortedValue ->
  Term s PAddress ->
  Term s (PAsData Value.PLedgerValue) ->
  Term s POutputDatum ->
  Term s PData ->
  Term s PData ->
  Term s PByteString ->
  Term s PRootMembershipProof ->
  Term s (PBuiltinList (PBuiltinPair (PAsData PScriptPurpose) (PAsData PRedeemer))) ->
  Term s PTxOutRef ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PBool
pinitializePayout
  payoutPolicy
  payoutAddr
  burnAssetName
  ownValueRaw
  outPlusWithdrawalNft
  outAddress
  outValue
  outDatum
  withdrawalId
  withdrawalInfoData
  withdrawalsRoot
  membershipProof
  redeemers
  ownOutRef
  inputIndex
  hubRefInputIndex
  payoutMintRedeemerIndex = P.do
    PWithdrawalInfo {pwithdrawalInfo'body, pwithdrawalInfo'validity} <-
      pmatch (pfromData (punsafeCoerce @(PAsData PWithdrawalInfo) withdrawalInfoData))
    PWithdrawalBody
      { pwithdrawalBody'l2Value
      , pwithdrawalBody'l1Address
      , pwithdrawalBody'l1Datum
      } <-
      pmatch (pfromData pwithdrawalInfo'body)
    PPayoutDatum {ppayoutDatum'l2Value, ppayoutDatum'l1Address, ppayoutDatum'l1Datum} <-
      pmatch $
        pmatch outDatum $ \case
          POutputDatum {poutputDatum'outputDatum} ->
            pfromData (punsafeCoerce @(PAsData PPayoutDatum) (pto poutputDatum'outputDatum))
          _ -> perror
    payoutMint <-
      plet $
        pfromData
          ( punsafeCoerce @(PAsData Payout.PMintRedeemer)
              ( pto
                  ( pfromData
                      ( pgetRedeemerAt
                          # redeemers
                          # pdata (pcon (PMinting payoutPolicy))
                          # payoutMintRedeemerIndex
                      )
                  )
              )
          )
    pand'List
      [         {- Aiken: @output == own - withdrawalNFT + payoutNFT@. Stated here as
        @output + withdrawalNFT == own + payoutNFT@ so neither side needs an
        entry removed — see the note at the call site. -}
        outPlusWithdrawalNft
          #== ( Value.punionWith
                  # plam (+)
                  # ownValueRaw
                  # ( Value.psingletonSortedValue
                        # pfromData payoutPolicy
                        # pfromData burnAssetName
                        # 1
                    )
              )
      , pmatch payoutMint $ \case
          Payout.PMintPayout
            { Payout.pmintPayout'withdrawalUtxoOutRef
            , Payout.pmintPayout'withdrawalInputIndex
            , Payout.pmintPayout'hubRefInputIndex
            } ->
              pand'List
                [ pfromData pmintPayout'withdrawalUtxoOutRef #== ownOutRef
                , pfromData pmintPayout'withdrawalInputIndex #== inputIndex
                , pfromData pmintPayout'hubRefInputIndex #== hubRefInputIndex
                ]
          _ -> pconstant False
      ,         -- Only a withdrawal the operator judged valid may open an accumulator.
        pmatch (pfromData pwithdrawalInfo'validity) $ \case
          PWithdrawalIsValid -> pconstant True
          _ -> pconstant False
      , pvalidCountedMembership
          (pdata (pcon PWithdrawalsRootDomain))
          withdrawalsRoot
          membershipProof
          withdrawalId
          withdrawalInfoData
          redeemers
      ,         -- The opening accumulator must not already exceed the target, or the
        -- exact-value conclusion path becomes unreachable.
        pvalueIsNonNegative
          #$ pmergeValues
          # (pfromAssetList # pfromData pwithdrawalBody'l2Value)
          #$ pnegateValue
          # ( Value.punionWith
                # plam (+)
                # pto (pfromData outValue)
                # ( Value.psingletonSortedValue
                      # pfromData payoutPolicy
                      # pfromData burnAssetName
                      # (-1)
                  )
            )
      , ppayoutDatum'l2Value #== pwithdrawalBody'l2Value
      , ppayoutDatum'l1Address #== pwithdrawalBody'l1Address
      , ppayoutDatum'l1Datum #== pwithdrawalBody'l1Datum
      , outAddress #== pfromData payoutAddr
      ]

{- | The @Refund@ branch.

The claimed verdict must not be @WithdrawalIsValid@ — a valid withdrawal has to
go through payout — and the settlement's tree must record a withdrawal whose
info is this one with that very verdict substituted. That substitution is the
crux: it is what stops a user refunding on a verdict the operator never gave.
-}
prefundBranch ::
  forall (s :: S).
  Term s (PAsData PWithdrawalValidity) ->
  Term s Value.PSortedValue ->
  Term s Value.PSortedValue ->
  Term s PAddress ->
  Term s (PAsData Value.PLedgerValue) ->
  Term s POutputDatum ->
  Term s (PAsData PAddress) ->
  Term s POutputDatum ->
  Term s PData ->
  Term s PData ->
  Term s PByteString ->
  Term s PRootMembershipProof ->
  Term s (PBuiltinList (PBuiltinPair (PAsData PScriptPurpose) (PAsData PRedeemer))) ->
  Term s PBool
prefundBranch
  validityOverride
  ownValueRaw
  outPlusWithdrawalNft
  outAddress
  outValue
  outDatum
  refundAddress
  refundDatum
  withdrawalId
  withdrawalInfoData
  withdrawalsRoot
  membershipProof
  redeemers = P.do
    PWithdrawalInfo {pwithdrawalInfo'body, pwithdrawalInfo'signature} <-
      pmatch (pfromData (punsafeCoerce @(PAsData PWithdrawalInfo) withdrawalInfoData))
    overridden <-
      plet $
        pforgetData
          ( pdata
              ( pcon
                  ( PWithdrawalInfo
                      { pwithdrawalInfo'body = pwithdrawalInfo'body
                      , pwithdrawalInfo'signature = pwithdrawalInfo'signature
                      , pwithdrawalInfo'validity = validityOverride
                      }
                  )
              )
          )
    pand'List
      [ pmatch (pfromData validityOverride) $ \case
          PWithdrawalIsValid -> pconstant False
          _ -> pconstant True
      , outPlusWithdrawalNft #== ownValueRaw
      , outAddress #== pfromData refundAddress
      , outDatum #== refundDatum
      , pvalidCountedMembership
          (pdata (pcon PWithdrawalsRootDomain))
          withdrawalsRoot
          membershipProof
          withdrawalId
          overridden
          redeemers
      ]
