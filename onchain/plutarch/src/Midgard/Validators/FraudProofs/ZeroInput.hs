{- |
Module      : Midgard.Validators.FraudProofs.ZeroInput
Description : Plutarch port of @validators/fraud-proofs/zero-input/step-0{1,2}.ak@.

The zero-input fraud proof (spec §5.1.1): a committed transaction that spends
nothing.

Two validators. Step-01 binds the transaction to the block and forwards its id;
step-02 opens field 0 through the §8.8 door, reads its item count, and finalises
when the count is zero.

=== The check is a count, deliberately, and not a hash

Comparing a forwarded spend-inputs commitment against the pinned commitment of
the empty field would be shorter and would be wrong. §4 removed field-index
domain separation, so the empty field has one commitment shared by all nine
slots: such an equality proves "some field of this transaction is empty" where
the rule needs "field 0 is". Reading the count through the door is what ties the
verdict to /this/ slot, because the door derives the commitment positionally
from the compact structures the verified id authenticates.

The count itself is authenticated rather than asserted. For carriage tiers 1–2
the door walks the whole preimage at view construction, so §5.2/§7.4's
arithmetic has already been checked against the bytes by the time
'pfieldItemCount' answers.
-}
module Midgard.Validators.FraudProofs.ZeroInput (
  zeroInputStep01Validator,
  zeroInputStep02Validator,
) where

import Plutarch.LedgerApi.V3 (
  PAddress,
  PCurrencySymbol,
  PScriptContext,
  PScriptHash,
  PTxInfo (..),
 )
import Plutarch.Monadic qualified as P
import Plutarch.Prelude

import Midgard.FraudProofs.Common (pfinalize, ppassNativeTxToNextStepCarried)
import Midgard.FraudProofs.FieldOpening (
  PNativeTxAnchorV1 (..),
  popenedFieldView,
  pspendInputsFieldIndex,
 )
import Midgard.FraudProofs.ZeroInput (PStep02Args (..), PStep02State (..))
import Midgard.NativeTxFieldAccess (pfieldItemCount)
import Midgard.Validators.FraudProofs.Step (
  pdispatch,
  pexpectDatum,
  pexpectStateAs,
  pexpecting,
  pstep,
 )

{- | Aiken @validators/fraud-proofs/zero-input/step-01.ak@.

Binds the disputed transaction to a committed block and forwards its id. No block
roots travel: step-02 concludes from the transaction alone.

Unlike the double-spend family's first step, this one does __not__ require the
prior state to be absent. That is the Aiken original's shape and it is not an
oversight: the thread's own token asset name pins which block and which category
the thread belongs to, and a thread carrying stale state still has to produce the
exact output state below.
-}
zeroInputStep01Validator ::
  forall (s :: S).
  Term
    s
    ( PAsData PScriptHash -- step-02's script hash
        :--> PAsData PCurrencySymbol -- computation thread token policy
        :--> PAsData PScriptHash -- hub oracle
        :--> PScriptContext
        :--> PUnit
    )
zeroInputStep01Validator = plam $
  \step02ValidatorScriptHash computationThreadTokenPolicyId hubOracle ctx ->
    pstep ctx $ \datum redeemer ownOutRef txInfo ->
      pdispatch computationThreadTokenPolicyId datum redeemer ownOutRef txInfo $
        \carriage -> P.do
          PTxInfo {ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs, ptxInfo'redeemers} <-
            pmatch txInfo
          ppassNativeTxToNextStepCarried
            computationThreadTokenPolicyId
            hubOracle
            datum
            carriage
            ownOutRef
            (pfromData ptxInfo'inputs)
            (pfromData ptxInfo'referenceInputs)
            (pfromData ptxInfo'outputs)
            (pto (pto (pfromData ptxInfo'redeemers)))
            $ \_ownScriptHash
               _threadTokenAssetName
               _fraudProver
               _mInputStateData
               outputScriptHash
               outputStateData
               _header
               badTxId
               _badTxView ->
                pexpecting (outputScriptHash #== step02ValidatorScriptHash) $
                  pexpecting
                    ( outputStateData
                        #== pforgetData
                          (pdata (pcon (PStep02State {pstep02State'badTxId = pdata badTxId})))
                    )
                    (pconstant True)

{- | Aiken @validators/fraud-proofs/zero-input/step-02.ak@.

Concludes the proof: field 0 of the disputed transaction must hold no items.
-}
zeroInputStep02Validator ::
  forall (s :: S).
  Term
    s
    ( PAsData PCurrencySymbol -- fraud proof token policy
        :--> PAsData PAddress -- fraud proof token address
        :--> PAsData PCurrencySymbol -- computation thread token policy
        :--> PAsData PCurrencySymbol -- field preimage certificate policy
        :--> PScriptContext
        :--> PUnit
    )
zeroInputStep02Validator = plam $
  \fraudProofTokenPolicyId
   fraudProofTokenAddress
   computationThreadTokenPolicyId
   fieldPreimageCertificatePolicyId
   ctx ->
      pstep ctx $ \datum redeemer ownOutRef txInfo ->
        pdispatch @_ @PStep02Args computationThreadTokenPolicyId datum redeemer ownOutRef txInfo $
          \args -> P.do
            PStep02Args
              { pstep02Args'inputIndex
              , pstep02Args'outputIndex
              , pstep02Args'fraudProofMintRedeemerIndex
              , pstep02Args'spendInputsOpening
              } <-
              pmatch args
            PTxInfo {ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs, ptxInfo'redeemers} <-
              pmatch txInfo
            referenceInputs <- plet $ pfromData ptxInfo'referenceInputs
            -- 1. The thread's own input must be authentic and reproduced at the
            --    fraud proof's spending address.
            pfinalize
              computationThreadTokenPolicyId
              fraudProofTokenPolicyId
              fraudProofTokenAddress
              (pexpectDatum datum)
              (pfromData pstep02Args'inputIndex)
              (pfromData pstep02Args'outputIndex)
              (pfromData pstep02Args'fraudProofMintRedeemerIndex)
              ownOutRef
              (pfromData ptxInfo'inputs)
              (pfromData ptxInfo'outputs)
              (pto (pto (pfromData ptxInfo'redeemers)))
              $ \_ownScriptHash _threadTokenAssetName _fraudProver mInputStateData -> P.do
                PStep02State {pstep02State'badTxId} <-
                  pmatch (pexpectStateAs @PStep02State mInputStateData)
                -- 2. Field 0 must hold no items.
                spendInputsView <-
                  plet $
                    popenedFieldView
                      # pfromData pstep02Args'spendInputsOpening
                      # pcon (PBodyAnchor {pbodyAnchor'txId = pstep02State'badTxId})
                      # pspendInputsFieldIndex
                      # referenceInputs
                      # fieldPreimageCertificatePolicyId
                pexpecting (pfieldItemCount # spendInputsView #== 0) (pconstant True)
