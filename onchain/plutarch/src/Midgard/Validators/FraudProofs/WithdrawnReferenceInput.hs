{- |
Module      : Midgard.Validators.FraudProofs.WithdrawnReferenceInput
Description : Plutarch port of @validators/fraud-proofs/withdrawn-reference-input/step-0{1,2,3}.ak@.

The withdrawn-reference-input fraud proof (spec §5.1.16): a committed transaction
referencing an output that a withdrawal event had already taken off L2.

Three validators. Step-01 binds the transaction and picks up the header's counted
withdrawals commitment; step-02 opens field 1 and names the reference input;
step-03 exhibits the withdrawal that spent it.

=== The absence is a presence

Every other absence family proves a /non/-membership: the output is not in the
initial ledger, not produced by the block. This one proves the opposite — that a
withdrawal event /is/ in the block's withdrawals tree and names exactly that
output reference. That makes it the shortest family with a real conclusion:
there is no second tree to rule out, because a withdrawal is by itself
disqualifying.

=== Both halves of the counted commitment travel

The withdrawals root and its count are read off the header in step-01. Step-03
could not re-read them soundly — it holds no inclusion argument, so it has no way
to say which block's header it is looking at — and it needs both, because a
Midgard root is a commitment to @(domain, phas_root, count)@ and cannot be
unwrapped without the count. A count a redeemer chose would let a prover present
a tree of the wrong size, which is exactly what the counted scheme exists to stop.

=== Field 1, read with field 0's reader

'Midgard.NativeTxMachineWalk.pspendInputAt' serves both collections: §5.3 gives
spend inputs and reference inputs the same 38-byte item and the same stride, so
the reader is shared and the /slot/ is the index passed to the door. §4 removed
field-index domain separation, so nothing in the preimage itself would catch a
step that passed the wrong one — which is why the index is a literal here and the
reader's stride guard refuses any view whose items are not that shape.
-}
module Midgard.Validators.FraudProofs.WithdrawnReferenceInput (
  withdrawnReferenceInputStep01Validator,
  withdrawnReferenceInputStep02Validator,
  withdrawnReferenceInputStep03Validator,
) where

import Plutarch.LedgerApi.V3 (
  PAddress,
  PCurrencySymbol,
  PScriptContext,
  PScriptHash,
  PTxInfo (..),
  PTxOutRef (..),
 )
import Plutarch.Builtin.Data (pserialiseData)
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.FraudProofs.Common (pcontinue, pfinalize, ppassNativeTxToNextStep)
import Midgard.FraudProofs.FieldOpening (
  PNativeTxAnchorV1 (..),
  popenedFieldView,
  preferenceInputsFieldIndex,
 )
import Midgard.FraudProofs.NativeTx.Types (PMidgardTxInput (..))
import Midgard.FraudProofs.WithdrawnReferenceInput (
  PStep02Args (..),
  PStep02State (..),
  PStep03Args (..),
  PStep03State (..),
 )
import Midgard.LedgerState (
  PHeaderV1 (..),
  PWithdrawalBody (..),
  PWithdrawalInfo (..),
  PWithdrawalValidity (..),
 )
import Midgard.NativeTxMachineWalk (pspendInputAt)
import Midgard.TransitionTrace (
  PRootDomain (..),
  PRootMembershipProof (..),
  pverifyRootMembershipWithBytes,
 )
import Midgard.Validators.FraudProofs.Step (
  pdispatch,
  pexpectDatum,
  pexpectStateAs,
  pexpecting,
  pstep,
 )

--------------------------------------------------------------------------------
-- Step 01
--------------------------------------------------------------------------------

-- | Aiken @validators/fraud-proofs/withdrawn-reference-input/step-01.ak@.
withdrawnReferenceInputStep01Validator ::
  forall (s :: S).
  Term
    s
    ( PAsData PScriptHash -- step-02's script hash
        :--> PAsData PCurrencySymbol -- computation thread token policy
        :--> PAsData PScriptHash -- hub oracle
        :--> PScriptContext
        :--> PUnit
    )
withdrawnReferenceInputStep01Validator = plam $
  \step02ValidatorScriptHash computationThreadTokenPolicyId hubOracle ctx ->
    pstep ctx $ \datum redeemer ownOutRef txInfo ->
      pdispatch computationThreadTokenPolicyId datum redeemer ownOutRef txInfo $
        \args -> P.do
          PTxInfo {ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs, ptxInfo'redeemers} <-
            pmatch txInfo
          ppassNativeTxToNextStep
            computationThreadTokenPolicyId
            hubOracle
            datum
            args
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
               header
               badTxId
               _badTxView -> P.do
                PHeaderV1 {pheader'withdrawalsRoot, pheader'withdrawalCount} <-
                  pmatch (pfromData header)
                pexpecting (outputScriptHash #== step02ValidatorScriptHash) $
                  pexpecting
                    ( outputStateData
                        #== pforgetData
                          ( pdata
                              ( pcon
                                  ( PStep02State
                                      { pstep02State'badTxId = pdata badTxId
                                      , pstep02State'blocksWithdrawalsRoot = pheader'withdrawalsRoot
                                      , pstep02State'blocksWithdrawalCount = pheader'withdrawalCount
                                      }
                                  )
                              )
                          )
                    )
                    (pconstant True)

--------------------------------------------------------------------------------
-- Step 02
--------------------------------------------------------------------------------

-- | Aiken @validators/fraud-proofs/withdrawn-reference-input/step-02.ak@.
withdrawnReferenceInputStep02Validator ::
  forall (s :: S).
  Term
    s
    ( PAsData PScriptHash -- step-03's script hash
        :--> PAsData PCurrencySymbol -- computation thread token policy
        :--> PAsData PCurrencySymbol -- field preimage certificate policy
        :--> PScriptContext
        :--> PUnit
    )
withdrawnReferenceInputStep02Validator = plam $
  \step03ValidatorScriptHash
   computationThreadTokenPolicyId
   fieldPreimageCertificatePolicyId
   ctx ->
      pstep ctx $ \datum redeemer ownOutRef txInfo ->
        pdispatch @_ @PStep02Args computationThreadTokenPolicyId datum redeemer ownOutRef txInfo $
          \args -> P.do
            PStep02Args
              { pstep02Args'inputIndex
              , pstep02Args'outputIndex
              , pstep02Args'referenceInputsOpening
              , pstep02Args'badReferenceInputIndex
              } <-
              pmatch args
            PTxInfo {ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs} <- pmatch txInfo
            referenceInputs <- plet $ pfromData ptxInfo'referenceInputs
            pcontinue
              computationThreadTokenPolicyId
              (pexpectDatum datum)
              (pfromData pstep02Args'inputIndex)
              (pfromData pstep02Args'outputIndex)
              ownOutRef
              (pfromData ptxInfo'inputs)
              (pfromData ptxInfo'outputs)
              $ \_ownScriptHash
                 _threadTokenAssetName
                 _fraudProver
                 mInputStateData
                 outputScriptHash
                 outputStateData -> P.do
                  PStep02State
                    { pstep02State'badTxId
                    , pstep02State'blocksWithdrawalsRoot
                    , pstep02State'blocksWithdrawalCount
                    } <-
                    pmatch (pexpectStateAs @PStep02State mInputStateData)
                  referenceInputsView <-
                    plet $
                      popenedFieldView
                        # pfromData pstep02Args'referenceInputsOpening
                        # pcon (PBodyAnchor {pbodyAnchor'txId = pstep02State'badTxId})
                        # preferenceInputsFieldIndex
                        # referenceInputs
                        # fieldPreimageCertificatePolicyId
                  missingReferenceInput <-
                    plet $
                      pspendInputAt
                        # referenceInputsView
                        # pfromData pstep02Args'badReferenceInputIndex
                  pexpecting (outputScriptHash #== step03ValidatorScriptHash) $
                    pexpecting
                      ( outputStateData
                          #== pforgetData
                            ( pdata
                                ( pcon
                                    ( PStep03State
                                        { pstep03State'missingReferenceInput =
                                            pdata missingReferenceInput
                                        , pstep03State'blocksWithdrawalsRoot =
                                            pstep02State'blocksWithdrawalsRoot
                                        , pstep03State'blocksWithdrawalCount =
                                            pstep02State'blocksWithdrawalCount
                                        }
                                    )
                                )
                            )
                      )
                      (pconstant True)

--------------------------------------------------------------------------------
-- Step 03
--------------------------------------------------------------------------------

{- | Aiken @validators/fraud-proofs/withdrawn-reference-input/step-03.ak@.

The conviction: a /valid/ withdrawal event, committed under the block's
withdrawals root, naming the very output the disputed transaction referenced.

Three things have to line up and each is a separate refusal. The event's validity
must be @WithdrawalIsValid@ — an event the operator itself marked invalid never
took the output off L2, so referencing it is no fault. Its @l2_outref@ must be
the named reference input, both halves. And it must actually be in the tree the
header committed, which is a counted-root unwrap followed by an MPF membership
walk over the canonically serialised key and value.

The key and value bytes are produced here rather than taken from the witness's
own fields, because a witness that supplied its own encoding could present one
tree entry under two different keys.
-}
withdrawnReferenceInputStep03Validator ::
  forall (s :: S).
  Term
    s
    ( PAsData PCurrencySymbol -- fraud proof token policy
        :--> PAsData PAddress -- fraud proof token address
        :--> PAsData PCurrencySymbol -- computation thread token policy
        :--> PScriptContext
        :--> PUnit
    )
withdrawnReferenceInputStep03Validator = plam $
  \fraudProofTokenPolicyId
   fraudProofTokenAddress
   computationThreadTokenPolicyId
   ctx ->
      pstep ctx $ \datum redeemer ownOutRef txInfo ->
        pdispatch @_ @PStep03Args computationThreadTokenPolicyId datum redeemer ownOutRef txInfo $
          \args -> P.do
            PStep03Args
              { pstep03Args'inputIndex
              , pstep03Args'outputIndex
              , pstep03Args'withdrawalMembership
              , pstep03Args'fraudProofMintRedeemerIndex
              } <-
              pmatch args
            PTxInfo {ptxInfo'inputs, ptxInfo'outputs, ptxInfo'redeemers} <- pmatch txInfo
            witness <- plet $ pfromData pstep03Args'withdrawalMembership
            pfinalize
              computationThreadTokenPolicyId
              fraudProofTokenPolicyId
              fraudProofTokenAddress
              (pexpectDatum datum)
              (pfromData pstep03Args'inputIndex)
              (pfromData pstep03Args'outputIndex)
              (pfromData pstep03Args'fraudProofMintRedeemerIndex)
              ownOutRef
              (pfromData ptxInfo'inputs)
              (pfromData ptxInfo'outputs)
              (pto (pto (pfromData ptxInfo'redeemers)))
              $ \_ownScriptHash _threadTokenAssetName _fraudProver mInputStateData -> P.do
                PStep03State
                  { pstep03State'missingReferenceInput
                  , pstep03State'blocksWithdrawalsRoot
                  , pstep03State'blocksWithdrawalCount
                  } <-
                  pmatch (pexpectStateAs @PStep03State mInputStateData)
                PMidgardTxInput {ptxInput'txId, ptxInput'outputIndex} <-
                  pmatch (pfromData pstep03State'missingReferenceInput)
                PRootMembershipProof {prootMembership'key, prootMembership'value} <-
                  pmatch witness
                PWithdrawalInfo {pwithdrawalInfo'body, pwithdrawalInfo'validity} <-
                  pmatch (pfromData (punsafeCoerce @(PAsData PWithdrawalInfo) prootMembership'value))
                PWithdrawalBody {pwithdrawalBody'l2Outref} <- pmatch (pfromData pwithdrawalInfo'body)
                PTxOutRef {ptxOutRef'id, ptxOutRef'idx} <-
                  pmatch (pfromData (punsafeCoerce @(PAsData PTxOutRef) pwithdrawalBody'l2Outref))
                pexpecting
                  ( pmatch (pfromData pwithdrawalInfo'validity) $ \case
                      PWithdrawalIsValid -> pconstant True
                      _ -> pconstant False
                  )
                  $ pexpecting (pto (pfromData ptxOutRef'id) #== pfromData ptxInput'txId)
                  $ pexpecting (ptxOutRef'idx #== ptxInput'outputIndex)
                  $ pexpecting
                    ( pverifyRootMembershipWithBytes
                        witness
                        (pdata (pcon PWithdrawalsRootDomain))
                        (pfromData pstep03State'blocksWithdrawalsRoot)
                        (pfromData pstep03State'blocksWithdrawalCount)
                        (pserialiseData # prootMembership'key)
                        (pserialiseData # prootMembership'value)
                    )
                    (pconstant True)
