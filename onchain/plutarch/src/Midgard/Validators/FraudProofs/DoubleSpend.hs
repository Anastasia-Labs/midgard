{- |
Module      : Midgard.Validators.FraudProofs.DoubleSpend
Description : Plutarch port of @validators/fraud-proofs/double-spend/step-0{1,2,3,4}.ak@.

The double-spend fraud proof (spec §5.1.1): two distinct transactions of one
committed block spending the same output.

Four spending validators, one per L1 transaction of the proof:

1. bind the first conflicting transaction to the block and forward its id;
2. bind the second, /require the two ids to differ/, and forward both;
3. open tx1's field-0 preimage through the §8.8 door and forward the disputed
   output reference;
4. open tx2's field-0 preimage, check the same output reference is in it, and
   finalise.

=== Where the soundness lives

Two guards, and neither is where a reader first looks for it.

__The distinctness check is step-02's.__ Two /identical/ transactions are one
transaction — a block committing the same canonical bytes twice commits one leaf
— so without @tx1_id != tx2_id@ a prover could bind the same transaction twice
and "prove" it double-spends against itself. The check is on canonical
transaction ids, which is the only place the two are comparable: the openings
happen two steps later, against different anchors.

__The slot is positional, never named.__ Steps 03 and 04 pass
'pspendInputsFieldIndex' to the door, and the door derives the commitment from
the compact structures the /verified id/ authenticates. Nothing here ever holds a
free-standing field hash, because under §4's plain hashing a field-0 preimage and
a field-1 preimage over the same items hash identically — a proof that carried
the commitment would be a proof that could not tell a spend input from a
reference input.

=== The cost defect this shape closed

Step-04 is where issue #551 (finding Q1X-F6) was measured. The retired idiom
reproduced tx2's whole spend-input collection in order to re-hash it, which put
the proof past the ledger's memory cap at the admissible 296-input cardinality.
The door hashes the preimage once and §5.3's fixed 38-byte item makes
'pspendInputAt' one multiplication and one slice (§10.5), so the cost no longer
scales with how many inputs tx2 spends.
-}
module Midgard.Validators.FraudProofs.DoubleSpend (
  doubleSpendStep01Validator,
  doubleSpendStep02Validator,
  doubleSpendStep03Validator,
  doubleSpendStep04Validator,
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
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.FraudProofs.Common (
  pcontinue,
  pfinalize,
  ppassNativeTxToNextStepCarried,
 )
import Midgard.FraudProofs.DoubleSpend (
  PStep02State (..),
  PStep03Args (..),
  PStep03State (..),
  PStep04Args (..),
  PStep04State (..),
 )
import Midgard.FraudProofs.FieldOpening (
  PNativeTxAnchorV1 (..),
  popenedFieldView,
  pspendInputsFieldIndex,
 )
import Midgard.NativeTxMachineWalk (pspendInputAt)
import Midgard.Validators.FraudProofs.Step (
  pdispatch,
  pexpectDatum,
  pexpectStateAs,
  pexpecting,
  pstateIsAbsent,
  pstep,
 )

--------------------------------------------------------------------------------
-- Step 01
--------------------------------------------------------------------------------

{- | Aiken @validators/fraud-proofs/double-spend/step-01.ak@.

Binds the first conflicting transaction to a committed block. Only a transaction
genuinely held by the header's counted @transactions_root@ may forward its id,
and the id is the whole of what step-02 needs.

The @expect None@ on the input state is what makes this the /first/ step: a
thread that already carries state is one this validator was not initialised for.
-}
doubleSpendStep01Validator ::
  forall (s :: S).
  Term
    s
    ( PAsData PScriptHash -- step-02's script hash
        :--> PAsData PCurrencySymbol -- computation thread token policy
        :--> PAsData PScriptHash -- hub oracle
        :--> PScriptContext
        :--> PUnit
    )
doubleSpendStep01Validator = plam $
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
               mInputStateData
               outputScriptHash
               outputStateData
               _header
               badTxId
               _badTxView ->
                -- 1. This is the first step, so there is no prior state.
                pexpecting (pstateIsAbsent mInputStateData) $
                  -- 2. The next step's UTxO carries the verified id, and nothing
                  --    else: step-03 re-opens field 0 through the door, which
                  --    extracts the commitment positionally from the compact
                  --    structures this id authenticates.
                  pexpecting (outputScriptHash #== step02ValidatorScriptHash) $
                    pexpecting
                      ( outputStateData
                          #== pforgetData
                            (pdata (pcon (PStep02State {pstep02State'verifiedTx1Id = pdata badTxId})))
                      )
                      (pconstant True)

--------------------------------------------------------------------------------
-- Step 02
--------------------------------------------------------------------------------

{- | Aiken @validators/fraud-proofs/double-spend/step-02.ak@.

Binds the second conflicting transaction and enforces the distinctness that makes
the family sound: the two canonical transaction ids must differ. Both ids then
travel together, because steps 03 and 04 each need one.
-}
doubleSpendStep02Validator ::
  forall (s :: S).
  Term
    s
    ( PAsData PScriptHash -- step-03's script hash
        :--> PAsData PCurrencySymbol
        :--> PAsData PScriptHash
        :--> PScriptContext
        :--> PUnit
    )
doubleSpendStep02Validator = plam $
  \step03ValidatorScriptHash computationThreadTokenPolicyId hubOracle ctx ->
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
               mInputStateData
               outputScriptHash
               outputStateData
               _header
               tx2Id
               _tx2View -> P.do
                PStep02State {pstep02State'verifiedTx1Id} <-
                  pmatch (pexpectStateAs @PStep02State mInputStateData)
                verifiedTx1Id <- plet pstep02State'verifiedTx1Id
                -- 2. The two transactions must be different. Identical bytes are
                --    one transaction and one leaf, so without this a prover
                --    could bind the same transaction twice.
                pexpecting (pnot #$ pfromData verifiedTx1Id #== tx2Id) $
                  -- 3. Both verified ids go forward.
                  pexpecting (outputScriptHash #== step03ValidatorScriptHash) $
                    pexpecting
                      ( outputStateData
                          #== pforgetData
                            ( pdata
                                ( pcon
                                    ( PStep03State
                                        { pstep03State'verifiedTx1Id = verifiedTx1Id
                                        , pstep03State'verifiedTx2Id = pdata tx2Id
                                        }
                                    )
                                )
                            )
                      )
                      (pconstant True)

--------------------------------------------------------------------------------
-- Step 03
--------------------------------------------------------------------------------

{- | Aiken @validators/fraud-proofs/double-spend/step-03.ak@.

Opens tx1's field-0 preimage through the §8.8 door and forwards the disputed
output reference.

The anchor is a @BodyAnchor@ over the id step-01 verified, so the opening is
pinned to thread state rather than to anything the redeemer says. Field 0 is a
body field, which is why a body anchor is the right — and the only admissible —
one.
-}
doubleSpendStep03Validator ::
  forall (s :: S).
  Term
    s
    ( PAsData PScriptHash -- step-04's script hash
        :--> PAsData PCurrencySymbol
        :--> PAsData PCurrencySymbol -- field preimage certificate policy
        :--> PScriptContext
        :--> PUnit
    )
doubleSpendStep03Validator = plam $
  \step04ValidatorScriptHash
   computationThreadTokenPolicyId
   fieldPreimageCertificatePolicyId
   ctx ->
      pstep ctx $ \datum redeemer ownOutRef txInfo ->
        pdispatch @_ @PStep03Args computationThreadTokenPolicyId datum redeemer ownOutRef txInfo $
          \args -> P.do
            PStep03Args
              { pstep03Args'inputIndex
              , pstep03Args'outputIndex
              , pstep03Args'tx1SpendInputsOpening
              , pstep03Args'doubleSpentInputIndex
              } <-
              pmatch args
            PTxInfo {ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs} <- pmatch txInfo
            referenceInputs <- plet $ pfromData ptxInfo'referenceInputs
            -- 1. The thread's own input must be authentic and reproduced.
            pcontinue
              computationThreadTokenPolicyId
              (pexpectDatum datum)
              (pfromData pstep03Args'inputIndex)
              (pfromData pstep03Args'outputIndex)
              ownOutRef
              (pfromData ptxInfo'inputs)
              (pfromData ptxInfo'outputs)
              $ \_ownScriptHash
                 _threadTokenAssetName
                 _fraudProver
                 mInputStateData
                 outputScriptHash
                 outputStateData -> P.do
                  PStep03State {pstep03State'verifiedTx1Id, pstep03State'verifiedTx2Id} <-
                    pmatch (pexpectStateAs @PStep03State mInputStateData)
                  -- 2. tx1's field-0 preimage, authenticated against the
                  --    commitment the door extracts positionally from the
                  --    compact structures `verified_tx1_id` binds.
                  tx1SpendInputsView <-
                    plet $
                      popenedFieldView
                        # pfromData pstep03Args'tx1SpendInputsOpening
                        # pcon (PBodyAnchor {pbodyAnchor'txId = pstep03State'verifiedTx1Id})
                        # pspendInputsFieldIndex
                        # referenceInputs
                        # fieldPreimageCertificatePolicyId
                  -- 3. The index must name an input in that collection.
                  tx1DoubleSpentInput <-
                    plet $
                      pspendInputAt
                        # tx1SpendInputsView
                        # pfromData pstep03Args'doubleSpentInputIndex
                  -- 4. It travels on, with the still-unused second id.
                  pexpecting (outputScriptHash #== step04ValidatorScriptHash) $
                    pexpecting
                      ( outputStateData
                          #== pforgetData
                            ( pdata
                                ( pcon
                                    ( PStep04State
                                        { pstep04State'verifiedTx2Id = pstep03State'verifiedTx2Id
                                        , pstep04State'doubleSpentInput = pdata tx1DoubleSpentInput
                                        }
                                    )
                                )
                            )
                      )
                      (pconstant True)

--------------------------------------------------------------------------------
-- Step 04
--------------------------------------------------------------------------------

{- | Aiken @validators/fraud-proofs/double-spend/step-04.ak@.

Closes the family: the second transaction's authenticated field-0 preimage must
contain the very output reference carried from step-03.

This is where a challenge against a /valid/ block dies, because distinct
transactions of a valid block spend disjoint inputs. The comparison is on decoded
values, both sides having come through the same decoder, so a non-canonical
spelling of the same reference cannot pass on one side and fail on the other.
-}
doubleSpendStep04Validator ::
  forall (s :: S).
  Term
    s
    ( PAsData PCurrencySymbol -- computation thread token policy
        :--> PAsData PCurrencySymbol -- fraud proof token policy
        :--> PAsData PAddress -- fraud proof token address
        :--> PAsData PCurrencySymbol -- field preimage certificate policy
        :--> PScriptContext
        :--> PUnit
    )
doubleSpendStep04Validator = plam $
  \computationThreadTokenPolicyId
   fraudProofTokenPolicyId
   fraudProofTokenAddress
   fieldPreimageCertificatePolicyId
   ctx ->
      pstep ctx $ \datum redeemer ownOutRef txInfo ->
        pdispatch @_ @PStep04Args computationThreadTokenPolicyId datum redeemer ownOutRef txInfo $
          \args -> P.do
            PStep04Args
              { pstep04Args'inputIndex
              , pstep04Args'outputIndex
              , pstep04Args'fraudProofMintRedeemerIndex
              , pstep04Args'tx2SpendInputsOpening
              , pstep04Args'doubleSpentInputIndex
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
              (pfromData pstep04Args'inputIndex)
              (pfromData pstep04Args'outputIndex)
              (pfromData pstep04Args'fraudProofMintRedeemerIndex)
              ownOutRef
              (pfromData ptxInfo'inputs)
              (pfromData ptxInfo'outputs)
              (pto (pto (pfromData ptxInfo'redeemers)))
              $ \_ownScriptHash _threadTokenAssetName _fraudProver mInputStateData -> P.do
                PStep04State {pstep04State'verifiedTx2Id, pstep04State'doubleSpentInput} <-
                  pmatch (pexpectStateAs @PStep04State mInputStateData)
                -- 2. tx2's field-0 preimage, through the same door.
                tx2SpendInputsView <-
                  plet $
                    popenedFieldView
                      # pfromData pstep04Args'tx2SpendInputsOpening
                      # pcon (PBodyAnchor {pbodyAnchor'txId = pstep04State'verifiedTx2Id})
                      # pspendInputsFieldIndex
                      # referenceInputs
                      # fieldPreimageCertificatePolicyId
                -- 3. The named input must be the one step-03 carried over.
                pexpecting
                  ( pstep04State'doubleSpentInput
                      #== pdata
                        ( pspendInputAt
                            # tx2SpendInputsView
                            # pfromData pstep04Args'doubleSpentInputIndex
                        )
                  )
                  (pconstant True)
