{- |
Module      : Midgard.Validators.FraudProofs.InputNoIdx
Description : Plutarch port of @validators/fraud-proofs/input-no-idx/step-0{1,2,3,4}.ak@.

The input-index fraud proof (spec §5.1.1): a committed transaction spending
output @n@ of a transaction that has fewer than @n+1@ outputs.

Four validators:

1. bind the disputed transaction and forward its id;
2. read the challenged input out of its field 0;
3. bind the /producing/ transaction the input names, and check it is the one the
   input names;
4. open the producing transaction's field 2 and check the index is at or beyond
   its output count.

=== The two bindings are separate on purpose

Steps 01 and 03 both run the full inclusion check, against the /same/ thread and
therefore the same block: the computation thread token's asset name is what ties
them together, so step-03 cannot bind a transaction from a different block. Two
bindings are needed because the proof is about a relationship between two
transactions, and neither can be reached from the other's bytes.

Step-03's own guard — @producing_tx_id == bad_input_tx_id@ — is where a challenge
against a valid block dies. In a valid block every spend input names its true
producing transaction, so a prover who binds some /other/ committed transaction
to the forwarded id is stopped there, and an input that really exists can never
be walked towards an out-of-range verdict.

=== The verdict rests on a count, so the count must be authenticated

Step-04's rule consumes a number, not an item, which is why it reads it from the
door rather than from a list it reproduced. 'pfieldItemCount' only answers where
the count is authenticated: tiers 1–2 walk the whole preimage at view
construction, and it refuses outright for a variable-width field under tier 3
(§5.2/§8.6). Field 2 /is/ variable-width, so a tier-3 carriage of it is refused
here rather than worked around — a non-existence verdict resting on an
unauthenticated count is exactly the fabricated evidence §7.4 exists to prevent.
-}
module Midgard.Validators.FraudProofs.InputNoIdx (
  inputNoIdxStep01Validator,
  inputNoIdxStep02Validator,
  inputNoIdxStep03Validator,
  inputNoIdxStep04Validator,
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

import Midgard.FraudProofs.Common (pcontinue, pfinalize, ppassNativeTxToNextStep)
import Midgard.FraudProofs.FieldOpening (
  PNativeTxAnchorV1 (..),
  popenedFieldView,
  poutputsFieldIndex,
  pspendInputsFieldIndex,
 )
import Midgard.FraudProofs.InputNoIdx (
  PStep02Args (..),
  PStep02State (..),
  PStep03State (..),
  PStep04Args (..),
  PStep04State (..),
 )
import Midgard.FraudProofs.NativeTx.Types (PMidgardTxInput (..))
import Midgard.NativeTxFieldAccess (pfieldItemCount)
import Midgard.NativeTxMachineWalk (pspendInputAt)
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

-- | Aiken @validators/fraud-proofs/input-no-idx/step-01.ak@.
inputNoIdxStep01Validator ::
  forall (s :: S).
  Term
    s
    ( PAsData PScriptHash -- step-02's script hash
        :--> PAsData PCurrencySymbol -- computation thread token policy
        :--> PAsData PScriptHash -- hub oracle
        :--> PScriptContext
        :--> PUnit
    )
inputNoIdxStep01Validator = plam $
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
               _header
               badTxId
               _badTxView ->
                pexpecting (outputScriptHash #== step02ValidatorScriptHash) $
                  pexpecting
                    ( outputStateData
                        #== pforgetData
                          (pdata (pcon (PStep02State {pstep02State'verifiedTxId = pdata badTxId})))
                    )
                    (pconstant True)

--------------------------------------------------------------------------------
-- Step 02
--------------------------------------------------------------------------------

{- | Aiken @validators/fraud-proofs/input-no-idx/step-02.ak@.

Reads the challenged input out of field 0 and splits it into the two halves the
remaining steps use separately.
-}
inputNoIdxStep02Validator ::
  forall (s :: S).
  Term
    s
    ( PAsData PScriptHash -- step-03's script hash
        :--> PAsData PCurrencySymbol -- computation thread token policy
        :--> PAsData PCurrencySymbol -- field preimage certificate policy
        :--> PScriptContext
        :--> PUnit
    )
inputNoIdxStep02Validator = plam $
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
              , pstep02Args'spendInputsOpening
              , pstep02Args'badInputsIndex
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
                  PStep02State {pstep02State'verifiedTxId} <-
                    pmatch (pexpectStateAs @PStep02State mInputStateData)
                  spendInputsView <-
                    plet $
                      popenedFieldView
                        # pfromData pstep02Args'spendInputsOpening
                        # pcon (PBodyAnchor {pbodyAnchor'txId = pstep02State'verifiedTxId})
                        # pspendInputsFieldIndex
                        # referenceInputs
                        # fieldPreimageCertificatePolicyId
                  PMidgardTxInput {ptxInput'txId, ptxInput'outputIndex} <-
                    pmatch (pspendInputAt # spendInputsView # pfromData pstep02Args'badInputsIndex)
                  pexpecting (outputScriptHash #== step03ValidatorScriptHash) $
                    pexpecting
                      ( outputStateData
                          #== pforgetData
                            ( pdata
                                ( pcon
                                    ( PStep03State
                                        { pstep03State'badInputTxId = ptxInput'txId
                                        , pstep03State'badInputOutputIndex = ptxInput'outputIndex
                                        }
                                    )
                                )
                            )
                      )
                      (pconstant True)

--------------------------------------------------------------------------------
-- Step 03
--------------------------------------------------------------------------------

{- | Aiken @validators/fraud-proofs/input-no-idx/step-03.ak@.

Binds the producing transaction the disputed input names, and requires it to be
/that/ transaction.

The inclusion check is the same one step-01 ran, against the same thread and so
the same block: the computation thread token's asset name is what makes "in the
same block" hold without this step re-deriving it.
-}
inputNoIdxStep03Validator ::
  forall (s :: S).
  Term
    s
    ( PAsData PScriptHash -- step-04's script hash
        :--> PAsData PCurrencySymbol -- computation thread token policy
        :--> PAsData PScriptHash -- hub oracle
        :--> PScriptContext
        :--> PUnit
    )
inputNoIdxStep03Validator = plam $
  \step04ValidatorScriptHash computationThreadTokenPolicyId hubOracle ctx ->
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
               mInputStateData
               outputScriptHash
               outputStateData
               _header
               producingTxId
               _producingTxView -> P.do
                PStep03State {pstep03State'badInputTxId, pstep03State'badInputOutputIndex} <-
                  pmatch (pexpectStateAs @PStep03State mInputStateData)
                -- 2. The transaction just bound must be the one the disputed
                --    input names. This is where a challenge against a valid
                --    block dies.
                pexpecting (producingTxId #== pfromData pstep03State'badInputTxId) $
                  pexpecting (outputScriptHash #== step04ValidatorScriptHash) $
                    pexpecting
                      ( outputStateData
                          #== pforgetData
                            ( pdata
                                ( pcon
                                    ( PStep04State
                                        { pstep04State'producingTxId = pdata producingTxId
                                        , pstep04State'badInputOutputIndex =
                                            pstep03State'badInputOutputIndex
                                        }
                                    )
                                )
                            )
                      )
                      (pconstant True)

--------------------------------------------------------------------------------
-- Step 04
--------------------------------------------------------------------------------

{- | Aiken @validators/fraud-proofs/input-no-idx/step-04.ak@.

The conviction: the challenged output index is at or beyond the producing
transaction's authenticated output count.
-}
inputNoIdxStep04Validator ::
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
inputNoIdxStep04Validator = plam $
  \fraudProofTokenPolicyId
   fraudProofTokenAddress
   computationThreadTokenPolicyId
   fieldPreimageCertificatePolicyId
   ctx ->
      pstep ctx $ \datum redeemer ownOutRef txInfo ->
        pdispatch @_ @PStep04Args computationThreadTokenPolicyId datum redeemer ownOutRef txInfo $
          \args -> P.do
            PStep04Args
              { pstep04Args'inputIndex
              , pstep04Args'outputIndex
              , pstep04Args'fraudProofMintRedeemerIndex
              , pstep04Args'outputsOpening
              } <-
              pmatch args
            PTxInfo {ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs, ptxInfo'redeemers} <-
              pmatch txInfo
            referenceInputs <- plet $ pfromData ptxInfo'referenceInputs
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
                PStep04State {pstep04State'producingTxId, pstep04State'badInputOutputIndex} <-
                  pmatch (pexpectStateAs @PStep04State mInputStateData)
                outputsView <-
                  plet $
                    popenedFieldView
                      # pfromData pstep04Args'outputsOpening
                      # pcon (PBodyAnchor {pbodyAnchor'txId = pstep04State'producingTxId})
                      # poutputsFieldIndex
                      # referenceInputs
                      # fieldPreimageCertificatePolicyId
                pexpecting
                  ( pfromData pstep04State'badInputOutputIndex
                      #>= pfieldItemCount # outputsView
                  )
                  (pconstant True)
