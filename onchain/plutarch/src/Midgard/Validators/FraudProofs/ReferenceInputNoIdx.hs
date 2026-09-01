{- |
Module      : Midgard.Validators.FraudProofs.ReferenceInputNoIdx
Description : Plutarch port of @validators/fraud-proofs/reference-input-no-idx/step-0{1,2,3,4}.ak@.

The reference-input-index fraud proof (spec §5.1.1): a committed transaction
/referencing/ output @n@ of a transaction that has fewer than @n+1@ outputs.

'Midgard.Validators.FraudProofs.InputNoIdx' one §2.5 slot over: step-02 opens
field 1 rather than field 0, and everything else is the same four-step shape.

=== The slot is the whole difference, and it is positional

§4 removed field-index domain separation, so a field-0 preimage and a field-1
preimage over the same items commit /identically/. Nothing in this family's types
or redeemers names which slot it is about; the index is a compiled-in literal
passed to the door, and the door derives the commitment from the position. A port
that passed the wrong constant here would prove the other family's fault and no
test of the /types/ would notice, which is why the tests drive the door.

=== The two bindings are separate on purpose

Steps 01 and 03 both run the full inclusion check, against the same thread and
therefore the same block: the computation thread token's asset name is what ties
them together. Two bindings are needed because the proof is about a relationship
between two transactions, and neither can be reached from the other's bytes.

Step-03's guard — @producing_tx_id == bad_reference_input_tx_id@ — is where a
challenge against a valid block dies. In a valid block every reference input
names its true producing transaction.

=== The verdict rests on a count, so the count must be authenticated

Step-04's rule consumes a number, not an item. 'pfieldItemCount' only answers
where the count is authenticated: tiers 1–2 walk the whole preimage at view
construction, and it refuses outright for a variable-width field under tier 3
(§5.2/§8.6). Field 2 /is/ variable-width, so a tier-3 carriage of it is refused
here rather than worked around.
-}
module Midgard.Validators.FraudProofs.ReferenceInputNoIdx (
  referenceInputNoIdxStep01Validator,
  referenceInputNoIdxStep02Validator,
  referenceInputNoIdxStep03Validator,
  referenceInputNoIdxStep04Validator,
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
  preferenceInputsFieldIndex,
 )
import Midgard.FraudProofs.ReferenceInputNoIdx (
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

-- | Aiken @validators/fraud-proofs/reference-input-no-idx/step-01.ak@.
referenceInputNoIdxStep01Validator ::
  forall (s :: S).
  Term
    s
    ( PAsData PScriptHash -- step-02's script hash
        :--> PAsData PCurrencySymbol -- computation thread token policy
        :--> PAsData PScriptHash -- hub oracle
        :--> PScriptContext
        :--> PUnit
    )
referenceInputNoIdxStep01Validator = plam $
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

{- | Aiken @validators/fraud-proofs/reference-input-no-idx/step-02.ak@.

Reads the challenged reference input out of field 1 and splits it into the two
halves the remaining steps use separately.
-}
referenceInputNoIdxStep02Validator ::
  forall (s :: S).
  Term
    s
    ( PAsData PScriptHash -- step-03's script hash
        :--> PAsData PCurrencySymbol -- computation thread token policy
        :--> PAsData PCurrencySymbol -- field preimage certificate policy
        :--> PScriptContext
        :--> PUnit
    )
referenceInputNoIdxStep02Validator = plam $
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
                  PStep02State {pstep02State'verifiedTxId} <-
                    pmatch (pexpectStateAs @PStep02State mInputStateData)
                  referenceInputsView <-
                    plet $
                      popenedFieldView
                        # pfromData pstep02Args'referenceInputsOpening
                        # pcon (PBodyAnchor {pbodyAnchor'txId = pstep02State'verifiedTxId})
                        # preferenceInputsFieldIndex
                        # referenceInputs
                        # fieldPreimageCertificatePolicyId
                  PMidgardTxInput {ptxInput'txId, ptxInput'outputIndex} <-
                    pmatch (pspendInputAt # referenceInputsView # pfromData pstep02Args'badReferenceInputIndex)
                  pexpecting (outputScriptHash #== step03ValidatorScriptHash) $
                    pexpecting
                      ( outputStateData
                          #== pforgetData
                            ( pdata
                                ( pcon
                                    ( PStep03State
                                        { pstep03State'badReferenceInputTxId = ptxInput'txId
                                        , pstep03State'badReferenceInputOutputIndex = ptxInput'outputIndex
                                        }
                                    )
                                )
                            )
                      )
                      (pconstant True)

--------------------------------------------------------------------------------
-- Step 03
--------------------------------------------------------------------------------

{- | Aiken @validators/fraud-proofs/reference-input-no-idx/step-03.ak@.

Binds the producing transaction the disputed reference input names, and requires
it to be /that/ transaction.

The inclusion check is the same one step-01 ran, against the same thread and so
the same block: the computation thread token's asset name is what makes "in the
same block" hold without this step re-deriving it.
-}
referenceInputNoIdxStep03Validator ::
  forall (s :: S).
  Term
    s
    ( PAsData PScriptHash -- step-04's script hash
        :--> PAsData PCurrencySymbol -- computation thread token policy
        :--> PAsData PScriptHash -- hub oracle
        :--> PScriptContext
        :--> PUnit
    )
referenceInputNoIdxStep03Validator = plam $
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
                PStep03State {pstep03State'badReferenceInputTxId, pstep03State'badReferenceInputOutputIndex} <-
                  pmatch (pexpectStateAs @PStep03State mInputStateData)
                -- 2. The transaction just bound must be the one the disputed
                --    reference input names. This is where a challenge against a
                --    valid block dies.
                pexpecting (producingTxId #== pfromData pstep03State'badReferenceInputTxId) $
                  pexpecting (outputScriptHash #== step04ValidatorScriptHash) $
                    pexpecting
                      ( outputStateData
                          #== pforgetData
                            ( pdata
                                ( pcon
                                    ( PStep04State
                                        { pstep04State'producingTxId = pdata producingTxId
                                        , pstep04State'badReferenceInputOutputIndex =
                                            pstep03State'badReferenceInputOutputIndex
                                        }
                                    )
                                )
                            )
                      )
                      (pconstant True)

--------------------------------------------------------------------------------
-- Step 04
--------------------------------------------------------------------------------

{- | Aiken @validators/fraud-proofs/reference-input-no-idx/step-04.ak@.

The conviction: the challenged output index is at or beyond the producing
transaction's authenticated output count.
-}
referenceInputNoIdxStep04Validator ::
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
referenceInputNoIdxStep04Validator = plam $
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
                PStep04State {pstep04State'producingTxId, pstep04State'badReferenceInputOutputIndex} <-
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
                  ( pfromData pstep04State'badReferenceInputOutputIndex
                      #>= pfieldItemCount # outputsView
                  )
                  (pconstant True)
