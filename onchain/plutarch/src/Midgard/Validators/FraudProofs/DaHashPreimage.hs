{- |
Module      : Midgard.Validators.FraudProofs.DaHashPreimage
Description : Plutarch port of @validators/fraud-proofs/da-hash-preimage/step-0{1,2}.ak@.

The DA hash/preimage fraud proof (GOAL_SPEC.md Q44): a committed
@transactions_root@ leaf whose key is not the canonical native-V1 transaction id
of its own value.

Two validators. Step-01 binds one raw @(key, value)@ leaf to the block header and
forwards the evidence triple; step-02 adjudicates it. The rule, and the argument
that neither step needs a decoder, live in 'Midgard.FraudProofs.DaHashPreimage'.

=== The one family that must not run the codec precondition

Every other native family opens its transaction through
@verify_native_tx_compact_cbor_v1@, which requires
@native_tx_id_for_version(version, body_cbor) == key@. Running it here would beg
the question: that equality is precisely what is in dispute, and a leaf violating
it would abort the step rather than be convicted by it. What step-01 binds is
therefore weaker and deliberately so — the leaf is a genuine member of the
block's counted @transactions_root@, and nothing more.

=== The conviction is arithmetic, and total

Step-02 has no bytes to look at: it adjudicates three values step-01 derived from
authenticated ones. That is what keeps the family sound against a leaf that is not
a transaction at all — arbitrary bytes committed under some key are convicted by
the same total computation as a well-formed transaction committed under a foreign
one, and an honest leaf can never be convicted because its slice /is/ the
encoder's body preimage.
-}
module Midgard.Validators.FraudProofs.DaHashPreimage (
  daHashPreimageStep01Validator,
  daHashPreimageStep02Validator,
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

import Midgard.FraudProofs.Common (pfinalize, ppassCommittedTransactionsLeafToNextStep)
import Midgard.FraudProofs.DaHashPreimage (
  PStep02Args (..),
  PStep02State (..),
  pderiveCommittedLeafTxIdV1,
  pisDaHashPreimageViolationV1,
 )
import Midgard.Validators.FraudProofs.Step (
  pdispatch,
  pexpectDatum,
  pexpectStateAs,
  pexpecting,
  pstep,
 )

{- | Aiken @validators/fraud-proofs/da-hash-preimage/step-01.ak@.

Binds one raw committed leaf and forwards the evidence triple: the committed key,
the id the leaf value itself commits to, and the leaf's byte length.
-}
daHashPreimageStep01Validator ::
  forall (s :: S).
  Term
    s
    ( PAsData PScriptHash -- step-02's script hash
        :--> PAsData PCurrencySymbol -- computation thread token policy
        :--> PAsData PScriptHash -- hub oracle
        :--> PScriptContext
        :--> PUnit
    )
daHashPreimageStep01Validator = plam $
  \step02ValidatorScriptHash computationThreadTokenPolicyId hubOracle ctx ->
    pstep ctx $ \datum redeemer ownOutRef txInfo ->
      pdispatch computationThreadTokenPolicyId datum redeemer ownOutRef txInfo $
        \args -> P.do
          PTxInfo {ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs, ptxInfo'redeemers} <-
            pmatch txInfo
          ppassCommittedTransactionsLeafToNextStep
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
               committedTxId
               committedLeafValue ->
                pexpecting (outputScriptHash #== step02ValidatorScriptHash) $
                  pexpecting
                    ( outputStateData
                        #== pforgetData
                          ( pdata
                              ( pcon
                                  ( PStep02State
                                      { pstep02State'committedTxId = pdata committedTxId
                                      , pstep02State'derivedTxId =
                                          pdata (pderiveCommittedLeafTxIdV1 # committedLeafValue)
                                      , pstep02State'committedLeafByteCount =
                                          pdata (plengthBS # committedLeafValue)
                                      }
                                  )
                              )
                          )
                    )
                    (pconstant True)

{- | Aiken @validators/fraud-proofs/da-hash-preimage/step-02.ak@.

The conviction: the committed leaf is either too short to carry the canonical
frame at all, or the id its own body preimage hashes to is not the key the block
committed it under.
-}
daHashPreimageStep02Validator ::
  forall (s :: S).
  Term
    s
    ( PAsData PCurrencySymbol -- fraud proof token policy
        :--> PAsData PAddress -- fraud proof token address
        :--> PAsData PCurrencySymbol -- computation thread token policy
        :--> PScriptContext
        :--> PUnit
    )
daHashPreimageStep02Validator = plam $
  \fraudProofTokenPolicyId fraudProofTokenAddress computationThreadTokenPolicyId ctx ->
    pstep ctx $ \datum redeemer ownOutRef txInfo ->
      pdispatch @_ @PStep02Args computationThreadTokenPolicyId datum redeemer ownOutRef txInfo $
        \args -> P.do
          PStep02Args
            { pstep02Args'inputIndex
            , pstep02Args'outputIndex
            , pstep02Args'fraudProofMintRedeemerIndex
            } <-
            pmatch args
          PTxInfo {ptxInfo'inputs, ptxInfo'outputs, ptxInfo'redeemers} <- pmatch txInfo
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
              PStep02State
                { pstep02State'committedTxId
                , pstep02State'derivedTxId
                , pstep02State'committedLeafByteCount
                } <-
                pmatch (pexpectStateAs @PStep02State mInputStateData)
              pexpecting
                ( pisDaHashPreimageViolationV1
                    # pfromData pstep02State'committedTxId
                    # pfromData pstep02State'derivedTxId
                    # pfromData pstep02State'committedLeafByteCount
                )
                (pconstant True)
