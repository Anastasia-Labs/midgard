{- |
Module      : Midgard.Validators.FraudProofs.NoInput
Description : Plutarch port of @validators/fraud-proofs/no-input/step-0{1,2,3,4}.ak@.

The non-existent-input fraud proof (spec §5.1.1): a committed transaction
spending an output that never existed.

Four validators:

1. bind the disputed transaction to the block, and forward its id together with
   the two roots the rest of the proof needs;
2. open field 0 through the §8.8 door and read the disputed input out of it;
3. prove the input absent from the block's __initial ledger__;
4. prove its producing transaction absent from the block's __transactions root__,
   and finalise.

=== Non-existence is two absences

An output either existed before the block or was produced inside it, so a proof
that closes one door proves nothing. Step-03's absence alone is satisfied by any
output produced mid-block — which is most of them — and step-04's alone is
satisfied by any output that predates the block. The family is the conjunction,
and it is spread over two transactions because each absence is its own MPF walk.

=== Two roots, two keys, and neither key is the obvious one

Both roots come from evidence step-01 authenticated: @prev_utxos_root@ off the
challenged header, and the raw transactions root off the carriage /after/
'Midgard.FraudProofs.Common.ppassNativeTxToNextStep' has checked it against the
header's counted @transactions_root@. Neither can be re-derived later, which is
why both are thread state.

The keys differ, and getting either wrong yields a proof that verifies against a
key nothing ever stored. The ledger MPF is keyed by the node's CBOR encoding of
a transaction input — a definite two-element array, which is
'pencodeMidgardTxInput' and __not__ a serialised Plutus constructor. The
transactions MPF is keyed by the raw 32-byte native transaction id, directly.

=== The carriage is the prover's choice, twice

Both absences take a 'Midgard.FraudProofs.Common.PNonMembershipCarriage': the
proof may ride in this transaction's redeemers or have been published beforehand
as chunks the transaction merely references (issue #545). Which route a prover
takes is its own business; both end at the same verified walk against the same
authenticated root.
-}
module Midgard.Validators.FraudProofs.NoInput (
  noInputStep01Validator,
  noInputStep02Validator,
  noInputStep03Validator,
  noInputStep04Validator,
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

import Midgard.FraudProofs.Common (
  pcarriageTransactionsPhasRoot,
  pcontinue,
  pfinalize,
  ppassNativeTxToNextStepCarried,
  pverifyNonMembershipCarried,
 )
import Midgard.FraudProofs.FieldOpening (
  PNativeTxAnchorV1 (..),
  popenedFieldView,
  pspendInputsFieldIndex,
 )
import Midgard.FraudProofs.NativeTx.Components (pencodeMidgardTxInput)
import Midgard.FraudProofs.NativeTx.Types (PMidgardTxInput (..))
import Midgard.FraudProofs.NoInput (
  PStep02Args (..),
  PStep02State (..),
  PStep03Args (..),
  PStep03State (..),
  PStep04Args (..),
  PStep04State (..),
 )
import Midgard.LedgerState (PHeaderV1 (..))
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

{- | Aiken @validators/fraud-proofs/no-input/step-01.ak@.

Binds the disputed transaction and forwards its id together with the two roots
the rest of the proof runs against.

The transactions root that travels is the /raw/ one the carriage named, and it is
safe to forward only because @pass_native_tx_to_next_step@ has already checked it
against the header's counted @transactions_root@ — the same check that made the
transaction's inclusion mean anything. Step-04 then walks a real MPF root rather
than a number the prover chose.
-}
noInputStep01Validator ::
  forall (s :: S).
  Term
    s
    ( PAsData PScriptHash -- step-02's script hash
        :--> PAsData PCurrencySymbol -- computation thread token policy
        :--> PAsData PScriptHash -- hub oracle
        :--> PScriptContext
        :--> PUnit
    )
noInputStep01Validator = plam $
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
               header
               badTxId
               _badTxView -> P.do
                PHeaderV1 {pheader'prevUtxosRoot} <- pmatch (pfromData header)
                pexpecting (outputScriptHash #== step02ValidatorScriptHash) $
                  pexpecting
                    ( outputStateData
                        #== pforgetData
                          ( pdata
                              ( pcon
                                  ( PStep02State
                                      { pstep02State'badTxId = pdata badTxId
                                      , pstep02State'blocksPrevUtxosRoot =
                                          pheader'prevUtxosRoot
                                      , pstep02State'blocksTransactionsRoot =
                                          pdata (pcarriageTransactionsPhasRoot # carriage)
                                      }
                                  )
                              )
                          )
                    )
                    (pconstant True)

--------------------------------------------------------------------------------
-- Step 02
--------------------------------------------------------------------------------

{- | Aiken @validators/fraud-proofs/no-input/step-02.ak@.

Reads the disputed input out of the transaction's authenticated field 0.

The expected commitment is extracted positionally from the compact structures
@bad_tx_id@ authenticates — never supplied — and the item itself is one
arithmetic slice at §5.3's fixed stride (§10.5) rather than a reproduction of the
whole collection.
-}
noInputStep02Validator ::
  forall (s :: S).
  Term
    s
    ( PAsData PScriptHash -- step-03's script hash
        :--> PAsData PCurrencySymbol -- computation thread token policy
        :--> PAsData PCurrencySymbol -- field preimage certificate policy
        :--> PScriptContext
        :--> PUnit
    )
noInputStep02Validator = plam $
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
              , pstep02Args'badInputIndex
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
                    , pstep02State'blocksPrevUtxosRoot
                    , pstep02State'blocksTransactionsRoot
                    } <-
                    pmatch (pexpectStateAs @PStep02State mInputStateData)
                  spendInputsView <-
                    plet $
                      popenedFieldView
                        # pfromData pstep02Args'spendInputsOpening
                        # pcon (PBodyAnchor {pbodyAnchor'txId = pstep02State'badTxId})
                        # pspendInputsFieldIndex
                        # referenceInputs
                        # fieldPreimageCertificatePolicyId
                  missingInput <-
                    plet $ pspendInputAt # spendInputsView # pfromData pstep02Args'badInputIndex
                  pexpecting (outputScriptHash #== step03ValidatorScriptHash) $
                    pexpecting
                      ( outputStateData
                          #== pforgetData
                            ( pdata
                                ( pcon
                                    ( PStep03State
                                        { pstep03State'missingInput = pdata missingInput
                                        , pstep03State'blocksPrevUtxosRoot =
                                            pstep02State'blocksPrevUtxosRoot
                                        , pstep03State'blocksTransactionsRoot =
                                            pstep02State'blocksTransactionsRoot
                                        }
                                    )
                                )
                            )
                      )
                      (pconstant True)

--------------------------------------------------------------------------------
-- Step 03
--------------------------------------------------------------------------------

{- | Aiken @validators/fraud-proofs/no-input/step-03.ak@.

The first absence: the disputed input is not in the block's initial ledger.

Only the /producing transaction's id/ survives into step-04, because that is the
transactions-root key. The output index is dropped deliberately: a transaction
that does not exist produced no output at any index, so carrying one would be
carrying a number the next check cannot use.
-}
noInputStep03Validator ::
  forall (s :: S).
  Term
    s
    ( PAsData PScriptHash -- step-04's script hash
        :--> PAsData PCurrencySymbol -- computation thread token policy
        :--> PScriptContext
        :--> PUnit
    )
noInputStep03Validator = plam $
  \step04ValidatorScriptHash computationThreadTokenPolicyId ctx ->
    pstep ctx $ \datum redeemer ownOutRef txInfo ->
      pdispatch @_ @PStep03Args computationThreadTokenPolicyId datum redeemer ownOutRef txInfo $
        \args -> P.do
          PStep03Args
            { pstep03Args'inputIndex
            , pstep03Args'outputIndex
            , pstep03Args'nonMembershipInLedger
            } <-
            pmatch args
          PTxInfo {ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs, ptxInfo'redeemers} <-
            pmatch txInfo
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
              PStep03State
                { pstep03State'missingInput
                , pstep03State'blocksPrevUtxosRoot
                , pstep03State'blocksTransactionsRoot
                } <-
                pmatch (pexpectStateAs @PStep03State mInputStateData)
              missingInput <- plet $ pfromData pstep03State'missingInput
              PMidgardTxInput {ptxInput'txId} <- pmatch missingInput
              -- 2. Absent from the block's initial ledger, under the ledger
              --    MPF's own key encoding.
              pexpecting
                ( pverifyNonMembershipCarried
                    (pfromData pstep03Args'nonMembershipInLedger)
                    (pfromData pstep03State'blocksPrevUtxosRoot)
                    (pencodeMidgardTxInput # missingInput)
                    (pfromData ptxInfo'referenceInputs)
                    (pto (pto (pfromData ptxInfo'redeemers)))
                )
                $ pexpecting (outputScriptHash #== step04ValidatorScriptHash)
                $ pexpecting
                  ( outputStateData
                      #== pforgetData
                        ( pdata
                            ( pcon
                                ( PStep04State
                                    { pstep04State'missingInputTxId = ptxInput'txId
                                    , pstep04State'blocksTransactionsRoot =
                                        pstep03State'blocksTransactionsRoot
                                    }
                                )
                            )
                        )
                  )
                  (pconstant True)

--------------------------------------------------------------------------------
-- Step 04
--------------------------------------------------------------------------------

{- | Aiken @validators/fraud-proofs/no-input/step-04.ak@.

The second absence, and the conviction: no transaction of the same block produced
the missing input.

The key here is the raw 32-byte native transaction id, because that is what the
transactions MPF is keyed by — not an encoded input, and not a serialised
constructor.
-}
noInputStep04Validator ::
  forall (s :: S).
  Term
    s
    ( PAsData PCurrencySymbol -- fraud proof token policy
        :--> PAsData PAddress -- fraud proof token address
        :--> PAsData PCurrencySymbol -- computation thread token policy
        :--> PScriptContext
        :--> PUnit
    )
noInputStep04Validator = plam $
  \fraudProofTokenPolicyId fraudProofTokenAddress computationThreadTokenPolicyId ctx ->
    pstep ctx $ \datum redeemer ownOutRef txInfo ->
      pdispatch @_ @PStep04Args computationThreadTokenPolicyId datum redeemer ownOutRef txInfo $
        \args -> P.do
          PStep04Args
            { pstep04Args'inputIndex
            , pstep04Args'outputIndex
            , pstep04Args'nonMembershipInTxs
            , pstep04Args'fraudProofMintRedeemerIndex
            } <-
            pmatch args
          PTxInfo {ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs, ptxInfo'redeemers} <-
            pmatch txInfo
          redeemers <- plet $ pto (pto (pfromData ptxInfo'redeemers))
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
            redeemers
            $ \_ownScriptHash _threadTokenAssetName _fraudProver mInputStateData -> P.do
              PStep04State
                {pstep04State'missingInputTxId, pstep04State'blocksTransactionsRoot} <-
                pmatch (pexpectStateAs @PStep04State mInputStateData)
              pexpecting
                ( pverifyNonMembershipCarried
                    (pfromData pstep04Args'nonMembershipInTxs)
                    (pfromData pstep04State'blocksTransactionsRoot)
                    (pfromData pstep04State'missingInputTxId)
                    (pfromData ptxInfo'referenceInputs)
                    redeemers
                )
                (pconstant True)
