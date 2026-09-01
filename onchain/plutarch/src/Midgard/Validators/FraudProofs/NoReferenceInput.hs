{- |
Module      : Midgard.Validators.FraudProofs.NoReferenceInput
Description : Plutarch port of @validators/fraud-proofs/no-reference-input/step-0{1,2,3,4}.ak@.

The non-existent-reference-input fraud proof (spec §5.1.1): a committed
transaction referencing an output that never existed.

'Midgard.Validators.FraudProofs.NoInput' one §2.5 slot over — step-02 opens field
1 rather than field 0 — and the same two absences, for the same reason: an output
either predates the block or was produced inside it, so step-03's absence from
@prev_utxos_root@ and step-04's absence from @transactions_root@ each prove
nothing alone.

=== Two differences from @no-input@, both faithful rather than tidy

__The absence proofs are redeemer-carried only.__ @no-input@ takes a
'Midgard.FraudProofs.Common.PNonMembershipCarriage' at both absences, so a prover
may publish the proof beforehand as chunks (issue #545); this family takes a bare
proof, so it must ride in the step transaction. The port keeps the difference:
the two families' redeemers are wire format and levelling them would break every
SDK built against either.

__The withdrawal index is vestigial and stays anyway.__ Aiken's
@plutarch_pexcludes_raw@ binds it and then finds the redeemer by script hash,
requiring uniqueness; 'Midgard.Common.Utils.pplutarchPexcludesRaw' drops the
parameter for the same reason. It remains a field of the redeemer because the
redeemer is the interface.

=== The keys are the same two as @no-input@

The ledger MPF is keyed by the node's CBOR encoding of a transaction input — a
definite two-element array — and the transactions MPF by the raw 32-byte
transaction id. Swapping them yields a proof that verifies against a key nothing
ever stored.
-}
module Midgard.Validators.FraudProofs.NoReferenceInput (
  noReferenceInputStep01Validator,
  noReferenceInputStep02Validator,
  noReferenceInputStep03Validator,
  noReferenceInputStep04Validator,
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

import Midgard.Common.Utils (pplutarchPexcludesRaw)
import Midgard.FraudProofs.Common (
  pcarriageTransactionsPhasRoot,
  pcontinue,
  pfinalize,
  ppassNativeTxToNextStepCarried,
 )
import Midgard.FraudProofs.FieldOpening (
  PNativeTxAnchorV1 (..),
  popenedFieldView,
  preferenceInputsFieldIndex,
 )
import Midgard.FraudProofs.NativeTx.Components (pencodeMidgardTxInput)
import Midgard.FraudProofs.NativeTx.Types (PMidgardTxInput (..))
import Midgard.FraudProofs.NoReferenceInput (
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

-- | Aiken @validators/fraud-proofs/no-reference-input/step-01.ak@.
noReferenceInputStep01Validator ::
  forall (s :: S).
  Term
    s
    ( PAsData PScriptHash -- step-02's script hash
        :--> PAsData PCurrencySymbol -- computation thread token policy
        :--> PAsData PScriptHash -- hub oracle
        :--> PScriptContext
        :--> PUnit
    )
noReferenceInputStep01Validator = plam $
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
                                      , pstep02State'blocksPrevUtxosRoot = pheader'prevUtxosRoot
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

{- | Aiken @validators/fraud-proofs/no-reference-input/step-02.ak@.

Reads the disputed reference input out of field 1.

The read is 'pspendInputAt' even though the field is the reference-input one:
§5.3 gives fields 0 and 1 the same 38-byte item and the same stride, so the
accessor is shared. What is /not/ shared is the slot, which comes from the index
passed to the door.
-}
noReferenceInputStep02Validator ::
  forall (s :: S).
  Term
    s
    ( PAsData PScriptHash -- step-03's script hash
        :--> PAsData PCurrencySymbol -- computation thread token policy
        :--> PAsData PCurrencySymbol -- field preimage certificate policy
        :--> PScriptContext
        :--> PUnit
    )
noReferenceInputStep02Validator = plam $
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
                    , pstep02State'blocksPrevUtxosRoot
                    , pstep02State'blocksTransactionsRoot
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

{- | Aiken @validators/fraud-proofs/no-reference-input/step-03.ak@.

The first absence: not in the block's initial ledger, keyed by the encoded input.
-}
noReferenceInputStep03Validator ::
  forall (s :: S).
  Term
    s
    ( PAsData PScriptHash -- step-04's script hash
        :--> PAsData PCurrencySymbol -- computation thread token policy
        :--> PScriptContext
        :--> PUnit
    )
noReferenceInputStep03Validator = plam $
  \step04ValidatorScriptHash computationThreadTokenPolicyId ctx ->
    pstep ctx $ \datum redeemer ownOutRef txInfo ->
      pdispatch @_ @PStep03Args computationThreadTokenPolicyId datum redeemer ownOutRef txInfo $
        \args -> P.do
          PStep03Args
            { pstep03Args'inputIndex
            , pstep03Args'outputIndex
            , pstep03Args'nonMembershipProofInLedger
            } <-
            pmatch args
          PTxInfo {ptxInfo'inputs, ptxInfo'outputs, ptxInfo'redeemers} <- pmatch txInfo
          redeemers <- plet $ pto (pto (pfromData ptxInfo'redeemers))
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
                { pstep03State'missingReferenceInput
                , pstep03State'blocksPrevUtxosRoot
                , pstep03State'blocksTransactionsRoot
                } <-
                pmatch (pexpectStateAs @PStep03State mInputStateData)
              missingReferenceInput <- plet $ pfromData pstep03State'missingReferenceInput
              PMidgardTxInput {ptxInput'txId} <- pmatch missingReferenceInput
              pexpecting
                ( pplutarchPexcludesRaw
                    (pfromData pstep03State'blocksPrevUtxosRoot)
                    (pencodeMidgardTxInput # missingReferenceInput)
                    (pforgetData pstep03Args'nonMembershipProofInLedger)
                    redeemers
                )
                $ pexpecting (outputScriptHash #== step04ValidatorScriptHash)
                $ pexpecting
                  ( outputStateData
                      #== pforgetData
                        ( pdata
                            ( pcon
                                ( PStep04State
                                    { pstep04State'missingReferenceInputTxId = ptxInput'txId
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

{- | Aiken @validators/fraud-proofs/no-reference-input/step-04.ak@.

The second absence, and the conviction: keyed by the raw 32-byte transaction id.
-}
noReferenceInputStep04Validator ::
  forall (s :: S).
  Term
    s
    ( PAsData PCurrencySymbol -- fraud proof token policy
        :--> PAsData PAddress -- fraud proof token address
        :--> PAsData PCurrencySymbol -- computation thread token policy
        :--> PScriptContext
        :--> PUnit
    )
noReferenceInputStep04Validator = plam $
  \fraudProofTokenPolicyId fraudProofTokenAddress computationThreadTokenPolicyId ctx ->
    pstep ctx $ \datum redeemer ownOutRef txInfo ->
      pdispatch @_ @PStep04Args computationThreadTokenPolicyId datum redeemer ownOutRef txInfo $
        \args -> P.do
          PStep04Args
            { pstep04Args'inputIndex
            , pstep04Args'outputIndex
            , pstep04Args'nonMembershipProofInTxs
            , pstep04Args'fraudProofMintRedeemerIndex
            } <-
            pmatch args
          PTxInfo {ptxInfo'inputs, ptxInfo'outputs, ptxInfo'redeemers} <- pmatch txInfo
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
                { pstep04State'missingReferenceInputTxId
                , pstep04State'blocksTransactionsRoot
                } <-
                pmatch (pexpectStateAs @PStep04State mInputStateData)
              pexpecting
                ( pplutarchPexcludesRaw
                    (pfromData pstep04State'blocksTransactionsRoot)
                    (pfromData pstep04State'missingReferenceInputTxId)
                    (pforgetData pstep04Args'nonMembershipProofInTxs)
                    redeemers
                )
                (pconstant True)
