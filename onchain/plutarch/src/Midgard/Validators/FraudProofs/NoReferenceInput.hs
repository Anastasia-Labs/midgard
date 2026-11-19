{- |
Module      : Midgard.Validators.FraudProofs.NoReferenceInput
Description : Plutarch port of @validators/fraud-proofs/no-reference-input/step-0{1,2,3,4}.ak@.

The non-existent-reference-input fraud proof (spec §5.1.1): a committed
transaction referencing an output that never existed.

'Midgard.Validators.FraudProofs.NoReferenceInput' one §2.5 slot over — step-02 opens field
1 rather than field 1 — and the same two absences, for the same reason: an output
either predates the block or was produced inside it, so step-03's absence from
@prev_utxos_root@ and step-04's absence from @transactions_root@ each prove
nothing alone.

=== The shared absence carriage

Like @no-input@, both absence steps take
'Midgard.FraudProofs.Common.PNonMembershipCarriage'. A fitting proof can travel
in the transaction's withdrawal redeemer; a deeper proof can travel through
published chunks. Both arms authenticate the same root and key.

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
  preferenceInputsFieldIndex,
 )
import Midgard.FraudProofs.NativeTx.Components (pencodeMidgardTxInput)
import Midgard.FraudProofs.NativeTx.Types (
  PMidgardTxInput (..),
  PNativeTxCompact (..),
  PVerifiedMidgardNativeTxCompact (..),
 )
import Midgard.FraudProofs.NoReferenceInput
import Midgard.FraudProofs.ProofThreadSubstrate qualified as Subject
import Midgard.LedgerState
import Midgard.MpfProof (phasValueHash)
import Midgard.NativeTxMachineWalk (pspendInputAt, pspendInputCount)
import Midgard.RejectionReason (PRejectionReasonV1 (PInputNotFound))
import Midgard.TransitionTrace
import Midgard.Validators.FraudProofs.Step (
  pdispatch,
  pexpectDatum,
  pexpectStateAs,
  pexpecting,
  pstateIsAbsent,
  pstep,
 )
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.Unsafe (punsafeCoerce)

--------------------------------------------------------------------------------
-- Step 01
--------------------------------------------------------------------------------

{- | Aiken @validators/fraud-proofs/no-reference-input/step-01.ak@.

Binds the disputed transaction and forwards its id together with the two roots
the rest of the proof runs against.

The transactions root that travels is the /raw/ one the carriage named, and it is
safe to forward only because @pass_native_tx_to_next_step@ has already checked it
against the header's counted @transactions_root@ — the same check that made the
transaction's inclusion mean anything. Step-04 then walks a real MPF root rather
than a number the prover chose.
-}
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
      pdispatch @_ @PStep01Args computationThreadTokenPolicyId datum redeemer ownOutRef txInfo $
        \args -> P.do
          PStep01Args source <- pmatch args
          pmatch (pfromData source) $ \case
            PAcceptedSource carried -> P.do
              carriage <- plet $ pfromData carried
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
                   badTxView -> P.do
                    PVerifiedMidgardNativeTxCompact {pverified'txCompact} <- pmatch badTxView
                    PNativeTxCompact {pcompact'validityCode} <- pmatch pverified'txCompact
                    PHeaderV1 {pheader'prevUtxosRoot} <- pmatch (pfromData header)
                    pexpecting (pcompact'validityCode #== 0) $
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
            PForcedSource inputIndex outputIndex header membership direction -> P.do
              PTxInfo {ptxInfo'inputs, ptxInfo'outputs} <- pmatch txInfo
              pcontinue
                computationThreadTokenPolicyId
                (pexpectDatum datum)
                (pfromData inputIndex)
                (pfromData outputIndex)
                ownOutRef
                (pfromData ptxInfo'inputs)
                (pfromData ptxInfo'outputs)
                $ \_ threadName _ inputState outputHash outputState -> P.do
                  subject <-
                    plet $
                      Subject.pbindForcedSubjectToThread
                        # pto (pfromData threadName)
                        # pfromData header
                        # pfromData membership
                        # pfromData direction
                  PInputNotFound sourceKind _ <- pmatch $ Subject.prejectionReasonOf # subject
                  PHeaderV1 {pheader'eventToStepRoot, pheader'totalEventCount, pheader'transitionTraceRoot, pheader'transitionStepCount} <- pmatch $ pfromData header
                  PRootMembershipProof {prootMembership'key} <- pmatch $ pfromData membership
                  let eventKey = pcon $ PForcedTransactionEventKey $ punsafeCoerce prootMembership'key
                      expected =
                        pcon $
                          PForcedStep02State
                            (pdata subject)
                            (pdata eventKey)
                            pheader'eventToStepRoot
                            pheader'totalEventCount
                            pheader'transitionTraceRoot
                            pheader'transitionStepCount
                  pstateIsAbsent inputState
                    #&& pfromData sourceKind
                    #== 1
                    #&& outputHash
                    #== step02ValidatorScriptHash
                    #&& outputState
                    #== pforgetData (pdata expected)

--------------------------------------------------------------------------------
-- Step 02
--------------------------------------------------------------------------------

{- | Aiken @validators/fraud-proofs/no-reference-input/step-02.ak@.

Reads the disputed input out of the transaction's authenticated field 1.

The expected commitment is extracted positionally from the compact structures
@bad_tx_id@ authenticates — never supplied — and the item itself is one
arithmetic slice at §5.3's fixed stride (§10.5) rather than a reproduction of the
whole collection.
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
noReferenceInputStep02Validator =
  plam $
    \step03ValidatorScriptHash
     computationThreadTokenPolicyId
     fieldPreimageCertificatePolicyId
     ctx ->
        pstep ctx $ \datum redeemer ownOutRef txInfo ->
          pdispatch @_ @PStep02Args computationThreadTokenPolicyId datum redeemer ownOutRef txInfo $
            \args -> pmatch args $ \case
              PStep02Args
                { pstep02Args'inputIndex
                , pstep02Args'outputIndex
                , pstep02Args'referenceInputsOpening
                , pstep02Args'badReferenceInputIndex
                } -> P.do
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
                        missingInput <-
                          plet $ pspendInputAt # referenceInputsView # pfromData pstep02Args'badReferenceInputIndex
                        pexpecting (outputScriptHash #== step03ValidatorScriptHash) $
                          pexpecting
                            ( outputStateData
                                #== pforgetData
                                  ( pdata
                                      ( pcon
                                          ( PStep03State
                                              { pstep03State'missingReferenceInput = pdata missingInput
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
              PForcedStep02Args inputIndex outputIndex opening membership -> P.do
                PTxInfo {ptxInfo'inputs, ptxInfo'outputs, ptxInfo'referenceInputs} <- pmatch txInfo
                pcontinue
                  computationThreadTokenPolicyId
                  (pexpectDatum datum)
                  (pfromData inputIndex)
                  (pfromData outputIndex)
                  ownOutRef
                  (pfromData ptxInfo'inputs)
                  (pfromData ptxInfo'outputs)
                  $ \_ _ _ inputState outputHash outputState -> P.do
                    PForcedStep02State subject eventKey eventRoot eventCount traceRoot traceCount <- pmatch $ pexpectStateAs @PStep02State inputState
                    PInputNotFound sourceKind selectedIndex <- pmatch $ Subject.prejectionReasonOf # pfromData subject
                    Subject.PVerdictSubject {Subject.psubject'transactionId} <- pmatch $ pfromData subject
                    view <-
                      plet $
                        popenedFieldView
                          # pfromData opening
                          # pcon (PBodyAnchor psubject'transactionId)
                          # preferenceInputsFieldIndex
                          # pfromData ptxInfo'referenceInputs
                          # fieldPreimageCertificatePolicyId
                    selected <-
                      plet $
                        pif
                          (pfromData selectedIndex #>= 0 #&& pfromData selectedIndex #< (pspendInputCount # view))
                          (pcon $ PDJust $ pdata $ pspendInputAt # view # pfromData selectedIndex)
                          (pcon PDNothing)
                    proof <- plet $ pfromData membership
                    PRootMembershipProof {prootMembership'key, prootMembership'value} <- pmatch proof
                    PEventToStepValue {peventToStepValue'stepIndex, peventToStepValue'phase} <- pmatch $ pfromData (punsafeCoerce prootMembership'value)
                    let expected = pcon $ PForcedStep03State eventKey traceRoot traceCount peventToStepValue'stepIndex (pdata selected)
                    pfromData sourceKind
                      #== 1
                      #&& pverifyRootMembershipWithBytes
                        proof
                        (pdata $ pcon PEventToStepRootDomain)
                        (pfromData eventRoot)
                        (pfromData eventCount)
                        (pserialiseData # prootMembership'key)
                        (pserialiseData # prootMembership'value)
                      #&& prootMembership'key
                      #== pforgetData eventKey
                      #&& pfromData peventToStepValue'phase
                      #== pcon PForcedTransaction
                      #&& pfromData peventToStepValue'stepIndex
                      #>= 0
                      #&& pfromData peventToStepValue'stepIndex
                      #< pfromData traceCount
                      #&& outputHash
                      #== step03ValidatorScriptHash
                      #&& outputState
                      #== pforgetData (pdata expected)

--------------------------------------------------------------------------------
-- Step 03
--------------------------------------------------------------------------------

{- | Aiken @validators/fraud-proofs/no-reference-input/step-03.ak@.

The first absence: the disputed input is not in the block's initial ledger.

Only the /producing transaction's id/ survives into step-04, because that is the
transactions-root key. The output index is dropped deliberately: a transaction
that does not exist produced no output at any index, so carrying one would be
carrying a number the next check cannot use.
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
        \args -> pmatch args $ \case
          PStep03Args
            { pstep03Args'inputIndex
            , pstep03Args'outputIndex
            , pstep03Args'nonMembershipInLedger
            } -> P.do
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
                      { pstep03State'missingReferenceInput
                      , pstep03State'blocksPrevUtxosRoot
                      , pstep03State'blocksTransactionsRoot
                      } <-
                      pmatch (pexpectStateAs @PStep03State mInputStateData)
                    missingInput <- plet $ pfromData pstep03State'missingReferenceInput
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
                                          { pstep04State'missingReferenceInputTxId = ptxInput'txId
                                          , pstep04State'blocksTransactionsRoot =
                                              pstep03State'blocksTransactionsRoot
                                          }
                                      )
                                  )
                              )
                        )
                        (pconstant True)
          PForcedStep03Args inputIndex outputIndex membership -> P.do
            PTxInfo {ptxInfo'inputs, ptxInfo'outputs} <- pmatch txInfo
            pcontinue
              computationThreadTokenPolicyId
              (pexpectDatum datum)
              (pfromData inputIndex)
              (pfromData outputIndex)
              ownOutRef
              (pfromData ptxInfo'inputs)
              (pfromData ptxInfo'outputs)
              $ \_ _ _ inputState outputHash outputState -> P.do
                PForcedStep03State eventKey traceRoot traceCount stepIndex selected <- pmatch $ pexpectStateAs @PStep03State inputState
                proof <- plet $ pfromData membership
                PRootMembershipProof {prootMembership'key, prootMembership'value} <- pmatch proof
                PTransitionStep {..} <- pmatch $ pfromData (punsafeCoerce prootMembership'value)
                let expected = pcon $ PForcedStep04State selected ptransitionStep'preUtxosRoot
                pverifyRootMembershipWithBytes
                  proof
                  (pdata $ pcon PTransitionTraceRootDomain)
                  (pfromData traceRoot)
                  (pfromData traceCount)
                  (pserialiseData # prootMembership'key)
                  (pserialiseData # prootMembership'value)
                  #&& prootMembership'key
                  #== pforgetData stepIndex
                  #&& ptransitionStep'stepIndex
                  #== stepIndex
                  #&& pfromData stepIndex
                  #>= 0
                  #&& pfromData stepIndex
                  #< pfromData traceCount
                  #&& pfromData ptransitionStep'schemaVersion
                  #== ptransitionStepSchemaVersionV1
                  #&& ptransitionStep'eventKey
                  #== eventKey
                  #&& pfromData ptransitionStep'phase
                  #== pcon PForcedTransaction
                  #&& outputHash
                  #== step04ValidatorScriptHash
                  #&& outputState
                  #== pforgetData (pdata expected)

--------------------------------------------------------------------------------
-- Step 04
--------------------------------------------------------------------------------

{- | Aiken @validators/fraud-proofs/no-reference-input/step-04.ak@.

The second absence, and the conviction: no transaction of the same block produced
the missing input.

The key here is the raw 32-byte native transaction id, because that is what the
transactions MPF is keyed by — not an encoded input, and not a serialised
constructor.
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
        \args -> pmatch args $ \case
          PStep04Args
            { pstep04Args'inputIndex
            , pstep04Args'outputIndex
            , pstep04Args'nonMembershipInTxs
            , pstep04Args'fraudProofMintRedeemerIndex
            } -> P.do
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
                    { pstep04State'missingReferenceInputTxId
                    , pstep04State'blocksTransactionsRoot
                    } <-
                    pmatch (pexpectStateAs @PStep04State mInputStateData)
                  pexpecting
                    ( pverifyNonMembershipCarried
                        (pfromData pstep04Args'nonMembershipInTxs)
                        (pfromData pstep04State'blocksTransactionsRoot)
                        (pfromData pstep04State'missingReferenceInputTxId)
                        (pfromData ptxInfo'referenceInputs)
                        redeemers
                    )
                    (pconstant True)
          PForcedStep04Args inputIndex outputIndex mintIndex membership -> P.do
            PTxInfo {ptxInfo'inputs, ptxInfo'outputs, ptxInfo'redeemers} <- pmatch txInfo
            pfinalize
              computationThreadTokenPolicyId
              fraudProofTokenPolicyId
              fraudProofTokenAddress
              (pexpectDatum datum)
              (pfromData inputIndex)
              (pfromData outputIndex)
              (pfromData mintIndex)
              ownOutRef
              (pfromData ptxInfo'inputs)
              (pfromData ptxInfo'outputs)
              (pto $ pto $ pfromData ptxInfo'redeemers)
              $ \_ _ _ inputState -> P.do
                PForcedStep04State selected root <- pmatch $ pexpectStateAs @PStep04State inputState
                pmatch (pfromData selected) $ \case
                  PDNothing -> pfromData membership #== pcon PDNothing
                  PDJust input -> pmatch (pfromData membership) $ \case
                    PDNothing -> perror
                    PDJust witness -> pmatch (pfromData witness) $ \(PLedgerMembership valueHash proof) ->
                      phasValueHash
                        # pfromData root
                        # (pencodeMidgardTxInput # pfromData input)
                        # pfromData valueHash
                        # pfromData proof
