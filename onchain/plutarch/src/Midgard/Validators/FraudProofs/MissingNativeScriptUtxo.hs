module Midgard.Validators.FraudProofs.MissingNativeScriptUtxo (
    missingNativeScriptUtxoStep01Validator,
    missingNativeScriptUtxoStep02Validator,
    missingNativeScriptUtxoStep03Validator,
    missingNativeScriptUtxoStep04Validator,
    missingNativeScriptUtxoStep05Validator,
    missingNativeScriptUtxoStep06Validator,
    missingNativeScriptUtxoStep07Validator,
) where

import Plutarch.LedgerApi.V3 (
    PAddress,
    PCurrencySymbol,
    PScriptContext,
    PScriptHash,
    PTxInfo (..),
    PTxOutRef (..),
 )
import Plutarch.Monadic qualified as P
import Plutarch.Prelude

import Midgard.FraudProofs.Common (
    pcontinue,
    pfinalize,
    ppassNativeTxToNextStepCarried,
    pverifyMembershipCarried,
 )
import Midgard.FraudProofs.FieldOpening (
    PNativeTxAnchorV1 (..),
    pfoldOpenedField,
    popenedCertifiedFieldWalkFromGrammar,
    popenedFieldGrammarCertification,
    popenedFieldView,
    popenedFieldWalk,
    presumeOpenedFieldGrammarCertification,
    presumeOpenedFieldWalk,
    pscriptWitnessesFieldIndex,
    pspendInputsFieldIndex,
 )
import Midgard.FraudProofs.MissingNativeScriptUtxo (
    PStep01Args (..),
    PStep02Args (..),
    PStep02State (..),
    PStep03Args (..),
    PStep03State (..),
    PStep04Args (..),
    PStep04State (..),
    PStep05Args (..),
    PStep05PhaseV1 (..),
    PStep05State (..),
    PStep06Args (..),
    PStep07Args (..),
    pdirectScriptWitnessLimit,
    pstagedScriptWitnessBatchLimit,
 )
import Midgard.FraudProofs.NativeTx.Components (
    pdecodeMidgardVersionedScriptAt,
    pencodeMidgardTxInput,
    pencodeMidgardVersionedScript,
 )
import Midgard.FraudProofs.NativeTx.Types (
    PMidgardAddress (..),
    PMidgardCredential (..),
    PMidgardScriptLanguage (..),
    PMidgardTxInput (..),
    PMidgardVersionedScript (..),
    PNativeTxCompact (..),
    PVerifiedMidgardNativeTxCompact (..),
 )
import Midgard.LedgerOutput (pdecodeCanonicalAddressBytes)
import Midgard.LedgerOutputCommitment (
    PLedgerOutputCommitmentV1 (..),
    pdecodeLedgerOutputCommitment,
 )
import Midgard.LedgerState (PHeaderV1 (..))
import Midgard.NativeTxFieldAccess (
    pfieldItemCount,
    pprovisionalFieldItemCountForCertification,
 )
import Midgard.NativeTxMachineWalk (
    pcertifyFieldGrammar,
    pfieldGrammarCheckpointHash,
    pfieldGrammarIsComplete,
    pfieldWalkCheckpointHash,
    pspendInputAt,
    pwalkFold,
    pwalkIsComplete,
    pwalkRemaining,
 )
import Midgard.ScriptProof (pversionedScriptHash)
import Midgard.Validators.FraudProofs.Step (
    pdispatch,
    pexpectDatum,
    pexpectStateAs,
    pexpecting,
    pstateIsAbsent,
    pstep,
 )

missingNativeScriptUtxoStep01Validator ::
    forall s.
    Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PAsData PScriptHash :--> PScriptContext :--> PUnit)
missingNativeScriptUtxoStep01Validator = plam $ \step02ScriptHash computationThreadPolicy hubOracle ctx ->
    pstep ctx $ \datum redeemer ownOutRef txInfo ->
        pdispatch @_ @PStep01Args computationThreadPolicy datum redeemer ownOutRef txInfo $ \args -> P.do
            PStep01Args{pstep01Args'carriage} <- pmatch args
            PTxInfo{ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs, ptxInfo'redeemers} <- pmatch txInfo
            ppassNativeTxToNextStepCarried
                computationThreadPolicy
                hubOracle
                datum
                (pfromData pstep01Args'carriage)
                ownOutRef
                (pfromData ptxInfo'inputs)
                (pfromData ptxInfo'referenceInputs)
                (pfromData ptxInfo'outputs)
                (pto $ pto $ pfromData ptxInfo'redeemers)
                $ \_ownScriptHash _threadName _prover inputState outputScriptHash outputStateData header badTxId badTxView -> P.do
                    PHeaderV1{pheader'prevUtxosRoot} <- pmatch $ pfromData header
                    PVerifiedMidgardNativeTxCompact{pverified'txCompact} <- pmatch badTxView
                    PNativeTxCompact{pcompact'witnessSetHash, pcompact'validityCode} <- pmatch pverified'txCompact
                    expected <-
                        plet $
                            pcon $
                                PStep02State
                                    (pdata badTxId)
                                    (pdata pcompact'witnessSetHash)
                                    pheader'prevUtxosRoot
                    pexpecting (pstateIsAbsent inputState) $
                        pexpecting (pcompact'validityCode #== 0) $
                            pexpecting (outputScriptHash #== step02ScriptHash) $
                                pexpecting (outputStateData #== pforgetData (pdata expected)) (pconstant True)

missingNativeScriptUtxoStep02Validator ::
    forall s.
    Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
missingNativeScriptUtxoStep02Validator = plam $ \step03ScriptHash computationThreadPolicy certificatePolicy ctx ->
    pstep ctx $ \datum redeemer ownOutRef txInfo ->
        pdispatch @_ @PStep02Args computationThreadPolicy datum redeemer ownOutRef txInfo $ \args -> P.do
            PStep02Args{pstep02Args'inputIndex, pstep02Args'outputIndex, pstep02Args'badInputIndex, pstep02Args'spendInputsOpening} <-
                pmatch args
            PTxInfo{ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs} <- pmatch txInfo
            pcontinue
                computationThreadPolicy
                (pexpectDatum datum)
                (pfromData pstep02Args'inputIndex)
                (pfromData pstep02Args'outputIndex)
                ownOutRef
                (pfromData ptxInfo'inputs)
                (pfromData ptxInfo'outputs)
                $ \_ownScriptHash _threadName _prover inputState outputScriptHash outputStateData -> P.do
                    PStep02State{pstep02State'badTxId, pstep02State'badTxWitnessSetHash, pstep02State'prevUtxosRoot} <-
                        pmatch $ pexpectStateAs @PStep02State inputState
                    view <-
                        plet $
                            popenedFieldView
                                # pfromData pstep02Args'spendInputsOpening
                                # pcon (PBodyAnchor pstep02State'badTxId)
                                # pspendInputsFieldIndex
                                # pfromData ptxInfo'referenceInputs
                                # certificatePolicy
                    inputWithMissingScript <- plet $ pspendInputAt # view # pfromData pstep02Args'badInputIndex
                    expected <-
                        plet $
                            pcon $
                                PStep03State
                                    (pdata inputWithMissingScript)
                                    pstep02State'badTxId
                                    pstep02State'badTxWitnessSetHash
                                    pstep02State'prevUtxosRoot
                    pexpecting (outputScriptHash #== step03ScriptHash) $
                        pexpecting (outputStateData #== pforgetData (pdata expected)) (pconstant True)

missingNativeScriptUtxoStep03Validator ::
    forall s.
    Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
missingNativeScriptUtxoStep03Validator = plam $ \step04ScriptHash computationThreadPolicy ctx ->
    pstep ctx $ \datum redeemer ownOutRef txInfo ->
        pdispatch @_ @PStep03Args computationThreadPolicy datum redeemer ownOutRef txInfo $ \args -> P.do
            PStep03Args
                { pstep03Args'inputIndex
                , pstep03Args'outputIndex
                , pstep03Args'outRef
                , pstep03Args'descriptorCbor
                , pstep03Args'membership
                } <-
                pmatch args
            PTxInfo{ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs, ptxInfo'redeemers} <- pmatch txInfo
            pcontinue
                computationThreadPolicy
                (pexpectDatum datum)
                (pfromData pstep03Args'inputIndex)
                (pfromData pstep03Args'outputIndex)
                ownOutRef
                (pfromData ptxInfo'inputs)
                (pfromData ptxInfo'outputs)
                $ \_ownScriptHash _threadName _prover inputState outputScriptHash outputStateData -> P.do
                    PStep03State
                        { pstep03State'inputWithMissingScript
                        , pstep03State'badTxId
                        , pstep03State'badTxWitnessSetHash
                        , pstep03State'prevUtxosRoot
                        } <-
                        pmatch $ pexpectStateAs @PStep03State inputState
                    PMidgardTxInput{ptxInput'txId, ptxInput'outputIndex} <- pmatch $ pfromData pstep03State'inputWithMissingScript
                    PTxOutRef{ptxOutRef'id, ptxOutRef'idx} <- pmatch $ pfromData pstep03Args'outRef
                    key <- plet $ pledgerOutrefKey # pfromData pstep03Args'outRef
                    expected <-
                        plet $
                            pcon $
                                PStep04State
                                    pstep03Args'outRef
                                    pstep03Args'descriptorCbor
                                    pstep03State'badTxId
                                    pstep03State'badTxWitnessSetHash
                    pexpecting (pfromData ptxInput'txId #== pto (pfromData ptxOutRef'id))
                        $ pexpecting (pfromData ptxInput'outputIndex #== pfromData ptxOutRef'idx)
                        $ pexpecting
                            ( pverifyMembershipCarried
                                (pfromData pstep03Args'membership)
                                (pfromData pstep03State'prevUtxosRoot)
                                key
                                (pfromData pstep03Args'descriptorCbor)
                                (pfromData ptxInfo'referenceInputs)
                                (pto $ pto $ pfromData ptxInfo'redeemers)
                            )
                        $ pexpecting (outputScriptHash #== step04ScriptHash)
                        $ pexpecting (outputStateData #== pforgetData (pdata expected)) (pconstant True)

missingNativeScriptUtxoStep04Validator ::
    forall s.
    Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
missingNativeScriptUtxoStep04Validator = plam $ \step05ScriptHash computationThreadPolicy ctx ->
    pstep ctx $ \datum redeemer ownOutRef txInfo ->
        pdispatch @_ @PStep04Args computationThreadPolicy datum redeemer ownOutRef txInfo $ \args -> P.do
            PStep04Args{pstep04Args'inputIndex, pstep04Args'outputIndex, pstep04Args'missingNativeScriptBytes} <- pmatch args
            PTxInfo{ptxInfo'inputs, ptxInfo'outputs} <- pmatch txInfo
            pcontinue
                computationThreadPolicy
                (pexpectDatum datum)
                (pfromData pstep04Args'inputIndex)
                (pfromData pstep04Args'outputIndex)
                ownOutRef
                (pfromData ptxInfo'inputs)
                (pfromData ptxInfo'outputs)
                $ \_ownScriptHash _threadName _prover inputState outputScriptHash outputStateData -> P.do
                    PStep04State{pstep04State'outRef, pstep04State'descriptorCbor, pstep04State'badTxId, pstep04State'badTxWitnessSetHash} <-
                        pmatch $ pexpectStateAs @PStep04State inputState
                    descriptor <- plet $ pdecodeLedgerOutputCommitment # pfromData pstep04State'descriptorCbor
                    PLedgerOutputCommitmentV1{poutputCommitment'outputIndex, poutputCommitment'address} <- pmatch descriptor
                    PTxOutRef{ptxOutRef'idx} <- pmatch $ pfromData pstep04State'outRef
                    address <-
                        plet $
                            pmatch (pdecodeCanonicalAddressBytes # pfromData poutputCommitment'address) $ \case
                                PJust value -> value
                                PNothing -> perror
                    PMidgardAddress{paddress'paymentCredential} <- pmatch address
                    expectedMissingScriptHashD <-
                        plet $
                            pmatch (pfromData paddress'paymentCredential) $ \case
                                PMidgardScriptCredential scriptHash -> scriptHash
                                PMidgardPubKeyCredential _ -> perror
                    expected <-
                        plet $
                            pcon $
                                PStep05State
                                    expectedMissingScriptHashD
                                    pstep04State'badTxId
                                    pstep04State'badTxWitnessSetHash
                                    (pdata $ pcon PReady)
                    suppliedScript <-
                        plet $
                            pcon $
                                PMidgardVersionedScript
                                    (pdata $ pcon PNativeCardanoScript)
                                    pstep04Args'missingNativeScriptBytes
                    pexpecting (pfromData poutputCommitment'outputIndex #== pfromData ptxOutRef'idx) $
                        pexpecting (pfromData expectedMissingScriptHashD #== pversionedScriptHash # suppliedScript) $
                            pexpecting (outputScriptHash #== step05ScriptHash) $
                                pexpecting (outputStateData #== pforgetData (pdata expected)) (pconstant True)

missingNativeScriptUtxoStep05Validator ::
    forall s.
    Term
        s
        ( PAsData PScriptHash
            :--> PAsData PCurrencySymbol
            :--> PAsData PCurrencySymbol
            :--> PAsData PAddress
            :--> PAsData PCurrencySymbol
            :--> PScriptContext
            :--> PUnit
        )
missingNativeScriptUtxoStep05Validator = plam $
    \step06ScriptHash computationThreadPolicy fraudProofPolicy fraudProofAddress certificatePolicy ctx ->
        pstep ctx $ \datum redeemer ownOutRef txInfo ->
            pdispatch @_ @PStep05Args computationThreadPolicy datum redeemer ownOutRef txInfo $ \action ->
                pmatch action $ \case
                    PDirectFinalize inputIndexD outputIndexD mintRedeemerIndexD openingD -> P.do
                        PTxInfo{ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs, ptxInfo'redeemers} <- pmatch txInfo
                        pfinalize
                            computationThreadPolicy
                            fraudProofPolicy
                            fraudProofAddress
                            (pexpectDatum datum)
                            (pfromData inputIndexD)
                            (pfromData outputIndexD)
                            (pfromData mintRedeemerIndexD)
                            ownOutRef
                            (pfromData ptxInfo'inputs)
                            (pfromData ptxInfo'outputs)
                            (pto $ pto $ pfromData ptxInfo'redeemers)
                            $ \_ownScriptHash _threadName _prover inputState -> P.do
                                state <- plet $ pexpectStateAs @PStep05State inputState
                                PStep05State{pstep05State'expectedMissingScriptHash, pstep05State'phase} <- pmatch state
                                pexpecting (pfromData pstep05State'phase #== pcon PReady) $ P.do
                                    opened <-
                                        plet $
                                            popenedFieldWalk
                                                # pfromData openingD
                                                # (pscriptWitnessAnchor # state)
                                                # pscriptWitnessesFieldIndex
                                                # pfromData ptxInfo'referenceInputs
                                                # certificatePolicy
                                    PPair view _start <- pmatch opened
                                    pexpecting (pfieldItemCount # view #<= pdirectScriptWitnessLimit) $ P.do
                                        found <-
                                            plet $
                                                pfoldOpenedField @PBool
                                                    # opened
                                                    # pconstant False
                                                    # (pscanScriptWitness # pfromData pstep05State'expectedMissingScriptHash)
                                        pexpecting (pnot # found) (pconstant True)
                    PStartGrammarCertification inputIndexD outputIndexD openingD itemBudgetD -> P.do
                        PTxInfo{ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs} <- pmatch txInfo
                        itemBudget <- plet $ pfromData itemBudgetD
                        pexpecting (pvalidStagedBudget # itemBudget)
                            $ pcontinue
                                computationThreadPolicy
                                (pexpectDatum datum)
                                (pfromData inputIndexD)
                                (pfromData outputIndexD)
                                ownOutRef
                                (pfromData ptxInfo'inputs)
                                (pfromData ptxInfo'outputs)
                            $ \_ownScriptHash _threadName _prover inputState outputScriptHash outputStateData -> P.do
                                state <- plet $ pexpectStateAs @PStep05State inputState
                                PStep05State{pstep05State'phase} <- pmatch state
                                pexpecting (pfromData pstep05State'phase #== pcon PReady) $ P.do
                                    PPair view start <-
                                        pmatch $
                                            popenedFieldGrammarCertification
                                                # pfromData openingD
                                                # (pscriptWitnessAnchor # state)
                                                # pscriptWitnessesFieldIndex
                                                # pfromData ptxInfo'referenceInputs
                                                # certificatePolicy
                                    pexpecting (pprovisionalFieldItemCountForCertification # view #> pdirectScriptWitnessLimit) $
                                        plet (pcertifyFieldGrammar # view # start # itemBudget) $ \next ->
                                            pexpecting (pnot #$ pfieldGrammarIsComplete # next) $
                                                pexpecting (outputScriptHash #== step06ScriptHash) $
                                                    pexpecting
                                                        ( outputStateData
                                                            #== pforgetData
                                                                ( pdata
                                                                    ( pstateWithPhase
                                                                        # state
                                                                        # (pcon $ PGrammarCertification $ pdata $ pfieldGrammarCheckpointHash # next)
                                                                    )
                                                                )
                                                        )
                                                        (pconstant True)

missingNativeScriptUtxoStep06Validator ::
    forall s.
    Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
missingNativeScriptUtxoStep06Validator = plam $ \step07ScriptHash computationThreadPolicy certificatePolicy ctx ->
    pstep ctx $ \datum redeemer ownOutRef txInfo ->
        pdispatch @_ @PStep06Args computationThreadPolicy datum redeemer ownOutRef txInfo $ \action -> P.do
            PTxInfo{ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs} <- pmatch txInfo
            pcontinue
                computationThreadPolicy
                (pexpectDatum datum)
                (pstep06InputIndex # action)
                (pstep06OutputIndex # action)
                ownOutRef
                (pfromData ptxInfo'inputs)
                (pfromData ptxInfo'outputs)
                $ \ownScriptHash _threadName _prover inputState outputScriptHash outputStateData -> P.do
                    state <- plet $ pexpectStateAs @PStep05State inputState
                    grammarCheckpointHash <- plet $ pgrammarCheckpointHashOf # state
                    itemBudget <- plet $ pstep06ItemBudget # action
                    pexpecting (pvalidStagedBudget # itemBudget) $
                        pmatch action $ \case
                            PResumeGrammarCertification _ _ openingD checkpointBytesD _ -> P.do
                                PPair view resumed <-
                                    pmatch $
                                        presumeOpenedFieldGrammarCertification
                                            # pfromData openingD
                                            # (pscriptWitnessAnchor # state)
                                            # pscriptWitnessesFieldIndex
                                            # grammarCheckpointHash
                                            # pfromData checkpointBytesD
                                            # pfromData ptxInfo'referenceInputs
                                            # certificatePolicy
                                pexpecting (pnot #$ pfieldGrammarIsComplete # resumed) $
                                    plet (pcertifyFieldGrammar # view # resumed # itemBudget) $ \next ->
                                        pexpecting (outputScriptHash #== ownScriptHash) $
                                            pexpecting
                                                ( outputStateData
                                                    #== pforgetData
                                                        (pdata $ pstateWithPhase # state # (pcon $ PGrammarCertification $ pdata $ pfieldGrammarCheckpointHash # next))
                                                )
                                                (pconstant True)
                            PStartSemanticScan _ _ openingD checkpointBytesD _ -> P.do
                                PPair view start <-
                                    pmatch $
                                        popenedCertifiedFieldWalkFromGrammar
                                            # pfromData openingD
                                            # (pscriptWitnessAnchor # state)
                                            # pscriptWitnessesFieldIndex
                                            # grammarCheckpointHash
                                            # pfromData checkpointBytesD
                                            # pfromData ptxInfo'referenceInputs
                                            # certificatePolicy
                                pexpecting (pwalkRemaining # start #> pdirectScriptWitnessLimit) $ P.do
                                    PStep05State{pstep05State'expectedMissingScriptHash} <- pmatch state
                                    PPair found next <-
                                        pmatch $
                                            pwalkFold @PBool
                                                # view
                                                # start
                                                # itemBudget
                                                # pconstant False
                                                # (pscanScriptWitness # pfromData pstep05State'expectedMissingScriptHash)
                                    pexpecting (pnot #$ pwalkIsComplete # next) $
                                        pexpecting (outputScriptHash #== step07ScriptHash) $
                                            pexpecting
                                                ( outputStateData
                                                    #== pforgetData
                                                        ( pdata $
                                                            pstateWithPhase
                                                                # state
                                                                # (pcon $ PSemanticScan (pdata $ pfieldWalkCheckpointHash # next) (pdata found))
                                                        )
                                                )
                                                (pconstant True)

missingNativeScriptUtxoStep07Validator ::
    forall s.
    Term
        s
        ( PAsData PCurrencySymbol
            :--> PAsData PCurrencySymbol
            :--> PAsData PAddress
            :--> PAsData PCurrencySymbol
            :--> PScriptContext
            :--> PUnit
        )
missingNativeScriptUtxoStep07Validator = plam $
    \computationThreadPolicy fraudProofPolicy fraudProofAddress certificatePolicy ctx ->
        pstep ctx $ \datum redeemer ownOutRef txInfo ->
            pdispatch @_ @PStep07Args computationThreadPolicy datum redeemer ownOutRef txInfo $ \action ->
                pmatch action $ \case
                    PResumeSemanticScan inputIndexD outputIndexD openingD checkpointBytesD itemBudgetD -> P.do
                        PTxInfo{ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs} <- pmatch txInfo
                        itemBudget <- plet $ pfromData itemBudgetD
                        pexpecting (pvalidStagedBudget # itemBudget)
                            $ pcontinue
                                computationThreadPolicy
                                (pexpectDatum datum)
                                (pfromData inputIndexD)
                                (pfromData outputIndexD)
                                ownOutRef
                                (pfromData ptxInfo'inputs)
                                (pfromData ptxInfo'outputs)
                            $ \ownScriptHash _threadName _prover inputState outputScriptHash outputStateData -> P.do
                                state <- plet $ pexpectStateAs @PStep05State inputState
                                PPair checkpointHash foundBefore <- pmatch $ psemanticPhaseOf # state
                                PPair view resumed <-
                                    pmatch $
                                        presumeOpenedFieldWalk
                                            # pfromData openingD
                                            # (pscriptWitnessAnchor # state)
                                            # pscriptWitnessesFieldIndex
                                            # checkpointHash
                                            # pfromData checkpointBytesD
                                            # pfromData ptxInfo'referenceInputs
                                            # certificatePolicy
                                PStep05State{pstep05State'expectedMissingScriptHash} <- pmatch state
                                PPair found next <-
                                    pmatch $
                                        pwalkFold @PBool
                                            # view
                                            # resumed
                                            # itemBudget
                                            # foundBefore
                                            # (pscanScriptWitness # pfromData pstep05State'expectedMissingScriptHash)
                                pexpecting (pnot #$ pwalkIsComplete # next) $
                                    pexpecting (outputScriptHash #== ownScriptHash) $
                                        pexpecting
                                            ( outputStateData
                                                #== pforgetData
                                                    ( pdata $
                                                        pstateWithPhase
                                                            # state
                                                            # (pcon $ PSemanticScan (pdata $ pfieldWalkCheckpointHash # next) (pdata found))
                                                    )
                                            )
                                            (pconstant True)
                    PFinalizeSemanticScan inputIndexD outputIndexD mintRedeemerIndexD openingD checkpointBytesD itemBudgetD -> P.do
                        PTxInfo{ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs, ptxInfo'redeemers} <- pmatch txInfo
                        itemBudget <- plet $ pfromData itemBudgetD
                        pexpecting (pvalidStagedBudget # itemBudget)
                            $ pfinalize
                                computationThreadPolicy
                                fraudProofPolicy
                                fraudProofAddress
                                (pexpectDatum datum)
                                (pfromData inputIndexD)
                                (pfromData outputIndexD)
                                (pfromData mintRedeemerIndexD)
                                ownOutRef
                                (pfromData ptxInfo'inputs)
                                (pfromData ptxInfo'outputs)
                                (pto $ pto $ pfromData ptxInfo'redeemers)
                            $ \_ownScriptHash _threadName _prover inputState -> P.do
                                state <- plet $ pexpectStateAs @PStep05State inputState
                                PPair checkpointHash foundBefore <- pmatch $ psemanticPhaseOf # state
                                PPair view resumed <-
                                    pmatch $
                                        presumeOpenedFieldWalk
                                            # pfromData openingD
                                            # (pscriptWitnessAnchor # state)
                                            # pscriptWitnessesFieldIndex
                                            # checkpointHash
                                            # pfromData checkpointBytesD
                                            # pfromData ptxInfo'referenceInputs
                                            # certificatePolicy
                                PStep05State{pstep05State'expectedMissingScriptHash} <- pmatch state
                                PPair found terminal <-
                                    pmatch $
                                        pwalkFold @PBool
                                            # view
                                            # resumed
                                            # itemBudget
                                            # foundBefore
                                            # (pscanScriptWitness # pfromData pstep05State'expectedMissingScriptHash)
                                pexpecting (pwalkIsComplete # terminal) $
                                    pexpecting (pnot # found) (pconstant True)

pscriptWitnessAnchor :: forall s. Term s (PStep05State :--> PNativeTxAnchorV1)
pscriptWitnessAnchor = phoistAcyclic $ plam $ \state ->
    pmatch state $ \PStep05State{pstep05State'badTxId, pstep05State'badTxWitnessSetHash} ->
        pcon $ PWitnessAnchor pstep05State'badTxId pstep05State'badTxWitnessSetHash

pscanScriptWitness :: forall s. Term s (PByteString :--> PBool :--> PInteger :--> PByteString :--> PBool)
pscanScriptWitness = phoistAcyclic $ plam $ \expectedHash found _index item -> P.do
    PPair _offset script <- pmatch $ pdecodeMidgardVersionedScriptAt # item # 0
    pexpecting (pencodeMidgardVersionedScript # script #== item) $
        found #|| (pversionedScriptHash # script #== expectedHash)

pvalidStagedBudget :: forall s. Term s (PInteger :--> PBool)
pvalidStagedBudget = phoistAcyclic $ plam $ \budget ->
    pexpecting (budget #> 0) $
        pexpecting (budget #<= pstagedScriptWitnessBatchLimit) (pconstant True)

pstateWithPhase :: forall s. Term s (PStep05State :--> PStep05PhaseV1 :--> PStep05State)
pstateWithPhase = phoistAcyclic $ plam $ \state phase ->
    pmatch state $ \PStep05State{pstep05State'expectedMissingScriptHash, pstep05State'badTxId, pstep05State'badTxWitnessSetHash} ->
        pcon $ PStep05State pstep05State'expectedMissingScriptHash pstep05State'badTxId pstep05State'badTxWitnessSetHash (pdata phase)

pgrammarCheckpointHashOf :: forall s. Term s (PStep05State :--> PByteString)
pgrammarCheckpointHashOf = phoistAcyclic $ plam $ \state ->
    pmatch state $ \PStep05State{pstep05State'phase} ->
        pmatch (pfromData pstep05State'phase) $ \case
            PGrammarCertification checkpointHashD -> pfromData checkpointHashD
            _ -> perror

psemanticPhaseOf :: forall s. Term s (PStep05State :--> PPair PByteString PBool)
psemanticPhaseOf = phoistAcyclic $ plam $ \state ->
    pmatch state $ \PStep05State{pstep05State'phase} ->
        pmatch (pfromData pstep05State'phase) $ \case
            PSemanticScan checkpointHashD foundD -> pcon $ PPair (pfromData checkpointHashD) (pfromData foundD)
            _ -> perror

pstep06InputIndex, pstep06OutputIndex, pstep06ItemBudget :: forall s. Term s (PStep06Args :--> PInteger)
pstep06InputIndex = phoistAcyclic $ plam $ \action -> pmatch action $ \case
    PResumeGrammarCertification inputIndexD _ _ _ _ -> pfromData inputIndexD
    PStartSemanticScan inputIndexD _ _ _ _ -> pfromData inputIndexD
pstep06OutputIndex = phoistAcyclic $ plam $ \action -> pmatch action $ \case
    PResumeGrammarCertification _ outputIndexD _ _ _ -> pfromData outputIndexD
    PStartSemanticScan _ outputIndexD _ _ _ -> pfromData outputIndexD
pstep06ItemBudget = phoistAcyclic $ plam $ \action -> pmatch action $ \case
    PResumeGrammarCertification _ _ _ _ itemBudgetD -> pfromData itemBudgetD
    PStartSemanticScan _ _ _ _ itemBudgetD -> pfromData itemBudgetD

pledgerOutrefKey :: forall s. Term s (PTxOutRef :--> PByteString)
pledgerOutrefKey = phoistAcyclic $ plam $ \outRef -> P.do
    PTxOutRef{ptxOutRef'id, ptxOutRef'idx} <- pmatch outRef
    pencodeMidgardTxInput
        # pcon (PMidgardTxInput (pdata $ pto $ pfromData ptxOutRef'id) ptxOutRef'idx)
