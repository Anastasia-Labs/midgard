module Midgard.Validators.FraudProofs.ExecutionNativeScriptInvalid (
    executionNativeScriptInvalidStep01Validator,
    executionNativeScriptInvalidStep02Validator,
    executionNativeScriptInvalidAcceptedReconstructionInitValidator,
    executionNativeScriptInvalidAcceptedSpendPrefixValidator,
    executionNativeScriptInvalidAcceptedMintPrefixValidator,
    executionNativeScriptInvalidAcceptedObserverPrefixValidator,
    executionNativeScriptInvalidAcceptedReceivePrefixValidator,
    executionNativeScriptInvalidAcceptedInlineSourceValidator,
    executionNativeScriptInvalidAcceptedReferenceSourceValidator,
    executionNativeScriptInvalidStep03Validator,
    executionNativeScriptInvalidStep04Validator,
    executionNativeScriptInvalidStep05Validator,
    executionNativeScriptInvalidStep06Validator,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Builtin.Crypto (pblake2b_224, pblake2b_256)
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.LedgerApi.V3 (PAddress, PCurrencySymbol, PScriptContext, PScriptHash, PTxInInfo, PTxInfo (..))
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Repr.Scott (DeriveAsScottRec (..))
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.BoundedItem qualified as Bounded
import Midgard.FraudProofs.Common (pcontinue, pfinalize, ppassNativeTxToNextStepCarried, pverifyMembershipCarried)
import Midgard.FraudProofs.ExecutionNativeScriptInvalid
import Midgard.FraudProofs.ExecutionNativeScriptInvalid.AcceptedReconstruction (PAcceptedReconstructionStateV1 (..))
import Midgard.FraudProofs.ExecutionNativeScriptInvalid.AcceptedReconstruction qualified as Accepted
import Midgard.FraudProofs.ExecutionNativeScriptInvalid.Types
import Midgard.FraudProofs.FieldOpening (
    PFieldOpeningV1,
    PNativeTxAnchorV1 (PBodyAnchor, PWitnessAnchor),
    paddressWitnessesFieldIndex,
    pfoldOpenedField,
    pmintFieldIndex,
    popenedFieldView,
    popenedFieldWalk,
    poutputsFieldIndex,
    preferenceInputsFieldIndex,
    prequiredObserversFieldIndex,
    presumeOpenedFieldWalk,
    pscriptWitnessesFieldIndex,
    pspendInputsFieldIndex,
 )
import Midgard.FraudProofs.MintAuthorization.Engine (PNativeScriptVerdictV1 (PScriptEvaluatedV1), pevaluateNativeScriptV1)
import Midgard.FraudProofs.NativeTx.Compact (pdecodeNativeTxCompactV1, pencodeNativeTxCompactV1, pverifyNativeTxProofSourceV1)
import Midgard.FraudProofs.NativeTx.Components (pdecodeMidgardAddressWitnessCbor, pdecodeMidgardTxOutputCbor, pdecodeMidgardVersionedScriptAt, pencodeMidgardAddressWitness, pencodeMidgardVersionedScript)
import Midgard.FraudProofs.NativeTx.Preimages (pdecodeMintPolicyItemCbor)
import Midgard.FraudProofs.NativeTx.Types (
    PMidgardAddress (..),
    PMidgardAddressWitness (..),
    PMidgardCredential (..),
    PMidgardScriptLanguage (PNativeCardanoScript),
    PMidgardTxInput (..),
    PMidgardTxOutput (..),
    PMidgardVersionedScript (..),
    PNativeTxBodyCompact (..),
    PNativeTxCompact (..),
    PVerifiedMidgardNativeTxCompact (..),
 )
import Midgard.FraudProofs.ProofThreadSubstrate qualified as Subject
import Midgard.FraudProofs.TransitionTrace.Proof qualified as Proof
import Midgard.LedgerOutput (pdecodeCanonicalAddressBytes)
import Midgard.LedgerOutputCommitment (PLedgerOutputCommitmentV1 (..), pdecodeLedgerOutputCommitment)
import Midgard.LedgerState (PForcedInclusionTxV1 (..), PHeaderV1 (..), PNativeTxProofSourceV1 (..))
import Midgard.NativeTxFieldAccess (PFieldViewV1, pfieldItemAt, pfieldItemCount)
import Midgard.NativeTxMachineWalk (PFieldWalkCheckpointV1, pfieldWalkCheckpointHash, pspendInputAt, pwalkFold, pwalkIsComplete)
import Midgard.NativeTxScriptPushdown (PNativeScriptWalkV1, pnativeScriptCursorHash, pnativeScriptRunWithSignerVerdict, pnativeScriptVerdict, pnativeScriptWalkIsComplete, popenNativeScriptWalk, presumeNativeScriptWalkFromCommitment)
import Midgard.ScriptProof qualified as ScriptProof
import Midgard.TransitionTrace (PRootMembershipProof (..))
import Midgard.ValidationMachine (PSignerSetProofV1 (..))
import Midgard.ValidationMerkle (PFrontierPeak, pappendLeaf, pemptyFrontier, pfrontierCommitment, pfrontierIsWellFormed, pverifyMembership)
import Midgard.Validators.FraudProofs.Step (pdispatch, pexpectDatum, pexpectStateAs, pexpecting, pstep)

data PExecutionSignerAccumulatorV1 (s :: S) = PExecutionSignerAccumulatorV1
    { pexecutionSignerAccumulator'previousSignerHash :: Term s PByteString
    , pexecutionSignerAccumulator'signerCount :: Term s PInteger
    , pexecutionSignerAccumulator'signerPeaks :: Term s (PBuiltinList (PAsData PFrontierPeak))
    }
    deriving stock (Generic)
    deriving anyclass (SOP.Generic)
    deriving (PlutusType) via (DeriveAsScottRec PExecutionSignerAccumulatorV1)

executionNativeScriptInvalidStep01Validator :: forall s. Term s (PAsData PScriptHash :--> PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PAsData PScriptHash :--> PScriptContext :--> PUnit)
executionNativeScriptInvalidStep01Validator = plam $ \acceptedInitHash forcedStep02Hash threadPolicy hubOracle ctx ->
    pstep ctx $ \datum redeemer ownRef tx -> pdispatch @_ @PStep01Args threadPolicy datum redeemer ownRef tx $ \args -> P.do
        PStep01Args{pstep01Args'source, pstep01Args'executionIndex} <- pmatch args
        PTxInfo{ptxInfo'inputs, ptxInfo'outputs, ptxInfo'referenceInputs, ptxInfo'redeemers} <- pmatch tx
        pmatch (pfromData pstep01Args'source) $ \case
            PAcceptedSource inclusion ->
                ppassNativeTxToNextStepCarried threadPolicy hubOracle datum (pfromData inclusion) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'referenceInputs) (pfromData ptxInfo'outputs) (pto $ pto $ pfromData ptxInfo'redeemers) $ \_ _ _ _ outputHash outputState header _ verified -> P.do
                    PHeaderV1{pheader'prevUtxosRoot, pheader'validationTracesRoot, pheader'validationTraceCount} <- pmatch $ pfromData header
                    PVerifiedMidgardNativeTxCompact{pverified'txCompact} <- pmatch verified
                    let expected =
                            pbindExecutionV1
                                # (Subject.pbindAcceptedSubject # verified)
                                # pfromData pheader'validationTracesRoot
                                # pfromData pheader'validationTraceCount
                                # pfromData pstep01Args'executionIndex
                                # pfromData pheader'prevUtxosRoot
                                # (pencodeNativeTxCompactV1 # pverified'txCompact)
                    outputHash #== acceptedInitHash #&& outputState #== pforgetData (pdata expected)
            PForcedSource inputIndex outputIndex header membership direction ->
                pcontinue threadPolicy (pexpectDatum datum) (pfromData inputIndex) (pfromData outputIndex) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) $ \_ threadName _ _ outputHash outputState -> P.do
                    subject <- plet $ Subject.pbindForcedSubjectToThread # pto (pfromData threadName) # pfromData header # pfromData membership # pfromData direction
                    Subject.PVerdictSubject{Subject.psubject'transactionId} <- pmatch subject
                    PRootMembershipProof{prootMembership'value} <- pmatch $ pfromData membership
                    PForcedInclusionTxV1{pforcedTx'txId, pforcedTx'source} <- pmatch $ pfromData $ punsafeCoerce @(PAsData PForcedInclusionTxV1) prootMembership'value
                    PNativeTxProofSourceV1{..} <- pmatch $ pfromData pforcedTx'source
                    PPair verified _ <- pmatch $ pverifyNativeTxProofSourceV1 # pfromData pforcedTx'txId # pfromData pnativeSource'compactCbor # pfromData pnativeSource'witnessSetCompactCbor # pfromData pnativeSource'fieldPreimageLengthsCbor
                    PVerifiedMidgardNativeTxCompact{pverified'txId, pverified'txCompact} <- pmatch verified
                    PHeaderV1{pheader'prevUtxosRoot, pheader'validationTracesRoot, pheader'validationTraceCount} <- pmatch $ pfromData header
                    let expected =
                            pbindExecutionV1
                                # subject
                                # pfromData pheader'validationTracesRoot
                                # pfromData pheader'validationTraceCount
                                # pfromData pstep01Args'executionIndex
                                # pfromData pheader'prevUtxosRoot
                                # (pencodeNativeTxCompactV1 # pverified'txCompact)
                    pverified'txId #== pfromData psubject'transactionId #&& outputHash #== forcedStep02Hash #&& outputState #== pforgetData (pdata expected)

executionNativeScriptInvalidStep02Validator :: forall s. Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
executionNativeScriptInvalidStep02Validator = plam $ \step03Hash threadPolicy ctx ->
    pstep ctx $ \datum redeemer ownRef tx -> pdispatch @_ @PStep02Args threadPolicy datum redeemer ownRef tx $ \args -> P.do
        PStep02Args{..} <- pmatch args
        PTxInfo{ptxInfo'inputs, ptxInfo'outputs} <- pmatch tx
        pcontinue threadPolicy (pexpectDatum datum) (pfromData pstep02Args'inputIndex) (pfromData pstep02Args'outputIndex) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) $ \_ _ _ inputState outputHash outputState ->
            let expected =
                    pauthenticateExecutionSourceV1
                        # pexpectStateAs @PBoundExecutionV1 inputState
                        # pfromData pstep02Args'traceMembership
                        # pfromData pstep02Args'machineState
                        # pfromData pstep02Args'traceProof
                        # pfromData pstep02Args'control
                        # pfromData pstep02Args'purposeKind
                        # pfromData pstep02Args'purposeIndex
                        # pfromData pstep02Args'scriptHash
                        # pfromData pstep02Args'purposeSubject
                        # pfromData pstep02Args'purposeSiblings
                        # pfromData pstep02Args'sourceIndex
                        # pfromData pstep02Args'originKind
                        # pfromData pstep02Args'sourceKey
                        # pfromData pstep02Args'languageTag
                        # pfromData pstep02Args'totalLength
                        # pfromData pstep02Args'itemCommitment
                        # pfromData pstep02Args'sourceSiblings
                        # pfromData pstep02Args'redeemerLeaf
                        # pfromData pstep02Args'executionSiblings
             in outputHash #== step03Hash #&& outputState #== pforgetData (pdata expected)

executionNativeScriptInvalidStep03Validator :: forall s. Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
executionNativeScriptInvalidStep03Validator = plam $ \step04Hash threadPolicy ctx ->
    pstep ctx $ \datum redeemer ownRef tx -> pdispatch @_ @PExecutionNativeStep03Args threadPolicy datum redeemer ownRef tx $ \args -> P.do
        PExecutionNativeStep03Args{..} <- pmatch args
        PTxInfo{ptxInfo'inputs, ptxInfo'outputs} <- pmatch tx
        pcontinue threadPolicy (pexpectDatum datum) (pfromData pexecutionStep03Args'inputIndex) (pfromData pexecutionStep03Args'outputIndex) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) $ \_ _ _ inputState outputHash outputState -> P.do
            source <- plet $ pexpectStateAs @PAuthenticatedExecutionSourceV1 inputState
            PAuthenticatedExecutionSourceV1{..} <- pmatch source
            PBoundExecutionV1{pboundExecution'subject, pboundExecution'executionIndex} <- pmatch $ pfromData pauthenticatedSource'bound
            Subject.PVerdictSubject{Subject.psubject'direction, Subject.psubject'transactionId} <- pmatch $ pfromData pboundExecution'subject
            compact <- plet $ pdecodeNativeTxCompactV1 # pfromData pauthenticatedSource'compactCbor
            PNativeTxCompact{pcompact'body, pcompact'witnessSetHash} <- pmatch compact
            PNativeTxBodyCompact{pbodyCompact'validityIntervalStart, pbodyCompact'validityIntervalEnd} <- pmatch pcompact'body
            PBuiltinPair fieldIndex itemIndex <- pmatch $ psourceItemCoordinate # source
            PPair offset script <- pmatch $ pdecodeMidgardVersionedScriptAt # pfromData pexecutionStep03Args'scriptItemCbor # 0
            PMidgardVersionedScript{pversionedScript'language} <- pmatch script
            let expected =
                    pcon $
                        PExecutionNativeStep04State
                            psubject'direction
                            pboundExecution'executionIndex
                            pauthenticatedSource'sourceIndex
                            pauthenticatedSource'originKind
                            pauthenticatedSource'itemCommitment
                            psubject'transactionId
                            (pdata pcompact'witnessSetHash)
                            (pdata $ pblake2b_256 # pfromData pexecutionStep03Args'scriptItemCbor)
                            (pdata pbodyCompact'validityIntervalStart)
                            (pdata pbodyCompact'validityIntervalEnd)
                scriptItem = pfromData pexecutionStep03Args'scriptItemCbor
                itemMatches = Bounded.pfromBytes # pfromData fieldIndex # pfromData itemIndex # scriptItem #== pfromData pauthenticatedSource'itemCommitment
                scriptCanonical = offset #== plengthBS # scriptItem #&& pencodeMidgardVersionedScript # script #== scriptItem
            pif
                (itemMatches #&& scriptCanonical #&& pfromData pversionedScript'language #== pcon PNativeCardanoScript)
                (outputHash #== step04Hash #&& outputState #== pforgetData (pdata expected))
                perror

executionNativeScriptInvalidStep04Validator :: forall s. Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PAsData PCurrencySymbol :--> PAsData PAddress :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
executionNativeScriptInvalidStep04Validator = plam $ \step05Hash threadPolicy fraudProofPolicy fraudProofAddress certificatePolicy ctx ->
    pstep ctx $ \datum redeemer ownRef tx -> pdispatch @_ @PExecutionNativeStep04Args threadPolicy datum redeemer ownRef tx $ \action ->
        pmatch action $ \case
            PExecutionNativeDirectFinalize inputIndex outputIndex mintRedeemerIndex scriptItem opening -> P.do
                PTxInfo{ptxInfo'inputs, ptxInfo'outputs, ptxInfo'referenceInputs, ptxInfo'redeemers} <- pmatch tx
                pfinalize threadPolicy fraudProofPolicy fraudProofAddress (pexpectDatum datum) (pfromData inputIndex) (pfromData outputIndex) (pfromData mintRedeemerIndex) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) (pto $ pto $ pfromData ptxInfo'redeemers) $ \_ _ _ inputState -> P.do
                    state <- plet $ pexpectStateAs @PExecutionNativeStep04State inputState
                    PExecutionNativeStep04State{pexecutionStep04State'direction, pexecutionStep04State'validityIntervalStart, pexecutionStep04State'validityIntervalEnd} <- pmatch state
                    scriptBytes <- plet $ pexecutionNativeScriptBytes # state # pfromData scriptItem
                    addressWalk <- plet $ popenedExecutionAddressWitnesses # state # pfromData opening # pfromData ptxInfo'referenceInputs # certificatePolicy
                    PPair addressView _ <- pmatch addressWalk
                    signerHashes <- plet $ pfoldOpenedField @(PBuiltinList PByteString) # addressWalk # pnil # plam (\acc _ item -> pcons # (pexecutionSignerHash # item) # acc)
                    pif
                        (plengthBS # scriptBytes #<= 1024 #&& pfieldItemCount # addressView #<= 28)
                        ( pmatch (pevaluateNativeScriptV1 # scriptBytes # signerHashes # pfromData pexecutionStep04State'validityIntervalStart # pfromData pexecutionStep04State'validityIntervalEnd) $ \case
                            PScriptEvaluatedV1 satisfied -> pfromData satisfied #== (pfromData pexecutionStep04State'direction #== 1)
                            _ -> perror
                        )
                        perror
            PExecutionNativeStartSignerScan inputIndex outputIndex scriptItem opening itemBudget -> P.do
                PTxInfo{ptxInfo'inputs, ptxInfo'outputs, ptxInfo'referenceInputs} <- pmatch tx
                let budget = pfromData itemBudget
                pif
                    (budget #> 0 #&& budget #<= 16)
                    ( pcontinue threadPolicy (pexpectDatum datum) (pfromData inputIndex) (pfromData outputIndex) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) $ \_ _ _ inputState outputHash outputState -> P.do
                        state <- plet $ pexpectStateAs @PExecutionNativeStep04State inputState
                        PExecutionNativeStep04State{..} <- pmatch state
                        scriptBytes <- plet $ pexecutionNativeScriptBytes # state # pfromData scriptItem
                        addressWalk <- plet $ popenedExecutionAddressWitnesses # state # pfromData opening # pfromData ptxInfo'referenceInputs # certificatePolicy
                        PPair addressView start <- pmatch addressWalk
                        PPair accumulator next <- pmatch $ pwalkFold @PExecutionSignerAccumulatorV1 # addressView # start # budget # pcon (PExecutionSignerAccumulatorV1 (pconstant "") 0 pemptyFrontier) # plam (\acc _ item -> pappendExecutionSigner # acc # (pexecutionSignerHash # item))
                        PExecutionSignerAccumulatorV1 previousSigner signerCount signerPeaks <- pmatch accumulator
                        let expected =
                                pcon $
                                    PExecutionNativeStep05State
                                        pexecutionStep04State'direction
                                        pexecutionStep04State'executionIndex
                                        pexecutionStep04State'sourceIndex
                                        pexecutionStep04State'originKind
                                        pexecutionStep04State'itemCommitment
                                        pexecutionStep04State'badTxId
                                        pexecutionStep04State'badTxWitnessSetHash
                                        pexecutionStep04State'scriptItemHash
                                        pexecutionStep04State'validityIntervalStart
                                        pexecutionStep04State'validityIntervalEnd
                                        (pdata $ pfieldWalkCheckpointHash # next)
                                        (pdata previousSigner)
                                        (pdata signerCount)
                                        (pdata signerPeaks)
                        pif
                            (pfieldItemCount # addressView #> 28 #|| plengthBS # scriptBytes #> 1024)
                            (outputHash #== step05Hash #&& outputState #== pforgetData (pdata expected))
                            perror
                    )
                    perror

popenedExecutionAddressWitnesses :: forall s. Term s (PExecutionNativeStep04State :--> PFieldOpeningV1 :--> PBuiltinList (PAsData PTxInInfo) :--> PAsData PCurrencySymbol :--> PPair PFieldViewV1 PFieldWalkCheckpointV1)
popenedExecutionAddressWitnesses = phoistAcyclic $ plam $ \state opening references certificatePolicy ->
    pmatch state $ \PExecutionNativeStep04State{pexecutionStep04State'badTxId, pexecutionStep04State'badTxWitnessSetHash} ->
        popenedFieldWalk # opening # pcon (PWitnessAnchor pexecutionStep04State'badTxId pexecutionStep04State'badTxWitnessSetHash) # paddressWitnessesFieldIndex # references # certificatePolicy

pexecutionNativeScriptBytes :: forall s. Term s (PExecutionNativeStep04State :--> PByteString :--> PByteString)
pexecutionNativeScriptBytes = phoistAcyclic $ plam $ \state item -> P.do
    PExecutionNativeStep04State{pexecutionStep04State'scriptItemHash} <- pmatch state
    PPair offset script <- pmatch $ pdecodeMidgardVersionedScriptAt # item # 0
    PMidgardVersionedScript{pversionedScript'language, pversionedScript'scriptBytes} <- pmatch script
    pif
        (pblake2b_256 # item #== pfromData pexecutionStep04State'scriptItemHash #&& offset #== plengthBS # item #&& pencodeMidgardVersionedScript # script #== item #&& pfromData pversionedScript'language #== pcon PNativeCardanoScript)
        (pfromData pversionedScript'scriptBytes)
        perror

pexecutionSignerHash :: forall s. Term s (PByteString :--> PByteString)
pexecutionSignerHash = phoistAcyclic $ plam $ \item -> P.do
    witness <- plet $ pdecodeMidgardAddressWitnessCbor # item
    PMidgardAddressWitness{paddressWitness'verificationKey} <- pmatch witness
    pif (pencodeMidgardAddressWitness # witness #== item) (pblake2b_224 # pfromData paddressWitness'verificationKey) perror

pappendExecutionSigner :: forall s. Term s (PExecutionSignerAccumulatorV1 :--> PByteString :--> PExecutionSignerAccumulatorV1)
pappendExecutionSigner = phoistAcyclic $ plam $ \accumulator signerHash ->
    pmatch accumulator $ \(PExecutionSignerAccumulatorV1 previous count peaks) ->
        pif
            (previous #== pconstant "" #|| previous #<= signerHash)
            (pif (previous #== signerHash) accumulator (pcon $ PExecutionSignerAccumulatorV1 signerHash (count + 1) (pappendLeaf # count # peaks # (ScriptProof.psignerLeafHash # signerHash))))
            perror

executionNativeScriptInvalidStep05Validator :: forall s. Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
executionNativeScriptInvalidStep05Validator = plam $ \step06Hash threadPolicy certificatePolicy ctx ->
    pstep ctx $ \datum redeemer ownRef tx -> pdispatch @_ @PExecutionNativeStep05Args threadPolicy datum redeemer ownRef tx $ \action -> P.do
        PTxInfo{ptxInfo'inputs, ptxInfo'outputs, ptxInfo'referenceInputs} <- pmatch tx
        let inputIndex = pmatch action $ \case PExecutionNativeResumeSignerScan input _ _ _ _ -> pfromData input; PExecutionNativeFinalizeSignerScan input _ _ _ _ -> pfromData input
            outputIndex = pmatch action $ \case PExecutionNativeResumeSignerScan _ output _ _ _ -> pfromData output; PExecutionNativeFinalizeSignerScan _ output _ _ _ -> pfromData output
            opening = pmatch action $ \case PExecutionNativeResumeSignerScan _ _ value _ _ -> pfromData value; PExecutionNativeFinalizeSignerScan _ _ value _ _ -> pfromData value
            checkpoint = pmatch action $ \case PExecutionNativeResumeSignerScan _ _ _ value _ -> pfromData value; PExecutionNativeFinalizeSignerScan _ _ _ value _ -> pfromData value
            budget = pmatch action $ \case PExecutionNativeResumeSignerScan _ _ _ _ value -> pfromData value; PExecutionNativeFinalizeSignerScan _ _ _ _ value -> pfromData value
        pcontinue threadPolicy (pexpectDatum datum) inputIndex outputIndex ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) $ \ownHash _ _ inputState outputHash outputState -> P.do
            state <- plet $ pexpectStateAs @PExecutionNativeStep05State inputState
            PExecutionNativeStep05State{..} <- pmatch state
            PPair view resumed <- pmatch $ presumeOpenedFieldWalk # opening # pcon (PWitnessAnchor pexecutionStep05State'badTxId pexecutionStep05State'badTxWitnessSetHash) # paddressWitnessesFieldIndex # pfromData pexecutionStep05State'signerCheckpointHash # checkpoint # pfromData ptxInfo'referenceInputs # certificatePolicy
            PPair accumulator next <- pmatch $ pwalkFold @PExecutionSignerAccumulatorV1 # view # resumed # budget # pcon (PExecutionSignerAccumulatorV1 (pfromData pexecutionStep05State'previousSignerHash) (pfromData pexecutionStep05State'signerCount) (pfromData pexecutionStep05State'signerPeaks)) # plam (\acc _ item -> pappendExecutionSigner # acc # (pexecutionSignerHash # item))
            PExecutionSignerAccumulatorV1 previousSigner signerCount signerPeaks <- pmatch accumulator
            pmatch action $ \case
                PExecutionNativeResumeSignerScan{} ->
                    let expected =
                            pcon $
                                PExecutionNativeStep05State
                                    pexecutionStep05State'direction
                                    pexecutionStep05State'executionIndex
                                    pexecutionStep05State'sourceIndex
                                    pexecutionStep05State'originKind
                                    pexecutionStep05State'itemCommitment
                                    pexecutionStep05State'badTxId
                                    pexecutionStep05State'badTxWitnessSetHash
                                    pexecutionStep05State'scriptItemHash
                                    pexecutionStep05State'validityIntervalStart
                                    pexecutionStep05State'validityIntervalEnd
                                    (pdata $ pfieldWalkCheckpointHash # next)
                                    (pdata previousSigner)
                                    (pdata signerCount)
                                    (pdata signerPeaks)
                     in pif
                            (budget #> 0 #&& budget #<= 16 #&& pnot # (pwalkIsComplete # next))
                            (outputHash #== ownHash #&& outputState #== pforgetData (pdata expected))
                            perror
                PExecutionNativeFinalizeSignerScan{} ->
                    let expected =
                            pcon $
                                PExecutionNativeStep06State
                                    pexecutionStep05State'direction
                                    pexecutionStep05State'executionIndex
                                    pexecutionStep05State'sourceIndex
                                    pexecutionStep05State'originKind
                                    pexecutionStep05State'itemCommitment
                                    pexecutionStep05State'badTxId
                                    pexecutionStep05State'scriptItemHash
                                    pexecutionStep05State'validityIntervalStart
                                    pexecutionStep05State'validityIntervalEnd
                                    (pdata signerCount)
                                    (pdata signerPeaks)
                                    (pdata $ pcon PExecutionNativeScriptReady)
                     in pif
                            (budget #>= 0 #&& budget #<= 16 #&& pwalkIsComplete # next)
                            (outputHash #== step06Hash #&& outputState #== pforgetData (pdata expected))
                            perror

executionNativeScriptInvalidStep06Validator :: forall s. Term s (PAsData PCurrencySymbol :--> PAsData PCurrencySymbol :--> PAsData PAddress :--> PScriptContext :--> PUnit)
executionNativeScriptInvalidStep06Validator = plam $ \threadPolicy fraudProofPolicy fraudProofAddress ctx ->
    pstep ctx $ \datum redeemer ownRef tx -> pdispatch @_ @PExecutionNativeStep06Args threadPolicy datum redeemer ownRef tx $ \action ->
        pif
            (pexecutionStep06IsFinalizing # action)
            ( P.do
                PTxInfo{ptxInfo'inputs, ptxInfo'outputs, ptxInfo'redeemers} <- pmatch tx
                pfinalize threadPolicy fraudProofPolicy fraudProofAddress (pexpectDatum datum) (pexecutionStep06InputIndex # action) (pexecutionStep06OutputIndex # action) (pexecutionStep06MintRedeemerIndex # action) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) (pto $ pto $ pfromData ptxInfo'redeemers) $ \_ _ _ inputState -> P.do
                    state <- plet $ pexpectStateAs @PExecutionNativeStep06State inputState
                    PExecutionNativeStep06State{pexecutionStep06State'direction} <- pmatch state
                    terminal <- plet $ prunExecutionStep06Action # state # action
                    pif
                        (pnativeScriptWalkIsComplete # terminal)
                        ( pmatch (pnativeScriptVerdict # terminal) $ \case
                            PJust verdict -> verdict #== (pfromData pexecutionStep06State'direction #== 1)
                            PNothing -> perror
                        )
                        perror
            )
            ( P.do
                PTxInfo{ptxInfo'inputs, ptxInfo'outputs} <- pmatch tx
                pcontinue threadPolicy (pexpectDatum datum) (pexecutionStep06InputIndex # action) (pexecutionStep06OutputIndex # action) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) $ \ownHash _ _ inputState outputHash outputState -> P.do
                    state <- plet $ pexpectStateAs @PExecutionNativeStep06State inputState
                    PExecutionNativeStep06State{..} <- pmatch state
                    next <- plet $ prunExecutionStep06Action # state # action
                    let expected =
                            pcon $
                                PExecutionNativeStep06State
                                    pexecutionStep06State'direction
                                    pexecutionStep06State'executionIndex
                                    pexecutionStep06State'sourceIndex
                                    pexecutionStep06State'originKind
                                    pexecutionStep06State'itemCommitment
                                    pexecutionStep06State'badTxId
                                    pexecutionStep06State'scriptItemHash
                                    pexecutionStep06State'validityIntervalStart
                                    pexecutionStep06State'validityIntervalEnd
                                    pexecutionStep06State'signerCount
                                    pexecutionStep06State'signerPeaks
                                    (pdata $ pcon $ PExecutionNativeScriptWalk $ pdata $ pnativeScriptCursorHash # next)
                    pif
                        (pnot # (pnativeScriptWalkIsComplete # next))
                        (outputHash #== ownHash #&& outputState #== pforgetData (pdata expected))
                        perror
            )

prunExecutionStep06Action :: forall s. Term s (PExecutionNativeStep06State :--> PExecutionNativeStep06Args :--> PNativeScriptWalkV1)
prunExecutionStep06Action = phoistAcyclic $ plam $ \state action -> P.do
    PExecutionNativeStep06State{..} <- pmatch state
    item <- plet $ pexecutionStep06ScriptItem # action
    PPair offset script <- pmatch $ pdecodeMidgardVersionedScriptAt # item # 0
    PMidgardVersionedScript{pversionedScript'language, pversionedScript'scriptBytes} <- pmatch script
    scriptBytes <- plet $ pfromData pversionedScript'scriptBytes
    nodeBudget <- plet $ pexecutionStep06NodeBudget # action
    start <- plet $ pmatch action $ \case
        PExecutionNativeStartScriptScan{} -> pif (pfromData pexecutionStep06State'phase #== pcon PExecutionNativeScriptReady) (popenNativeScriptWalk # scriptBytes) perror
        PExecutionNativeStartScriptFinalize{} -> pif (pfromData pexecutionStep06State'phase #== pcon PExecutionNativeScriptReady) (popenNativeScriptWalk # scriptBytes) perror
        PExecutionNativeResumeScriptScan _ _ _ cursor frames _ _ -> pmatch (pfromData pexecutionStep06State'phase) $ \case
            PExecutionNativeScriptWalk cursorHash -> presumeNativeScriptWalkFromCommitment # pfromData cursorHash # pfromData cursor # pfromData frames # scriptBytes
            PExecutionNativeScriptReady -> perror
        PExecutionNativeFinalizeScriptScan _ _ _ _ cursor frames _ _ -> pmatch (pfromData pexecutionStep06State'phase) $ \case
            PExecutionNativeScriptWalk cursorHash -> presumeNativeScriptWalkFromCommitment # pfromData cursorHash # pfromData cursor # pfromData frames # scriptBytes
            PExecutionNativeScriptReady -> perror
    pexpecting (pblake2b_256 # item #== pfromData pexecutionStep06State'scriptItemHash) $
        pexpecting (offset #== plengthBS # item) $
            pexpecting (pencodeMidgardVersionedScript # script #== item) $
                pexpecting (pfromData pversionedScript'language #== pcon PNativeCardanoScript) $
                    pexpecting (nodeBudget #> 0) $
                        pexpecting (nodeBudget #<= 16) $
                            pnativeScriptRunWithSignerVerdict
                                # start
                                # scriptBytes
                                # pfromData pexecutionStep06State'validityIntervalStart
                                # pfromData pexecutionStep06State'validityIntervalEnd
                                # nodeBudget
                                # ( pexecutionAuthenticatedSignerVerdict
                                        # pfromData pexecutionStep06State'signerCount
                                        # pfromData pexecutionStep06State'signerPeaks
                                        # (pexecutionStep06SignerQueries # action)
                                  )

pexecutionStep06InputIndex :: forall s. Term s (PExecutionNativeStep06Args :--> PInteger)
pexecutionStep06InputIndex = phoistAcyclic $ plam $ \action -> pmatch action $ \case
    PExecutionNativeStartScriptScan input _ _ _ _ -> pfromData input
    PExecutionNativeResumeScriptScan input _ _ _ _ _ _ -> pfromData input
    PExecutionNativeStartScriptFinalize input _ _ _ _ _ -> pfromData input
    PExecutionNativeFinalizeScriptScan input _ _ _ _ _ _ _ -> pfromData input

pexecutionStep06OutputIndex :: forall s. Term s (PExecutionNativeStep06Args :--> PInteger)
pexecutionStep06OutputIndex = phoistAcyclic $ plam $ \action -> pmatch action $ \case
    PExecutionNativeStartScriptScan _ output _ _ _ -> pfromData output
    PExecutionNativeResumeScriptScan _ output _ _ _ _ _ -> pfromData output
    PExecutionNativeStartScriptFinalize _ output _ _ _ _ -> pfromData output
    PExecutionNativeFinalizeScriptScan _ output _ _ _ _ _ _ -> pfromData output

pexecutionStep06MintRedeemerIndex :: forall s. Term s (PExecutionNativeStep06Args :--> PInteger)
pexecutionStep06MintRedeemerIndex = phoistAcyclic $ plam $ \action -> pmatch action $ \case
    PExecutionNativeStartScriptFinalize _ _ mintIndex _ _ _ -> pfromData mintIndex
    PExecutionNativeFinalizeScriptScan _ _ mintIndex _ _ _ _ _ -> pfromData mintIndex
    _ -> perror

pexecutionStep06ScriptItem :: forall s. Term s (PExecutionNativeStep06Args :--> PByteString)
pexecutionStep06ScriptItem = phoistAcyclic $ plam $ \action -> pmatch action $ \case
    PExecutionNativeStartScriptScan _ _ item _ _ -> pfromData item
    PExecutionNativeResumeScriptScan _ _ item _ _ _ _ -> pfromData item
    PExecutionNativeStartScriptFinalize _ _ _ item _ _ -> pfromData item
    PExecutionNativeFinalizeScriptScan _ _ _ item _ _ _ _ -> pfromData item

pexecutionStep06NodeBudget :: forall s. Term s (PExecutionNativeStep06Args :--> PInteger)
pexecutionStep06NodeBudget = phoistAcyclic $ plam $ \action -> pmatch action $ \case
    PExecutionNativeStartScriptScan _ _ _ budget _ -> pfromData budget
    PExecutionNativeResumeScriptScan _ _ _ _ _ budget _ -> pfromData budget
    PExecutionNativeStartScriptFinalize _ _ _ _ budget _ -> pfromData budget
    PExecutionNativeFinalizeScriptScan _ _ _ _ _ _ budget _ -> pfromData budget

pexecutionStep06SignerQueries :: forall s. Term s (PExecutionNativeStep06Args :--> PBuiltinList (PAsData PExecutionNativeSignerQueryV1))
pexecutionStep06SignerQueries = phoistAcyclic $ plam $ \action -> pmatch action $ \case
    PExecutionNativeStartScriptScan _ _ _ _ queries -> pfromData queries
    PExecutionNativeResumeScriptScan _ _ _ _ _ _ queries -> pfromData queries
    PExecutionNativeStartScriptFinalize _ _ _ _ _ queries -> pfromData queries
    PExecutionNativeFinalizeScriptScan _ _ _ _ _ _ _ queries -> pfromData queries

pexecutionStep06IsFinalizing :: forall s. Term s (PExecutionNativeStep06Args :--> PBool)
pexecutionStep06IsFinalizing = phoistAcyclic $ plam $ \action -> pmatch action $ \case
    PExecutionNativeStartScriptFinalize{} -> pconstant True
    PExecutionNativeFinalizeScriptScan{} -> pconstant True
    _ -> pconstant False

pexecutionAuthenticatedSignerVerdict :: forall s. Term s (PInteger :--> PBuiltinList (PAsData PFrontierPeak) :--> PBuiltinList (PAsData PExecutionNativeSignerQueryV1) :--> PByteString :--> PBool)
pexecutionAuthenticatedSignerVerdict = phoistAcyclic $ pfix $ \self -> plam $ \count peaks queries signerHash ->
    pif
        (plengthBS # signerHash #== 28)
        ( pelimList
            (\query rest -> pmatch (pfromData query) $ \(PExecutionNativeSignerQueryV1 queryHash proof) -> pif (pfromData queryHash #== signerHash) (pexecutionSignerProofVerdict # signerHash # count # peaks # pfromData proof) (self # count # peaks # rest # signerHash))
            perror
            queries
        )
        perror

pexecutionSignerFrontierMatches :: forall s. Term s (PInteger :--> PByteString :--> PBuiltinList (PAsData PFrontierPeak) :--> PBool)
pexecutionSignerFrontierMatches = phoistAcyclic $ plam $ \count commitment peaks -> pfrontierIsWellFormed # count # peaks #&& pfrontierCommitment # count # peaks #== commitment

pexecutionSignerMembershipIsValid :: forall s. Term s (PByteString :--> PInteger :--> PByteString :--> PBuiltinList (PAsData PFrontierPeak) :--> PInteger :--> PBuiltinList (PAsData PByteString) :--> PBool)
pexecutionSignerMembershipIsValid = phoistAcyclic $ plam $ \signerHash count commitment peaks index siblings -> plengthBS # signerHash #== 28 #&& pexecutionSignerFrontierMatches # count # commitment # peaks #&& pverifyMembership # count # peaks # index # (ScriptProof.psignerLeafHash # signerHash) # siblings

pexecutionSignerProofVerdict :: forall s. Term s (PByteString :--> PInteger :--> PBuiltinList (PAsData PFrontierPeak) :--> PSignerSetProofV1 :--> PBool)
pexecutionSignerProofVerdict = phoistAcyclic $ plam $ \signerHash count signerPeaks proof ->
    plet (pfrontierCommitment # count # signerPeaks) $ \commitment -> pmatch proof $ \case
        PSignerMembershipProof peaks index siblings -> pif (pfromData peaks #== signerPeaks #&& pexecutionSignerMembershipIsValid # signerHash # count # commitment # pfromData peaks # pfromData index # pfromData siblings) (pconstant True) perror
        PEmptySignerSetProof peaks -> pif (count #== 0 #&& pexecutionSignerFrontierMatches # count # commitment # pfromData peaks) (pconstant False) perror
        PSignerBelowFirstProof peaks firstHash siblings -> pif (signerHash #< pfromData firstHash #&& pexecutionSignerMembershipIsValid # pfromData firstHash # count # commitment # pfromData peaks # 0 # pfromData siblings) (pconstant False) perror
        PSignerAboveLastProof peaks lastHash siblings -> pif (count #> 0 #&& pfromData lastHash #< signerHash #&& pexecutionSignerMembershipIsValid # pfromData lastHash # count # commitment # pfromData peaks # (count - 1) # pfromData siblings) (pconstant False) perror
        PSignerBetweenProof peaks lowerIndex lowerHash lowerSiblings upperHash upperSiblings -> pif (pfromData lowerIndex #>= 0 #&& pfromData lowerIndex + 1 #< count #&& pfromData lowerHash #< signerHash #&& signerHash #< pfromData upperHash #&& pexecutionSignerMembershipIsValid # pfromData lowerHash # count # commitment # pfromData peaks # pfromData lowerIndex # pfromData lowerSiblings #&& pexecutionSignerMembershipIsValid # pfromData upperHash # count # commitment # pfromData peaks # (pfromData lowerIndex + 1) # pfromData upperSiblings) (pconstant False) perror
        PNoSignerSetProof -> perror

executionNativeScriptInvalidAcceptedReconstructionInitValidator :: forall s. Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
executionNativeScriptInvalidAcceptedReconstructionInitValidator = plam $ \acceptedSpendPrefixHash threadPolicy ctx ->
    pstep ctx $ \datum redeemer ownRef tx -> pdispatch @_ @PAcceptedReconstructionInitArgs threadPolicy datum redeemer ownRef tx $ \args -> P.do
        PAcceptedReconstructionInitArgs{..} <- pmatch args
        PTxInfo{ptxInfo'inputs, ptxInfo'outputs} <- pmatch tx
        pcontinue threadPolicy (pexpectDatum datum) (pfromData pacceptedInitArgs'inputIndex) (pfromData pacceptedInitArgs'outputIndex) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) $ \_ _ _ inputState outputHash outputState ->
            let expected = Accepted.pinitialV1 # pexpectStateAs @PBoundExecutionV1 inputState # pto (pfromData acceptedSpendPrefixHash)
             in outputHash #== acceptedSpendPrefixHash #&& outputState #== pforgetData (pdata expected)

executionNativeScriptInvalidAcceptedSpendPrefixValidator :: forall s. Term s (PAsData PScriptHash :--> PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
executionNativeScriptInvalidAcceptedSpendPrefixValidator = plam $ \mintPrefixHash inlineSourceHash threadPolicy certificatePolicy ctx ->
    pstep ctx $ \datum redeemer ownRef tx -> pdispatch @_ @PAcceptedSpendPrefixAction threadPolicy datum redeemer ownRef tx $ \action -> P.do
        PTxInfo{ptxInfo'inputs, ptxInfo'outputs, ptxInfo'referenceInputs, ptxInfo'redeemers} <- pmatch tx
        inputIndex <- plet $ pspendActionInputIndex action
        outputIndex <- plet $ pspendActionOutputIndex action
        opening <- plet $ pspendActionOpening action
        pcontinue threadPolicy (pexpectDatum datum) inputIndex outputIndex ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) $ \ownHash _ _ inputState outputHash outputState -> P.do
            state@PAcceptedReconstructionStateV1{..} <- pmatch $ pexpectStateAs @PAcceptedReconstructionStateV1 inputState
            PBoundExecutionV1{pboundExecution'subject, pboundExecution'executionIndex, pboundExecution'priorLedgerRoot} <- pmatch $ pfromData preconstruction'bound
            Subject.PVerdictSubject{Subject.psubject'transactionId} <- pmatch $ pfromData pboundExecution'subject
            let ownHashBytes = pto (pfromData ownHash)
                stateAuthentic = Accepted.pstateIsAuthenticV1 # pcon state #&& pfromData preconstruction'phase #== Accepted.pphaseSpend #&& pfromData preconstruction'nextExpectedScriptHash #== ownHashBytes
            view <- plet $ popenedFieldView # opening # pcon (PBodyAnchor psubject'transactionId) # pspendInputsFieldIndex # pfromData ptxInfo'referenceInputs # certificatePolicy
            pif
                stateAuthentic
                ( pmatch action $ \case
                    PScanSpend _ _ _ descriptorCbor membership -> P.do
                        selected <- plet $ pspendInputAt # view # pfromData preconstruction'fieldCursor
                        PMidgardTxInput{ptxInput'outputIndex} <- pmatch selected
                        descriptor <- plet $ pdecodeLedgerOutputCommitment # pfromData descriptorCbor
                        PLedgerOutputCommitmentV1{poutputCommitment'outputIndex, poutputCommitment'address} <- pmatch descriptor
                        canonicalKey <- plet $ pfieldItemAt # view # pfromData preconstruction'fieldCursor
                        let nextHash = pif (pfromData preconstruction'executionCursor #== pfromData pboundExecution'executionIndex) (pto $ pfromData inlineSourceHash) ownHashBytes
                            membershipHolds =
                                pverifyMembershipCarried
                                    (pfromData membership)
                                    (pfromData pboundExecution'priorLedgerRoot)
                                    (Proof.pledgerOutrefKey $ pforgetData $ pdata selected)
                                    (pfromData descriptorCbor)
                                    (pfromData ptxInfo'referenceInputs)
                                    (pto $ pto $ pfromData ptxInfo'redeemers)
                                    #&& pfromData poutputCommitment'outputIndex
                                    #== pfromData ptxInput'outputIndex
                        pif
                            (pfromData preconstruction'fieldCursor #< pfieldItemCount # view #&& membershipHolds)
                            ( pmatch (pdecodeCanonicalAddressBytes # pfromData poutputCommitment'address) $ \case
                                PNothing -> perror
                                PJust address -> pmatch address $ \PMidgardAddress{paddress'paymentCredential} ->
                                    let advanced = pmatch (pfromData paddress'paymentCredential) $ \case
                                            PMidgardScriptCredential scriptHash -> Accepted.pappendPurposeV1 # pcon state # Accepted.pphaseSpend # pfromData preconstruction'fieldCursor # pfromData scriptHash # canonicalKey # canonicalKey # nextHash
                                            PMidgardPubKeyCredential _ -> Accepted.padvanceNonScriptItemV1 # pcon state # canonicalKey # ownHashBytes
                                     in pmatch advanced $ \PAcceptedReconstructionStateV1{preconstruction'nextExpectedScriptHash} ->
                                            pto (pfromData outputHash)
                                                #== pfromData preconstruction'nextExpectedScriptHash
                                                #&& outputState
                                                #== pforgetData (pdata advanced)
                            )
                            perror
                    PFinishSpends _ _ _ ->
                        pif
                            (pfromData preconstruction'fieldCursor #== pfieldItemCount # view)
                            ( let advanced = Accepted.pfinishPurposePhaseV1 # pcon state # pto (pfromData mintPrefixHash)
                               in outputHash #== mintPrefixHash #&& outputState #== pforgetData (pdata advanced)
                            )
                            perror
                )
                perror

executionNativeScriptInvalidAcceptedMintPrefixValidator :: forall s. Term s (PAsData PScriptHash :--> PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
executionNativeScriptInvalidAcceptedMintPrefixValidator = plam $ \observerPrefixHash inlineSourceHash threadPolicy certificatePolicy ctx ->
    pstep ctx $ \datum redeemer ownRef tx -> pdispatch @_ @PAcceptedMintPrefixAction threadPolicy datum redeemer ownRef tx $ \action -> P.do
        PTxInfo{ptxInfo'inputs, ptxInfo'outputs, ptxInfo'referenceInputs} <- pmatch tx
        inputIndex <- plet $ pmintActionInputIndex action
        outputIndex <- plet $ pmintActionOutputIndex action
        opening <- plet $ pmintActionOpening action
        pcontinue threadPolicy (pexpectDatum datum) inputIndex outputIndex ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) $ \ownHash _ _ inputState outputHash outputState -> P.do
            state@PAcceptedReconstructionStateV1{..} <- pmatch $ pexpectStateAs @PAcceptedReconstructionStateV1 inputState
            PBoundExecutionV1{pboundExecution'subject, pboundExecution'executionIndex} <- pmatch $ pfromData preconstruction'bound
            Subject.PVerdictSubject{Subject.psubject'transactionId} <- pmatch $ pfromData pboundExecution'subject
            let ownHashBytes = pto (pfromData ownHash)
                stateAuthentic = Accepted.pstateIsAuthenticV1 # pcon state #&& pfromData preconstruction'phase #== Accepted.pphaseMint #&& pfromData preconstruction'nextExpectedScriptHash #== ownHashBytes
            view <- plet $ popenedFieldView # opening # pcon (PBodyAnchor psubject'transactionId) # pmintFieldIndex # pfromData ptxInfo'referenceInputs # certificatePolicy
            pif
                stateAuthentic
                ( pmatch action $ \case
                    PScanMint _ _ _ ->
                        pif
                            (pfromData preconstruction'fieldCursor #< pfieldItemCount # view)
                            ( pmatch (pdecodeMintPolicyItemCbor # (pfieldItemAt # view # pfromData preconstruction'fieldCursor)) $ \(PPair policyId _) ->
                                let nextHash = pif (pfromData preconstruction'executionCursor #== pfromData pboundExecution'executionIndex) (pto $ pfromData inlineSourceHash) ownHashBytes
                                    advanced = Accepted.pappendPurposeV1 # pcon state # Accepted.pphaseMint # pfromData preconstruction'fieldCursor # policyId # policyId # policyId # nextHash
                                 in pmatch advanced $ \PAcceptedReconstructionStateV1{preconstruction'nextExpectedScriptHash} ->
                                        pto (pfromData outputHash)
                                            #== pfromData preconstruction'nextExpectedScriptHash
                                            #&& outputState
                                            #== pforgetData (pdata advanced)
                            )
                            perror
                    PFinishMint _ _ _ ->
                        pif
                            (pfromData preconstruction'fieldCursor #== pfieldItemCount # view)
                            ( let advanced = Accepted.pfinishPurposePhaseV1 # pcon state # pto (pfromData observerPrefixHash)
                               in outputHash #== observerPrefixHash #&& outputState #== pforgetData (pdata advanced)
                            )
                            perror
                )
                perror

executionNativeScriptInvalidAcceptedObserverPrefixValidator :: forall s. Term s (PAsData PScriptHash :--> PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
executionNativeScriptInvalidAcceptedObserverPrefixValidator = plam $ \receivePrefixHash inlineSourceHash threadPolicy certificatePolicy ctx ->
    pstep ctx $ \datum redeemer ownRef tx -> pdispatch @_ @PAcceptedObserverPrefixAction threadPolicy datum redeemer ownRef tx $ \action -> P.do
        PTxInfo{ptxInfo'inputs, ptxInfo'outputs, ptxInfo'referenceInputs} <- pmatch tx
        inputIndex <- plet $ pobserverActionInputIndex action
        outputIndex <- plet $ pobserverActionOutputIndex action
        opening <- plet $ pobserverActionOpening action
        pcontinue threadPolicy (pexpectDatum datum) inputIndex outputIndex ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) $ \ownHash _ _ inputState outputHash outputState -> P.do
            state@PAcceptedReconstructionStateV1{..} <- pmatch $ pexpectStateAs @PAcceptedReconstructionStateV1 inputState
            PBoundExecutionV1{pboundExecution'subject, pboundExecution'executionIndex} <- pmatch $ pfromData preconstruction'bound
            Subject.PVerdictSubject{Subject.psubject'transactionId} <- pmatch $ pfromData pboundExecution'subject
            let ownHashBytes = pto (pfromData ownHash)
                stateAuthentic = Accepted.pstateIsAuthenticV1 # pcon state #&& pfromData preconstruction'phase #== Accepted.pphaseObserve #&& pfromData preconstruction'nextExpectedScriptHash #== ownHashBytes
            view <- plet $ popenedFieldView # opening # pcon (PBodyAnchor psubject'transactionId) # prequiredObserversFieldIndex # pfromData ptxInfo'referenceInputs # certificatePolicy
            pif
                stateAuthentic
                ( pmatch action $ \case
                    PScanObserver _ _ _ ->
                        pif
                            (pfromData preconstruction'fieldCursor #< pfieldItemCount # view)
                            ( let observer = pfieldItemAt # view # pfromData preconstruction'fieldCursor
                                  nextHash = pif (pfromData preconstruction'executionCursor #== pfromData pboundExecution'executionIndex) (pto $ pfromData inlineSourceHash) ownHashBytes
                                  advanced = Accepted.pappendPurposeV1 # pcon state # Accepted.pphaseObserve # pfromData preconstruction'fieldCursor # observer # observer # observer # nextHash
                               in pif
                                    (plengthBS # observer #== 28)
                                    ( pmatch advanced $ \PAcceptedReconstructionStateV1{preconstruction'nextExpectedScriptHash} ->
                                        pto (pfromData outputHash)
                                            #== pfromData preconstruction'nextExpectedScriptHash
                                            #&& outputState
                                            #== pforgetData (pdata advanced)
                                    )
                                    perror
                            )
                            perror
                    PFinishObservers _ _ _ ->
                        pif
                            (pfromData preconstruction'fieldCursor #== pfieldItemCount # view)
                            ( let advanced = Accepted.pfinishPurposePhaseV1 # pcon state # pto (pfromData receivePrefixHash)
                               in outputHash #== receivePrefixHash #&& outputState #== pforgetData (pdata advanced)
                            )
                            perror
                )
                perror

executionNativeScriptInvalidAcceptedReceivePrefixValidator :: forall s. Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
executionNativeScriptInvalidAcceptedReceivePrefixValidator = plam $ \inlineSourceHash threadPolicy certificatePolicy ctx ->
    pstep ctx $ \datum redeemer ownRef tx -> pdispatch @_ @PAcceptedReceivePrefixAction threadPolicy datum redeemer ownRef tx $ \action -> P.do
        PTxInfo{ptxInfo'inputs, ptxInfo'outputs, ptxInfo'referenceInputs} <- pmatch tx
        inputIndex <- plet $ preceiveActionInputIndex action
        outputIndex <- plet $ preceiveActionOutputIndex action
        opening <- plet $ preceiveActionOpening action
        pcontinue threadPolicy (pexpectDatum datum) inputIndex outputIndex ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) $ \ownHash _ _ inputState outputHash outputState -> P.do
            state@PAcceptedReconstructionStateV1{..} <- pmatch $ pexpectStateAs @PAcceptedReconstructionStateV1 inputState
            PBoundExecutionV1{pboundExecution'subject} <- pmatch $ pfromData preconstruction'bound
            Subject.PVerdictSubject{Subject.psubject'transactionId} <- pmatch $ pfromData pboundExecution'subject
            let ownHashBytes = pto (pfromData ownHash)
                stateAuthentic = Accepted.pstateIsAuthenticV1 # pcon state #&& pfromData preconstruction'phase #== Accepted.pphaseReceive #&& pfromData preconstruction'nextExpectedScriptHash #== ownHashBytes
            view <- plet $ popenedFieldView # opening # pcon (PBodyAnchor psubject'transactionId) # poutputsFieldIndex # pfromData ptxInfo'referenceInputs # certificatePolicy
            pif
                stateAuthentic
                ( pmatch action $ \case
                    PScanOutput _ _ _ ->
                        pif
                            (pfromData preconstruction'fieldCursor #< pfieldItemCount # view)
                            ( pmatch (pdecodeMidgardTxOutputCbor # (pfieldItemAt # view # pfromData preconstruction'fieldCursor)) $ \PMidgardTxOutput{ptxOutput'address} ->
                                pmatch (pfromData ptxOutput'address) $ \PMidgardAddress{paddress'protected, paddress'paymentCredential} ->
                                    let candidate =
                                            pif
                                                (pfromData paddress'protected)
                                                ( pmatch (pfromData paddress'paymentCredential) $ \case
                                                    PMidgardScriptCredential scriptHash -> pcon $ PDJust scriptHash
                                                    PMidgardPubKeyCredential _ -> pcon PDNothing
                                                )
                                                (pcon PDNothing)
                                        advanced = Accepted.pscanReceiveOutputV1 # pcon state # candidate # ownHashBytes
                                     in outputHash #== ownHash #&& outputState #== pforgetData (pdata advanced)
                            )
                            perror
                    PFinishOutputPass _ _ _ ->
                        pif
                            (pfromData preconstruction'fieldCursor #== pfieldItemCount # view)
                            ( let advanced = Accepted.pfinishReceivePassV1 # pcon state # ownHashBytes # pto (pfromData inlineSourceHash)
                               in pmatch advanced $ \PAcceptedReconstructionStateV1{preconstruction'nextExpectedScriptHash} ->
                                    pto (pfromData outputHash)
                                        #== pfromData preconstruction'nextExpectedScriptHash
                                        #&& outputState
                                        #== pforgetData (pdata advanced)
                            )
                            perror
                )
                perror

executionNativeScriptInvalidAcceptedInlineSourceValidator :: forall s. Term s (PAsData PScriptHash :--> PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
executionNativeScriptInvalidAcceptedInlineSourceValidator = plam $ \step03Hash referenceSourceHash threadPolicy certificatePolicy ctx ->
    pstep ctx $ \datum redeemer ownRef tx -> pdispatch @_ @PAcceptedInlineSourceAction threadPolicy datum redeemer ownRef tx $ \action -> P.do
        PTxInfo{ptxInfo'inputs, ptxInfo'outputs, ptxInfo'referenceInputs} <- pmatch tx
        inputIndex <- plet $ pinlineActionInputIndex action
        outputIndex <- plet $ pinlineActionOutputIndex action
        opening <- plet $ pinlineActionOpening action
        pcontinue threadPolicy (pexpectDatum datum) inputIndex outputIndex ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) $ \ownHash _ _ inputState outputHash outputState -> P.do
            state@PAcceptedReconstructionStateV1{..} <- pmatch $ pexpectStateAs @PAcceptedReconstructionStateV1 inputState
            PBoundExecutionV1{pboundExecution'subject, pboundExecution'compactCbor} <- pmatch $ pfromData preconstruction'bound
            Subject.PVerdictSubject{Subject.psubject'transactionId} <- pmatch $ pfromData pboundExecution'subject
            PNativeTxCompact{pcompact'witnessSetHash} <- pmatch $ pdecodeNativeTxCompactV1 # pfromData pboundExecution'compactCbor
            let ownHashBytes = pto (pfromData ownHash)
                stateAuthentic = Accepted.pstateIsAuthenticV1 # pcon state #&& pfromData preconstruction'phase #== Accepted.pphaseInlineSource #&& pfromData preconstruction'nextExpectedScriptHash #== ownHashBytes
            view <- plet $ popenedFieldView # opening # pcon (PWitnessAnchor psubject'transactionId (pdata pcompact'witnessSetHash)) # pscriptWitnessesFieldIndex # pfromData ptxInfo'referenceInputs # certificatePolicy
            pif
                stateAuthentic
                ( pmatch action $ \case
                    PScanInline _ _ _ ->
                        pif
                            (pfromData preconstruction'fieldCursor #< pfieldItemCount # view)
                            ( P.do
                                item <- plet $ pfieldItemAt # view # pfromData preconstruction'fieldCursor
                                PPair offset script <- pmatch $ pdecodeMidgardVersionedScriptAt # item # 0
                                PMidgardVersionedScript{pversionedScript'language} <- pmatch script
                                scriptHash <- plet $ ScriptProof.pversionedScriptHash # script
                                let source =
                                        pcon $
                                            Accepted.PSelectedSourceV1
                                                (pdata $ pfromData preconstruction'sourceCursor)
                                                (pdata 0)
                                                (pdata $ pserialiseData # pforgetData preconstruction'fieldCursor)
                                                (pdata $ ScriptProof.planguageTag # pfromData pversionedScript'language)
                                                (pdata scriptHash)
                                                (pdata $ plengthBS # item)
                                                (pdata $ Bounded.pfromBytes # pscriptWitnessesFieldIndex # pfromData preconstruction'fieldCursor # item)
                                    nextHash = pif (scriptHash #== pselectedPurposeHash (pcon state)) (pto $ pfromData step03Hash) ownHashBytes
                                    advanced = Accepted.pappendSourceV1 # pcon state # source # nextHash
                                pif
                                    (offset #== plengthBS # item #&& pencodeMidgardVersionedScript # script #== item)
                                    ( pmatch advanced $ \PAcceptedReconstructionStateV1{preconstruction'nextExpectedScriptHash, preconstruction'selectedSource} ->
                                        let expected =
                                                pif
                                                    (pfromData preconstruction'selectedSource #== pcon PDNothing)
                                                    (pforgetData $ pdata advanced)
                                                    (pforgetData $ pdata $ Accepted.pauthenticatedSourceV1 # advanced)
                                         in pto (pfromData outputHash)
                                                #== pfromData preconstruction'nextExpectedScriptHash
                                                #&& outputState
                                                #== expected
                                    )
                                    perror
                            )
                            perror
                    PFinishInline _ _ _ ->
                        pif
                            (pfromData preconstruction'fieldCursor #== pfieldItemCount # view)
                            ( let advanced = Accepted.pfinishInlineSourcesV1 # pcon state # pto (pfromData referenceSourceHash)
                               in outputHash #== referenceSourceHash #&& outputState #== pforgetData (pdata advanced)
                            )
                            perror
                )
                perror

executionNativeScriptInvalidAcceptedReferenceSourceValidator :: forall s. Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
executionNativeScriptInvalidAcceptedReferenceSourceValidator = plam $ \step03Hash threadPolicy certificatePolicy ctx ->
    pstep ctx $ \datum redeemer ownRef tx -> pdispatch @_ @PAcceptedReferenceSourceArgs threadPolicy datum redeemer ownRef tx $ \args -> P.do
        PAcceptedReferenceSourceArgs{..} <- pmatch args
        PTxInfo{ptxInfo'inputs, ptxInfo'outputs, ptxInfo'referenceInputs, ptxInfo'redeemers} <- pmatch tx
        pcontinue threadPolicy (pexpectDatum datum) (pfromData pacceptedReferenceArgs'inputIndex) (pfromData pacceptedReferenceArgs'outputIndex) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) $ \ownHash _ _ inputState outputHash outputState -> P.do
            state@PAcceptedReconstructionStateV1{..} <- pmatch $ pexpectStateAs @PAcceptedReconstructionStateV1 inputState
            PBoundExecutionV1{pboundExecution'subject, pboundExecution'priorLedgerRoot} <- pmatch $ pfromData preconstruction'bound
            Subject.PVerdictSubject{Subject.psubject'transactionId} <- pmatch $ pfromData pboundExecution'subject
            let ownHashBytes = pto (pfromData ownHash)
                stateAuthentic = Accepted.pstateIsAuthenticV1 # pcon state #&& pfromData preconstruction'phase #== Accepted.pphaseReferenceSource #&& pfromData preconstruction'nextExpectedScriptHash #== ownHashBytes
            view <- plet $ popenedFieldView # pfromData pacceptedReferenceArgs'referenceInputsOpening # pcon (PBodyAnchor psubject'transactionId) # preferenceInputsFieldIndex # pfromData ptxInfo'referenceInputs # certificatePolicy
            pif
                (stateAuthentic #&& pfromData preconstruction'fieldCursor #< pfieldItemCount # view)
                ( P.do
                    selected <- plet $ pspendInputAt # view # pfromData preconstruction'fieldCursor
                    PMidgardTxInput{ptxInput'outputIndex} <- pmatch selected
                    descriptor <- plet $ pdecodeLedgerOutputCommitment # pfromData pacceptedReferenceArgs'descriptorCbor
                    PLedgerOutputCommitmentV1{..} <- pmatch descriptor
                    let membershipHolds =
                            pverifyMembershipCarried
                                (pfromData pacceptedReferenceArgs'membership)
                                (pfromData pboundExecution'priorLedgerRoot)
                                (Proof.pledgerOutrefKey $ pforgetData $ pdata selected)
                                (pfromData pacceptedReferenceArgs'descriptorCbor)
                                (pfromData ptxInfo'referenceInputs)
                                (pto $ pto $ pfromData ptxInfo'redeemers)
                                #&& pfromData poutputCommitment'outputIndex
                                #== pfromData ptxInput'outputIndex
                        advanced =
                            pif
                                (pfromData poutputCommitment'referenceScriptLanguage #== -1)
                                (Accepted.padvanceReferenceWithoutSourceV1 # pcon state # ownHashBytes)
                                ( let source =
                                        pcon $
                                            Accepted.PSelectedSourceV1
                                                (pdata $ pfromData preconstruction'sourceCursor)
                                                (pdata 1)
                                                (pdata $ pfieldItemAt # view # pfromData preconstruction'fieldCursor)
                                                poutputCommitment'referenceScriptLanguage
                                                poutputCommitment'referenceScriptHash
                                                poutputCommitment'referenceScriptTotalLength
                                                poutputCommitment'referenceScriptItemCommitment
                                      nextHash = pif (pfromData poutputCommitment'referenceScriptHash #== pselectedPurposeHash (pcon state)) (pto $ pfromData step03Hash) ownHashBytes
                                   in Accepted.pappendSourceV1 # pcon state # source # nextHash
                                )
                    pif
                        membershipHolds
                        ( pmatch advanced $ \PAcceptedReconstructionStateV1{preconstruction'nextExpectedScriptHash, preconstruction'selectedSource} ->
                            let expected =
                                    pif
                                        (pfromData preconstruction'selectedSource #== pcon PDNothing)
                                        (pforgetData $ pdata advanced)
                                        (pforgetData $ pdata $ Accepted.pauthenticatedSourceV1 # advanced)
                             in pto (pfromData outputHash)
                                    #== pfromData preconstruction'nextExpectedScriptHash
                                    #&& outputState
                                    #== expected
                        )
                        perror
                )
                perror

pselectedPurposeHash :: forall s. Term s PAcceptedReconstructionStateV1 -> Term s PByteString
pselectedPurposeHash state = pmatch state $ \PAcceptedReconstructionStateV1{preconstruction'selectedPurpose} ->
    pmatch (pfromData preconstruction'selectedPurpose) $ \case
        PDNothing -> perror
        PDJust purposeData -> pmatch (pfromData purposeData) $ \Accepted.PSelectedPurposeV1{Accepted.pselectedPurpose'scriptHash} -> pfromData pselectedPurpose'scriptHash

pinlineActionInputIndex :: forall s. Term s PAcceptedInlineSourceAction -> Term s PInteger
pinlineActionInputIndex action = pmatch action $ \case PScanInline inputIndex _ _ -> pfromData inputIndex; PFinishInline inputIndex _ _ -> pfromData inputIndex

pinlineActionOutputIndex :: forall s. Term s PAcceptedInlineSourceAction -> Term s PInteger
pinlineActionOutputIndex action = pmatch action $ \case PScanInline _ outputIndex _ -> pfromData outputIndex; PFinishInline _ outputIndex _ -> pfromData outputIndex

pinlineActionOpening :: forall s. Term s PAcceptedInlineSourceAction -> Term s PFieldOpeningV1
pinlineActionOpening action = pmatch action $ \case PScanInline _ _ opening -> pfromData opening; PFinishInline _ _ opening -> pfromData opening

pspendActionInputIndex :: forall s. Term s PAcceptedSpendPrefixAction -> Term s PInteger
pspendActionInputIndex action = pmatch action $ \case
    PScanSpend inputIndex _ _ _ _ -> pfromData inputIndex
    PFinishSpends inputIndex _ _ -> pfromData inputIndex

pspendActionOutputIndex :: forall s. Term s PAcceptedSpendPrefixAction -> Term s PInteger
pspendActionOutputIndex action = pmatch action $ \case
    PScanSpend _ outputIndex _ _ _ -> pfromData outputIndex
    PFinishSpends _ outputIndex _ -> pfromData outputIndex

pspendActionOpening :: forall s. Term s PAcceptedSpendPrefixAction -> Term s PFieldOpeningV1
pspendActionOpening action = pmatch action $ \case
    PScanSpend _ _ opening _ _ -> pfromData opening
    PFinishSpends _ _ opening -> pfromData opening

pmintActionInputIndex :: forall s. Term s PAcceptedMintPrefixAction -> Term s PInteger
pmintActionInputIndex action = pmatch action $ \case PScanMint inputIndex _ _ -> pfromData inputIndex; PFinishMint inputIndex _ _ -> pfromData inputIndex

pmintActionOutputIndex :: forall s. Term s PAcceptedMintPrefixAction -> Term s PInteger
pmintActionOutputIndex action = pmatch action $ \case PScanMint _ outputIndex _ -> pfromData outputIndex; PFinishMint _ outputIndex _ -> pfromData outputIndex

pmintActionOpening :: forall s. Term s PAcceptedMintPrefixAction -> Term s PFieldOpeningV1
pmintActionOpening action = pmatch action $ \case PScanMint _ _ opening -> pfromData opening; PFinishMint _ _ opening -> pfromData opening

pobserverActionInputIndex :: forall s. Term s PAcceptedObserverPrefixAction -> Term s PInteger
pobserverActionInputIndex action = pmatch action $ \case PScanObserver inputIndex _ _ -> pfromData inputIndex; PFinishObservers inputIndex _ _ -> pfromData inputIndex

pobserverActionOutputIndex :: forall s. Term s PAcceptedObserverPrefixAction -> Term s PInteger
pobserverActionOutputIndex action = pmatch action $ \case PScanObserver _ outputIndex _ -> pfromData outputIndex; PFinishObservers _ outputIndex _ -> pfromData outputIndex

pobserverActionOpening :: forall s. Term s PAcceptedObserverPrefixAction -> Term s PFieldOpeningV1
pobserverActionOpening action = pmatch action $ \case PScanObserver _ _ opening -> pfromData opening; PFinishObservers _ _ opening -> pfromData opening

preceiveActionInputIndex :: forall s. Term s PAcceptedReceivePrefixAction -> Term s PInteger
preceiveActionInputIndex action = pmatch action $ \case PScanOutput inputIndex _ _ -> pfromData inputIndex; PFinishOutputPass inputIndex _ _ -> pfromData inputIndex

preceiveActionOutputIndex :: forall s. Term s PAcceptedReceivePrefixAction -> Term s PInteger
preceiveActionOutputIndex action = pmatch action $ \case PScanOutput _ outputIndex _ -> pfromData outputIndex; PFinishOutputPass _ outputIndex _ -> pfromData outputIndex

preceiveActionOpening :: forall s. Term s PAcceptedReceivePrefixAction -> Term s PFieldOpeningV1
preceiveActionOpening action = pmatch action $ \case PScanOutput _ _ opening -> pfromData opening; PFinishOutputPass _ _ opening -> pfromData opening
