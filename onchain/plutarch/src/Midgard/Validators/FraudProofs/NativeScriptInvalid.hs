module Midgard.Validators.FraudProofs.NativeScriptInvalid (
  nativeScriptInvalidStep01Validator,
  nativeScriptInvalidStep02Validator,
  nativeScriptInvalidStep03Validator,
  nativeScriptInvalidStep04Validator,
  nativeScriptInvalidStep05Validator,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Builtin.Crypto (pblake2b_256)
import Plutarch.LedgerApi.V3 (PAddress, PCurrencySymbol, PScriptContext, PScriptHash, PTxInInfo, PTxInfo (..))
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Repr.Scott (DeriveAsScottRec (..))

import Midgard.FraudProofs.Common (pcontinue, pfinalize, ppassNativeTxToNextStepCarried)
import Midgard.FraudProofs.FieldOpening (
  PFieldOpeningV1,
  PNativeTxAnchorV1 (..),
  paddressWitnessesFieldIndex,
  pfoldOpenedField,
  popenedCertifiedFieldWalkFromGrammar,
  popenedFieldGrammarCertification,
  popenedFieldView,
  popenedFieldWalk,
  presumeOpenedFieldGrammarCertification,
  presumeOpenedFieldWalk,
  pscriptWitnessesFieldIndex,
 )
import Midgard.FraudProofs.MintAuthorization.Engine (PNativeScriptVerdictV1 (..), pevaluateNativeScriptV1)
import Midgard.FraudProofs.NativeScriptInvalid (
  PSignerQueryV1 (..),
  PStep01Args (..),
  PStep01Source (..),
  PStep02Args (..),
  PStep02State (..),
  PStep03Args (..),
  PStep03State (..),
  PStep04Args (..),
  PStep04State (..),
  PStep05Args (..),
  PStep05PhaseV1 (..),
  PStep05State (..),
  pauthenticatedSigner,
  pbindScriptIndex,
  pdirectScriptBytesLimit,
  pdirectSignerLimit,
  pstagedNodeBatchLimit,
  pstagedSignerFinalizeBatchLimit,
  pstagedSignerResumeBatchLimit,
  pstagedSignerStartBatchLimit,
 )
import Midgard.FraudProofs.NativeTx.Compact (pverifyNativeTxCompactCborV1)
import Midgard.FraudProofs.NativeTx.Components (
  pdecodeMidgardVersionedScriptAt,
  pencodeMidgardVersionedScript,
 )
import Midgard.FraudProofs.NativeTx.Types (
  PMidgardScriptLanguage (..),
  PMidgardVersionedScript (..),
  PNativeTxBodyCompact (..),
  PNativeTxCompact (..),
  PVerifiedMidgardNativeTxCompact (..),
 )
import Midgard.FraudProofs.ProofThreadSubstrate qualified as Subject
import Midgard.LedgerState (PForcedInclusionTxV1 (..), PNativeTxProofSourceV1 (..))
import Midgard.NativeTxFieldAccess (PFieldViewV1, pfieldItemAt, pfieldItemCount)
import Midgard.NativeTxMachineWalk (PFieldWalkCheckpointV1, pcertifyFieldGrammar, pfieldGrammarCheckpointHash, pfieldGrammarIsComplete, pfieldWalkCheckpointHash, pwalkFold, pwalkIsComplete, pwalkNext, pwalkNextItemIndex, pwalkSkip)
import Midgard.NativeTxScriptPushdown (
  PNativeScriptWalkV1,
  pnativeScriptCursorHash,
  pnativeScriptRunWithSignerVerdict,
  pnativeScriptVerdict,
  pnativeScriptWalkIsComplete,
  popenNativeScriptWalk,
  presumeNativeScriptWalkFromCommitment,
 )
import Midgard.RejectionReason (PRejectionReasonV1 (..))
import Midgard.ScriptProof (psignerLeafHash)
import Midgard.TransitionTrace (PRootMembershipProof (..))
import Midgard.ValidationMachine (PSignerSetProofV1 (..))
import Midgard.ValidationMerkle (
  PFrontierPeak,
  pappendLeaf,
  pemptyFrontier,
  pfrontierCommitment,
  pfrontierIsWellFormed,
  pverifyMembership,
 )
import Midgard.Validators.FraudProofs.Step (pdispatch, pexpectDatum, pexpectStateAs, pexpecting, pstateIsAbsent, pstep)
import Plutarch.Unsafe (punsafeCoerce)

data PSignerAccumulatorV1 s = PSignerAccumulatorV1
  { psignerAccumulator'previousSignerHash :: Term s PByteString
  , psignerAccumulator'signerCount :: Term s PInteger
  , psignerAccumulator'signerPeaks :: Term s (PBuiltinList (PAsData PFrontierPeak))
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic)
  deriving (PlutusType) via (DeriveAsScottRec PSignerAccumulatorV1)

nativeScriptInvalidStep01Validator ::
  forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PAsData PScriptHash :--> PScriptContext :--> PUnit)
nativeScriptInvalidStep01Validator = plam $ \step02ScriptHash computationThreadPolicy hubOracle ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
    pdispatch @_ @PStep01Args computationThreadPolicy datum redeemer ownOutRef txInfo $ \args -> P.do
      PStep01Args source <- pmatch args
      PTxInfo {ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs, ptxInfo'redeemers} <- pmatch txInfo
      pmatch (pfromData source) $ \case
        PAcceptedSource carriage ->
          ppassNativeTxToNextStepCarried
            computationThreadPolicy
            hubOracle
            datum
            (pfromData carriage)
            ownOutRef
            (pfromData ptxInfo'inputs)
            (pfromData ptxInfo'referenceInputs)
            (pfromData ptxInfo'outputs)
            (pto $ pto $ pfromData ptxInfo'redeemers)
            $ \_ _ _ inputState outputHash outputData _ _ verified ->
              pexpecting (pstateIsAbsent inputState) $
                pexpecting (outputHash #== step02ScriptHash) $
                  outputData #== expectedState (Subject.pbindAcceptedSubject # verified) verified
        PForcedSource inputIndex outputIndex header membership direction ->
          pcontinue
            computationThreadPolicy
            (pexpectDatum datum)
            (pfromData inputIndex)
            (pfromData outputIndex)
            ownOutRef
            (pfromData ptxInfo'inputs)
            (pfromData ptxInfo'outputs)
            $ \_ name _ prior outputHash outputData -> P.do
              subject <-
                plet $
                  Subject.pbindForcedSubjectToThread
                    # pto (pfromData name)
                    # pfromData header
                    # pfromData membership
                    # pfromData direction
              Subject.PVerdictSubject {Subject.psubject'direction, Subject.psubject'transactionId} <- pmatch subject
              pexpecting (pstateIsAbsent prior) $
                pexpecting (pfromData psubject'direction #== 1) $
                  pmatch (Subject.prejectionReasonOf # subject) $ \case
                    PWitnessNativeScriptFalse scriptIndex -> P.do
                      PRootMembershipProof {prootMembership'value} <- pmatch $ pfromData membership
                      PForcedInclusionTxV1 {pforcedTx'source} <- pmatch $ pfromData $ punsafeCoerce prootMembership'value
                      PNativeTxProofSourceV1 {pnativeSource'compactCbor} <- pmatch $ pfromData pforcedTx'source
                      verified <- plet $ pverifyNativeTxCompactCborV1 # pfromData psubject'transactionId # pfromData pnativeSource'compactCbor
                      pexpecting (pfromData scriptIndex #>= 0) $
                        pexpecting (outputHash #== step02ScriptHash) $
                          outputData #== expectedState subject verified
                    _ -> perror
  where
    expectedState subject verified = P.do
      PVerifiedMidgardNativeTxCompact {pverified'txId, pverified'txCompact} <- pmatch verified
      PNativeTxCompact {pcompact'body, pcompact'witnessSetHash} <- pmatch pverified'txCompact
      PNativeTxBodyCompact {pbodyCompact'validityIntervalStart, pbodyCompact'validityIntervalEnd} <- pmatch pcompact'body
      pforgetData $
        pdata $
          pcon $
            PStep02State
              (pdata subject)
              (pdata pverified'txId)
              (pdata pcompact'witnessSetHash)
              (pdata pbodyCompact'validityIntervalStart)
              (pdata pbodyCompact'validityIntervalEnd)
              (pdata $ pconstant "")
              (pdata $ pconstant False)
              (pdata $ pconstant "")

nativeScriptInvalidStep02Validator ::
  forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
nativeScriptInvalidStep02Validator = plam $ \step03ScriptHash computationThreadPolicy certificatePolicy ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
    pdispatch @_ @PStep02Args computationThreadPolicy datum redeemer ownOutRef txInfo $ \action -> P.do
      PPair inputIndex outputIndex <- pmatch $ pmatch action $ \case
        PStep02Args input output _ _ -> pcon $ PPair input output
        PCertifyScriptField input output _ _ _ -> pcon $ PPair input output
        PSelectCertifiedScript input output _ _ _ _ _ -> pcon $ PPair input output
      PTxInfo {ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs} <- pmatch txInfo
      pcontinue
        computationThreadPolicy
        (pexpectDatum datum)
        (pfromData inputIndex)
        (pfromData outputIndex)
        ownOutRef
        (pfromData ptxInfo'inputs)
        (pfromData ptxInfo'outputs)
        $ \ownHash _ _ inputState outputHash outputData -> P.do
          state <- plet $ pexpectStateAs @PStep02State inputState
          st@PStep02State {..} <- pmatch state
          let anchor = pcon $ PWitnessAnchor pstep02State'badTxId pstep02State'badTxWitnessSetHash
              references = pfromData ptxInfo'referenceInputs
              selected item =
                pexpecting (outputHash #== step03ScriptHash) $
                  outputData #== (pselectedScriptState # state # item)
              continuing expected =
                pexpecting (outputHash #== ownHash) $
                  outputData #== pforgetData (pdata $ pcon expected)
          pmatch action $ \case
            PStep02Args _ _ index opening ->
              pexpecting (pfromData pstep02State'grammarCheckpointHash #== pconstant "") $
                pexpecting (pbindScriptIndex # pfromData pstep02State'subject # pfromData index) $ P.do
                  view <- plet $ popenedFieldView # pfromData opening # anchor # pscriptWitnessesFieldIndex # references # certificatePolicy
                  selected (pfieldItemAt # view # pfromData index)
            PCertifyScriptField _ _ opening checkpoint budget ->
              pexpecting (pnot # pfromData pstep02State'grammarComplete) $
                pexpecting (pfromData pstep02State'scriptCheckpointHash #== pconstant "") $
                  pexpecting (pfromData budget #> 0 #&& pfromData budget #<= 32) $ P.do
                    PPair view prior <-
                      pmatch $
                        pif
                          (pfromData pstep02State'grammarCheckpointHash #== pconstant "")
                          ( pexpecting (pfromData checkpoint #== pconstant "") $
                              popenedFieldGrammarCertification # pfromData opening # anchor # pscriptWitnessesFieldIndex # references # certificatePolicy
                          )
                          ( presumeOpenedFieldGrammarCertification
                              # pfromData opening
                              # anchor
                              # pscriptWitnessesFieldIndex
                              # pfromData pstep02State'grammarCheckpointHash
                              # pfromData checkpoint
                              # references
                              # certificatePolicy
                          )
                    next <- plet $ pcertifyFieldGrammar # view # prior # pfromData budget
                    continuing
                      st
                        { pstep02State'grammarCheckpointHash = pdata $ pfieldGrammarCheckpointHash # next
                        , pstep02State'grammarComplete = pdata $ pfieldGrammarIsComplete # next
                        }
            PSelectCertifiedScript _ _ index opening grammarBytes walkBytes budget ->
              pexpecting (pfromData pstep02State'grammarComplete) $
                pexpecting (pbindScriptIndex # pfromData pstep02State'subject # pfromData index) $
                  pexpecting (pfromData budget #> 0 #&& pfromData budget #<= 32) $ P.do
                    PPair view prior <-
                      pmatch $
                        pif
                          (pfromData pstep02State'scriptCheckpointHash #== pconstant "")
                          ( pexpecting (pfromData walkBytes #== pconstant "") $
                              popenedCertifiedFieldWalkFromGrammar
                                # pfromData opening
                                # anchor
                                # pscriptWitnessesFieldIndex
                                # pfromData pstep02State'grammarCheckpointHash
                                # pfromData grammarBytes
                                # references
                                # certificatePolicy
                          )
                          ( presumeOpenedFieldWalk
                              # pfromData opening
                              # anchor
                              # pscriptWitnessesFieldIndex
                              # pfromData pstep02State'scriptCheckpointHash
                              # pfromData walkBytes
                              # references
                              # certificatePolicy
                          )
                    remaining <- plet $ pfromData index - (pwalkNextItemIndex # prior)
                    pexpecting (remaining #>= 0) $
                      pif
                        (remaining #< pfromData budget)
                        (pmatch (pwalkNext # view # (pwalkSkip # view # prior # remaining)) $ \(PPair item _) -> selected item)
                        ( plet (pwalkSkip # view # prior # pfromData budget) $ \next ->
                            continuing st {pstep02State'scriptCheckpointHash = pdata $ pfieldWalkCheckpointHash # next}
                        )

pselectedScriptState :: forall s. Term s (PStep02State :--> PByteString :--> PData)
pselectedScriptState = phoistAcyclic $ plam $ \state item -> P.do
  PStep02State {..} <- pmatch state
  PPair offset script <- pmatch $ pdecodeMidgardVersionedScriptAt # item # 0
  PMidgardVersionedScript {pversionedScript'language} <- pmatch script
  pexpecting (offset #== plengthBS # item) $
    pexpecting (pencodeMidgardVersionedScript # script #== item) $
      pexpecting (pfromData pversionedScript'language #== pcon PNativeCardanoScript) $
        pforgetData $
          pdata $
            pcon $
              PStep03State
                pstep02State'subject
                pstep02State'badTxId
                pstep02State'badTxWitnessSetHash
                (pdata $ pblake2b_256 # item)
                pstep02State'validityIntervalStart
                pstep02State'validityIntervalEnd

nativeScriptInvalidStep03Validator ::
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
nativeScriptInvalidStep03Validator =
  plam $ \step04ScriptHash computationThreadPolicy fraudProofPolicy fraudProofAddress certificatePolicy ctx ->
    pstep ctx $ \datum redeemer ownOutRef txInfo ->
      pdispatch @_ @PStep03Args computationThreadPolicy datum redeemer ownOutRef txInfo $ \args ->
        pmatch args $ \case
          PDirectFinalize inputIndexD outputIndexD mintRedeemerIndexD scriptItemD openingD -> P.do
            PTxInfo {ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs, ptxInfo'redeemers} <- pmatch txInfo
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
                state <- plet $ pexpectStateAs @PStep03State inputState
                PStep03State
                  { pstep03State'subject
                  , pstep03State'validityIntervalStart
                  , pstep03State'validityIntervalEnd
                  } <-
                  pmatch state
                scriptBytes <- plet $ pnativeScriptBytes # state # pfromData scriptItemD
                addressWalk <-
                  plet $
                    popenedAddressWitnesses
                      # state
                      # pfromData openingD
                      # pfromData ptxInfo'referenceInputs
                      # certificatePolicy
                PPair addressView _start <- pmatch addressWalk
                signerHashes <-
                  plet $
                    pfoldOpenedField @(PBuiltinList PByteString)
                      # addressWalk
                      # pnil
                      # plam
                        ( \acc _index item -> pmatch (pauthenticatedSigner # pfromData pstep03State'subject # item) $ \case
                            PJust hash -> pcons # hash # acc
                            PNothing -> acc
                        )
                pexpecting (plengthBS # scriptBytes #<= pdirectScriptBytesLimit)
                  $ pexpecting (pfieldItemCount # addressView #<= pdirectSignerLimit)
                  $ pmatch
                    ( pevaluateNativeScriptV1
                        # scriptBytes
                        # signerHashes
                        # pfromData pstep03State'validityIntervalStart
                        # pfromData pstep03State'validityIntervalEnd
                    )
                  $ \case
                    PScriptEvaluatedV1 satisfiedD -> Subject.pterminalContradiction # pfromData pstep03State'subject # (pnot # pfromData satisfiedD)
                    _ -> perror
          PStartSignerScan inputIndexD outputIndexD scriptItemD openingD itemBudgetD -> P.do
            PTxInfo {ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs} <- pmatch txInfo
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
                state <- plet $ pexpectStateAs @PStep03State inputState
                PStep03State
                  { pstep03State'subject
                  , pstep03State'badTxId
                  , pstep03State'badTxWitnessSetHash
                  , pstep03State'scriptItemHash
                  , pstep03State'validityIntervalStart
                  , pstep03State'validityIntervalEnd
                  } <-
                  pmatch state
                scriptBytes <- plet $ pnativeScriptBytes # state # pfromData scriptItemD
                addressWalk <-
                  plet $
                    popenedAddressWitnesses
                      # state
                      # pfromData openingD
                      # pfromData ptxInfo'referenceInputs
                      # certificatePolicy
                PPair addressView start <- pmatch addressWalk
                PPair accumulator next <-
                  pmatch $
                    pwalkFold @PSignerAccumulatorV1
                      # addressView
                      # start
                      # itemBudget
                      # pcon (PSignerAccumulatorV1 (pconstant "") 0 pemptyFrontier)
                      # plam
                        ( \acc index item -> pmatch (pauthenticatedSigner # pfromData pstep03State'subject # item) $ \case
                            PJust hash -> pappendSigner # acc # index # hash
                            PNothing -> acc
                        )
                PSignerAccumulatorV1 previousSigner signerCount signerPeaks <- pmatch accumulator
                expectedState <-
                  plet $
                    pcon $
                      PStep04State
                        pstep03State'subject
                        pstep03State'badTxId
                        pstep03State'badTxWitnessSetHash
                        pstep03State'scriptItemHash
                        pstep03State'validityIntervalStart
                        pstep03State'validityIntervalEnd
                        (pdata $ pfieldWalkCheckpointHash # next)
                        (pdata previousSigner)
                        (pdata signerCount)
                        (pdata signerPeaks)
                pexpecting
                  ( pfieldItemCount
                      # addressView
                      #> pdirectSignerLimit
                      #|| plengthBS
                      # scriptBytes
                      #> pdirectScriptBytesLimit
                  )
                  $ pexpecting (outputScriptHash #== step04ScriptHash)
                  $ pexpecting (outputStateData #== pforgetData (pdata expectedState)) (pconstant True)

popenedAddressWitnesses ::
  forall s.
  Term
    s
    ( PStep03State
        :--> PFieldOpeningV1
        :--> PBuiltinList (PAsData PTxInInfo)
        :--> PAsData PCurrencySymbol
        :--> PPair PFieldViewV1 PFieldWalkCheckpointV1
    )
popenedAddressWitnesses = phoistAcyclic $ plam $ \state opening referenceInputs certificatePolicy ->
  pmatch state $ \PStep03State {pstep03State'badTxId, pstep03State'badTxWitnessSetHash} ->
    popenedFieldWalk
      # opening
      # pcon
        ( PWitnessAnchor
            { pwitnessAnchor'txId = pstep03State'badTxId
            , pwitnessAnchor'witnessSetHash = pstep03State'badTxWitnessSetHash
            }
        )
      # paddressWitnessesFieldIndex
      # referenceInputs
      # certificatePolicy

pnativeScriptBytes :: forall s. Term s (PStep03State :--> PByteString :--> PByteString)
pnativeScriptBytes = phoistAcyclic $ plam $ \state scriptItem -> P.do
  PStep03State {pstep03State'scriptItemHash} <- pmatch state
  PPair offset script <- pmatch $ pdecodeMidgardVersionedScriptAt # scriptItem # 0
  PMidgardVersionedScript {pversionedScript'language, pversionedScript'scriptBytes} <- pmatch script
  pexpecting (pblake2b_256 # scriptItem #== pfromData pstep03State'scriptItemHash) $
    pexpecting (offset #== plengthBS # scriptItem) $
      pexpecting (pencodeMidgardVersionedScript # script #== scriptItem) $
        pexpecting (pfromData pversionedScript'language #== pcon PNativeCardanoScript) $
          pfromData pversionedScript'scriptBytes

pappendSigner :: forall s. Term s (PSignerAccumulatorV1 :--> PInteger :--> PByteString :--> PSignerAccumulatorV1)
pappendSigner = phoistAcyclic $ plam $ \accumulator _index signerHash ->
  pmatch accumulator $
    \PSignerAccumulatorV1
       { psignerAccumulator'previousSignerHash = previousSigner
       , psignerAccumulator'signerCount = signerCount
       , psignerAccumulator'signerPeaks = signerPeaks
       } ->
        pexpecting (previousSigner #== pconstant "" #|| previousSigner #<= signerHash) $
          pif
            (previousSigner #== signerHash)
            accumulator
            ( pcon $
                PSignerAccumulatorV1
                  signerHash
                  (signerCount + 1)
                  (pappendLeaf # signerCount # signerPeaks # (psignerLeafHash # signerHash))
            )

pvalidStagedBudget :: forall s. Term s (PInteger :--> PBool)
pvalidStagedBudget = phoistAcyclic $ plam $ \budget ->
  pexpecting (budget #> 0) $
    pexpecting (budget #<= pstagedSignerStartBatchLimit) (pconstant True)

nativeScriptInvalidStep04Validator ::
  forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
nativeScriptInvalidStep04Validator = plam $ \step05ScriptHash computationThreadPolicy certificatePolicy ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
    pdispatch @_ @PStep04Args computationThreadPolicy datum redeemer ownOutRef txInfo $ \action -> P.do
      PTxInfo {ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs} <- pmatch txInfo
      pcontinue
        computationThreadPolicy
        (pexpectDatum datum)
        (pstep04InputIndex # action)
        (pstep04OutputIndex # action)
        ownOutRef
        (pfromData ptxInfo'inputs)
        (pfromData ptxInfo'outputs)
        $ \ownScriptHash _threadName _prover inputState outputScriptHash outputStateData -> P.do
          state <- plet $ pexpectStateAs @PStep04State inputState
          PStep04State
            { pstep04State'subject
            , pstep04State'badTxId
            , pstep04State'badTxWitnessSetHash
            , pstep04State'scriptItemHash
            , pstep04State'validityIntervalStart
            , pstep04State'validityIntervalEnd
            , pstep04State'signerCheckpointHash
            , pstep04State'previousSignerHash
            , pstep04State'signerCount
            , pstep04State'signerPeaks
            } <-
            pmatch state
          PPair addressView resumed <-
            pmatch $
              presumeOpenedFieldWalk
                # (pstep04Opening # action)
                # pcon
                  ( PWitnessAnchor
                      { pwitnessAnchor'txId = pstep04State'badTxId
                      , pwitnessAnchor'witnessSetHash = pstep04State'badTxWitnessSetHash
                      }
                  )
                # paddressWitnessesFieldIndex
                # pfromData pstep04State'signerCheckpointHash
                # (pstep04CheckpointBytes # action)
                # pfromData ptxInfo'referenceInputs
                # certificatePolicy
          PPair accumulator next <-
            pmatch $
              pwalkFold @PSignerAccumulatorV1
                # addressView
                # resumed
                # (pstep04Budget # action)
                # pcon
                  ( PSignerAccumulatorV1
                      (pfromData pstep04State'previousSignerHash)
                      (pfromData pstep04State'signerCount)
                      (pfromData pstep04State'signerPeaks)
                  )
                # plam
                  ( \acc index item -> pmatch (pauthenticatedSigner # pfromData pstep04State'subject # item) $ \case
                      PJust hash -> pappendSigner # acc # index # hash
                      PNothing -> acc
                  )
          PSignerAccumulatorV1
            { psignerAccumulator'previousSignerHash = previousSigner
            , psignerAccumulator'signerCount = signerCount
            , psignerAccumulator'signerPeaks = signerPeaks
            } <-
            pmatch accumulator
          pmatch action $ \case
            PResumeSignerScan _ _ _ _ itemBudgetD ->
              pexpecting (pfromData itemBudgetD #> 0) $
                pexpecting (pfromData itemBudgetD #<= pstagedSignerResumeBatchLimit) $
                  pexpecting (pnot #$ pwalkIsComplete # next) $
                    pexpecting (outputScriptHash #== ownScriptHash) $
                      pexpecting
                        ( outputStateData
                            #== pforgetData
                              ( pdata $
                                  pcon $
                                    PStep04State
                                      pstep04State'subject
                                      pstep04State'badTxId
                                      pstep04State'badTxWitnessSetHash
                                      pstep04State'scriptItemHash
                                      pstep04State'validityIntervalStart
                                      pstep04State'validityIntervalEnd
                                      (pdata $ pfieldWalkCheckpointHash # next)
                                      (pdata previousSigner)
                                      (pdata signerCount)
                                      (pdata signerPeaks)
                              )
                        )
                        (pconstant True)
            PFinalizeSignerScan _ _ _ _ itemBudgetD ->
              pexpecting (pfromData itemBudgetD #>= 0) $
                pexpecting (pfromData itemBudgetD #<= pstagedSignerFinalizeBatchLimit) $
                  pexpecting (pwalkIsComplete # next) $
                    pexpecting (outputScriptHash #== step05ScriptHash) $
                      pexpecting
                        ( outputStateData
                            #== pforgetData
                              ( pdata $
                                  pcon $
                                    PStep05State
                                      pstep04State'subject
                                      pstep04State'badTxId
                                      pstep04State'scriptItemHash
                                      pstep04State'validityIntervalStart
                                      pstep04State'validityIntervalEnd
                                      (pdata signerCount)
                                      (pdata signerPeaks)
                                      (pdata $ pcon PScriptReady)
                              )
                        )
                        (pconstant True)

pstep04InputIndex :: forall s. Term s (PStep04Args :--> PInteger)
pstep04InputIndex = phoistAcyclic $ plam $ \action -> pmatch action $ \case
  PResumeSignerScan inputIndexD _ _ _ _ -> pfromData inputIndexD
  PFinalizeSignerScan inputIndexD _ _ _ _ -> pfromData inputIndexD

pstep04OutputIndex :: forall s. Term s (PStep04Args :--> PInteger)
pstep04OutputIndex = phoistAcyclic $ plam $ \action -> pmatch action $ \case
  PResumeSignerScan _ outputIndexD _ _ _ -> pfromData outputIndexD
  PFinalizeSignerScan _ outputIndexD _ _ _ -> pfromData outputIndexD

pstep04Opening :: forall s. Term s (PStep04Args :--> PFieldOpeningV1)
pstep04Opening = phoistAcyclic $ plam $ \action -> pmatch action $ \case
  PResumeSignerScan _ _ openingD _ _ -> pfromData openingD
  PFinalizeSignerScan _ _ openingD _ _ -> pfromData openingD

pstep04CheckpointBytes :: forall s. Term s (PStep04Args :--> PByteString)
pstep04CheckpointBytes = phoistAcyclic $ plam $ \action -> pmatch action $ \case
  PResumeSignerScan _ _ _ checkpointD _ -> pfromData checkpointD
  PFinalizeSignerScan _ _ _ checkpointD _ -> pfromData checkpointD

pstep04Budget :: forall s. Term s (PStep04Args :--> PInteger)
pstep04Budget = phoistAcyclic $ plam $ \action -> pmatch action $ \case
  PResumeSignerScan _ _ _ _ budgetD -> pfromData budgetD
  PFinalizeSignerScan _ _ _ _ budgetD -> pfromData budgetD

nativeScriptInvalidStep05Validator ::
  forall s.
  Term
    s
    ( PAsData PCurrencySymbol
        :--> PAsData PCurrencySymbol
        :--> PAsData PAddress
        :--> PScriptContext
        :--> PUnit
    )
nativeScriptInvalidStep05Validator = plam $ \computationThreadPolicy fraudProofPolicy fraudProofAddress ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
    pdispatch @_ @PStep05Args computationThreadPolicy datum redeemer ownOutRef txInfo $ \action ->
      pif
        (pstep05IsFinalizing # action)
        ( P.do
            PTxInfo {ptxInfo'inputs, ptxInfo'outputs, ptxInfo'redeemers} <- pmatch txInfo
            pfinalize
              computationThreadPolicy
              fraudProofPolicy
              fraudProofAddress
              (pexpectDatum datum)
              (pstep05InputIndex # action)
              (pstep05OutputIndex # action)
              (pstep05MintRedeemerIndex # action)
              ownOutRef
              (pfromData ptxInfo'inputs)
              (pfromData ptxInfo'outputs)
              (pto $ pto $ pfromData ptxInfo'redeemers)
              $ \_ownScriptHash _threadName _prover inputState -> P.do
                state <- plet $ pexpectStateAs @PStep05State inputState
                terminal <- plet $ prunStep05Action # state # action
                pexpecting (pnativeScriptWalkIsComplete # terminal) $
                  pmatch (pnativeScriptVerdict # terminal) $ \case
                    PJust verdict -> pmatch state $ \PStep05State {pstep05State'subject} ->
                      Subject.pterminalContradiction # pfromData pstep05State'subject # (pnot # verdict)
                    PNothing -> perror
        )
        ( P.do
            PTxInfo {ptxInfo'inputs, ptxInfo'outputs} <- pmatch txInfo
            pcontinue
              computationThreadPolicy
              (pexpectDatum datum)
              (pstep05InputIndex # action)
              (pstep05OutputIndex # action)
              ownOutRef
              (pfromData ptxInfo'inputs)
              (pfromData ptxInfo'outputs)
              $ \ownScriptHash _threadName _prover inputState outputScriptHash outputStateData -> P.do
                state <- plet $ pexpectStateAs @PStep05State inputState
                PStep05State
                  { pstep05State'subject
                  , pstep05State'badTxId
                  , pstep05State'scriptItemHash
                  , pstep05State'validityIntervalStart
                  , pstep05State'validityIntervalEnd
                  , pstep05State'signerCount
                  , pstep05State'signerPeaks
                  } <-
                  pmatch state
                next <- plet $ prunStep05Action # state # action
                expectedState <-
                  plet $
                    pcon $
                      PStep05State
                        pstep05State'subject
                        pstep05State'badTxId
                        pstep05State'scriptItemHash
                        pstep05State'validityIntervalStart
                        pstep05State'validityIntervalEnd
                        pstep05State'signerCount
                        pstep05State'signerPeaks
                        (pdata $ pcon $ PScriptWalk $ pdata $ pnativeScriptCursorHash # next)
                pexpecting (pnot #$ pnativeScriptWalkIsComplete # next) $
                  pexpecting (outputScriptHash #== ownScriptHash) $
                    pexpecting (outputStateData #== pforgetData (pdata expectedState)) (pconstant True)
        )

prunStep05Action :: forall s. Term s (PStep05State :--> PStep05Args :--> PNativeScriptWalkV1)
prunStep05Action = phoistAcyclic $ plam $ \state action -> P.do
  PStep05State
    { pstep05State'scriptItemHash
    , pstep05State'validityIntervalStart
    , pstep05State'validityIntervalEnd
    , pstep05State'signerCount
    , pstep05State'signerPeaks
    , pstep05State'phase
    } <-
    pmatch state
  scriptItem <- plet $ pstep05ScriptItem # action
  PPair offset script <- pmatch $ pdecodeMidgardVersionedScriptAt # scriptItem # 0
  PMidgardVersionedScript {pversionedScript'language, pversionedScript'scriptBytes} <- pmatch script
  scriptBytes <- plet $ pfromData pversionedScript'scriptBytes
  nodeBudget <- plet $ pstep05NodeBudget # action
  start <-
    plet $
      pmatch action $ \case
        PStartScriptScan {} ->
          pexpecting (pfromData pstep05State'phase #== pcon PScriptReady) $
            popenNativeScriptWalk # scriptBytes
        PStartScriptFinalize {} ->
          pexpecting (pfromData pstep05State'phase #== pcon PScriptReady) $
            popenNativeScriptWalk # scriptBytes
        PResumeScriptScan _ _ _ cursorD framesD _ _ ->
          pmatch (pfromData pstep05State'phase) $ \case
            PScriptWalk cursorHashD ->
              presumeNativeScriptWalkFromCommitment
                # pfromData cursorHashD
                # pfromData cursorD
                # pfromData framesD
                # scriptBytes
            PScriptReady -> perror
        PFinalizeScriptScan _ _ _ _ cursorD framesD _ _ ->
          pmatch (pfromData pstep05State'phase) $ \case
            PScriptWalk cursorHashD ->
              presumeNativeScriptWalkFromCommitment
                # pfromData cursorHashD
                # pfromData cursorD
                # pfromData framesD
                # scriptBytes
            PScriptReady -> perror
  pexpecting (pblake2b_256 # scriptItem #== pfromData pstep05State'scriptItemHash) $
    pexpecting (offset #== plengthBS # scriptItem) $
      pexpecting (pencodeMidgardVersionedScript # script #== scriptItem) $
        pexpecting (pfromData pversionedScript'language #== pcon PNativeCardanoScript) $
          pexpecting (nodeBudget #> 0) $
            pexpecting (nodeBudget #<= pstagedNodeBatchLimit) $
              pnativeScriptRunWithSignerVerdict
                # start
                # scriptBytes
                # pfromData pstep05State'validityIntervalStart
                # pfromData pstep05State'validityIntervalEnd
                # nodeBudget
                # ( pauthenticatedSignerVerdict
                      # pfromData pstep05State'signerCount
                      # pfromData pstep05State'signerPeaks
                      # (pstep05SignerQueries # action)
                  )

pstep05InputIndex :: forall s. Term s (PStep05Args :--> PInteger)
pstep05InputIndex = phoistAcyclic $ plam $ \action -> pmatch action $ \case
  PStartScriptScan inputD _ _ _ _ -> pfromData inputD
  PResumeScriptScan inputD _ _ _ _ _ _ -> pfromData inputD
  PStartScriptFinalize inputD _ _ _ _ _ -> pfromData inputD
  PFinalizeScriptScan inputD _ _ _ _ _ _ _ -> pfromData inputD

pstep05OutputIndex :: forall s. Term s (PStep05Args :--> PInteger)
pstep05OutputIndex = phoistAcyclic $ plam $ \action -> pmatch action $ \case
  PStartScriptScan _ outputD _ _ _ -> pfromData outputD
  PResumeScriptScan _ outputD _ _ _ _ _ -> pfromData outputD
  PStartScriptFinalize _ outputD _ _ _ _ -> pfromData outputD
  PFinalizeScriptScan _ outputD _ _ _ _ _ _ -> pfromData outputD

pstep05MintRedeemerIndex :: forall s. Term s (PStep05Args :--> PInteger)
pstep05MintRedeemerIndex = phoistAcyclic $ plam $ \action -> pmatch action $ \case
  PStartScriptFinalize _ _ mintD _ _ _ -> pfromData mintD
  PFinalizeScriptScan _ _ mintD _ _ _ _ _ -> pfromData mintD
  _ -> perror

pstep05ScriptItem :: forall s. Term s (PStep05Args :--> PByteString)
pstep05ScriptItem = phoistAcyclic $ plam $ \action -> pmatch action $ \case
  PStartScriptScan _ _ itemD _ _ -> pfromData itemD
  PResumeScriptScan _ _ itemD _ _ _ _ -> pfromData itemD
  PStartScriptFinalize _ _ _ itemD _ _ -> pfromData itemD
  PFinalizeScriptScan _ _ _ itemD _ _ _ _ -> pfromData itemD

pstep05NodeBudget :: forall s. Term s (PStep05Args :--> PInteger)
pstep05NodeBudget = phoistAcyclic $ plam $ \action -> pmatch action $ \case
  PStartScriptScan _ _ _ budgetD _ -> pfromData budgetD
  PResumeScriptScan _ _ _ _ _ budgetD _ -> pfromData budgetD
  PStartScriptFinalize _ _ _ _ budgetD _ -> pfromData budgetD
  PFinalizeScriptScan _ _ _ _ _ _ budgetD _ -> pfromData budgetD

pstep05SignerQueries :: forall s. Term s (PStep05Args :--> PBuiltinList (PAsData PSignerQueryV1))
pstep05SignerQueries = phoistAcyclic $ plam $ \action -> pmatch action $ \case
  PStartScriptScan _ _ _ _ queriesD -> pfromData queriesD
  PResumeScriptScan _ _ _ _ _ _ queriesD -> pfromData queriesD
  PStartScriptFinalize _ _ _ _ _ queriesD -> pfromData queriesD
  PFinalizeScriptScan _ _ _ _ _ _ _ queriesD -> pfromData queriesD

pstep05IsFinalizing :: forall s. Term s (PStep05Args :--> PBool)
pstep05IsFinalizing = phoistAcyclic $ plam $ \action -> pmatch action $ \case
  PStartScriptFinalize {} -> pconstant True
  PFinalizeScriptScan {} -> pconstant True
  _ -> pconstant False

pauthenticatedSignerVerdict ::
  forall s.
  Term
    s
    ( PInteger
        :--> PBuiltinList (PAsData PFrontierPeak)
        :--> PBuiltinList (PAsData PSignerQueryV1)
        :--> PByteString
        :--> PBool
    )
pauthenticatedSignerVerdict = phoistAcyclic $ pfix $ \self -> plam $ \signerCount signerPeaks queries signerHash ->
  pexpecting (plengthBS # signerHash #== 28) $
    pelimList
      ( \queryD rest ->
          pmatch (pfromData queryD) $
            \PSignerQueryV1
               { psignerQuery'signerHash = queryHashD
               , psignerQuery'proof = proofD
               } ->
                pif
                  (pfromData queryHashD #== signerHash)
                  (psignerProofVerdict # signerHash # signerCount # signerPeaks # pfromData proofD)
                  (self # signerCount # signerPeaks # rest # signerHash)
      )
      perror
      queries

psignerFrontierMatches :: forall s. Term s (PInteger :--> PByteString :--> PBuiltinList (PAsData PFrontierPeak) :--> PBool)
psignerFrontierMatches = phoistAcyclic $ plam $ \signerCount commitment peaks ->
  pfrontierIsWellFormed
    # signerCount
    # peaks
    #&& pfrontierCommitment
    # signerCount
    # peaks
    #== commitment

psignerMembershipIsValid ::
  forall s.
  Term
    s
    ( PByteString
        :--> PInteger
        :--> PByteString
        :--> PBuiltinList (PAsData PFrontierPeak)
        :--> PInteger
        :--> PBuiltinList (PAsData PByteString)
        :--> PBool
    )
psignerMembershipIsValid = phoistAcyclic $ plam $ \signerHash signerCount commitment peaks signerIndex siblings ->
  plengthBS
    # signerHash
    #== 28
    #&& psignerFrontierMatches
    # signerCount
    # commitment
    # peaks
    #&& pverifyMembership
    # signerCount
    # peaks
    # signerIndex
    # (psignerLeafHash # signerHash)
    # siblings

psignerProofVerdict ::
  forall s.
  Term
    s
    ( PByteString
        :--> PInteger
        :--> PBuiltinList (PAsData PFrontierPeak)
        :--> PSignerSetProofV1
        :--> PBool
    )
psignerProofVerdict = phoistAcyclic $ plam $ \signerHash signerCount signerPeaks proof ->
  plet (pfrontierCommitment # signerCount # signerPeaks) $ \commitment ->
    pmatch proof $ \case
      PSignerMembershipProof peaksD signerIndexD siblingsD ->
        pexpecting (pfromData peaksD #== signerPeaks) $
          pexpecting
            ( psignerMembershipIsValid
                # signerHash
                # signerCount
                # commitment
                # pfromData peaksD
                # pfromData signerIndexD
                # pfromData siblingsD
            )
            (pconstant True)
      PEmptySignerSetProof peaksD ->
        pexpecting (signerCount #== 0) $
          pexpecting (psignerFrontierMatches # signerCount # commitment # pfromData peaksD) (pconstant False)
      PSignerBelowFirstProof peaksD firstHashD siblingsD ->
        pexpecting (signerHash #< pfromData firstHashD) $
          pexpecting
            ( psignerMembershipIsValid
                # pfromData firstHashD
                # signerCount
                # commitment
                # pfromData peaksD
                # 0
                # pfromData siblingsD
            )
            (pconstant False)
      PSignerAboveLastProof peaksD lastHashD siblingsD ->
        pexpecting (signerCount #> 0) $
          pexpecting (pfromData lastHashD #< signerHash) $
            pexpecting
              ( psignerMembershipIsValid
                  # pfromData lastHashD
                  # signerCount
                  # commitment
                  # pfromData peaksD
                  # (signerCount - 1)
                  # pfromData siblingsD
              )
              (pconstant False)
      PSignerBetweenProof peaksD lowerIndexD lowerHashD lowerSiblingsD upperHashD upperSiblingsD ->
        pexpecting (pfromData lowerIndexD #>= 0)
          $ pexpecting (pfromData lowerIndexD + 1 #< signerCount)
          $ pexpecting (pfromData lowerHashD #< signerHash)
          $ pexpecting (signerHash #< pfromData upperHashD)
          $ pexpecting
            ( psignerMembershipIsValid
                # pfromData lowerHashD
                # signerCount
                # commitment
                # pfromData peaksD
                # pfromData lowerIndexD
                # pfromData lowerSiblingsD
            )
          $ pexpecting
            ( psignerMembershipIsValid
                # pfromData upperHashD
                # signerCount
                # commitment
                # pfromData peaksD
                # (pfromData lowerIndexD + 1)
                # pfromData upperSiblingsD
            )
            (pconstant False)
      PNoSignerSetProof -> perror
