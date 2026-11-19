module Midgard.Validators.FraudProofs.ValueUnion where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Midgard.Common.Types (PProof)
import Midgard.FraudProofs.Common (PNativeTxInclusionCarriage, pcontinue, pfinalize, ppassNativeTxToNextStepCarried)
import Midgard.FraudProofs.FieldOpening (PFieldOpeningV1, PNativeTxAnchorV1 (..), popenedCertifiedFieldWalkFromGrammar, popenedFieldGrammarCertification, popenedFieldView, presumeOpenedFieldGrammarCertification, presumeOpenedFieldWalk)
import Midgard.FraudProofs.NativeTx.Compact (pverifyNativeTxProofSourceV1)
import Midgard.FraudProofs.NativeTx.Components (pencodeMidgardTxInput)
import Midgard.FraudProofs.NativeTx.Types (PNativeTxBodyCompact (..), PNativeTxCompact (..), PVerifiedMidgardNativeTxCompact (..))
import Midgard.FraudProofs.ProofThreadSubstrate (PVerdictSubject (..), pbindForcedSubjectToThread, prejectionReasonOf)
import Midgard.FraudProofs.ValueNotPreserved (pclaimedAssetIsWellFormedV1, pfraudCategoryIsValueNotPreservedV1)
import Midgard.FraudProofs.ValueUnion
import Midgard.LedgerOutputCommitment
import Midgard.LedgerOutputScan qualified as Scan
import Midgard.LedgerState
import Midgard.MpfProof qualified as Mpf
import Midgard.NativeTxFieldAccess (pfieldReadRange, pfieldTotalLength)
import Midgard.NativeTxMachineWalk (pspendInputAt, pspendInputCount)
import Midgard.NativeTxMachineWalk qualified as Walk
import Midgard.RejectionReason (PRejectionReasonV1 (PValueNotPreserved))
import Midgard.TransitionTrace
import Midgard.ValidationMerkle qualified as Frontier
import Midgard.Validators.FraudProofs.Step (pdispatch, pexpectDatum, pexpectStateAs, pexpecting, pstep)
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.LedgerApi.V3 (PAddress, PCurrencySymbol, PScriptContext, PScriptHash, PTxInfo (..))
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

data PAssetAction s
  = PSelect
      (Term s (PAsData PByteString))
      (Term s (PAsData PByteString))
      (Term s (PAsData PInteger))
      (Term s (PAsData (PBuiltinList (PAsData Frontier.PFrontierPeak))))
      (Term s (PAsData (PBuiltinList (PAsData PByteString))))
  | PFinish
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PAssetAction)

data PForcedSourceArgs s = PForcedSourceArgs (Term s (PAsData PInteger)) (Term s (PAsData PInteger)) (Term s (PAsData PHeaderV1)) (Term s (PAsData PRootMembershipProof))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PForcedSourceArgs)

data PMembershipArgs s = PMembershipArgs (Term s (PAsData PInteger)) (Term s (PAsData PInteger)) (Term s (PAsData PRootMembershipProof))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PMembershipArgs)

data PInputsArgs s = PInputsArgs (Term s (PAsData PInteger)) (Term s (PAsData PInteger)) (Term s (PAsData PFieldOpeningV1))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PInputsArgs)

data PInputValueArgs s = PInputValueArgs (Term s (PAsData PInteger)) (Term s (PAsData PInteger)) (Term s (PAsData PByteString)) (Term s (PAsData PProof))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PInputValueArgs)

data PAssetsArgs s = PAssetsArgs (Term s (PAsData PInteger)) (Term s (PAsData PInteger)) (Term s (PAsData PAssetAction))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PAssetsArgs)

data PUpdateArgs s = PUpdateArgs (Term s (PAsData PInteger)) (Term s (PAsData PInteger)) (Term s (PAsData PInteger)) (Term s (PAsData PProof))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PUpdateArgs)

data PTerminalArgs s = PTerminalArgs (Term s (PAsData PInteger)) (Term s (PAsData PInteger)) (Term s (PAsData PInteger)) (Term s (PMaybeData PAssetDeltaWitness))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PTerminalArgs)

phaseOf :: forall s. Term s PEventKey -> Term s PTransitionPhase
phaseOf key = pmatch key $ \case PL2TransactionEventKey _ -> pcon PL2Transaction; PForcedTransactionEventKey _ -> pcon PForcedTransaction; _ -> perror

valueUnionAcceptedSourceValidator :: forall s. Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PAsData PScriptHash :--> PScriptContext :--> PUnit)
valueUnionAcceptedSourceValidator = plam $ \eventHash threadPolicy hub ctx ->
  pstep ctx $ \datum redeemer ownRef tx -> pdispatch @_ @PNativeTxInclusionCarriage threadPolicy datum redeemer ownRef tx $ \carriage -> P.do
    PTxInfo{ptxInfo'inputs, ptxInfo'outputs, ptxInfo'referenceInputs, ptxInfo'redeemers} <- pmatch tx
    ppassNativeTxToNextStepCarried threadPolicy hub datum carriage ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'referenceInputs) (pfromData ptxInfo'outputs) (pto $ pto $ pfromData ptxInfo'redeemers) $ \_ _ _ prior outputHash outputState header txId verified -> P.do
      claim <- plet $ pexpectStateAs @PConservationClaim prior
      PAcceptedImbalance asset _ <- pmatch claim
      PVerifiedMidgardNativeTxCompact{pverified'txCompact} <- pmatch verified
      PNativeTxCompact{pcompact'body, pcompact'validityCode} <- pmatch pverified'txCompact
      PNativeTxBodyCompact{pbodyCompact'fee} <- pmatch pcompact'body
      h <- pmatch $ pfromData header
      let expected =
            pcon $
              PSourceState
                (pdata txId)
                (pdata claim)
                (pdata pbodyCompact'fee)
                (pdata $ pcon $ PL2TransactionEventKey $ pdata txId)
                (pheader'eventToStepRoot h)
                (pheader'totalEventCount h)
                (pheader'transitionTraceRoot h)
                (pheader'transitionStepCount h)
      pclaimedAssetIsWellFormedV1 # pfromData asset #&& pcompact'validityCode #== 0 #&& outputHash #== eventHash #&& outputState #== pforgetData (pdata expected)

valueUnionForcedSourceValidator :: forall s. Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
valueUnionForcedSourceValidator = plam $ \eventHash threadPolicy ctx ->
  pstep ctx $ \datum redeemer ownRef tx -> pdispatch @_ @PForcedSourceArgs threadPolicy datum redeemer ownRef tx $ \args -> P.do
    PForcedSourceArgs i o headerD membershipD <- pmatch args
    PTxInfo{ptxInfo'inputs, ptxInfo'outputs, ptxInfo'referenceInputs} <- pmatch tx
    pcontinue threadPolicy (pexpectDatum datum) (pfromData i) (pfromData o) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) $ \ownHash threadName _ prior outputHash outputState -> P.do
      claim <- plet $ pexpectStateAs @PConservationClaim prior
      pForced <- plet $ claim #== pcon PForcedConservation
      header <- plet $ pfromData headerD
      membership <- plet $ pfromData membershipD
      subject <- plet $ pbindForcedSubjectToThread # (pto $ pfromData threadName) # header # membership # 1
      PVerdictSubject{psubject'transactionId} <- pmatch subject
      m <- pmatch membership
      PForcedInclusionTxV1{pforcedTx'source} <- pmatch $ punsafeCoerce @PForcedInclusionTxV1 $ prootMembership'value m
      PNativeTxProofSourceV1 compact witness lengths <- pmatch $ pfromData pforcedTx'source
      PPair verified _ <- pmatch $ pverifyNativeTxProofSourceV1 # pfromData psubject'transactionId # pfromData compact # pfromData witness # pfromData lengths
      PVerifiedMidgardNativeTxCompact{pverified'txCompact} <- pmatch verified
      PNativeTxCompact{pcompact'body} <- pmatch pverified'txCompact
      PNativeTxBodyCompact{pbodyCompact'fee} <- pmatch pcompact'body
      h <- pmatch header
      let expected =
            pcon $
              PSourceState
                psubject'transactionId
                (pdata claim)
                (pdata pbodyCompact'fee)
                (pdata $ pcon $ PForcedTransactionEventKey $ punsafeCoerce $ prootMembership'key m)
                (pheader'eventToStepRoot h)
                (pheader'totalEventCount h)
                (pheader'transitionTraceRoot h)
                (pheader'transitionStepCount h)
      pForced #&& prejectionReasonOf # subject #== pcon PValueNotPreserved #&& outputHash #== eventHash #&& outputState #== pforgetData (pdata expected)

valueUnionEventValidator :: forall s. Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
valueUnionEventValidator = plam $ \preStateHash threadPolicy ctx ->
  pstep ctx $ \datum redeemer ownRef tx -> pdispatch @_ @PMembershipArgs threadPolicy datum redeemer ownRef tx $ \args -> P.do
    PMembershipArgs i o membershipD <- pmatch args
    PTxInfo{ptxInfo'inputs, ptxInfo'outputs, ptxInfo'referenceInputs} <- pmatch tx
    pcontinue threadPolicy (pexpectDatum datum) (pfromData i) (pfromData o) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) $ \ownHash threadName _ prior outputHash outputState -> P.do
      state <- plet $ pexpectStateAs @PSourceState prior
      s <- pmatch state
      membership <- plet $ pfromData membershipD
      m <- pmatch membership
      PEventToStepValue index phase <- pmatch $ punsafeCoerce @PEventToStepValue $ prootMembership'value m
      let expected = pcon $ PEventState (psourceState'transactionId s) (psourceState'claim s) (psourceState'fee s) (psourceState'eventKey s) (psourceState'traceRoot s) (psourceState'traceCount s) index
      pverifyRootMembershipWithBytes membership (pdata $ pcon PEventToStepRootDomain) (pfromData $ psourceState'eventRoot s) (pfromData $ psourceState'eventCount s) (pserialiseData # prootMembership'key m) (pserialiseData # prootMembership'value m)
        #&& prootMembership'key m
        #== pforgetData (psourceState'eventKey s)
        #&& pfromData phase
        #== phaseOf (pfromData $ psourceState'eventKey s)
        #&& pfromData index
        #>= 0
        #&& pfromData index
        #< pfromData (psourceState'traceCount s)
        #&& outputHash
        #== preStateHash
        #&& outputState
        #== pforgetData (pdata expected)

valueUnionPreStateValidator :: forall s. Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
valueUnionPreStateValidator = plam $ \inputsHash threadPolicy ctx ->
  pstep ctx $ \datum redeemer ownRef tx -> pdispatch @_ @PMembershipArgs threadPolicy datum redeemer ownRef tx $ \args -> P.do
    PMembershipArgs i o membershipD <- pmatch args
    PTxInfo{ptxInfo'inputs, ptxInfo'outputs, ptxInfo'referenceInputs} <- pmatch tx
    pcontinue threadPolicy (pexpectDatum datum) (pfromData i) (pfromData o) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) $ \ownHash threadName _ prior outputHash outputState -> P.do
      state <- plet $ pexpectStateAs @PEventState prior
      s <- pmatch state
      membership <- plet $ pfromData membershipD
      m <- pmatch membership
      t <- pmatch $ punsafeCoerce @PTransitionStep $ prootMembership'value m
      let expected = pcon $ PFoldState (pdata $ pcon $ PBalanceState (peventState'transactionId s) (peventState'claim s) (ptransitionStep'preUtxosRoot t) (pdata $ 0 - pfromData (peventState'fee s)) (pdata pemptyDeltaRoot)) (pforgetData $ pdata $ pcon $ PInputs $ pdata 0)
      pverifyRootMembershipWithBytes membership (pdata $ pcon PTransitionTraceRootDomain) (pfromData $ peventState'traceRoot s) (pfromData $ peventState'traceCount s) (pserialiseData # prootMembership'key m) (pserialiseData # prootMembership'value m)
        #&& prootMembership'key m
        #== pforgetData (peventState'stepIndex s)
        #&& ptransitionStep'stepIndex t
        #== peventState'stepIndex s
        #&& pfromData (ptransitionStep'schemaVersion t)
        #== ptransitionStepSchemaVersionV1
        #&& ptransitionStep'eventKey t
        #== peventState'eventKey s
        #&& pfromData (ptransitionStep'phase t)
        #== phaseOf (pfromData $ peventState'eventKey s)
        #&& outputHash
        #== inputsHash
        #&& outputState
        #== pforgetData (pdata expected)

valueUnionInputsValidator :: forall s. Term s (PAsData PScriptHash :--> PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
valueUnionInputsValidator = plam $ \inputValueHash grammarHash certificatePolicy threadPolicy ctx ->
  pstep ctx $ \datum redeemer ownRef tx -> pdispatch @_ @PInputsArgs threadPolicy datum redeemer ownRef tx $ \args -> P.do
    PInputsArgs i o opening <- pmatch args
    PTxInfo{ptxInfo'inputs, ptxInfo'outputs, ptxInfo'referenceInputs} <- pmatch tx
    pcontinue threadPolicy (pexpectDatum datum) (pfromData i) (pfromData o) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) $ \ownHash threadName _ prior outputHash outputState -> P.do
      state <- plet $ pexpectStateAs @PFoldState prior
      s <- pmatch state
      b <- pmatch $ pfromData $ pfoldState'balance s
      PInputs cursorD <- pmatch $ punsafeCoerce @PInputs $ pfoldState'continuation s
      view <- plet $ popenedFieldView # pfromData opening # pcon (PBodyAnchor $ pbalanceState'transactionId b) # 0 # pfromData ptxInfo'referenceInputs # certificatePolicy
      count <- plet $ pspendInputCount # view
      let cursor = pfromData cursorD
      pif
        (cursor #>= 0 #&& cursor #<= count)
        ( pif
            (cursor #< count)
            ( let selected = pcon $ PSelectedInput (pdata $ pspendInputAt # view # cursor) cursorD (punsafeCoerce ownHash)
                  expected = pcon s{pfoldState'continuation = pforgetData $ pdata selected}
               in outputHash #== inputValueHash #&& outputState #== pforgetData (pdata expected)
            )
            ( let grammar = pcon $ PFieldGrammar (pdata 2) (pcon PDNothing)
                  expected = pcon s{pfoldState'continuation = pforgetData $ pdata grammar}
               in outputHash #== grammarHash #&& outputState #== pforgetData (pdata expected)
            )
        )
        perror

valueUnionInputValueValidator :: forall s. Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
valueUnionInputValueValidator = plam $ \assetsHash threadPolicy ctx ->
  pstep ctx $ \datum redeemer ownRef tx -> pdispatch @_ @PInputValueArgs threadPolicy datum redeemer ownRef tx $ \args -> P.do
    PInputValueArgs i o descriptorD proof <- pmatch args
    PTxInfo{ptxInfo'inputs, ptxInfo'outputs, ptxInfo'referenceInputs} <- pmatch tx
    pcontinue threadPolicy (pexpectDatum datum) (pfromData i) (pfromData o) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) $ \ownHash threadName _ prior outputHash outputState -> P.do
      state <- plet $ pexpectStateAs @PFoldState prior
      s <- pmatch state
      b <- pmatch $ pfromData $ pfoldState'balance s
      selected <- pmatch $ punsafeCoerce @PSelectedInput $ pfoldState'continuation s
      pIfMember <- plet $ Mpf.phasV1 # pfromData (pbalanceState'preUtxosRoot b) # (pencodeMidgardTxInput # pfromData (pselectedInput'input selected)) # pfromData descriptorD # pfromData proof
      d <- pmatch $ pdecodeLedgerOutputCommitment # pfromData descriptorD
      let next = pforgetData $ pdata $ pcon $ PInputs $ pdata $ pfromData (pselectedInput'cursor selected) + 1
          cursor = pcon $ PAssetCursor (poutputCommitment'assetCount d) (poutputCommitment'assetFrontierCommitment d) (pdata 0) (pdata 1) (pselectedInput'selectorHash selected) next
          balance = pcon b{pbalanceState'lovelaceDelta = pdata $ pfromData (pbalanceState'lovelaceDelta b) + pfromData (poutputCommitment'lovelace d)}
          expected = pcon $ PFoldState (pdata balance) (pforgetData $ pdata cursor)
      pIfMember #&& outputHash #== assetsHash #&& outputState #== pforgetData (pdata expected)

valueUnionAssetsValidator :: forall s. Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
valueUnionAssetsValidator = plam $ \updateHash threadPolicy ctx ->
  pstep ctx $ \datum redeemer ownRef tx -> pdispatch @_ @PAssetsArgs threadPolicy datum redeemer ownRef tx $ \args -> P.do
    PAssetsArgs i o action <- pmatch args
    PTxInfo{ptxInfo'inputs, ptxInfo'outputs, ptxInfo'referenceInputs} <- pmatch tx
    pcontinue threadPolicy (pexpectDatum datum) (pfromData i) (pfromData o) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) $ \ownHash threadName _ prior outputHash outputState -> P.do
      state <- plet $ pexpectStateAs @PFoldState prior
      s <- pmatch state
      cursor <- pmatch $ punsafeCoerce @PAssetCursor $ pfoldState'continuation s
      pmatch (pfromData action) $ \case
        PFinish ->
          let expected = pcon s{pfoldState'continuation = passetCursor'nextContinuation cursor}
           in passetCursor'cursor cursor #== passetCursor'count cursor #&& pto (pfromData outputHash) #== pfromData (passetCursor'nextScriptHash cursor) #&& outputState #== pforgetData (pdata expected)
        PSelect policy name quantity peaks siblings ->
          let next = pforgetData $ pdata $ pcon cursor{passetCursor'cursor = pdata $ pfromData (passetCursor'cursor cursor) + 1}
              pending = pcon $ PPendingContribution (pdata $ pfromData policy <> pfromData name) (pdata $ pfromData (passetCursor'quantitySign cursor) * pfromData quantity) (punsafeCoerce ownHash) next
              expected = pcon s{pfoldState'continuation = pforgetData $ pdata pending}
           in pfromData (passetCursor'cursor cursor)
                #< pfromData (passetCursor'count cursor)
                #&& Frontier.pfrontierCommitment
                # pfromData (passetCursor'count cursor)
                # pfromData peaks
                #== pfromData (passetCursor'frontierCommitment cursor)
                #&& Frontier.pverifyMembership
                # pfromData (passetCursor'count cursor)
                # pfromData peaks
                # pfromData (passetCursor'cursor cursor)
                # (passetLeafHash # pfromData policy # pfromData name # pfromData quantity)
                # pfromData siblings
                #&& outputHash
                #== updateHash
                #&& outputState
                #== pforgetData (pdata expected)

valueUnionUpdateValidator :: forall s. Term s (PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
valueUnionUpdateValidator = plam $ \threadPolicy ctx ->
  pstep ctx $ \datum redeemer ownRef tx -> pdispatch @_ @PUpdateArgs threadPolicy datum redeemer ownRef tx $ \args -> P.do
    PUpdateArgs i o old proof <- pmatch args
    PTxInfo{ptxInfo'inputs, ptxInfo'outputs, ptxInfo'referenceInputs} <- pmatch tx
    pcontinue threadPolicy (pexpectDatum datum) (pfromData i) (pfromData o) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) $ \ownHash threadName _ prior outputHash outputState -> P.do
      state <- plet $ pexpectStateAs @PFoldState prior
      s <- pmatch state
      b <- pmatch $ pfromData $ pfoldState'balance s
      pending <- pmatch $ punsafeCoerce @PPendingContribution $ pfoldState'continuation s
      root <- plet $ papplyContribution # pfromData (pbalanceState'assetDeltaRoot b) # pfromData (ppendingContribution'unit pending) # pfromData (ppendingContribution'quantity pending) # pfromData old # pfromData proof
      let expected = pcon $ PFoldState (pdata $ pcon b{pbalanceState'assetDeltaRoot = pdata root}) (ppendingContribution'nextContinuation pending)
      pto (pfromData outputHash) #== pfromData (ppendingContribution'returnScriptHash pending) #&& outputState #== pforgetData (pdata expected)

valueUnionTerminalValidator :: forall s. Term s (PAsData PCurrencySymbol :--> PAsData PAddress :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
valueUnionTerminalValidator = plam $ \fraudPolicy fraudAddress threadPolicy ctx ->
  pstep ctx $ \datum redeemer ownRef tx -> pdispatch @_ @PTerminalArgs threadPolicy datum redeemer ownRef tx $ \args -> P.do
    PTerminalArgs i o mintIndex witness <- pmatch args
    PTxInfo{ptxInfo'inputs, ptxInfo'outputs, ptxInfo'redeemers} <- pmatch tx
    pfinalize threadPolicy fraudPolicy fraudAddress (pexpectDatum datum) (pfromData i) (pfromData o) (pfromData mintIndex) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) (pto $ pto $ pfromData ptxInfo'redeemers) $ \_ name _ prior -> P.do
      state <- pmatch $ pexpectStateAs @PFoldState prior
      pfoldState'continuation state #== pforgetData (pdata $ pcon PComplete) #&& pfraudCategoryIsValueNotPreservedV1 # pfromData name #&& pterminalClaimHolds # pfromData (pfoldState'balance state) # witness

valueUnionFieldGrammarValidator :: forall s. Term s (PAsData PScriptHash :--> PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
valueUnionFieldGrammarValidator = plam $ \outputsHash mintHash certificatePolicy threadPolicy ctx ->
  pstep ctx $ \datum redeemer ownRef tx -> pdispatch @_ @PWalkArgs threadPolicy datum redeemer ownRef tx $ \args -> P.do
    PWalkArgs i o opening checkpointBytes <- pmatch args
    PTxInfo{ptxInfo'inputs, ptxInfo'outputs, ptxInfo'referenceInputs} <- pmatch tx
    pcontinue threadPolicy (pexpectDatum datum) (pfromData i) (pfromData o) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) $ \ownHash threadName _ prior outputHash outputState -> P.do
      state <- plet $ pexpectStateAs @PFoldState prior
      s <- pmatch state
      b <- pmatch $ pfromData $ pfoldState'balance s
      grammar <- pmatch $ punsafeCoerce @PFieldGrammar $ pfoldState'continuation s
      let field = pfromData $ pfieldGrammar'fieldIndex grammar
          anchor = pcon $ PBodyAnchor $ pbalanceState'transactionId b
      pIfField <- plet $ field #== 2 #|| field #== 5
      PPair view checkpoint <- pmatch $ pmatch (pfieldGrammar'checkpointHash grammar) $ \case
        PDNothing ->
          pif
            (pfromData checkpointBytes #== pconstant "")
            (popenedFieldGrammarCertification # pfromData opening # anchor # field # pfromData ptxInfo'referenceInputs # certificatePolicy)
            perror
        PDJust hash -> presumeOpenedFieldGrammarCertification # pfromData opening # anchor # field # pfromData hash # pfromData checkpointBytes # pfromData ptxInfo'referenceInputs # certificatePolicy
      advanced <- plet $ Walk.pcertifyFieldGrammar # view # checkpoint # 16
      hash <- plet $ Walk.pfieldGrammarCheckpointHash # advanced
      pIfField
        #&& pif
          (Walk.pfieldGrammarIsComplete # advanced)
          ( P.do
              PPair _ start <- pmatch $ popenedCertifiedFieldWalkFromGrammar # pfromData opening # anchor # field # hash # (Walk.pencodeFieldGrammarCheckpoint # advanced) # pfromData ptxInfo'referenceInputs # certificatePolicy
              let cursor = pcon $ PFieldCursor (pdata field) (pdata $ Walk.pfieldWalkCheckpointHash # start) (punsafeCoerce ownHash)
                  context = pif (field #== 2) (pforgetData $ pdata cursor) (pforgetData $ pdata $ pcon $ PMintCursor (pdata cursor) (pcon PDNothing) (pcon PDNothing))
                  expected = pcon s{pfoldState'continuation = context}
              outputHash #== pif (field #== 2) outputsHash mintHash #&& outputState #== pforgetData (pdata expected)
          )
          ( let context = pcon grammar{pfieldGrammar'checkpointHash = pcon $ PDJust $ pdata hash}
                expected = pcon s{pfoldState'continuation = pforgetData $ pdata context}
             in outputHash #== ownHash #&& outputState #== pforgetData (pdata expected)
          )

valueUnionOutputsValidator :: forall s. Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
valueUnionOutputsValidator = plam $ \scanHash certificatePolicy threadPolicy ctx ->
  pstep ctx $ \datum redeemer ownRef tx -> pdispatch @_ @PWalkArgs threadPolicy datum redeemer ownRef tx $ \args -> P.do
    PWalkArgs i o opening checkpointBytes <- pmatch args
    PTxInfo{ptxInfo'inputs, ptxInfo'outputs, ptxInfo'referenceInputs} <- pmatch tx
    pcontinue threadPolicy (pexpectDatum datum) (pfromData i) (pfromData o) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) $ \ownHash threadName _ prior outputHash outputState -> P.do
      state <- plet $ pexpectStateAs @PFoldState prior
      s <- pmatch state
      b <- pmatch $ pfromData $ pfoldState'balance s
      cursor <- pmatch $ punsafeCoerce @PFieldCursor $ pfoldState'continuation s
      PPair view checkpoint <- pmatch $ presumeOpenedFieldWalk # pfromData opening # pcon (PBodyAnchor $ pbalanceState'transactionId b) # 2 # pfromData (pfieldCursor'checkpointHash cursor) # pfromData checkpointBytes # pfromData ptxInfo'referenceInputs # certificatePolicy
      pfromData (pfieldCursor'fieldIndex cursor)
        #== 2
        #&& pif
          (Walk.pwalkIsComplete # checkpoint)
          ( let context = pcon $ PFieldGrammar (pdata 5) (pcon PDNothing)
                expected = pcon s{pfoldState'continuation = pforgetData $ pdata context}
             in pto (pfromData outputHash) #== pfromData (pfieldCursor'grammarScriptHash cursor) #&& outputState #== pforgetData (pdata expected)
          )
          ( P.do
              PPair extent advanced <- pmatch $ Walk.pwalkNextExtent # view # checkpoint
              PPair offset len <- pmatch extent
              let item =
                    pcon $
                      POutputItem
                        (pdata $ pfieldTotalLength # view)
                        (pdata $ pmap # plam pdata # (pauthenticatedChunkHashes # view))
                        (pdata $ Walk.pwalkNextItemIndex # checkpoint)
                        (pdata offset)
                        (pdata len)
                        (pfieldCursor'checkpointHash cursor)
                        (pdata $ Walk.pfieldWalkCheckpointHash # advanced)
                        (punsafeCoerce ownHash)
                        (pfieldCursor'grammarScriptHash cursor)
                  context = pcon $ POutputScan (pdata item) (pdata Scan.pinitialControlV1)
                  expected = pcon s{pfoldState'continuation = pforgetData $ pdata context}
              outputHash #== scanHash #&& outputState #== pforgetData (pdata expected)
          )

valueUnionOutputScanValidator :: forall s. Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
valueUnionOutputScanValidator = plam $ \assetsHash threadPolicy ctx ->
  pstep ctx $ \datum redeemer ownRef tx -> pdispatch @_ @POutputScanArgs threadPolicy datum redeemer ownRef tx $ \args -> P.do
    POutputScanArgs i o carriage budget <- pmatch args
    PTxInfo{ptxInfo'inputs, ptxInfo'outputs, ptxInfo'referenceInputs} <- pmatch tx
    pcontinue threadPolicy (pexpectDatum datum) (pfromData i) (pfromData o) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) $ \ownHash threadName _ prior outputHash outputState -> P.do
      state <- plet $ pexpectStateAs @PFoldState prior
      s <- pmatch state
      b <- pmatch $ pfromData $ pfoldState'balance s
      scan <- pmatch $ punsafeCoerce @POutputScan $ pfoldState'continuation s
      item <- pmatch $ pfromData $ poutputScan'item scan
      pIfBudget <- plet $ pfromData budget #> 0 #&& pfromData budget #<= 4
      view <- plet $ popenChunks # pfromData carriage # pfromData ptxInfo'referenceInputs # (pmap # plam pfromData # pfromData (poutputItem'fieldChunkHashes item)) # pfromData (poutputItem'fieldTotalLength item)
      control <- plet $ padvanceOutput # view # pfromData (poutputItem'offset item) # pfromData (poutputItem'length item) # pfromData (poutputScan'control scan) # pfromData budget
      pIfBudget
        #&& pif
          (Scan.pterminalIsExactV1 # control # pfromData (poutputItem'length item))
          ( P.do
              c <- pmatch control
              let next = pforgetData $ pdata $ pcon $ PFieldCursor (pdata 2) (poutputItem'nextCheckpointHash item) (poutputItem'grammarScriptHash item)
                  context = pcon $ PAssetCursor (Scan.pscan'assetCount c) (pdata $ Frontier.pfrontierCommitment # pfromData (Scan.pscan'assetCount c) # pfromData (Scan.pscan'assetPeaks c)) (pdata 0) (pdata (-1)) (poutputItem'selectorHash item) next
                  balance = pcon b{pbalanceState'lovelaceDelta = pdata $ pfromData (pbalanceState'lovelaceDelta b) - pfromData (Scan.pscan'lovelace c)}
                  expected = pcon $ PFoldState (pdata balance) (pforgetData $ pdata context)
              outputHash #== assetsHash #&& outputState #== pforgetData (pdata expected)
          )
          ( let context = pcon scan{poutputScan'control = pdata control}
                expected = pcon s{pfoldState'continuation = pforgetData $ pdata context}
             in outputHash #== ownHash #&& outputState #== pforgetData (pdata expected)
          )

valueUnionMintValidator :: forall s. Term s (PAsData PScriptHash :--> PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
valueUnionMintValidator = plam $ \updateHash terminalHash certificatePolicy threadPolicy ctx ->
  pstep ctx $ \datum redeemer ownRef tx -> pdispatch @_ @PWalkArgs threadPolicy datum redeemer ownRef tx $ \args -> P.do
    PWalkArgs i o opening checkpointBytes <- pmatch args
    PTxInfo{ptxInfo'inputs, ptxInfo'outputs, ptxInfo'referenceInputs} <- pmatch tx
    pcontinue threadPolicy (pexpectDatum datum) (pfromData i) (pfromData o) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) $ \ownHash threadName _ prior outputHash outputState -> P.do
      state <- plet $ pexpectStateAs @PFoldState prior
      s <- pmatch state
      b <- pmatch $ pfromData $ pfoldState'balance s
      cursor <- pmatch $ punsafeCoerce @PMintCursor $ pfoldState'continuation s
      field <- pmatch $ pfromData $ pmintCursor'field cursor
      PPair view checkpoint <- pmatch $ presumeOpenedFieldWalk # pfromData opening # pcon (PBodyAnchor $ pbalanceState'transactionId b) # 5 # pfromData (pfieldCursor'checkpointHash field) # pfromData checkpointBytes # pfromData ptxInfo'referenceInputs # certificatePolicy
      pfromData (pfieldCursor'fieldIndex field)
        #== 5
        #&& pmatch
          (pmintCursor'active cursor)
          ( \case
              PDNothing ->
                pif
                  (Walk.pwalkIsComplete # checkpoint)
                  ( let expected = pcon s{pfoldState'continuation = pforgetData $ pdata $ pcon PComplete}
                     in outputHash #== terminalHash #&& outputState #== pforgetData (pdata expected)
                  )
                  ( P.do
                      PPair extent advanced <- pmatch $ Walk.pwalkNextExtent # view # checkpoint
                      PPair offset len <- pmatch extent
                      window <- plet $ pfieldReadRange # view # offset # pif (len #< 132) len 132
                      PPair policy header <- pmatch $ ppolicyHeader # window # pmintCursor'previousPolicy cursor
                      PPair consumed count <- pmatch header
                      let active = pcon $ PMintPolicy (pdata offset) (pdata len) (pdata $ Walk.pfieldWalkCheckpointHash # advanced) (pdata policy) (pdata consumed) (pdata count) (pcon PDNothing)
                          context = pcon cursor{pmintCursor'active = pcon $ PDJust $ pdata active}
                          expected = pcon s{pfoldState'continuation = pforgetData $ pdata context}
                      consumed #<= len #&& outputHash #== ownHash #&& outputState #== pforgetData (pdata expected)
                  )
              PDJust policyD -> pmatch (pfromData policyD) $ \policy ->
                pif
                  (pfromData (pmintPolicy'remaining policy) #== 0)
                  ( let nextField = pcon field{pfieldCursor'checkpointHash = pmintPolicy'nextCheckpointHash policy}
                        context = pcon $ PMintCursor (pdata nextField) (pcon $ PDJust $ pmintPolicy'policyId policy) (pcon PDNothing)
                        expected = pcon s{pfoldState'continuation = pforgetData $ pdata context}
                     in pmintPolicy'cursor policy #== pmintPolicy'length policy #&& outputHash #== ownHash #&& outputState #== pforgetData (pdata expected)
                  )
                  ( P.do
                      let remaining = pfromData (pmintPolicy'length policy) - pfromData (pmintPolicy'cursor policy)
                      pif
                        (pfromData (pmintPolicy'remaining policy) #> 0 #&& remaining #> 0)
                        ( P.do
                            window <- plet $ pfieldReadRange # view # (pfromData (pmintPolicy'offset policy) + pfromData (pmintPolicy'cursor policy)) # pif (remaining #< 132) remaining 132
                            PPair name pair <- pmatch $ passet # window # pmintPolicy'previousAsset policy
                            PPair quantity consumed <- pmatch pair
                            let nextCursor = pfromData (pmintPolicy'cursor policy) + consumed
                                advanced = pcon policy{pmintPolicy'cursor = pdata nextCursor, pmintPolicy'remaining = pdata $ pfromData (pmintPolicy'remaining policy) - 1, pmintPolicy'previousAsset = pcon $ PDJust $ pdata name}
                                next = pforgetData $ pdata $ pcon cursor{pmintCursor'active = pcon $ PDJust $ pdata advanced}
                                pending = pcon $ PPendingContribution (pdata $ pfromData (pmintPolicy'policyId policy) <> name) (pdata quantity) (punsafeCoerce ownHash) next
                                expected = pcon s{pfoldState'continuation = pforgetData $ pdata pending}
                            nextCursor #<= pfromData (pmintPolicy'length policy) #&& outputHash #== updateHash #&& outputState #== pforgetData (pdata expected)
                        )
                        perror
                  )
          )

data PWalkArgs s = PWalkArgs (Term s (PAsData PInteger)) (Term s (PAsData PInteger)) (Term s (PAsData PFieldOpeningV1)) (Term s (PAsData PByteString))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PWalkArgs)

data POutputScanArgs s = POutputScanArgs (Term s (PAsData PInteger)) (Term s (PAsData PInteger)) (Term s (PAsData PChunkCarriage)) (Term s (PAsData PInteger))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct POutputScanArgs)
