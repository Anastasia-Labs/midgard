{-# LANGUAGE OverloadedStrings #-}

module Midgard.FraudProofs.NativeScriptDecoding.Engine (
  PBindStateV1 (..),
  PScanThreadStateV1 (..),
  PScanWindowV1 (..),
  PMachineBindResultV1 (..),
  PScanOutcomeV1 (..),
  PScanAccusationV1 (..),
  pdirectionWrongfulAcceptance,
  pdirectionWrongfulRejection,
  psourceKindNormal,
  psourceKindForced,
  poutpointSourceSpend,
  poutpointSourceReference,
  prefusalClassMalformed,
  prefusalClassNodeLimit,
  prefusalClassDepthLimit,
  pclassPending,
  planguageUnbound,
  pmaxTokenByteWidth,
  pmachineControlDomain,
  pencodeScanThreadStateV1,
  ppreBindScanStateV1,
  popenedSubjectScanStateV1,
  pboundDescriptorScanStateV1,
  pscanStateWithMachineHashV1,
  pscanStateWithRefusalClassV1,
  phashMachineControlV1,
  pauthenticatedScanWindowV1,
  pbindMachineV1,
  pbudgetedScanV1,
  pverifyCommittedPreStateV1,
  pverifyForcedLeafV1,
  pscanAccusationOfV1,
) where

import Data.Kind (Type)
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Builtin.Crypto (pblake2b_256)
import Plutarch.Core.Utils (pand'List, (#/=))
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.BoundedItem qualified as Bounded
import Midgard.FraudProofs.NativeTx.Compact (pverifyNativeTxProofSourceV1)
import Midgard.FraudProofs.NativeTx.Codec (pcborInt, pencodeDefiniteArrayHeader, pencodeDefiniteBytes)
import Midgard.FraudProofs.NativeTx.Types (PNativeTxCompact (..), PVerifiedMidgardNativeTxCompact (..))
import Midgard.LedgerOutputCommitment (PLedgerOutputCommitmentV1 (..), poutputFieldIndex)
import Midgard.LedgerState (
  PEventKey (..),
  PEventToStepValue (..),
  PForcedInclusionTxV1 (..),
  PHeaderV1 (..),
  PNativeTxProofSourceV1 (..),
  PTransitionPhase (..),
  PTransitionStep (..),
  pprotocolVersionV1,
  ptransitionStepSchemaVersionV1,
 )
import Midgard.NativeScriptScan qualified as Scan
import Midgard.RejectionReason (POperatorVerdictV1 (..), PRejectionReasonV1 (..))
import Midgard.TransitionTrace qualified as Trace

pdirectionWrongfulAcceptance, pdirectionWrongfulRejection :: forall s. Term s PInteger
pdirectionWrongfulAcceptance = 0
pdirectionWrongfulRejection = 1

psourceKindNormal, psourceKindForced :: forall s. Term s PInteger
psourceKindNormal = 0
psourceKindForced = 1

poutpointSourceSpend, poutpointSourceReference :: forall s. Term s PInteger
poutpointSourceSpend = 0
poutpointSourceReference = 1

prefusalClassMalformed, prefusalClassNodeLimit, prefusalClassDepthLimit :: forall s. Term s PInteger
prefusalClassMalformed = 0
prefusalClassNodeLimit = 1
prefusalClassDepthLimit = 2

pclassPending, planguageUnbound, pmaxTokenByteWidth :: forall s. Term s PInteger
pclassPending = -1
planguageUnbound = -2
pmaxTokenByteWidth = 33

pmachineControlDomain :: forall s. Term s PByteString
pmachineControlDomain = pconstant "midgard/fraud-proofs/native-script-decoding/control-v1"

data PBindStateV1 s = PBindStateV1
  { pbindState'direction :: Term s (PAsData PInteger)
  , pbindState'sourceKind :: Term s (PAsData PInteger)
  , pbindState'verifiedTxId :: Term s (PAsData PByteString)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PBindStateV1)

data PScanThreadStateV1 s = PScanThreadStateV1
  { pscanState'direction :: Term s (PAsData PInteger)
  , pscanState'sourceKind :: Term s (PAsData PInteger)
  , pscanState'verifiedTxId :: Term s (PAsData PByteString)
  , pscanState'txOrderId :: Term s (PAsData PByteString)
  , pscanState'scanReasonClass :: Term s (PAsData PInteger)
  , pscanState'priorLedgerRoot :: Term s (PAsData PByteString)
  , pscanState'outpointSourceKind :: Term s (PAsData PInteger)
  , pscanState'outpointCursor :: Term s (PAsData PInteger)
  , pscanState'outpointKeyHash :: Term s (PAsData PByteString)
  , pscanState'referenceScriptLanguage :: Term s (PAsData PInteger)
  , pscanState'outputIndex :: Term s (PAsData PInteger)
  , pscanState'totalLength :: Term s (PAsData PInteger)
  , pscanState'itemCommitment :: Term s (PAsData PByteString)
  , pscanState'machineStateHash :: Term s (PAsData PByteString)
  , pscanState'refusalClass :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PScanThreadStateV1)

data PScanWindowV1 s = PScanWindowV1
  { pscanWindow'bytes :: Term s (PAsData PByteString)
  , pscanWindow'startOffset :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PScanWindowV1)

data PMachineBindResultV1 s
  = PMachineBindMalformedV1
  | PMachineBindNonNativeV1 (Term s (PAsData PInteger))
  | PMachineBoundV1 (Term s (PAsData Scan.PNativeScriptStructureControlV1))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PMachineBindResultV1)

data PScanOutcomeV1 s
  = PScanAdvancedV1 (Term s (PAsData Scan.PNativeScriptStructureControlV1))
  | PScanRefusedV1 (Term s (PAsData PInteger))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PScanOutcomeV1)

data PScanAccusationV1 s = PScanAccusationV1
  { pscanAccusation'scanReasonClass :: Term s (PAsData PInteger)
  , pscanAccusation'outpointSourceKind :: Term s (PAsData PInteger)
  , pscanAccusation'outpointCursor :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PScanAccusationV1)

pencodeScanThreadStateV1 :: forall s. Term s (PScanThreadStateV1 :--> PByteString)
pencodeScanThreadStateV1 = phoistAcyclic $ plam $ \state -> pmatch state $ \s ->
  (pencodeDefiniteArrayHeader # 15)
    <> pcborInt (pfromData $ pscanState'direction s)
    <> pcborInt (pfromData $ pscanState'sourceKind s)
    <> (pencodeDefiniteBytes # pfromData (pscanState'verifiedTxId s))
    <> (pencodeDefiniteBytes # pfromData (pscanState'txOrderId s))
    <> pcborInt (pfromData $ pscanState'scanReasonClass s)
    <> (pencodeDefiniteBytes # pfromData (pscanState'priorLedgerRoot s))
    <> pcborInt (pfromData $ pscanState'outpointSourceKind s)
    <> pcborInt (pfromData $ pscanState'outpointCursor s)
    <> (pencodeDefiniteBytes # pfromData (pscanState'outpointKeyHash s))
    <> pcborInt (pfromData $ pscanState'referenceScriptLanguage s)
    <> pcborInt (pfromData $ pscanState'outputIndex s)
    <> pcborInt (pfromData $ pscanState'totalLength s)
    <> (pencodeDefiniteBytes # pfromData (pscanState'itemCommitment s))
    <> (pencodeDefiniteBytes # pfromData (pscanState'machineStateHash s))
    <> pcborInt (pfromData $ pscanState'refusalClass s)

ppreBindScanStateV1 :: forall s. Term s (PInteger :--> PInteger :--> PByteString :--> PByteString :--> PInteger :--> PByteString :--> PInteger :--> PInteger :--> PScanThreadStateV1)
ppreBindScanStateV1 = phoistAcyclic $ plam $ \direction sourceKind verifiedTxId txOrderId scanReasonClass priorLedgerRoot outpointSourceKind outpointCursor ->
  pcon $ PScanThreadStateV1
    (pdata direction) (pdata sourceKind) (pdata verifiedTxId) (pdata txOrderId)
    (pdata scanReasonClass) (pdata priorLedgerRoot) (pdata outpointSourceKind) (pdata outpointCursor)
    (pdata $ pconstant "") (pdata planguageUnbound) (pdata $ -1) (pdata $ -1)
    (pdata $ pconstant "") (pdata $ pconstant "") (pdata pclassPending)

popenedSubjectScanStateV1 :: forall s. Term s (PScanThreadStateV1 :--> PByteString :--> PInteger :--> PScanThreadStateV1)
popenedSubjectScanStateV1 = phoistAcyclic $ plam $ \state outpointKeyBytes outputIndex -> pmatch state $ \s ->
  pcon $ PScanThreadStateV1
    (pscanState'direction s) (pscanState'sourceKind s) (pscanState'verifiedTxId s) (pscanState'txOrderId s)
    (pscanState'scanReasonClass s) (pscanState'priorLedgerRoot s) (pscanState'outpointSourceKind s) (pscanState'outpointCursor s)
    (pdata $ pblake2b_256 # outpointKeyBytes) (pscanState'referenceScriptLanguage s) (pdata outputIndex)
    (pscanState'totalLength s) (pscanState'itemCommitment s) (pscanState'machineStateHash s) (pscanState'refusalClass s)

pboundDescriptorScanStateV1 :: forall s. Term s (PScanThreadStateV1 :--> PLedgerOutputCommitmentV1 :--> PScanThreadStateV1)
pboundDescriptorScanStateV1 = phoistAcyclic $ plam $ \state descriptor -> pmatch state $ \s -> pmatch descriptor $ \d ->
  pcon $ PScanThreadStateV1
    (pscanState'direction s) (pscanState'sourceKind s) (pscanState'verifiedTxId s) (pscanState'txOrderId s)
    (pscanState'scanReasonClass s) (pscanState'priorLedgerRoot s) (pscanState'outpointSourceKind s) (pscanState'outpointCursor s)
    (pscanState'outpointKeyHash s) (poutputCommitment'referenceScriptLanguage d) (pscanState'outputIndex s)
    (poutputCommitment'referenceScriptTotalLength d) (poutputCommitment'referenceScriptItemCommitment d)
    (pscanState'machineStateHash s) (pscanState'refusalClass s)

pscanStateWithMachineHashV1 :: forall s. Term s (PScanThreadStateV1 :--> PByteString :--> PScanThreadStateV1)
pscanStateWithMachineHashV1 = phoistAcyclic $ plam $ \state machineHash -> pmatch state $ \s ->
  pcon $ PScanThreadStateV1
    (pscanState'direction s) (pscanState'sourceKind s) (pscanState'verifiedTxId s) (pscanState'txOrderId s)
    (pscanState'scanReasonClass s) (pscanState'priorLedgerRoot s) (pscanState'outpointSourceKind s) (pscanState'outpointCursor s)
    (pscanState'outpointKeyHash s) (pscanState'referenceScriptLanguage s) (pscanState'outputIndex s)
    (pscanState'totalLength s) (pscanState'itemCommitment s) (pdata machineHash) (pscanState'refusalClass s)

pscanStateWithRefusalClassV1 :: forall s. Term s (PScanThreadStateV1 :--> PInteger :--> PScanThreadStateV1)
pscanStateWithRefusalClassV1 = phoistAcyclic $ plam $ \state refusalClass -> pmatch state $ \s ->
  pcon $ PScanThreadStateV1
    (pscanState'direction s) (pscanState'sourceKind s) (pscanState'verifiedTxId s) (pscanState'txOrderId s)
    (pscanState'scanReasonClass s) (pscanState'priorLedgerRoot s) (pscanState'outpointSourceKind s) (pscanState'outpointCursor s)
    (pscanState'outpointKeyHash s) (pscanState'referenceScriptLanguage s) (pscanState'outputIndex s)
    (pscanState'totalLength s) (pscanState'itemCommitment s) (pscanState'machineStateHash s) (pdata refusalClass)

phashMachineControlV1 :: forall s. Term s (PByteString :--> PByteString)
phashMachineControlV1 = phoistAcyclic $ plam $ \controlCbor -> pblake2b_256 #$ pmachineControlDomain <> controlCbor

pauthenticatedReferenceScriptChunk :: forall s. Term s (PInteger :--> PInteger :--> PByteString :--> PInteger :--> Bounded.PChunkProofV1 :--> PBool)
pauthenticatedReferenceScriptChunk = phoistAcyclic $ plam $ \outputIndex totalLength itemCommitment chunkIndex proof -> pmatch proof $ \p ->
  pand'List
    [ pfromData (Bounded.pchunkProof'fieldIndex p) #== poutputFieldIndex
    , pfromData (Bounded.pchunkProof'itemIndex p) #== outputIndex
    , pfromData (Bounded.pchunkProof'totalLength p) #== totalLength
    , pfromData (Bounded.pchunkProof'chunkIndex p) #== chunkIndex
    , Bounded.pverifyChunk # itemCommitment # proof
    ]

pauthenticatedScanWindowV1 :: forall s. Term s (PInteger :--> PInteger :--> PByteString :--> PInteger :--> Bounded.PChunkProofV1 :--> PMaybe Bounded.PChunkProofV1 :--> PScanWindowV1)
pauthenticatedScanWindowV1 = phoistAcyclic $ plam $ \outputIndex totalLength itemCommitment cursor chunkProof nextChunkProof ->
  pif (cursor #>= 0 #&& cursor #< totalLength)
    (plet (pdiv # cursor # Bounded.pchunkBytes) $ \chunkIndex ->
      pif (pauthenticatedReferenceScriptChunk # outputIndex # totalLength # itemCommitment # chunkIndex # chunkProof)
        (pmatch chunkProof $ \chunk ->
          pif (chunkIndex + 1 #< Bounded.pchunkCount # totalLength)
            (pmatch nextChunkProof $ \case
              PNothing -> perror
              PJust nextProof -> pif
                (pauthenticatedReferenceScriptChunk # outputIndex # totalLength # itemCommitment # (chunkIndex + 1) # nextProof)
                (pmatch nextProof $ \next -> pcon $ PScanWindowV1
                  (pdata $ pfromData (Bounded.pchunkProof'chunk chunk) <> pfromData (Bounded.pchunkProof'chunk next))
                  (pdata $ chunkIndex * Bounded.pchunkBytes))
                perror)
            (pmatch nextChunkProof $ \case
              PNothing -> pcon $ PScanWindowV1
                (Bounded.pchunkProof'chunk chunk) (pdata $ chunkIndex * Bounded.pchunkBytes)
              PJust _ -> perror))
        perror)
    perror

pbindMachineV1 :: forall s. Term s (PByteString :--> PInteger :--> PMachineBindResultV1)
pbindMachineV1 = phoistAcyclic $ plam $ \firstChunk totalLength ->
  pmatch (Scan.pversionedScriptHeaderV1 # firstChunk # totalLength) $ \case
    PNothing -> pcon PMachineBindMalformedV1
    PJust header -> pmatch header $ \h ->
      plet (pfromData $ Scan.pheader'languageTag h) $ \languageTag ->
      plet (pfromData $ Scan.pheader'payloadLength h) $ \payloadLength ->
      pif (languageTag #/= 0)
        (pcon $ PMachineBindNonNativeV1 $ pdata languageTag)
        (pif (payloadLength #== 0)
          (pcon PMachineBindMalformedV1)
          (pcon $ PMachineBoundV1 $ pdata $
            Scan.pinitialStructureControlV1 # pfromData (Scan.pheader'payloadOffset h) # payloadLength))

psafeTokenRead :: forall s. Term s (Scan.PNativeScriptStructureControlV1 :--> PInteger :--> PInteger :--> PBool)
psafeTokenRead = phoistAcyclic $ plam $ \control windowStart windowEnd -> pmatch control $ \c ->
  plet (pfromData $ Scan.pstructure'cursor c) $ \cursor ->
  pand'List
    [ cursor #>= windowStart
    , cursor #< windowEnd
    , windowEnd #>= pfromData (Scan.pstructure'endOffset c) #|| windowEnd - cursor #>= pmaxTokenByteWidth
    ]

pbudgetedScanV1 :: forall s. Term s (Scan.PNativeScriptStructureControlV1 :--> PMaybe PScanWindowV1 :--> PBuiltinList (PAsData Scan.PNativeScriptFrameV1) :--> PInteger :--> PScanOutcomeV1)
pbudgetedScanV1 = phoistAcyclic $ pfix $ \self -> plam $ \control window frames maxSteps ->
  let advanced = pcon $ PScanAdvancedV1 $ pdata control
      continue result remainingFrames = pmatch result $ \case
        Scan.PNativeScriptStructureAdvanced nextControl ->
          self # pfromData nextControl # window # remainingFrames # (maxSteps - 1)
        Scan.PNativeScriptStructureInvalid -> pcon $ PScanRefusedV1 $ pdata prefusalClassMalformed
        Scan.PNativeScriptStructureNodeLimit -> pcon $ PScanRefusedV1 $ pdata prefusalClassNodeLimit
        Scan.PNativeScriptStructureDepthLimit -> pcon $ PScanRefusedV1 $ pdata prefusalClassDepthLimit
   in pif (maxSteps #<= 0) advanced $
      pmatch control $ \c ->
      plet (pfromData $ Scan.pstructure'stage c) $ \stage ->
      pif (stage #== Scan.pstructureStageTerminal) advanced $
        pif (stage #== Scan.pstructureStageToken)
          (pmatch window $ \case
            PNothing -> advanced
            PJust scanWindow -> pmatch scanWindow $ \w ->
              plet (pfromData (pscanWindow'startOffset w) + plengthBS # pfromData (pscanWindow'bytes w)) $ \windowEnd ->
              pif (psafeTokenRead # control # pfromData (pscanWindow'startOffset w) # windowEnd)
                (pmatch (Scan.pstructureTokenStepV1 # control # pfromData (pscanWindow'bytes w)
                  # (pfromData (Scan.pstructure'cursor c) - pfromData (pscanWindow'startOffset w))) $ \case
                    PNothing -> perror
                    PJust result -> continue result frames)
                advanced)
          (pif (stage #== Scan.pstructureStageFrame)
            (pmatch frames $ \case
              PNil -> advanced
              PCons frame remainingFrames ->
                pmatch (Scan.pstructureFrameStepV1 # control # pfromData frame) $ \case
                  PNothing -> perror
                  PJust result -> continue result remainingFrames)
            (pmatch (Scan.pfinalizeStructureV1 # control) $ \case
              PNothing -> perror
              PJust result -> continue result frames))

pcoerceData :: forall (a :: S -> Type) s. PIsData a => Term s PData -> Term s a
pcoerceData value = pfromData (punsafeCoerce @(PAsData a) value)

pphaseForEventKey :: forall s. Term s PEventKey -> Term s PTransitionPhase
pphaseForEventKey eventKey = pmatch eventKey $ \case
  PWithdrawalEventKey _ -> pcon PWithdrawal
  PForcedTransactionEventKey _ -> pcon PForcedTransaction
  PL2TransactionEventKey _ -> pcon PL2Transaction
  PDepositEventKey _ -> pcon PDeposit

pverifyCommittedPreStateV1 :: forall s. Term s (PHeaderV1 :--> PEventKey :--> Trace.PRootMembershipProof :--> Trace.PRootMembershipProof :--> PByteString)
pverifyCommittedPreStateV1 = phoistAcyclic $ plam $ \header eventKey eventMembership stepMembership -> P.do
  h <- pmatch header
  eventProof <- pmatch eventMembership
  stepProof <- pmatch stepMembership
  eventToStep <- plet $ pcoerceData (Trace.prootMembership'value eventProof)
  transitionStep <- plet $ pcoerceData (Trace.prootMembership'value stepProof)
  e <- pmatch eventToStep
  step <- pmatch transitionStep
  stepIndex <- plet $ pasInt # Trace.prootMembership'key stepProof
  pif
    ( pand'List
        [ pfromData (pheader'protocolVersion h) #== pprotocolVersionV1
        , Trace.pverifyRootMembershipWithBytes stepMembership
            (pdata $ pcon Trace.PTransitionTraceRootDomain)
            (pfromData $ pheader'transitionTraceRoot h)
            (pfromData $ pheader'transitionStepCount h)
            (pserialiseData # Trace.prootMembership'key stepProof)
            (pserialiseData # Trace.prootMembership'value stepProof)
        , stepIndex #== pfromData (ptransitionStep'stepIndex step)
        , stepIndex #>= 0
        , stepIndex #< pfromData (Trace.prootMembership'count stepProof)
        , pfromData (ptransitionStep'schemaVersion step) #== ptransitionStepSchemaVersionV1
        , Trace.pverifyRootMembershipWithBytes eventMembership
            (pdata $ pcon Trace.PEventToStepRootDomain)
            (pfromData $ pheader'eventToStepRoot h)
            (pfromData $ pheader'totalEventCount h)
            (pserialiseData # Trace.prootMembership'key eventProof)
            (pserialiseData # Trace.prootMembership'value eventProof)
        , stepIndex #== pfromData (peventToStepValue'stepIndex e)
        , Trace.prootMembership'key eventProof #== pforgetData (pdata eventKey)
        , ptransitionStep'eventKey step #== pdata eventKey
        , ptransitionStep'phase step #== pdata (pphaseForEventKey eventKey)
        , ptransitionStep'phase step #== peventToStepValue'phase e
        ]
    )
    (pfromData $ ptransitionStep'preUtxosRoot step)
    perror

pverifyForcedLeafV1 :: forall s. Term s (PHeaderV1 :--> Trace.PRootMembershipProof :--> PForcedInclusionTxV1)
pverifyForcedLeafV1 = phoistAcyclic $ plam $ \header membership -> P.do
  h <- pmatch header
  proof <- pmatch membership
  leaf <- plet $ pcoerceData (Trace.prootMembership'value proof)
  f <- pmatch leaf
  source <- pmatch (pfromData $ pforcedTx'source f)
  verifiedPair <- plet $
    pverifyNativeTxProofSourceV1
      # pfromData (pforcedTx'txId f)
      # pfromData (pnativeSource'compactCbor source)
      # pfromData (pnativeSource'witnessSetCompactCbor source)
      # pfromData (pnativeSource'fieldPreimageLengthsCbor source)
  pmatch verifiedPair $ \(PPair verified _) -> pmatch verified $ \v -> pmatch (pverified'txCompact v) $ \compact ->
    pif
      ( Trace.pverifyRootMembershipWithBytes membership
          (pdata $ pcon Trace.PForcedTransactionsV1RootDomain)
          (pfromData $ pheader'forcedTransactionsRoot h)
          (pfromData $ pheader'forcedTransactionCount h)
          (pserialiseData # Trace.prootMembership'key proof)
          (pserialiseData # Trace.prootMembership'value proof)
          #&& pverified'version v #== 1
          #&& pmatch (pfromData $ pforcedTx'verdict f) (\case
            PForcedTxValid -> pcompact'validityCode compact #== 0
            PForcedTxInvalid _ -> pcompact'validityCode compact #== 1)
      )
      leaf
      perror

pscanAccusationOfV1 :: forall s. Term s (PRejectionReasonV1 :--> PScanAccusationV1)
pscanAccusationOfV1 = phoistAcyclic $ plam $ \reason -> pmatch reason $ \case
  PResolvedReferenceScriptMalformed sourceKind inputIndex ->
    pcon $ PScanAccusationV1 (pdata prefusalClassMalformed) sourceKind inputIndex
  PResolvedReferenceScriptNodeLimit sourceKind inputIndex ->
    pcon $ PScanAccusationV1 (pdata prefusalClassNodeLimit) sourceKind inputIndex
  PResolvedReferenceScriptDepthLimit sourceKind inputIndex ->
    pcon $ PScanAccusationV1 (pdata prefusalClassDepthLimit) sourceKind inputIndex
  _ -> perror
