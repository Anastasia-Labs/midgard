module Midgard.FraudProofs.InputSetUniqueness (
  PStep01Source (..),
  PStep01Args (..),
  PStep03State (..),
  PStep03Args (..),
  PStep04Args (..),
  PBoundDuplicateInput (..),
  PUniqueScanState (..),
  pbindForcedDuplicateReason,
  pcheckpoint,
  pinitialScanState,
  pscanStateIsAuthentic,
  padvanceUniqueScan,
  padvanceUniqueBatch,
  puniqueScanIsComplete,
  PStep02State (..),
  PStep02Args (..),
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Prelude

import Midgard.FraudProofs.Common (PNativeTxInclusionCarriage)
import Midgard.FraudProofs.FieldOpening (PFieldOpeningV1)
import Midgard.FraudProofs.NativeTx.Codec (pencodeDefiniteBytes)
import Midgard.FraudProofs.ProofThreadSubstrate qualified as Subject
import Midgard.LedgerState (PHeaderV1)
import Midgard.NativeTxFieldAccess (PFieldCarriageV1)
import Midgard.RejectionReason (PRejectionReasonV1 (PDuplicateInput))
import Midgard.TransitionTrace (PRootMembershipProof)
import Plutarch.Builtin.Crypto (pblake2b_256)
import Plutarch.Monadic qualified as P

data PStep02State s = PStep02State
  { pstep02State'badTxId :: Term s (PAsData PByteString)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep02State)

data PStep02Args s
  = PDuplicateSpendInputs
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PFieldOpeningV1))
  | PDuplicateReferenceInputs
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PFieldOpeningV1))
  | PSpendReferenceOverlap
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PByteString))
      (Term s (PAsData PFieldCarriageV1))
      (Term s (PAsData PFieldCarriageV1))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep02Args)

data PStep01Source s
  = PAcceptedSource (Term s (PAsData PNativeTxInclusionCarriage))
  | PForcedSource
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PHeaderV1))
      (Term s (PAsData PRootMembershipProof))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep01Source)

newtype PStep01Args s = PStep01Args (Term s (PAsData PStep01Source))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep01Args)

newtype PStep03State s = PStep03State (Term s (PAsData PBoundDuplicateInput))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep03State)

data PStep03Args s
  = PStep03Args
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PByteString))
      (Term s (PAsData PFieldCarriageV1))
      (Term s (PAsData PFieldCarriageV1))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep03Args)

data PStep04Args s
  = PAdvance
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PFieldOpeningV1))
  | PFinalize
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep04Args)

data PBoundDuplicateInput s = PBoundDuplicateInput
  { pbound'subject :: Term s (PAsData Subject.PVerdictSubject)
  , pbound'firstField :: Term s (PAsData PInteger)
  , pbound'firstItem :: Term s (PAsData PInteger)
  , pbound'secondField :: Term s (PAsData PInteger)
  , pbound'secondItem :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PBoundDuplicateInput)

data PUniqueScanState s = PUniqueScanState
  { pscan'bound :: Term s (PAsData PBoundDuplicateInput)
  , pscan'spendCount :: Term s (PAsData PInteger)
  , pscan'referenceCount :: Term s (PAsData PInteger)
  , pscan'cursor :: Term s (PAsData PInteger)
  , pscan'previousItem :: Term s (PAsData PByteString)
  , pscan'nextHash :: Term s (PAsData PByteString)
  , pscan'checkpoint :: Term s (PAsData PByteString)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PUniqueScanState)

pbindForcedDuplicateReason :: forall s. Term s (Subject.PVerdictSubject :--> PBoundDuplicateInput)
pbindForcedDuplicateReason = phoistAcyclic $ plam $ \subject -> P.do
  Subject.PVerdictSubject {Subject.psubject'direction, Subject.psubject'sourceKind} <- pmatch subject
  pif
    (pfromData psubject'direction #== 1 #&& pfromData psubject'sourceKind #== 1)
    ( pmatch (Subject.prejectionReasonOf # subject) $ \case
        PDuplicateInput firstField firstItem secondField secondItem ->
          pcon $ PBoundDuplicateInput (pdata subject) firstField firstItem secondField secondItem
        _ -> perror
    )
    perror

-- Aiken serialises each integer separately and uses the substrate's definite
-- subject encoding. Serialising the state Data would produce a different hash.
pcheckpoint :: forall s. Term s (PBoundDuplicateInput :--> PInteger :--> PInteger :--> PInteger :--> PByteString :--> PByteString :--> PByteString)
pcheckpoint = phoistAcyclic $ plam $ \bound spendCount referenceCount cursor previousItem nextHash -> P.do
  PBoundDuplicateInput {..} <- pmatch bound
  pblake2b_256
    # ( pconstant "midgard/fraud-proofs/input-set-uniqueness/checkpoint-v1"
          <> (Subject.pencodeVerdictSubject # pfromData pbound'subject)
          <> (pserialiseData # pforgetData pbound'firstField)
          <> (pserialiseData # pforgetData pbound'firstItem)
          <> (pserialiseData # pforgetData pbound'secondField)
          <> (pserialiseData # pforgetData pbound'secondItem)
          <> (pserialiseData # pforgetData (pdata spendCount))
          <> (pserialiseData # pforgetData (pdata referenceCount))
          <> (pserialiseData # pforgetData (pdata cursor))
          <> (pencodeDefiniteBytes # previousItem)
          <> (pencodeDefiniteBytes # nextHash)
      )

pstateWith :: forall s. Term s (PBoundDuplicateInput :--> PInteger :--> PInteger :--> PInteger :--> PByteString :--> PByteString :--> PUniqueScanState)
pstateWith = phoistAcyclic $ plam $ \bound spendCount referenceCount cursor previousItem nextHash ->
  pcon $
    PUniqueScanState
      (pdata bound)
      (pdata spendCount)
      (pdata referenceCount)
      (pdata cursor)
      (pdata previousItem)
      (pdata nextHash)
      (pdata $ pcheckpoint # bound # spendCount # referenceCount # cursor # previousItem # nextHash)

pcoordinateInRange :: forall s. Term s (PInteger :--> PInteger :--> PInteger :--> PInteger :--> PBool)
pcoordinateInRange = phoistAcyclic $ plam $ \field item spendCount referenceCount ->
  item #>= 0 #&& pif (field #== 0) (item #< spendCount) (field #== 1 #&& item #< referenceCount)

pinitialScanState :: forall s. Term s (PBoundDuplicateInput :--> PInteger :--> PInteger :--> PByteString :--> PUniqueScanState)
pinitialScanState = phoistAcyclic $ plam $ \bound spendCount referenceCount scanHash -> P.do
  PBoundDuplicateInput {..} <- pmatch bound
  let firstField = pfromData pbound'firstField
      firstItem = pfromData pbound'firstItem
      secondField = pfromData pbound'secondField
      secondItem = pfromData pbound'secondItem
  pif
    ( spendCount
        #>= 0
        #&& referenceCount
        #>= 0
        #&& pcoordinateInRange
        # firstField
        # firstItem
        # spendCount
        # referenceCount
        #&& pcoordinateInRange
        # secondField
        # secondItem
        # spendCount
        # referenceCount
        #&& (firstField #< secondField #|| (firstField #== secondField #&& firstItem #< secondItem))
    )
    (pstateWith # bound # spendCount # referenceCount # 0 # pconstant "" # scanHash)
    perror

pscanStateIsAuthentic :: forall s. Term s (PUniqueScanState :--> PBool)
pscanStateIsAuthentic = phoistAcyclic $ plam $ \state -> P.do
  PUniqueScanState {..} <- pmatch state
  let spendCount = pfromData pscan'spendCount
      referenceCount = pfromData pscan'referenceCount
      cursor = pfromData pscan'cursor
      previousItem = pfromData pscan'previousItem
      nextHash = pfromData pscan'nextHash
  spendCount
    #>= 0
    #&& referenceCount
    #>= 0
    #&& cursor
    #>= 0
    #&& cursor
    #<= spendCount
    + referenceCount
      #&& plengthBS
      # nextHash
      #== 28
      #&& pif (cursor #== 0) (previousItem #== pconstant "") (plengthBS # previousItem #== 38)
      #&& pfromData pscan'checkpoint
      #== pcheckpoint
      # pfromData pscan'bound
      # spendCount
      # referenceCount
      # cursor
      # previousItem
      # nextHash

-- Input authenticity is checked once by the spending validator before opening
-- the field, exactly as in the target; these transitions do not rehash it.
padvanceUniqueScan :: forall s. Term s (PUniqueScanState :--> PByteString :--> PByteString :--> PUniqueScanState)
padvanceUniqueScan = phoistAcyclic $ plam $ \state item nextHash -> P.do
  PUniqueScanState {..} <- pmatch state
  let cursor = pfromData pscan'cursor
  pif
    ( cursor #< pfromData pscan'spendCount
        + pfromData pscan'referenceCount
          #&& plengthBS
          # item
          #== 38
          #&& (cursor #== 0 #|| pfromData pscan'previousItem #< item)
    )
    (pstateWith # pfromData pscan'bound # pfromData pscan'spendCount # pfromData pscan'referenceCount # (cursor + 1) # item # nextHash)
    perror

padvanceUniqueBatch :: forall s. Term s (PUniqueScanState :--> PBuiltinList PByteString :--> PByteString :--> PUniqueScanState)
padvanceUniqueBatch = phoistAcyclic $ plam $ \state items nextHash -> P.do
  PUniqueScanState {..} <- pmatch state
  count <- plet $ plength # items
  let cursor = pfromData pscan'cursor
  pif
    (count #> 0 #&& cursor + count #<= pfromData pscan'spendCount + pfromData pscan'referenceCount)
    ( P.do
        PPair previous consumed <-
          pmatch $
            pfoldl
              # plam
                ( \acc item -> pmatch acc $ \(PPair previous offset) ->
                    pif
                      (plengthBS # item #== 38 #&& (cursor + offset #== 0 #|| previous #< item))
                      (pcon $ PPair item (offset + 1))
                      perror
                )
              # pcon (PPair (pfromData pscan'previousItem) 0)
              # items
        pstateWith # pfromData pscan'bound # pfromData pscan'spendCount # pfromData pscan'referenceCount # (cursor + consumed) # previous # nextHash
    )
    perror

puniqueScanIsComplete :: forall s. Term s (PUniqueScanState :--> PBool)
puniqueScanIsComplete = phoistAcyclic $ plam $ \state -> P.do
  PUniqueScanState {..} <- pmatch state
  PBoundDuplicateInput {pbound'subject} <- pmatch $ pfromData pscan'bound
  pscanStateIsAuthentic
    # state
    #&& pfromData pscan'cursor
    #== pfromData pscan'spendCount
    + pfromData pscan'referenceCount
      #&& Subject.pterminalContradiction
      # pfromData pbound'subject
      # pconstant False
