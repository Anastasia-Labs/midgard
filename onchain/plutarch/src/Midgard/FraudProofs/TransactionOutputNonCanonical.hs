module Midgard.FraudProofs.TransactionOutputNonCanonical (
  PSource (..),
  PStep01Args (..),
  PStep02Args (..),
  PStep03Args (..),
  PStep04Args (..),
  PBoundOutput (..),
  POutputScanState (..),
  poutputsFieldIndex,
  pmaxOutputBytes,
  poutcomeScanning,
  poutcomeCanonical,
  poutcomeNonCanonical,
  pscanChunkBytes,
  pscanWindowBytes,
  pbindOutput,
  pinitialScan,
  pstateIsWellFormed,
  padvanceScan,
  pterminalContradiction,
  pencodeBoundOutput,
  pencodeScanState,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Builtin.Crypto (pblake2b_256)
import Plutarch.Core.Utils ((#/=))
import Plutarch.Monadic qualified as P
import Plutarch.Prelude

import Midgard.FraudProofs.Common (PNativeTxInclusionCarriage)
import Midgard.FraudProofs.FieldOpening (PFieldOpeningV1)
import Midgard.FraudProofs.ProofThreadSubstrate (PVerdictSubject (..))
import Midgard.FraudProofs.ProofThreadSubstrate qualified as Subject
import Midgard.LedgerOutputScan qualified as Scan
import Midgard.LedgerState (PHeaderV1)
import Midgard.RejectionReason (PRejectionReasonV1 (POutputNonCanonical))
import Midgard.TransitionTrace (PRootMembershipProof)
import Midgard.Validators.FraudProofs.Step (pexpecting)

poutputsFieldIndex
  , pmaxOutputBytes
  , poutcomeScanning
  , poutcomeCanonical
  , poutcomeNonCanonical
  , pscanChunkBytes
  , pscanWindowBytes ::
    forall s. Term s PInteger
poutputsFieldIndex = 2
pmaxOutputBytes = 16384
poutcomeScanning = 0
poutcomeCanonical = 1
poutcomeNonCanonical = 2
pscanChunkBytes = 4095
pscanWindowBytes = 8190

data PBoundOutput (s :: S) = PBoundOutput
  { pboundOutput'subject :: Term s (PAsData PVerdictSubject)
  , pboundOutput'index :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PBoundOutput)

data POutputScanState (s :: S) = POutputScanState
  { poutputScan'subject :: Term s (PAsData PVerdictSubject)
  , poutputScan'index :: Term s (PAsData PInteger)
  , poutputScan'itemLength :: Term s (PAsData PInteger)
  , poutputScan'itemHash :: Term s (PAsData PByteString)
  , poutputScan'chunkHashes :: Term s (PAsData (PBuiltinList (PAsData PByteString)))
  , poutputScan'control :: Term s (PAsData Scan.PLedgerOutputScanControlV1)
  , poutputScan'outcome :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct POutputScanState)

pbindOutput :: forall s. Term s (PVerdictSubject :--> PInteger :--> PBoundOutput)
pbindOutput = phoistAcyclic $ plam $ \subject index ->
  pexpecting (Subject.psubjectIsCanonical # subject #&& index #>= 0) $ P.do
    PVerdictSubject{psubject'direction} <- pmatch subject
    pexpecting
      ( pif
          (pfromData psubject'direction #== 1)
          ( pmatch (Subject.prejectionReasonOf # subject) $ \case
              POutputNonCanonical committed -> pfromData committed #== index
              _ -> perror
          )
          (pconstant True)
      )
      $ pcon
      $ PBoundOutput (pdata subject) (pdata index)

pinitialScan :: forall s. Term s (PBoundOutput :--> PByteString :--> POutputScanState)
pinitialScan = phoistAcyclic $ plam $ \bound item -> P.do
  PBoundOutput subject index <- pmatch bound
  len <- plet $ plengthBS # item
  pexpecting (len #<= pmaxOutputBytes) $
    pcon $
      POutputScanState
        subject
        index
        (pdata len)
        (pdata $ pblake2b_256 # item)
        (pdata $ pchunkHashes # item # 0 # len)
        (pdata Scan.pinitialControlV1)
        (pdata poutcomeScanning)

pchunkHashes :: forall s. Term s (PByteString :--> PInteger :--> PInteger :--> PBuiltinList (PAsData PByteString))
pchunkHashes = phoistAcyclic $ pfix $ \self -> plam $ \item offset total ->
  pif (offset #>= total) pnil $ P.do
    len <- plet $ pminimumInteger # pscanChunkBytes # (total - offset)
    pcons # pdata (pblake2b_256 # (psliceBS # offset # len # item)) # (self # item # (offset + len) # total)

pstateIsWellFormed :: forall s. Term s (POutputScanState :--> PBool)
pstateIsWellFormed = phoistAcyclic $ plam $ \state -> P.do
  POutputScanState subject index itemLength itemHash chunkHashes control outcome <- pmatch state
  let len = pfromData itemLength
      hashes = pfromData chunkHashes
      result = pfromData outcome
  Subject.psubjectIsCanonical
    # pfromData subject
    #&& pfromData index
    #>= 0
    #&& len
    #>= 0
    #&& len
    #<= pmaxOutputBytes
    #&& plengthBS
    # pfromData itemHash
    #== 32
    #&& plength
    # hashes
    #== pdiv
    # (len + pscanChunkBytes - 1)
    # pscanChunkBytes
    #&& pall
    # (plam $ \h -> plengthBS # pfromData h #== 32)
    # hashes
    #&& Scan.pcontrolIsWellFormed
    # pfromData control
    #&& (result #== poutcomeScanning #|| result #== poutcomeCanonical #|| result #== poutcomeNonCanonical)
    #&& pif (result #== poutcomeCanonical) (Scan.pterminalIsExactV1 # pfromData control # len) (pconstant True)

padvanceScan :: forall s. Term s (POutputScanState :--> PByteString :--> POutputScanState)
padvanceScan = phoistAcyclic $ plam $ \state window -> P.do
  st@POutputScanState{poutputScan'itemLength, poutputScan'chunkHashes, poutputScan'control, poutputScan'outcome} <- pmatch state
  pexpecting (pstateIsWellFormed # state #&& pfromData poutputScan'outcome #== poutcomeScanning) $ P.do
    control <- plet $ pfromData poutputScan'control
    Scan.PLedgerOutputScanControlV1{Scan.pscan'cursor = cursorData, Scan.pscan'stage = stageData} <- pmatch control
    let len = pfromData poutputScan'itemLength
        cursor = pfromData cursorData
        stage = pfromData stageData
        hashes = pfromData poutputScan'chunkHashes
    next <-
      plet $
        pif
          (stage #== Scan.pstageOptionalField #&& cursor #== len)
          (Scan.pfinishV1 # control # len)
          ( pif
              (cursor #< len)
              ( P.do
                  chunkStart <- plet $ cursor - pmod # cursor # pscanChunkBytes
                  chunkIndex <- plet $ pdiv # cursor # pscanChunkBytes
                  currentLength <- plet $ pminimumInteger # pscanChunkBytes # (len - chunkStart)
                  currentHash <- plet $ pfromData $ pelemAt # chunkIndex # hashes
                  pexpecting (pblake2b_256 # (psliceBS # 0 # currentLength # window) #== currentHash) $ P.do
                    nextLength <- plet $ pminimumInteger # pscanChunkBytes # (len - chunkStart - currentLength)
                    windowLength <- plet $ currentLength + pif (stage #<= Scan.pstageOptionalField #&& nextLength #> 0) nextLength 0
                    pexpecting (plengthBS # window #== windowLength)
                      $ pexpecting
                        ( pif
                            (windowLength #> currentLength)
                            ( pblake2b_256
                                # (psliceBS # currentLength # (windowLength - currentLength) # window)
                                #== pfromData (pelemAt # (chunkIndex + 1) # hashes)
                            )
                            (pconstant True)
                        )
                      $ Scan.pstepV1 # control # len # window # (cursor - chunkStart)
              )
              (pcon PNothing)
          )
    pmatch next $ \case
      PNothing -> pcon st{poutputScan'outcome = pdata poutcomeNonCanonical}
      PJust nextControl ->
        pcon
          st
            { poutputScan'control = pdata nextControl
            , poutputScan'outcome = pdata $ pif (Scan.pterminalIsExactV1 # nextControl # len) poutcomeCanonical poutcomeScanning
            }

pterminalContradiction :: forall s. Term s (POutputScanState :--> PBool)
pterminalContradiction = phoistAcyclic $ plam $ \state -> P.do
  POutputScanState{poutputScan'subject, poutputScan'outcome} <- pmatch state
  pexpecting (pstateIsWellFormed # state #&& pfromData poutputScan'outcome #/= poutcomeScanning) $
    Subject.pterminalContradiction # pfromData poutputScan'subject # (pfromData poutputScan'outcome #== poutcomeNonCanonical)

pencodeBoundOutput :: forall s. Term s (PBoundOutput :--> PByteString)
pencodeBoundOutput = phoistAcyclic $ plam $ \bound -> pserialiseData # pforgetData (pdata bound)

pencodeScanState :: forall s. Term s (POutputScanState :--> PByteString)
pencodeScanState = phoistAcyclic $ plam $ \state -> pserialiseData # pforgetData (pdata state)

pminimumInteger :: forall s. Term s (PInteger :--> PInteger :--> PInteger)
pminimumInteger = phoistAcyclic $ plam $ \left right -> pif (left #< right) left right

-- Source and argument constructor order is the target step ABI.
data PSource (s :: S)
  = PAcceptedSource (Term s (PAsData PNativeTxInclusionCarriage))
  | PForcedSource (Term s (PAsData PInteger)) (Term s (PAsData PInteger)) (Term s (PAsData PHeaderV1)) (Term s (PAsData PRootMembershipProof)) (Term s (PAsData PInteger))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PSource)
data PStep01Args (s :: S) = PStep01Args (Term s (PAsData PSource)) (Term s (PAsData PInteger))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep01Args)
data PStep02Args (s :: S) = PStep02Args (Term s (PAsData PInteger)) (Term s (PAsData PInteger)) (Term s (PAsData PFieldOpeningV1))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep02Args)
data PStep03Args (s :: S) = PStep03Args (Term s (PAsData PInteger)) (Term s (PAsData PInteger)) (Term s (PAsData PByteString))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep03Args)
data PStep04Args (s :: S) = PStep04Args (Term s (PAsData PInteger)) (Term s (PAsData PInteger)) (Term s (PAsData PInteger))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep04Args)
