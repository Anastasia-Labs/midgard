module Midgard.FraudProofs.MinAda (
  PMinAdaFaultV1 (..),
  PPostUtxoMembershipV1 (..),
  PPostUtxoStateV1 (..),
  PForcedSource (..),
  PStep01Args (..),
  PStep02State (..),
  PStep02Args (..),
  PStep03State (..),
  PStep03Args (..),
  PStep04State (..),
  PStep04Args (..),
  PStep05State (..),
  PStep05Args (..),
  padvanceMinAdaScan,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.LedgerApi.Utils (PMaybeData)
import Plutarch.LedgerApi.V3 (PTxOutRef)
import Plutarch.Prelude

import Midgard.FraudProofs.Common (
  PMembershipCarriage,
  PNativeTxInclusionCarriage,
  PNonMembershipCarriage,
 )
import Midgard.FraudProofs.FieldOpening (PFieldOpeningV1)
import Midgard.FraudProofs.TransactionOutputNonCanonical (POutputScanState (..))
import Midgard.FraudProofs.TransactionOutputNonCanonical qualified as OutputScan
import Midgard.LedgerOutputScan qualified as Scan
import Midgard.LedgerState (PHeaderV1)
import Midgard.TransitionTrace (PRootMembershipProof)

data PMinAdaFaultV1 (s :: S)
  = PMinAdaTx {pminAdaTx'outputIndex :: Term s (PAsData PInteger)}
  | PMinAdaUtxo
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PMinAdaFaultV1)

data PPostUtxoMembershipV1 (s :: S) = PPostUtxoMembershipV1
  { ppostMembership'inputIndex :: Term s (PAsData PInteger)
  , ppostMembership'outputIndex :: Term s (PAsData PInteger)
  , ppostMembership'hubRefInputIndex :: Term s (PAsData PInteger)
  , ppostMembership'stateQueueNodeRefInputIndex :: Term s (PAsData PInteger)
  , ppostMembership'outRef :: Term s (PAsData PTxOutRef)
  , ppostMembership'descriptorCbor :: Term s (PAsData PByteString)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PPostUtxoMembershipV1)

data PPostUtxoStateV1 (s :: S) = PPostUtxoStateV1
  { ppostState'outRef :: Term s (PAsData PTxOutRef)
  , ppostState'descriptorCbor :: Term s (PAsData PByteString)
  , ppostState'postUtxosRoot :: Term s (PAsData PByteString)
  , ppostState'prevUtxosRoot :: Term s (PAsData PByteString)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PPostUtxoStateV1)

data PStep01Args (s :: S) = PStep01Args
  { pstep01Args'forcedSource :: Term s (PMaybeData PForcedSource)
  , pstep01Args'txInclusion :: Term s (PMaybeData PNativeTxInclusionCarriage)
  , pstep01Args'postUtxoMembership :: Term s (PMaybeData PPostUtxoMembershipV1)
  , pstep01Args'fault :: Term s (PAsData PMinAdaFaultV1)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep01Args)

data PStep02State (s :: S) = PStep02State
  { pstep02State'grammarCheckpointHash :: Term s (PAsData PByteString)
  , pstep02State'grammarComplete :: Term s (PAsData PBool)
  , pstep02State'walkCheckpointHash :: Term s (PAsData PByteString)
  , pstep02State'direction :: Term s (PAsData PInteger)
  , pstep02State'badTxId :: Term s (PAsData PByteString)
  , pstep02State'fault :: Term s (PAsData PMinAdaFaultV1)
  , pstep02State'postUtxo :: Term s (PMaybeData PPostUtxoStateV1)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep02State)

data PStep02Args (s :: S) = PStep02Args
  { pstep02Args'grammarCheckpointBytes :: Term s (PAsData PByteString)
  , pstep02Args'walkCheckpointBytes :: Term s (PAsData PByteString)
  , pstep02Args'inputIndex :: Term s (PAsData PInteger)
  , pstep02Args'outputIndex :: Term s (PAsData PInteger)
  , pstep02Args'yieldToRefInputIndex :: Term s (PAsData PInteger)
  , pstep02Args'outputsOpening :: Term s (PMaybeData PFieldOpeningV1)
  , pstep02Args'postMembership :: Term s (PMaybeData PMembershipCarriage)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep02Args)

data PStep03State (s :: S)
  = PMinAdaTxScan (Term s (PAsData PInteger)) (Term s (PAsData POutputScanState))
  | PMinAdaTxDescriptor (Term s (PAsData PInteger)) (Term s (PAsData PInteger)) (Term s (PAsData PInteger))
  | PMinAdaUtxoDescriptor
      { pstep03State'descriptorCbor :: Term s (PAsData PByteString)
      , pstep03State'outRefKey :: Term s (PAsData PByteString)
      , pstep03State'prevUtxosRoot :: Term s (PAsData PByteString)
      }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep03State)

data PStep03Args (s :: S) = PStep03Args
  { pstep03Args'window :: Term s (PAsData PByteString)
  , pstep03Args'inputIndex :: Term s (PAsData PInteger)
  , pstep03Args'outputIndex :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep03Args)

data PStep04State (s :: S) = PStep04State
  { pstep04State'outRefKey :: Term s (PAsData PByteString)
  , pstep04State'prevUtxosRoot :: Term s (PAsData PByteString)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep04State)

data PStep04Args (s :: S) = PStep04Args
  { pstep04Args'inputIndex :: Term s (PAsData PInteger)
  , pstep04Args'outputIndex :: Term s (PAsData PInteger)
  , pstep04Args'predecessorNonMembership :: Term s (PAsData PNonMembershipCarriage)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep04Args)

data PStep05State (s :: S) = PPredicateAndCulpabilityAuthenticated
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep05State)

data PStep05Args (s :: S) = PStep05Args
  { pstep05Args'inputIndex :: Term s (PAsData PInteger)
  , pstep05Args'outputIndex :: Term s (PAsData PInteger)
  , pstep05Args'fraudProofMintRedeemerIndex :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep05Args)

data PForcedSource (s :: S)
  = PForcedSource
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PHeaderV1))
      (Term s (PAsData PRootMembershipProof))
      (Term s (PAsData PInteger))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PForcedSource)

-- Exactly four semantic scanner transitions, stopping before the next chunk
-- or a token/payload window-mode change, as target min_ada/scan.advance.
padvanceMinAdaScan :: forall s. Term s (POutputScanState :--> PByteString :--> POutputScanState)
padvanceMinAdaScan = phoistAcyclic $ plam $ \state window -> padvanceBatch # state # window # 4

padvanceBatch :: forall s. Term s (POutputScanState :--> PByteString :--> PInteger :--> POutputScanState)
padvanceBatch = phoistAcyclic $ pfix $ \self -> plam $ \state window remaining ->
  plet (OutputScan.padvanceScan # state # window) $ \next ->
    pmatch state $ \POutputScanState{poutputScan'control = oldControl} ->
      pmatch next $ \POutputScanState{poutputScan'control = nextControl, poutputScan'outcome} ->
        pmatch (pfromData oldControl) $ \old ->
          pmatch (pfromData nextControl) $ \new ->
            pif
              ( remaining
                  #> 1
                  #&& pfromData poutputScan'outcome
                  #== OutputScan.poutcomeScanning
                  #&& (pdiv # pfromData (Scan.pscan'cursor old) # OutputScan.pscanChunkBytes)
                  #== (pdiv # pfromData (Scan.pscan'cursor new) # OutputScan.pscanChunkBytes)
                  #&& (pfromData (Scan.pscan'stage old) #<= Scan.pstageOptionalField)
                  #== (pfromData (Scan.pscan'stage new) #<= Scan.pstageOptionalField)
              )
              (self # next # window # (remaining - 1))
              next
