module Midgard.FraudProofs.NetworkId (
  PNetworkIdFaultV1 (..),
  PPostUtxoPredecessorClaimV1 (..),
  PPostUtxoMembershipV1 (..),
  PPostUtxoStateV1 (..),
  PStep01Args (..),
  PForcedDispatch (..),
  PForcedStepArgs (..),
  PForcedBound (..),
  PForcedScanState (..),
  PForcedScanAction (..),
  pgrammarBatch,
  pscanBatch,
  PPostUtxoPredecessorCarriageV1 (..),
  PStep02State (..),
  PStep02Args (..),
  pnativeNetworkIdNone,
  pisSupportedNetworkIdV1,
  pisTransactionNetworkViolationV1,
  pisOutputNetworkViolationV1,
  pisAnyNetworkViolationV1,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Core.Utils ((#/=))
import Plutarch.LedgerApi.Utils (PMaybeData)
import Plutarch.LedgerApi.V3 (PTxOutRef)
import Plutarch.Prelude

import Midgard.FraudProofs.Common (
  PMembershipCarriage,
  PNativeTxInclusionCarriage,
  PNonMembershipCarriage,
 )
import Midgard.FraudProofs.FieldOpening (PFieldOpeningV1)
import Midgard.LedgerState (PHeaderV1)
import Midgard.TransitionTrace (PRootMembershipProof)

-- | Accepted network faults and the exact forced-rejection marker.
data PNetworkIdFaultV1 (s :: S)
  = PTransactionNetwork
  | POutputNetwork {poutputNetwork'outputIndex :: Term s (PAsData PInteger)}
  | POutputNetworkUtxo {poutputNetworkUtxo'observedNetworkId :: Term s (PAsData PInteger)}
  | PForcedNetworkIdMismatch
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PNetworkIdFaultV1)

-- | Why the wrong-network post-state UTxO is attributable to this transition.
data PPostUtxoPredecessorClaimV1 (s :: S)
  = PIntroduced
  | PNetworkChanged
      { pnetworkChanged'previousDescriptorCbor :: Term s (PAsData PByteString)
      }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PPostUtxoPredecessorClaimV1)

-- | Direct membership of one descriptor under the challenged post-state root.
data PPostUtxoMembershipV1 (s :: S) = PPostUtxoMembershipV1
  { ppostMembership'inputIndex :: Term s (PAsData PInteger)
  , ppostMembership'outputIndex :: Term s (PAsData PInteger)
  , ppostMembership'hubRefInputIndex :: Term s (PAsData PInteger)
  , ppostMembership'stateQueueNodeRefInputIndex :: Term s (PAsData PInteger)
  , ppostMembership'outRef :: Term s (PAsData PTxOutRef)
  , ppostMembership'descriptorCbor :: Term s (PAsData PByteString)
  , ppostMembership'membership :: Term s (PAsData PMembershipCarriage)
  , ppostMembership'predecessor :: Term s (PAsData PPostUtxoPredecessorClaimV1)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PPostUtxoMembershipV1)

-- | Post-state evidence frozen for predecessor culpability in step 02.
data PPostUtxoStateV1 (s :: S) = PPostUtxoStateV1
  { ppostState'outRef :: Term s (PAsData PTxOutRef)
  , ppostState'descriptorCbor :: Term s (PAsData PByteString)
  , ppostState'prevUtxosRoot :: Term s (PAsData PByteString)
  , ppostState'predecessor :: Term s (PAsData PPostUtxoPredecessorClaimV1)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PPostUtxoStateV1)

-- | Step-01 arguments, in Aiken declaration/wire order.
data PStep01Args (s :: S) = PStep01Args
  { pstep01Args'txInclusion :: Term s (PMaybeData PNativeTxInclusionCarriage)
  , pstep01Args'postUtxoMembership :: Term s (PMaybeData PPostUtxoMembershipV1)
  , pstep01Args'forcedSource :: Term s (PMaybeData PForcedDispatch)
  , pstep01Args'fault :: Term s (PAsData PNetworkIdFaultV1)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep01Args)

-- | The predecessor opening must agree with step 01's culpability claim.
data PPostUtxoPredecessorCarriageV1 (s :: S)
  = PIntroducedPredecessor (Term s (PAsData PNonMembershipCarriage))
  | PNetworkChangedPredecessor (Term s (PAsData PMembershipCarriage))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PPostUtxoPredecessorCarriageV1)

-- | Everything step 02 may trust from the authenticated first opening.
data PStep02State (s :: S) = PStep02State
  { pstep02State'badTxId :: Term s (PAsData PByteString)
  , pstep02State'committedTxNetworkId :: Term s (PAsData PInteger)
  , pstep02State'expectedNetworkId :: Term s (PAsData PInteger)
  , pstep02State'fault :: Term s (PAsData PNetworkIdFaultV1)
  , pstep02State'postUtxo :: Term s (PMaybeData PPostUtxoStateV1)
  , pstep02State'forcedSourceKey :: Term s (PMaybeData PByteString)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep02State)

-- | Step-02 arguments, in Aiken declaration/wire order.
data PStep02Args (s :: S) = PStep02Args
  { pstep02Args'inputIndex :: Term s (PAsData PInteger)
  , pstep02Args'outputIndex :: Term s (PAsData PInteger)
  , pstep02Args'fraudProofMintRedeemerIndex :: Term s (PAsData PInteger)
  , pstep02Args'outputsOpening :: Term s (PMaybeData PFieldOpeningV1)
  , pstep02Args'predecessorCarriage :: Term s (PMaybeData PPostUtxoPredecessorCarriageV1)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep02Args)

-- | Canonical compact spelling of an absent optional Cardano network-id field.
pnativeNetworkIdNone :: forall s. Term s PInteger
pnativeNetworkIdNone = 255

pisSupportedNetworkIdV1 :: forall s. Term s (PInteger :--> PBool)
pisSupportedNetworkIdV1 = phoistAcyclic $ plam $ \networkId ->
  networkId #== 0 #|| networkId #== 1

pisTransactionNetworkViolationV1 :: forall s. Term s (PInteger :--> PInteger :--> PBool)
pisTransactionNetworkViolationV1 = phoistAcyclic $ plam $ \committed expected ->
  committed #/= pnativeNetworkIdNone #&& committed #/= expected

pisOutputNetworkViolationV1 :: forall s. Term s (PInteger :--> PInteger :--> PBool)
pisOutputNetworkViolationV1 = phoistAcyclic $ plam $ \observed expected -> observed #/= expected

-- Constructor and field order follow the target forced-step/forced-scan ABI.
data PForcedDispatch (s :: S) = PForcedDispatch (Term s (PAsData PInteger)) (Term s (PAsData PInteger))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PForcedDispatch)
data PForcedStepArgs (s :: S)
  = PForcedStepArgs
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PHeaderV1))
      (Term s (PAsData PRootMembershipProof))
      (Term s (PAsData PInteger))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PForcedStepArgs)
data PForcedBound (s :: S) = PForcedBound
  { pforcedBound'txId :: Term s (PAsData PByteString)
  , pforcedBound'committedNetwork :: Term s (PAsData PInteger)
  , pforcedBound'expectedNetwork :: Term s (PAsData PInteger)
  , pforcedBound'sourceKey :: Term s (PAsData PByteString)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PForcedBound)
data PForcedScanState (s :: S)
  = PReady (Term s (PAsData PForcedBound))
  | PGrammar (Term s (PAsData PForcedBound)) (Term s (PAsData PByteString))
  | PScanning (Term s (PAsData PForcedBound)) (Term s (PAsData PByteString))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PForcedScanState)
data PForcedScanAction (s :: S)
  = POpen (Term s (PAsData PInteger)) (Term s (PAsData PInteger)) (Term s (PAsData PFieldOpeningV1))
  | PStartGrammar (Term s (PAsData PInteger)) (Term s (PAsData PInteger)) (Term s (PAsData PFieldOpeningV1)) (Term s (PAsData PInteger))
  | PResumeGrammar (Term s (PAsData PInteger)) (Term s (PAsData PInteger)) (Term s (PAsData PFieldOpeningV1)) (Term s (PAsData PByteString)) (Term s (PAsData PInteger))
  | PFinishGrammar (Term s (PAsData PInteger)) (Term s (PAsData PInteger)) (Term s (PAsData PFieldOpeningV1)) (Term s (PAsData PByteString))
  | PAdvance (Term s (PAsData PInteger)) (Term s (PAsData PInteger)) (Term s (PAsData PFieldOpeningV1)) (Term s (PAsData PByteString)) (Term s (PAsData PInteger))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PForcedScanAction)

pgrammarBatch, pscanBatch :: forall s. Term s PInteger
pgrammarBatch = 128
pscanBatch = 64

pisAnyNetworkViolationV1 :: forall s. Term s (PInteger :--> PBuiltinList PInteger :--> PInteger :--> PBool)
pisAnyNetworkViolationV1 = phoistAcyclic $ plam $ \committed outputs expected ->
  pisTransactionNetworkViolationV1
    # committed
    # expected
    #|| pany
    # (plam $ \observed -> observed #/= expected)
    # outputs
