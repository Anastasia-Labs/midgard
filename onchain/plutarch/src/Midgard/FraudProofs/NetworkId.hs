module Midgard.FraudProofs.NetworkId (
  PNetworkIdFaultV1 (..),
  PPostUtxoPredecessorClaimV1 (..),
  PPostUtxoMembershipV1 (..),
  PPostUtxoStateV1 (..),
  PStep01Args (..),
  PPostUtxoPredecessorCarriageV1 (..),
  PStep02State (..),
  PStep02Args (..),
  pnativeNetworkIdNone,
  pisSupportedNetworkIdV1,
  pisTransactionNetworkViolationV1,
  pisOutputNetworkViolationV1,
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

-- | Which of Q35's three authenticated network claims is being adjudicated.
data PNetworkIdFaultV1 (s :: S)
  = PTransactionNetwork
  | POutputNetwork {poutputNetwork'outputIndex :: Term s (PAsData PInteger)}
  | POutputNetworkUtxo {poutputNetworkUtxo'observedNetworkId :: Term s (PAsData PInteger)}
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
