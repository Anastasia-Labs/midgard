module Midgard.FraudProofs.MinAda (
  PMinAdaFaultV1 (..),
  PPostUtxoMembershipV1 (..),
  PPostUtxoStateV1 (..),
  PStep01Args (..),
  PStep02State (..),
  PStep02Args (..),
  PStep03State (..),
  PStep03Args (..),
  PStep04State (..),
  PStep04Args (..),
  PStep05State (..),
  PStep05Args (..),
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
  { pstep01Args'txInclusion :: Term s (PMaybeData PNativeTxInclusionCarriage)
  , pstep01Args'postUtxoMembership :: Term s (PMaybeData PPostUtxoMembershipV1)
  , pstep01Args'fault :: Term s (PAsData PMinAdaFaultV1)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep01Args)

data PStep02State (s :: S) = PStep02State
  { pstep02State'badTxId :: Term s (PAsData PByteString)
  , pstep02State'fault :: Term s (PAsData PMinAdaFaultV1)
  , pstep02State'postUtxo :: Term s (PMaybeData PPostUtxoStateV1)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep02State)

data PStep02Args (s :: S) = PStep02Args
  { pstep02Args'inputIndex :: Term s (PAsData PInteger)
  , pstep02Args'outputIndex :: Term s (PAsData PInteger)
  , pstep02Args'outputsOpening :: Term s (PMaybeData PFieldOpeningV1)
  , pstep02Args'postMembership :: Term s (PMaybeData PMembershipCarriage)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep02Args)

data PStep03State (s :: S) = PStep03State
  { pstep03State'descriptorCbor :: Term s (PAsData PByteString)
  , pstep03State'outRefKey :: Term s (PAsData PByteString)
  , pstep03State'prevUtxosRoot :: Term s (PAsData PByteString)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PStep03State)

data PStep03Args (s :: S) = PStep03Args
  { pstep03Args'inputIndex :: Term s (PAsData PInteger)
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
