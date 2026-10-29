module Midgard.FraudProofs.MissingNativeScriptUtxo (
    PStep01Args (..),
    PStep02State (..),
    PStep02Args (..),
    PStep03State (..),
    PStep03Args (..),
    PStep04State (..),
    PStep04Args (..),
    PStep05PhaseV1 (..),
    PStep05State (..),
    PStep05Args (..),
    PStep06Args (..),
    PStep07Args (..),
    pdirectScriptWitnessLimit,
    pstagedScriptWitnessBatchLimit,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.LedgerApi.V3 (PTxOutRef)
import Plutarch.Prelude

import Midgard.FraudProofs.Common (PMembershipCarriage, PNativeTxInclusionCarriage)
import Midgard.FraudProofs.FieldOpening (PFieldOpeningV1)
import Midgard.FraudProofs.NativeTx.Types (PMidgardTxInput)

newtype PStep01Args s = PStep01Args
    {pstep01Args'carriage :: Term s (PAsData PNativeTxInclusionCarriage)}
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PStep01Args)

data PStep02State s = PStep02State
    { pstep02State'badTxId :: Term s (PAsData PByteString)
    , pstep02State'badTxWitnessSetHash :: Term s (PAsData PByteString)
    , pstep02State'prevUtxosRoot :: Term s (PAsData PByteString)
    }
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PStep02State)

data PStep02Args s = PStep02Args
    { pstep02Args'inputIndex :: Term s (PAsData PInteger)
    , pstep02Args'outputIndex :: Term s (PAsData PInteger)
    , pstep02Args'badInputIndex :: Term s (PAsData PInteger)
    , pstep02Args'spendInputsOpening :: Term s (PAsData PFieldOpeningV1)
    }
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PStep02Args)

data PStep03State s = PStep03State
    { pstep03State'inputWithMissingScript :: Term s (PAsData PMidgardTxInput)
    , pstep03State'badTxId :: Term s (PAsData PByteString)
    , pstep03State'badTxWitnessSetHash :: Term s (PAsData PByteString)
    , pstep03State'prevUtxosRoot :: Term s (PAsData PByteString)
    }
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PStep03State)

data PStep03Args s = PStep03Args
    { pstep03Args'inputIndex :: Term s (PAsData PInteger)
    , pstep03Args'outputIndex :: Term s (PAsData PInteger)
    , pstep03Args'outRef :: Term s (PAsData PTxOutRef)
    , pstep03Args'descriptorCbor :: Term s (PAsData PByteString)
    , pstep03Args'membership :: Term s (PAsData PMembershipCarriage)
    }
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PStep03Args)

data PStep04State s = PStep04State
    { pstep04State'outRef :: Term s (PAsData PTxOutRef)
    , pstep04State'descriptorCbor :: Term s (PAsData PByteString)
    , pstep04State'badTxId :: Term s (PAsData PByteString)
    , pstep04State'badTxWitnessSetHash :: Term s (PAsData PByteString)
    }
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PStep04State)

data PStep04Args s = PStep04Args
    { pstep04Args'inputIndex :: Term s (PAsData PInteger)
    , pstep04Args'outputIndex :: Term s (PAsData PInteger)
    , pstep04Args'missingNativeScriptBytes :: Term s (PAsData PByteString)
    }
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PStep04Args)

data PStep05PhaseV1 s
    = PReady
    | PGrammarCertification (Term s (PAsData PByteString))
    | PSemanticScan (Term s (PAsData PByteString)) (Term s (PAsData PBool))
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PStep05PhaseV1)

data PStep05State s = PStep05State
    { pstep05State'expectedMissingScriptHash :: Term s (PAsData PByteString)
    , pstep05State'badTxId :: Term s (PAsData PByteString)
    , pstep05State'badTxWitnessSetHash :: Term s (PAsData PByteString)
    , pstep05State'phase :: Term s (PAsData PStep05PhaseV1)
    }
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PStep05State)

data PStep05Args s
    = PDirectFinalize
        (Term s (PAsData PInteger))
        (Term s (PAsData PInteger))
        (Term s (PAsData PInteger))
        (Term s (PAsData PFieldOpeningV1))
    | PStartGrammarCertification
        (Term s (PAsData PInteger))
        (Term s (PAsData PInteger))
        (Term s (PAsData PFieldOpeningV1))
        (Term s (PAsData PInteger))
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PStep05Args)

data PStep06Args s
    = PResumeGrammarCertification
        (Term s (PAsData PInteger))
        (Term s (PAsData PInteger))
        (Term s (PAsData PFieldOpeningV1))
        (Term s (PAsData PByteString))
        (Term s (PAsData PInteger))
    | PStartSemanticScan
        (Term s (PAsData PInteger))
        (Term s (PAsData PInteger))
        (Term s (PAsData PFieldOpeningV1))
        (Term s (PAsData PByteString))
        (Term s (PAsData PInteger))
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PStep06Args)

data PStep07Args s
    = PResumeSemanticScan
        (Term s (PAsData PInteger))
        (Term s (PAsData PInteger))
        (Term s (PAsData PFieldOpeningV1))
        (Term s (PAsData PByteString))
        (Term s (PAsData PInteger))
    | PFinalizeSemanticScan
        (Term s (PAsData PInteger))
        (Term s (PAsData PInteger))
        (Term s (PAsData PInteger))
        (Term s (PAsData PFieldOpeningV1))
        (Term s (PAsData PByteString))
        (Term s (PAsData PInteger))
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PStep07Args)

pdirectScriptWitnessLimit :: forall s. Term s PInteger
pdirectScriptWitnessLimit = 64

pstagedScriptWitnessBatchLimit :: forall s. Term s PInteger
pstagedScriptWitnessBatchLimit = 32
