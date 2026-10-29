-- | CEK-owned carriers passed through the shared redeemer-item executor.
module Midgard.CekContextItem where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Midgard.CekContextChain qualified as Chain
import Midgard.CekContextItemWire qualified as Wire
import Midgard.CekContextWire qualified as ContextWire
import Midgard.CekData qualified as Data
import Midgard.RedeemerItemProof qualified as Item
import Midgard.ValidationMachine qualified as VM
import Midgard.ValidationResolutionData (bytesField, decodeTransition, integerField, recordFields)
import Plutarch.Prelude

data PAction s = PAction
    { paction'inputIndex :: Term s (PAsData PInteger)
    , paction'outputIndex :: Term s (PAsData PInteger)
    , paction'transition :: Term s (PAsData VM.PValidationOneStepWitnessV1)
    , paction'auxiliary :: Term s PData
    , paction'claimedNext :: Term s (PAsData Item.PRedeemerItemProofControlV1)
    }
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PAction)
data PPending s = PPending
    { ppending'staged :: Term s PData
    , ppending'control :: Term s (PAsData Item.PRedeemerItemProofControlV1)
    , ppending'witnessHash :: Term s (PAsData PByteString)
    , ppending'claimedNext :: Term s (PAsData Item.PRedeemerItemProofControlV1)
    }
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PPending)
data PVerified s = PVerified {pverified'staged :: Term s PData, pverified'next :: Term s (PAsData Item.PRedeemerItemProofControlV1)}
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PVerified)
data PHashResult s = PHashResult {phash'verified :: Term s PData, phash'next :: Term s (PAsData PByteString)}
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PHashResult)
data PValueResult s = PValueResult {pvalue'verified :: Term s PData, pvalue'summary :: Term s (PAsData Data.PDataSummaryV1)}
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PValueResult)

pdecodeAction :: forall s. Term s (PData :--> PAction)
pdecodeAction = phoistAcyclic $ plam $ \raw -> plet (recordFields 5 raw) $ \f ->
    pcon $ PAction (integerField f 0) (integerField f 1) (pdata $ decodeTransition $ pelemAt # 2 # f) (pelemAt # 3 # f) (pdata $ Wire.pdecodeRedeemerItemProofControl # (pelemAt # 4 # f))
pcontextAction :: forall s. Term s (PAction :--> Chain.PAction)
pcontextAction = phoistAcyclic $ plam $ \action -> pmatch action $ \a ->
    pcon $ Chain.PAction (paction'inputIndex a) (paction'outputIndex a) (paction'transition a) (paction'auxiliary a)
pdecodePending :: forall s. Term s (PData :--> PPending)
pdecodePending = phoistAcyclic $ plam $ \raw -> plet (recordFields 4 raw) $ \f ->
    pcon $ PPending (phead # f) (pdata $ Wire.pdecodeRedeemerItemProofControl # (pelemAt # 1 # f)) (bytesField f 2) (pdata $ Wire.pdecodeRedeemerItemProofControl # (pelemAt # 3 # f))
pdecodeVerified :: forall s. Term s (PData :--> PVerified)
pdecodeVerified = phoistAcyclic $ plam $ \raw -> plet (recordFields 2 raw) $ \f ->
    pcon $ PVerified (phead # f) (pdata $ Wire.pdecodeRedeemerItemProofControl # (pelemAt # 1 # f))
pdecodeHashResult :: forall s. Term s (PData :--> PHashResult)
pdecodeHashResult = phoistAcyclic $ plam $ \raw -> plet (recordFields 2 raw) $ \f -> pcon $ PHashResult (phead # f) (bytesField f 1)
pdecodeValueResult :: forall s. Term s (PData :--> PValueResult)
pdecodeValueResult = phoistAcyclic $ plam $ \raw -> plet (recordFields 2 raw) $ \f -> pcon $ PValueResult (phead # f) (pdata $ ContextWire.pdecodeDataSummary # (pelemAt # 1 # f))
