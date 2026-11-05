-- | Authenticated redeemer-selection carriers and their context successor.
module Midgard.CekContextRedeemer where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Midgard.CekContextChain qualified as Chain
import Midgard.CekContextStep qualified as Step
import Midgard.CekContextWire qualified as Wire
import Midgard.CekData qualified as Data
import Midgard.ValidationMachine qualified as VM
import Midgard.ValidationResolutionData (bytesField, integerField, recordFields)
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.Prelude

data PSelectionPending s = PSelectionPending
    { pselection'staged :: Term s PData
    , pselection'control :: Term s (PAsData VM.PCekRedeemerContextControlV1)
    , pselection'itemIndex :: Term s (PAsData PInteger)
    , pselection'itemCount :: Term s (PAsData PInteger)
    , pselection'totalLength :: Term s (PAsData PInteger)
    , pselection'itemCommitment :: Term s (PAsData PByteString)
    , pselection'redeemerLeaf :: Term s (PAsData PByteString)
    , pselection'purposeKind :: Term s (PAsData PInteger)
    , pselection'purposeIndex :: Term s (PAsData PInteger)
    , pselection'scriptHash :: Term s (PAsData PByteString)
    , pselection'subject :: Term s (PAsData PByteString)
    }
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PSelectionPending)

data PPurposePending s = PPurposePending
    { ppurpose'selection :: Term s (PAsData PSelectionPending)
    , ppurpose'purpose :: Term s (PAsData (PMaybeData Data.PDataSummaryV1))
    }
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PPurposePending)

data PInitialPending s = PInitialPending
    { pinitial'selection :: Term s (PAsData PSelectionPending)
    , pinitial'purpose :: Term s (PAsData (PMaybeData Data.PDataSummaryV1))
    , pinitial'hash :: Term s (PAsData PByteString)
    }
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PInitialPending)

pdecodeSelection :: forall s. Term s (PData :--> PSelectionPending)
pdecodeSelection = phoistAcyclic $ plam $ \raw -> plet (recordFields 11 raw) $ \f ->
    pcon $
        PSelectionPending
            (pelemAt # 0 # f)
            (pdata $ Wire.pdecodeCekRedeemerContextControl # (pelemAt # 1 # f))
            (integerField f 2)
            (integerField f 3)
            (integerField f 4)
            (bytesField f 5)
            (bytesField f 6)
            (integerField f 7)
            (integerField f 8)
            (bytesField f 9)
            (bytesField f 10)

pdecodeOptionalSummary :: forall s. Term s PData -> Term s (PMaybeData Data.PDataSummaryV1)
pdecodeOptionalSummary raw = pmatch (pasConstr # raw) $ \(PBuiltinPair tag fields) ->
    pif (tag #== 1 #&& pnull # fields) (pcon PDNothing) $
        pif (tag #== 0 #&& plength # fields #== 1) (pcon $ PDJust $ pdata $ Wire.pdecodeDataSummary # (phead # fields)) perror

pdecodePurpose :: forall s. Term s (PData :--> PPurposePending)
pdecodePurpose = phoistAcyclic $ plam $ \raw -> plet (recordFields 2 raw) $ \f ->
    pcon $
        PPurposePending
            (pdata $ pdecodeSelection # (phead # f))
            (pdata $ pdecodeOptionalSummary $ pelemAt # 1 # f)
pdecodeInitial :: forall s. Term s (PData :--> PInitialPending)
pdecodeInitial = phoistAcyclic $ plam $ \raw -> plet (recordFields 3 raw) $ \f ->
    pcon $
        PInitialPending
            (pdata $ pdecodeSelection # (phead # f))
            (pdata $ pdecodeOptionalSummary $ pelemAt # 1 # f)
            (bytesField f 2)

pcontextSuccessor :: forall s. Term s (Chain.PStaged :--> VM.PCekRedeemerContextControlV1 :--> VM.PCekContextControlV1)
pcontextSuccessor = phoistAcyclic $ plam $ \staged next -> pmatch staged $ \s -> pmatch next $ \n ->
    pmatch (pfromData $ Chain.pstaged'context s) $ \c -> pmatch (pfromData $ Chain.pstaged'native s) $ \native ->
        pif
            (VM.pcekRedeemerContextControlIsWellFormed # pfromData (Step.pnative'redeemerCount native) # next)
            ( pif
                (VM.pcekRedeemer'cursor n #== Step.pnative'redeemerCount native)
                ( pmatch (pfromData $ VM.pcekRedeemer'currentRedeemer n) $ \summary ->
                    pif
                        (pfromData (VM.pcekRedeemer'activeScanHash n) #== pconstant "" #&& plengthBS # pfromData (Data.psummary'root summary) #== 32)
                        (pcon c{VM.pcekContext'stage = pdata 10, VM.pcekContext'redeemerContextControlHash = pdata $ VM.phashCekRedeemerContextControlV1 # next})
                        perror
                )
                (pcon c{VM.pcekContext'redeemerContextControlHash = pdata $ VM.phashCekRedeemerContextControlV1 # next})
            )
            perror
