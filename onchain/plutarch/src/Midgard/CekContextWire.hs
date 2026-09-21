-- | Checked context wire records; no domain checks on deployment parameters.
module Midgard.CekContextWire where

import Midgard.CekData qualified as Data
import Midgard.ValidationMachine qualified as VM
import Midgard.ValidationMerkle qualified as Merkle
import Midgard.ValidationResolutionData (recordFields)
import Plutarch.Prelude

pdecodeFrontierPeak :: forall s. Term s (PData :--> Merkle.PFrontierPeak)
pdecodeFrontierPeak = phoistAcyclic $ plam $ \raw -> plet (recordFields 2 raw) $ \fields ->
    pcon $
        Merkle.PFrontierPeak
            (pdata $ (pasInt # (pelemAt # 0 # fields)))
            (pdata $ (pasByteStr # (pelemAt # 1 # fields)))

pdecodeDataSummary :: forall s. Term s (PData :--> Data.PDataSummaryV1)
pdecodeDataSummary = phoistAcyclic $ plam $ \raw -> plet (recordFields 3 raw) $ \fields ->
    pcon $
        Data.PDataSummaryV1
            (pdata $ (pasByteStr # (pelemAt # 0 # fields)))
            (pdata $ (pasInt # (pelemAt # 1 # fields)))
            (pdata $ (pasInt # (pelemAt # 2 # fields)))

pdecodeDataSequenceSummary :: forall s. Term s (PData :--> Data.PDataSequenceSummaryV1)
pdecodeDataSequenceSummary = phoistAcyclic $ plam $ \raw -> plet (recordFields 4 raw) $ \fields ->
    pcon $
        Data.PDataSequenceSummaryV1
            (pdata $ (pasByteStr # (pelemAt # 0 # fields)))
            (pdata $ (pasInt # (pelemAt # 1 # fields)))
            (pdata $ (pasInt # (pelemAt # 2 # fields)))
            (pdata $ (pasInt # (pelemAt # 3 # fields)))

pdecodeNativeScriptsControl :: forall s. Term s (PData :--> VM.PNativeScriptsControlV1)
pdecodeNativeScriptsControl = phoistAcyclic $ plam $ \raw -> plet (recordFields 26 raw) $ \fields ->
    pcon $
        VM.PNativeScriptsControlV1
            (pdata $ (pasByteStr # (pelemAt # 0 # fields)))
            (pdata $ (pasByteStr # (pelemAt # 1 # fields)))
            (pdata $ (pasByteStr # (pelemAt # 2 # fields)))
            (pdata $ (pasByteStr # (pelemAt # 3 # fields)))
            (pdata $ (pasInt # (pelemAt # 4 # fields)))
            (pdata $ (pasByteStr # (pelemAt # 5 # fields)))
            (pdata $ (pasInt # (pelemAt # 6 # fields)))
            (pdata $ (pmap # plam (\item -> (pdata $ (pdecodeFrontierPeak # item))) # (pasList # (pelemAt # 7 # fields))))
            (pdata $ (pasInt # (pelemAt # 8 # fields)))
            (pdata $ (pasByteStr # (pelemAt # 9 # fields)))
            (pdata $ (pasInt # (pelemAt # 10 # fields)))
            (pdata $ (pmap # plam (\item -> (pdata $ (pdecodeFrontierPeak # item))) # (pasList # (pelemAt # 11 # fields))))
            (pdata $ (pasInt # (pelemAt # 12 # fields)))
            (pdata $ (pmap # plam (\item -> (pdata $ (pdecodeFrontierPeak # item))) # (pasList # (pelemAt # 13 # fields))))
            (pdata $ (pasInt # (pelemAt # 14 # fields)))
            (pdata $ (pmap # plam (\item -> (pdata $ (pdecodeFrontierPeak # item))) # (pasList # (pelemAt # 15 # fields))))
            (pdata $ (pasInt # (pelemAt # 16 # fields)))
            (pdata $ (pmap # plam (\item -> (pdata $ (pdecodeFrontierPeak # item))) # (pasList # (pelemAt # 17 # fields))))
            (pdata $ (pmap # plam (\item -> (pdata $ (pdecodeFrontierPeak # item))) # (pasList # (pelemAt # 18 # fields))))
            (pdata $ (pasInt # (pelemAt # 19 # fields)))
            (pdata $ (pmap # plam (\item -> (pdata $ (pdecodeFrontierPeak # item))) # (pasList # (pelemAt # 20 # fields))))
            (pdata $ (pasInt # (pelemAt # 21 # fields)))
            (pdata $ (pmap # plam (\item -> (pdata $ (pdecodeFrontierPeak # item))) # (pasList # (pelemAt # 22 # fields))))
            (pdata $ (pasInt # (pelemAt # 23 # fields)))
            (pdata $ (pasInt # (pelemAt # 24 # fields)))
            (pdata $ (pasByteStr # (pelemAt # 25 # fields)))

pdecodeCekRedeemerContextControl :: forall s. Term s (PData :--> VM.PCekRedeemerContextControlV1)
pdecodeCekRedeemerContextControl = phoistAcyclic $ plam $ \raw -> plet (recordFields 6 raw) $ \fields ->
    pcon $
        VM.PCekRedeemerContextControlV1
            (pdata $ (pasInt # (pelemAt # 0 # fields)))
            (pdata $ (pdecodeDataSequenceSummary # (pelemAt # 1 # fields)))
            (pdata $ (pasByteStr # (pelemAt # 2 # fields)))
            (pdata $ (pasByteStr # (pelemAt # 3 # fields)))
            (pdata $ (pdecodeDataSummary # (pelemAt # 4 # fields)))
            (pdata $ (pdecodeDataSummary # (pelemAt # 5 # fields)))

pdecodeCekContextPartsControl :: forall s. Term s (PData :--> VM.PCekContextPartsControlV1)
pdecodeCekContextPartsControl = phoistAcyclic $ plam $ \raw -> plet (recordFields 3 raw) $ \fields ->
    pcon $
        VM.PCekContextPartsControlV1
            (pdata $ (pdecodeDataSequenceSummary # (pelemAt # 0 # fields)))
            (pdata $ (pdecodeDataSummary # (pelemAt # 1 # fields)))
            (pdata $ (pdecodeDataSummary # (pelemAt # 2 # fields)))

pdecodeCekFinalContextControl :: forall s. Term s (PData :--> VM.PCekFinalContextControlV1)
pdecodeCekFinalContextControl = phoistAcyclic $ plam $ \raw -> plet (recordFields 3 raw) $ \fields ->
    pcon $
        VM.PCekFinalContextControlV1
            (pdata $ (pdecodeDataSummary # (pelemAt # 0 # fields)))
            (pdata $ (pdecodeDataSummary # (pelemAt # 1 # fields)))
            (pdata $ (pdecodeDataSummary # (pelemAt # 2 # fields)))

pdecodeCekTxInfoAssemblyControl :: forall s. Term s (PData :--> VM.PCekTxInfoAssemblyControlV1)
pdecodeCekTxInfoAssemblyControl = phoistAcyclic $ plam $ \raw -> plet (recordFields 3 raw) $ \fields ->
    pcon $
        VM.PCekTxInfoAssemblyControlV1
            (pdata $ (pdecodeDataSequenceSummary # (pelemAt # 0 # fields)))
            (pdata $ (pdecodeDataSummary # (pelemAt # 1 # fields)))
            (pdata $ (pdecodeDataSummary # (pelemAt # 2 # fields)))

pdecodeCekContextControl :: forall s. Term s (PData :--> VM.PCekContextControlV1)
pdecodeCekContextControl = phoistAcyclic $ plam $ \raw -> plet (recordFields 25 raw) $ \fields ->
    pcon $
        VM.PCekContextControlV1
            (pdata $ (pasInt # (pelemAt # 0 # fields)))
            (pdata $ (pasInt # (pelemAt # 1 # fields)))
            (pdata $ (pasByteStr # (pelemAt # 2 # fields)))
            (pdata $ (pasByteStr # (pelemAt # 3 # fields)))
            (pdata $ (pasInt # (pelemAt # 4 # fields)))
            (pdata $ (pasInt # (pelemAt # 5 # fields)))
            (pdata $ (pasByteStr # (pelemAt # 6 # fields)))
            (pdata $ (pasByteStr # (pelemAt # 7 # fields)))
            (pdata $ (pasByteStr # (pelemAt # 8 # fields)))
            (pdata $ (pasByteStr # (pelemAt # 9 # fields)))
            (pdata $ (pasInt # (pelemAt # 10 # fields)))
            (pdata $ (pasInt # (pelemAt # 11 # fields)))
            (pdata $ (pdecodeDataSequenceSummary # (pelemAt # 12 # fields)))
            (pdata $ (pdecodeDataSequenceSummary # (pelemAt # 13 # fields)))
            (pdata $ (pdecodeDataSequenceSummary # (pelemAt # 14 # fields)))
            (pdata $ (pdecodeDataSequenceSummary # (pelemAt # 15 # fields)))
            (pdata $ (pasInt # (pelemAt # 16 # fields)))
            (pdata $ (pdecodeDataSequenceSummary # (pelemAt # 17 # fields)))
            (pdata $ (pasByteStr # (pelemAt # 18 # fields)))
            (pdata $ (pdecodeDataSummary # (pelemAt # 19 # fields)))
            (pdata $ (pasInt # (pelemAt # 20 # fields)))
            (pdata $ (pasByteStr # (pelemAt # 21 # fields)))
            (pdata $ (pdecodeDataSequenceSummary # (pelemAt # 22 # fields)))
            (pdata $ (pdecodeDataSequenceSummary # (pelemAt # 23 # fields)))
            (pdata $ (pdecodeDataSummary # (pelemAt # 24 # fields)))
