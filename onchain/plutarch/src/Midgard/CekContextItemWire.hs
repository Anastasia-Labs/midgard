-- | Checked Data openings for the shared CEK redeemer-item machine.
module Midgard.CekContextItemWire where

import Midgard.Blake2b256Trace qualified as M0
import Midgard.CekBlobFrontier qualified as M1
import Midgard.CekContextWire qualified as Wire
import Midgard.CekDataBytes qualified as M4
import Midgard.CekDataInteger qualified as M3
import Midgard.CekDataTraverse qualified as M5
import Midgard.CekSourceBlob qualified as M2
import Midgard.RedeemerItemProof qualified as M6
import Midgard.ValidationResolutionData (recordFields)
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.Prelude

poptional :: forall s a. (PIsData a) => (Term s PData -> Term s a) -> Term s PData -> Term s (PMaybeData a)
poptional decode raw = pmatch (pasConstr # raw) $ \(PBuiltinPair tag f) ->
    pif (tag #== 1 #&& pnull # f) (pcon PDNothing) $
        pif (tag #== 0 #&& plength # f #== 1) (pcon $ PDJust $ pdata $ decode $ phead # f) perror

pdecodeBlake2b256TraceControl :: forall s. Term s (PData :--> M0.PBlake2b256TraceControlV1)
pdecodeBlake2b256TraceControl = phoistAcyclic $ plam $ \raw -> plet (recordFields 9 raw) $ \f ->
    pcon $
        M0.PBlake2b256TraceControlV1
            (pdata $ (pasInt # (pelemAt # 0 # f)))
            (pdata $ (pasInt # (pelemAt # 1 # f)))
            (pdata $ (pasInt # (pelemAt # 2 # f)))
            (pdata $ (pasInt # (pelemAt # 3 # f)))
            (pdata $ (pasByteStr # (pelemAt # 4 # f)))
            (pdata $ (pasByteStr # (pelemAt # 5 # f)))
            (pdata $ (pasInt # (pelemAt # 6 # f)))
            (pdata $ (pasByteStr # (pelemAt # 7 # f)))
            (pdata $ (pasInt # (pelemAt # 8 # f)))

pdecodeCekBlobFrontierPeak :: forall s. Term s (PData :--> M1.PCekBlobFrontierPeakV1)
pdecodeCekBlobFrontierPeak = phoistAcyclic $ plam $ \raw -> plet (recordFields 3 raw) $ \f ->
    pcon $
        M1.PCekBlobFrontierPeakV1
            (pdata $ (pasInt # (pelemAt # 0 # f)))
            (pdata $ (pasByteStr # (pelemAt # 1 # f)))
            (pdata $ (pasInt # (pelemAt # 2 # f)))

pdecodeCekBlobFrontier :: forall s. Term s (PData :--> M1.PCekBlobFrontierV1)
pdecodeCekBlobFrontier = phoistAcyclic $ plam $ \raw -> plet (recordFields 3 raw) $ \f ->
    pcon $
        M1.PCekBlobFrontierV1
            (pdata $ (pasInt # (pelemAt # 0 # f)))
            (pdata $ (pasInt # (pelemAt # 1 # f)))
            (pdata $ (pmap # plam (\value -> (pdata $ (pdecodeCekBlobFrontierPeak # value))) # (pasList # (pelemAt # 2 # f))))

pdecodeCekSourceBlobControl :: forall s. Term s (PData :--> M2.PCekSourceBlobControlV1)
pdecodeCekSourceBlobControl = phoistAcyclic $ plam $ \raw -> plet (recordFields 6 raw) $ \f ->
    pcon $
        M2.PCekSourceBlobControlV1
            (pdata $ (pasInt # (pelemAt # 0 # f)))
            (pdata $ (pasInt # (pelemAt # 1 # f)))
            (pdata $ (pasInt # (pelemAt # 2 # f)))
            (pdata $ (pasInt # (pelemAt # 3 # f)))
            (pdata $ (pdecodeCekBlobFrontier # (pelemAt # 4 # f)))
            (pdata $ (poptional (\value -> (pdecodeBlake2b256TraceControl # value)) (pelemAt # 5 # f)))

pdecodeCekDataIntegerControl :: forall s. Term s (PData :--> M3.PCekDataIntegerControlV1)
pdecodeCekDataIntegerControl = phoistAcyclic $ plam $ \raw -> plet (recordFields 6 raw) $ \f ->
    pcon $
        M3.PCekDataIntegerControlV1
            (pdata $ (pasInt # (pelemAt # 0 # f)))
            (pdata $ (pasInt # (pelemAt # 1 # f)))
            (pdata $ (pasInt # (pelemAt # 2 # f)))
            (pdata $ (pasInt # (pelemAt # 3 # f)))
            (pdata $ (pasInt # (pelemAt # 4 # f)))
            (pdata $ (poptional (\value -> (pdecodeCekSourceBlobControl # value)) (pelemAt # 5 # f)))

pdecodeCekDataBytesControl :: forall s. Term s (PData :--> M4.PCekDataBytesControlV1)
pdecodeCekDataBytesControl = phoistAcyclic $ plam $ \raw -> plet (recordFields 6 raw) $ \f ->
    pcon $
        M4.PCekDataBytesControlV1
            (pdata $ (pasInt # (pelemAt # 0 # f)))
            (pdata $ (pasInt # (pelemAt # 1 # f)))
            (pdata $ (pasInt # (pelemAt # 2 # f)))
            (pdata $ (pasInt # (pelemAt # 3 # f)))
            (pdata $ (pasInt # (pelemAt # 4 # f)))
            (pdata $ (poptional (\value -> (pdecodeCekSourceBlobControl # value)) (pelemAt # 5 # f)))

pdecodeDataTraverseControl :: forall s. Term s (PData :--> M5.PDataTraverseControlV1)
pdecodeDataTraverseControl = phoistAcyclic $ plam $ \raw -> plet (recordFields 10 raw) $ \f ->
    pcon $
        M5.PDataTraverseControlV1
            (pdata $ (pasInt # (pelemAt # 0 # f)))
            (pdata $ (pasInt # (pelemAt # 1 # f)))
            (pdata $ (pasInt # (pelemAt # 2 # f)))
            (pdata $ (pasInt # (pelemAt # 3 # f)))
            (pdata $ (pasInt # (pelemAt # 4 # f)))
            (pdata $ (pasByteStr # (pelemAt # 5 # f)))
            (pdata $ (poptional (\value -> (pasInt # value)) (pelemAt # 6 # f)))
            (pdata $ (poptional (\value -> (pdecodeCekDataIntegerControl # value)) (pelemAt # 7 # f)))
            (pdata $ (poptional (\value -> (pdecodeCekDataBytesControl # value)) (pelemAt # 8 # f)))
            (pdata $ (poptional (\value -> (Wire.pdecodeDataSummary # value)) (pelemAt # 9 # f)))

pdecodeRedeemerItemProofControl :: forall s. Term s (PData :--> M6.PRedeemerItemProofControlV1)
pdecodeRedeemerItemProofControl = phoistAcyclic $ plam $ \raw -> plet (recordFields 16 raw) $ \f ->
    pcon $
        M6.PRedeemerItemProofControlV1
            (pdata $ (pasInt # (pelemAt # 0 # f)))
            (pdata $ (pasInt # (pelemAt # 1 # f)))
            (pdata $ (pasInt # (pelemAt # 2 # f)))
            (pdata $ (pasInt # (pelemAt # 3 # f)))
            (pdata $ (pasInt # (pelemAt # 4 # f)))
            (pdata $ (pasInt # (pelemAt # 5 # f)))
            (pdata $ (pasByteStr # (pelemAt # 6 # f)))
            (pdata $ (pasInt # (pelemAt # 7 # f)))
            (pdata $ (pasInt # (pelemAt # 8 # f)))
            (pdata $ (pasInt # (pelemAt # 9 # f)))
            (pdata $ (pasInt # (pelemAt # 10 # f)))
            (pdata $ (pasInt # (pelemAt # 11 # f)))
            (pdata $ (pasInt # (pelemAt # 12 # f)))
            (pdata $ (pasInt # (pelemAt # 13 # f)))
            (pdata $ (pasInt # (pelemAt # 14 # f)))
            (pdata $ (poptional (\value -> (pdecodeDataTraverseControl # value)) (pelemAt # 15 # f)))
