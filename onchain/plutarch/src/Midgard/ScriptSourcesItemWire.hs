-- | Checked witness records for the shared redeemer-item executor stages.
module Midgard.ScriptSourcesItemWire where

import Midgard.BoundedItem qualified as Bounded
import Midgard.CekContextItemWire (poptional)
import Midgard.CekContextWire qualified as Wire
import Midgard.CekDataFrame qualified as Frame
import Midgard.CekDataTraverse qualified as Traverse
import Midgard.CekSelection (bytesList)
import Midgard.RedeemerItemProof qualified as Item
import Midgard.ValidationResolutionData (bytesField, integerField, recordFields)
import Plutarch.Prelude

pdecodeFrame :: forall s. Term s (PData :--> Frame.PDataFrameV1)
pdecodeFrame = phoistAcyclic $ plam $ \raw -> plet (recordFields 11 raw) $ \f ->
  pcon $ Frame.PDataFrameV1 (integerField f 0) (integerField f 1) (bytesField f 2) (integerField f 3) (integerField f 4) (bytesField f 5) (integerField f 6) (integerField f 7)
    (pdata $ pmap # plam (\peak -> pdata $ Wire.pdecodeFrontierPeak # peak) # (pasList # (pelemAt # 8 # f)))
    (integerField f 9) (pdata $ Wire.pdecodeDataSequenceSummary # (pelemAt # 10 # f))

pdecodeChunk :: forall s. Term s (PData :--> Bounded.PChunkProofV1)
pdecodeChunk = phoistAcyclic $ plam $ \raw -> plet (recordFields 8 raw) $ \f ->
  pcon $ Bounded.PChunkProofV1 (integerField f 0) (integerField f 1) (integerField f 2) (integerField f 3) (integerField f 4) (bytesField f 5)
    (pdata $ pmap # plam (\peak -> pdata $ Wire.pdecodeFrontierPeak # peak) # (pasList # (pelemAt # 6 # f))) (bytesList $ pelemAt # 7 # f)

pdecodeTraversalAction :: forall s. Term s (PData :--> Traverse.PDataTraverseActionV1)
pdecodeTraversalAction = phoistAcyclic $ plam $ \raw -> pmatch (pasConstr # raw) $ \(PBuiltinPair tag f) ->
  let frame i = pdata $ pdecodeFrame # (pelemAt # i # f)
      summary i = pdata $ Wire.pdecodeDataSummary # (pelemAt # i # f)
      optional i = pdata $ poptional (\x -> pdecodeFrame # x) (pelemAt # i # f)
      bytes i = bytesList $ pelemAt # i # f
      at t n next otherwise = pif (tag #== t) (pif (plength # f #== n) next perror) otherwise
  in at 0 0 (pcon Traverse.PNoAction) $
    at 1 1 (pcon $ Traverse.PHeadScalar $ integerField f 0) $
    at 2 1 (pcon $ Traverse.PHeadSequence $ integerField f 0) $
    at 3 0 (pcon Traverse.PHeadMap) $
    at 4 2 (pcon $ Traverse.PHeadLargeConstructor (integerField f 0) (integerField f 1)) $
    at 5 1 (pcon $ Traverse.PAttachScalar $ optional 0) $
    at 6 4 (pcon $ Traverse.PFoldList (frame 0) (integerField f 1) (summary 2) (bytes 3)) $
    at 7 6 (pcon $ Traverse.PFoldMap (frame 0) (integerField f 1) (summary 2) (summary 3) (bytes 4) (bytes 5)) $
    at 8 2 (pcon $ Traverse.PFinalizeFrame (frame 0) (optional 1)) perror

pdecodeItemAction :: forall s. Term s (PData :--> Item.PRedeemerItemProofActionV1)
pdecodeItemAction = phoistAcyclic $ plam $ \raw -> pmatch (pasConstr # raw) $ \(PBuiltinPair tag f) ->
  pif (tag #== 0 #&& pnull # f) (pcon Item.PRedeemerItemOpenHeader) $
  pif (tag #== 1 #&& pnull # f) (pcon Item.PRedeemerItemOpenTail) $
  pif (tag #== 2 #&& plength # f #== 1) (pcon $ Item.PRedeemerItemTraverseData $ pdata $ pdecodeTraversalAction # (phead # f)) $
  pif (tag #== 3 #&& pnull # f) (pcon Item.PRedeemerItemFinishData) perror

pdecodeWitness :: forall s. Term s (PData :--> Item.PRedeemerItemProofWitnessV1)
pdecodeWitness = phoistAcyclic $ plam $ \raw -> plet (recordFields 3 raw) $ \f ->
  pcon $ Item.PRedeemerItemProofWitnessV1 (pdata $ pdecodeItemAction # (phead # f))
    (pdata $ poptional (\x -> pdecodeChunk # x) (pelemAt # 1 # f))
    (pdata $ poptional (\x -> pdecodeChunk # x) (pelemAt # 2 # f))
