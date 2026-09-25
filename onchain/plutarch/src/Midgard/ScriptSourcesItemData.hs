-- | Checked Data openings at shared item pipeline trust boundaries.
module Midgard.ScriptSourcesItemData where

import Midgard.CekContextItemWire qualified as ItemWire
import Midgard.ScriptSourcesItemWire qualified as ActionWire
import Midgard.ScriptSourcesItemNormalization qualified as Normalized
import Midgard.ScriptSourcesRedeemerNormalization qualified as Envelope
import Midgard.ValidationResolutionData (recordFields)
import Plutarch.Prelude

pdecodeEnvelope :: forall s. Term s (PData :--> Envelope.PPreparedScriptSourcesRedeemerEnvelopeV1)
pdecodeEnvelope = phoistAcyclic $ plam $ \raw -> plet (recordFields 20 raw) $ \f ->
  pcon $ Envelope.PPreparedScriptSourcesRedeemerEnvelopeV1
    (pdata $ pasInt # (pelemAt # 0 # f))
    (pdata $ pasByteStr # (pelemAt # 1 # f))
    (pdata $ pasByteStr # (pelemAt # 2 # f))
    (pdata $ pasInt # (pelemAt # 3 # f))
    (pelemAt # 4 # f)
    (pdata $ pasByteStr # (pelemAt # 5 # f))
    (pdata $ pasByteStr # (pelemAt # 6 # f))
    (pdata $ pasInt # (pelemAt # 7 # f))
    (pdata $ pasByteStr # (pelemAt # 8 # f))
    (pdata $ pasByteStr # (pelemAt # 9 # f))
    (pdata $ pasByteStr # (pelemAt # 10 # f))
    (pdata $ pasByteStr # (pelemAt # 11 # f))
    (pdata $ pasInt # (pelemAt # 12 # f))
    (pdata $ pasInt # (pelemAt # 13 # f))
    (pdata $ pasByteStr # (pelemAt # 14 # f))
    (pdata $ pasByteStr # (pelemAt # 15 # f))
    (pdata $ pasByteStr # (pelemAt # 16 # f))
    (pdata $ pasByteStr # (pelemAt # 17 # f))
    (pdata $ pasByteStr # (pelemAt # 18 # f))
    (pdata $ pasByteStr # (pelemAt # 19 # f))

pdecodeAttestation :: forall s. Term s (PData :--> Envelope.PScriptSourcesRedeemerExecutionAttestedStateV1)
pdecodeAttestation = phoistAcyclic $ plam $ \raw -> plet (recordFields 17 raw) $ \f ->
  pcon $ Envelope.PScriptSourcesRedeemerExecutionAttestedStateV1
    (pdata $ pasInt # (pelemAt # 0 # f))
    (pdata $ pasByteStr # (pelemAt # 1 # f))
    (pdata $ pasByteStr # (pelemAt # 2 # f))
    (pdata $ pasByteStr # (pelemAt # 3 # f))
    (pdata $ pasByteStr # (pelemAt # 4 # f))
    (pdata $ pasByteStr # (pelemAt # 5 # f))
    (pdata $ pasByteStr # (pelemAt # 6 # f))
    (pdata $ pasByteStr # (pelemAt # 7 # f))
    (pdata $ pasByteStr # (pelemAt # 8 # f))
    (pdata $ pasInt # (pelemAt # 9 # f))
    (pdata $ pasByteStr # (pelemAt # 10 # f))
    (pdata $ pasByteStr # (pelemAt # 11 # f))
    (pdata $ pasByteStr # (pelemAt # 12 # f))
    (pdata $ pasByteStr # (pelemAt # 13 # f))
    (pdata $ pasByteStr # (pelemAt # 14 # f))
    (pdata $ pasInt # (pelemAt # 15 # f))
    (pdata $ pasInt # (pelemAt # 16 # f))

pdecodeTraversalChecked :: forall s. Term s (PData :--> Normalized.PTraversalChecked)
pdecodeTraversalChecked = phoistAcyclic $ plam $ \raw -> plet (recordFields 6 raw) $ \f ->
  pcon $ Normalized.PTraversalChecked
    (pdata $ pdecodeBool # (pelemAt # 0 # f))
    (pdata $ pdecodeEnvelope # (pelemAt # 1 # f))
    (pdata $ ItemWire.pdecodeRedeemerItemProofControl # (pelemAt # 2 # f))
    (pelemAt # 3 # f)
    (pdata $ pasByteStr # (pelemAt # 4 # f))
    (pdata $ pasByteStr # (pelemAt # 5 # f))

pdecodeCurrentChecked :: forall s. Term s (PData :--> Normalized.PCurrentChecked)
pdecodeCurrentChecked = phoistAcyclic $ plam $ \raw -> plet (recordFields 4 raw) $ \f ->
  pcon $ Normalized.PCurrentChecked
    (pdata $ pdecodeEnvelope # (pelemAt # 0 # f))
    (pdata $ ItemWire.pdecodeRedeemerItemProofControl # (pelemAt # 1 # f))
    (pelemAt # 2 # f)
    (pdata $ pasByteStr # (pelemAt # 3 # f))

pdecodeControlsChecked :: forall s. Term s (PData :--> Normalized.PControlsChecked)
pdecodeControlsChecked = phoistAcyclic $ plam $ \raw -> plet (recordFields 4 raw) $ \f ->
  pcon $ Normalized.PControlsChecked
    (pdata $ pdecodeEnvelope # (pelemAt # 0 # f))
    (pdata $ ItemWire.pdecodeRedeemerItemProofControl # (pelemAt # 1 # f))
    (pdata $ ItemWire.pdecodeRedeemerItemProofControl # (pelemAt # 2 # f))
    (pdata $ pasByteStr # (pelemAt # 3 # f))

pdecodeExecutionOutput :: forall s. Term s (PData :--> Normalized.PExecutionOutput)
pdecodeExecutionOutput = phoistAcyclic $ plam $ \raw -> plet (recordFields 5 raw) $ \f ->
  pcon $ Normalized.PExecutionOutput
    (pdata $ pasByteStr # (pelemAt # 0 # f))
    (pdata $ pasInt # (pelemAt # 1 # f))
    (pdata $ pasByteStr # (pelemAt # 2 # f))
    (pdata $ pasByteStr # (pelemAt # 3 # f))
    (pelemAt # 4 # f)

pdecodeTraversalExecution :: forall s. Term s (PData :--> Normalized.PTraversalExecution)
pdecodeTraversalExecution = phoistAcyclic $ plam $ \raw -> plet (recordFields 5 raw) $ \f ->
  pcon $ Normalized.PTraversalExecution
    (pdata $ pdecodeExecutionOutput # (pelemAt # 0 # f))
    (pdata $ ItemWire.pdecodeDataTraverseControl # (pelemAt # 1 # f))
    (pdata $ ItemWire.pdecodeDataTraverseControl # (pelemAt # 2 # f))
    (pdata $ ActionWire.pdecodeTraversalAction # (pelemAt # 3 # f))
    (pdata $ ItemWire.poptional (\value -> pasByteStr # value) (pelemAt # 4 # f))

pdecodeOuterExecution :: forall s. Term s (PData :--> Normalized.POuterExecution)
pdecodeOuterExecution = phoistAcyclic $ plam $ \raw -> plet (recordFields 5 raw) $ \f ->
  pcon $ Normalized.POuterExecution
    (pdata $ pdecodeExecutionOutput # (pelemAt # 0 # f))
    (pdata $ ItemWire.pdecodeRedeemerItemProofControl # (pelemAt # 1 # f))
    (pdata $ ItemWire.pdecodeRedeemerItemProofControl # (pelemAt # 2 # f))
    (pdata $ ActionWire.pdecodeItemAction # (pelemAt # 3 # f))
    (pdata $ ItemWire.poptional (\value -> pasByteStr # value) (pelemAt # 4 # f))

pdecodeBool :: forall s. Term s (PData :--> PBool)
pdecodeBool = phoistAcyclic $ plam $ \raw -> pmatch (pasConstr # raw) $ \(PBuiltinPair tag fields) ->
  pif (pnull # fields #&& (tag #== 0 #|| tag #== 1)) (tag #== 1) perror
