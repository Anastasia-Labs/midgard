-- | Target raw-frame entry and completion of output-proof traversal.
module Midgard.ScriptSourcesOutputSemantics (pbegin, pfinish) where

import Midgard.LedgerOutputProof qualified as Proof
import Midgard.ScriptProof qualified as Script
import Midgard.ScriptSourcesRawFrame qualified as Raw
import Midgard.ValidationMachine (PValidationOneStepWitnessV1 (..))
import Midgard.ValidationMerkle (pverifyMembership)
import Midgard.ValidationTrace (PValidationMachineStateV1)
import Plutarch.Core.Utils (pand'List)
import Plutarch.Prelude

pbegin :: forall s. Term s (PValidationMachineStateV1 :--> PValidationOneStepWitnessV1 :--> PInteger :--> PInteger :--> PByteString :--> PBuiltinList (PAsData PByteString) :--> PBool)
pbegin = phoistAcyclic $ plam $ \pre witness index length commitment siblings -> pmatch witness $ \w ->
  plet (Raw.popenFrameV1 # pre # witness # 30 # 5) $ \frame ->
  plet (Raw.pitemIntV1 # frame # 21) $ \count ->
    pif
      (pand'List
      [ index #== Raw.pitemIntV1 # frame # 20
      , index #< count
      , length #> 0
      , plengthBS # commitment #== 32
      , pverifyMembership # count # (Raw.pitemFrontierV1 # frame # 22) # index
          # (Script.poutputItemLeafHash # index # commitment) # siblings
      ])
      (Raw.psuccessorIsExactV1 # pre # witness
          # (Raw.pappendExtensionV1 # pfromData (poneStep'workWitnessCbor w)
              # (Proof.pencodeInitialControlV1 # index # length # commitment)))
      (pconstant False)

pfinish :: forall s. Term s (PValidationMachineStateV1 :--> PValidationOneStepWitnessV1 :--> PBool)
pfinish = phoistAcyclic $ plam $ \pre witness -> pmatch witness $ \w ->
  plet (Raw.popenFrameV1 # pre # witness # 30 # 5) $ \frame ->
    Raw.pitemIntV1 # frame # 20 #== Raw.pitemIntV1 # frame # 21
      #&& Raw.psuccessorIsExactV1 # pre # witness
        # (Raw.preplaceStageV1 # frame # pfromData (poneStep'workWitnessCbor w) # 6)
