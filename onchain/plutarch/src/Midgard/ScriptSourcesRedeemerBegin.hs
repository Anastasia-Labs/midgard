-- | Stage one authenticates the first item, then delegates to the shared machine.
module Midgard.ScriptSourcesRedeemerBegin (pverifyBegin) where

import Midgard.FraudProofs.NativeTx.Compact qualified as Compact
import Midgard.FraudProofs.NativeTx.Types (PNativeTxWitnessSetCompact (..), PVerifiedMidgardNativeTxCompact (..))
import Midgard.NativeTxFieldAccess (PFieldCarriageV1)
import Midgard.RedeemerItemProof qualified as Item
import Midgard.ScriptSourcesRawFrame qualified as Frame
import Midgard.ScriptSourcesRedeemerNormalization (prawCommonControlIsInitial)
import Midgard.ValidationMachine (PValidationOneStepWitnessV1 (..))
import Midgard.ValidationMachineFieldDoor qualified as Door
import Midgard.ValidationTrace qualified as Trace
import Midgard.FraudProofs.NativeTx.Codec (pcborInt)
import Plutarch.Prelude

pverifyBegin :: forall s. Term s (Trace.PValidationMachineStateV1 :--> PValidationOneStepWitnessV1 :--> PFieldCarriageV1 :--> Door.PMachineFieldDoorV1 :--> PBool)
pverifyBegin = phoistAcyclic $ plam $ \pre witness carriage door ->
  plet (Frame.popenFrameV1 # pre # witness # 30 # 1) $ \frame -> pmatch frame $ \f ->
  pmatch pre $ \p -> pmatch witness $ \w ->
  let fields = Frame.pframe'items f; field i = pelemAt # i # fields
  in pif (prawCommonControlIsInitial (field 8) (field 10) (field 14) (field 17) (field 18) (field 20) (field 21) (field 24) (field 25)
    #&& plengthBS # (pasByteStr # field 29) #== 32)
    (pmatch (Compact.pverifyNativeTxProofSourceV1 # pfromData (Trace.pmachineState'transactionId p) # Frame.pframe'compactCbor f # Frame.pframe'witnessSetCompactCbor f # Frame.pframe'fieldPreimageLengthsCbor f) $ \(PPair verified witnessSet) ->
      pmatch verified $ \v -> pmatch witnessSet $ \ws ->
      plet (pasInt # field 12) $ \count -> plet (pasInt # field 26) $ \total ->
      plet (Door.popenMachineFieldItemByCommitment # door # pverified'txId v # pfromData (pwitnessSetCompact'redeemerTxWitsHash ws) # 8 # count # carriage) $ \opened ->
      plet (Door.pmachineFieldItemCount # opened) $ \itemCount -> plet (pif (total #== 0) itemCount total) $ \activeTotal ->
        pif (itemCount #== activeTotal #&& activeTotal #> 0 #&& activeTotal #<= 16384)
          (plet (Item.pinitialControlHash # Item.pmodeData # count # activeTotal # (Door.pmachineFieldItemLength # opened) # (Door.pmachineFieldItemCommitment # opened) # (-1) # (-1)) $ \pending ->
            plet (Frame.preplaceItemsV1 # frame # pfromData (poneStep'workWitnessCbor w) # 26 # 1 # pcborInt activeTotal) $ \nextTotal ->
              Frame.psuccessorIsExactV1 # pre # witness # (Frame.pappendExtensionV1 # nextTotal # pending)) perror) perror
