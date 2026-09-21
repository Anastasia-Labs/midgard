-- | The target's eight rewarding validators for ScriptSources middle stages.
module Midgard.Validators.FraudProofs.ValidationTrace.ScriptSourcesMiddleYields (validator, fieldValidator) where

import Midgard.ScriptSourcesMiddleSemantics qualified as Semantic
import Midgard.ScriptSourcesMiddleYield qualified as Yield
import Midgard.BoundedItem (PChunkProofV1 (..))
import Midgard.PhaseANativeItemYield (pdecodeCarriage)
import Midgard.ValidationMerkle (PFrontierPeak (..))
import Midgard.ValidationResolutionData (recordFields, integerField, bytesField)
import Midgard.ValidationMachineFieldDoor (PMachineFieldDoorV1 (..))
import Plutarch.LedgerApi.V3
import Plutarch.Prelude

validator :: forall s. Int -> Term s (PAsData (PBuiltinList (PAsData PScriptHash)) :--> PScriptContext :--> PUnit)
validator role = plam $ \hashes ctx -> run role (pfromData hashes) (pdata $ pcon $ PCurrencySymbol $ pconstant "") ctx

fieldValidator :: forall s. Int -> Term s (PAsData (PBuiltinList (PAsData PScriptHash)) :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
fieldValidator role = plam $ \hashes certificate ctx -> run role (pfromData hashes) certificate ctx

run :: forall s. Int -> Term s (PBuiltinList (PAsData PScriptHash)) -> Term s (PAsData PCurrencySymbol) -> Term s PScriptContext -> Term s PUnit
run role hashes certificate ctx = pmatch ctx $ \PScriptContext{pscriptContext'txInfo, pscriptContext'scriptInfo} ->
  pmatch pscriptContext'scriptInfo $ \case
    PRewardingScript _ -> pmatch pscriptContext'txInfo $ \tx ->
      let tag = [0, 7, 0, 29, 0, 1, 39, 0] !! role
          count = [0, 4, 0, 1, 0, 3, 2, 0] !! role
          valid = Yield.pdispatch hashes pscriptContext'txInfo (pconstant $ toInteger role) (pconstant tag) $ \pre witness fields ->
            pif (plength # fields #== pconstant count)
              (let at i = pelemAt # i # fields
                   checked decoded raw use = plet decoded $ \value -> pforgetData (pdata value) #== raw #&& use value
                   door = pcon $ PMachineFieldDoorV1 (pfromData $ ptxInfo'referenceInputs tx) certificate
               in case role of
                    0 -> Semantic.pstageTwoAdvance # pre # witness
                    1 -> Semantic.pstageThreeReplay # pre # witness # (pasInt # at 0) # (pasByteStr # at 1) # (pasByteStr # at 2) # (pasByteStr # at 3)
                    2 -> Semantic.pstageThreeFinish # pre # witness
                    3 -> checked (pdecodeCarriage $ at 0) (at 0) $ \carriage -> Semantic.pstageFourBegin # pre # witness # door # carriage
                    4 -> Semantic.pstageFourFinish # pre # witness
                    5 -> checked (pdecodeCarriage $ at 2) (at 2) $ \carriage -> Semantic.pstageSixBegin # pre # witness # door # (pasInt # at 0) # (pasInt # at 1) # carriage
                    6 -> checked (decodeChunk $ at 0) (at 0) $ \chunk ->
                      checked (decodeNext $ at 1) (at 1) $ \next -> Semantic.pstageSixAsset # pre # witness # chunk # next
                    7 -> Semantic.pstageSixFinish # pre # witness
                    _ -> error "Unmapped middle yield") perror
       in pif valid (pconstant ()) perror
    _ -> perror

-- Explicit reconstruction forces the same recursively checked Data shape as
-- the target's `expect proof: ChunkProofV1`, without extra domain restrictions.
decodeChunk :: forall s. Term s PData -> Term s PChunkProofV1
decodeChunk raw = plet (recordFields 8 raw) $ \f ->
  pcon $ PChunkProofV1 (integerField f 0) (integerField f 1) (integerField f 2) (integerField f 3) (integerField f 4) (bytesField f 5)
    (pdata $ pmap # plam (\peak -> plet (recordFields 2 peak) $ \p -> pdata $ pcon $ PFrontierPeak (integerField p 0) (bytesField p 1)) # (pasList # (pelemAt # 6 # f)))
    (pdata $ pmap # plam (\hash -> pdata $ pasByteStr # hash) # (pasList # (pelemAt # 7 # f)))

decodeNext :: forall s. Term s PData -> Term s (PMaybeData PChunkProofV1)
decodeNext raw = pmatch (pasConstr # raw) $ \(PBuiltinPair tag fields) ->
  pif (tag #== 0 #&& plength # fields #== 1) (pcon $ PDJust $ pdata $ decodeChunk $ phead # fields)
    (pif (tag #== 1 #&& pnull # fields) (pcon PDNothing) perror)
