-- | Checked Data openings for late ScriptSources descriptor claims.
module Midgard.ScriptSourcesDescriptorWire (pdecodeClaim) where

import Midgard.CekContextItemWire (poptional)
import Midgard.ScriptSourcesDescriptor (PDescriptorControl (..), PDescriptorStepClaim (..))
import Midgard.ScriptSourcesItemWire (pdecodeChunk)
import Midgard.ValidationResolutionData (bytesField, integerField, recordFields)
import Plutarch.Prelude

pdecodeBool :: forall s. Term s PData -> Term s (PAsData PBool)
pdecodeBool raw = pmatch (pasConstr # raw) $ \(PBuiltinPair tag fields) ->
  pif
    (pnull # fields)
    ( pif
        (tag #== 0)
        (pdata $ pconstant False)
        (pif (tag #== 1) (pdata $ pconstant True) perror)
    )
    perror

pdecodeControl :: forall s. Term s PData -> Term s PDescriptorControl
pdecodeControl raw = plet (recordFields 15 raw) $ \fields ->
  pcon $
    PDescriptorControl
      (integerField fields 0)
      (integerField fields 1)
      (integerField fields 2)
      (integerField fields 3)
      (integerField fields 4)
      (integerField fields 5)
      (bytesField fields 6)
      (integerField fields 7)
      (integerField fields 8)
      (integerField fields 9)
      (integerField fields 10)
      (integerField fields 11)
      (integerField fields 12)
      (integerField fields 13)
      (integerField fields 14)

pdecodeClaim :: forall s. Term s (PData :--> PDescriptorStepClaim)
pdecodeClaim = phoistAcyclic $ plam $ \raw -> plet (recordFields 5 raw) $ \fields ->
  pcon $
    PDescriptorStepClaim
      (pdata $ pdecodeControl $ pelemAt # 0 # fields)
      (pdecodeBool $ pelemAt # 1 # fields)
      (pdata $ pdecodeChunk # (pelemAt # 2 # fields))
      (pdata $ poptional (\value -> pdecodeChunk # value) (pelemAt # 3 # fields))
      (pdata $ pdecodeControl $ pelemAt # 4 # fields)
