-- | Checked wire openings at validation-resolution trust boundaries.
module Midgard.ValidationResolutionData (recordFields, integerField, bytesField, enumField, decodeState, decodePrepared, decodeTransition) where

import Midgard.ValidationMachine (PValidationOneStepWitnessV1 (..))
import Midgard.ValidationResolution qualified as Resolution
import Midgard.ValidationTrace (PValidationMachineStateV1 (..))
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

-- Checked openings shared by CEK entry, settlement, and selection yields.
recordFields :: forall s. Term s PInteger -> Term s PData -> Term s (PBuiltinList PData)
recordFields count raw = pmatch (pasConstr # raw) $ \(PBuiltinPair tag fields) -> pif (tag #== 0 #&& plength # fields #== count) fields perror
integerField :: forall s. Term s (PBuiltinList PData) -> Term s PInteger -> Term s (PAsData PInteger)
integerField f i = pdata $ pasInt # (pelemAt # i # f)
bytesField :: forall s. Term s (PBuiltinList PData) -> Term s PInteger -> Term s (PAsData PByteString)
bytesField f i = pdata $ pasByteStr # (pelemAt # i # f)
enumField :: forall a s. Term s PInteger -> Term s PData -> Term s (PAsData a)
enumField count raw = pmatch (pasConstr # raw) $ \(PBuiltinPair tag fields) ->
    pif (tag #>= 0 #&& tag #< count #&& pnull # fields) (punsafeCoerce raw) perror

decodeState :: forall s. Term s PData -> Term s PValidationMachineStateV1
decodeState raw = plet (recordFields 15 raw) $ \f ->
    pcon $
        PValidationMachineStateV1
            (integerField f 0)
            (bytesField f 1)
            (bytesField f 2)
            (bytesField f 3)
            (bytesField f 4)
            (enumField 2 $ pelemAt # 5 # f)
            (bytesField f 6)
            (enumField 15 $ pelemAt # 7 # f)
            (integerField f 8)
            (bytesField f 9)
            (integerField f 10)
            (integerField f 11)
            (enumField 3 $ pelemAt # 12 # f)
            (bytesField f 13)
            (bytesField f 14)

decodePrepared :: forall s. Term s PData -> Term s Resolution.PPreparedValidationResolutionStateV1
decodePrepared raw = plet (recordFields 3 raw) $ \f ->
    plet (recordFields 4 $ pelemAt # 1 # f) $ \r ->
        pcon $
            Resolution.PPreparedValidationResolutionStateV1
                (integerField f 0)
                (pdata $ pcon $ Resolution.PValidationResolutionStateV1 (integerField r 0) (pdata $ decodeState $ pelemAt # 1 # r) (bytesField r 2) (bytesField r 3))
                (bytesField f 2)

decodeTransition :: forall s. Term s PData -> Term s PValidationOneStepWitnessV1
decodeTransition raw = plet (recordFields 2 raw) $ \f -> pcon $ PValidationOneStepWitnessV1 (bytesField f 0) (pdata $ decodeState $ pelemAt # 1 # f)
