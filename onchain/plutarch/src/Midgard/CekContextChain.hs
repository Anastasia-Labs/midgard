-- | Physical CEK context carriers. Opaque fields stay opaque until their owner.
module Midgard.CekContextChain where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Midgard.CekContextStep qualified as Step
import Midgard.CekContextWire qualified as Wire
import Midgard.ValidationMachine qualified as VM
import Midgard.ValidationMachine.CekSemantics qualified as Cek
import Midgard.ValidationResolutionData (bytesField, decodeTransition, integerField, recordFields)
import Plutarch.Builtin.Crypto (pblake2b_256)
import Plutarch.Prelude

data PBound s = PBound
    { pbound'prepared :: Term s PData
    , pbound'control :: Term s PData
    , pbound'auxiliaryHash :: Term s (PAsData PByteString)
    , pbound'transactionId :: Term s (PAsData PByteString)
    }
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PBound)

data PStaged s = PStaged
    { pstaged'bound :: Term s (PAsData PBound)
    , pstaged'context :: Term s (PAsData VM.PCekContextControlV1)
    , pstaged'native :: Term s (PAsData Step.PNativeFacts)
    , pstaged'cursor :: Term s (PAsData PInteger)
    }
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PStaged)

data PSuccessor s
    = PContinueContext (Term s (PAsData VM.PCekContextControlV1))
    | PStartExecution (Term s (PAsData PByteString)) (Term s (PAsData PInteger)) (Term s (PAsData PInteger))
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PSuccessor)

data PVerified s = PVerified
    { pverified'staged :: Term s PData
    , pverified'successor :: Term s (PAsData PSuccessor)
    }
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PVerified)

data PAction s = PAction
    { paction'inputIndex :: Term s (PAsData PInteger)
    , paction'outputIndex :: Term s (PAsData PInteger)
    , paction'transition :: Term s (PAsData VM.PValidationOneStepWitnessV1)
    , paction'auxiliary :: Term s PData
    }
    deriving stock (Generic)
    deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
    deriving (PlutusType) via (DeriveAsDataStruct PAction)

-- Aiken serializes its witness-control record, while the machine's Plutarch
-- representation is Scott encoded. Build the nine-field Data record explicitly.
pcontrolData :: forall s. Term s (Cek.PCekWitnessControlV1 :--> PData)
pcontrolData = phoistAcyclic $ plam $ \control -> pmatch control $ \(Cek.PCekWitnessControlV1 native context cursor cpu memory active envelope cpuLimit memoryLimit) ->
    pforgetData $
        pconstrBuiltin
            # 0
            # ( pcons
                    # pforgetData (pdata native)
                    # ( pcons
                            # pforgetData (pdata context)
                            # ( pcons
                                    # pforgetData (pdata cursor)
                                    # ( pcons
                                            # pforgetData (pdata cpu)
                                            # ( pcons
                                                    # pforgetData (pdata memory)
                                                    # ( pcons
                                                            # pforgetData (pdata active)
                                                            # (pcons # pforgetData (pdata envelope) # (pcons # pforgetData (pdata cpuLimit) # (pcons # pforgetData (pdata memoryLimit) # pnil)))
                                                      )
                                              )
                                      )
                              )
                      )
              )

pdecodeControl :: forall s. Term s (PData :--> Cek.PCekWitnessControlV1)
pdecodeControl = phoistAcyclic $ plam $ \raw -> plet (recordFields 9 raw) $ \f ->
    pcon $
        Cek.PCekWitnessControlV1
            (Wire.pdecodeNativeScriptsControl # (phead # f))
            (pfromData $ bytesField f 1)
            (pfromData $ integerField f 2)
            (pfromData $ integerField f 3)
            (pfromData $ integerField f 4)
            (pfromData $ bytesField f 5)
            (pfromData $ bytesField f 6)
            (pfromData $ integerField f 7)
            (pfromData $ integerField f 8)

pdecodeAction :: forall s. Term s (PData :--> PAction)
pdecodeAction = phoistAcyclic $ plam $ \raw -> plet (recordFields 4 raw) $ \f ->
    pcon $ PAction (integerField f 0) (integerField f 1) (pdata $ decodeTransition $ pelemAt # 2 # f) (pelemAt # 3 # f)

pdecodeBound :: forall s. Term s (PData :--> PBound)
pdecodeBound = phoistAcyclic $ plam $ \raw -> plet (recordFields 4 raw) $ \f ->
    pcon $ PBound (pelemAt # 0 # f) (pelemAt # 1 # f) (bytesField f 2) (bytesField f 3)

pdecodeStaged :: forall s. Term s (PData :--> PStaged)
pdecodeStaged = phoistAcyclic $ plam $ \raw -> plet (recordFields 4 raw) $ \f ->
    pcon $
        PStaged
            (pdata $ pdecodeBound # (pelemAt # 0 # f))
            (pdata $ Wire.pdecodeCekContextControl # (pelemAt # 1 # f))
            (pdata $ pdecodeNative # (pelemAt # 2 # f))
            (integerField f 3)

pdecodeNative :: forall s. Term s (PData :--> Step.PNativeFacts)
pdecodeNative = phoistAcyclic $ plam $ \raw -> plet (recordFields 16 raw) $ \f ->
    let peaks i = pdata $ pmap # plam (\x -> pdata $ Wire.pdecodeFrontierPeak # x) # (pasList # (pelemAt # i # f))
     in pcon $
            Step.PNativeFacts
                (bytesField f 0)
                (bytesField f 1)
                (bytesField f 2)
                (integerField f 3)
                (integerField f 4)
                (peaks 5)
                (integerField f 6)
                (bytesField f 7)
                (integerField f 8)
                (peaks 9)
                (integerField f 10)
                (peaks 11)
                (integerField f 12)
                (peaks 13)
                (integerField f 14)
                (peaks 15)

pdecodeSuccessor :: forall s. Term s (PData :--> PSuccessor)
pdecodeSuccessor = phoistAcyclic $ plam $ \raw -> pmatch (pasConstr # raw) $ \(PBuiltinPair tag f) ->
    pif (tag #== 0 #&& plength # f #== 1) (pcon $ PContinueContext $ pdata $ Wire.pdecodeCekContextControl # (phead # f)) $
        pif (tag #== 1 #&& plength # f #== 3) (pcon $ PStartExecution (bytesField f 0) (integerField f 1) (integerField f 2)) perror

pdecodeVerified :: forall s. Term s (PData :--> PVerified)
pdecodeVerified = phoistAcyclic $ plam $ \raw -> plet (recordFields 2 raw) $ \f ->
    pcon $ PVerified (pelemAt # 0 # f) (pdata $ pdecodeSuccessor # (pelemAt # 1 # f))

pauxiliaryIsBound :: forall s. Term s (PBound :--> PData :--> PBool)
pauxiliaryIsBound = phoistAcyclic $ plam $ \bound raw -> pmatch bound $ \b -> pblake2b_256 # (pserialiseData # raw) #== pfromData (pbound'auxiliaryHash b)
