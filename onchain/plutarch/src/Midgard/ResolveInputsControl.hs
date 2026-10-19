{-# LANGUAGE OverloadedStrings #-}

-- | Resolve-input carriers retain the nested output proof as authenticated CBOR.
module Midgard.ResolveInputsControl (
    PControl (..),
    PPending (..),
    pcontrolRawFromWitness,
    pencodeControlRaw,
    pcontrolNoPendingFromWitness,
    pcontrolNoPendingIsBound,
    pencodePendingRaw,
    ppendingRawFromCbor,
    pencodePending,
    pcontrolRawIsBound,
    pcontrolRawIsBoundWithDescriptor,
    psplicePendingSuccessorV1,
    pcontrolRawLopIsPinned,
    pcontrolRawLopIsPinnedWithDescriptor,
) where

import Aiken.Cbor (pdeserialise)
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Midgard.FraudProofs.NativeTx.Codec (pcborInt, pencodeDefiniteBytes)
import Midgard.FraudProofs.NativeTx.Compact (pnativeTxProofCommitmentV1)
import Midgard.FraudProofs.NativeTx.Components (pdecodeMidgardTxInputCbor)
import Midgard.FraudProofs.NativeTx.Types (PMidgardTxInput (..))
import Midgard.LedgerOutputCommitment qualified as Descriptor
import Midgard.LedgerOutputProofRaw qualified as Raw
import Midgard.ValidationMachine (PValidationOneStepWitnessV1 (..), presolutionScheduleNodeHash)
import Midgard.ValidationTrace qualified as Trace
import Plutarch.Prelude

data PControl s = PControl
    { pcontrol'compactCbor :: Term s PByteString
    , pcontrol'witnessSetCompactCbor :: Term s PByteString
    , pcontrol'fieldPreimageLengthsCbor :: Term s PByteString
    , pcontrol'contextCbor :: Term s PByteString
    , pcontrol'cursor :: Term s PInteger
    , pcontrol'accumulator :: Term s PByteString
    , pcontrol'remainingScheduleHash :: Term s PByteString
    , pcontrol'signerCount :: Term s PInteger
    , pcontrol'signerFrontierCommitment :: Term s PByteString
    , pcontrol'pendingCbor :: Term s PByteString
    , pcontrol'resolutionScheduleHash :: Term s PByteString
    }
    deriving stock (Generic)
    deriving anyclass (SOP.Generic)
    deriving (PlutusType) via (DeriveAsSOPStruct PControl)

data PPending s = PPending
    { ppending'sourceKind :: Term s PInteger
    , ppending'key :: Term s PByteString
    , ppending'nextScheduleHash :: Term s PByteString
    , ppending'descriptorCbor :: Term s PByteString
    , ppending'outputProofCbor :: Term s PByteString
    }
    deriving stock (Generic)
    deriving anyclass (SOP.Generic)
    deriving (PlutusType) via (DeriveAsSOPStruct PPending)

pitems :: forall s. Term s PInteger -> Term s PByteString -> Term s (PBuiltinList PData)
pitems count cbor = pmatch (pdeserialise # cbor) $ \case
    PNothing -> perror
    PJust dat -> plet (pasList # dat) $ \items -> pif (plength # items #== count) items perror

pint :: forall s. Term s (PBuiltinList PData) -> Term s PInteger -> Term s PInteger
pint items index = pasInt # (pelemAt # index # items)
pbytes :: forall s. Term s (PBuiltinList PData) -> Term s PInteger -> Term s PByteString
pbytes items index = pasByteStr # (pelemAt # index # items)

pcontrolRawFromWitness :: forall s. Term s (PByteString :--> PControl)
pcontrolRawFromWitness = phoistAcyclic $ plam $ \cbor -> plet (pitems 11 cbor) $ \f ->
    pcon $ PControl (pbytes f 0) (pbytes f 1) (pbytes f 2) (pbytes f 3) (pint f 4) (pbytes f 5) (pbytes f 6) (pint f 7) (pbytes f 8) (pbytes f 9) (pbytes f 10)

pencodeControlRaw :: forall s. Term s (PControl :--> PByteString)
pencodeControlRaw = phoistAcyclic $ plam $ \control -> pmatch control $ \c ->
    pif
        ( pcontrol'cursor c
            #>= 0
            #&& plengthBS
            # pcontrol'accumulator c
            #== 32
            #&& plengthBS
            # pcontrol'remainingScheduleHash c
            #== 32
            #&& pcontrol'signerCount c
            #>= 0
            #&& plengthBS
            # pcontrol'signerFrontierCommitment c
            #== 32
            #&& plengthBS
            # pcontrol'resolutionScheduleHash c
            #== 32
        )
        ( pconstant "\x8b"
            <> (pencodeDefiniteBytes # pcontrol'compactCbor c)
            <> (pencodeDefiniteBytes # pcontrol'witnessSetCompactCbor c)
            <> (pencodeDefiniteBytes # pcontrol'fieldPreimageLengthsCbor c)
            <> (pencodeDefiniteBytes # pcontrol'contextCbor c)
            <> pcborInt (pcontrol'cursor c)
            <> (pencodeDefiniteBytes # pcontrol'accumulator c)
            <> (pencodeDefiniteBytes # pcontrol'remainingScheduleHash c)
            <> pcborInt (pcontrol'signerCount c)
            <> (pencodeDefiniteBytes # pcontrol'signerFrontierCommitment c)
            <> (pencodeDefiniteBytes # pcontrol'pendingCbor c)
            <> (pencodeDefiniteBytes # pcontrol'resolutionScheduleHash c)
        )
        perror

pcontrolNoPendingFromWitness :: forall s. Term s (PByteString :--> PControl)
pcontrolNoPendingFromWitness = phoistAcyclic $ plam $ \cbor -> plet (pcontrolRawFromWitness # cbor) $ \control -> pmatch control $ \c ->
    pif (pcontrol'pendingCbor c #== pconstant "\x00") control perror

pcommonBound :: forall s. Trace.PValidationMachineStateV1 s -> PValidationOneStepWitnessV1 s -> Term s PControl -> PControl s -> Term s PBool
pcommonBound pre witness control c =
    pnativeTxProofCommitmentV1
        # pcontrol'compactCbor c
        # pcontrol'witnessSetCompactCbor c
        # pcontrol'fieldPreimageLengthsCbor c
        #== pfromData (Trace.pmachineState'transactionCommitment pre)
        #&& Trace.phashValidationContext
        # pcontrol'contextCbor c
        #== pfromData (Trace.pmachineState'validationContextHash pre)
        #&& plengthBS
        # pcontrol'accumulator c
        #== 32
        #&& plengthBS
        # pcontrol'remainingScheduleHash c
        #== 32
        #&& plengthBS
        # pcontrol'resolutionScheduleHash c
        #== 32
        #&& (pcontrol'cursor c #> 1 #|| pcontrol'remainingScheduleHash c #== pcontrol'resolutionScheduleHash c)
        #&& pcontrol'signerCount c
        #>= 0
        #&& plengthBS
        # pcontrol'signerFrontierCommitment c
        #== 32
        #&& pfromData (poneStep'workWitnessCbor witness)
        #== pencodeControlRaw
        # control

pcontrolNoPendingIsBound :: forall s. Term s (Trace.PValidationMachineStateV1 :--> PValidationOneStepWitnessV1 :--> PControl :--> PBool)
pcontrolNoPendingIsBound = phoistAcyclic $ plam $ \pre witness control -> pmatch pre $ \p -> pmatch witness $ \w -> pmatch control $ \c ->
    pcontrol'pendingCbor c #== pconstant "\x00" #&& pcommonBound p w control c #&& pcontrol'cursor c #>= 0

pencodePendingRaw :: forall s. Term s (PInteger :--> PByteString :--> PByteString :--> PByteString :--> PByteString :--> PByteString)
pencodePendingRaw = phoistAcyclic $ plam $ \kind key schedule descriptor proof ->
    pconstant "\x85" <> pcborInt kind <> (pencodeDefiniteBytes # key) <> (pencodeDefiniteBytes # schedule) <> (pencodeDefiniteBytes # descriptor) <> (pencodeDefiniteBytes # proof)

ppendingRawFromCbor :: forall s. Term s (PByteString :--> PPending)
ppendingRawFromCbor = phoistAcyclic $ plam $ \cbor -> pif (cbor #== pconstant "\x00") perror $
    plet (pitems 5 cbor) $
        \f -> pcon $ PPending (pint f 0) (pbytes f 1) (pbytes f 2) (pbytes f 3) (pbytes f 4)

pencodePending :: forall s. Term s (PPending :--> PByteString)
pencodePending = phoistAcyclic $ plam $ \pending -> pmatch pending $ \p -> pencodePendingRaw # ppending'sourceKind p # ppending'key p # ppending'nextScheduleHash p # ppending'descriptorCbor p # ppending'outputProofCbor p

pcontrolRawIsBoundWithDescriptor :: forall s. Term s (Trace.PValidationMachineStateV1 :--> PValidationOneStepWitnessV1 :--> PControl :--> PPending :--> Descriptor.PLedgerOutputCommitmentV1 :--> PBool)
pcontrolRawIsBoundWithDescriptor = phoistAcyclic $ plam $ \pre witness control pending descriptor ->
    pmatch pre $ \p -> pmatch witness $ \w -> pmatch control $ \c -> pmatch pending $ \pd -> pmatch descriptor $ \d ->
        pmatch (pdecodeMidgardTxInputCbor # ppending'key pd) $ \input ->
            pcontrol'pendingCbor c
                #== pencodePending
                # pending
                #&& pcommonBound p w control c
                #&& pcontrol'cursor c
                #> 0
                #&& (ppending'sourceKind pd #== 0 #|| ppending'sourceKind pd #== 1)
                #&& presolutionScheduleNodeHash
                # ppending'sourceKind pd
                # ppending'key pd
                # ppending'nextScheduleHash pd
                #== pcontrol'remainingScheduleHash c
                #&& pfromData (ptxInput'outputIndex input)
                #== pfromData (Descriptor.poutputCommitment'outputIndex d)

pcontrolRawIsBound :: forall s. Term s (Trace.PValidationMachineStateV1 :--> PValidationOneStepWitnessV1 :--> PControl :--> PPending :--> PBool)
pcontrolRawIsBound = phoistAcyclic $ plam $ \pre witness control pending -> pmatch pending $ \p ->
    pcontrolRawIsBoundWithDescriptor # pre # witness # control # pending # (Descriptor.pdecodeLedgerOutputCommitment # ppending'descriptorCbor p)

psplicePendingSuccessorV1 :: forall s. Term s (PByteString :--> PByteString :--> PByteString :--> PByteString :--> PByteString)
psplicePendingSuccessorV1 = phoistAcyclic $ plam $ \witness pending oldProof next ->
    plet (pencodeDefiniteBytes # oldProof) $ \oldItem -> plet (plengthBS # pending - plengthBS # oldItem) $ \itemOffset ->
        pif
            (itemOffset #>= 0 #&& psliceBS # itemOffset # (plengthBS # oldItem) # pending #== oldItem)
            ( plet ((psliceBS # 0 # itemOffset # pending) <> (pencodeDefiniteBytes # next)) $ \newPending ->
                plet (pencodeDefiniteBytes # pending) $ \oldField -> plet (plengthBS # witness - plengthBS # oldField - 34) $ \fieldOffset ->
                    pif
                        (fieldOffset #>= 0 #&& psliceBS # fieldOffset # (plengthBS # oldField) # witness #== oldField)
                        ((psliceBS # 0 # fieldOffset # witness) <> (pencodeDefiniteBytes # newPending) <> (psliceBS # (fieldOffset + plengthBS # oldField) # 34 # witness))
                        perror
            )
            perror

pcontrolRawLopIsPinnedWithDescriptor :: forall s. Term s (Raw.PFrame :--> Descriptor.PLedgerOutputCommitmentV1 :--> PBool)
pcontrolRawLopIsPinnedWithDescriptor = phoistAcyclic $ plam $ \frame descriptor -> pmatch descriptor $ \d ->
    Raw.pinteger
        # frame
        # 2
        #== pfromData (Descriptor.poutputCommitment'outputIndex d)
        #&& Raw.pinteger
        # frame
        # 3
        #== pfromData (Descriptor.poutputCommitment'totalLength d)
        #&& Raw.pbytes
        # frame
        # 4
        #== pfromData (Descriptor.poutputCommitment'itemCommitment d)

pcontrolRawLopIsPinned :: forall s. Term s (Raw.PFrame :--> PByteString :--> PBool)
pcontrolRawLopIsPinned = phoistAcyclic $ plam $ \frame cbor -> pcontrolRawLopIsPinnedWithDescriptor # frame # (Descriptor.pdecodeLedgerOutputCommitment # cbor)
