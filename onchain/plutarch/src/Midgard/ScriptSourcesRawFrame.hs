{-# LANGUAGE OverloadedStrings #-}

-- | Canonical predecessor fields establish splice boundaries for ScriptSources.
module Midgard.ScriptSourcesRawFrame (
    PFrame (..),
    pitemIntV1,
    pitemBytesV1,
    pitemFrontierV1,
    popenFrameV1,
    pspliceV1,
    pitemOffsetV1,
    preplaceItemsV1,
    preplaceStageV1,
    pappendExtensionV1,
    preplaceExtensionV1,
    pdropExtensionV1,
    psuccessorIsExactV1,
    pemptyObserverScanCbor,
    pemptyMintFoldCbor,
    pinitialOutputScanCbor,
) where

import Aiken.Cbor (pdeserialise)
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Midgard.FraudProofs.NativeTx.Codec (pcborInt, pencodeDefiniteArrayHeader, pencodeDefiniteBytes)
import Midgard.FraudProofs.NativeTx.Compact (pnativeTxProofCommitmentV1)
import Midgard.LedgerOutputScan qualified as Scan
import Midgard.ValidationMachine (PValidationOneStepWitnessV1 (..))
import Midgard.ValidationMerkle qualified as Merkle
import Midgard.ValidationTrace qualified as Trace
import Plutarch.Prelude

data PFrame s = PFrame
    { pframe'items :: Term s (PBuiltinList PData)
    , pframe'itemCount :: Term s PInteger
    , pframe'prefix :: Term s PByteString
    , pframe'stageOffset :: Term s PInteger
    , pframe'compactCbor :: Term s PByteString
    , pframe'witnessSetCompactCbor :: Term s PByteString
    , pframe'fieldPreimageLengthsCbor :: Term s PByteString
    , pframe'contextCbor :: Term s PByteString
    }
    deriving stock (Generic)
    deriving anyclass (SOP.Generic)
    deriving (PlutusType) via (DeriveAsSOPStruct PFrame)

pitemIntV1 :: forall s. Term s (PFrame :--> PInteger :--> PInteger)
pitemIntV1 = phoistAcyclic $ plam $ \frame index -> pmatch frame $ \f -> pif (index #>= 0) (pasInt # (pelemAt # index # pframe'items f)) perror
pitemBytesV1 :: forall s. Term s (PFrame :--> PInteger :--> PByteString)
pitemBytesV1 = phoistAcyclic $ plam $ \frame index -> pmatch frame $ \f -> pif (index #>= 0) (pasByteStr # (pelemAt # index # pframe'items f)) perror

ppeaks :: forall s. Term s PData -> Term s (PBuiltinList (PAsData Merkle.PFrontierPeak))
ppeaks dat =
    pmap
        # plam
            ( \peak -> plet (pasList # peak) $ \fields ->
                pif (plength # fields #== 2) (pdata $ pcon $ Merkle.PFrontierPeak (pdata $ pasInt # (pelemAt # 0 # fields)) (pdata $ pasByteStr # (pelemAt # 1 # fields))) perror
            )
        # (pasList # dat)
pitemFrontierV1 :: forall s. Term s (PFrame :--> PInteger :--> PBuiltinList (PAsData Merkle.PFrontierPeak))
pitemFrontierV1 = phoistAcyclic $ plam $ \frame index -> pmatch frame $ \f -> pif (index #>= 0) (ppeaks $ pelemAt # index # pframe'items f) perror

popenFrameV1 :: forall s. Term s (Trace.PValidationMachineStateV1 :--> PValidationOneStepWitnessV1 :--> PInteger :--> PInteger :--> PFrame)
popenFrameV1 = phoistAcyclic $ plam $ \pre witness count stage -> pmatch pre $ \p -> pmatch witness $ \w ->
    plet (pfromData $ poneStep'workWitnessCbor w) $ \cbor ->
        pif
            (count #== 30 #|| count #== 31)
            ( pmatch (pdeserialise # cbor) $ \case
                PNothing -> perror
                PJust dat -> plet (pasList # dat) $ \items ->
                    pif
                        (plength # items #== count)
                        ( plet (pasByteStr # (pelemAt # 0 # items)) $ \compact -> plet (pasByteStr # (pelemAt # 1 # items)) $ \ws ->
                            plet (pasByteStr # (pelemAt # 2 # items)) $ \lengths -> plet (pasByteStr # (pelemAt # 3 # items)) $ \context ->
                                plet
                                    ( (pencodeDefiniteArrayHeader # count)
                                        <> (pencodeDefiniteBytes # compact)
                                        <> (pencodeDefiniteBytes # ws)
                                        <> (pencodeDefiniteBytes # lengths)
                                        <> (pencodeDefiniteBytes # context)
                                        <> pcborInt (pasInt # (pelemAt # 4 # items))
                                        <> (pencodeDefiniteBytes # (pasByteStr # (pelemAt # 5 # items)))
                                        <> pcborInt (pasInt # (pelemAt # 6 # items))
                                        <> (pencodeDefiniteBytes # (pasByteStr # (pelemAt # 7 # items)))
                                        <> (Merkle.pencodeFrontier # ppeaks (pelemAt # 8 # items))
                                    )
                                    $ \prefix ->
                                        pif
                                            ( psliceBS
                                                # 0
                                                # (plengthBS # prefix)
                                                # cbor
                                                #== prefix
                                                #&& pnativeTxProofCommitmentV1
                                                # compact
                                                # ws
                                                # lengths
                                                #== pfromData (Trace.pmachineState'transactionCommitment p)
                                                #&& Trace.phashValidationContext
                                                # context
                                                #== pfromData (Trace.pmachineState'validationContextHash p)
                                                #&& pasInt
                                                # (pelemAt # 9 # items)
                                                #== stage
                                            )
                                            (pcon $ PFrame items count prefix (plengthBS # prefix) compact ws lengths context)
                                            perror
                        )
                        perror
            )
            perror

pspliceV1 :: forall s. Term s (PByteString :--> PInteger :--> PByteString :--> PByteString :--> PByteString)
pspliceV1 = phoistAcyclic $ plam $ \cbor offset old new -> plet (offset + plengthBS # old) $ \suffix ->
    pif
        (offset #>= 0 #&& suffix #<= plengthBS # cbor #&& psliceBS # offset # (plengthBS # old) # cbor #== old)
        ((psliceBS # 0 # offset # cbor) <> new <> (psliceBS # suffix # (plengthBS # cbor - suffix) # cbor))
        perror

pencodedItemLengthV1 :: forall s. Term s (PData :--> PInteger)
pencodedItemLengthV1 = phoistAcyclic $ pfix $ \self -> plam $ \item ->
    pforce $
        pchooseData
            # item
            # pdelay perror
            # pdelay perror
            # pdelay (plet (pasList # item) $ \items -> plengthBS # (pencodeDefiniteArrayHeader # (plength # items)) + (pfoldl # plam (\size value -> size + self # value) # 0 # items))
            # pdelay (plengthBS # pcborInt (pasInt # item))
            # pdelay (plet (plengthBS # (pasByteStr # item)) $ \length -> plengthBS # (pencodeDefiniteArrayHeader # length) + length)

pitemOffsetV1 :: forall s. Term s (PFrame :--> PInteger :--> PInteger)
pitemOffsetV1 = phoistAcyclic $ plam $ \frame index -> pmatch frame $ \f ->
    pif
        (index #>= 0 #&& index #<= pframe'itemCount f)
        ( plengthBS # (pencodeDefiniteArrayHeader # pframe'itemCount f)
            + ((pfix $ \self -> plam $ \remaining items -> pif (remaining #== 0) 0 (pencodedItemLengthV1 # (phead # items) + self # (remaining - 1) # (ptail # items))) # index # pframe'items f)
        )
        perror

preplaceItemsV1 :: forall s. Term s (PFrame :--> PByteString :--> PInteger :--> PInteger :--> PByteString :--> PByteString)
preplaceItemsV1 = phoistAcyclic $ plam $ \frame cbor first count next ->
    plet (pitemOffsetV1 # frame # first) $ \start -> plet (pitemOffsetV1 # frame # (first + count)) $ \end ->
        pif (count #> 0 #&& end #<= plengthBS # cbor) (pspliceV1 # cbor # start # (psliceBS # start # (end - start) # cbor) # next) perror

preplaceStageV1 :: forall s. Term s (PFrame :--> PByteString :--> PInteger :--> PByteString)
preplaceStageV1 = phoistAcyclic $ plam $ \frame cbor next -> pmatch frame $ \f ->
    pif (next #>= 0) (pspliceV1 # cbor # pframe'stageOffset f # pcborInt (pitemIntV1 # frame # 9) # pcborInt next) perror

pappendExtensionV1 :: forall s. Term s (PByteString :--> PByteString :--> PByteString)
pappendExtensionV1 = phoistAcyclic $ plam $ \cbor extension -> (pspliceV1 # cbor # 0 # pconstant "\x98\x1e" # pconstant "\x98\x1f") <> (pencodeDefiniteBytes # extension)
preplaceExtensionV1 :: forall s. Term s (PByteString :--> PByteString :--> PByteString :--> PByteString)
preplaceExtensionV1 = phoistAcyclic $ plam $ \cbor old next -> plet (pencodeDefiniteBytes # old) $ \encoded ->
    pif (psliceBS # 0 # 2 # cbor #== pconstant "\x98\x1f") (pspliceV1 # cbor # (plengthBS # cbor - plengthBS # encoded) # encoded # (pencodeDefiniteBytes # next)) perror
pdropExtensionV1 :: forall s. Term s (PByteString :--> PByteString :--> PByteString)
pdropExtensionV1 = phoistAcyclic $ plam $ \cbor old -> plet (pencodeDefiniteBytes # old) $ \encoded ->
    pspliceV1 # (pspliceV1 # cbor # (plengthBS # cbor - plengthBS # encoded) # encoded # pconstant "") # 0 # pconstant "\x98\x1f" # pconstant "\x98\x1e"

psuccessorIsExactV1 :: forall s. Term s (Trace.PValidationMachineStateV1 :--> PValidationOneStepWitnessV1 :--> PByteString :--> PBool)
psuccessorIsExactV1 = phoistAcyclic $ plam $ \pre witness next -> pmatch pre $ \p -> pmatch witness $ \w -> pmatch (pfromData $ poneStep'claimedSuccessor w) $ \post ->
    pfromData (Trace.pmachineState'phase post)
        #== pcon Trace.PScriptSources
        #&& pfromData (Trace.pmachineState'workRoot post)
        #== Trace.phashWorkWitness
        # pcon Trace.PScriptSources
        # (pfromData (Trace.pmachineState'programCounter p) + 1)
        # next

pemptyObserverScanCbor, pemptyMintFoldCbor, pinitialOutputScanCbor :: forall s. Term s PByteString
pemptyObserverScanCbor = pconstant "\x83\x00\x40\x00"
pemptyMintFoldCbor = pconstant "\x8c\x20\x00\x40\x40\x00\x40\x00\x00\x00\x40\x00\x80"
pinitialOutputScanCbor = Scan.pinitialControlCborV1
