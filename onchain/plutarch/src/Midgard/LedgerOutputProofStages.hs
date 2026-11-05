{-# LANGUAGE OverloadedStrings #-}

-- | Narrow output-proof transitions preserve all unrelated raw sub-controls.
module Midgard.LedgerOutputProofStages (
    PStepResult (..),
    pfactsEmpty,
    pextensionItemsEmpty,
    pfactAttach,
    pfactsAreExact,
    pstructure,
    pstructureHeaders,
    pstructureAssets,
    pstructureOptional,
    pstructureFinish,
    pvalueFold,
    pspanAttach,
    pboundWindowSpan,
    preferenceScript,
    pscriptHash,
    pnativeScript,
    pinitialDatumData,
    pinitialValueData,
) where

import Aiken.Cbor (pdeserialise)
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Midgard.Blake2b224Trace qualified as Blake
import Midgard.BoundedItem qualified as Bounded
import Midgard.LedgerOutputProof qualified as Proof
import Midgard.LedgerOutputProofRaw qualified as Raw
import Midgard.LedgerOutputScan qualified as Scan
import Midgard.LedgerOutputValue qualified as Value
import Midgard.NativeScriptScan qualified as Native
import Midgard.ValidationMerkle qualified as Merkle
import Plutarch.Builtin.Crypto (pblake2b_256)
import Plutarch.Core.Utils ((#/=))
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.Prelude

-- Internal result, never a datum/redeemer encoding.
data PStepResult s = PAdvanced (Term s Raw.PFrame) | PInvalidOutput | PInvalidReferenceScript | PNativeScriptNodeLimit | PNativeScriptDepthLimit
    deriving stock (Generic)
    deriving anyclass (SOP.Generic)
    deriving (PlutusType) via (DeriveAsSOPStruct PStepResult)

pencodedData :: forall s. Term s PByteString -> Term s PData
pencodedData bytes = pmatch (pdeserialise # bytes) $ \case PNothing -> perror; PJust dat -> dat

pnone :: forall s. Term s PData
pnone = pforgetData $ pconstrBuiltin # 1 # pnil

psome :: forall s. Term s PData -> Term s PData
psome dat = pforgetData $ pconstrBuiltin # 0 # (pcons # dat # pnil)

pinner :: forall s. Term s Raw.PFrame -> Term s PInteger -> Term s PData
pinner control index = pmatch (pasConstr # (Raw.pitem # control # index)) $ \(PBuiltinPair tag fields) ->
    pif (tag #== 0 #&& plength # fields #== 1) (phead # fields) perror

pint :: forall s. Term s PInteger -> Term s PData
pint = pforgetData . pdata

padvanced :: forall s. Term s Raw.PFrame -> Term s (PMaybe PStepResult)
padvanced control = pcon $ PJust $ pcon $ PAdvanced control

pfactsEmpty :: forall s. Term s (Raw.PFrame :--> PBool)
pfactsEmpty = phoistAcyclic $ plam $ \control ->
    Raw.pitem
        # control
        # 13
        #== pnone
        #&& Raw.pitem
        # control
        # 14
        #== pnone
        #&& Raw.pitem
        # control
        # 15
        #== pnone
        #&& Raw.pitem
        # control
        # 16
        #== pnone

pextensionItemsEmpty :: forall s. Term s (Raw.PFrame :--> PBool)
pextensionItemsEmpty = phoistAcyclic $ plam $ \control -> Raw.pitem # control # 12 #== pnone #&& pfactsEmpty # control

pfactAttach :: forall s. Term s (Raw.PFrame :--> PBuiltinList PInteger :--> PByteString :--> PData :--> PData :--> PMaybe Raw.PFrame)
pfactAttach = phoistAcyclic $ plam $ \control roles descriptor value datum ->
    pif
        (Raw.pinteger # control # 1 #== Proof.pstageTerminal)
        ( plet
            ( pfind
                # plam (\group -> pany # plam (\role -> Raw.pfact # control # role #== pcon PDNothing) # group)
                # Proof.pfactAttachGroupsV1
            )
            $ \selected -> pmatch selected $ \case
                PNothing -> pcon PNothing
                PJust group ->
                    pif (roles #/= group) (pcon PNothing) $
                        pif
                            (pall # plam (\role -> Raw.pfact # control # role #== pcon PDNothing) # group)
                            ( pcon $
                                PJust $
                                    pfoldl
                                        # plam
                                            ( \acc role ->
                                                Raw.preplace
                                                    # acc
                                                    # (13 + role)
                                                    # (Raw.pfactData # (Proof.pfactCommitmentV1 # role # descriptor # value # datum))
                                            )
                                        # control
                                        # group
                            )
                            perror
        )
        perror

pfactsAreExact :: forall s. Term s (Raw.PFrame :--> PByteString :--> PData :--> PData :--> PBool)
pfactsAreExact = phoistAcyclic $ plam $ \control descriptor value datum ->
    Raw.pinteger
        # control
        # 1
        #== Proof.pstageTerminal
        #&& ( pall
                # plam (\role -> Raw.pfact # control # role #== pcon (PDJust $ pdata $ Proof.pfactCommitmentV1 # role # descriptor # value # datum))
                # pconstant @(PBuiltinList PInteger) [0, 1, 2, 3]
            )

pstructureExtras :: forall s. Term s Raw.PFrame -> Term s PBool
pstructureExtras control =
    Raw.pitem
        # control
        # 6
        #== pnone
        #&& Raw.pitem
        # control
        # 7
        #== pnone
        #&& Raw.pinteger
        # control
        # 8
        #== 0
        #&& Raw.pitem
        # control
        # 9
        #== plistData
        # pnil
        #&& Raw.pitem
        # control
        # 10
        #== pnone
        #&& Raw.pitem
        # control
        # 11
        #== pnone
        #&& pextensionItemsEmpty
        # control

pstartValue :: forall s. Term s Raw.PFrame -> Term s Scan.PLedgerOutputScanControlV1 -> Term s (PMaybe PStepResult)
pstartValue control scan = pmatch scan $ \s ->
    padvanced $
        Raw.preplace
            # (Raw.preplace # control # 1 # pint Proof.pstageValueFold)
            # 6
            # psome (pinitialValueData # pfromData (Scan.pscan'assetCount s))

pstructure :: forall s. Term s (Raw.PFrame :--> Proof.PLedgerOutputProofWitnessV1 :--> PMaybe PStepResult)
pstructure = phoistAcyclic $ plam $ \control witness ->
    plet (Scan.pcontrolFromDataV1 # (Raw.pitem # control # 5)) $ \scan -> pmatch scan $ \s ->
        plet (Raw.pinteger # control # 3) $ \total ->
            pif
                (Raw.pinteger # control # 1 #== Proof.pstageStructure #&& pfromData (Scan.pscan'cursor s) #<= total #&& pstructureExtras control)
                ( pif
                    (Scan.pterminalIsExactV1 # scan # total)
                    (pif (witness #== pcon Proof.PLedgerOutputProofNoWitness) (pstartValue control scan) (pcon PNothing))
                    $ pmatch (Scan.pfinishV1 # scan # total)
                    $ \case
                        PJust finished ->
                            pif
                                (witness #== pcon Proof.PLedgerOutputProofNoWitness)
                                (padvanced $ Raw.preplace # control # 5 # pencodedData (Scan.pencodeControlV1 # finished))
                                (pcon PNothing)
                        PNothing -> pscanWindow control scan total witness Scan.pstepV1
                )
                perror

pscanWindow ::
    forall s.
    Term s Raw.PFrame ->
    Term s Scan.PLedgerOutputScanControlV1 ->
    Term s PInteger ->
    Term s Proof.PLedgerOutputProofWitnessV1 ->
    Term s (Scan.PLedgerOutputScanControlV1 :--> PInteger :--> PByteString :--> PInteger :--> PMaybe Scan.PLedgerOutputScanControlV1) ->
    Term s (PMaybe PStepResult)
pscanWindow control scan total witness transition = pmatch scan $ \s ->
    pmatch
        ( Proof.pauthenticatedChunkWindowForOutput
            (Raw.pinteger # control # 2)
            total
            (Raw.pbytes # control # 4)
            (pfromData $ Scan.pscan'cursor s)
            witness
            (pfromData (Scan.pscan'stage s) #<= Scan.pstageOptionalField)
        )
        $ \case
            PNothing -> pcon PNothing
            PJust pair -> pmatch pair $ \(PPair window offset) -> pmatch (transition # scan # total # window # offset) $ \case
                PNothing -> pcon $ PJust $ pcon PInvalidOutput
                PJust next -> padvanced $ Raw.preplace # control # 5 # pencodedData (Scan.pencodeControlV1 # next)

pstructureScan ::
    forall s.
    Integer ->
    Term s (Scan.PLedgerOutputScanControlV1 :--> PInteger :--> PByteString :--> PInteger :--> PMaybe Scan.PLedgerOutputScanControlV1) ->
    Term s (Raw.PFrame :--> Proof.PLedgerOutputProofWitnessV1 :--> PMaybe PStepResult)
pstructureScan group transition = plam $ \control witness ->
    plet (Scan.pcontrolFromDataV1 # (Raw.pitem # control # 5)) $ \scan -> pmatch scan $ \s ->
        plet (pfromData $ Scan.pscan'stage s) $ \stage ->
            plet (Raw.pinteger # control # 3) $ \total ->
                let stageMatches = case group of
                        0 -> stage #== Scan.pstageRequiredFields #|| stage #== Scan.pstageValueHeader
                        1 -> stage #== Scan.pstagePolicyHeader #|| stage #== Scan.pstageAsset
                        _ -> stage #>= Scan.pstageOptionalField #&& stage #<= Scan.pstageReferenceScriptPayload
                 in pif
                        ( Raw.pinteger
                            # control
                            # 1
                            #== Proof.pstageStructure
                            #&& stageMatches
                            #&& pfromData (Scan.pscan'cursor s)
                            #<= total
                            #&& Scan.pfinishV1
                            # scan
                            # total
                            #== pcon PNothing
                            #&& pstructureExtras control
                        )
                        (pscanWindow control scan total witness transition)
                        perror

pstructureHeaders, pstructureAssets, pstructureOptional, pstructureFinish :: forall s. Term s (Raw.PFrame :--> Proof.PLedgerOutputProofWitnessV1 :--> PMaybe PStepResult)
pstructureHeaders = phoistAcyclic $ pstructureScan 0 Scan.pstepHeaders
pstructureAssets = phoistAcyclic $ pstructureScan 1 Scan.pstepAssets
pstructureOptional = phoistAcyclic $ pstructureScan 2 Scan.pstepOptional
pstructureFinish = phoistAcyclic $ plam $ \control witness ->
    plet (Scan.pcontrolFromDataV1 # (Raw.pitem # control # 5)) $ \scan ->
        plet (Raw.pinteger # control # 3) $ \total ->
            pif
                (Raw.pinteger # control # 1 #== Proof.pstageStructure #&& witness #== pcon Proof.PLedgerOutputProofNoWitness #&& pstructureExtras control)
                ( pif (Scan.pterminalIsExactV1 # scan # total) (pstartValue control scan) $
                    pmatch (Scan.pfinishV1 # scan # total) $ \case
                        PNothing -> pcon PNothing
                        PJust next -> padvanced $ Raw.preplace # control # 5 # pencodedData (Scan.pencodeControlV1 # next)
                )
                perror

pinitialDatumData :: forall s. Term s (PInteger :--> PInteger :--> PData)
pinitialDatumData = phoistAcyclic $ plam $ \offset length ->
    pif
        (offset #>= 0 #&& offset #<= 4294967295 #&& length #> 0 #&& length #<= 4294967295)
        ( plistData
            # ( pcons
                    # pint 1
                    # ( pcons
                            # pint 0
                            # ( pcons
                                    # pint offset
                                    # ( pcons
                                            # pint length
                                            # ( pcons
                                                    # pint 0
                                                    # (pcons # pforgetData (pdata $ pconstant @PByteString "") # (pcons # pnone # (pcons # pnone # (pcons # pnone # (pcons # pnone # pnil)))))
                                              )
                                      )
                              )
                      )
              )
        )
        perror

pinitialValueData :: forall s. Term s (PInteger :--> PData)
pinitialValueData = phoistAcyclic $ plam $ \count ->
    pif
        (count #>= 0 #&& count #<= 4294967295)
        (pencodedData $ Value.pencodeControlV1 # (Value.pinitialControlV1 # count))
        perror

pvalueFold :: forall s. Term s (Raw.PFrame :--> Proof.PLedgerOutputProofWitnessV1 :--> PMaybe PStepResult)
pvalueFold = phoistAcyclic $ plam $ \control witness ->
    plet (Scan.pcontrolFromDataV1 # (Raw.pitem # control # 5)) $ \scan -> pmatch scan $ \s ->
        plet (Value.pcontrolFromDataV1 # pinner control 6) $ \value -> pmatch value $ \v ->
            pif
                ( Raw.pinteger
                    # control
                    # 1
                    #== Proof.pstageValueFold
                    #&& Scan.pterminalIsExactV1
                    # scan
                    # (Raw.pinteger # control # 3)
                    #&& pfromData (Value.pvalueControl'assetRemaining v)
                    #<= pfromData (Scan.pscan'assetCount s)
                    #&& Raw.pitem
                    # control
                    # 7
                    #== pnone
                    #&& Raw.pinteger
                    # control
                    # 8
                    #== 0
                    #&& Raw.pitem
                    # control
                    # 9
                    #== plistData
                    # pnil
                    #&& Raw.pitem
                    # control
                    # 10
                    #== pnone
                    #&& Raw.pitem
                    # control
                    # 11
                    #== pnone
                    #&& pextensionItemsEmpty
                    # control
                )
                ( pif
                    (pfromData (Value.pvalueControl'stage v) #== Value.pstageTerminal)
                    ( pif (witness #/= pcon Proof.PLedgerOutputProofNoWitness) (pcon PNothing) $
                        pif
                            (pfromData (Scan.pscan'datumOffset s) #/= (-1))
                            ( padvanced $
                                Raw.preplace
                                    # (Raw.preplace # control # 1 # pint Proof.pstageDatumTraversal)
                                    # 7
                                    # psome (pinitialDatumData # pfromData (Scan.pscan'datumOffset s) # pfromData (Scan.pscan'datumLength s))
                            )
                            (padvanced $ Raw.preplace # control # 1 # pint (pif (pfromData (Scan.pscan'referenceScriptLanguage s) #== (-1)) Proof.pstageTerminal Proof.pstageReferenceScriptCommitment))
                    )
                    $ pmatch witness
                    $ \case
                        Proof.PLedgerOutputProofNoWitness -> pnextValue # control # value # scan # pcon Value.PLedgerOutputValueNoWitness
                        Proof.PLedgerOutputProofValue index policy asset qty siblings previous ->
                            pnextValue # control # value # scan # pcon (Value.PLedgerOutputValueAsset index policy asset qty siblings previous)
                        _ -> pcon PNothing
                )
                perror

pnextValue :: forall s. Term s (Raw.PFrame :--> Value.PLedgerOutputValueControlV1 :--> Scan.PLedgerOutputScanControlV1 :--> Value.PLedgerOutputValueWitnessV1 :--> PMaybe PStepResult)
pnextValue = phoistAcyclic $ plam $ \control value scan witness -> pmatch scan $ \s ->
    pmatch (Value.pstepV1 # value # pfromData (Scan.pscan'assetCount s) # pfromData (Scan.pscan'assetPeaks s) # pfromData (Scan.pscan'lovelace s) # witness) $ \case
        PNothing -> pcon PNothing
        PJust next -> padvanced $ Raw.preplace # control # 6 # psome (pencodedData $ Value.pencodeControlV1 # next)

pboundWindowSpan :: forall s. Term s (Raw.PFrame :--> PInteger :--> PInteger :--> Proof.PLedgerOutputProofWitnessV1 :--> PMaybe PByteString)
pboundWindowSpan = phoistAcyclic $ plam $ \control start length witness -> pmatch witness $ \case
    Proof.PLedgerOutputProofWindow bytes -> Proof.pboundWindowBytesV1 # (Raw.pspanWindow # control) # start # length # pfromData bytes
    _ -> pcon PNothing

pspanAttach :: forall s. Term s (Raw.PFrame :--> Proof.PLedgerOutputProofWitnessV1 :--> PMaybe PStepResult)
pspanAttach = phoistAcyclic $ plam $ \control witness ->
    plet (Raw.pinteger # control # 1) $ \stage ->
        pif
            ((stage #== Proof.pstageDatumTraversal #|| stage #== Proof.pstageReferenceScriptCommitment #|| stage #== Proof.pstageScriptHash) #&& pfactsEmpty # control)
            ( pmatch witness $ \case
                Proof.PLedgerOutputProofSpanAttach start length first next ->
                    pmatch
                        ( Proof.pauthenticatedOutputSpanForOutput
                            (Raw.pinteger # control # 2)
                            (Raw.pinteger # control # 3)
                            (Raw.pbytes # control # 4)
                            (pfromData start)
                            (pfromData length)
                            (pcon $ Proof.PLedgerOutputProofChunks first next)
                        )
                        $ \case
                            PNothing -> pcon PNothing
                            PJust bytes -> padvanced $ Raw.preplace # control # 12 # (Raw.pspanWindowData # pfromData start # pfromData length # (pblake2b_256 # bytes))
                _ -> pcon PNothing
            )
            perror

pdecodePeaks :: forall s. Term s PData -> Term s (PBuiltinList (PAsData Merkle.PFrontierPeak))
pdecodePeaks dat =
    pmap
        # plam
            ( \peak -> plet (pasList # peak) $ \fields ->
                pif
                    (plength # fields #== 2)
                    (pdata $ pcon $ Merkle.PFrontierPeak (pdata $ pasInt # (phead # fields)) (pdata $ pasByteStr # (pelemAt # 1 # fields)))
                    perror
            )
        # (pasList # dat)

preferenceScript :: forall s. Term s (Raw.PFrame :--> Proof.PLedgerOutputProofWitnessV1 :--> PMaybe PStepResult)
preferenceScript = phoistAcyclic $ plam $ \control witness ->
    plet (Scan.pcontrolFromDataV1 # (Raw.pitem # control # 5)) $ \scan -> pmatch scan $ \s ->
        plet (Raw.pinteger # control # 3) $ \total ->
            plet (total - pfromData (Scan.pscan'referenceScriptItemOffset s)) $ \length ->
                plet (Bounded.pchunkCount # length) $ \count ->
                    plet (Raw.pinteger # control # 8) $ \index ->
                        plet (pdecodePeaks $ Raw.pitem # control # 9) $ \peaks ->
                            pif
                                ( Raw.pinteger
                                    # control
                                    # 1
                                    #== Proof.pstageReferenceScriptCommitment
                                    #&& pfactsEmpty
                                    # control
                                    #&& Scan.pterminalIsExactV1
                                    # scan
                                    # total
                                    #&& pfromData (Scan.pscan'referenceScriptLanguage s)
                                    #/= (-1)
                                    #&& length
                                    #> 0
                                    #&& index
                                    #>= 0
                                    #&& index
                                    #<= count
                                    #&& Merkle.pfrontierIsWellFormed
                                    # index
                                    # peaks
                                    #&& Raw.pitem
                                    # control
                                    # 10
                                    #== pnone
                                    #&& Raw.pitem
                                    # control
                                    # 11
                                    #== pnone
                                )
                                ( pif
                                    (index #== count)
                                    ( pif
                                        (witness #== pcon Proof.PLedgerOutputProofNoWitness)
                                        ( padvanced $
                                            Raw.preplace
                                                # (Raw.preplace # control # 1 # pint Proof.pstageScriptHash)
                                                # 10
                                                # psome (pencodedData $ Blake.pencodeControlV1 # (Blake.pinitialControlV1 # (pfromData (Scan.pscan'referenceScriptLength s) + 1)))
                                        )
                                        (pcon PNothing)
                                    )
                                    $ pmatch
                                        ( pboundWindowSpan
                                            # control
                                            # (pfromData (Scan.pscan'referenceScriptItemOffset s) + index * Bounded.pchunkBytes)
                                            # (Bounded.pexpectedChunkLength # length # index)
                                            # witness
                                        )
                                    $ \case
                                        PNothing -> pcon PNothing
                                        PJust chunk ->
                                            padvanced $
                                                Raw.preplace
                                                    # (Raw.preplace # control # 8 # pint (index + 1))
                                                    # 9
                                                    # pencodedData
                                                        ( Merkle.pencodeFrontier
                                                            # ( Merkle.pappendLeaf
                                                                    # index
                                                                    # peaks
                                                                    # (Bounded.phashChunk # 2 # (Raw.pinteger # control # 2) # index # chunk)
                                                              )
                                                        )
                                )
                                perror

pnextHash :: forall s. Term s Raw.PFrame -> Term s (PMaybe Blake.PBlake2b224TraceControlV1) -> Term s (PMaybe PStepResult)
pnextHash control result = pmatch result $ \case
    PNothing -> pcon PNothing
    PJust next -> padvanced $ Raw.preplace # control # 10 # psome (pencodedData $ Blake.pencodeControlV1 # next)

pscriptHash :: forall s. Term s (Raw.PFrame :--> Proof.PLedgerOutputProofWitnessV1 :--> PMaybe PStepResult)
pscriptHash = phoistAcyclic $ plam $ \control witness ->
    plet (Scan.pcontrolFromDataV1 # (Raw.pitem # control # 5)) $ \scan -> pmatch scan $ \s ->
        plet (Blake.pcontrolFromDataV1 # pinner control 10) $ \hash -> pmatch hash $ \h ->
            pif
                ( Raw.pinteger
                    # control
                    # 1
                    #== Proof.pstageScriptHash
                    #&& pfactsEmpty
                    # control
                    #&& pfromData (Blake.pctl'totalLength h)
                    #== pfromData (Scan.pscan'referenceScriptLength s)
                    + 1
                        #&& Raw.pitem
                        # control
                        # 11
                        #== pnone
                )
                ( pif
                    (pfromData (Blake.pctl'stage h) #== Blake.pstageTerminal)
                    ( pif (witness #/= pcon Proof.PLedgerOutputProofNoWitness) (pcon PNothing)
                        $ pif
                            (pfromData (Scan.pscan'referenceScriptLanguage s) #/= 0)
                            (padvanced $ Raw.preplace # control # 1 # pint Proof.pstageTerminal)
                        $ pif (pfromData (Scan.pscan'referenceScriptLength s) #== 0) (pcon $ PJust $ pcon PInvalidReferenceScript)
                        $ padvanced
                        $ Raw.preplace
                            # (Raw.preplace # control # 1 # pint Proof.pstageNativeScript)
                            # 11
                            # psome
                                ( pencodedData $
                                    Native.pencodeStructureControlV1
                                        # ( Native.pinitialStructureControlV1
                                                # pfromData (Scan.pscan'referenceScriptOffset s)
                                                # pfromData (Scan.pscan'referenceScriptLength s)
                                          )
                                )
                    )
                    $ pif
                        (pfromData (Blake.pctl'stage h) #== Blake.pstageReady)
                        ( plet (pfromData (Blake.pctl'totalLength h) - pfromData (Blake.pctl'cursor h)) $ \remaining ->
                            plet (pif (remaining #< Blake.pblockBytes) remaining Blake.pblockBytes) $ \expected ->
                                plet (pfromData (Blake.pctl'cursor h) #== 0) $ \includesLanguage ->
                                    plet (expected - pif includesLanguage 1 0) $ \contentLength ->
                                        plet (pfromData (Scan.pscan'referenceScriptOffset s) + pfromData (Blake.pctl'cursor h) - pif includesLanguage 0 1) $ \start ->
                                            plet
                                                ( pif
                                                    (contentLength #== 0)
                                                    (pif (witness #== pcon Proof.PLedgerOutputProofNoWitness) (pcon $ PJust $ pconstant "") (pcon PNothing))
                                                    (pboundWindowSpan # control # start # contentLength # witness)
                                                )
                                                $ \content -> pmatch content $ \case
                                                    PNothing -> pcon PNothing
                                                    PJust bytes ->
                                                        pnextHash control $
                                                            Blake.pstepV1
                                                                # hash
                                                                # pcon
                                                                    ( PJust $
                                                                        pif includesLanguage ((preplicateBS # 1 # (pintegerToByte # pfromData (Scan.pscan'referenceScriptLanguage s))) <> bytes) bytes
                                                                    )
                        )
                        (pif (witness #== pcon Proof.PLedgerOutputProofNoWitness) (pnextHash control $ Blake.pstepV1 # hash # pcon PNothing) (pcon PNothing))
                )
                perror

pmapNative :: forall s. Term s Raw.PFrame -> Term s (PMaybe Native.PNativeScriptStructureStepResultV1) -> Term s (PMaybe PStepResult)
pmapNative control result = pmatch result $ \case
    PNothing -> pcon PNothing
    PJust step -> pmatch step $ \case
        Native.PNativeScriptStructureAdvanced next ->
            padvanced $
                Raw.preplace
                    # control
                    # 11
                    # psome (pencodedData $ Native.pencodeStructureControlV1 # pfromData next)
        Native.PNativeScriptStructureInvalid -> pcon $ PJust $ pcon PInvalidReferenceScript
        Native.PNativeScriptStructureNodeLimit -> pcon $ PJust $ pcon PNativeScriptNodeLimit
        Native.PNativeScriptStructureDepthLimit -> pcon $ PJust $ pcon PNativeScriptDepthLimit

pnativeScript :: forall s. Term s (Raw.PFrame :--> Proof.PLedgerOutputProofWitnessV1 :--> PMaybe PStepResult)
pnativeScript = phoistAcyclic $ plam $ \control witness ->
    plet (Native.pstructureControlFromDataV1 # pinner control 11) $ \native -> pmatch native $ \n ->
        pif
            (Raw.pinteger # control # 1 #== Proof.pstageNativeScript #&& pfactsEmpty # control)
            ( pif
                (pfromData (Native.pstructure'stage n) #== Native.pstructureStageTerminal)
                (pif (witness #== pcon Proof.PLedgerOutputProofNoWitness) (padvanced $ Raw.preplace # control # 1 # pint Proof.pstageTerminal) (pcon PNothing))
                $ pif
                    (pfromData (Native.pstructure'stage n) #== Native.pstructureStageToken)
                    ( pmatch
                        ( Proof.pauthenticatedChunkWindowForOutput
                            (Raw.pinteger # control # 2)
                            (Raw.pinteger # control # 3)
                            (Raw.pbytes # control # 4)
                            (pfromData $ Native.pstructure'cursor n)
                            witness
                            (pconstant True)
                        )
                        $ \case
                            PNothing -> pcon PNothing
                            PJust pair -> pmatch pair $ \(PPair window offset) -> pmapNative control (Native.pstructureTokenStepV1 # native # window # offset)
                    )
                $ pif
                    (pfromData (Native.pstructure'stage n) #== Native.pstructureStageFrame)
                    ( pmatch witness $ \case
                        Proof.PLedgerOutputProofNativeFrame frame -> pmapNative control (Native.pstructureFrameStepV1 # native # pfromData frame)
                        _ -> pcon PNothing
                    )
                $ pif (witness #== pcon Proof.PLedgerOutputProofNoWitness) (pmapNative control $ Native.pfinalizeStructureV1 # native) (pcon PNothing)
            )
            perror
