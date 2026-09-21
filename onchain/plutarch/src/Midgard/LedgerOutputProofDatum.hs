{-# LANGUAGE OverloadedStrings #-}

-- | Stage-specific datum traversal over an authenticated output-proof frame.
module Midgard.LedgerOutputProofDatum (
    pfoldMap,
    pfoldList,
    pfinalizeFrame,
    pheadScalar,
    pheadSequence,
    pheadMap,
    pheadLargeConstructor,
    pintegerScalarClaimIsExact,
    pbytesScalarClaimIsExact,
    pattachInteger,
    pattachBytes,
    padvanceInteger,
    padvanceBytes,
    padvanceLargeConstructor,
    padvanceLargeFields,
    pclose,
    pfinish,
) where

import Aiken.Cbor (pdeserialise)
import Midgard.Blake2b256Trace qualified as Blake
import Midgard.CekBlobFrontier qualified as Frontier
import Midgard.CekData qualified as Summary
import Midgard.CekDataBytes qualified as Bytes
import Midgard.CekDataFrame qualified as Frame
import Midgard.CekDataInteger qualified as Integer
import Midgard.CekDataTraverse qualified as Traverse
import Midgard.CekSourceBlob qualified as Blob
import Midgard.FraudProofs.NativeTx.Codec (pbyteAt)
import Midgard.LedgerOutputProof qualified as Proof
import Midgard.LedgerOutputProofRaw qualified as Raw
import Midgard.LedgerOutputProofStages qualified as Stages
import Midgard.ValidationMerkle qualified as Merkle
import Plutarch.Core.Utils ((#/=))
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.Prelude hiding (pfield)

pnone :: forall s. Term s PData
pnone = pforgetData $ pconstrBuiltin # 1 # pnil

psome :: forall s. Term s PData -> Term s PData
psome dat = pforgetData $ pconstrBuiltin # 0 # (pcons # dat # pnil)

pint :: forall s. Term s PInteger -> Term s PData
pint = pforgetData . pdata

pbytesData :: forall s. Term s PByteString -> Term s PData
pbytesData = pforgetData . pdata

pencoded :: forall s. Term s PByteString -> Term s PData
pencoded cbor = pmatch (pdeserialise # cbor) $ \case PNothing -> perror; PJust dat -> dat

pfields :: forall s. Term s PInteger -> Term s PInteger -> Term s PData -> Term s (PBuiltinList PData)
pfields tag length dat = pmatch (pasConstr # dat) $ \(PBuiltinPair index fields) ->
    pif (index #== tag #&& plength # fields #== length) fields perror

pfield :: forall s. Term s (PBuiltinList PData) -> Term s PInteger -> Term s PData
pfield fields index = pelemAt # index # fields

pinteger :: forall s. Term s (PBuiltinList PData) -> Term s PInteger -> Term s PInteger
pinteger fields index = pasInt # pfield fields index

pdatumItems :: forall s. Term s Raw.PFrame -> Term s (PBuiltinList PData)
pdatumItems control =
    pif
        (Raw.pinteger # control # 1 #== Proof.pstageDatumTraversal #&& Stages.pfactsEmpty # control)
        (pasList # (phead # pfields 0 1 (Raw.pitem # control # 7)))
        perror

pupdateItems :: forall s. Term s (PBuiltinList PData) -> Term s PInteger -> Term s PData -> Term s (PBuiltinList PData)
pupdateItems items index value =
    ( pfix $ \self -> plam $ \offset rest ->
        pelimList (\head tail -> pcons # (pif (offset #== index) value head) # (self # (offset + 1) # tail)) pnil rest
    )
        # 0
        # items

pnextItems :: forall s. Term s Raw.PFrame -> Term s (PBuiltinList PData) -> Term s (PMaybe Stages.PStepResult)
pnextItems control items = pcon $ PJust $ pcon $ Stages.PAdvanced $ Raw.preplace # control # 7 # psome (plistData # items)

pcheckPrefix :: forall s. Term s (PBuiltinList PData) -> Term s PInteger -> Term s PBool
pcheckPrefix items stage =
    plength
        # items
        #== 10
        #&& pinteger items 0
        #== 1
        #&& pinteger items 1
        #== stage
        #&& pinteger items 2
        #>= 0
        #&& pinteger items 2
        #<= 4294967295
        #&& pinteger items 3
        #> 0
        #&& pinteger items 3
        #<= 4294967295
        #&& pinteger items 4
        #>= 0
        #&& pinteger items 4
        #<= pinteger items 3

pemptySlots :: forall s. Term s (PBuiltinList PData) -> Term s PBool
pemptySlots items = pfield items 6 #== pnone #&& pfield items 7 #== pnone #&& pfield items 8 #== pnone #&& pfield items 9 #== pnone

pfoldControl :: forall s. Term s Raw.PFrame -> Term s (PBuiltinList PData)
pfoldControl control = plet (pdatumItems control) $ \items ->
    pif (pcheckPrefix items Traverse.pstageFold #&& plengthBS # (pasByteStr # pfield items 5) #== 32 #&& pemptySlots items) items perror

pactionFields :: forall s. Term s PData -> Term s PInteger -> Term s PInteger -> Term s (PBuiltinList PData)
pactionFields witness tag count = plet (pfields 3 2 witness) $ \fields ->
    pif (pfield fields 1 #== pnone) (pfields tag count (phead # fields)) perror

pfoldMap :: forall s. Term s (Raw.PFrame :--> PData :--> PMaybe Stages.PStepResult)
pfoldMap = phoistAcyclic $ plam $ \control witness ->
    plet (pfoldControl control) $ \items -> plet (pactionFields witness 7 6) $ \fields ->
        pmatch
            ( Traverse.pprevalidatedFoldMapNextFrameRootV1
                # (pasByteStr # pfield items 5)
                # pdecodeFrame (pfield fields 0)
                # pinteger fields 1
                # pdecodeSummary (pfield fields 2)
                # pdecodeSummary (pfield fields 3)
                # pdecodePath (pfield fields 4)
                # pdecodePath (pfield fields 5)
            )
            $ \case
                PNothing -> pcon PNothing
                PJust root -> pnextItems control (pupdateItems items 5 $ pbytesData root)

pfoldList :: forall s. Term s (Raw.PFrame :--> PData :--> PMaybe Stages.PStepResult)
pfoldList = phoistAcyclic $ plam $ \control witness ->
    plet (pfoldControl control) $ \items -> plet (pactionFields witness 6 4) $ \fields ->
        pmatch
            ( Traverse.pprevalidatedFoldListNextFrameRoot
                # (pasByteStr # pfield items 5)
                # pdecodeFrame (pfield fields 0)
                # pinteger fields 1
                # pdecodeSummary (pfield fields 2)
                # pdecodePath (pfield fields 3)
            )
            $ \case
                PNothing -> pcon PNothing
                PJust root -> pnextItems control (pupdateItems items 5 $ pbytesData root)

pfinalizeFrame :: forall s. Term s (Raw.PFrame :--> PData :--> PMaybe Stages.PStepResult)
pfinalizeFrame = phoistAcyclic $ plam $ \control witness ->
    plet (pfoldControl control) $ \items -> plet (pactionFields witness 8 2) $ \fields ->
        pmatch
            ( Traverse.pprevalidatedFinalizeFrameTransitionV1
                # (pasByteStr # pfield items 5)
                # pinteger items 4
                # pinteger items 3
                # pdecodeFrame (pfield fields 0)
                # pdecodeParent (pfield fields 1)
            )
            $ \case
                PNothing -> pcon PNothing
                PJust transition -> pmatch transition $ \(Traverse.PFinalizeFrameTransitionV1 stage root result) ->
                    pnextItems control $
                        pupdateItems (pupdateItems (pupdateItems items 1 $ pint stage) 5 $ pbytesData root) 9 $
                            pencoded (Traverse.pencodeOptionalSummaryV1 # result)

psimpleControl :: forall s. Term s Raw.PFrame -> Term s PInteger -> Term s Traverse.PDataTraverseControlV1
psimpleControl control stage = plet (pdatumItems control) $ \items ->
    plet (pasByteStr # pfield items 5) $ \root ->
        pif
            ( pcheckPrefix items stage
                #&& pinteger items 4
                #< pinteger items 3
                #&& pemptySlots items
                #&& (plengthBS # root #== 32 #|| (stage #== Traverse.pstageHead #&& root #== pconstant "" #&& pinteger items 4 #== 0))
            )
            ( pcon $
                Traverse.PDataTraverseControlV1
                    (pdata 1)
                    (pdata stage)
                    (pdata $ pinteger items 2)
                    (pdata $ pinteger items 3)
                    (pdata $ pinteger items 4)
                    (pdata root)
                    (pdata $ pcon PDNothing)
                    (pdata $ pcon PDNothing)
                    (pdata $ pcon PDNothing)
                    (pdata $ pcon PDNothing)
            )
            perror

psource :: forall s. Term s Raw.PFrame -> Term s (PMaybe Blob.PCekSourceBlobSpanV1) -> Term s PData -> Term s (PPair PData (PMaybe PByteString))
psource control required witness = plet (pfields 3 2 witness) $ \fields ->
    plet (pfield fields 1) $ \window ->
        pcon $ PPair (phead # fields) $ pmatch required $ \case
            PNothing -> pif (window #== pnone) (pcon PNothing) perror
            PJust span -> pmatch span $ \s ->
                pmatch
                    ( Proof.pboundWindowBytesV1
                        # (Raw.pspanWindow # control)
                        # pfromData (Blob.pspan'absoluteStart s)
                        # pfromData (Blob.pspan'length s)
                        # (pasByteStr # (phead # pfields 0 1 window))
                    )
                    $ \case
                        PNothing -> perror
                        PJust bytes -> pcon $ PJust bytes

pnextControl :: forall s. Term s Raw.PFrame -> Term s (PMaybe Traverse.PDataTraverseControlV1) -> Term s (PMaybe Stages.PStepResult)
pnextControl control result = pmatch result $ \case
    PNothing -> pcon PNothing
    PJust next -> pcon $ PJust $ pcon $ Stages.PAdvanced $ Raw.preplace # control # 7 # psome (pencoded $ Traverse.pencodeControlV1 # next)

pnextSimpleControl :: forall s. Term s Raw.PFrame -> Term s (PMaybe Traverse.PDataTraverseControlV1) -> Term s (PMaybe Stages.PStepResult)
pnextSimpleControl control result = pmatch result $ \case
    PNothing -> pcon PNothing
    PJust next -> pmatch next $ \n ->
        pif
            ( pfromData (Traverse.ptraverse'pendingLargeExpectedChildren n)
                #== pcon PDNothing
                #&& pfromData (Traverse.ptraverse'integer n)
                #== pcon PDNothing
                #&& pfromData (Traverse.ptraverse'bytes n)
                #== pcon PDNothing
                #&& pfromData (Traverse.ptraverse'result n)
                #== pcon PDNothing
            )
            ( pnextItems control $
                pcons
                    # pforgetData (Traverse.ptraverse'version n)
                    # ( pcons
                            # pforgetData (Traverse.ptraverse'stage n)
                            # ( pcons
                                    # pforgetData (Traverse.ptraverse'sourceStart n)
                                    # ( pcons
                                            # pforgetData (Traverse.ptraverse'sourceLength n)
                                            # ( pcons
                                                    # pforgetData (Traverse.ptraverse'offset n)
                                                    # ( pcons
                                                            # pforgetData (Traverse.ptraverse'frameRoot n)
                                                            # (pcons # pnone # (pcons # pnone # (pcons # pnone # (pcons # pnone # pnil))))
                                                      )
                                              )
                                      )
                              )
                      )
            )
            perror

pheadTransition :: forall s. Integer -> Term s Raw.PFrame -> Term s PData -> Term s (PMaybe Stages.PStepResult)
pheadTransition kind control witness =
    plet (psimpleControl control Traverse.pstageHead) $ \current -> pmatch current $ \c ->
        plet (pfromData (Traverse.ptraverse'sourceLength c) - pfromData (Traverse.ptraverse'offset c)) $ \remaining ->
            pmatch
                ( psource
                    control
                    ( pcon $
                        PJust $
                            pcon $
                                Blob.PCekSourceBlobSpanV1
                                    (pdata $ pfromData (Traverse.ptraverse'sourceStart c) + pfromData (Traverse.ptraverse'offset c))
                                    (pdata $ pif (remaining #< Traverse.pheadBytes) remaining Traverse.pheadBytes)
                    )
                    witness
                )
                $ \(PPair action sourceBytes) ->
                    pmatch sourceBytes $ \case
                        PNothing -> perror
                        PJust bytes -> case kind of
                            1 -> plet (pfields 1 1 action) $ \fields -> pnextControl control (Traverse.pprevalidatedHeadScalar # current # bytes # pinteger fields 0)
                            2 -> plet (pfields 2 1 action) $ \fields -> pnextSimpleControl control (Traverse.pprevalidatedHeadSequence # current # bytes # pinteger fields 0)
                            3 -> pif (action #== pforgetData (pconstrBuiltin # 3 # pnil)) (pnextSimpleControl control $ Traverse.pprevalidatedHeadMap # current # bytes) perror
                            _ -> plet (pfields 4 2 action) $ \fields -> pnextControl control (Traverse.pprevalidatedHeadLargeConstructor # current # bytes # pinteger fields 0 # pinteger fields 1)

pheadScalar, pheadSequence, pheadMap, pheadLargeConstructor :: forall s. Term s (Raw.PFrame :--> PData :--> PMaybe Stages.PStepResult)
pheadScalar = phoistAcyclic $ plam $ pheadTransition 1
pheadSequence = phoistAcyclic $ plam $ pheadTransition 2
pheadMap = phoistAcyclic $ plam $ pheadTransition 3
pheadLargeConstructor = phoistAcyclic $ plam $ pheadTransition 4

pclose :: forall s. Term s (Raw.PFrame :--> PData :--> PMaybe Stages.PStepResult)
pclose = phoistAcyclic $ plam $ \control witness ->
    plet (psimpleControl control Traverse.pstageClose) $ \current ->
        pmatch (psource control (Traverse.pprevalidatedNextSourceSpan # current) witness) $ \(PPair action bytes) ->
            pif
                (action #== pforgetData (pconstrBuiltin # 0 # pnil))
                (pnextSimpleControl control $ Traverse.pprevalidatedClose # current # bytes # pcon Traverse.PNoAction)
                perror

pfinish :: forall s. Term s (Raw.PFrame :--> PData :--> PMaybe Stages.PStepResult)
pfinish = phoistAcyclic $ plam $ \control witness ->
    plet (pdatumItems control) $ \items ->
        pif
            (pinteger items 1 #== Traverse.pstageTerminal #&& witness #== pforgetData (pconstrBuiltin # 0 # pnil))
            ( pcon $
                PJust $
                    pcon $
                        Stages.PAdvanced $
                            Raw.preplace
                                # control
                                # 1
                                # pint (pif (pinteger (pasList # (Raw.pitem # control # 5)) 19 #== (-1)) Proof.pstageTerminal Proof.pstageReferenceScriptCommitment)
            )
            perror

pscalarPrefix :: forall s. Term s Raw.PFrame -> Term s PInteger -> Term s (PBuiltinList PData)
pscalarPrefix control kind = plet (pdatumItems control) $ \items ->
    plet (pasByteStr # pfield items 5) $ \root ->
        pif
            ( pcheckPrefix items kind
                #&& (root #== pconstant "" #|| plengthBS # root #== 32)
                #&& pfield items 6
                #== pnone
                #&& pfield items 9
                #== pnone
                #&& pif (kind #== Traverse.pstageInteger) (pfield items 8 #== pnone) (kind #== Traverse.pstageBytes #&& pfield items 7 #== pnone)
            )
            items
            perror

pbuildTraverse :: forall s. Term s (PBuiltinList PData) -> Term s (PMaybeData Integer.PCekDataIntegerControlV1) -> Term s (PMaybeData Bytes.PCekDataBytesControlV1) -> Term s Traverse.PDataTraverseControlV1
pbuildTraverse items integer bytes =
    pcon $
        Traverse.PDataTraverseControlV1
            (pdata 1)
            (pdata $ pinteger items 1)
            (pdata $ pinteger items 2)
            (pdata $ pinteger items 3)
            (pdata $ pinteger items 4)
            (pdata $ pasByteStr # pfield items 5)
            (pdata $ pcon PDNothing)
            (pdata integer)
            (pdata bytes)
            (pdata $ pcon PDNothing)

pintegerScalarControl :: forall s. Term s Raw.PFrame -> Term s Traverse.PDataTraverseControlV1
pintegerScalarControl control = plet (pscalarPrefix control Traverse.pstageInteger) $ \items ->
    plet (Integer.pcontrolFromDataV1 # (phead # pfields 0 1 (pfield items 7))) $ \integer -> pmatch integer $ \i ->
        pif
            ( pfromData (Integer.pint'sourceStart i) #== pinteger items 2
                + pinteger items 4
                    #&& pfromData (Integer.pint'sourceStart i)
                + pfromData (Integer.pint'sourceLength i) #<= pinteger items 2
                + pinteger items 3
            )
            (pbuildTraverse items (pcon $ PDJust $ pdata integer) (pcon PDNothing))
            perror

pbytesScalarControl :: forall s. Term s Raw.PFrame -> Term s Traverse.PDataTraverseControlV1
pbytesScalarControl control = plet (pscalarPrefix control Traverse.pstageBytes) $ \items ->
    plet (Bytes.pcontrolFromDataV1 # (phead # pfields 0 1 (pfield items 8))) $ \bytes -> pmatch bytes $ \b ->
        pif
            ( pfromData (Bytes.pbytes'sourceStart b) #== pinteger items 2
                + pinteger items 4
                    #&& pfromData (Bytes.pbytes'sourceStart b)
                + pfromData (Bytes.pbytes'sourceLength b) #<= pinteger items 2
                + pinteger items 3
            )
            (pbuildTraverse items (pcon PDNothing) (pcon $ PDJust $ pdata bytes))
            perror

pscalarClaimData :: forall s. Term s Traverse.PDataTraverseControlV1 -> Term s PData -> Term s PData
pscalarClaimData current scalar = pmatch current $ \c ->
    pforgetData $
        pconstrBuiltin
            # 0
            # ( pcons
                    # pforgetData (Traverse.ptraverse'sourceStart c)
                    # ( pcons
                            # pforgetData (Traverse.ptraverse'sourceLength c)
                            # ( pcons
                                    # pforgetData (Traverse.ptraverse'offset c)
                                    # (pcons # pforgetData (Traverse.ptraverse'frameRoot c) # (pcons # scalar # pnil))
                              )
                      )
              )

pintegerScalarClaimIsExact, pbytesScalarClaimIsExact :: forall s. Term s (Raw.PFrame :--> PData :--> PBool)
pintegerScalarClaimIsExact = phoistAcyclic $ plam $ \control claimed ->
    plet (pintegerScalarControl control) $ \current -> pmatch current $ \c -> pmatch (pfromData $ Traverse.ptraverse'integer c) $ \case
        PDNothing -> perror
        PDJust scalar -> claimed #== pscalarClaimData current (pforgetData scalar)
pbytesScalarClaimIsExact = phoistAcyclic $ plam $ \control claimed ->
    plet (pbytesScalarControl control) $ \current -> pmatch current $ \c -> pmatch (pfromData $ Traverse.ptraverse'bytes c) $ \case
        PDNothing -> perror
        PDJust scalar -> claimed #== pscalarClaimData current (pforgetData scalar)

-- These readers consume claims authenticated by the scalar yield in the same
-- transaction. They mirror the target's shape-only readers, without attaching
-- the sibling family's codec to a stage script.
panyFields :: forall s. Term s PInteger -> Term s PData -> Term s (PBuiltinList PData)
panyFields count dat = plet (psndBuiltin # (pasConstr # dat)) $ \fields -> pif (plength # fields #== count) fields perror

puncheckedBlake :: forall s. Term s PData -> Term s Blake.PBlake2b256TraceControlV1
puncheckedBlake dat = plet (panyFields 9 dat) $ \f ->
    pcon $
        Blake.PBlake2b256TraceControlV1
            (pdata $ pinteger f 0)
            (pdata $ pinteger f 1)
            (pdata $ pinteger f 2)
            (pdata $ pinteger f 3)
            (pdata $ pasByteStr # pfield f 4)
            (pdata $ pasByteStr # pfield f 5)
            (pdata $ pinteger f 6)
            (pdata $ pasByteStr # pfield f 7)
            (pdata $ pinteger f 8)

puncheckedPeaks :: forall s. Term s (PBuiltinList PData) -> Term s (PBuiltinList (PAsData Frontier.PCekBlobFrontierPeakV1))
puncheckedPeaks items =
    pmap
        # plam
            ( \dat -> plet (panyFields 3 dat) $ \f ->
                pdata $
                    pcon $
                        Frontier.PCekBlobFrontierPeakV1
                            (pdata $ pinteger f 0)
                            (pdata $ pasByteStr # pfield f 1)
                            (pdata $ pinteger f 2)
            )
        # items

puncheckedBlob :: forall s. Term s PData -> Term s (PMaybeData Blob.PCekSourceBlobControlV1)
puncheckedBlob dat = pmatch (pasConstr # dat) $ \(PBuiltinPair tag fields) ->
    pif (tag #== 1) (pcon PDNothing) $
        pif
            (plength # fields #== 1)
            ( plet (panyFields 6 (phead # fields)) $ \f -> plet (pfields 0 3 (pfield f 4)) $ \frontier ->
                pmatch (pasConstr # pfield f 5) $ \(PBuiltinPair hashTag hashFields) ->
                    pcon $
                        PDJust $
                            pdata $
                                pcon $
                                    Blob.PCekSourceBlobControlV1
                                        (pdata $ pinteger f 0)
                                        (pdata $ pinteger f 1)
                                        (pdata $ pinteger f 2)
                                        (pdata $ pinteger f 3)
                                        ( pdata $
                                            pcon $
                                                Frontier.PCekBlobFrontierV1
                                                    (pdata $ pinteger frontier 0)
                                                    (pdata $ pinteger frontier 1)
                                                    (pdata $ puncheckedPeaks $ pasList # pfield frontier 2)
                                        )
                                        ( pdata $
                                            pif (hashTag #== 1) (pcon PDNothing) $
                                                pif (plength # hashFields #== 1) (pcon $ PDJust $ pdata $ puncheckedBlake (phead # hashFields)) perror
                                        )
            )
            perror

puncheckedInteger :: forall s. Term s PData -> Term s Integer.PCekDataIntegerControlV1
puncheckedInteger dat = plet (panyFields 6 dat) $ \f ->
    pcon $
        Integer.PCekDataIntegerControlV1
            (pdata $ pinteger f 0)
            (pdata $ pinteger f 1)
            (pdata $ pinteger f 2)
            (pdata $ pinteger f 3)
            (pdata $ pinteger f 4)
            (pdata $ puncheckedBlob $ pfield f 5)

puncheckedBytes :: forall s. Term s PData -> Term s Bytes.PCekDataBytesControlV1
puncheckedBytes dat = plet (panyFields 6 dat) $ \f ->
    pcon $
        Bytes.PCekDataBytesControlV1
            (pdata $ pinteger f 0)
            (pdata $ pinteger f 1)
            (pdata $ pinteger f 2)
            (pdata $ pinteger f 3)
            (pdata $ pinteger f 4)
            (pdata $ puncheckedBlob $ pfield f 5)

pclaimedTraverse :: forall s. Term s PData -> Term s PInteger -> Term s (PMaybeData Integer.PCekDataIntegerControlV1) -> Term s (PMaybeData Bytes.PCekDataBytesControlV1) -> Term s Traverse.PDataTraverseControlV1
pclaimedTraverse claimed stage integer bytes = plet (pfields 0 5 claimed) $ \f ->
    pcon $
        Traverse.PDataTraverseControlV1
            (pdata 1)
            (pdata stage)
            (pdata $ pinteger f 0)
            (pdata $ pinteger f 1)
            (pdata $ pinteger f 2)
            (pdata $ pasByteStr # pfield f 3)
            (pdata $ pcon PDNothing)
            (pdata integer)
            (pdata bytes)
            (pdata $ pcon PDNothing)

pcompletedScalar :: forall s. Term s Raw.PFrame -> Term s (PMaybe Traverse.PDataTraverseControlV1) -> Term s (PMaybe Stages.PStepResult)
pcompletedScalar control result = pmatch result $ \case
    PNothing -> pcon PNothing
    PJust next -> pmatch next $ \n ->
        pif
            ( pfromData (Traverse.ptraverse'pendingLargeExpectedChildren n)
                #== pcon PDNothing
                #&& pfromData (Traverse.ptraverse'integer n)
                #== pcon PDNothing
                #&& pfromData (Traverse.ptraverse'bytes n)
                #== pcon PDNothing
            )
            ( pnextItems control $
                pcons
                    # pforgetData (Traverse.ptraverse'version n)
                    # ( pcons
                            # pforgetData (Traverse.ptraverse'stage n)
                            # ( pcons
                                    # pforgetData (Traverse.ptraverse'sourceStart n)
                                    # ( pcons
                                            # pforgetData (Traverse.ptraverse'sourceLength n)
                                            # ( pcons
                                                    # pforgetData (Traverse.ptraverse'offset n)
                                                    # ( pcons
                                                            # pforgetData (Traverse.ptraverse'frameRoot n)
                                                            # (pcons # pnone # (pcons # pnone # (pcons # pnone # (pcons # pencoded (Traverse.pencodeOptionalSummaryV1 # pfromData (Traverse.ptraverse'result n)) # pnil))))
                                                      )
                                              )
                                      )
                              )
                      )
            )
            perror

pattachInteger, pattachBytes :: forall s. Term s (Raw.PFrame :--> PData :--> PData :--> PMaybe Stages.PStepResult)
pattachInteger = phoistAcyclic $ plam $ \control witness claimed ->
    pif
        (Raw.pinteger # control # 1 #== Proof.pstageDatumTraversal #&& Stages.pfactsEmpty # control)
        ( plet (puncheckedInteger $ pfield (pfields 0 5 claimed) 4) $ \scalar ->
            plet (pclaimedTraverse claimed Traverse.pstageInteger (pcon $ PDJust $ pdata scalar) (pcon PDNothing)) $ \current ->
                plet (pactionFields witness 5 1) $ \fields ->
                    pcompletedScalar control $ Traverse.pprevalidatedAttachInteger # current # pdecodeParent (phead # fields)
        )
        perror
pattachBytes = phoistAcyclic $ plam $ \control witness claimed ->
    pif
        (Raw.pinteger # control # 1 #== Proof.pstageDatumTraversal #&& Stages.pfactsEmpty # control)
        ( plet (puncheckedBytes $ pfield (pfields 0 5 claimed) 4) $ \scalar ->
            plet (pclaimedTraverse claimed Traverse.pstageBytes (pcon PDNothing) (pcon $ PDJust $ pdata scalar)) $ \current ->
                plet (pactionFields witness 5 1) $ \fields ->
                    pcompletedScalar control $ Traverse.pprevalidatedAttachBytes # current # pdecodeParent (phead # fields)
        )
        perror

preplaceScalar :: forall s. Term s Raw.PFrame -> Term s PInteger -> Term s PData -> Term s (PMaybe Stages.PStepResult)
preplaceScalar control index next =
    pnextItems control $
        pupdateItems
            (pasList # (phead # pfields 0 1 (Raw.pitem # control # 7)))
            index
            (psome next)

padvanceInteger, padvanceBytes :: forall s. Term s (Raw.PFrame :--> PData :--> PData :--> PMaybe Stages.PStepResult)
padvanceInteger = phoistAcyclic $ plam $ \control witness claimed ->
    plet (puncheckedInteger $ pfield (pfields 0 5 claimed) 4) $ \scalar -> pmatch scalar $ \s ->
        pif
            (Raw.pinteger # control # 1 #== Proof.pstageDatumTraversal #&& Stages.pfactsEmpty # control #&& pfromData (Integer.pint'stage s) #/= Integer.pstageTerminal)
            ( pmatch (psource control (Integer.pprevalidatedNextSourceSpan # scalar) witness) $ \(PPair action bytes) ->
                pif
                    (action #== pforgetData (pconstrBuiltin # 0 # pnil))
                    ( pmatch (Integer.pprevalidatedStep # scalar # bytes) $ \case
                        PNothing -> pcon PNothing
                        PJust next -> pmatch next $ \n ->
                            pif
                                (Integer.pint'sourceStart n #== Integer.pint'sourceStart s #&& Integer.pint'sourceLength n #== Integer.pint'sourceLength s)
                                (preplaceScalar control 7 $ Integer.pcontrolData # next)
                                perror
                    )
                    perror
            )
            perror
padvanceBytes = phoistAcyclic $ plam $ \control witness claimed ->
    plet (puncheckedBytes $ pfield (pfields 0 5 claimed) 4) $ \scalar -> pmatch scalar $ \s ->
        pif
            (Raw.pinteger # control # 1 #== Proof.pstageDatumTraversal #&& Stages.pfactsEmpty # control #&& pfromData (Bytes.pbytes'stage s) #/= Bytes.pstageTerminal)
            ( pmatch (psource control (Bytes.pprevalidatedNextSourceSpan # scalar) witness) $ \(PPair action bytes) ->
                pif
                    (action #== pforgetData (pconstrBuiltin # 0 # pnil))
                    ( pmatch (Bytes.pprevalidatedStep # scalar # bytes) $ \case
                        PNothing -> pcon PNothing
                        PJust next -> pmatch next $ \n ->
                            pif
                                (Bytes.pbytes'sourceStart n #== Bytes.pbytes'sourceStart s #&& Bytes.pbytes'sourceLength n #== Bytes.pbytes'sourceLength s)
                                (preplaceScalar control 8 $ Bytes.pcontrolData # next)
                                perror
                    )
                    perror
            )
            perror

plargeStageControl :: forall s. Term s Raw.PFrame -> Term s PInteger -> Term s Traverse.PDataTraverseControlV1
plargeStageControl control kind = plet (pdatumItems control) $ \items ->
    plet (pasByteStr # pfield items 5) $ \root ->
        plet (pasInt # (phead # pfields 0 1 (pfield items 6))) $ \children ->
            plet (Integer.pcontrolFromDataV1 # (phead # pfields 0 1 (pfield items 7))) $ \integer -> pmatch integer $ \i ->
                pif
                    ( pcheckPrefix items kind
                        #&& (root #== pconstant "" #|| plengthBS # root #== 32)
                        #&& children
                        #>= 0
                        #&& children
                        #<= 4294967295
                        #&& pfield items 8
                        #== pnone
                        #&& pfield items 9
                        #== pnone
                        #&& pif
                            (kind #== Traverse.pstageLargeConstructor)
                            ( pfromData (Integer.pint'sourceStart i) #== pinteger items 2
                                + pinteger items 4
                                    #&& pinteger items 4
                                + pfromData (Integer.pint'sourceLength i) #< pinteger items 3
                            )
                            ( pfromData (Integer.pint'stage i)
                                #== Integer.pstageTerminal
                                #&& pfromData (Integer.pint'sourceStart i)
                                + pfromData (Integer.pint'sourceLength i) #== pinteger items 2
                                + pinteger items 4
                                    #&& pinteger items 4
                                    #< pinteger items 3
                            )
                        #&& pfromData (Integer.pint'sourceStart i)
                        #>= pinteger items 2
                        #&& pfromData (Integer.pint'sourceStart i)
                        + pfromData (Integer.pint'sourceLength i) #<= pinteger items 2
                        + pinteger items 3
                    )
                    ( pcon $
                        Traverse.PDataTraverseControlV1
                            (pdata 1)
                            (pdata kind)
                            (pdata $ pinteger items 2)
                            (pdata $ pinteger items 3)
                            (pdata $ pinteger items 4)
                            (pdata root)
                            (pdata $ pcon $ PDJust $ pdata children)
                            (pdata $ pcon $ PDJust $ pdata integer)
                            (pdata $ pcon PDNothing)
                            (pdata $ pcon PDNothing)
                    )
                    perror

padvanceLargeConstructor :: forall s. Term s (Raw.PFrame :--> PData :--> PMaybe Stages.PStepResult)
padvanceLargeConstructor = phoistAcyclic $ plam $ \control witness ->
    plet (plargeStageControl control Traverse.pstageLargeConstructor) $ \current -> pmatch current $ \c ->
        pmatch (pfromData $ Traverse.ptraverse'integer c) $ \case
            PDNothing -> perror
            PDJust int -> plet (pfromData int) $ \integer -> pmatch integer $ \i ->
                pif
                    (pfromData (Integer.pint'stage i) #== Integer.pstageTerminal)
                    ( pmatch (psource control (pcon PNothing) witness) $ \(PPair action bytes) ->
                        pif
                            (action #== pforgetData (pconstrBuiltin # 0 # pnil) #&& bytes #== pcon PNothing)
                            ( pnextItems control $
                                pupdateItems (pupdateItems (pdatumItems control) 1 $ pint Traverse.pstageLargeFields) 4 $
                                    pint (pfromData (Traverse.ptraverse'offset c) + pfromData (Integer.pint'sourceLength i))
                            )
                            perror
                    )
                    $ pmatch (psource control (Integer.pprevalidatedNextSourceSpan # integer) witness)
                    $ \(PPair action bytes) ->
                        pif
                            (action #== pforgetData (pconstrBuiltin # 0 # pnil))
                            ( pif
                                ( pif
                                    (pfromData (Integer.pint'stage i) #== Integer.pstageSyntax)
                                    ( pmatch bytes $ \case
                                        PNothing -> pconstant False
                                        PJust window -> Integer.pparseLargeConstructorSyntaxV1 # window # pfromData (Integer.pint'sourceLength i) #/= pcon PNothing
                                    )
                                    (pconstant True)
                                )
                                ( pmatch (Integer.pprevalidatedStep # integer # bytes) $ \case
                                    PNothing -> pcon PNothing
                                    PJust next -> pmatch next $ \n ->
                                        pif
                                            (Integer.pint'sourceStart n #== Integer.pint'sourceStart i #&& Integer.pint'sourceLength n #== Integer.pint'sourceLength i)
                                            (preplaceScalar control 7 $ Integer.pcontrolData # next)
                                            perror
                                )
                                (pcon PNothing)
                            )
                            perror

padvanceLargeFields :: forall s. Term s (Raw.PFrame :--> PData :--> PMaybe Stages.PStepResult)
padvanceLargeFields = phoistAcyclic $ plam $ \control witness ->
    plet (plargeStageControl control Traverse.pstageLargeFields) $ \current -> pmatch current $ \c ->
        pmatch (pfromData $ Traverse.ptraverse'integer c) $ \case
            PDNothing -> perror
            PDJust integer -> pmatch (pfromData integer) $ \i ->
                pmatch (pfromData $ Traverse.ptraverse'pendingLargeExpectedChildren c) $ \case
                    PDNothing -> perror
                    PDJust children -> pmatch (pfromData $ Integer.pint'blob i) $ \case
                        PDNothing -> perror
                        PDJust blob ->
                            pmatch
                                ( psource
                                    control
                                    ( pcon $
                                        PJust $
                                            pcon $
                                                Blob.PCekSourceBlobSpanV1
                                                    (pdata $ pfromData (Traverse.ptraverse'sourceStart c) + pfromData (Traverse.ptraverse'offset c))
                                                    (pdata 1)
                                    )
                                    witness
                                )
                                $ \(PPair action bytes) ->
                                    pif
                                        (action #== pforgetData (pconstrBuiltin # 0 # pnil))
                                        ( pmatch bytes $ \case
                                            PNothing -> pcon PNothing
                                            PJust window -> pmatch (Blob.pfinalizeV1 # pfromData blob) $ \case
                                                PNothing -> pcon PNothing
                                                PJust root ->
                                                    pif
                                                        (pbyteAt # window # 0 #== pif (pfromData children #== 0) 128 159)
                                                        ( plet
                                                            ( Frame.pinitialLargeConstrFrameV1
                                                                # root
                                                                # pfromData (Integer.pint'sourceLength i)
                                                                # pfromData (Integer.pint'memory i)
                                                                # pfromData (Traverse.ptraverse'frameRoot c)
                                                                # pfromData children
                                                            )
                                                            $ \opened ->
                                                                plet (pif (pfromData children #== 0) Traverse.pstageFold Traverse.pstageHead) $ \stage ->
                                                                    pif (stage #== Traverse.pstageHead #&& pfromData (Traverse.ptraverse'offset c) + 1 #>= pfromData (Traverse.ptraverse'sourceLength c)) (pcon PNothing) $
                                                                        pnextSimpleControl control $
                                                                            pcon $
                                                                                PJust $
                                                                                    pcon
                                                                                        c
                                                                                            { Traverse.ptraverse'stage = pdata stage
                                                                                            , Traverse.ptraverse'offset = pdata $ pfromData (Traverse.ptraverse'offset c) + 1
                                                                                            , Traverse.ptraverse'frameRoot = pdata $ Frame.phashFrameV1 # opened
                                                                                            , Traverse.ptraverse'pendingLargeExpectedChildren = pdata $ pcon PDNothing
                                                                                            , Traverse.ptraverse'integer = pdata $ pcon PDNothing
                                                                                            }
                                                        )
                                                        (pcon PNothing)
                                        )
                                        perror

pdecodeSummary :: forall s. Term s PData -> Term s Summary.PDataSummaryV1
pdecodeSummary dat = plet (pfields 0 3 dat) $ \f ->
    pcon $ Summary.PDataSummaryV1 (pdata $ pasByteStr # pfield f 0) (pdata $ pinteger f 1) (pdata $ pinteger f 2)

pdecodeSequence :: forall s. Term s PData -> Term s Summary.PDataSequenceSummaryV1
pdecodeSequence dat = plet (pfields 0 4 dat) $ \f ->
    pcon $ Summary.PDataSequenceSummaryV1 (pdata $ pasByteStr # pfield f 0) (pdata $ pinteger f 1) (pdata $ pinteger f 2) (pdata $ pinteger f 3)

pdecodeFrame :: forall s. Term s PData -> Term s Frame.PDataFrameV1
pdecodeFrame dat = plet (pfields 0 11 dat) $ \f ->
    pcon $
        Frame.PDataFrameV1
            (pdata $ pinteger f 0)
            (pdata $ pinteger f 1)
            (pdata $ pasByteStr # pfield f 2)
            (pdata $ pinteger f 3)
            (pdata $ pinteger f 4)
            (pdata $ pasByteStr # pfield f 5)
            (pdata $ pinteger f 6)
            (pdata $ pinteger f 7)
            ( pdata $
                pmap
                    # plam
                        ( \peak -> plet (pfields 0 2 peak) $ \pf ->
                            pdata $ pcon $ Merkle.PFrontierPeak (pdata $ pinteger pf 0) (pdata $ pasByteStr # pfield pf 1)
                        )
                    # (pasList # pfield f 8)
            )
            (pdata $ pinteger f 9)
            (pdata $ pdecodeSequence $ pfield f 10)

pdecodeParent :: forall s. Term s PData -> Term s (PMaybeData Frame.PDataFrameV1)
pdecodeParent dat = pmatch (pasConstr # dat) $ \(PBuiltinPair tag fields) ->
    pif (tag #== 1) (pif (pnull # fields) (pcon PDNothing) perror) $
        pif (tag #== 0 #&& plength # fields #== 1) (pcon $ PDJust $ pdata $ pdecodeFrame $ phead # fields) perror

pdecodePath :: forall s. Term s PData -> Term s (PBuiltinList (PAsData PByteString))
pdecodePath dat = pmap # plam (\item -> pdata $ pasByteStr # item) # (pasList # dat)
