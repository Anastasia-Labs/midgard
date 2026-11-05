{-# LANGUAGE OverloadedStrings #-}

-- | Claims read from the unique authenticated output-proof dispatcher spend.
module Midgard.LedgerOutputProofYield (
    PClaim (..),
    pdispatch,
    pdispatchAttestation,
    pattest,
    pspanAttachWitness,
    pwindowWitness,
    pchunkWitness,
    pvalueWitness,
    pnativeWitness,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Midgard.BoundedItem qualified as Bounded
import Midgard.CekData qualified as Summary
import Midgard.Common.Utils (pheadSingleton)
import Midgard.LedgerOutputProof qualified as Proof
import Midgard.LedgerOutputProofRaw qualified as Raw
import Midgard.LedgerOutputProofStages qualified as Stages
import Midgard.LedgerOutputValue qualified as Value
import Midgard.NativeScriptScan qualified as Native
import Midgard.RejectionReason qualified as Rejection
import Midgard.ValidationMachine (pencodeTerminalRejectionWitness)
import Midgard.ValidationMerkle qualified as Merkle
import Midgard.ValidationTrace qualified as Trace
import Plutarch.LedgerApi.V3
import Plutarch.Monadic qualified as P
import Plutarch.Prelude

-- Internal claim, never a wire record.
data PClaim s = PClaim
    { pclaim'preProgramCounter :: Term s PInteger
    , pclaim'prePriorLedgerRoot :: Term s PByteString
    , pclaim'claimedSuccessor :: Term s PData
    , pclaim'witness :: Term s PData
    , pclaim'control :: Term s Raw.PFrame
    , pclaim'nextControlCbor :: Term s PByteString
    , pclaim'claimedScalar :: Term s PData
    }
    deriving stock (Generic)
    deriving anyclass (SOP.Generic)
    deriving (PlutusType) via (DeriveAsSOPStruct PClaim)

pfields :: forall s. Term s PInteger -> Term s PData -> Term s (PBuiltinList PData)
pfields count dat = pmatch (pasConstr # dat) $ \(PBuiltinPair _ fields) -> pif (plength # fields #== count) fields perror

ptagged :: forall s. Term s PInteger -> Term s PInteger -> Term s PData -> Term s (PBuiltinList PData)
ptagged tag count dat = pmatch (pasConstr # dat) $ \(PBuiltinPair actual fields) -> pif (actual #== tag #&& plength # fields #== count) fields perror

pat :: forall s. Term s (PBuiltinList PData) -> Term s PInteger -> Term s PData
pat fields index = pelemAt # index # fields

pdispatchAction :: forall s. Term s (PBuiltinList (PAsData PScriptHash) :--> PBuiltinList (PAsData PTxInInfo) :--> PBuiltinList (PBuiltinPair (PAsData PScriptPurpose) (PAsData PRedeemer)) :--> PPair PClaim PInteger)
pdispatchAction = phoistAcyclic $ plam $ \dispatchers inputs redeemers -> P.do
    PTxInInfo{ptxInInfo'outRef, ptxInInfo'resolved} <-
        pmatch $
            pfromData $
                pheadSingleton
                    # ( pfilter
                            # plam
                                ( \input -> P.do
                                    PTxInInfo{ptxInInfo'resolved = resolved} <- pmatch $ pfromData input
                                    PTxOut{ptxOut'address} <- pmatch resolved
                                    PAddress credential _ <- pmatch ptxOut'address
                                    pmatch credential $ \case
                                        PScriptCredential hash -> pelem # hash # dispatchers
                                        _ -> pconstant False
                                )
                            # inputs
                      )
    PTxOut{ptxOut'datum} <- pmatch ptxInInfo'resolved
    datum <- plet $ pmatch ptxOut'datum $ \case POutputDatum dat -> pto dat; _ -> perror
    PBuiltinPair _ redeemer <-
        pmatch $
            pheadSingleton
                # (pfilter # plam (\pair -> pfromData (pfstBuiltin # pair) #== pcon (PSpending ptxInInfo'outRef)) # redeemers)
    action <- plet $ ptagged 0 9 $ phead # ptagged 1 1 (pto $ pfromData redeemer)
    transition <- plet $ pfields 2 $ pat action 2
    stepDatum <- plet $ pfields 2 datum
    prepared <- plet $ pfields 3 $ phead # ptagged 0 1 (pat stepDatum 1)
    resolution <- plet $ pfields 4 $ pat prepared 1
    pre <- plet $ pfields 15 $ pat resolution 1
    pcon $
        PPair
            ( pcon $
                PClaim
                    (pasInt # pat pre 8)
                    (pasByteStr # pat pre 6)
                    (pat transition 1)
                    (pat action 3)
                    (Raw.popen # (pasByteStr # pat action 4))
                    (pasByteStr # pat action 5)
                    (pat action 6)
            )
            (pasInt # pat action 7)

pdispatch :: forall s. Term s (PBuiltinList (PAsData PScriptHash) :--> PInteger :--> PBuiltinList (PAsData PTxInInfo) :--> PBuiltinList (PBuiltinPair (PAsData PScriptPurpose) (PAsData PRedeemer)) :--> PClaim)
pdispatch = phoistAcyclic $ plam $ \dispatchers expected inputs redeemers ->
    pmatch (pdispatchAction # dispatchers # inputs # redeemers) $ \(PPair claim role) -> pif (role #== expected) claim perror

pdispatchAttestation :: forall s. Term s (PBuiltinList (PAsData PScriptHash) :--> PBuiltinList (PAsData PTxInInfo) :--> PBuiltinList (PBuiltinPair (PAsData PScriptPurpose) (PAsData PRedeemer)) :--> PClaim)
pdispatchAttestation = phoistAcyclic $ plam $ \dispatchers inputs redeemers ->
    pmatch (pdispatchAction # dispatchers # inputs # redeemers) $ \(PPair claim _) -> claim

prejectedSuccessorIsExact :: forall s. Term s (PClaim :--> PByteString :--> PBool)
prejectedSuccessorIsExact = phoistAcyclic $ plam $ \claim code -> pmatch claim $ \c -> plet (pfields 15 $ pclaim'claimedSuccessor c) $ \fields ->
    pat fields 7
        #== pforgetData (pconstrBuiltin # 14 # pnil)
        #&& pat fields 12
        #== pforgetData (pconstrBuiltin # 2 # pnil)
        #&& pasByteStr
        # pat fields 13
        #== Trace.phashRejectionCode
        # code
        #&& pasByteStr
        # pat fields 9
        #== Trace.phashWorkWitness
        # pcon Trace.PTerminal
        # (pclaim'preProgramCounter c + 1)
        # (pencodeTerminalRejectionWitness # code # pclaim'prePriorLedgerRoot c)

pattest :: forall s. Term s (PClaim :--> PMaybe Stages.PStepResult :--> PBool)
pattest = phoistAcyclic $ plam $ \claim result -> pmatch claim $ \c -> pmatch result $ \case
    PNothing -> pconstant False
    PJust step -> pmatch step $ \case
        Stages.PAdvanced next -> pnot # (pclaim'nextControlCbor c #== pconstant "") #&& Raw.pencode # next #== pclaim'nextControlCbor c
        Stages.PInvalidOutput -> rejected claim c Rejection.prejectInvalidOutput
        Stages.PInvalidReferenceScript -> rejected claim c Rejection.prejectInvalidFieldType
        Stages.PNativeScriptNodeLimit -> rejected claim c Rejection.prejectNativeScriptNodeCount
        Stages.PNativeScriptDepthLimit -> rejected claim c Rejection.prejectNativeScriptDepth
  where
    rejected claim c code = pclaim'nextControlCbor c #== pconstant "" #&& prejectedSuccessorIsExact # claim # code

poptional :: forall s a. (PIsData a) => (Term s PData -> Term s a) -> Term s PData -> Term s (PMaybeData a)
poptional decode dat = pmatch (pasConstr # dat) $ \(PBuiltinPair tag fields) ->
    pif (tag #== 1 #&& pnull # fields) (pcon PDNothing) $
        pif (tag #== 0 #&& plength # fields #== 1) (pcon $ PDJust $ pdata $ decode $ phead # fields) perror

pbyteList :: forall s. Term s PData -> Term s (PBuiltinList (PAsData PByteString))
pbyteList dat = pmap # plam (\item -> pdata $ pasByteStr # item) # (pasList # dat)

pchunk :: forall s. Term s PData -> Term s Bounded.PChunkProofV1
pchunk dat = plet (ptagged 0 8 dat) $ \f ->
    pcon $
        Bounded.PChunkProofV1
            (pdata $ pasInt # pat f 0)
            (pdata $ pasInt # pat f 1)
            (pdata $ pasInt # pat f 2)
            (pdata $ pasInt # pat f 3)
            (pdata $ pasInt # pat f 4)
            (pdata $ pasByteStr # pat f 5)
            (pdata $ pmap # plam (\peak -> plet (ptagged 0 2 peak) $ \p -> pdata $ pcon $ Merkle.PFrontierPeak (pdata $ pasInt # pat p 0) (pdata $ pasByteStr # pat p 1)) # (pasList # pat f 6))
            (pdata $ pbyteList $ pat f 7)

pspanAttachWitness :: forall s. Term s (PData :--> Proof.PLedgerOutputProofWitnessV1)
pspanAttachWitness = phoistAcyclic $ plam $ \dat -> plet (ptagged 5 4 dat) $ \f ->
    pcon $ Proof.PLedgerOutputProofSpanAttach (pdata $ pasInt # pat f 0) (pdata $ pasInt # pat f 1) (pdata $ pchunk $ pat f 2) (pdata $ poptional pchunk $ pat f 3)

pwindowWitness :: forall s. Term s (PData :--> Proof.PLedgerOutputProofWitnessV1)
pwindowWitness = phoistAcyclic $ plam $ \dat -> pmatch (pasConstr # dat) $ \(PBuiltinPair tag fields) ->
    pif (tag #== 0) (pif (pnull # fields) (pcon Proof.PLedgerOutputProofNoWitness) perror) $
        pif (tag #== 6 #&& plength # fields #== 1) (pcon $ Proof.PLedgerOutputProofWindow $ pdata $ pasByteStr # (phead # fields)) perror

pchunkWitness :: forall s. Term s (PData :--> Proof.PLedgerOutputProofWitnessV1)
pchunkWitness = phoistAcyclic $ plam $ \dat -> pmatch (pasConstr # dat) $ \(PBuiltinPair tag fields) ->
    pif (tag #== 0) (pif (pnull # fields) (pcon Proof.PLedgerOutputProofNoWitness) perror) $
        pif
            (tag #== 1 #&& plength # fields #== 2)
            (pcon $ Proof.PLedgerOutputProofChunks (pdata $ pchunk $ pat fields 0) (pdata $ poptional pchunk $ pat fields 1))
            perror

pvalueHead :: forall s. Term s PData -> Term s Value.PLedgerOutputValueHeadV1
pvalueHead dat = plet (ptagged 0 3 dat) $ \f -> plet (ptagged 0 4 $ pat f 2) $ \s ->
    pcon $
        Value.PLedgerOutputValueHeadV1
            (pdata $ pasByteStr # pat f 0)
            (pdata $ pasInt # pat f 1)
            (pdata $ pcon $ Summary.PDataSequenceSummaryV1 (pdata $ pasByteStr # pat s 0) (pdata $ pasInt # pat s 1) (pdata $ pasInt # pat s 2) (pdata $ pasInt # pat s 3))

pvalueWitness :: forall s. Term s (PData :--> Proof.PLedgerOutputProofWitnessV1)
pvalueWitness = phoistAcyclic $ plam $ \dat -> pmatch (pasConstr # dat) $ \(PBuiltinPair tag fields) ->
    pif (tag #== 0) (pif (pnull # fields) (pcon Proof.PLedgerOutputProofNoWitness) perror) $
        pif
            (tag #== 2 #&& plength # fields #== 6)
            ( pcon $
                Proof.PLedgerOutputProofValue
                    (pdata $ pasInt # pat fields 0)
                    (pdata $ pasByteStr # pat fields 1)
                    (pdata $ pasByteStr # pat fields 2)
                    (pdata $ pasInt # pat fields 3)
                    (pdata $ pbyteList $ pat fields 4)
                    (poptional pvalueHead $ pat fields 5)
            )
            perror

pnativeWitness :: forall s. Term s (PData :--> Proof.PLedgerOutputProofWitnessV1)
pnativeWitness = phoistAcyclic $ plam $ \dat -> pmatch (pasConstr # dat) $ \(PBuiltinPair tag _) ->
    pif
        (tag #== 4)
        ( plet (ptagged 0 6 $ phead # ptagged 4 1 dat) $ \f ->
            pcon $
                Proof.PLedgerOutputProofNativeFrame $
                    pdata $
                        pcon $
                            Native.PNativeScriptFrameV1
                                (pdata $ pasByteStr # pat f 0)
                                (pdata $ pasInt # pat f 1)
                                (pdata $ pasInt # pat f 2)
                                (pdata $ pasInt # pat f 3)
                                (pdata $ pasInt # pat f 4)
                                (pdata $ pasInt # pat f 5)
        )
        (pchunkWitness # dat)
