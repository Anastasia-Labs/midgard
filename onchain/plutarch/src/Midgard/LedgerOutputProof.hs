{-# LANGUAGE OverloadedStrings #-}

module Midgard.LedgerOutputProof (
  PLedgerOutputSpanWindowV1 (..),
  PLedgerOutputProofControlV1 (..),
  PLedgerOutputProofWitnessV1 (..),
  PLedgerOutputProofStepResultV1 (..),
  pversion,
  poutputFieldIndex,
  pstageStructure,
  pstageValueFold,
  pstageDatumTraversal,
  pstageReferenceScriptCommitment,
  pstageScriptHash,
  pstageNativeScript,
  pstageTerminal,
  pcontrolIsWellFormed,
  pinitialControlV1,
  pencodeControlV1,
  pdecodeControlV1,
  pstepV1,
  pvalueSummaryV1,
  pcardanoTxOutSummaryV1,
  pmidgardTxOutSummaryV1,
  pcardanoSpendDatumSummaryV1,
  preferenceScriptItemCommitmentV1,
  pterminalIsExactV1,
  preferenceScriptDigestV1,
  pdescriptorIsExactV1,
  pencodeInitialControlV1,
  pauthenticatedChunkWindowForOutput,
  pauthenticatedOutputSpanForOutput,
  pboundWindowBytesV1,
  pfactCommitmentV1,
  pfactV1,
  pwithFactV1,
  pfactsCompleteV1,
  pterminalClaimedSummariesV1,
  pfactAttachGroupsV1,
  pfactAttachV1,
  pfactsAreExactV1,
  pstructureStep,
  pvalueFoldStep,
  pdatumStep,
  pdatumHeadStep,
  pdatumFoldStep,
  preferenceCommitmentStep,
  phashStep,
  pnativeStep,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Builtin.Crypto (pblake2b_256)
import Plutarch.Core.Utils (pand'List, (#/=))
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.Prelude

import Aiken.Cbor (pdeserialise)
import Midgard.Blake2b224Trace qualified as Blake
import Midgard.BoundedItem (PChunkProofV1 (..))
import Midgard.BoundedItem qualified as Bounded
import Midgard.CekData (PDataSummaryV1)
import Midgard.CekData qualified as Data
import Midgard.CekDataTraverse (PDataTraverseActionV1, PDataTraverseControlV1)
import Midgard.CekDataTraverse qualified as Traverse
import Midgard.CekSourceBlob qualified as Blob
import Midgard.FraudProofs.NativeTx.Codec (pcborInt, pencodeDefiniteArrayHeader, pencodeDefiniteBytes)
import Midgard.FraudProofs.NativeTx.Types (PMidgardAddress (..), PMidgardCredential (..))
import Midgard.LedgerOutput (pdecodeCanonicalAddressBytes)
import Midgard.LedgerOutputCommitment (PLedgerOutputCommitmentV1 (..), pledgerOutputCommitmentVersion)
import Midgard.LedgerOutputScan (PLedgerOutputScanControlV1)
import Midgard.LedgerOutputScan qualified as Scan
import Midgard.LedgerOutputValue (PLedgerOutputValueControlV1)
import Midgard.LedgerOutputValue qualified as Value
import Midgard.NativeScriptScan (PNativeScriptFrameV1, PNativeScriptStructureControlV1)
import Midgard.NativeScriptScan qualified as Native
import Midgard.ValidationMerkle (PFrontierPeak (..), pappendLeaf, pemptyFrontier, pencodeFrontier, pfrontierCommitment, pfrontierIsWellFormed)

pversion
  , poutputFieldIndex
  , pstageStructure
  , pstageValueFold
  , pstageDatumTraversal
  , pstageReferenceScriptCommitment
  , pstageScriptHash
  , pstageNativeScript
  , pstageTerminal ::
    forall s. Term s PInteger
pversion = 1
poutputFieldIndex = 2
pstageStructure = 0
pstageValueFold = 1
pstageDatumTraversal = 2
pstageReferenceScriptCommitment = 3
pstageScriptHash = 4
pstageNativeScript = 5
pstageTerminal = 6

data PLedgerOutputSpanWindowV1 s = PLedgerOutputSpanWindowV1
  { pwindow'start :: Term s (PAsData PInteger)
  , pwindow'length :: Term s (PAsData PInteger)
  , pwindow'digest :: Term s (PAsData PByteString)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PLedgerOutputSpanWindowV1)

data PLedgerOutputProofControlV1 s = PLedgerOutputProofControlV1
  { pproof'version :: Term s (PAsData PInteger)
  , pproof'stage :: Term s (PAsData PInteger)
  , pproof'outputIndex :: Term s (PAsData PInteger)
  , pproof'totalLength :: Term s (PAsData PInteger)
  , pproof'itemCommitment :: Term s (PAsData PByteString)
  , pproof'outputScan :: Term s (PAsData PLedgerOutputScanControlV1)
  , pproof'value :: Term s (PAsData (PMaybeData PLedgerOutputValueControlV1))
  , pproof'datum :: Term s (PAsData (PMaybeData PDataTraverseControlV1))
  , pproof'referenceScriptCount :: Term s (PAsData PInteger)
  , pproof'referenceScriptPeaks :: Term s (PAsData (PBuiltinList (PAsData PFrontierPeak)))
  , pproof'scriptHash :: Term s (PAsData (PMaybeData Blake.PBlake2b224TraceControlV1))
  , pproof'nativeScript :: Term s (PAsData (PMaybeData PNativeScriptStructureControlV1))
  , pproof'spanWindow :: Term s (PAsData (PMaybeData PLedgerOutputSpanWindowV1))
  , pproof'scanFactsFact :: Term s (PAsData (PMaybeData PByteString))
  , pproof'referenceScriptFact :: Term s (PAsData (PMaybeData PByteString))
  , pproof'datumSummaryFact :: Term s (PAsData (PMaybeData PByteString))
  , pproof'valueSummaryFact :: Term s (PAsData (PMaybeData PByteString))
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PLedgerOutputProofControlV1)

data PLedgerOutputProofWitnessV1 s
  = PLedgerOutputProofNoWitness
  | PLedgerOutputProofChunks (Term s (PAsData PChunkProofV1)) (Term s (PAsData (PMaybeData PChunkProofV1)))
  | PLedgerOutputProofValue
      (Term s (PAsData PInteger))
      (Term s (PAsData PByteString))
      (Term s (PAsData PByteString))
      (Term s (PAsData PInteger))
      (Term s (PAsData (PBuiltinList (PAsData PByteString))))
      (Term s (PMaybeData Value.PLedgerOutputValueHeadV1))
  | PLedgerOutputProofDatum (Term s (PAsData PDataTraverseActionV1)) (Term s (PAsData (PMaybeData PByteString)))
  | PLedgerOutputProofNativeFrame (Term s (PAsData PNativeScriptFrameV1))
  | PLedgerOutputProofSpanAttach
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PChunkProofV1))
      (Term s (PAsData (PMaybeData PChunkProofV1)))
  | PLedgerOutputProofWindow (Term s (PAsData PByteString))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PLedgerOutputProofWitnessV1)

data PLedgerOutputProofStepResultV1 s
  = PLedgerOutputProofAdvanced (Term s (PAsData PLedgerOutputProofControlV1))
  | PLedgerOutputProofInvalidOutput
  | PLedgerOutputProofInvalidReferenceScript
  | PLedgerOutputProofNativeScriptNodeLimit
  | PLedgerOutputProofNativeScriptDepthLimit
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PLedgerOutputProofStepResultV1)

pjustIs :: forall s a. (PIsData a) => (Term s a -> Term s PBool) -> Term s (PMaybeData a) -> Term s PBool
pjustIs predicate value = pmatch value $ \case PDNothing -> pconstant False; PDJust exact -> predicate (pfromData exact)

pcontrolIsWellFormed :: forall s. Term s (PLedgerOutputProofControlV1 :--> PBool)
pcontrolIsWellFormed = phoistAcyclic $ plam $ \control -> pmatch control $ \c ->
  plet (pfromData $ pproof'stage c) $ \stage ->
    plet (pfromData $ pproof'totalLength c) $ \totalLength ->
      plet (pfromData $ pproof'outputScan c) $ \scan -> pmatch scan $ \s ->
        plet (pfromData $ Scan.pscan'referenceScriptLanguage s) $ \language ->
          plet (totalLength - pfromData (Scan.pscan'referenceScriptItemOffset s)) $ \referenceItemLength ->
            plet (Bounded.pchunkCount # referenceItemLength) $ \referenceChunkCount ->
              plet (pfromData $ pproof'value c) $ \value ->
                plet (pfromData $ pproof'datum c) $ \datum ->
                  plet (pfromData $ pproof'scriptHash c) $ \scriptHash ->
                    plet (pfromData $ pproof'nativeScript c) $ \native ->
                      plet (Scan.pterminalIsExactV1 # scan # totalLength) $ \scanTerminal ->
                        plet (pjustIs (\v -> Value.pcontrolIsWellFormed # v #&& pmatch v (\vf -> pfromData (Value.pvalueControl'assetRemaining vf) #<= pfromData (Scan.pscan'assetCount s))) value) $ \valueWellFormed ->
                          plet (pjustIs (\v -> valueWellFormed #&& pnot # (Value.pfinalizeV1 # v #== pcon PNothing)) value) $ \valueTerminal ->
                            plet (pfromData (Scan.pscan'datumOffset s) #/= -1) $ \datumPresent ->
                              plet
                                ( pjustIs
                                    ( \d ->
                                        Traverse.pcontrolIsWellFormed
                                          # d
                                          #&& pmatch
                                            d
                                            ( \df ->
                                                pfromData (Traverse.ptraverse'sourceStart df)
                                                  #== pfromData (Scan.pscan'datumOffset s)
                                                  #&& pfromData (Traverse.ptraverse'sourceLength df)
                                                  #== pfromData (Scan.pscan'datumLength s)
                                            )
                                    )
                                    datum
                                )
                                $ \datumWellFormed ->
                                  plet
                                    ( pjustIs
                                        ( \d ->
                                            datumWellFormed
                                              #&& pmatch d (\df -> pfromData (Traverse.ptraverse'stage df) #== Traverse.pstageTerminal)
                                              #&& pnot
                                              # (Traverse.pfinalizeV1 # d #== pcon PNothing)
                                        )
                                        datum
                                    )
                                    $ \datumTerminal ->
                                      plet (pif datumPresent datumTerminal (datum #== pcon PDNothing)) $ \datumComplete ->
                                        plet
                                          ( pjustIs
                                              ( \h ->
                                                  Blake.pcontrolIsWellFormed
                                                    # h
                                                    #&& pmatch
                                                      h
                                                      ( \hf ->
                                                          pfromData (Blake.pctl'totalLength hf) #== pfromData (Scan.pscan'referenceScriptLength s) + 1
                                                      )
                                              )
                                              scriptHash
                                          )
                                          $ \hashWellFormed ->
                                            plet (pjustIs (\h -> hashWellFormed #&& pnot # (Blake.pdigestV1 # h #== pcon PNothing)) scriptHash) $ \hashTerminal ->
                                              plet
                                                ( pjustIs
                                                    ( \n ->
                                                        Native.pstructureControlIsWellFormedV1
                                                          # n
                                                          #&& pmatch
                                                            n
                                                            ( \nf ->
                                                                pfromData (Native.pstructure'startOffset nf)
                                                                  #== pfromData (Scan.pscan'referenceScriptOffset s)
                                                                  #&& pfromData (Native.pstructure'endOffset nf)
                                                                  #== pfromData (Scan.pscan'referenceScriptOffset s)
                                                                  + pfromData (Scan.pscan'referenceScriptLength s)
                                                            )
                                                    )
                                                    native
                                                )
                                                $ \nativeWellFormed ->
                                                  plet (pjustIs (\n -> nativeWellFormed #&& Native.pstructureTerminalIsExactV1 # n) native) $ \nativeTerminal ->
                                                    plet
                                                      ( language
                                                          #/= -1
                                                          #&& referenceItemLength
                                                          #> 0
                                                          #&& pfromData (pproof'referenceScriptCount c)
                                                          #== referenceChunkCount
                                                          #&& pfrontierIsWellFormed
                                                          # pfromData (pproof'referenceScriptCount c)
                                                          # pfromData (pproof'referenceScriptPeaks c)
                                                      )
                                                      $ \frontierComplete ->
                                                        pand'List
                                                          [ pfromData (pproof'version c) #== pversion
                                                          , stage #>= pstageStructure
                                                          , stage #<= pstageTerminal
                                                          , pfromData (pproof'outputIndex c) #>= 0
                                                          , totalLength #> 0
                                                          , plengthBS # pfromData (pproof'itemCommitment c) #== 32
                                                          , pmatch (pfromData $ pproof'spanWindow c) $ \case
                                                              PDNothing -> pconstant True
                                                              PDJust window -> pmatch (pfromData window) $ \w ->
                                                                stage
                                                                  #>= pstageDatumTraversal
                                                                  #&& pfromData (pwindow'start w)
                                                                  #>= 0
                                                                  #&& pfromData (pwindow'length w)
                                                                  #> 0
                                                                  #&& pfromData (pwindow'length w)
                                                                  #<= Bounded.pchunkBytes
                                                                  #&& pfromData (pwindow'start w)
                                                                  + pfromData (pwindow'length w)
                                                                    #<= totalLength
                                                                    #&& plengthBS
                                                                    # pfromData (pwindow'digest w)
                                                                    #== 32
                                                          , pfactIsWellFormed stage (pfromData $ pproof'scanFactsFact c)
                                                          , pfactIsWellFormed stage (pfromData $ pproof'referenceScriptFact c)
                                                          , pfactIsWellFormed stage (pfromData $ pproof'datumSummaryFact c)
                                                          , pfactIsWellFormed stage (pfromData $ pproof'valueSummaryFact c)
                                                          , Scan.pcontrolIsWellFormed # scan
                                                          , pfromData (Scan.pscan'cursor s) #<= totalLength
                                                          , pif
                                                              (stage #== pstageStructure)
                                                              ( value
                                                                  #== pcon PDNothing
                                                                  #&& datum
                                                                  #== pcon PDNothing
                                                                  #&& pfromData (pproof'referenceScriptCount c)
                                                                  #== 0
                                                                  #&& pnull
                                                                  # pfromData (pproof'referenceScriptPeaks c)
                                                                  #&& scriptHash
                                                                  #== pcon PDNothing
                                                                  #&& native
                                                                  #== pcon PDNothing
                                                              )
                                                              ( pif (pnot # scanTerminal) (pconstant False)
                                                                  $ pif
                                                                    (stage #== pstageValueFold)
                                                                    ( valueWellFormed
                                                                        #&& datum
                                                                        #== pcon PDNothing
                                                                        #&& pfromData (pproof'referenceScriptCount c)
                                                                        #== 0
                                                                        #&& pnull
                                                                        # pfromData (pproof'referenceScriptPeaks c)
                                                                        #&& scriptHash
                                                                        #== pcon PDNothing
                                                                        #&& native
                                                                        #== pcon PDNothing
                                                                    )
                                                                  $ pif (pnot # valueTerminal) (pconstant False)
                                                                  $ pif (pnot # datumPresent #&& pnot # (datum #== pcon PDNothing)) (pconstant False)
                                                                  $ pif
                                                                    (stage #== pstageDatumTraversal)
                                                                    ( datumPresent
                                                                        #&& pfromData (Scan.pscan'datumLength s)
                                                                        #> 0
                                                                        #&& datumWellFormed
                                                                        #&& pfromData (pproof'referenceScriptCount c)
                                                                        #== 0
                                                                        #&& pnull
                                                                        # pfromData (pproof'referenceScriptPeaks c)
                                                                        #&& scriptHash
                                                                        #== pcon PDNothing
                                                                        #&& native
                                                                        #== pcon PDNothing
                                                                    )
                                                                  $ pif (pnot # datumComplete) (pconstant False)
                                                                  $ pif
                                                                    (language #== -1)
                                                                    ( stage
                                                                        #== pstageTerminal
                                                                        #&& pfromData (pproof'referenceScriptCount c)
                                                                        #== 0
                                                                        #&& pnull
                                                                        # pfromData (pproof'referenceScriptPeaks c)
                                                                        #&& scriptHash
                                                                        #== pcon PDNothing
                                                                        #&& native
                                                                        #== pcon PDNothing
                                                                    )
                                                                  $ pif
                                                                    ( referenceItemLength
                                                                        #<= 0
                                                                        #|| pfromData (pproof'referenceScriptCount c)
                                                                        #< 0
                                                                        #|| pfromData (pproof'referenceScriptCount c)
                                                                        #> referenceChunkCount
                                                                        #|| pnot
                                                                        # (pfrontierIsWellFormed # pfromData (pproof'referenceScriptCount c) # pfromData (pproof'referenceScriptPeaks c))
                                                                    )
                                                                    (pconstant False)
                                                                  $ pif (stage #== pstageReferenceScriptCommitment) (scriptHash #== pcon PDNothing #&& native #== pcon PDNothing)
                                                                  $ pif (pnot # frontierComplete #|| pnot # hashWellFormed) (pconstant False)
                                                                  $ pif (stage #== pstageScriptHash) (native #== pcon PDNothing)
                                                                  $ pif
                                                                    (language #== 0 #&& stage #== pstageNativeScript)
                                                                    (hashTerminal #&& nativeWellFormed)
                                                                    (stage #== pstageTerminal #&& hashTerminal #&& pif (language #== 0) nativeTerminal (native #== pcon PDNothing))
                                                              )
                                                          ]

pinitialControlV1 :: forall s. Term s (PInteger :--> PInteger :--> PByteString :--> PLedgerOutputProofControlV1)
pinitialControlV1 = phoistAcyclic $ plam $ \index total commitment -> plet
  ( pcon $
      PLedgerOutputProofControlV1
        (pdata pversion)
        (pdata pstageStructure)
        (pdata index)
        (pdata total)
        (pdata commitment)
        (pdata Scan.pinitialControlV1)
        (pdata $ pcon PDNothing)
        (pdata $ pcon PDNothing)
        (pdata 0)
        (pdata pemptyFrontier)
        (pdata $ pcon PDNothing)
        (pdata $ pcon PDNothing)
        (pdata $ pcon PDNothing)
        (pdata $ pcon PDNothing)
        (pdata $ pcon PDNothing)
        (pdata $ pcon PDNothing)
        (pdata $ pcon PDNothing)
  )
  $ \control -> pif (pcontrolIsWellFormed # control) control perror

pencodeOptional :: forall s a. (PIsData a) => Term s (a :--> PByteString) -> Term s (PMaybeData a :--> PByteString)
pencodeOptional encoder = plam $ \value -> pmatch value $ \case
  PDNothing -> pconstant "\xd8\x7a\x80"
  PDJust exact -> pconstant "\xd8\x79\x9f" <> (encoder # pfromData exact) <> pconstant "\xff"

pencodeControlV1 :: forall s. Term s (PLedgerOutputProofControlV1 :--> PByteString)
pencodeControlV1 = phoistAcyclic $ plam $ \control ->
  pif
    (pcontrolIsWellFormed # control)
    ( pmatch control $ \c ->
        (pencodeDefiniteArrayHeader # 17)
          <> pcborInt pversion
          <> pcborInt (pfromData $ pproof'stage c)
          <> pcborInt (pfromData $ pproof'outputIndex c)
          <> pcborInt (pfromData $ pproof'totalLength c)
          <> (pencodeDefiniteBytes # pfromData (pproof'itemCommitment c))
          <> (Scan.pencodeControlV1 # pfromData (pproof'outputScan c))
          <> (pencodeOptional Value.pencodeControlV1 # pfromData (pproof'value c))
          <> (pencodeOptional Traverse.pencodeControlV1 # pfromData (pproof'datum c))
          <> pcborInt (pfromData $ pproof'referenceScriptCount c)
          <> (pencodeFrontier # pfromData (pproof'referenceScriptPeaks c))
          <> (pencodeOptional Blake.pencodeControlV1 # pfromData (pproof'scriptHash c))
          <> (pencodeOptional Native.pencodeStructureControlV1 # pfromData (pproof'nativeScript c))
          <> (pencodeOptional pencodeSpanWindow # pfromData (pproof'spanWindow c))
          <> (pencodeOptional pencodeDefiniteBytes # pfromData (pproof'scanFactsFact c))
          <> (pencodeOptional pencodeDefiniteBytes # pfromData (pproof'referenceScriptFact c))
          <> (pencodeOptional pencodeDefiniteBytes # pfromData (pproof'datumSummaryFact c))
          <> (pencodeOptional pencodeDefiniteBytes # pfromData (pproof'valueSummaryFact c))
    )
    perror

poptionalFromData :: forall s a. (PIsData a) => (Term s PData -> Term s a) -> Term s PData -> Term s (PMaybeData a)
poptionalFromData decode dat = pmatch (pasConstr # dat) $ \(PBuiltinPair index fields) ->
  pif
    (index #== 0)
    (pif (plength # fields #== 1) (pcon $ PDJust $ pdata $ decode (pelemAt # 0 # fields)) perror)
    (pif (index #== 1 #&& pnull # fields) (pcon PDNothing) perror)

pdecodePeaks :: forall s. Term s (PBuiltinList PData :--> PBuiltinList (PAsData PFrontierPeak))
pdecodePeaks = phoistAcyclic $ pfix $ \self -> plam $ \items ->
  pelimList
    ( \item rest -> plet (pasList # item) $ \fields ->
        pif
          (plength # fields #== 2)
          ( pcons
              # pdata
                ( pcon $
                    PFrontierPeak
                      (pdata $ pasInt # (pelemAt # 0 # fields))
                      (pdata $ pasByteStr # (pelemAt # 1 # fields))
                )
              # (self # rest)
          )
          perror
    )
    pnil
    items

pdecodeControlV1 :: forall s. Term s (PByteString :--> PLedgerOutputProofControlV1)
pdecodeControlV1 = phoistAcyclic $ plam $ \cbor -> pmatch (pdeserialise # cbor) $ \case
  PNothing -> perror
  PJust dat -> plet (pasList # dat) $ \xs ->
    pif
      (plength # xs #== 17)
      ( plet
          ( pcon $
              PLedgerOutputProofControlV1
                (pdata $ pasInt # (pelemAt # 0 # xs))
                (pdata $ pasInt # (pelemAt # 1 # xs))
                (pdata $ pasInt # (pelemAt # 2 # xs))
                (pdata $ pasInt # (pelemAt # 3 # xs))
                (pdata $ pasByteStr # (pelemAt # 4 # xs))
                (pdata $ Scan.pcontrolFromDataV1 # (pelemAt # 5 # xs))
                (pdata $ poptionalFromData (\d -> Value.pcontrolFromDataV1 # d) (pelemAt # 6 # xs))
                (pdata $ poptionalFromData (\d -> Traverse.pcontrolFromDataV1 # d) (pelemAt # 7 # xs))
                (pdata $ pasInt # (pelemAt # 8 # xs))
                (pdata $ pdecodePeaks # (pasList # (pelemAt # 9 # xs)))
                (pdata $ poptionalFromData (\d -> Blake.pcontrolFromDataV1 # d) (pelemAt # 10 # xs))
                (pdata $ poptionalFromData (\d -> Native.pstructureControlFromDataV1 # d) (pelemAt # 11 # xs))
                (pdata $ poptionalFromData pspanWindowFromData (pelemAt # 12 # xs))
                (pdata $ poptionalFromData (\d -> pasByteStr # d) (pelemAt # 13 # xs))
                (pdata $ poptionalFromData (\d -> pasByteStr # d) (pelemAt # 14 # xs))
                (pdata $ poptionalFromData (\d -> pasByteStr # d) (pelemAt # 15 # xs))
                (pdata $ poptionalFromData (\d -> pasByteStr # d) (pelemAt # 16 # xs))
          )
          $ \control ->
            pif (pcontrolIsWellFormed # control #&& pencodeControlV1 # control #== cbor) control perror
      )
      perror

pterminalIsExactV1 :: forall s. Term s (PLedgerOutputProofControlV1 :--> PBool)
pterminalIsExactV1 = phoistAcyclic $ plam $ \control -> pmatch control $ \c ->
  pcontrolIsWellFormed # control #&& pfromData (pproof'stage c) #== pstageTerminal

pvalueSummaryV1 :: forall s. Term s (PLedgerOutputProofControlV1 :--> PMaybe PDataSummaryV1)
pvalueSummaryV1 = phoistAcyclic $ plam $ \control -> pmatch control $ \c ->
  pif (pnot # (pterminalIsExactV1 # control)) (pcon PNothing) $ pmatch (pfromData $ pproof'value c) $ \case
    PDNothing -> pcon PNothing
    PDJust value -> Value.pfinalizeV1 # pfromData value

pconstructorData :: forall s. Term s PInteger -> Term s (PBuiltinList PData) -> Term s PData
pconstructorData index fields = pforgetData $ pconstrBuiltin # index # fields

pcredentialData :: forall s. Term s PMidgardCredential -> Term s PData
pcredentialData credential = pmatch credential $ \case
  PMidgardPubKeyCredential hashValue -> pconstructorData 0 (pcons # pforgetData hashValue # pnil)
  PMidgardScriptCredential hashValue -> pconstructorData 1 (pcons # pforgetData hashValue # pnil)

pstakeCredentialData :: forall s. Term s (PMaybeData PMidgardCredential) -> Term s PData
pstakeCredentialData credential = pmatch credential $ \case
  PDNothing -> pconstructorData 1 pnil
  PDJust value -> pconstructorData 0 (pcons # pconstructorData 0 (pcons # pcredentialData (pfromData value) # pnil) # pnil)

poutputAddressSummary :: forall s. Term s PLedgerOutputProofControlV1 -> Term s PBool -> Term s (PMaybe PDataSummaryV1)
poutputAddressSummary control midgardEncoding = pmatch control $ \c -> pmatch (pfromData $ pproof'outputScan c) $ \s ->
  pmatch (pdecodeCanonicalAddressBytes # pfromData (Scan.pscan'address s)) $ \case
    PNothing -> pcon PNothing
    PJust address -> pmatch address $ \a ->
      pcon $
        PJust $
          Data.psemanticDataSummaryV1
            # ( pconstructorData
                  (pif (midgardEncoding #&& pfromData (paddress'protected a)) 1 0)
                  ( pcons
                      # pcredentialData (pfromData $ paddress'paymentCredential a)
                      # (pcons # pstakeCredentialData (pfromData $ paddress'stakeCredential a) # pnil)
                  )
              )

poutputDatumSummary :: forall s. Term s PLedgerOutputProofControlV1 -> Term s (PMaybe PDataSummaryV1)
poutputDatumSummary control = pmatch control $ \c -> pmatch (pfromData $ pproof'outputScan c) $ \s ->
  pif
    (pfromData (Scan.pscan'datumOffset s) #== -1)
    (pcon $ PJust $ Data.psmallConstrDataSummaryV1 # 0 # Data.pemptyDataListSummaryV1)
    ( pmatch (pfromData $ pproof'datum c) $ \case
        PDNothing -> pcon PNothing
        PDJust datum -> pmatch (Traverse.pfinalizeV1 # pfromData datum) $ \case
          PNothing -> pcon PNothing
          PJust summary ->
            pcon $
              PJust $
                Data.psmallConstrDataSummaryV1
                  # 2
                  # (Data.pprependDataListSummaryV1 # summary # Data.pemptyDataListSummaryV1)
    )

preferenceScriptDigestV1 :: forall s. Term s (PLedgerOutputProofControlV1 :--> PMaybe PByteString)
preferenceScriptDigestV1 = phoistAcyclic $ plam $ \control -> pmatch control $ \c ->
  pif (pnot # (pterminalIsExactV1 # control)) (pcon PNothing) $ pmatch (pfromData $ pproof'scriptHash c) $ \case
    PDNothing -> pcon PNothing
    PDJust hashControl -> Blake.pdigestV1 # pfromData hashControl

poutputReferenceScriptSummary :: forall s. Term s PLedgerOutputProofControlV1 -> Term s (PMaybe PDataSummaryV1)
poutputReferenceScriptSummary control = pmatch control $ \c -> pmatch (pfromData $ pproof'outputScan c) $ \s ->
  pif
    (pfromData (Scan.pscan'referenceScriptLanguage s) #== -1)
    (pcon $ PJust $ Data.psmallConstrDataSummaryV1 # 1 # Data.pemptyDataListSummaryV1)
    ( pmatch (preferenceScriptDigestV1 # control) $ \case
        PNothing -> pcon PNothing
        PJust digest ->
          pcon $
            PJust $
              Data.psemanticDataSummaryV1
                # (pconstructorData 0 (pcons # pforgetData (pdata digest) # pnil))
    )

ptxOutSummary :: forall s. Term s PLedgerOutputProofControlV1 -> Term s PBool -> Term s (PMaybe PDataSummaryV1)
ptxOutSummary control midgardEncoding = pif (pnot # (pterminalIsExactV1 # control)) (pcon PNothing) $
  pmatch (poutputAddressSummary control midgardEncoding) $ \case
    PNothing -> pcon PNothing
    PJust address -> pmatch (pvalueSummaryV1 # control) $ \case
      PNothing -> pcon PNothing
      PJust value -> pmatch (poutputDatumSummary control) $ \case
        PNothing -> pcon PNothing
        PJust datum -> pmatch (poutputReferenceScriptSummary control) $ \case
          PNothing -> pcon PNothing
          PJust script ->
            pcon $
              PJust $
                Data.psmallConstrDataSummaryV1
                  # 0
                  # ( Data.pprependDataListSummaryV1
                        # address
                        # ( Data.pprependDataListSummaryV1
                              # value
                              # (Data.pprependDataListSummaryV1 # datum # (Data.pprependDataListSummaryV1 # script # Data.pemptyDataListSummaryV1))
                          )
                    )

pcardanoTxOutSummaryV1, pmidgardTxOutSummaryV1 :: forall s. Term s (PLedgerOutputProofControlV1 :--> PMaybe PDataSummaryV1)
pcardanoTxOutSummaryV1 = phoistAcyclic $ plam $ \control -> ptxOutSummary control (pconstant False)
pmidgardTxOutSummaryV1 = phoistAcyclic $ plam $ \control -> ptxOutSummary control (pconstant True)

pcardanoSpendDatumSummaryV1 :: forall s. Term s (PLedgerOutputProofControlV1 :--> PMaybe PDataSummaryV1)
pcardanoSpendDatumSummaryV1 = phoistAcyclic $ plam $ \control -> pmatch control $ \c -> pmatch (pfromData $ pproof'outputScan c) $ \s ->
  pif (pnot # (pterminalIsExactV1 # control)) (pcon PNothing) $
    pif
      (pfromData (Scan.pscan'datumOffset s) #== -1)
      (pcon $ PJust $ Data.psmallConstrDataSummaryV1 # 1 # Data.pemptyDataListSummaryV1)
      ( pmatch (pfromData $ pproof'datum c) $ \case
          PDNothing -> pcon PNothing
          PDJust datum -> pmatch (Traverse.pfinalizeV1 # pfromData datum) $ \case
            PNothing -> pcon PNothing
            PJust summary ->
              pcon $
                PJust $
                  Data.psmallConstrDataSummaryV1
                    # 0
                    # (Data.pprependDataListSummaryV1 # summary # Data.pemptyDataListSummaryV1)
      )

preferenceScriptItemCommitmentV1 :: forall s. Term s (PLedgerOutputProofControlV1 :--> PMaybe PByteString)
preferenceScriptItemCommitmentV1 = phoistAcyclic $ plam $ \control -> pmatch control $ \c -> pmatch (pfromData $ pproof'outputScan c) $ \s ->
  pif
    (pnot # (pterminalIsExactV1 # control) #|| pfromData (Scan.pscan'referenceScriptLanguage s) #== -1)
    (pcon PNothing)
    ( pcon $
        PJust $
          Bounded.pcommitment
            # poutputFieldIndex
            # pfromData (pproof'outputIndex c)
            # (pfromData (pproof'totalLength c) - pfromData (Scan.pscan'referenceScriptItemOffset s))
            # pfromData (pproof'referenceScriptPeaks c)
    )

pdescriptorReferenceExact :: forall s. Term s PLedgerOutputProofControlV1 -> Term s PLedgerOutputCommitmentV1 -> Term s PBool
pdescriptorReferenceExact control descriptor = pmatch control $ \c -> pmatch (pfromData $ pproof'outputScan c) $ \s -> pmatch descriptor $ \d ->
  pif
    (pfromData (Scan.pscan'referenceScriptLanguage s) #== -1)
    ( pfromData (poutputCommitment'referenceScriptLanguage d)
        #== -1
        #&& pfromData (poutputCommitment'referenceScriptHash d)
        #== pconstant ""
        #&& pfromData (poutputCommitment'referenceScriptTotalLength d)
        #== 0
        #&& pfromData (poutputCommitment'referenceScriptItemCommitment d)
        #== pconstant ""
    )
    ( pmatch (preferenceScriptDigestV1 # control) $ \case
        PNothing -> pconstant False
        PJust digest -> pmatch (preferenceScriptItemCommitmentV1 # control) $ \case
          PNothing -> pconstant False
          PJust commitment ->
            pfromData (poutputCommitment'referenceScriptLanguage d)
              #== pfromData (Scan.pscan'referenceScriptLanguage s)
              #&& pfromData (poutputCommitment'referenceScriptHash d)
              #== digest
              #&& pfromData (poutputCommitment'referenceScriptTotalLength d)
              #== pfromData (pproof'totalLength c)
              - pfromData (Scan.pscan'referenceScriptItemOffset s)
                #&& pfromData (poutputCommitment'referenceScriptItemCommitment d)
                #== commitment
    )

pdescriptorIsExactV1 :: forall s. Term s (PLedgerOutputProofControlV1 :--> PLedgerOutputCommitmentV1 :--> PBool)
pdescriptorIsExactV1 = phoistAcyclic $ plam $ \control descriptor -> pif (pnot # (pterminalIsExactV1 # control)) (pconstant False) $
  pmatch control $ \c -> pmatch (pfromData $ pproof'outputScan c) $ \s -> pmatch descriptor $ \d ->
    pmatch (pcardanoTxOutSummaryV1 # control) $ \case
      PNothing -> pconstant False
      PJust cardano -> pmatch (pmidgardTxOutSummaryV1 # control) $ \case
        PNothing -> pconstant False
        PJust midgard -> pmatch (pcardanoSpendDatumSummaryV1 # control) $ \case
          PNothing -> pconstant False
          PJust spend ->
            pand'List
              [ pfromData (poutputCommitment'version d) #== pledgerOutputCommitmentVersion
              , pfromData (poutputCommitment'outputIndex d) #== pfromData (pproof'outputIndex c)
              , pfromData (poutputCommitment'totalLength d) #== pfromData (pproof'totalLength c)
              , pfromData (poutputCommitment'itemCommitment d) #== pfromData (pproof'itemCommitment c)
              , pfromData (poutputCommitment'address d) #== pfromData (Scan.pscan'address s)
              , pfromData (poutputCommitment'lovelace d) #== pfromData (Scan.pscan'lovelace s)
              , pfromData (poutputCommitment'assetCount d) #== pfromData (Scan.pscan'assetCount s)
              , pfromData (poutputCommitment'assetFrontierCommitment d)
                  #== pfrontierCommitment
                  # pfromData (Scan.pscan'assetCount s)
                  # pfromData (Scan.pscan'assetPeaks s)
              , pfromData (poutputCommitment'cardanoValueSize d) #== pfromData (Scan.pscan'cardanoValueSize s)
              , pdescriptorReferenceExact control descriptor
              , pfromData (poutputCommitment'cardanoTxOut d) #== cardano
              , pfromData (poutputCommitment'midgardTxOut d) #== midgard
              , pfromData (poutputCommitment'cardanoSpendDatum d) #== spend
              ]

data ProofUpdate s = ProofUpdate
  { uStage, uReferenceCount :: Maybe (Term s PInteger)
  , uScan :: Maybe (Term s PLedgerOutputScanControlV1)
  , uValue :: Maybe (Term s (PMaybeData PLedgerOutputValueControlV1))
  , uDatum :: Maybe (Term s (PMaybeData PDataTraverseControlV1))
  , uPeaks :: Maybe (Term s (PBuiltinList (PAsData PFrontierPeak)))
  , uHash :: Maybe (Term s (PMaybeData Blake.PBlake2b224TraceControlV1))
  , uNative :: Maybe (Term s (PMaybeData PNativeScriptStructureControlV1))
  , uSpanWindow :: Maybe (Term s (PMaybeData PLedgerOutputSpanWindowV1))
  }

emptyProofUpdate :: ProofUpdate s
emptyProofUpdate = ProofUpdate Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

pchoose :: (PIsData a) => Maybe (Term s a) -> Term s (PAsData a) -> Term s (PAsData a)
pchoose replacement original = maybe original pdata replacement

pupdate :: forall s. Term s PLedgerOutputProofControlV1 -> ProofUpdate s -> Term s PLedgerOutputProofControlV1
pupdate control u = pmatch control $ \c ->
  pcon $
    PLedgerOutputProofControlV1
      (pproof'version c)
      (pchoose (uStage u) $ pproof'stage c)
      (pproof'outputIndex c)
      (pproof'totalLength c)
      (pproof'itemCommitment c)
      (pchoose (uScan u) $ pproof'outputScan c)
      (pchoose (uValue u) $ pproof'value c)
      (pchoose (uDatum u) $ pproof'datum c)
      (pchoose (uReferenceCount u) $ pproof'referenceScriptCount c)
      (pchoose (uPeaks u) $ pproof'referenceScriptPeaks c)
      (pchoose (uHash u) $ pproof'scriptHash c)
      (pchoose (uNative u) $ pproof'nativeScript c)
      (pchoose (uSpanWindow u) $ pproof'spanWindow c)
      (pproof'scanFactsFact c)
      (pproof'referenceScriptFact c)
      (pproof'datumSummaryFact c)
      (pproof'valueSummaryFact c)

padvanced :: forall s. Term s PLedgerOutputProofControlV1 -> Term s (PMaybe PLedgerOutputProofStepResultV1)
padvanced control =
  pif
    (pcontrolIsWellFormed # control)
    (pcon $ PJust $ pcon $ PLedgerOutputProofAdvanced $ pdata control)
    (pcon PNothing)

pchunkMatches :: forall s. Term s PInteger -> Term s PInteger -> Term s PByteString -> Term s PChunkProofV1 -> Term s PInteger -> Term s PBool
pchunkMatches outputIndex totalLength commitment proof chunkIndex = pmatch proof $ \p ->
  pand'List
    [ pfromData (pchunkProof'fieldIndex p) #== poutputFieldIndex
    , pfromData (pchunkProof'itemIndex p) #== outputIndex
    , pfromData (pchunkProof'totalLength p) #== totalLength
    , pfromData (pchunkProof'chunkIndex p) #== chunkIndex
    , Bounded.pverifyChunk # commitment # proof
    ]

pauthenticatedChunkWindowForOutput :: forall s. Term s PInteger -> Term s PInteger -> Term s PByteString -> Term s PInteger -> Term s PLedgerOutputProofWitnessV1 -> Term s PBool -> Term s (PMaybe (PPair PByteString PInteger))
pauthenticatedChunkWindowForOutput outputIndex totalLength commitment cursor witness requireFollowing = pmatch witness $ \case
  PLedgerOutputProofChunks chunkProof nextProof ->
    plet (pfromData chunkProof) $ \first -> pmatch first $ \f ->
      plet (pdiv # cursor # Bounded.pchunkBytes) $ \chunkIndex ->
        plet (Bounded.pchunkCount # totalLength) $ \count ->
          plet (chunkIndex + 1 #< count) $ \hasFollowing ->
            pif (pnot # (pchunkMatches outputIndex totalLength commitment first chunkIndex)) (pcon PNothing) $
              pif
                (requireFollowing #&& hasFollowing)
                ( pmatch (pfromData nextProof) $ \case
                    PDNothing -> pcon PNothing
                    PDJust next -> plet (pfromData next) $ \second -> pmatch second $ \n ->
                      pif
                        (pchunkMatches outputIndex totalLength commitment second (chunkIndex + 1))
                        ( pcon $
                            PJust $
                              pcon $
                                PPair
                                  (pfromData (pchunkProof'chunk f) <> pfromData (pchunkProof'chunk n))
                                  (cursor - chunkIndex * Bounded.pchunkBytes)
                        )
                        (pcon PNothing)
                )
                ( pmatch (pfromData nextProof) $ \case
                    PDNothing -> pcon $ PJust $ pcon $ PPair (pfromData $ pchunkProof'chunk f) (cursor - chunkIndex * Bounded.pchunkBytes)
                    PDJust _ -> pcon PNothing
                )
  _ -> pcon PNothing

pauthenticatedOutputSpanForOutput :: forall s. Term s PInteger -> Term s PInteger -> Term s PByteString -> Term s PInteger -> Term s PInteger -> Term s PLedgerOutputProofWitnessV1 -> Term s (PMaybe PByteString)
pauthenticatedOutputSpanForOutput outputIndex totalLength commitment start length witness =
  pif (length #<= 0 #|| length #> Bounded.pchunkBytes #|| start #< 0 #|| start + length #> totalLength) (pcon PNothing) $
    pmatch witness $ \case
      PLedgerOutputProofChunks chunkProof nextProof -> plet (pfromData chunkProof) $ \first -> pmatch first $ \f ->
        plet (pdiv # start # Bounded.pchunkBytes) $ \firstIndex ->
          plet (pdiv # (start + length - 1) # Bounded.pchunkBytes) $ \lastIndex ->
            plet (start - firstIndex * Bounded.pchunkBytes) $ \localStart ->
              pif (lastIndex #> firstIndex + 1 #|| pnot # (pchunkMatches outputIndex totalLength commitment first firstIndex)) (pcon PNothing) $
                pif
                  (lastIndex #== firstIndex)
                  ( pmatch (pfromData nextProof) $ \case
                      PDNothing -> pcon $ PJust $ psliceBS # localStart # length # pfromData (pchunkProof'chunk f)
                      PDJust _ -> pcon PNothing
                  )
                  ( pmatch (pfromData nextProof) $ \case
                      PDNothing -> pcon PNothing
                      PDJust next -> plet (pfromData next) $ \second -> pmatch second $ \n ->
                        pif
                          (pchunkMatches outputIndex totalLength commitment second lastIndex)
                          (pcon $ PJust $ psliceBS # localStart # length # (pfromData (pchunkProof'chunk f) <> pfromData (pchunkProof'chunk n)))
                          (pcon PNothing)
                  )
      _ -> pcon PNothing

pauthenticatedChunkWindow :: forall s. Term s PLedgerOutputProofControlV1 -> Term s PInteger -> Term s PLedgerOutputProofWitnessV1 -> Term s PBool -> Term s (PMaybe (PPair PByteString PInteger))
pauthenticatedChunkWindow control cursor witness following = pmatch control $ \c ->
  pauthenticatedChunkWindowForOutput (pfromData $ pproof'outputIndex c) (pfromData $ pproof'totalLength c) (pfromData $ pproof'itemCommitment c) cursor witness following

pauthenticatedOutputSpan :: forall s. Term s PLedgerOutputProofControlV1 -> Term s PInteger -> Term s PInteger -> Term s PLedgerOutputProofWitnessV1 -> Term s (PMaybe PByteString)
pauthenticatedOutputSpan control start length witness = pmatch control $ \c ->
  pauthenticatedOutputSpanForOutput (pfromData $ pproof'outputIndex c) (pfromData $ pproof'totalLength c) (pfromData $ pproof'itemCommitment c) start length witness

pstructureStep :: forall s. Term s (PLedgerOutputProofControlV1 :--> PLedgerOutputProofWitnessV1 :--> PMaybe PLedgerOutputProofStepResultV1)
pstructureStep = phoistAcyclic $ plam $ \control witness -> pmatch control $ \c -> plet (pfromData $ pproof'outputScan c) $ \scan -> pmatch scan $ \s ->
  pif
    (Scan.pterminalIsExactV1 # scan # pfromData (pproof'totalLength c))
    ( pmatch witness $ \case
        PLedgerOutputProofNoWitness ->
          padvanced $
            pupdate control $
              emptyProofUpdate
                { uStage = Just pstageValueFold
                , uValue = Just $ pcon $ PDJust $ pdata $ Value.pinitialControlV1 # pfromData (Scan.pscan'assetCount s)
                }
        _ -> pcon PNothing
    )
    ( pmatch (Scan.pfinishV1 # scan # pfromData (pproof'totalLength c)) $ \case
        PJust finished -> pmatch witness $ \case
          PLedgerOutputProofNoWitness -> padvanced $ pupdate control $ emptyProofUpdate{uScan = Just finished}
          _ -> pcon PNothing
        PNothing -> pmatch
          ( pauthenticatedChunkWindow
              control
              (pfromData $ Scan.pscan'cursor s)
              witness
              (pfromData (Scan.pscan'stage s) #<= Scan.pstageOptionalField)
          )
          $ \case
            PNothing -> pcon PNothing
            PJust windowPair -> pmatch windowPair $ \(PPair window offset) ->
              pmatch (Scan.pstepV1 # scan # pfromData (pproof'totalLength c) # window # offset) $ \case
                PNothing -> pcon $ PJust $ pcon PLedgerOutputProofInvalidOutput
                PJust next -> padvanced $ pupdate control $ emptyProofUpdate{uScan = Just next}
    )

pvalueFoldStep :: forall s. Term s (PLedgerOutputProofControlV1 :--> PLedgerOutputProofWitnessV1 :--> PLedgerOutputValueControlV1 :--> PMaybe PLedgerOutputProofStepResultV1)
pvalueFoldStep = phoistAcyclic $ plam $ \control witness valueControl -> pmatch control $ \c -> pmatch (pfromData $ pproof'outputScan c) $ \s -> pmatch valueControl $ \v ->
  pif
    (pfromData (Value.pvalueControl'stage v) #== Value.pstageTerminal)
    ( pmatch witness $ \case
        PLedgerOutputProofNoWitness ->
          pif
            (pfromData (Scan.pscan'datumOffset s) #/= -1)
            ( padvanced $
                pupdate control $
                  emptyProofUpdate
                    { uStage = Just pstageDatumTraversal
                    , uDatum = Just $ pcon $ PDJust $ pdata $ Traverse.pinitialControlV1 # pfromData (Scan.pscan'datumOffset s) # pfromData (Scan.pscan'datumLength s)
                    }
            )
            (padvanced $ pupdate control $ emptyProofUpdate{uStage = Just $ pif (pfromData (Scan.pscan'referenceScriptLanguage s) #== -1) pstageTerminal pstageReferenceScriptCommitment})
        _ -> pcon PNothing
    )
    ( plet
        ( pmatch witness $ \case
            PLedgerOutputProofNoWitness -> pcon $ PJust $ pcon Value.PLedgerOutputValueNoWitness
            PLedgerOutputProofValue index policy name quantity siblings previous -> pcon $ PJust $ pcon $ Value.PLedgerOutputValueAsset index policy name quantity siblings previous
            _ -> pcon PNothing
        )
        $ \inner -> pmatch inner $ \case
          PNothing -> pcon PNothing
          PJust valueWitness -> pmatch
            ( Value.pstepV1
                # valueControl
                # pfromData (Scan.pscan'assetCount s)
                # pfromData (Scan.pscan'assetPeaks s)
                # pfromData (Scan.pscan'lovelace s)
                # valueWitness
            )
            $ \case
              PNothing -> pcon PNothing
              PJust next -> padvanced $ pupdate control $ emptyProofUpdate{uValue = Just $ pcon $ PDJust $ pdata next}
    )

pdatumStepWith ::
  forall s.
  Term s (PDataTraverseControlV1 :--> PMaybe PByteString :--> PDataTraverseActionV1 :--> PMaybe PDataTraverseControlV1) ->
  Term s PLedgerOutputProofControlV1 ->
  Term s PLedgerOutputProofWitnessV1 ->
  Term s PDataTraverseControlV1 ->
  Term s (PMaybe PLedgerOutputProofStepResultV1)
pdatumStepWith traverseStep control witness datumControl = pmatch control $ \c -> pmatch (pfromData $ pproof'outputScan c) $ \s -> pmatch datumControl $ \d ->
  pif
    (pfromData (Traverse.ptraverse'stage d) #== Traverse.pstageTerminal)
    ( pmatch witness $ \case
        PLedgerOutputProofNoWitness ->
          padvanced $
            pupdate control $
              emptyProofUpdate
                { uStage = Just $ pif (pfromData (Scan.pscan'referenceScriptLanguage s) #== -1) pstageTerminal pstageReferenceScriptCommitment
                }
        _ -> pcon PNothing
    )
    ( pmatch witness $ \case
        PLedgerOutputProofDatum action window ->
          plet
            ( pmatch (Traverse.pnextSourceSpanV1 # datumControl) $ \case
                PNothing ->
                  pif
                    (pfromData window #== pcon PDNothing)
                    (pcon $ PJust $ pcon PNothing)
                    (pcon PNothing)
                PJust spanValue -> pmatch spanValue $ \spanFields -> pmatch (pfromData window) $ \case
                  PDNothing -> pcon PNothing
                  PDJust bytes -> pmatch
                    ( pboundWindowBytesV1
                        # pfromData (pproof'spanWindow c)
                        # pfromData (Blob.pspan'absoluteStart spanFields)
                        # pfromData (Blob.pspan'length spanFields)
                        # pfromData bytes
                    )
                    $ \case
                      PNothing -> pcon PNothing
                      PJust bytes -> pcon $ PJust $ pcon $ PJust bytes
            )
            $ \authenticated -> pmatch authenticated $ \case
              PNothing -> pcon PNothing
              PJust source -> pmatch (traverseStep # datumControl # source # pfromData action) $ \case
                PNothing -> pcon PNothing
                PJust nextDatum -> padvanced $ pupdate control $ emptyProofUpdate{uDatum = Just $ pcon $ PDJust $ pdata nextDatum}
        _ -> pcon PNothing
    )

pdatumStep :: forall s. Term s (PLedgerOutputProofControlV1 :--> PLedgerOutputProofWitnessV1 :--> PDataTraverseControlV1 :--> PMaybe PLedgerOutputProofStepResultV1)
pdatumStep = phoistAcyclic $ plam $ \control witness datumControl ->
  pdatumStepWith Traverse.pstepV1 control witness datumControl

pdatumHeadStep :: forall s. Term s (PLedgerOutputProofControlV1 :--> PLedgerOutputProofWitnessV1 :--> PDataTraverseControlV1 :--> PMaybe PLedgerOutputProofStepResultV1)
pdatumHeadStep = phoistAcyclic $ plam $ \control witness datumControl ->
  pdatumStepWith Traverse.pstepHead control witness datumControl

pdatumFoldStep :: forall s. Term s (PLedgerOutputProofControlV1 :--> PLedgerOutputProofWitnessV1 :--> PDataTraverseControlV1 :--> PMaybe PLedgerOutputProofStepResultV1)
pdatumFoldStep = phoistAcyclic $ plam $ \control witness datumControl ->
  pdatumStepWith Traverse.pstepFold control witness datumControl

preferenceCommitmentStep :: forall s. Term s (PLedgerOutputProofControlV1 :--> PLedgerOutputProofWitnessV1 :--> PMaybe PLedgerOutputProofStepResultV1)
preferenceCommitmentStep = phoistAcyclic $ plam $ \control witness -> pmatch control $ \c -> pmatch (pfromData $ pproof'outputScan c) $ \s ->
  plet (pfromData $ Scan.pscan'referenceScriptItemOffset s) $ \itemOffset ->
    plet (pfromData (pproof'totalLength c) - itemOffset) $ \itemLength ->
      plet (Bounded.pchunkCount # itemLength) $ \count -> plet (pfromData $ pproof'referenceScriptCount c) $ \index ->
        pif
          (index #== count)
          ( pmatch witness $ \case
              PLedgerOutputProofNoWitness ->
                padvanced $
                  pupdate control $
                    emptyProofUpdate
                      { uStage = Just pstageScriptHash
                      , uHash = Just $ pcon $ PDJust $ pdata $ Blake.pinitialControlV1 # (pfromData (Scan.pscan'referenceScriptLength s) + 1)
                      }
              _ -> pcon PNothing
          )
          ( plet (Bounded.pexpectedChunkLength # itemLength # index) $ \chunkLength ->
              pmatch (pwindowWitness control (itemOffset + index * Bounded.pchunkBytes) chunkLength witness) $ \case
                PNothing -> pcon PNothing
                PJust chunk ->
                  padvanced $
                    pupdate control $
                      emptyProofUpdate
                        { uReferenceCount = Just $ index + 1
                        , uPeaks =
                            Just $
                              pappendLeaf
                                # index
                                # pfromData (pproof'referenceScriptPeaks c)
                                # (Bounded.phashChunk # poutputFieldIndex # pfromData (pproof'outputIndex c) # index # chunk)
                        }
          )

phashStep :: forall s. Term s (PLedgerOutputProofControlV1 :--> PLedgerOutputProofWitnessV1 :--> Blake.PBlake2b224TraceControlV1 :--> PMaybe PLedgerOutputProofStepResultV1)
phashStep = phoistAcyclic $ plam $ \control witness hashControl -> pmatch control $ \c -> pmatch (pfromData $ pproof'outputScan c) $ \s -> pmatch hashControl $ \h ->
  pif
    (pfromData (Blake.pctl'stage h) #== Blake.pstageTerminal)
    ( pmatch witness $ \case
        PLedgerOutputProofNoWitness ->
          pif
            (pfromData (Scan.pscan'referenceScriptLanguage s) #/= 0)
            (padvanced $ pupdate control $ emptyProofUpdate{uStage = Just pstageTerminal})
            ( pif
                (pfromData (Scan.pscan'referenceScriptLength s) #== 0)
                (pcon $ PJust $ pcon PLedgerOutputProofInvalidReferenceScript)
                ( padvanced $
                    pupdate control $
                      emptyProofUpdate
                        { uStage = Just pstageNativeScript
                        , uNative =
                            Just $
                              pcon $
                                PDJust $
                                  pdata $
                                    Native.pinitialStructureControlV1
                                      # pfromData (Scan.pscan'referenceScriptOffset s)
                                      # pfromData (Scan.pscan'referenceScriptLength s)
                        }
                )
            )
        _ -> pcon PNothing
    )
    ( pif
        (pfromData (Blake.pctl'stage h) #== Blake.pstageReady)
        ( plet (pfromData (Blake.pctl'totalLength h) - pfromData (Blake.pctl'cursor h)) $ \remaining ->
            plet (pif (remaining #< Blake.pblockBytes) remaining Blake.pblockBytes) $ \expected ->
              plet (pfromData (Blake.pctl'cursor h) #== 0) $ \includesLanguage ->
                plet (expected - pif includesLanguage 1 0) $ \contentLength ->
                  plet (pfromData (Scan.pscan'referenceScriptOffset s) + pfromData (Blake.pctl'cursor h) - pif includesLanguage 0 1) $ \start ->
                    plet
                      ( pif
                          (contentLength #== 0)
                          (pmatch witness $ \case PLedgerOutputProofNoWitness -> pcon $ PJust $ pconstant ""; _ -> pcon PNothing)
                          (pwindowWitness control start contentLength witness)
                      )
                      $ \content -> pmatch content $ \case
                        PNothing -> pcon PNothing
                        PJust bytes -> plet
                          ( pif
                              includesLanguage
                              ((preplicateBS # 1 # (pintegerToByte # pfromData (Scan.pscan'referenceScriptLanguage s))) <> bytes)
                              bytes
                          )
                          $ \block ->
                            pmatch (Blake.pstepV1 # hashControl # pcon (PJust block)) $ \case
                              PNothing -> pcon PNothing
                              PJust next -> padvanced $ pupdate control $ emptyProofUpdate{uHash = Just $ pcon $ PDJust $ pdata next}
        )
        ( pmatch witness $ \case
            PLedgerOutputProofNoWitness -> pmatch (Blake.pstepV1 # hashControl # pcon PNothing) $ \case
              PNothing -> pcon PNothing
              PJust next -> padvanced $ pupdate control $ emptyProofUpdate{uHash = Just $ pcon $ PDJust $ pdata next}
            _ -> pcon PNothing
        )
    )

pmapNative :: forall s. Term s (PMaybe Native.PNativeScriptStructureStepResultV1) -> Term s PLedgerOutputProofControlV1 -> Term s (PMaybe PLedgerOutputProofStepResultV1)
pmapNative result control = pmatch result $ \case
  PNothing -> pcon PNothing
  PJust nativeResult -> pmatch nativeResult $ \case
    Native.PNativeScriptStructureAdvanced next -> padvanced $ pupdate control $ emptyProofUpdate{uNative = Just $ pcon $ PDJust next}
    Native.PNativeScriptStructureInvalid -> pcon $ PJust $ pcon PLedgerOutputProofInvalidReferenceScript
    Native.PNativeScriptStructureNodeLimit -> pcon $ PJust $ pcon PLedgerOutputProofNativeScriptNodeLimit
    Native.PNativeScriptStructureDepthLimit -> pcon $ PJust $ pcon PLedgerOutputProofNativeScriptDepthLimit

pnativeStep :: forall s. Term s (PLedgerOutputProofControlV1 :--> PLedgerOutputProofWitnessV1 :--> PNativeScriptStructureControlV1 :--> PMaybe PLedgerOutputProofStepResultV1)
pnativeStep = phoistAcyclic $ plam $ \control witness nativeControl -> pmatch nativeControl $ \n ->
  pif
    (pfromData (Native.pstructure'stage n) #== Native.pstructureStageTerminal)
    (pmatch witness $ \case PLedgerOutputProofNoWitness -> padvanced $ pupdate control $ emptyProofUpdate{uStage = Just pstageTerminal}; _ -> pcon PNothing)
    $ pif
      (pfromData (Native.pstructure'stage n) #== Native.pstructureStageToken)
      ( pmatch (pauthenticatedChunkWindow control (pfromData $ Native.pstructure'cursor n) witness (pconstant True)) $ \case
          PNothing -> pcon PNothing
          PJust pair -> pmatch pair $ \(PPair window offset) -> pmapNative (Native.pstructureTokenStepV1 # nativeControl # window # offset) control
      )
    $ pif
      (pfromData (Native.pstructure'stage n) #== Native.pstructureStageFrame)
      (pmatch witness $ \case PLedgerOutputProofNativeFrame frame -> pmapNative (Native.pstructureFrameStepV1 # nativeControl # pfromData frame) control; _ -> pcon PNothing)
      (pmatch witness $ \case PLedgerOutputProofNoWitness -> pmapNative (Native.pfinalizeStructureV1 # nativeControl) control; _ -> pcon PNothing)

pstepV1 :: forall s. Term s (PLedgerOutputProofControlV1 :--> PLedgerOutputProofWitnessV1 :--> PMaybe PLedgerOutputProofStepResultV1)
pstepV1 = phoistAcyclic $ plam $ \control witness -> pmatch control $ \c ->
  pif (pnot # (pcontrolIsWellFormed # control)) (pcon PNothing)
    $ pif
      (pfstBuiltin # (pasConstr # pforgetData (pdata witness)) #== 5)
      ( pmatch witness $ \case
          PLedgerOutputProofSpanAttach start length first next ->
            pif
              ( pfromData (pproof'stage c)
                  #== pstageDatumTraversal
                  #|| pfromData (pproof'stage c)
                  #== pstageReferenceScriptCommitment
                  #|| pfromData (pproof'stage c)
                  #== pstageScriptHash
              )
              (pspanAttachStep control (pfromData start) (pfromData length) first next)
              (pcon PNothing)
          _ -> perror
      )
    $ pif (pfromData (pproof'stage c) #== pstageStructure) (pstructureStep # control # witness)
    $ pif (pfromData (pproof'stage c) #== pstageValueFold) (pmatch (pfromData $ pproof'value c) $ \case PDNothing -> pcon PNothing; PDJust v -> pvalueFoldStep # control # witness # pfromData v)
    $ pif (pfromData (pproof'stage c) #== pstageScriptHash) (pmatch (pfromData $ pproof'scriptHash c) $ \case PDNothing -> pcon PNothing; PDJust h -> phashStep # control # witness # pfromData h)
    $ pif (pfromData (pproof'stage c) #== pstageDatumTraversal) (pmatch (pfromData $ pproof'datum c) $ \case PDNothing -> pcon PNothing; PDJust d -> pdatumStep # control # witness # pfromData d)
    $ pif (pfromData (pproof'stage c) #== pstageReferenceScriptCommitment) (preferenceCommitmentStep # control # witness)
    $ pif
      (pfromData (pproof'stage c) #== pstageNativeScript)
      (pmatch (pfromData $ pproof'nativeScript c) $ \case PDNothing -> pcon PNothing; PDJust n -> pnativeStep # control # witness # pfromData n)
      (pcon PNothing)

pfactIsWellFormed :: forall s. Term s PInteger -> Term s (PMaybeData PByteString) -> Term s PBool
pfactIsWellFormed stage fact = pmatch fact $ \case
  PDNothing -> pconstant True
  PDJust digest -> stage #== pstageTerminal #&& plengthBS # pfromData digest #== 32

pencodeInitialControlV1 :: forall s. Term s (PInteger :--> PInteger :--> PByteString :--> PByteString)
pencodeInitialControlV1 = phoistAcyclic $ plam $ \index length commitment ->
  pif
    (index #>= 0 #&& length #> 0 #&& plengthBS # commitment #== 32)
    ( pconstant "\x91"
        <> pcborInt pversion
        <> pcborInt pstageStructure
        <> pcborInt index
        <> pcborInt length
        <> (pencodeDefiniteBytes # commitment)
        <> Scan.pinitialControlCborV1
        <> pconstant "\xd8\x7a\x80\xd8\x7a\x80\x00\x80\xd8\x7a\x80\xd8\x7a\x80"
        <> pconstant "\xd8\x7a\x80\xd8\x7a\x80\xd8\x7a\x80\xd8\x7a\x80\xd8\x7a\x80"
    )
    perror

pencodeSpanWindow :: forall s. Term s (PLedgerOutputSpanWindowV1 :--> PByteString)
pencodeSpanWindow = phoistAcyclic $ plam $ \window -> pmatch window $ \w ->
  pconstant "\x83"
    <> pcborInt (pfromData $ pwindow'start w)
    <> pcborInt (pfromData $ pwindow'length w)
    <> (pencodeDefiniteBytes # pfromData (pwindow'digest w))

pspanWindowFromData :: forall s. Term s PData -> Term s PLedgerOutputSpanWindowV1
pspanWindowFromData dat = plet (pasList # dat) $ \fields ->
  pif
    (plength # fields #== 3)
    ( pcon $
        PLedgerOutputSpanWindowV1
          (pdata $ pasInt # (pelemAt # 0 # fields))
          (pdata $ pasInt # (pelemAt # 1 # fields))
          (pdata $ pasByteStr # (pelemAt # 2 # fields))
    )
    perror

pboundWindowBytesV1 :: forall s. Term s (PMaybeData PLedgerOutputSpanWindowV1 :--> PInteger :--> PInteger :--> PByteString :--> PMaybe PByteString)
pboundWindowBytesV1 = phoistAcyclic $ plam $ \window start length bytes -> pmatch window $ \case
  PDNothing -> pcon PNothing
  PDJust exact -> pmatch (pfromData exact) $ \w ->
    pif
      ( length
          #> 0
          #&& start
          #>= pfromData (pwindow'start w)
          #&& start
          + length #<= pfromData (pwindow'start w)
          + pfromData (pwindow'length w)
            #&& plengthBS
            # bytes
            #== pfromData (pwindow'length w)
            #&& pblake2b_256
            # bytes
            #== pfromData (pwindow'digest w)
      )
      (pcon $ PJust $ psliceBS # (start - pfromData (pwindow'start w)) # length # bytes)
      (pcon PNothing)

pwindowWitness :: forall s. Term s PLedgerOutputProofControlV1 -> Term s PInteger -> Term s PInteger -> Term s PLedgerOutputProofWitnessV1 -> Term s (PMaybe PByteString)
pwindowWitness control start length witness = pmatch control $ \c -> pmatch witness $ \case
  PLedgerOutputProofWindow bytes -> pboundWindowBytesV1 # pfromData (pproof'spanWindow c) # start # length # pfromData bytes
  _ -> pcon PNothing

pspanAttachStep ::
  forall s.
  Term s PLedgerOutputProofControlV1 ->
  Term s PInteger ->
  Term s PInteger ->
  Term s (PAsData PChunkProofV1) ->
  Term s (PAsData (PMaybeData PChunkProofV1)) ->
  Term s (PMaybe PLedgerOutputProofStepResultV1)
pspanAttachStep control start length first next =
  pmatch (pauthenticatedOutputSpan control start length (pcon $ PLedgerOutputProofChunks first next)) $ \case
    PNothing -> pcon PNothing
    PJust bytes ->
      padvanced $
        pupdate control $
          emptyProofUpdate
            { uSpanWindow = Just $ pcon $ PDJust $ pdata $ pcon $ PLedgerOutputSpanWindowV1 (pdata start) (pdata length) (pdata $ pblake2b_256 # bytes)
            }

pfactCommitmentV1 :: forall s. Term s (PInteger :--> PByteString :--> PData :--> PData :--> PByteString)
pfactCommitmentV1 = phoistAcyclic $ plam $ \role descriptor value datum ->
  plet (pforgetData $ pdata descriptor) $ \descriptorData ->
    pblake2b_256
      # ( pserialiseData
            # ( plistData
                  # ( pcons
                        # descriptorData
                        # ( pif (role #== 0) (pcons # value # (pcons # datum # pnil)) $
                              pif (role #== 1) pnil $
                                pif (role #== 2) (pcons # datum # pnil) $
                                  pif (role #== 3) (pcons # value # pnil) perror
                          )
                    )
              )
        )

pfactV1 :: forall s. Term s (PLedgerOutputProofControlV1 :--> PInteger :--> PMaybeData PByteString)
pfactV1 = phoistAcyclic $ plam $ \control role -> pmatch control $ \c ->
  pif (role #== 0) (pfromData $ pproof'scanFactsFact c) $
    pif (role #== 1) (pfromData $ pproof'referenceScriptFact c) $
      pif (role #== 2) (pfromData $ pproof'datumSummaryFact c) $
        pif (role #== 3) (pfromData $ pproof'valueSummaryFact c) perror

pwithFactV1 :: forall s. Term s (PLedgerOutputProofControlV1 :--> PInteger :--> PByteString :--> PLedgerOutputProofControlV1)
pwithFactV1 = phoistAcyclic $ plam $ \control role digest -> pmatch control $ \c ->
  plet (pdata $ pcon $ PDJust $ pdata digest) $ \fact ->
    pif (role #== 0) (pcon c{pproof'scanFactsFact = fact}) $
      pif (role #== 1) (pcon c{pproof'referenceScriptFact = fact}) $
        pif (role #== 2) (pcon c{pproof'datumSummaryFact = fact}) $
          pif (role #== 3) (pcon c{pproof'valueSummaryFact = fact}) perror

pfactsCompleteV1 :: forall s. Term s (PLedgerOutputProofControlV1 :--> PBool)
pfactsCompleteV1 = phoistAcyclic $ plam $ \control -> pmatch control $ \c ->
  pfromData (pproof'scanFactsFact c)
    #/= pcon PDNothing
    #&& pfromData (pproof'referenceScriptFact c)
    #/= pcon PDNothing
    #&& pfromData (pproof'datumSummaryFact c)
    #/= pcon PDNothing
    #&& pfromData (pproof'valueSummaryFact c)
    #/= pcon PDNothing

pterminalClaimedSummariesV1 :: forall s. Term s (PLedgerOutputProofControlV1 :--> PMaybe (PPair PData PData))
pterminalClaimedSummariesV1 = phoistAcyclic $ plam $ \control ->
  pif (pnot # (pterminalIsExactV1 # control)) (pcon PNothing) $
    pmatch (pvalueSummaryV1 # control) $ \case
      PNothing -> pcon PNothing
      PJust value -> pmatch control $ \c -> pmatch (pfromData $ pproof'outputScan c) $ \scan ->
        plet (pconstructorData 1 pnil) $ \none ->
          plet
            ( pif (pfromData (Scan.pscan'datumOffset scan) #== (-1)) none $
                pmatch (pfromData $ pproof'datum c) $ \case
                  PDNothing -> none
                  PDJust datum -> pmatch (Traverse.pfinalizeV1 # pfromData datum) $ \case
                    PNothing -> none
                    PJust summary -> pconstructorData 0 (pcons # pforgetData (pdata summary) # pnil)
            )
            $ \datumData ->
              pcon $ PJust $ pcon $ PPair (pforgetData $ pdata value) datumData

pfactAttachGroupsV1 :: forall s. Term s (PBuiltinList (PBuiltinList PInteger))
pfactAttachGroupsV1 = pconstant [[2, 3], [0], [1]]

pfactAttachV1 :: forall s. Term s (PLedgerOutputProofControlV1 :--> PByteString :--> PMaybe PLedgerOutputProofControlV1)
pfactAttachV1 = phoistAcyclic $ plam $ \control descriptor ->
  pmatch (pterminalClaimedSummariesV1 # control) $ \case
    PNothing -> pcon PNothing
    PJust pair -> pmatch pair $ \(PPair value datum) ->
      plet (pfactV1 # control # 2 #== pcon PDNothing) $ \datumMissing ->
        plet (pfactV1 # control # 3 #== pcon PDNothing) $ \valueMissing ->
          let attach next role = pwithFactV1 # next # role # (pfactCommitmentV1 # role # descriptor # value # datum)
           in pif
                (datumMissing #|| valueMissing)
                (pif (datumMissing #&& valueMissing) (pcon $ PJust $ attach (attach control 2) 3) perror)
                $ pif (pfactV1 # control # 0 #== pcon PDNothing) (pcon $ PJust $ attach control 0)
                $ pif (pfactV1 # control # 1 #== pcon PDNothing) (pcon $ PJust $ attach control 1) (pcon PNothing)

pfactsAreExactV1 :: forall s. Term s (PLedgerOutputProofControlV1 :--> PByteString :--> PData :--> PData :--> PBool)
pfactsAreExactV1 = phoistAcyclic $ plam $ \control descriptor value datum ->
  pall
    # plam (\role -> pfactV1 # control # role #== pcon (PDJust $ pdata $ pfactCommitmentV1 # role # descriptor # value # datum))
    # pconstant @(PBuiltinList PInteger) [0, 1, 2, 3]
