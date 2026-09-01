{-# LANGUAGE OverloadedStrings #-}

module Midgard.LedgerOutputValue (
  PLedgerOutputValueControlV1 (..),
  PLedgerOutputValueWitnessV1 (..),
  pversion,
  pstageAssets,
  pstageFinalize,
  pstageTerminal,
  pcontrolIsWellFormed,
  pinitialControlV1,
  pencodeControlV1,
  pcontrolFromDataV1,
  pdecodeControlV1,
  pstepV1,
  pfinalizeV1,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Core.Utils (pand'List)
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.Prelude

import Aiken.Cbor (pdeserialise)
import Midgard.CekData (
  PDataSequenceSummaryV1 (..),
  PDataSummaryV1 (..),
  pemptyDataPairSummaryV1,
  pmapDataSummaryV1,
  pprependDataPairSummaryV1,
  psemanticDataSummaryV1,
 )
import Midgard.FraudProofs.NativeTx.Codec (pcborInt, pencodeDefiniteArrayHeader, pencodeDefiniteBytes)
import Midgard.LedgerOutputCommitment (passetLeafHash)
import Midgard.ValidationMerkle (PFrontierPeak, pfrontierIsWellFormed, pverifyMembership)

pversion, pstageAssets, pstageFinalize, pstageTerminal :: forall (s :: S). Term s PInteger
pversion = 1
pstageAssets = 0
pstageFinalize = 1
pstageTerminal = 2

puint32Max, puint64Max :: forall (s :: S). Term s PInteger
puint32Max = 4_294_967_295
puint64Max = 18_446_744_073_709_551_615

data PLedgerOutputValueControlV1 (s :: S) = PLedgerOutputValueControlV1
  { pvalueControl'version :: Term s (PAsData PInteger)
  , pvalueControl'stage :: Term s (PAsData PInteger)
  , pvalueControl'assetRemaining :: Term s (PAsData PInteger)
  , pvalueControl'currentPolicy :: Term s (PAsData PByteString)
  , pvalueControl'currentAssets :: Term s (PAsData PDataSequenceSummaryV1)
  , pvalueControl'valueEntries :: Term s (PAsData PDataSequenceSummaryV1)
  , pvalueControl'result :: Term s (PAsData (PMaybeData PDataSummaryV1))
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PLedgerOutputValueControlV1)

data PLedgerOutputValueWitnessV1 (s :: S)
  = PLedgerOutputValueNoWitness
  | PLedgerOutputValueAsset
      (Term s (PAsData PByteString))
      (Term s (PAsData PByteString))
      (Term s (PAsData PInteger))
      (Term s (PAsData (PBuiltinList (PAsData PByteString))))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PLedgerOutputValueWitnessV1)

puint64IsValid :: forall (s :: S). Term s PInteger -> Term s PBool
puint64IsValid value = value #>= 0 #&& value #<= puint64Max

psequenceIsWellFormed :: forall (s :: S). Term s PDataSequenceSummaryV1 -> Term s PBool
psequenceIsWellFormed summary = pmatch summary $ \s -> pand'List
  [ plengthBS # pfromData (pseq'root s) #== 32
  , pfromData (pseq'length s) #>= 0
  , pfromData (pseq'length s) #<= puint32Max
  , puint64IsValid (pfromData $ pseq'payloadCborLength s)
  , puint64IsValid (pfromData $ pseq'memory s)
  ]

psummaryIsWellFormed :: forall (s :: S). Term s PDataSummaryV1 -> Term s PBool
psummaryIsWellFormed summary = pmatch summary $ \s -> pand'List
  [ plengthBS # pfromData (psummary'root s) #== 32
  , puint64IsValid (pfromData $ psummary'cborLength s)
  , puint64IsValid (pfromData $ psummary'memory s)
  ]

pcontrolIsWellFormed :: forall (s :: S). Term s (PLedgerOutputValueControlV1 :--> PBool)
pcontrolIsWellFormed = phoistAcyclic $ plam $ \control -> pmatch control $ \c ->
  plet (pfromData $ pvalueControl'stage c) $ \stage ->
  plet (pfromData $ pvalueControl'assetRemaining c) $ \remaining ->
  plet (pfromData $ pvalueControl'currentPolicy c) $ \policy ->
  plet (pfromData $ pvalueControl'currentAssets c) $ \currentAssets ->
  plet (pfromData $ pvalueControl'valueEntries c) $ \valueEntries ->
  plet (pfromData $ pvalueControl'result c) $ \result ->
    pand'List
      [ pfromData (pvalueControl'version c) #== pversion
      , stage #>= pstageAssets
      , stage #<= pstageTerminal
      , remaining #>= 0
      , remaining #<= puint32Max
      , policy #== pconstant "" #|| plengthBS # policy #== 28
      , psequenceIsWellFormed currentAssets
      , psequenceIsWellFormed valueEntries
      , pmatch result $ \case
          PDNothing -> pnot # (stage #== pstageTerminal)
          PDJust exact -> stage #== pstageTerminal #&& psummaryIsWellFormed (pfromData exact)
      , pif (stage #== pstageTerminal)
          ( remaining #== 0
              #&& policy #== pconstant ""
              #&& currentAssets #== pemptyDataPairSummaryV1
              #&& valueEntries #== pemptyDataPairSummaryV1
          )
          ( pif (policy #== pconstant "") (currentAssets #== pemptyDataPairSummaryV1) (pconstant True)
              #&& pif (stage #== pstageFinalize) (remaining #== 0) (pconstant True)
          )
      ]

pinitialControlV1 :: forall (s :: S). Term s (PInteger :--> PLedgerOutputValueControlV1)
pinitialControlV1 = phoistAcyclic $ plam $ \assetCount ->
  plet (pcon $ PLedgerOutputValueControlV1
    (pdata pversion) (pdata pstageAssets) (pdata assetCount) (pdata $ pconstant "")
    (pdata pemptyDataPairSummaryV1) (pdata pemptyDataPairSummaryV1) (pdata $ pcon PDNothing)) $ \control ->
      pif (pcontrolIsWellFormed # control) control perror

pencodeSequence :: forall (s :: S). Term s (PDataSequenceSummaryV1 :--> PByteString)
pencodeSequence = phoistAcyclic $ plam $ \summary -> pmatch summary $ \s ->
  pif (psequenceIsWellFormed summary)
    ( (pencodeDefiniteArrayHeader # 4)
        <> (pencodeDefiniteBytes # pfromData (pseq'root s))
        <> pcborInt (pfromData $ pseq'length s)
        <> pcborInt (pfromData $ pseq'payloadCborLength s)
        <> pcborInt (pfromData $ pseq'memory s)
    )
    perror

pencodeSummary :: forall (s :: S). Term s (PDataSummaryV1 :--> PByteString)
pencodeSummary = phoistAcyclic $ plam $ \summary -> pmatch summary $ \s ->
  pif (psummaryIsWellFormed summary)
    ( (pencodeDefiniteArrayHeader # 3)
        <> (pencodeDefiniteBytes # pfromData (psummary'root s))
        <> pcborInt (pfromData $ psummary'cborLength s)
        <> pcborInt (pfromData $ psummary'memory s)
    )
    perror

pencodeOptionalSummary :: forall (s :: S). Term s (PMaybeData PDataSummaryV1 :--> PByteString)
pencodeOptionalSummary = phoistAcyclic $ plam $ \summary -> pmatch summary $ \case
  PDNothing -> pconstant "\xd8\x7a\x80"
  PDJust value -> pconstant "\xd8\x79\x9f" <> (pencodeSummary # pfromData value) <> pconstant "\xff"

pencodeControlV1 :: forall (s :: S). Term s (PLedgerOutputValueControlV1 :--> PByteString)
pencodeControlV1 = phoistAcyclic $ plam $ \control ->
  pif (pcontrolIsWellFormed # control)
    (pmatch control $ \c ->
      (pencodeDefiniteArrayHeader # 7)
        <> pcborInt pversion
        <> pcborInt (pfromData $ pvalueControl'stage c)
        <> pcborInt (pfromData $ pvalueControl'assetRemaining c)
        <> (pencodeDefiniteBytes # pfromData (pvalueControl'currentPolicy c))
        <> (pencodeSequence # pfromData (pvalueControl'currentAssets c))
        <> (pencodeSequence # pfromData (pvalueControl'valueEntries c))
        <> (pencodeOptionalSummary # pfromData (pvalueControl'result c)))
    perror

psequenceFromData :: forall (s :: S). Term s (PData :--> PDataSequenceSummaryV1)
psequenceFromData = phoistAcyclic $ plam $ \dat -> plet (pasList # dat) $ \items ->
  pif (plength # items #== 4)
    (plet (pcon $ PDataSequenceSummaryV1
      (pdata $ pasByteStr # (pelemAt # 0 # items))
      (pdata $ pasInt # (pelemAt # 1 # items))
      (pdata $ pasInt # (pelemAt # 2 # items))
      (pdata $ pasInt # (pelemAt # 3 # items))) $ \summary ->
        pif (psequenceIsWellFormed summary) summary perror)
    perror

psummaryFromData :: forall (s :: S). Term s (PData :--> PDataSummaryV1)
psummaryFromData = phoistAcyclic $ plam $ \dat -> plet (pasList # dat) $ \items ->
  pif (plength # items #== 3)
    (plet (pcon $ PDataSummaryV1
      (pdata $ pasByteStr # (pelemAt # 0 # items))
      (pdata $ pasInt # (pelemAt # 1 # items))
      (pdata $ pasInt # (pelemAt # 2 # items))) $ \summary ->
        pif (psummaryIsWellFormed summary) summary perror)
    perror

poptionalSummaryFromData :: forall (s :: S). Term s (PData :--> PMaybeData PDataSummaryV1)
poptionalSummaryFromData = phoistAcyclic $ plam $ \dat -> pmatch (pasConstr # dat) $ \(PBuiltinPair index fields) ->
  pif (index #== 0)
    (pif (plength # fields #== 1)
      (pcon $ PDJust $ pdata $ psummaryFromData # (pelemAt # 0 # fields))
      perror)
    (pif (index #== 1 #&& pnull # fields) (pcon PDNothing) perror)

pcontrolFromDataV1 :: forall (s :: S). Term s (PData :--> PLedgerOutputValueControlV1)
pcontrolFromDataV1 = phoistAcyclic $ plam $ \dat -> plet (pasList # dat) $ \items ->
  pif (plength # items #== 7)
    (plet (pcon $ PLedgerOutputValueControlV1
      (pdata $ pasInt # (pelemAt # 0 # items))
      (pdata $ pasInt # (pelemAt # 1 # items))
      (pdata $ pasInt # (pelemAt # 2 # items))
      (pdata $ pasByteStr # (pelemAt # 3 # items))
      (pdata $ psequenceFromData # (pelemAt # 4 # items))
      (pdata $ psequenceFromData # (pelemAt # 5 # items))
      (pdata $ poptionalSummaryFromData # (pelemAt # 6 # items))) $ \control ->
        pif (pcontrolIsWellFormed # control) control perror)
    perror

pdecodeControlV1 :: forall (s :: S). Term s (PByteString :--> PLedgerOutputValueControlV1)
pdecodeControlV1 = phoistAcyclic $ plam $ \controlCbor ->
  pmatch (pdeserialise # controlCbor) $ \case
    PNothing -> perror
    PJust dat -> plet (pcontrolFromDataV1 # dat) $ \control ->
      pif (pencodeControlV1 # control #== controlCbor) control perror

pboundedIntegerSummary :: forall (s :: S). Term s PInteger -> Term s PDataSummaryV1
pboundedIntegerSummary integer = psemanticDataSummaryV1 # pforgetData (pdata integer)

pboundedBytesSummary :: forall (s :: S). Term s PByteString -> Term s PDataSummaryV1
pboundedBytesSummary bytes = psemanticDataSummaryV1 # pforgetData (pdata bytes)

pfinalizeCurrentPolicy :: forall (s :: S). Term s PLedgerOutputValueControlV1 -> Term s PDataSequenceSummaryV1
pfinalizeCurrentPolicy control = pmatch control $ \c ->
  plet (pfromData $ pvalueControl'currentPolicy c) $ \policy ->
    pif (policy #== pconstant "")
      (pfromData $ pvalueControl'valueEntries c)
      ( pprependDataPairSummaryV1
          # pboundedBytesSummary policy
          # (pmapDataSummaryV1 # pfromData (pvalueControl'currentAssets c))
          # pfromData (pvalueControl'valueEntries c)
      )

padvanced :: forall (s :: S). Term s PLedgerOutputValueControlV1 -> Term s (PMaybe PLedgerOutputValueControlV1)
padvanced control = pif (pcontrolIsWellFormed # control) (pcon $ PJust control) (pcon PNothing)

passetStep ::
  forall (s :: S).
  Term s PLedgerOutputValueControlV1 -> Term s PInteger ->
  Term s (PBuiltinList (PAsData PFrontierPeak)) -> Term s PByteString -> Term s PByteString ->
  Term s PInteger -> Term s (PBuiltinList (PAsData PByteString)) -> Term s (PMaybe PLedgerOutputValueControlV1)
passetStep control assetCount peaks policyId assetName quantity siblings = pmatch control $ \c ->
  plet (pfromData (pvalueControl'assetRemaining c) - 1) $ \leafIndex ->
  pif
    ( pnot # (plengthBS # policyId #== 28)
        #|| plengthBS # assetName #> 32
        #|| quantity #<= 0
        #|| quantity #> puint64Max
        #|| pnot # (pverifyMembership # assetCount # peaks # leafIndex # (passetLeafHash # policyId # assetName # quantity) # siblings)
    )
    (pcon PNothing)
    ( plet (pfromData $ pvalueControl'currentPolicy c) $ \currentPolicy ->
      pif (pnot # (currentPolicy #== pconstant "") #&& policyId #> currentPolicy)
        (pcon PNothing)
        ( plet (currentPolicy #== pconstant "" #|| policyId #< currentPolicy) $ \policyChanged ->
          plet
            (pif (policyChanged #&& pnot # (currentPolicy #== pconstant ""))
              (pfinalizeCurrentPolicy control)
              (pfromData $ pvalueControl'valueEntries c))
            $ \valueEntries ->
            plet
              (pif policyChanged pemptyDataPairSummaryV1 (pfromData $ pvalueControl'currentAssets c))
              $ \currentAssets ->
                padvanced $ pcon $ PLedgerOutputValueControlV1
                  (pvalueControl'version c) (pvalueControl'stage c) (pdata leafIndex) (pdata policyId)
                  (pdata $ pprependDataPairSummaryV1 # pboundedBytesSummary assetName # pboundedIntegerSummary quantity # currentAssets)
                  (pdata valueEntries) (pvalueControl'result c)
        )
    )

pfinalizeStep :: forall (s :: S). Term s PLedgerOutputValueControlV1 -> Term s PInteger -> Term s (PMaybe PLedgerOutputValueControlV1)
pfinalizeStep control lovelace = pmatch control $ \c ->
  plet (pfinalizeCurrentPolicy control) $ \policyEntries ->
  plet
    (pif (lovelace #== 0)
      policyEntries
      ( plet (pboundedBytesSummary $ pconstant "") $ \emptyBytes ->
        plet (pprependDataPairSummaryV1 # emptyBytes # pboundedIntegerSummary lovelace # pemptyDataPairSummaryV1) $ \coinAssets ->
          pprependDataPairSummaryV1 # emptyBytes # (pmapDataSummaryV1 # coinAssets) # policyEntries
      ))
    $ \valueEntries -> padvanced $ pcon $ PLedgerOutputValueControlV1
      (pvalueControl'version c) (pdata pstageTerminal) (pvalueControl'assetRemaining c) (pdata $ pconstant "")
      (pdata pemptyDataPairSummaryV1) (pdata pemptyDataPairSummaryV1)
      (pdata $ pcon $ PDJust $ pdata $ pmapDataSummaryV1 # valueEntries)

pstepV1 ::
  forall (s :: S).
  Term s
    ( PLedgerOutputValueControlV1 :--> PInteger :--> PBuiltinList (PAsData PFrontierPeak)
        :--> PInteger :--> PLedgerOutputValueWitnessV1 :--> PMaybe PLedgerOutputValueControlV1
    )
pstepV1 = phoistAcyclic $ plam $ \control assetCount peaks lovelace witness -> pmatch control $ \c ->
  pif
    ( pnot # (pcontrolIsWellFormed # control)
        #|| assetCount #< 0 #|| assetCount #> puint32Max
        #|| pfromData (pvalueControl'assetRemaining c) #> assetCount
        #|| pnot # (pfrontierIsWellFormed # assetCount # peaks)
        #|| pnot # puint64IsValid lovelace
    )
    (pcon PNothing)
    ( pif (pfromData (pvalueControl'stage c) #== pstageAssets)
        ( pif (pfromData (pvalueControl'assetRemaining c) #== 0)
            (pmatch witness $ \case
              PLedgerOutputValueNoWitness -> padvanced $ pcon $ PLedgerOutputValueControlV1
                (pvalueControl'version c) (pdata pstageFinalize) (pvalueControl'assetRemaining c)
                (pvalueControl'currentPolicy c) (pvalueControl'currentAssets c)
                (pvalueControl'valueEntries c) (pvalueControl'result c)
              _ -> pcon PNothing)
            (pmatch witness $ \case
              PLedgerOutputValueAsset policyId assetName quantity siblings ->
                passetStep control assetCount peaks (pfromData policyId) (pfromData assetName) (pfromData quantity) (pfromData siblings)
              _ -> pcon PNothing)
        )
        ( pif (pfromData (pvalueControl'stage c) #== pstageFinalize)
            (pmatch witness $ \case
              PLedgerOutputValueNoWitness -> pfinalizeStep control lovelace
              _ -> pcon PNothing)
            (pcon PNothing)
        )
    )

pfinalizeV1 :: forall (s :: S). Term s (PLedgerOutputValueControlV1 :--> PMaybe PDataSummaryV1)
pfinalizeV1 = phoistAcyclic $ plam $ \control -> pmatch control $ \c ->
  pif (pcontrolIsWellFormed # control #&& pfromData (pvalueControl'stage c) #== pstageTerminal)
    (pmatch (pfromData $ pvalueControl'result c) $ \case
      PDNothing -> pcon PNothing
      PDJust result -> pcon $ PJust $ pfromData result)
    (pcon PNothing)
