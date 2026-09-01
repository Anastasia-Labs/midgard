{-# LANGUAGE OverloadedStrings #-}

module Midgard.LedgerOutputScan (
  PLedgerOutputScanControlV1 (..),
  pversion,
  pstageRequiredFields, pstageValueHeader, pstagePolicyHeader, pstageAsset,
  pstageOptionalField, pstageDatumPayload, pstageReferenceScriptPayload, pstageTerminal,
  pinitialControlV1, pcontrolIsWellFormed, pencodeControlV1, pcontrolFromDataV1,
  pdecodeControlV1, pstepV1, pfinishV1, pterminalIsExactV1,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Builtin.Data (pserialiseData)
import Plutarch.Core.Utils (pand'List, (#/=))
import Plutarch.Prelude

import Aiken.Cbor (pdeserialise)
import Midgard.CanonicalCborScan (PCborBytesV1 (..), PCborHeadV1 (..), pbytesAtV1, pheadAtV1)
import Midgard.FraudProofs.NativeTx.Codec (pcborInt, pencodeDefiniteArrayHeader, pencodeDefiniteBytes)
import Midgard.LedgerOutput (pdecodeCanonicalAddressBytes)
import Midgard.LedgerOutputCommitment (passetLeafHash)
import Midgard.ValidationMerkle (
  PFrontierPeak (..), pappendLeaf, pemptyFrontier, pencodeFrontier,
  pfrontierIsWellFormed, pmaximumLeafCount,
 )

pversion, pstageRequiredFields, pstageValueHeader, pstagePolicyHeader, pstageAsset,
  pstageOptionalField, pstageDatumPayload, pstageReferenceScriptPayload, pstageTerminal :: forall s. Term s PInteger
pversion = 1
pstageRequiredFields = 0
pstageValueHeader = 1
pstagePolicyHeader = 2
pstageAsset = 3
pstageOptionalField = 4
pstageDatumPayload = 5
pstageReferenceScriptPayload = 6
pstageTerminal = 7

data PLedgerOutputScanControlV1 (s :: S) = PLedgerOutputScanControlV1
  { pscan'version :: Term s (PAsData PInteger)
  , pscan'stage :: Term s (PAsData PInteger)
  , pscan'cursor :: Term s (PAsData PInteger)
  , pscan'mapEntryCount :: Term s (PAsData PInteger)
  , pscan'optionalFieldCount :: Term s (PAsData PInteger)
  , pscan'address :: Term s (PAsData PByteString)
  , pscan'lovelace :: Term s (PAsData PInteger)
  , pscan'cardanoValueSize :: Term s (PAsData PInteger)
  , pscan'policyRemaining :: Term s (PAsData PInteger)
  , pscan'assetRemaining :: Term s (PAsData PInteger)
  , pscan'policyAssetCursor :: Term s (PAsData PInteger)
  , pscan'previousPolicy :: Term s (PAsData PByteString)
  , pscan'currentPolicy :: Term s (PAsData PByteString)
  , pscan'previousAssetName :: Term s (PAsData PByteString)
  , pscan'assetCount :: Term s (PAsData PInteger)
  , pscan'assetPeaks :: Term s (PAsData (PBuiltinList (PAsData PFrontierPeak)))
  , pscan'datumOffset :: Term s (PAsData PInteger)
  , pscan'datumLength :: Term s (PAsData PInteger)
  , pscan'payloadRemaining :: Term s (PAsData PInteger)
  , pscan'referenceScriptLanguage :: Term s (PAsData PInteger)
  , pscan'referenceScriptItemOffset :: Term s (PAsData PInteger)
  , pscan'referenceScriptOffset :: Term s (PAsData PInteger)
  , pscan'referenceScriptLength :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PLedgerOutputScanControlV1)

pinitialControlV1 :: forall s. Term s PLedgerOutputScanControlV1
pinitialControlV1 = pcon $ PLedgerOutputScanControlV1
  (pdata pversion) (pdata pstageRequiredFields) (pdata 0) (pdata 0) (pdata 0)
  (pdata $ pconstant "") (pdata 0) (pdata 0) (pdata 0) (pdata 0) (pdata 0)
  (pdata $ pconstant "") (pdata $ pconstant "") (pdata $ pconstant "")
  (pdata 0) (pdata pemptyFrontier) (pdata $ -1) (pdata 0) (pdata 0)
  (pdata $ -1) (pdata $ -1) (pdata $ -1) (pdata 0)

pcontrolIsWellFormed :: forall s. Term s (PLedgerOutputScanControlV1 :--> PBool)
pcontrolIsWellFormed = phoistAcyclic $ plam $ \control -> pmatch control $ \c ->
  plet (pfromData $ pscan'stage c) $ \stage ->
  plet (pfromData $ pscan'mapEntryCount c) $ \mapCount ->
  plet (pfromData $ pscan'address c) $ \address ->
  plet (pfromData $ pscan'previousPolicy c) $ \previousPolicy ->
  plet (pfromData $ pscan'currentPolicy c) $ \currentPolicy ->
  plet (pfromData $ pscan'datumOffset c) $ \datumOffset ->
  plet (pfromData $ pscan'datumLength c) $ \datumLength ->
  plet (pfromData $ pscan'referenceScriptLanguage c) $ \language ->
  plet (pfromData $ pscan'referenceScriptItemOffset c) $ \itemOffset ->
  plet (pfromData $ pscan'referenceScriptOffset c) $ \scriptOffset ->
  plet (pfromData $ pscan'referenceScriptLength c) $ \scriptLength -> pand'List
    [ pfromData (pscan'version c) #== pversion
    , stage #>= pstageRequiredFields, stage #<= pstageTerminal
    , pfromData (pscan'cursor c) #>= 0
    , mapCount #== 0 #|| (mapCount #>= 2 #&& mapCount #<= 4)
    , pfromData (pscan'optionalFieldCount c) #>= 0
    , pfromData (pscan'optionalFieldCount c) #<= 2
    , address #== pconstant "" #|| pmatch (pdecodeCanonicalAddressBytes # address) (\case PNothing -> pconstant False; PJust _ -> pconstant True)
    , pfromData (pscan'lovelace c) #>= 0
    , pfromData (pscan'cardanoValueSize c) #>= 0
    , pfromData (pscan'policyRemaining c) #>= 0
    , pfromData (pscan'assetRemaining c) #>= 0
    , pfromData (pscan'policyAssetCursor c) #>= 0
    , previousPolicy #== pconstant "" #|| plengthBS # previousPolicy #== 28
    , currentPolicy #== pconstant "" #|| plengthBS # currentPolicy #== 28
    , plengthBS # pfromData (pscan'previousAssetName c) #<= 32
    , pfrontierIsWellFormed # pfromData (pscan'assetCount c) # pfromData (pscan'assetPeaks c)
    , datumOffset #>= -1, datumLength #>= 0
    , pif (datumOffset #== -1) (datumLength #== 0) (pconstant True)
    , pfromData (pscan'payloadRemaining c) #>= 0
    , language #== -1 #|| language #== 0 #|| language #== 3 #|| language #== 128
    , itemOffset #>= -1, scriptOffset #>= -1, scriptLength #>= 0
    , pif (language #== -1)
        (itemOffset #== -1 #&& scriptOffset #== -1 #&& scriptLength #== 0)
        (itemOffset #>= 0 #&& scriptOffset #> itemOffset)
    ]

pencodeControlV1 :: forall s. Term s (PLedgerOutputScanControlV1 :--> PByteString)
pencodeControlV1 = phoistAcyclic $ plam $ \control -> pif (pcontrolIsWellFormed # control)
  (pmatch control $ \c ->
    (pencodeDefiniteArrayHeader # 23)
      <> pcborInt pversion <> pcborInt (i $ pscan'stage c) <> pcborInt (i $ pscan'cursor c)
      <> pcborInt (i $ pscan'mapEntryCount c) <> pcborInt (i $ pscan'optionalFieldCount c)
      <> (pencodeDefiniteBytes # b (pscan'address c)) <> pcborInt (i $ pscan'lovelace c)
      <> pcborInt (i $ pscan'cardanoValueSize c) <> pcborInt (i $ pscan'policyRemaining c)
      <> pcborInt (i $ pscan'assetRemaining c) <> pcborInt (i $ pscan'policyAssetCursor c)
      <> (pencodeDefiniteBytes # b (pscan'previousPolicy c))
      <> (pencodeDefiniteBytes # b (pscan'currentPolicy c))
      <> (pencodeDefiniteBytes # b (pscan'previousAssetName c))
      <> pcborInt (i $ pscan'assetCount c) <> (pencodeFrontier # pfromData (pscan'assetPeaks c))
      <> pcborInt (i $ pscan'datumOffset c) <> pcborInt (i $ pscan'datumLength c)
      <> pcborInt (i $ pscan'payloadRemaining c) <> pcborInt (i $ pscan'referenceScriptLanguage c)
      <> pcborInt (i $ pscan'referenceScriptItemOffset c) <> pcborInt (i $ pscan'referenceScriptOffset c)
      <> pcborInt (i $ pscan'referenceScriptLength c))
  perror
  where i = pfromData; b = pfromData

pdecodePeaks :: forall s. Term s (PBuiltinList PData :--> PBuiltinList (PAsData PFrontierPeak))
pdecodePeaks = phoistAcyclic $ pfix $ \self -> plam $ \items -> pelimList
  (\item rest -> plet (pasList # item) $ \fields -> pif (plength # fields #== 2)
    (pcons # pdata (pcon $ PFrontierPeak (pdata $ pasInt # (pelemAt # 0 # fields)) (pdata $ pasByteStr # (pelemAt # 1 # fields))) # (self # rest))
    perror)
  pnil items

pcontrolFromDataV1 :: forall s. Term s (PData :--> PLedgerOutputScanControlV1)
pcontrolFromDataV1 = phoistAcyclic $ plam $ \dat -> plet (pasList # dat) $ \xs ->
  pif (plength # xs #== 23)
    (plet (pcon $ PLedgerOutputScanControlV1
      (di 0 xs) (di 1 xs) (di 2 xs) (di 3 xs) (di 4 xs) (db 5 xs) (di 6 xs) (di 7 xs)
      (di 8 xs) (di 9 xs) (di 10 xs) (db 11 xs) (db 12 xs) (db 13 xs) (di 14 xs)
      (pdata $ pdecodePeaks # (pasList # (pelemAt # 15 # xs))) (di 16 xs) (di 17 xs) (di 18 xs)
      (di 19 xs) (di 20 xs) (di 21 xs) (di 22 xs)) $ \control ->
        pif (pcontrolIsWellFormed # control) control perror)
    perror
  where
    di n xs = pdata $ pasInt # (pelemAt # pconstant n # xs)
    db n xs = pdata $ pasByteStr # (pelemAt # pconstant n # xs)

pdecodeControlV1 :: forall s. Term s (PByteString :--> PLedgerOutputScanControlV1)
pdecodeControlV1 = phoistAcyclic $ plam $ \cbor -> pmatch (pdeserialise # cbor) $ \case
  PNothing -> perror
  PJust dat -> plet (pcontrolFromDataV1 # dat) $ \control ->
    pif (pencodeControlV1 # control #== cbor) control perror

data Update s = Update
  { uStage, uCursor, uMapCount, uOptionalCount, uLovelace, uValueSize,
    uPolicyRemaining, uAssetRemaining, uPolicyCursor, uAssetCount,
    uDatumOffset, uDatumLength, uPayloadRemaining, uLanguage, uItemOffset,
    uScriptOffset, uScriptLength :: Maybe (Term s PInteger)
  , uAddress, uPreviousPolicy, uCurrentPolicy, uPreviousName :: Maybe (Term s PByteString)
  , uPeaks :: Maybe (Term s (PBuiltinList (PAsData PFrontierPeak)))
  }

emptyUpdate :: Update s
emptyUpdate = Update Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
  Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

pchooseUpdate :: forall s a. PIsData a => Maybe (Term s a) -> Term s (PAsData a) -> Term s (PAsData a)
pchooseUpdate replacement original = maybe original pdata replacement

pupdate :: forall s. Term s PLedgerOutputScanControlV1 -> Update s -> Term s PLedgerOutputScanControlV1
pupdate control u = pmatch control $ \c -> pcon $ PLedgerOutputScanControlV1
  (pscan'version c) (pchooseUpdate (uStage u) $ pscan'stage c) (pchooseUpdate (uCursor u) $ pscan'cursor c)
  (pchooseUpdate (uMapCount u) $ pscan'mapEntryCount c) (pchooseUpdate (uOptionalCount u) $ pscan'optionalFieldCount c)
  (pchooseUpdate (uAddress u) $ pscan'address c) (pchooseUpdate (uLovelace u) $ pscan'lovelace c)
  (pchooseUpdate (uValueSize u) $ pscan'cardanoValueSize c) (pchooseUpdate (uPolicyRemaining u) $ pscan'policyRemaining c)
  (pchooseUpdate (uAssetRemaining u) $ pscan'assetRemaining c) (pchooseUpdate (uPolicyCursor u) $ pscan'policyAssetCursor c)
  (pchooseUpdate (uPreviousPolicy u) $ pscan'previousPolicy c) (pchooseUpdate (uCurrentPolicy u) $ pscan'currentPolicy c)
  (pchooseUpdate (uPreviousName u) $ pscan'previousAssetName c) (pchooseUpdate (uAssetCount u) $ pscan'assetCount c)
  (pchooseUpdate (uPeaks u) $ pscan'assetPeaks c) (pchooseUpdate (uDatumOffset u) $ pscan'datumOffset c)
  (pchooseUpdate (uDatumLength u) $ pscan'datumLength c) (pchooseUpdate (uPayloadRemaining u) $ pscan'payloadRemaining c)
  (pchooseUpdate (uLanguage u) $ pscan'referenceScriptLanguage c) (pchooseUpdate (uItemOffset u) $ pscan'referenceScriptItemOffset c)
  (pchooseUpdate (uScriptOffset u) $ pscan'referenceScriptOffset c) (pchooseUpdate (uScriptLength u) $ pscan'referenceScriptLength c)

pabsoluteOffset :: forall s. Term s PLedgerOutputScanControlV1 -> Term s PInteger -> Term s PInteger -> Term s PInteger
pabsoluteOffset control windowOffset localOffset = pmatch control $ \c -> pfromData (pscan'cursor c) + localOffset - windowOffset

pcanonicalKeyAt :: forall s. Term s (PByteString :--> PInteger :--> PInteger :--> PMaybe PInteger)
pcanonicalKeyAt = phoistAcyclic $ plam $ \window offset expected -> pmatch (pheadAtV1 # window # offset # 0) $ \case
  PNothing -> pcon PNothing
  PJust headValue -> pmatch headValue $ \h -> pif (pcborHead'value h #== expected) (pcon $ PJust $ pcborHead'nextOffset h) (pcon PNothing)

pmajorHeadLength :: forall s. Term s (PInteger :--> PInteger)
pmajorHeadLength = phoistAcyclic $ plam $ \value -> pif (value #< 0) perror $
  pif (value #< 24) 1 $ pif (value #<= 255) 2 $ pif (value #<= 65_535) 3 $ pif (value #<= 4_294_967_295) 5 9

pstepRequiredFields :: forall s. Term s PLedgerOutputScanControlV1 -> Term s PByteString -> Term s PInteger -> Term s (PMaybe PLedgerOutputScanControlV1)
pstepRequiredFields control window windowOffset = pmatch (pheadAtV1 # window # windowOffset # 5) $ \case
  PNothing -> pcon PNothing
  PJust outputMap -> pmatch outputMap $ \m ->
    pif (pcborHead'value m #< 2 #|| pcborHead'value m #> 4) (pcon PNothing) $
      pmatch (pcanonicalKeyAt # window # pcborHead'nextOffset m # 0) $ \case
        PNothing -> pcon PNothing
        PJust addressOffset -> pmatch (pbytesAtV1 # window # addressOffset) $ \case
          PNothing -> pcon PNothing
          PJust addressResult -> pmatch addressResult $ \address ->
            pmatch (pdecodeCanonicalAddressBytes # pcborBytes'value address) $ \case
              PNothing -> pcon PNothing
              PJust _ -> pcon $ PJust $ pupdate control $ emptyUpdate
                { uStage = Just pstageValueHeader
                , uCursor = Just $ pabsoluteOffset control windowOffset (pcborBytes'nextOffset address)
                , uMapCount = Just $ pcborHead'value m
                , uAddress = Just $ pcborBytes'value address
                }

pstepValueHeader :: forall s. Term s PLedgerOutputScanControlV1 -> Term s PByteString -> Term s PInteger -> Term s (PMaybe PLedgerOutputScanControlV1)
pstepValueHeader control window windowOffset = pmatch (pcanonicalKeyAt # window # windowOffset # 1) $ \case
  PNothing -> pcon PNothing
  PJust valueOffset -> pmatch (pheadAtV1 # window # valueOffset # 4) $ \case
    PNothing -> pcon PNothing
    PJust valueArray -> pmatch valueArray $ \array -> pif (pcborHead'value array #/= 2) (pcon PNothing) $
      pmatch (pheadAtV1 # window # pcborHead'nextOffset array # 0) $ \case
        PNothing -> pcon PNothing
        PJust lovelaceResult -> pmatch lovelaceResult $ \lovelace ->
          pmatch (pheadAtV1 # window # pcborHead'nextOffset lovelace # 5) $ \case
            PNothing -> pcon PNothing
            PJust policyResult -> pmatch policyResult $ \policies ->
              pif (pcborHead'value policies #> pmaximumLeafCount) (pcon PNothing) $
                plet (pcborHead'value policies) $ \policyCount ->
                pcon $ PJust $ pupdate control $ emptyUpdate
                  { uStage = Just $ pif (policyCount #== 0) pstageOptionalField pstagePolicyHeader
                  , uCursor = Just $ pabsoluteOffset control windowOffset (pcborHead'nextOffset policies)
                  , uLovelace = Just $ pcborHead'value lovelace
                  , uValueSize = Just $ pif (policyCount #== 0)
                      (plengthBS #$ pserialiseData # pforgetData (pdata $ pcborHead'value lovelace))
                      (1 + (plengthBS #$ pserialiseData # pforgetData (pdata $ pcborHead'value lovelace)) + (pmajorHeadLength # policyCount))
                  , uPolicyRemaining = Just policyCount
                  }

pstepPolicyHeader :: forall s. Term s PLedgerOutputScanControlV1 -> Term s PByteString -> Term s PInteger -> Term s (PMaybe PLedgerOutputScanControlV1)
pstepPolicyHeader control window windowOffset = pmatch control $ \c -> pmatch (pbytesAtV1 # window # windowOffset) $ \case
  PNothing -> pcon PNothing
  PJust policyResult -> pmatch policyResult $ \policy ->
    pmatch (pheadAtV1 # window # pcborBytes'nextOffset policy # 5) $ \case
      PNothing -> pcon PNothing
      PJust assetResult -> pmatch assetResult $ \assets ->
        plet (pcborBytes'value policy) $ \policyId ->
        plet (pcborHead'value assets) $ \assetCount ->
        plet (pfromData $ pscan'previousPolicy c) $ \previous ->
        pif
          ( plengthBS # policyId #/= 28
              #|| pnot # (previous #== pconstant "" #|| previous #< policyId)
              #|| assetCount #<= 0 #|| assetCount #> pmaximumLeafCount
              #|| pfromData (pscan'policyRemaining c) #<= 0
          )
          (pcon PNothing)
          (pcon $ PJust $ pupdate control $ emptyUpdate
            { uStage = Just pstageAsset
            , uCursor = Just $ pabsoluteOffset control windowOffset (pcborHead'nextOffset assets)
            , uAssetRemaining = Just assetCount, uPolicyCursor = Just 0
            , uCurrentPolicy = Just policyId, uPreviousName = Just $ pconstant ""
            , uValueSize = Just $ pfromData (pscan'cardanoValueSize c)
                + (plengthBS #$ pencodeDefiniteBytes # policyId) + (pmajorHeadLength # assetCount)
            })

pstepAsset :: forall s. Term s PLedgerOutputScanControlV1 -> Term s PByteString -> Term s PInteger -> Term s (PMaybe PLedgerOutputScanControlV1)
pstepAsset control window windowOffset = pmatch control $ \c -> pmatch (pbytesAtV1 # window # windowOffset) $ \case
  PNothing -> pcon PNothing
  PJust nameResult -> pmatch nameResult $ \name ->
    pmatch (pheadAtV1 # window # pcborBytes'nextOffset name # 0) $ \case
      PNothing -> pcon PNothing
      PJust quantityResult -> pmatch quantityResult $ \quantity ->
        plet (pcborBytes'value name) $ \assetName ->
        plet (pcborHead'value quantity) $ \amount ->
        plet (pfromData $ pscan'policyAssetCursor c) $ \policyCursor ->
        plet (pfromData $ pscan'previousAssetName c) $ \previousName ->
        plet
          (pif (policyCursor #== 0) (pconstant True) $
            plet (plengthBS # previousName) $ \previousLength -> plet (plengthBS # assetName) $ \nameLength ->
              pif (previousLength #< nameLength) (pconstant True) $
                pif (previousLength #> nameLength) (pconstant False) (previousName #< assetName))
          $ \nameOrdered ->
          plet (pfromData (pscan'assetRemaining c) - 1) $ \nextAssetRemaining ->
          plet (nextAssetRemaining #== 0) $ \policyComplete ->
          plet (pfromData (pscan'policyRemaining c) - pif policyComplete 1 0) $ \nextPolicyRemaining ->
          pif
            ( plengthBS # pfromData (pscan'currentPolicy c) #/= 28
                #|| plengthBS # assetName #> 32 #|| pnot # nameOrdered #|| amount #<= 0
                #|| pfromData (pscan'assetRemaining c) #<= 0
                #|| pfromData (pscan'assetCount c) #>= pmaximumLeafCount
            )
            (pcon PNothing)
            (pcon $ PJust $ pupdate control $ emptyUpdate
              { uStage = Just $ pif policyComplete (pif (nextPolicyRemaining #== 0) pstageOptionalField pstagePolicyHeader) pstageAsset
              , uCursor = Just $ pabsoluteOffset control windowOffset (pcborHead'nextOffset quantity)
              , uPolicyRemaining = Just nextPolicyRemaining, uAssetRemaining = Just nextAssetRemaining
              , uPolicyCursor = Just $ pif policyComplete 0 (policyCursor + 1)
              , uPreviousPolicy = Just $ pif policyComplete (pfromData $ pscan'currentPolicy c) (pfromData $ pscan'previousPolicy c)
              , uCurrentPolicy = Just $ pif policyComplete (pconstant "") (pfromData $ pscan'currentPolicy c)
              , uPreviousName = Just $ pif policyComplete (pconstant "") assetName
              , uAssetCount = Just $ pfromData (pscan'assetCount c) + 1
              , uPeaks = Just $ pappendLeaf # pfromData (pscan'assetCount c) # pfromData (pscan'assetPeaks c)
                  # (passetLeafHash # pfromData (pscan'currentPolicy c) # assetName # amount)
              , uValueSize = Just $ pfromData (pscan'cardanoValueSize c)
                  + (plengthBS #$ pencodeDefiniteBytes # assetName)
                  + (plengthBS #$ pserialiseData # pforgetData (pdata amount))
              })

poptionalFieldsComplete :: forall s. Term s PLedgerOutputScanControlV1 -> Term s PBool
poptionalFieldsComplete control = pmatch control $ \c ->
  pfromData (pscan'optionalFieldCount c) + 2 #== pfromData (pscan'mapEntryCount c)

preferenceScriptHeader :: forall s. Term s PLedgerOutputScanControlV1 -> Term s PByteString -> Term s PInteger -> Term s (PMaybe PLedgerOutputScanControlV1)
preferenceScriptHeader control window windowOffset = pmatch control $ \c -> pmatch (pcanonicalKeyAt # window # windowOffset # 3) $ \case
  PNothing -> pcon PNothing
  PJust scriptOffset -> plet (pabsoluteOffset control windowOffset scriptOffset) $ \itemOffset ->
    pmatch (pheadAtV1 # window # scriptOffset # 4) $ \case
      PNothing -> pcon PNothing
      PJust arrayResult -> pmatch arrayResult $ \array -> pif (pcborHead'value array #/= 2) (pcon PNothing) $
        pmatch (pheadAtV1 # window # pcborHead'nextOffset array # 0) $ \case
          PNothing -> pcon PNothing
          PJust languageResult -> pmatch languageResult $ \language ->
            pmatch (pheadAtV1 # window # pcborHead'nextOffset language # 2) $ \case
              PNothing -> pcon PNothing
              PJust payloadResult -> pmatch payloadResult $ \payload ->
                plet (pcborHead'value language) $ \languageTag ->
                pif (pnot # (languageTag #== 0 #|| languageTag #== 3 #|| languageTag #== 128)) (pcon PNothing) $
                  pcon $ PJust $ pupdate control $ emptyUpdate
                    { uStage = Just $ pif (pcborHead'value payload #== 0) pstageTerminal pstageReferenceScriptPayload
                    , uCursor = Just $ pabsoluteOffset control windowOffset (pcborHead'nextOffset payload)
                    , uOptionalCount = Just $ pfromData (pscan'optionalFieldCount c) + 1
                    , uPayloadRemaining = Just $ pcborHead'value payload
                    , uLanguage = Just languageTag, uItemOffset = Just itemOffset
                    , uScriptOffset = Just $ pabsoluteOffset control windowOffset (pcborHead'nextOffset payload)
                    , uScriptLength = Just $ pcborHead'value payload
                    }

pdatumHeader :: forall s. Term s PLedgerOutputScanControlV1 -> Term s PByteString -> Term s PInteger -> Term s PInteger -> Term s (PMaybe PLedgerOutputScanControlV1)
pdatumHeader control window windowOffset datumOffset = pmatch control $ \c ->
  pmatch (pheadAtV1 # window # datumOffset # 2) $ \case
    PNothing -> pcon PNothing
    PJust datumResult -> pmatch datumResult $ \datum ->
      pif (pcborHead'value datum #== 0) (pcon PNothing) $
        pcon $ PJust $ pupdate control $ emptyUpdate
          { uStage = Just pstageDatumPayload
          , uCursor = Just $ pabsoluteOffset control windowOffset (pcborHead'nextOffset datum)
          , uOptionalCount = Just $ pfromData (pscan'optionalFieldCount c) + 1
          , uDatumOffset = Just $ pabsoluteOffset control windowOffset (pcborHead'nextOffset datum)
          , uDatumLength = Just $ pcborHead'value datum
          , uPayloadRemaining = Just $ pcborHead'value datum
          }

pstepOptionalField :: forall s. Term s PLedgerOutputScanControlV1 -> Term s PByteString -> Term s PInteger -> Term s (PMaybe PLedgerOutputScanControlV1)
pstepOptionalField control window windowOffset = pmatch control $ \c ->
  pif (poptionalFieldsComplete control)
    (pcon $ PJust $ pupdate control $ emptyUpdate {uStage = Just pstageTerminal})
    ( pif (pfromData (pscan'mapEntryCount c) #== 4 #&& pfromData (pscan'optionalFieldCount c) #== 0)
        (pmatch (pcanonicalKeyAt # window # windowOffset # 2) $ \case
          PNothing -> pcon PNothing
          PJust datumOffset -> pdatumHeader control window windowOffset datumOffset)
        (pmatch (pheadAtV1 # window # windowOffset # 0) $ \case
          PNothing -> pcon PNothing
          PJust keyResult -> pmatch keyResult $ \key ->
            pif (pcborHead'value key #== 2)
              (pdatumHeader control window windowOffset (pcborHead'nextOffset key))
              (pif (pcborHead'value key #== 3)
                (preferenceScriptHeader control window windowOffset)
                (pcon PNothing)))
    )

pstepPayload :: forall s. Term s PLedgerOutputScanControlV1 -> Term s PInteger -> Term s (PMaybe PLedgerOutputScanControlV1)
pstepPayload control totalLength = pmatch control $ \c ->
  plet (pfromData $ pscan'cursor c) $ \cursor ->
  plet (4_095 - pmod # cursor # 4_095) $ \chunkRemaining ->
  plet (totalLength - cursor) $ \outputRemaining ->
  plet (pfromData $ pscan'payloadRemaining c) $ \payloadRemaining ->
  plet (pif (payloadRemaining #< chunkRemaining) payloadRemaining chunkRemaining) $ \consumed ->
  pif (payloadRemaining #<= 0 #|| consumed #<= 0 #|| consumed #> outputRemaining)
    (pcon PNothing)
    (plet (payloadRemaining - consumed) $ \nextRemaining ->
      pcon $ PJust $ pupdate control $ emptyUpdate
        { uStage = Just $ pif (nextRemaining #== 0) pstageOptionalField (pfromData $ pscan'stage c)
        , uCursor = Just $ cursor + consumed
        , uPayloadRemaining = Just nextRemaining
        })

pstepV1 :: forall s. Term s (PLedgerOutputScanControlV1 :--> PInteger :--> PByteString :--> PInteger :--> PMaybe PLedgerOutputScanControlV1)
pstepV1 = phoistAcyclic $ plam $ \control totalLength window windowOffset -> pmatch control $ \c ->
  plet (pfromData $ pscan'cursor c) $ \cursor ->
  pif
    ( pcontrolIsWellFormed # control #&& cursor #>= 0 #&& cursor #<= totalLength
        #&& windowOffset #>= 0 #&& windowOffset #< plengthBS # window
    )
    ( plet
        (pif (pfromData (pscan'stage c) #== pstageRequiredFields) (pstepRequiredFields control window windowOffset) $
          pif (pfromData (pscan'stage c) #== pstageValueHeader) (pstepValueHeader control window windowOffset) $
            pif (pfromData (pscan'stage c) #== pstagePolicyHeader) (pstepPolicyHeader control window windowOffset) $
              pif (pfromData (pscan'stage c) #== pstageAsset) (pstepAsset control window windowOffset) $
                pif (pfromData (pscan'stage c) #== pstageOptionalField) (pstepOptionalField control window windowOffset) $
                  pif (pfromData (pscan'stage c) #== pstageDatumPayload #|| pfromData (pscan'stage c) #== pstageReferenceScriptPayload)
                    (pstepPayload control totalLength) (pcon PNothing))
        $ \result -> pmatch result $ \case
          PNothing -> pcon PNothing
          PJust next -> pmatch next $ \n ->
            pif
              ( pcontrolIsWellFormed # next
                  #&& pfromData (pscan'cursor n) #>= cursor
                  #&& pfromData (pscan'cursor n) #<= totalLength
                  #&& (pfromData (pscan'cursor n) #> cursor #|| pfromData (pscan'stage n) #/= pfromData (pscan'stage c))
              )
              (pcon $ PJust next)
              (pcon PNothing)
    )
    (pcon PNothing)

pfinishV1 :: forall s. Term s (PLedgerOutputScanControlV1 :--> PInteger :--> PMaybe PLedgerOutputScanControlV1)
pfinishV1 = phoistAcyclic $ plam $ \control totalLength -> pmatch control $ \c ->
  pif
    ( pcontrolIsWellFormed # control
        #&& pfromData (pscan'stage c) #== pstageOptionalField
        #&& poptionalFieldsComplete control
        #&& pfromData (pscan'cursor c) #== totalLength
        #&& pfromData (pscan'payloadRemaining c) #== 0
    )
    (pcon $ PJust $ pupdate control $ emptyUpdate {uStage = Just pstageTerminal})
    (pcon PNothing)

pterminalIsExactV1 :: forall s. Term s (PLedgerOutputScanControlV1 :--> PInteger :--> PBool)
pterminalIsExactV1 = phoistAcyclic $ plam $ \control totalLength -> pmatch control $ \c ->
  plet (pfromData $ pscan'datumOffset c) $ \datumOffset ->
  plet (pfromData $ pscan'datumLength c) $ \datumLength ->
  plet (pfromData $ pscan'referenceScriptLanguage c) $ \language ->
  plet (pfromData $ pscan'referenceScriptItemOffset c) $ \itemOffset ->
  plet (pfromData $ pscan'referenceScriptOffset c) $ \scriptOffset ->
  plet (pfromData $ pscan'referenceScriptLength c) $ \scriptLength -> pand'List
    [ pcontrolIsWellFormed # control
    , pfromData (pscan'stage c) #== pstageTerminal
    , pfromData (pscan'cursor c) #== totalLength
    , pfromData (pscan'mapEntryCount c) #>= 2, pfromData (pscan'mapEntryCount c) #<= 4
    , poptionalFieldsComplete control
    , plengthBS # pfromData (pscan'address c) #== 29 #|| plengthBS # pfromData (pscan'address c) #== 57
    , pfromData (pscan'lovelace c) #>= 0, pfromData (pscan'cardanoValueSize c) #> 0
    , pfromData (pscan'policyRemaining c) #== 0, pfromData (pscan'assetRemaining c) #== 0
    , pfromData (pscan'policyAssetCursor c) #== 0
    , pfromData (pscan'currentPolicy c) #== pconstant ""
    , pfromData (pscan'previousAssetName c) #== pconstant ""
    , pfrontierIsWellFormed # pfromData (pscan'assetCount c) # pfromData (pscan'assetPeaks c)
    , pfromData (pscan'payloadRemaining c) #== 0
    , pif (datumOffset #== -1) (datumLength #== 0)
        (datumOffset #>= 0 #&& datumLength #> 0 #&& datumOffset + datumLength #<= totalLength)
    , pif (language #== -1)
        (itemOffset #== -1 #&& scriptOffset #== -1 #&& scriptLength #== 0)
        ( (language #== 0 #|| language #== 3 #|| language #== 128)
            #&& itemOffset #>= 0 #&& itemOffset #< scriptOffset #&& scriptOffset #>= 0
            #&& scriptLength #>= 0 #&& scriptOffset + scriptLength #== totalLength
        )
    ]
