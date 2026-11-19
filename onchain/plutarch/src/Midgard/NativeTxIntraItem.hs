{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Midgard.NativeTxIntraItem
Description : Plutarch port of @native-tx-intra-item-v1.ak@.

Interior access to an authenticated native-transaction item: §11.1's monotone
value bookmark and §11.2's canonical-data acceptor.
-}
module Midgard.NativeTxIntraItem (
  PValueBookmarkV1,
  pcanonicalKeyPrecedesV1,
  ppolicyIdBytes,
  pmaxAssetNameBytes,
  popenValueBookmark,
  pvalueLovelace,
  pvalueQuantityOf,
  pdatumChildExtentV1,
  pdatumChildBytesV1,
  pdatumBytesAtV1,
  pdatumAlternativeAtV1,
  pdatumIntegerAtV1,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Builtin.ByteString (pbyteStringToInteger, pmostSignificantFirst)
import Plutarch.Core.Internal.Builtins (pindexBS')
import Plutarch.Core.Utils ((#/=))
import Plutarch.Prelude

import Midgard.CanonicalPlutusData (
  pcanonicalDataEndAtV1,
  pdataBytesChunk,
  pgenericConstrTag,
  plargeConstrTagBase,
  pmaxLargeConstrAlternative,
  pmaxSmallConstrAlternative,
  pminGenericConstrAlternative,
  pnegativeBignumTag,
  pisCanonicalPlutusDataV1,
  ppositiveBignumTag,
  psmallConstrTagBase,
 )
import Midgard.IntraItemBytes (pbyteIn, pheadAt, psliceExact)

ppolicyIdBytes, pmaxAssetNameBytes :: forall s. Term s PInteger
ppolicyIdBytes = 28
pmaxAssetNameBytes = 32

pcanonicalKeyPrecedesV1 :: forall s. Term s (PByteString :--> PByteString :--> PBool)
pcanonicalKeyPrecedesV1 = phoistAcyclic $ plam $ \left right ->
  plet (plengthBS # left) $ \leftLen -> plet (plengthBS # right) $ \rightLen ->
    pif (leftLen #< rightLen) (pconstant True) $
      pif (leftLen #> rightLen) (pconstant False) (left #< right)

data PValueBookmarkV1 (s :: S) = PValueBookmarkV1
  { pbookmark'bytes :: Term s (PAsData PByteString)
  , pbookmark'lovelace :: Term s (PAsData PInteger)
  , pbookmark'offset :: Term s (PAsData PInteger)
  , pbookmark'policiesRemaining :: Term s (PAsData PInteger)
  , pbookmark'assetsRemaining :: Term s (PAsData PInteger)
  , pbookmark'policyId :: Term s (PAsData PByteString)
  , pbookmark'queries :: Term s (PAsData PInteger)
  , pbookmark'lastPolicy :: Term s (PAsData PByteString)
  , pbookmark'lastName :: Term s (PAsData PByteString)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PValueBookmarkV1)

pmakeBookmark :: forall s.
  Term s PByteString -> Term s PInteger -> Term s PInteger -> Term s PInteger ->
  Term s PInteger -> Term s PByteString -> Term s PInteger -> Term s PByteString ->
  Term s PByteString -> Term s PValueBookmarkV1
pmakeBookmark bytes lovelace offset policies assets policy queries lastPolicy lastName =
  pcon $ PValueBookmarkV1 (pdata bytes) (pdata lovelace) (pdata offset)
    (pdata policies) (pdata assets) (pdata policy) (pdata queries)
    (pdata lastPolicy) (pdata lastName)

popenValueBookmark :: forall s. Term s (PByteString :--> PValueBookmarkV1)
popenValueBookmark = phoistAcyclic $ plam $ \item ->
  plet (pvalueOffsetInOutput # item) $ \valueOffset ->
    pif (pbyteIn # item # valueOffset #== 0x82)
      (pmatch (pheadAt # item # (valueOffset + 1) # 0) $ \(PPair afterLovelace lovelace) ->
        pmatch (pheadAt # item # afterLovelace # 5) $ \(PPair afterMap policyCount) ->
          plet (pscanPolicyGroups # item # afterMap # policyCount # pconstant "") $ \end ->
            pif (end #<= plengthBS # item)
              (pmakeBookmark item lovelace afterMap policyCount 0 (pconstant "") 0 (pconstant "") (pconstant ""))
              perror)
      perror

pvalueOffsetInOutput :: forall s. Term s (PByteString :--> PInteger)
pvalueOffsetInOutput = phoistAcyclic $ plam $ \item ->
  plet (pbyteIn # item # 0) $ \entries ->
    pif (entries #== 0xa2 #|| entries #== 0xa3 #|| entries #== 0xa4) `flip` perror $
      pif (pbyteIn # item # 1 #== 0 #&& pbyteIn # item # 2 #== 0x58) `flip` perror $
        plet (pbyteIn # item # 3) $ \addressLen ->
          pif (addressLen #== 29 #|| addressLen #== 57) `flip` perror $
            plet (4 + addressLen) $ \keyOffset ->
              pif (pbyteIn # item # keyOffset #== 1) (keyOffset + 1) perror

pscanPolicyGroups :: forall s. Term s (PByteString :--> PInteger :--> PInteger :--> PByteString :--> PInteger)
pscanPolicyGroups = pfix $ \self -> plam $ \item offset remaining previous ->
  pif (remaining #<= 0) offset $
    pmatch (pheadAt # item # offset # 2) $ \(PPair policyOffset policyLen) ->
      pif (policyLen #== ppolicyIdBytes) `flip` perror $
        plet (psliceExact # item # policyOffset # policyLen) $ \policy ->
          pif (previous #== pconstant "" #|| pcanonicalKeyPrecedesV1 # previous # policy) `flip` perror $
            pmatch (pheadAt # item # (policyOffset + policyLen) # 5) $ \(PPair afterMap assetCount) ->
              pif (assetCount #> 0) `flip` perror $
                self # item # (pscanAssets # item # afterMap # assetCount # pconstant "" # pconstant False)
                  # (remaining - 1) # policy

pscanAssets :: forall s. Term s (PByteString :--> PInteger :--> PInteger :--> PByteString :--> PBool :--> PInteger)
pscanAssets = pfix $ \self -> plam $ \item offset remaining previous seen ->
  pif (remaining #<= 0) offset $
    pmatch (pheadAt # item # offset # 2) $ \(PPair nameOffset nameLen) ->
      pif (nameLen #<= pmaxAssetNameBytes) `flip` perror $
        plet (psliceExact # item # nameOffset # nameLen) $ \name ->
          pif (pnot # seen #|| pcanonicalKeyPrecedesV1 # previous # name) `flip` perror $
            pmatch (pheadAt # item # (nameOffset + nameLen) # 0) $ \(PPair afterQuantity quantity) ->
              pif (quantity #> 0)
                (self # item # afterQuantity # (remaining - 1) # name # pconstant True)
                perror

pvalueLovelace :: forall s. Term s (PValueBookmarkV1 :--> PInteger)
pvalueLovelace = phoistAcyclic $ plam $ \bookmark -> pmatch bookmark $ \b -> pfromData (pbookmark'lovelace b)

pvalueQuantityOf :: forall s.
  Term s (PValueBookmarkV1 :--> PByteString :--> PByteString :--> PPair PInteger PValueBookmarkV1)
pvalueQuantityOf = phoistAcyclic $ plam $ \bookmark policy name -> pmatch bookmark $ \b ->
  pif (plengthBS # policy #== ppolicyIdBytes #&& plengthBS # name #<= pmaxAssetNameBytes) `flip` perror $
    pif
      (pfromData (pbookmark'queries b) #== 0
        #|| punitPrecedes (pfromData $ pbookmark'lastPolicy b) (pfromData $ pbookmark'lastName b) policy name)
      (pmatch (psweepToUnit # bookmark # policy # name) $ \(PPair quantity swept) -> pmatch swept $ \s ->
        pcon $ PPair quantity $ pmakeBookmark
          (pfromData $ pbookmark'bytes s) (pfromData $ pbookmark'lovelace s)
          (pfromData $ pbookmark'offset s) (pfromData $ pbookmark'policiesRemaining s)
          (pfromData $ pbookmark'assetsRemaining s) (pfromData $ pbookmark'policyId s)
          (pfromData (pbookmark'queries b) + 1) policy name)
      perror

punitPrecedes :: forall s. Term s PByteString -> Term s PByteString -> Term s PByteString -> Term s PByteString -> Term s PBool
punitPrecedes leftPolicy leftName rightPolicy rightName =
  pif (leftPolicy #== rightPolicy)
    (pcanonicalKeyPrecedesV1 # leftName # rightName)
    (pcanonicalKeyPrecedesV1 # leftPolicy # rightPolicy)

psweepToUnit :: forall s.
  Term s (PValueBookmarkV1 :--> PByteString :--> PByteString :--> PPair PInteger PValueBookmarkV1)
psweepToUnit = pfix $ \sweep -> plam $ \bookmark policy name -> pmatch bookmark $ \b ->
  pif (pfromData (pbookmark'assetsRemaining b) #> 0)
    (pif (pfromData (pbookmark'policyId b) #== policy)
      (psweepWithin sweep # bookmark # policy # name)
      (plet (pskipAssets # pfromData (pbookmark'bytes b) # pfromData (pbookmark'offset b)
              # pfromData (pbookmark'assetsRemaining b)) $ \after ->
        sweep # (pupdatePosition b after (pfromData $ pbookmark'policiesRemaining b) 0 (pconstant "")) # policy # name)) $
  pif (pfromData (pbookmark'policiesRemaining b) #<= 0) (pcon $ PPair 0 bookmark) $
    pmatch (pheadAt # pfromData (pbookmark'bytes b) # pfromData (pbookmark'offset b) # 2) $ \(PPair policyOffset policyLen) ->
      plet (psliceExact # pfromData (pbookmark'bytes b) # policyOffset # policyLen) $ \groupPolicy ->
        pmatch (pheadAt # pfromData (pbookmark'bytes b) # (policyOffset + policyLen) # 5) $ \(PPair afterMap assetCount) ->
          pif (groupPolicy #== policy)
            (psweepWithin sweep # (pupdatePosition b afterMap (pfromData (pbookmark'policiesRemaining b) - 1) assetCount groupPolicy) # policy # name) $
          pif (pcanonicalKeyPrecedesV1 # groupPolicy # policy)
            (plet (pskipAssets # pfromData (pbookmark'bytes b) # afterMap # assetCount) $ \after ->
              sweep # (pupdatePosition b after (pfromData (pbookmark'policiesRemaining b) - 1)
                (pfromData $ pbookmark'assetsRemaining b) (pfromData $ pbookmark'policyId b)) # policy # name)
            (pcon $ PPair 0 bookmark)

psweepWithin :: forall s.
  Term s (PValueBookmarkV1 :--> PByteString :--> PByteString :--> PPair PInteger PValueBookmarkV1) ->
  Term s (PValueBookmarkV1 :--> PByteString :--> PByteString :--> PPair PInteger PValueBookmarkV1)
psweepWithin sweep = pfix $ \within -> plam $ \bookmark policy wanted -> pmatch bookmark $ \b ->
  pif (pfromData (pbookmark'assetsRemaining b) #<= 0)
    (sweep # (pupdatePosition b (pfromData $ pbookmark'offset b)
      (pfromData $ pbookmark'policiesRemaining b) 0 (pconstant "")) # policy # wanted) $
    pmatch (pheadAt # pfromData (pbookmark'bytes b) # pfromData (pbookmark'offset b) # 2) $ \(PPair nameOffset nameLen) ->
      plet (psliceExact # pfromData (pbookmark'bytes b) # nameOffset # nameLen) $ \actual ->
        pmatch (pheadAt # pfromData (pbookmark'bytes b) # (nameOffset + nameLen) # 0) $ \(PPair after quantity) ->
          pif (actual #== wanted)
            (pcon $ PPair quantity $ pupdatePosition b after (pfromData $ pbookmark'policiesRemaining b)
              (pfromData (pbookmark'assetsRemaining b) - 1) (pfromData $ pbookmark'policyId b)) $
          pif (pcanonicalKeyPrecedesV1 # actual # wanted)
            (within # (pupdatePosition b after (pfromData $ pbookmark'policiesRemaining b)
              (pfromData (pbookmark'assetsRemaining b) - 1) (pfromData $ pbookmark'policyId b)) # policy # wanted)
            (pcon $ PPair 0 bookmark)

pupdatePosition :: forall s. PValueBookmarkV1 s -> Term s PInteger -> Term s PInteger -> Term s PInteger -> Term s PByteString -> Term s PValueBookmarkV1
pupdatePosition b offset policies assets policy = pmakeBookmark
  (pfromData $ pbookmark'bytes b) (pfromData $ pbookmark'lovelace b) offset policies assets policy
  (pfromData $ pbookmark'queries b) (pfromData $ pbookmark'lastPolicy b) (pfromData $ pbookmark'lastName b)

pskipAssets :: forall s. Term s (PByteString :--> PInteger :--> PInteger :--> PInteger)
pskipAssets = pfix $ \self -> plam $ \item offset remaining ->
  pif (remaining #<= 0) offset $
    pmatch (pheadAt # item # offset # 2) $ \(PPair nameOffset nameLen) ->
      pmatch (pheadAt # item # (nameOffset + nameLen) # 0) $ \(PPair after _) ->
        self # item # after # (remaining - 1)

pdatumChildExtentV1 ::
  forall s.
  Term s (PByteString :--> PBuiltinList PInteger :--> PMaybe (PPair PInteger PInteger))
pdatumChildExtentV1 = phoistAcyclic $ plam $ \datum path ->
  pif (pisCanonicalPlutusDataV1 # datum)
    (pmatch (pdescend # datum # 0 # path) $ \case
      PNothing -> pcon PNothing
      PJust offset -> pmatch (pcanonicalDataEndAtV1 # datum # offset) $ \case
        PNothing -> pcon PNothing
        PJust end -> pcon $ PJust $ pcon $ PPair offset (end - offset))
    (pcon PNothing)

pdatumChildBytesV1 ::
  forall s.
  Term s (PByteString :--> PBuiltinList PInteger :--> PMaybe PByteString)
pdatumChildBytesV1 = phoistAcyclic $ plam $ \datum path ->
  pmatch (pdatumChildExtentV1 # datum # path) $ \case
    PNothing -> pcon PNothing
    PJust extent -> pmatch extent $ \(PPair offset len) ->
      pcon $ PJust $ psliceExact # datum # offset # len

pdatumBytesAtV1 :: forall s. Term s (PByteString :--> PInteger :--> PMaybe PByteString)
pdatumBytesAtV1 = phoistAcyclic $ plam $ \datum offset ->
  pmatch (pcanonicalDataEndAtV1 # datum # offset) $ \case
    PNothing -> pcon PNothing
    PJust _ ->
      pif (pdiv # (pindexBS' # datum # offset) # 32 #== 2)
        (pbyteStringPayload # datum # offset)
        (pcon PNothing)

pdescend ::
  forall s.
  Term s (PByteString :--> PInteger :--> PBuiltinList PInteger :--> PMaybe PInteger)
pdescend = pfix $ \self -> plam $ \datum offset path -> pmatch path $ \case
  PNil -> pcon $ PJust offset
  PCons index rest -> pmatch (pchildOffset # datum # offset # index) $ \case
    PNothing -> pcon PNothing
    PJust child -> self # datum # child # rest

pchildOffset ::
  forall s.
  Term s (PByteString :--> PInteger :--> PInteger :--> PMaybe PInteger)
pchildOffset = phoistAcyclic $ plam $ \datum offset index ->
  pif (index #< 0) (pcon PNothing) $
    pmatch (pcontainerBody # datum # offset) $ \case
      PNothing -> pcon PNothing
      PJust body -> pmatch body $ \(PPair bodyOffset childCount) ->
        pif (childCount #>= 0)
          (pif (index #>= childCount) (pcon PNothing) (pnthSibling # datum # bodyOffset # index))
          (pnthIndefiniteSibling # datum # bodyOffset # index)

pcontainerBody ::
  forall s.
  Term s (PByteString :--> PInteger :--> PMaybe (PPair PInteger PInteger))
pcontainerBody = phoistAcyclic $ plam $ \datum offset ->
  pif (offset #< 0 #|| offset #>= plengthBS # datum) (pcon PNothing) $
    plet (pindexBS' # datum # offset) $ \first ->
      plet (pdiv # first # 32) $ \major ->
        pif (major #== 4)
          (plistBody datum offset) $
          pif (major #== 5)
            (pmatch (pcanonicalDataEndAtV1 # datum # offset) $ \case
              PNothing -> pcon PNothing
              PJust _ -> pmatch (pheadAt # datum # offset # 5) $ \(PPair bodyOffset entries) ->
                pjustPair bodyOffset (entries * 2)) $
          pif (major #== 6) (pconstrBody # datum # offset) (pcon PNothing)

pconstrBody ::
  forall s.
  Term s (PByteString :--> PInteger :--> PMaybe (PPair PInteger PInteger))
pconstrBody = phoistAcyclic $ plam $ \datum offset ->
  pmatch (pconstrHead # datum # offset) $ \case
    PNothing -> pcon PNothing
    PJust head' -> pmatch head' $ \(PPair argsOffset _) -> plistBody datum argsOffset

-- Constructor arguments are always a list. Keeping this reader separate also
-- makes the source's structural recursion explicit instead of forming a
-- host-language cycle between the two hoisted terms.
plistBody ::
  forall s.
  Term s PByteString -> Term s PInteger -> Term s (PMaybe (PPair PInteger PInteger))
plistBody datum offset =
  pif (offset #< 0 #|| offset #>= plengthBS # datum) (pcon PNothing) $
    plet (pindexBS' # datum # offset) $ \first ->
      pif (first #== 0x80)
        (pjustPair (offset + 1) 0)
        (pif (first #== 0x9f) (pjustPair (offset + 1) (-1)) (pcon PNothing))

pconstrHead ::
  forall s.
  Term s (PByteString :--> PInteger :--> PMaybe (PPair PInteger PInteger))
pconstrHead = phoistAcyclic $ plam $ \datum offset ->
  pif
    (offset #< 0 #|| offset #>= plengthBS # datum
      #|| (pdiv # (pindexBS' # datum # offset) # 32) #/= 6)
    (pcon PNothing) $
      pmatch (pheadAt # datum # offset # 6) $ \(PPair afterTag tag) ->
        pif (tag #>= psmallConstrTagBase #&& tag #<= psmallConstrTagBase + pmaxSmallConstrAlternative)
          (pjustPair afterTag (tag - psmallConstrTagBase)) $
        pif
          (tag #>= plargeConstrTagBase
            #&& tag #<= plargeConstrTagBase + pmaxLargeConstrAlternative - 7)
          (pjustPair afterTag (tag - plargeConstrTagBase + 7)) $
        pif (tag #== pgenericConstrTag)
          (pif (pbyteIn # datum # afterTag #/= 0x82) (pcon PNothing) $
            pmatch (pheadAt # datum # (afterTag + 1) # 0) $ \(PPair afterAlternative alternative) ->
              pif (alternative #>= pminGenericConstrAlternative)
                (pjustPair afterAlternative alternative)
                (pcon PNothing))
          (pcon PNothing)

pdatumAlternativeAtV1 :: forall s. Term s (PByteString :--> PInteger :--> PMaybe PInteger)
pdatumAlternativeAtV1 = phoistAcyclic $ plam $ \datum offset ->
  pmatch (pcanonicalDataEndAtV1 # datum # offset) $ \case
    PNothing -> pcon PNothing
    PJust _ -> pmatch (pconstrHead # datum # offset) $ \case
      PNothing -> pcon PNothing
      PJust head' -> pmatch head' $ \(PPair _ alternative) -> pcon $ PJust alternative

pnthSibling ::
  forall s.
  Term s (PByteString :--> PInteger :--> PInteger :--> PMaybe PInteger)
pnthSibling = pfix $ \self -> plam $ \datum offset index ->
  pif (index #<= 0) (pcon $ PJust offset) $
    pmatch (pcanonicalDataEndAtV1 # datum # offset) $ \case
      PNothing -> pcon PNothing
      PJust next -> self # datum # next # (index - 1)

pnthIndefiniteSibling ::
  forall s.
  Term s (PByteString :--> PInteger :--> PInteger :--> PMaybe PInteger)
pnthIndefiniteSibling = pfix $ \self -> plam $ \datum offset index ->
  pif (offset #< 0 #|| offset #>= plengthBS # datum) (pcon PNothing) $
    pif (pindexBS' # datum # offset #== 0xff) (pcon PNothing) $
      pif (index #<= 0) (pcon $ PJust offset) $
        pmatch (pcanonicalDataEndAtV1 # datum # offset) $ \case
          PNothing -> pcon PNothing
          PJust next -> self # datum # next # (index - 1)

pdatumIntegerAtV1 :: forall s. Term s (PByteString :--> PInteger :--> PMaybe PInteger)
pdatumIntegerAtV1 = phoistAcyclic $ plam $ \datum offset ->
  pif (offset #< 0 #|| offset #>= plengthBS # datum) (pcon PNothing) $
    plet (pdiv # (pindexBS' # datum # offset) # 32) $ \major ->
      pif (major #== 0 #|| major #== 1)
        (pmatch (pcanonicalDataEndAtV1 # datum # offset) $ \case
          PNothing -> pcon PNothing
          PJust _ -> pmatch (pheadAt # datum # offset # major) $ \(PPair _ value) ->
            pcon $ PJust $ pif (major #== 0) value (-1 - value)) $
      pif (major #== 6)
        (pmatch (pcanonicalDataEndAtV1 # datum # offset) $ \case
          PNothing -> pcon PNothing
          PJust _ -> pmatch (pheadAt # datum # offset # 6) $ \(PPair afterTag tag) ->
            pif (tag #== ppositiveBignumTag #|| tag #== pnegativeBignumTag)
              (pmatch (pbyteStringPayload # datum # afterTag) $ \case
                PNothing -> pcon PNothing
                PJust magnitude ->
                  plet (pbyteStringToInteger # pmostSignificantFirst # magnitude) $ \value ->
                    pcon $ PJust $ pif (tag #== ppositiveBignumTag) value (-1 - value))
              (pcon PNothing))
        (pcon PNothing)

pbyteStringPayload :: forall s. Term s (PByteString :--> PInteger :--> PMaybe PByteString)
pbyteStringPayload = phoistAcyclic $ plam $ \datum offset ->
  pif (offset #< 0 #|| offset #>= plengthBS # datum) (pcon PNothing) $
    pif (pindexBS' # datum # offset #== 0x5f)
      (pcon $ PJust $ pconcatChunks # datum # (offset + 1) # pconstant "") $
      pmatch (pheadAt # datum # offset # 2) $ \(PPair payloadOffset len) ->
        pcon $ PJust $ psliceExact # datum # payloadOffset # len

pconcatChunks ::
  forall s.
  Term s (PByteString :--> PInteger :--> PByteString :--> PByteString)
pconcatChunks = pfix $ \self -> plam $ \datum offset accumulated ->
  pif (pbyteIn # datum # offset #== 0xff) accumulated $
    pmatch (pheadAt # datum # offset # 2) $ \(PPair payloadOffset len) ->
      pif (len #<= pdataBytesChunk)
        (self # datum # (payloadOffset + len)
          # (accumulated <> (psliceExact # datum # payloadOffset # len)))
        perror

pjustPair :: forall s a b. Term s a -> Term s b -> Term s (PMaybe (PPair a b))
pjustPair left right = pcon $ PJust $ pcon $ PPair left right
