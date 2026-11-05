{-# LANGUAGE OverloadedStrings #-}

module Midgard.NativeScript (
  PNativeScriptProofV1 (..),
  pmaxNativeScriptDepth, pmaxNativeScriptNodeCount,
  pinspectNativeScriptV1, pcheckNativeScriptV1,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Prelude

import Aiken.Cbor (pdeserialise)
import Midgard.FraudProofs.NativeTx.Codec (pcborInt, pencodeDefiniteArrayHeader, pencodeDefiniteBytes)

pmaxNativeScriptDepth, pmaxNativeScriptNodeCount :: forall s. Term s PInteger
pmaxNativeScriptDepth = 16
pmaxNativeScriptNodeCount = 32

data PNativeScriptProofV1 s = PNativeScriptProofV1
  { pnativeProof'canonicalCbor :: Term s (PAsData PByteString)
  , pnativeProof'valid :: Term s (PAsData PBool)
  , pnativeProof'nodeCount :: Term s (PAsData PInteger)
  , pnativeProof'depth :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PNativeScriptProofV1)

data PNativeChildrenCheck s = PNativeChildrenCheck
  { pchildren'canonicalChildren :: Term s (PAsData PByteString)
  , pchildren'allValid :: Term s (PAsData PBool)
  , pchildren'anyValid :: Term s (PAsData PBool)
  , pchildren'validCount :: Term s (PAsData PInteger)
  , pchildren'nodeCount :: Term s (PAsData PInteger)
  , pchildren'maxDepth :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PNativeChildrenCheck)

plistDataMaybe :: forall s. Term s (PData :--> PMaybe (PBuiltinList PData))
plistDataMaybe = phoistAcyclic $ plam $ \d -> pforce $
  pchooseData # d
    # pdelay (pcon PNothing)
    # pdelay (pcon PNothing)
    # pdelay (pcon $ PJust $ pasList # d)
    # pdelay (pcon PNothing)
    # pdelay (pcon PNothing)

pintDataMaybe :: forall s. Term s (PData :--> PMaybe PInteger)
pintDataMaybe = phoistAcyclic $ plam $ \d -> pforce $
  pchooseData # d
    # pdelay (pcon PNothing)
    # pdelay (pcon PNothing)
    # pdelay (pcon PNothing)
    # pdelay (pcon $ PJust $ pasInt # d)
    # pdelay (pcon PNothing)

pbytesDataMaybe :: forall s. Term s (PData :--> PMaybe PByteString)
pbytesDataMaybe = phoistAcyclic $ plam $ \d -> pforce $
  pchooseData # d
    # pdelay (pcon PNothing)
    # pdelay (pcon PNothing)
    # pdelay (pcon PNothing)
    # pdelay (pcon PNothing)
    # pdelay (pcon $ PJust $ pasByteStr # d)

pemptyChildren :: forall s. Term s PNativeChildrenCheck
pemptyChildren = pcon $ PNativeChildrenCheck
  (pdata $ pconstant "") (pdata $ pconstant True) (pdata $ pconstant False)
  (pdata 0) (pdata 0) (pdata 0)

pcheckChildren :: forall s.
  Term s (PData :--> PInteger :--> PInteger :--> PBuiltinList PByteString :--> PMaybe PNativeScriptProofV1) ->
  Term s (PBuiltinList PData :--> PInteger :--> PInteger :--> PBuiltinList PByteString :--> PMaybe PNativeChildrenCheck)
pcheckChildren checkData = pfix $ \self -> plam $ \children start end signers -> pmatch children $ \case
  PNil -> pcon $ PJust pemptyChildren
  PCons child rest -> pmatch (checkData # child # start # end # signers) $ \case
    PNothing -> pcon PNothing
    PJust checkedChild -> pmatch (self # rest # start # end # signers) $ \case
      PNothing -> pcon PNothing
      PJust checkedRest -> pmatch checkedChild $ \childFields -> pmatch checkedRest $ \restFields ->
        pcon $ PJust $ pcon $ PNativeChildrenCheck
          (pdata $ pfromData (pnativeProof'canonicalCbor childFields) <> pfromData (pchildren'canonicalChildren restFields))
          (pdata $ pfromData (pnativeProof'valid childFields) #&& pfromData (pchildren'allValid restFields))
          (pdata $ pfromData (pnativeProof'valid childFields) #|| pfromData (pchildren'anyValid restFields))
          (pdata $ pfromData (pchildren'validCount restFields)
            + pif (pfromData $ pnativeProof'valid childFields) 1 0)
          (pdata $ pfromData (pnativeProof'nodeCount childFields) + pfromData (pchildren'nodeCount restFields))
          (pdata $ pmax (pfromData $ pnativeProof'depth childFields) (pfromData $ pchildren'maxDepth restFields))

pcheckTwoFields :: forall s.
  Term s (PData :--> PInteger :--> PInteger :--> PBuiltinList PByteString :--> PMaybe PNativeScriptProofV1) ->
  Term s PData -> Term s PData -> Term s PInteger -> Term s PInteger -> Term s (PBuiltinList PByteString) ->
  Term s (PMaybe PNativeScriptProofV1)
pcheckTwoFields checkData tagData argumentData start end signers = pmatch (pintDataMaybe # tagData) $ \case
  PNothing -> pcon PNothing
  PJust tag ->
    pif (tag #== 0)
      (pmatch (pbytesDataMaybe # argumentData) $ \case
        PNothing -> pcon PNothing
        PJust keyHash -> pif (plengthBS # keyHash #== 28)
          (pcon $ PJust $ pcon $ PNativeScriptProofV1
            (pdata $ pencodeDefiniteArrayHeader # 2 <> pcborInt tag <> pencodeDefiniteBytes # keyHash)
            (pdata $ pelem # keyHash # signers) (pdata 1) (pdata 1)) (pcon PNothing)) $
    pif (tag #== 1 #|| tag #== 2)
      (pmatch (plistDataMaybe # argumentData) $ \case
        PNothing -> pcon PNothing
        PJust children -> pmatch (pcheckChildren checkData # children # start # end # signers) $ \case
          PNothing -> pcon PNothing
          PJust checked -> pmatch checked $ \c -> pcon $ PJust $ pcon $ PNativeScriptProofV1
            (pdata $ pencodeDefiniteArrayHeader # 2 <> pcborInt tag
              <> pencodeDefiniteArrayHeader # (plength # children) <> pfromData (pchildren'canonicalChildren c))
            (pdata $ pif (tag #== 1) (pfromData $ pchildren'allValid c) (pfromData $ pchildren'anyValid c))
            (pdata $ pfromData (pchildren'nodeCount c) + 1) (pdata $ pfromData (pchildren'maxDepth c) + 1)) $
    pif (tag #== 4 #|| tag #== 5)
      (pmatch (pintDataMaybe # argumentData) $ \case
        PNothing -> pcon PNothing
        PJust slot -> pif (slot #>= 0)
          (pcon $ PJust $ pcon $ PNativeScriptProofV1
            (pdata $ pencodeDefiniteArrayHeader # 2 <> pcborInt tag <> pcborInt slot)
            (pdata $ pif (tag #== 4) (start #>= 0 #&& start #>= slot) (end #>= 0 #&& end #<= slot))
            (pdata 1) (pdata 1)) (pcon PNothing))
      (pcon PNothing)

pcheckThreeFields :: forall s.
  Term s (PData :--> PInteger :--> PInteger :--> PBuiltinList PByteString :--> PMaybe PNativeScriptProofV1) ->
  Term s PData -> Term s PData -> Term s PData -> Term s PInteger -> Term s PInteger ->
  Term s (PBuiltinList PByteString) -> Term s (PMaybe PNativeScriptProofV1)
pcheckThreeFields checkData tagData requiredData childrenData start end signers =
  pmatch (pintDataMaybe # tagData) $ \case
    PNothing -> pcon PNothing
    PJust tag -> pif (tag #== 3)
      (pmatch (pintDataMaybe # requiredData) $ \case
        PNothing -> pcon PNothing
        PJust required -> pmatch (plistDataMaybe # childrenData) $ \case
          PNothing -> pcon PNothing
          PJust children -> pif (required #>= 0)
            (pmatch (pcheckChildren checkData # children # start # end # signers) $ \case
              PNothing -> pcon PNothing
              PJust checked -> pmatch checked $ \c -> pcon $ PJust $ pcon $ PNativeScriptProofV1
                (pdata $ pencodeDefiniteArrayHeader # 3 <> pcborInt tag <> pcborInt required
                  <> pencodeDefiniteArrayHeader # (plength # children) <> pfromData (pchildren'canonicalChildren c))
                (pdata $ pfromData (pchildren'validCount c) #>= required)
                (pdata $ pfromData (pchildren'nodeCount c) + 1) (pdata $ pfromData (pchildren'maxDepth c) + 1))
            (pcon PNothing))
      (pcon PNothing)

pcheckNativeScriptData :: forall s.
  Term s (PData :--> PInteger :--> PInteger :--> PBuiltinList PByteString :--> PMaybe PNativeScriptProofV1)
pcheckNativeScriptData = phoistAcyclic $ pfix $ \self -> plam $ \dat start end signers ->
  pmatch (plistDataMaybe # dat) $ \case
    PNothing -> pcon PNothing
    PJust fields -> plet (plength # fields) $ \fieldCount ->
      pif (fieldCount #== 2)
        (pcheckTwoFields self (pelemAt # 0 # fields) (pelemAt # 1 # fields) start end signers) $
      pif (fieldCount #== 3)
        (pcheckThreeFields self (pelemAt # 0 # fields) (pelemAt # 1 # fields) (pelemAt # 2 # fields)
          start end signers)
        (pcon PNothing)

pinspectNativeScriptV1 :: forall s.
  Term s (PByteString :--> PInteger :--> PInteger :--> PBuiltinList PByteString :--> PMaybe PNativeScriptProofV1)
pinspectNativeScriptV1 = phoistAcyclic $ plam $ \script start end signers ->
  pmatch (pdeserialise # script) $ \case
    PNothing -> pcon PNothing
    PJust dat -> pmatch (pcheckNativeScriptData # dat # start # end # signers) $ \case
      PNothing -> pcon PNothing
      PJust checked -> pmatch checked $ \c ->
        pif (pfromData (pnativeProof'canonicalCbor c) #== script) (pcon $ PJust checked) (pcon PNothing)

pcheckNativeScriptV1 :: forall s.
  Term s (PByteString :--> PInteger :--> PInteger :--> PBuiltinList PByteString :--> PMaybe PNativeScriptProofV1)
pcheckNativeScriptV1 = phoistAcyclic $ plam $ \script start end signers ->
  pmatch (pinspectNativeScriptV1 # script # start # end # signers) $ \case
    PNothing -> pcon PNothing
    PJust checked -> pmatch checked $ \c ->
      pif (pfromData (pnativeProof'depth c) #<= pmaxNativeScriptDepth
          #&& pfromData (pnativeProof'nodeCount c) #<= pmaxNativeScriptNodeCount)
        (pcon $ PJust checked) (pcon PNothing)
