{-# LANGUAGE OverloadedStrings #-}

module Midgard.NativeScriptScan (
  PVersionedScriptHeaderV1 (..), PNativeScriptTokenV1 (..), PNativeScriptTokenHeadV1 (..),
  PNativeScriptFrameV1 (..), PNativeScriptStructureControlV1 (..),
  PNativeScriptStructureStepResultV1 (..), PNativeFrameResultV1 (..),
  psignatureNode, pallNode, panyNode, patLeastNode, pafterNode, pbeforeNode,
  pmaxNativeScriptNodes, pmaxNativeScriptDepth, pversion,
  pstructureStageToken, pstructureStageFrame, pstructureStageFinalize, pstructureStageTerminal,
  pversionedScriptHeaderV1, ptokenHeadAtV1, ptokenHeadIsWellFormedV1,
  psignaturePayloadAtV1, pallOrAnyPayloadAtV1, patLeastPayloadAtV1, ptimelockPayloadAtV1,
  psignatureTokenFromHeadV1, pcontainerTokenFromHeadV1, ptimelockTokenFromHeadV1, ptokenAtV1,
  pframeIsWellFormedV1, phashFrameV1, pframeForTokenV1, pemptyContainerResultV1, papplyChildV1,
  pstructureControlIsWellFormedV1, pinitialStructureControlV1, pencodeStructureControlV1,
  pstructureControlFromDataV1, pdecodeStructureControlV1, pstructureTokenStepV1,
  pstructureFrameStepV1, pfinalizeStructureV1, pstructureTerminalIsExactV1,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Builtin.Crypto (pblake2b_256)
import Plutarch.Core.Utils (pand'List, (#/=))
import Plutarch.Prelude

import Aiken.Cbor (pdeserialise)
import Midgard.CanonicalCborScan (PCborHeadV1 (..), pheadAtV1)
import Midgard.FraudProofs.NativeTx.Codec (pcborInt, pencodeDefiniteArrayHeader, pencodeDefiniteBytes, psliceLen)

psignatureNode, pallNode, panyNode, patLeastNode, pafterNode, pbeforeNode :: forall s. Term s PInteger
psignatureNode = 0; pallNode = 1; panyNode = 2; patLeastNode = 3; pafterNode = 4; pbeforeNode = 5
pmaxNativeScriptNodes, pmaxNativeScriptDepth, pversion :: forall s. Term s PInteger
pmaxNativeScriptNodes = 16_384; pmaxNativeScriptDepth = 16_384; pversion = 1
pstructureStageToken, pstructureStageFrame, pstructureStageFinalize, pstructureStageTerminal :: forall s. Term s PInteger
pstructureStageToken = 0; pstructureStageFrame = 1; pstructureStageFinalize = 2; pstructureStageTerminal = 3

data PVersionedScriptHeaderV1 s = PVersionedScriptHeaderV1
  { pheader'languageTag :: Term s (PAsData PInteger), pheader'payloadOffset :: Term s (PAsData PInteger), pheader'payloadLength :: Term s (PAsData PInteger) }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PVersionedScriptHeaderV1)

data PNativeScriptTokenV1 s = PNativeScriptTokenV1
  { ptoken'kind :: Term s (PAsData PInteger), ptoken'nextOffset :: Term s (PAsData PInteger)
  , ptoken'childCount :: Term s (PAsData PInteger), ptoken'required :: Term s (PAsData PInteger)
  , ptoken'keyHash :: Term s (PAsData PByteString), ptoken'slot :: Term s (PAsData PInteger) }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PNativeScriptTokenV1)

data PNativeScriptTokenHeadV1 s = PNativeScriptTokenHeadV1
  { ptokenHead'outerLength :: Term s (PAsData PInteger), ptokenHead'tag :: Term s (PAsData PInteger), ptokenHead'nextOffset :: Term s (PAsData PInteger) }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PNativeScriptTokenHeadV1)

data PNativeScriptFrameV1 s = PNativeScriptFrameV1
  { pframe'tail :: Term s (PAsData PByteString), pframe'kind :: Term s (PAsData PInteger)
  , pframe'childCount :: Term s (PAsData PInteger), pframe'remaining :: Term s (PAsData PInteger)
  , pframe'validCount :: Term s (PAsData PInteger), pframe'required :: Term s (PAsData PInteger) }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PNativeScriptFrameV1)

data PNativeScriptStructureControlV1 s = PNativeScriptStructureControlV1
  { pstructure'version :: Term s (PAsData PInteger), pstructure'stage :: Term s (PAsData PInteger)
  , pstructure'startOffset :: Term s (PAsData PInteger), pstructure'cursor :: Term s (PAsData PInteger)
  , pstructure'endOffset :: Term s (PAsData PInteger), pstructure'stackRoot :: Term s (PAsData PByteString)
  , pstructure'stackDepth :: Term s (PAsData PInteger), pstructure'nodeCount :: Term s (PAsData PInteger) }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PNativeScriptStructureControlV1)

data PNativeScriptStructureStepResultV1 s
  = PNativeScriptStructureAdvanced (Term s (PAsData PNativeScriptStructureControlV1))
  | PNativeScriptStructureInvalid | PNativeScriptStructureNodeLimit | PNativeScriptStructureDepthLimit
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PNativeScriptStructureStepResultV1)

data PNativeFrameResultV1 s
  = PNativeFramePending (Term s (PAsData PNativeScriptFrameV1))
  | PNativeFrameComplete (Term s (PAsData PByteString)) (Term s (PAsData PBool))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PNativeFrameResultV1)

pversionedScriptHeaderV1 :: forall s. Term s (PByteString :--> PInteger :--> PMaybe PVersionedScriptHeaderV1)
pversionedScriptHeaderV1 = phoistAcyclic $ plam $ \firstChunk itemLength -> pmatch (pheadAtV1 # firstChunk # 0 # 4) $ \case
  PNothing -> pcon PNothing
  PJust outer -> pmatch outer $ \o -> pif (pcborHead'value o #/= 2) (pcon PNothing) $
    pmatch (pheadAtV1 # firstChunk # pcborHead'nextOffset o # 0) $ \case
      PNothing -> pcon PNothing
      PJust language -> pmatch language $ \l -> pmatch (pheadAtV1 # firstChunk # pcborHead'nextOffset l # 2) $ \case
        PNothing -> pcon PNothing
        PJust payload -> pmatch payload $ \p ->
          pif ((pcborHead'value l #== 0 #|| pcborHead'value l #== 3 #|| pcborHead'value l #== 128)
            #&& pcborHead'value p #>= 0 #&& pcborHead'nextOffset p + pcborHead'value p #== itemLength)
            (pcon $ PJust $ pcon $ PVersionedScriptHeaderV1 (pdata $ pcborHead'value l) (pdata $ pcborHead'nextOffset p) (pdata $ pcborHead'value p))
            (pcon PNothing)

ptokenHeadAtV1 :: forall s. Term s (PByteString :--> PInteger :--> PMaybe PNativeScriptTokenHeadV1)
ptokenHeadAtV1 = phoistAcyclic $ plam $ \window offset -> pmatch (pheadAtV1 # window # offset # 4) $ \case
  PNothing -> pcon PNothing
  PJust outer -> pmatch outer $ \o -> pmatch (pheadAtV1 # window # pcborHead'nextOffset o # 0) $ \case
    PNothing -> pcon PNothing
    PJust tag -> pmatch tag $ \t -> pcon $ PJust $ pcon $ PNativeScriptTokenHeadV1
      (pdata $ pcborHead'value o) (pdata $ pcborHead'value t) (pdata $ pcborHead'nextOffset t)

ptokenHeadIsWellFormedV1 :: forall s. Term s (PNativeScriptTokenHeadV1 :--> PBool)
ptokenHeadIsWellFormedV1 = phoistAcyclic $ plam $ \headValue -> pmatch headValue $ \h ->
  pfromData (ptokenHead'tag h) #>= psignatureNode #&& pfromData (ptokenHead'tag h) #<= pbeforeNode
    #&& pif (pfromData (ptokenHead'tag h) #== patLeastNode)
      (pfromData (ptokenHead'outerLength h) #== 3) (pfromData (ptokenHead'outerLength h) #== 2)

pmkToken :: forall s. Term s PInteger -> Term s PInteger -> Term s PInteger -> Term s PInteger -> Term s PByteString -> Term s PInteger -> Term s PNativeScriptTokenV1
pmkToken kind next children required key slot = pcon $ PNativeScriptTokenV1
  (pdata kind) (pdata next) (pdata children) (pdata required) (pdata key) (pdata slot)

psignaturePayloadAtV1 :: forall s. Term s (PByteString :--> PInteger :--> PInteger :--> PMaybe PNativeScriptTokenV1)
psignaturePayloadAtV1 = phoistAcyclic $ plam $ \window offset absolute -> pmatch (pheadAtV1 # window # offset # 2) $ \case
  PNothing -> pcon PNothing
  PJust hashHead -> pmatch hashHead $ \h -> pif (pcborHead'value h #== 28 #&& pcborHead'nextOffset h + 28 #<= plengthBS # window)
    (pcon $ PJust $ pmkToken psignatureNode (absolute + pcborHead'nextOffset h + 28 - offset) 0 0 (psliceLen # window # pcborHead'nextOffset h # 28) 0)
    (pcon PNothing)

pallOrAnyPayloadAtV1 :: forall s. Term s (PByteString :--> PInteger :--> PInteger :--> PInteger :--> PMaybe PNativeScriptTokenV1)
pallOrAnyPayloadAtV1 = phoistAcyclic $ plam $ \window offset absolute kind ->
  pif (kind #/= pallNode #&& kind #/= panyNode) (pcon PNothing) $ pmatch (pheadAtV1 # window # offset # 4) $ \case
    PNothing -> pcon PNothing
    PJust children -> pmatch children $ \h -> pif (pcborHead'value h #>= 0 #&& pcborHead'value h #<= pmaxNativeScriptNodes)
      (pcon $ PJust $ pmkToken kind (absolute + pcborHead'nextOffset h - offset) (pcborHead'value h) 0 (pconstant "") 0) (pcon PNothing)

patLeastPayloadAtV1 :: forall s. Term s (PByteString :--> PInteger :--> PInteger :--> PMaybe PNativeScriptTokenV1)
patLeastPayloadAtV1 = phoistAcyclic $ plam $ \window offset absolute -> pmatch (pheadAtV1 # window # offset # 0) $ \case
  PNothing -> pcon PNothing
  PJust required -> pmatch required $ \r -> pmatch (pheadAtV1 # window # pcborHead'nextOffset r # 4) $ \case
    PNothing -> pcon PNothing
    PJust children -> pmatch children $ \h -> pif (pcborHead'value r #>= 0 #&& pcborHead'value h #>= 0 #&& pcborHead'value h #<= pmaxNativeScriptNodes)
      (pcon $ PJust $ pmkToken patLeastNode (absolute + pcborHead'nextOffset h - offset) (pcborHead'value h) (pcborHead'value r) (pconstant "") 0)
      (pcon PNothing)

ptimelockPayloadAtV1 :: forall s. Term s (PByteString :--> PInteger :--> PInteger :--> PInteger :--> PMaybe PNativeScriptTokenV1)
ptimelockPayloadAtV1 = phoistAcyclic $ plam $ \window offset absolute kind ->
  pif (kind #/= pafterNode #&& kind #/= pbeforeNode) (pcon PNothing) $ pmatch (pheadAtV1 # window # offset # 0) $ \case
    PNothing -> pcon PNothing
    PJust slot -> pmatch slot $ \h -> pcon $ PJust $ pmkToken kind (absolute + pcborHead'nextOffset h - offset) 0 0 (pconstant "") (pcborHead'value h)

psignatureTokenFromHeadV1 :: forall s. Term s (PByteString :--> PInteger :--> PInteger :--> PNativeScriptTokenHeadV1 :--> PMaybe PNativeScriptTokenV1)
psignatureTokenFromHeadV1 = phoistAcyclic $ plam $ \window offset absolute headValue -> pmatch headValue $ \h ->
  pif (pfromData (ptokenHead'tag h) #/= psignatureNode #|| pfromData (ptokenHead'outerLength h) #/= 2) (pcon PNothing) $
    pmatch (pheadAtV1 # window # pfromData (ptokenHead'nextOffset h) # 2) $ \case
      PNothing -> pcon PNothing
      PJust keyHash -> pmatch keyHash $ \key ->
        pif (pcborHead'value key #== 28 #&& pcborHead'nextOffset key + 28 #<= plengthBS # window)
          (pcon $ PJust $ pmkToken psignatureNode (absolute + pcborHead'nextOffset key + 28 - offset) 0 0
            (psliceLen # window # pcborHead'nextOffset key # 28) 0)
          (pcon PNothing)

pcontainerTokenFromHeadV1 :: forall s. Term s (PByteString :--> PInteger :--> PInteger :--> PNativeScriptTokenHeadV1 :--> PMaybe PNativeScriptTokenV1)
pcontainerTokenFromHeadV1 = phoistAcyclic $ plam $ \window offset absolute headValue -> pmatch headValue $ \h ->
  plet (pfromData $ ptokenHead'tag h) $ \tag -> plet (pfromData $ ptokenHead'outerLength h) $ \outerLength ->
  pif (tag #== pallNode #|| tag #== panyNode)
    (pif (outerLength #== 2)
      (pmatch (pheadAtV1 # window # pfromData (ptokenHead'nextOffset h) # 4) $ \case
        PNothing -> pcon PNothing
        PJust children -> pmatch children $ \childHead ->
          pif (pcborHead'value childHead #>= 0 #&& pcborHead'value childHead #<= pmaxNativeScriptNodes)
            (pcon $ PJust $ pmkToken tag (absolute + pcborHead'nextOffset childHead - offset)
              (pcborHead'value childHead) 0 (pconstant "") 0)
            (pcon PNothing))
      (pcon PNothing))
    (pif (tag #== patLeastNode #&& outerLength #== 3)
      (pmatch (pheadAtV1 # window # pfromData (ptokenHead'nextOffset h) # 0) $ \case
        PNothing -> pcon PNothing
        PJust required -> pmatch required $ \requiredHead ->
          pmatch (pheadAtV1 # window # pcborHead'nextOffset requiredHead # 4) $ \case
            PNothing -> pcon PNothing
            PJust children -> pmatch children $ \childHead ->
              pif (pcborHead'value requiredHead #>= 0 #&& pcborHead'value childHead #>= 0
                  #&& pcborHead'value childHead #<= pmaxNativeScriptNodes)
                (pcon $ PJust $ pmkToken patLeastNode (absolute + pcborHead'nextOffset childHead - offset)
                  (pcborHead'value childHead) (pcborHead'value requiredHead) (pconstant "") 0)
                (pcon PNothing))
      (pcon PNothing))

ptimelockTokenFromHeadV1 :: forall s. Term s (PByteString :--> PInteger :--> PInteger :--> PNativeScriptTokenHeadV1 :--> PMaybe PNativeScriptTokenV1)
ptimelockTokenFromHeadV1 = phoistAcyclic $ plam $ \window offset absolute headValue -> pmatch headValue $ \h ->
  plet (pfromData $ ptokenHead'tag h) $ \tag -> pif ((tag #== pafterNode #|| tag #== pbeforeNode) #&& pfromData (ptokenHead'outerLength h) #== 2)
    (pmatch (pheadAtV1 # window # pfromData (ptokenHead'nextOffset h) # 0) $ \case
      PNothing -> pcon PNothing
      PJust slot -> pmatch slot $ \slotHead -> pcon $ PJust $ pmkToken tag
        (absolute + pcborHead'nextOffset slotHead - offset) 0 0 (pconstant "") (pcborHead'value slotHead))
    (pcon PNothing)

ptokenAtV1 :: forall s. Term s (PByteString :--> PInteger :--> PInteger :--> PMaybe PNativeScriptTokenV1)
ptokenAtV1 = phoistAcyclic $ plam $ \window offset absolute -> pmatch (ptokenHeadAtV1 # window # offset) $ \case
  PNothing -> pcon PNothing
  PJust headValue -> pmatch headValue $ \h -> plet (pfromData $ ptokenHead'tag h) $ \tag ->
    pif (tag #== psignatureNode) (psignatureTokenFromHeadV1 # window # offset # absolute # headValue) $
      pif (tag #>= pallNode #&& tag #<= patLeastNode) (pcontainerTokenFromHeadV1 # window # offset # absolute # headValue)
        (ptimelockTokenFromHeadV1 # window # offset # absolute # headValue)

pframeIsWellFormedV1 :: forall s. Term s (PNativeScriptFrameV1 :--> PBool)
pframeIsWellFormedV1 = phoistAcyclic $ plam $ \frame -> pmatch frame $ \f ->
  plet (pfromData (pframe'childCount f) - pfromData (pframe'remaining f)) $ \processed -> pand'List
    [ plengthBS # pfromData (pframe'tail f) #== 0 #|| plengthBS # pfromData (pframe'tail f) #== 32
    , pfromData (pframe'kind f) #>= pallNode, pfromData (pframe'kind f) #<= patLeastNode
    , pfromData (pframe'childCount f) #> 0, pfromData (pframe'childCount f) #<= pmaxNativeScriptNodes
    , pfromData (pframe'remaining f) #> 0, pfromData (pframe'remaining f) #<= pfromData (pframe'childCount f)
    , pfromData (pframe'validCount f) #>= 0, pfromData (pframe'validCount f) #<= processed
    , pif (pfromData (pframe'kind f) #== patLeastNode) (pfromData (pframe'required f) #>= 0) (pfromData (pframe'required f) #== 0)
    ]

pframeDomain :: forall s. Term s PByteString
pframeDomain = pconstant "MidgardNativeScriptScanFrameV1"

phashFrameV1 :: forall s. Term s (PNativeScriptFrameV1 :--> PByteString)
phashFrameV1 = phoistAcyclic $ plam $ \frame -> pif (pframeIsWellFormedV1 # frame)
  (pmatch frame $ \f -> pblake2b_256 #$ pframeDomain <> (pencodeDefiniteArrayHeader # 6)
    <> (pencodeDefiniteBytes # pfromData (pframe'tail f)) <> pcborInt (pfromData $ pframe'kind f)
    <> pcborInt (pfromData $ pframe'childCount f) <> pcborInt (pfromData $ pframe'remaining f)
    <> pcborInt (pfromData $ pframe'validCount f) <> pcborInt (pfromData $ pframe'required f))
  perror

pframeForTokenV1 :: forall s. Term s (PNativeScriptTokenV1 :--> PByteString :--> PMaybe PNativeScriptFrameV1)
pframeForTokenV1 = phoistAcyclic $ plam $ \token tailValue -> pmatch token $ \t ->
  pif (pfromData (ptoken'kind t) #>= pallNode #&& pfromData (ptoken'kind t) #<= patLeastNode
      #&& pfromData (ptoken'childCount t) #> 0 #&& pfromData (ptoken'childCount t) #<= pmaxNativeScriptNodes)
    (pcon $ PJust $ pcon $ PNativeScriptFrameV1 (pdata tailValue) (ptoken'kind t) (ptoken'childCount t)
      (ptoken'childCount t) (pdata 0) (ptoken'required t))
    (pcon PNothing)

pemptyContainerResultV1 :: forall s. Term s (PNativeScriptTokenV1 :--> PMaybe PBool)
pemptyContainerResultV1 = phoistAcyclic $ plam $ \token -> pmatch token $ \t ->
  pif (pfromData (ptoken'childCount t) #/= 0) (pcon PNothing) $
    pif (pfromData (ptoken'kind t) #== pallNode) (pcon $ PJust $ pconstant True) $
      pif (pfromData (ptoken'kind t) #== panyNode) (pcon $ PJust $ pconstant False) $
        pif (pfromData (ptoken'kind t) #== patLeastNode) (pcon $ PJust $ pfromData (ptoken'required t) #== 0) (pcon PNothing)

papplyChildV1 :: forall s. Term s (PNativeScriptFrameV1 :--> PBool :--> PMaybe PNativeFrameResultV1)
papplyChildV1 = phoistAcyclic $ plam $ \frame childValid -> pmatch frame $ \f ->
  pif (pnot # (pframeIsWellFormedV1 # frame)) (pcon PNothing) $
    plet (pfromData (pframe'validCount f) + pif childValid 1 0) $ \validCount ->
    pif (pfromData (pframe'remaining f) #== 1)
      (plet (pif (pfromData (pframe'kind f) #== pallNode) (validCount #== pfromData (pframe'childCount f)) $
        pif (pfromData (pframe'kind f) #== panyNode) (validCount #> 0) (validCount #>= pfromData (pframe'required f))) $ \valid ->
          pcon $ PJust $ pcon $ PNativeFrameComplete (pframe'tail f) (pdata valid))
      (pcon $ PJust $ pcon $ PNativeFramePending $ pdata $ pcon $ PNativeScriptFrameV1
        (pframe'tail f) (pframe'kind f) (pframe'childCount f) (pdata $ pfromData (pframe'remaining f) - 1)
        (pdata validCount) (pframe'required f))

pstructureControlIsWellFormedV1 :: forall s. Term s (PNativeScriptStructureControlV1 :--> PBool)
pstructureControlIsWellFormedV1 = phoistAcyclic $ plam $ \control -> pmatch control $ \c ->
  plet (pfromData $ pstructure'stage c) $ \stage ->
  plet (pfromData $ pstructure'startOffset c) $ \start ->
  plet (pfromData $ pstructure'cursor c) $ \cursor ->
  plet (pfromData $ pstructure'endOffset c) $ \end ->
  plet (pfromData $ pstructure'stackDepth c) $ \depth ->
  plet (pfromData $ pstructure'nodeCount c) $ \count ->
  plet (pfromData $ pstructure'stackRoot c) $ \root -> pand'List
    [ pfromData (pstructure'version c) #== pversion, stage #>= pstructureStageToken, stage #<= pstructureStageTerminal
    , start #>= 0, end #> start, cursor #>= start, cursor #<= end
    , depth #>= 0, depth #<= pmaxNativeScriptDepth, count #>= 0, count #<= pmaxNativeScriptNodes
    , pif (depth #== 0) (root #== pconstant "") (plengthBS # root #== 32)
    , pif (stage #== pstructureStageToken) (cursor #< end) $
        pif (stage #== pstructureStageFrame) (depth #> 0 #&& plengthBS # root #== 32) $
          pif (stage #== pstructureStageFinalize) (depth #== 0 #&& root #== pconstant "")
            (cursor #== end #&& depth #== 0 #&& root #== pconstant "" #&& count #> 0)
    ]

pinitialStructureControlV1 :: forall s. Term s (PInteger :--> PInteger :--> PNativeScriptStructureControlV1)
pinitialStructureControlV1 = phoistAcyclic $ plam $ \start totalLength ->
  plet (pcon $ PNativeScriptStructureControlV1 (pdata pversion) (pdata pstructureStageToken) (pdata start)
    (pdata start) (pdata $ start + totalLength) (pdata $ pconstant "") (pdata 0) (pdata 0)) $ \control ->
      pif (pstructureControlIsWellFormedV1 # control) control perror

pencodeStructureControlV1 :: forall s. Term s (PNativeScriptStructureControlV1 :--> PByteString)
pencodeStructureControlV1 = phoistAcyclic $ plam $ \control -> pif (pstructureControlIsWellFormedV1 # control)
  (pmatch control $ \c -> (pencodeDefiniteArrayHeader # 8) <> pcborInt pversion <> pcborInt (pfromData $ pstructure'stage c)
    <> pcborInt (pfromData $ pstructure'startOffset c) <> pcborInt (pfromData $ pstructure'cursor c)
    <> pcborInt (pfromData $ pstructure'endOffset c) <> (pencodeDefiniteBytes # pfromData (pstructure'stackRoot c))
    <> pcborInt (pfromData $ pstructure'stackDepth c) <> pcborInt (pfromData $ pstructure'nodeCount c)) perror

pstructureControlFromDataV1 :: forall s. Term s (PData :--> PNativeScriptStructureControlV1)
pstructureControlFromDataV1 = phoistAcyclic $ plam $ \dat -> plet (pasList # dat) $ \xs -> pif (plength # xs #== 8)
  (plet (pcon $ PNativeScriptStructureControlV1
    (pdata $ pasInt # (pelemAt # 0 # xs)) (pdata $ pasInt # (pelemAt # 1 # xs))
    (pdata $ pasInt # (pelemAt # 2 # xs)) (pdata $ pasInt # (pelemAt # 3 # xs))
    (pdata $ pasInt # (pelemAt # 4 # xs)) (pdata $ pasByteStr # (pelemAt # 5 # xs))
    (pdata $ pasInt # (pelemAt # 6 # xs)) (pdata $ pasInt # (pelemAt # 7 # xs))) $ \control ->
      pif (pstructureControlIsWellFormedV1 # control) control perror) perror

pdecodeStructureControlV1 :: forall s. Term s (PByteString :--> PNativeScriptStructureControlV1)
pdecodeStructureControlV1 = phoistAcyclic $ plam $ \cbor -> pmatch (pdeserialise # cbor) $ \case
  PNothing -> perror
  PJust dat -> plet (pstructureControlFromDataV1 # dat) $ \control -> pif (pencodeStructureControlV1 # control #== cbor) control perror

pmkControl :: forall s. Term s PInteger -> Term s PInteger -> Term s PInteger -> Term s PInteger -> Term s PByteString -> Term s PInteger -> Term s PInteger -> Term s PNativeScriptStructureControlV1
pmkControl stage start cursor end root depth count = pcon $ PNativeScriptStructureControlV1
  (pdata pversion) (pdata stage) (pdata start) (pdata cursor) (pdata end) (pdata root) (pdata depth) (pdata count)

padvancedStructure :: forall s. Term s PNativeScriptStructureControlV1 -> Term s PNativeScriptStructureStepResultV1
padvancedStructure control = pif (pstructureControlIsWellFormedV1 # control)
  (pcon $ PNativeScriptStructureAdvanced $ pdata control) (pcon PNativeScriptStructureInvalid)

pcompleteStructureNode :: forall s. Term s PNativeScriptStructureControlV1 -> Term s PInteger -> Term s PInteger -> Term s PNativeScriptStructureStepResultV1
pcompleteStructureNode control cursor count = pmatch control $ \c -> padvancedStructure $ pmkControl
  (pif (pfromData (pstructure'stackDepth c) #> 0) pstructureStageFrame pstructureStageFinalize)
  (pfromData $ pstructure'startOffset c) cursor (pfromData $ pstructure'endOffset c)
  (pfromData $ pstructure'stackRoot c) (pfromData $ pstructure'stackDepth c) count

pstructureTokenStepV1 :: forall s. Term s (PNativeScriptStructureControlV1 :--> PByteString :--> PInteger :--> PMaybe PNativeScriptStructureStepResultV1)
pstructureTokenStepV1 = phoistAcyclic $ plam $ \control window windowOffset -> pmatch control $ \c ->
  pif (pstructureControlIsWellFormedV1 # control #&& pfromData (pstructure'stage c) #== pstructureStageToken
      #&& windowOffset #>= 0 #&& windowOffset #< plengthBS # window)
    (pmatch (ptokenAtV1 # window # windowOffset # pfromData (pstructure'cursor c)) $ \case
      PNothing -> pcon $ PJust $ pcon PNativeScriptStructureInvalid
      PJust token -> pmatch token $ \t ->
        plet (pfromData $ ptoken'nextOffset t) $ \next ->
        pif (next #<= pfromData (pstructure'cursor c) #|| next #> pfromData (pstructure'endOffset c))
          (pcon $ PJust $ pcon PNativeScriptStructureInvalid)
          (plet (pfromData (pstructure'nodeCount c) + 1) $ \count ->
            pif (count #> pmaxNativeScriptNodes) (pcon $ PJust $ pcon PNativeScriptStructureNodeLimit) $
              pif (pfromData (ptoken'kind t) #>= pallNode #&& pfromData (ptoken'kind t) #<= patLeastNode
                    #&& pfromData (ptoken'childCount t) #> 0)
                (plet (pfromData (pstructure'stackDepth c) + 1) $ \depth ->
                  pif (depth #> pmaxNativeScriptDepth) (pcon $ PJust $ pcon PNativeScriptStructureDepthLimit) $
                    pmatch (pframeForTokenV1 # token # pfromData (pstructure'stackRoot c)) $ \case
                      PNothing -> pcon $ PJust $ pcon PNativeScriptStructureInvalid
                      PJust frame -> pcon $ PJust $ padvancedStructure $ pmkControl pstructureStageToken
                        (pfromData $ pstructure'startOffset c) next (pfromData $ pstructure'endOffset c)
                        (phashFrameV1 # frame) depth count)
                (pcon $ PJust $ pcompleteStructureNode control next count)))
    (pcon PNothing)

pstructureFrameStepV1 :: forall s. Term s (PNativeScriptStructureControlV1 :--> PNativeScriptFrameV1 :--> PMaybe PNativeScriptStructureStepResultV1)
pstructureFrameStepV1 = phoistAcyclic $ plam $ \control frame -> pmatch control $ \c ->
  pif (pstructureControlIsWellFormedV1 # control #&& pfromData (pstructure'stage c) #== pstructureStageFrame
      #&& pframeIsWellFormedV1 # frame #&& phashFrameV1 # frame #== pfromData (pstructure'stackRoot c))
    (pmatch (papplyChildV1 # frame # pconstant False) $ \case
      PNothing -> pcon PNothing
      PJust result -> pmatch result $ \case
        PNativeFramePending nextFrame -> pcon $ PJust $ padvancedStructure $ pmkControl pstructureStageToken
          (pfromData $ pstructure'startOffset c) (pfromData $ pstructure'cursor c) (pfromData $ pstructure'endOffset c)
          (phashFrameV1 # pfromData nextFrame) (pfromData $ pstructure'stackDepth c) (pfromData $ pstructure'nodeCount c)
        PNativeFrameComplete tailValue _ ->
          plet (pfromData (pstructure'stackDepth c) - 1) $ \depth -> pcon $ PJust $ padvancedStructure $ pmkControl
            (pif (depth #> 0) pstructureStageFrame pstructureStageFinalize)
            (pfromData $ pstructure'startOffset c) (pfromData $ pstructure'cursor c) (pfromData $ pstructure'endOffset c)
            (pfromData tailValue) depth (pfromData $ pstructure'nodeCount c))
    (pcon PNothing)

pfinalizeStructureV1 :: forall s. Term s (PNativeScriptStructureControlV1 :--> PMaybe PNativeScriptStructureStepResultV1)
pfinalizeStructureV1 = phoistAcyclic $ plam $ \control -> pmatch control $ \c ->
  pif (pstructureControlIsWellFormedV1 # control #&& pfromData (pstructure'stage c) #== pstructureStageFinalize)
    (pif (pfromData (pstructure'cursor c) #== pfromData (pstructure'endOffset c) #&& pfromData (pstructure'nodeCount c) #> 0)
      (pcon $ PJust $ padvancedStructure $ pmkControl pstructureStageTerminal
        (pfromData $ pstructure'startOffset c) (pfromData $ pstructure'cursor c) (pfromData $ pstructure'endOffset c)
        (pfromData $ pstructure'stackRoot c) (pfromData $ pstructure'stackDepth c) (pfromData $ pstructure'nodeCount c))
      (pcon $ PJust $ pcon PNativeScriptStructureInvalid))
    (pcon PNothing)

pstructureTerminalIsExactV1 :: forall s. Term s (PNativeScriptStructureControlV1 :--> PBool)
pstructureTerminalIsExactV1 = phoistAcyclic $ plam $ \control -> pmatch control $ \c ->
  pstructureControlIsWellFormedV1 # control #&& pfromData (pstructure'stage c) #== pstructureStageTerminal
