-- | Exact semantic checks consumed by the nineteen shared item executors.
module Midgard.ScriptSourcesItemSemantics where

import Midgard.CekDataBytes qualified as Bytes
import Midgard.CekDataInteger qualified as Integer
import Midgard.CekDataTraverse qualified as Traverse
import Midgard.RedeemerItemProof qualified as Item
import Midgard.ScriptSourcesItemNormalization qualified as Normalized
import Plutarch.Core.Utils ((#/=))
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.Prelude

poptionalBytes :: forall s. Term s (PMaybeData PByteString) -> Term s (PMaybe PByteString)
poptionalBytes source = pmatch source $ \case PDNothing -> pcon PNothing; PDJust bytes -> pcon $ PJust $ pfromData bytes
pexpectBytes :: forall s. Term s (PMaybeData PByteString) -> Term s PByteString
pexpectBytes source = pmatch source $ \case PDNothing -> perror; PDJust bytes -> pfromData bytes

traversalStep :: forall s. Term s PInteger -> (Term s Traverse.PDataTraverseControlV1 -> Term s Traverse.PDataTraverseActionV1 -> Term s (PMaybeData PByteString) -> Term s (PMaybe Traverse.PDataTraverseControlV1)) -> Term s (Normalized.PTraversalExecution :--> PBool)
traversalStep stage evaluate = plam $ \state -> pmatch state $ \s ->
  plet (pfromData $ Normalized.pexecution'current s) $ \current -> pmatch current $ \c ->
  pif (pfromData (Traverse.ptraverse'stage c) #== stage)
    (pmatch (evaluate current (pfromData $ Normalized.pexecution'action s) (pfromData $ Normalized.pexecution'sourceBytes s)) $ \case
      PNothing -> pconstant False
      PJust next -> next #== pfromData (Normalized.pexecution'next s)) perror

pheadScalar, pheadSequence, pheadMap, pheadLargeConstructor :: forall s. Term s (Normalized.PTraversalExecution :--> PBool)
pheadScalar = traversalStep Traverse.pstageHead $ \current action source -> pmatch action $ \case
  Traverse.PHeadScalar length -> Traverse.pprevalidatedHeadScalar # current # pexpectBytes source # pfromData length
  _ -> perror
pheadSequence = traversalStep Traverse.pstageHead $ \current action source -> pmatch action $ \case
  Traverse.PHeadSequence count -> Traverse.pprevalidatedHeadSequence # current # pexpectBytes source # pfromData count
  _ -> perror
pheadMap = traversalStep Traverse.pstageHead $ \current action source -> pmatch action $ \case
  Traverse.PHeadMap -> Traverse.pprevalidatedHeadMap # current # pexpectBytes source
  _ -> perror
pheadLargeConstructor = traversalStep Traverse.pstageHead $ \current action source -> pmatch action $ \case
  Traverse.PHeadLargeConstructor length count -> Traverse.pprevalidatedHeadLargeConstructor # current # pexpectBytes source # pfromData length # pfromData count
  _ -> perror

pattachInteger, pattachBytes, padvanceInteger, padvanceBytes :: forall s. Term s (Normalized.PTraversalExecution :--> PBool)
pattachInteger = traversalStep Traverse.pstageInteger $ \current action source -> pmatch action $ \case
  Traverse.PAttachScalar parent -> pif (source #== pcon PDNothing) (Traverse.pprevalidatedAttachInteger # current # pfromData parent) perror
  _ -> perror
pattachBytes = traversalStep Traverse.pstageBytes $ \current action source -> pmatch action $ \case
  Traverse.PAttachScalar parent -> pif (source #== pcon PDNothing) (Traverse.pprevalidatedAttachBytes # current # pfromData parent) perror
  _ -> perror
padvanceInteger = traversalStep Traverse.pstageInteger $ \current action source -> pmatch current $ \c ->
  pmatch (pfromData $ Traverse.ptraverse'integer c) $ \case
    PDNothing -> perror
    PDJust scalar -> pmatch (pfromData scalar) $ \i ->
      pif (action #== pcon Traverse.PNoAction #&& pfromData (Integer.pint'stage i) #/= Integer.pstageTerminal)
        (Traverse.pprevalidatedAdvanceInteger # current # poptionalBytes source) perror
padvanceBytes = traversalStep Traverse.pstageBytes $ \current action source -> pmatch current $ \c ->
  pmatch (pfromData $ Traverse.ptraverse'bytes c) $ \case
    PDNothing -> perror
    PDJust scalar -> pmatch (pfromData scalar) $ \b ->
      pif (action #== pcon Traverse.PNoAction #&& pfromData (Bytes.pbytes'stage b) #/= Bytes.pstageTerminal)
        (Traverse.pprevalidatedAdvanceBytes # current # poptionalBytes source) perror

padvanceLargeConstructor, padvanceLargeFields, pclose :: forall s. Term s (Normalized.PTraversalExecution :--> PBool)
padvanceLargeConstructor = traversalStep Traverse.pstageLargeConstructor $ \current action source ->
  pif (action #== pcon Traverse.PNoAction) (Traverse.pprevalidatedAdvanceLargeConstructor # current # poptionalBytes source # pcon Traverse.PNoAction) perror
padvanceLargeFields = traversalStep Traverse.pstageLargeFields $ \current action source ->
  pif (action #== pcon Traverse.PNoAction) (Traverse.pprevalidatedAdvanceLargeFields # current # poptionalBytes source # pcon Traverse.PNoAction) perror
pclose = traversalStep Traverse.pstageClose $ \current action source ->
  pif (action #== pcon Traverse.PNoAction) (Traverse.pprevalidatedClose # current # poptionalBytes source # pcon Traverse.PNoAction) perror

pfoldList, pfoldMap, pfinalizeFrame :: forall s. Term s (Normalized.PTraversalExecution :--> PBool)
pfoldList = traversalStep Traverse.pstageFold $ \current action source -> pmatch current $ \c -> pmatch action $ \case
  Traverse.PFoldList frame index child siblings -> pif (source #== pcon PDNothing)
    (pmatch (Traverse.pprevalidatedFoldListNextFrameRoot # pfromData (Traverse.ptraverse'frameRoot c) # pfromData frame # pfromData index # pfromData child # pfromData siblings) $ \case
      PNothing -> perror
      PJust root -> pcon $ PJust $ pcon c { Traverse.ptraverse'frameRoot = pdata root }) perror
  _ -> perror
pfoldMap = traversalStep Traverse.pstageFold $ \current action source -> pmatch current $ \c -> pmatch action $ \case
  Traverse.PFoldMap frame index key value keySiblings valueSiblings -> pif (source #== pcon PDNothing)
    (pmatch (Traverse.pprevalidatedFoldMapNextFrameRootV1 # pfromData (Traverse.ptraverse'frameRoot c) # pfromData frame # pfromData index # pfromData key # pfromData value # pfromData keySiblings # pfromData valueSiblings) $ \case
      PNothing -> perror
      PJust root -> pcon $ PJust $ pcon c { Traverse.ptraverse'frameRoot = pdata root }) perror
  _ -> perror
pfinalizeFrame = traversalStep Traverse.pstageFold $ \current action source -> pmatch current $ \c -> pmatch action $ \case
  Traverse.PFinalizeFrame frame parent -> pif (source #== pcon PDNothing)
    (pmatch (Traverse.pprevalidatedFinalizeFrameTransitionV1 # pfromData (Traverse.ptraverse'frameRoot c) # pfromData (Traverse.ptraverse'offset c) # pfromData (Traverse.ptraverse'sourceLength c) # pfromData frame # pfromData parent) $ \case
      PNothing -> perror
      PJust transition -> pmatch transition $ \t -> pcon $ PJust $ pcon c
        { Traverse.ptraverse'stage = pdata $ Traverse.ptransition'nextStage t
        , Traverse.ptraverse'frameRoot = pdata $ Traverse.ptransition'nextFrameRoot t
        , Traverse.ptraverse'pendingLargeExpectedChildren = pdata $ pcon PDNothing
        , Traverse.ptraverse'integer = pdata $ pcon PDNothing
        , Traverse.ptraverse'bytes = pdata $ pcon PDNothing
        , Traverse.ptraverse'result = pdata $ Traverse.ptransition'nextResult t
        }) perror
  _ -> perror

outerStep :: forall s. Term s PInteger -> Term s Item.PRedeemerItemProofActionV1 -> (Term s Item.PRedeemerItemProofControlV1 -> Term s PByteString -> Term s Item.PRedeemerItemProofStepResultV1) -> Bool -> Term s (Normalized.POuterExecution :--> PBool)
outerStep stage action evaluate invalid = plam $ \state -> pmatch state $ \s ->
  plet (pfromData $ Normalized.pouter'current s) $ \current -> pmatch current $ \c ->
  pif (pfromData (Item.predeemerControl'stage c) #== stage #&& pfromData (Normalized.pouter'action s) #== action
    #&& (if invalid then pfromData (Item.predeemerControl'mode c) #== Item.pmodeData #&& Normalized.pouter'next s #== Normalized.pouter'current s
      else pfromData (Item.predeemerControl'mode c) #== Item.pmodeDescriptor #|| pfromData (Item.predeemerControl'mode c) #== Item.pmodeData))
    (evaluate current (pexpectBytes $ pfromData $ Normalized.pouter'sourceBytes s)
      #== if invalid then pcon Item.PRedeemerItemProofInvalid else pcon $ Item.PRedeemerItemProofAdvanced $ Normalized.pouter'next s) perror

popenHeader, popenTail, pinvalidHeader, pinvalidTail, pfinishData :: forall s. Term s (Normalized.POuterExecution :--> PBool)
popenHeader = outerStep Item.pstageHeader (pcon Item.PRedeemerItemOpenHeader) (\c bytes -> Item.pprevalidatedOpenHeader # c # bytes) False
popenTail = outerStep Item.pstageTail (pcon Item.PRedeemerItemOpenTail) (\c bytes -> Item.pprevalidatedOpenTail # c # bytes) False
pinvalidHeader = outerStep Item.pstageHeader (pcon Item.PRedeemerItemOpenHeader) (\c bytes -> Item.pprevalidatedOpenHeader # c # bytes) True
pinvalidTail = outerStep Item.pstageTail (pcon Item.PRedeemerItemOpenTail) (\c bytes -> Item.pprevalidatedOpenTail # c # bytes) True
pfinishData = phoistAcyclic $ plam $ \state -> pmatch state $ \s -> pmatch (pfromData $ Normalized.pouter'current s) $ \c ->
  pmatch (pfromData $ Item.predeemerControl'traversal c) $ \case
    PDNothing -> perror
    PDJust traversal -> pmatch (pfromData traversal) $ \t ->
      pif (pfromData (Item.predeemerControl'stage c) #== Item.pstageData #&& pfromData (Item.predeemerControl'mode c) #== Item.pmodeData
        #&& pfromData (Normalized.pouter'action s) #== pcon Item.PRedeemerItemFinishData
        #&& pfromData (Normalized.pouter'sourceBytes s) #== pcon PDNothing #&& pfromData (Traverse.ptraverse'stage t) #== Traverse.pstageTerminal)
        (pfromData (Normalized.pouter'next s) #== pcon c { Item.predeemerControl'stage = pdata Item.pstageTerminal }) perror
