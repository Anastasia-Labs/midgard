module Midgard.NativeScriptScanOneShot (
  ppayloadStructureIsCanonicalAtBoundsV1,
  ppayloadStructureIsCanonicalV1,
) where

import Plutarch.Prelude

import Midgard.NativeScriptScan (
  PNativeFrameResultV1 (..),
  PNativeScriptFrameV1,
  PNativeScriptTokenV1 (..),
  pallNode,
  papplyChildV1,
  patLeastNode,
  pframeForTokenV1,
  pmaxNativeScriptDepth,
  pmaxNativeScriptNodes,
  ptokenAtV1,
 )

-- A single recursive driver represents Aiken's mutually recursive token and
-- frame functions. The Boolean selects the active half of the state machine.
poneShotScanV1 ::
  forall s.
  Term
    s
    ( PBool
        :--> PByteString
        :--> PInteger
        :--> PInteger
        :--> PBuiltinList (PAsData PNativeScriptFrameV1)
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PBool
    )
poneShotScanV1 = phoistAcyclic $ pfix $ \self ->
  plam $ \tokenStage payload endOffset cursor stack stackDepth nodeCount nodeBound depthBound ->
    pif tokenStage
      ( pif (cursor #>= endOffset) (pconstant False) $
          pmatch (ptokenAtV1 # payload # cursor # cursor) $ \case
            PNothing -> pconstant False
            PJust token -> pmatch token $ \t ->
              plet (pfromData $ ptoken'nextOffset t) $ \nextOffset ->
              pif (nextOffset #<= cursor #|| nextOffset #> endOffset) (pconstant False) $
                plet (nodeCount + 1) $ \nextNodeCount ->
                pif (nextNodeCount #> nodeBound) (pconstant False) $
                  pif
                    ( pfromData (ptoken'kind t) #>= pallNode
                        #&& pfromData (ptoken'kind t) #<= patLeastNode
                        #&& pfromData (ptoken'childCount t) #> 0
                    )
                    ( plet (stackDepth + 1) $ \nextDepth ->
                      pif (nextDepth #> depthBound) (pconstant False) $
                        pmatch (pframeForTokenV1 # token # pconstant "") $ \case
                          PNothing -> pconstant False
                          PJust frame ->
                            self
                              # pconstant True
                              # payload
                              # endOffset
                              # nextOffset
                              # (pcons # pdata frame # stack)
                              # nextDepth
                              # nextNodeCount
                              # nodeBound
                              # depthBound
                    )
                    ( self
                        # pconstant False
                        # payload
                        # endOffset
                        # nextOffset
                        # stack
                        # stackDepth
                        # nextNodeCount
                        # nodeBound
                        # depthBound
                    )
      )
      ( pelimList
          ( \frame rest ->
              pmatch (papplyChildV1 # pfromData frame # pconstant False) $ \case
                PNothing -> pconstant False
                PJust result -> pmatch result $ \case
                  PNativeFramePending nextFrameData ->
                    self
                      # pconstant True
                      # payload
                      # endOffset
                      # cursor
                      # (pcons # nextFrameData # rest)
                      # stackDepth
                      # nodeCount
                      # nodeBound
                      # depthBound
                  PNativeFrameComplete _ _ ->
                    self
                      # pconstant False
                      # payload
                      # endOffset
                      # cursor
                      # rest
                      # (stackDepth - 1)
                      # nodeCount
                      # nodeBound
                      # depthBound
          )
          (stackDepth #== 0 #&& cursor #== endOffset #&& nodeCount #> 0)
          stack
      )

-- | Aiken @payload_structure_is_canonical_at_bounds_v1@.
ppayloadStructureIsCanonicalAtBoundsV1 ::
  forall s. Term s (PByteString :--> PInteger :--> PInteger :--> PBool)
ppayloadStructureIsCanonicalAtBoundsV1 = phoistAcyclic $ plam $ \payload nodeBound depthBound ->
  plet (plengthBS # payload) $ \endOffset ->
    pif (endOffset #<= 0) (pconstant False) $
      poneShotScanV1
        # pconstant True
        # payload
        # endOffset
        # 0
        # pcon PNil
        # 0
        # 0
        # nodeBound
        # depthBound

-- | Aiken @payload_structure_is_canonical_v1@ at the staged bounds.
ppayloadStructureIsCanonicalV1 :: forall s. Term s (PByteString :--> PBool)
ppayloadStructureIsCanonicalV1 = phoistAcyclic $ plam $ \payload ->
  ppayloadStructureIsCanonicalAtBoundsV1
    # payload
    # pmaxNativeScriptNodes
    # pmaxNativeScriptDepth
