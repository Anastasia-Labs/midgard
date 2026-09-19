{-# LANGUAGE OverloadedStrings #-}

module Midgard.FraudProofs.MintAuthorization.Engine (
  PNativeScriptVerdictV1 (..),
  pdirectionScriptAbsent,
  pdirectionScriptUnsatisfied,
  ppolicyIdOfMintItemV1,
  pevaluateNativeScriptV1,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Prelude

import Midgard.FraudProofs.NativeTx.Preimages (pdecodeMintPolicyItemCbor)
import Midgard.NativeScriptScan qualified as Scan

pdirectionScriptAbsent, pdirectionScriptUnsatisfied :: forall s. Term s PInteger
pdirectionScriptAbsent = 0
pdirectionScriptUnsatisfied = 1

data PNativeScriptVerdictV1 s
  = PScriptEvaluatedV1 (Term s (PAsData PBool))
  | PScriptMalformedV1
  | PScriptNodeLimitV1
  | PScriptDepthLimitV1
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PNativeScriptVerdictV1)

-- | Aiken @mint_authorization/engine.policy_id_of_mint_item_v1@.
ppolicyIdOfMintItemV1 :: forall s. Term s (PByteString :--> PByteString)
ppolicyIdOfMintItemV1 = phoistAcyclic $ plam $ \itemCbor ->
  pmatch (pdecodeMintPolicyItemCbor # itemCbor) $ \(PPair policyId _) -> policyId

{- | Evaluate a native-script payload with the validation machine's scanner.

Both Aiken recursive functions are represented by one fixed-point loop. Mode
zero reads a token; mode one unwinds a child result through the frame stack.
-}
pevaluateNativeScriptV1 ::
  forall s.
  Term
    s
    ( PByteString
        :--> PBuiltinList PByteString
        :--> PInteger
        :--> PInteger
        :--> PNativeScriptVerdictV1
    )
pevaluateNativeScriptV1 = phoistAcyclic $ plam $ \payload signerHashes validityStart validityEnd ->
  pif
    (plengthBS # payload #== 0)
    (pcon PScriptMalformedV1)
    ( pnativeScriptLoop
        # payload
        # signerHashes
        # validityStart
        # validityEnd
        # 0
        # 0
        # pnil
        # 0
        # 0
        # pconstant False
    )

pnativeScriptLoop ::
  forall s.
  Term
    s
    ( PByteString
        :--> PBuiltinList PByteString
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PBuiltinList (PAsData Scan.PNativeScriptFrameV1)
        :--> PInteger
        :--> PInteger
        :--> PBool
        :--> PNativeScriptVerdictV1
    )
pnativeScriptLoop = phoistAcyclic $ pfix $ \self ->
  plam $ \payload signerHashes validityStart validityEnd mode cursor stack depth nodeCount valid ->
    pif
      (mode #== 0)
      ( pmatch (Scan.ptokenAtV1 # payload # cursor # cursor) $ \case
          PNothing -> pcon PScriptMalformedV1
          PJust token -> pmatch token $ \t ->
            plet (nodeCount + 1) $ \nextNodeCount ->
              pif
                (nextNodeCount #> Scan.pmaxNativeScriptNodes)
                (pcon PScriptNodeLimitV1)
                ( pif
                    (pfromData (Scan.ptoken'nextOffset t) #> plengthBS # payload)
                    (pcon PScriptMalformedV1)
                    ( pif
                        ( pfromData (Scan.ptoken'kind t)
                            #>= Scan.pallNode
                            #&& pfromData (Scan.ptoken'kind t)
                            #<= Scan.patLeastNode
                            #&& pfromData (Scan.ptoken'childCount t)
                            #> 0
                        )
                        ( plet (depth + 1) $ \nextDepth ->
                            pif
                              (nextDepth #> Scan.pmaxNativeScriptDepth)
                              (pcon PScriptDepthLimitV1)
                              ( pmatch (Scan.pframeForTokenV1 # token # pconstant "") $ \case
                                  PNothing -> pcon PScriptMalformedV1
                                  PJust frame ->
                                    self
                                      # payload
                                      # signerHashes
                                      # validityStart
                                      # validityEnd
                                      # 0
                                      # pfromData (Scan.ptoken'nextOffset t)
                                      # (pcons # pdata frame # stack)
                                      # nextDepth
                                      # nextNodeCount
                                      # valid
                              )
                        )
                        ( plet
                            ( pif
                                (pfromData (Scan.ptoken'kind t) #== Scan.psignatureNode)
                                (pcon $ PJust $ pelem # pfromData (Scan.ptoken'keyHash t) # signerHashes)
                                ( pif
                                    (pfromData (Scan.ptoken'kind t) #== Scan.pafterNode)
                                    ( pcon $
                                        PJust $
                                          validityStart
                                            #>= 0
                                            #&& validityStart
                                            #>= pfromData (Scan.ptoken'slot t)
                                    )
                                    ( pif
                                        (pfromData (Scan.ptoken'kind t) #== Scan.pbeforeNode)
                                        ( pcon $
                                            PJust $
                                              validityEnd
                                                #>= 0
                                                #&& validityEnd
                                                #<= pfromData (Scan.ptoken'slot t)
                                        )
                                        (Scan.pemptyContainerResultV1 # token)
                                    )
                                )
                            )
                            $ \maybeValid -> pmatch maybeValid $ \case
                              PNothing -> pcon PScriptMalformedV1
                              PJust childValid ->
                                self
                                  # payload
                                  # signerHashes
                                  # validityStart
                                  # validityEnd
                                  # 1
                                  # pfromData (Scan.ptoken'nextOffset t)
                                  # stack
                                  # depth
                                  # nextNodeCount
                                  # childValid
                        )
                    )
                )
      )
      ( pmatch stack $ \case
          PNil ->
            pif
              (cursor #== plengthBS # payload)
              (pcon $ PScriptEvaluatedV1 $ pdata valid)
              (pcon PScriptMalformedV1)
          PCons frame rest ->
            pmatch (Scan.papplyChildV1 # pfromData frame # valid) $ \case
              PNothing -> pcon PScriptMalformedV1
              PJust result -> pmatch result $ \case
                Scan.PNativeFramePending pending ->
                  self
                    # payload
                    # signerHashes
                    # validityStart
                    # validityEnd
                    # 0
                    # cursor
                    # (pcons # pending # rest)
                    # depth
                    # nodeCount
                    # valid
                Scan.PNativeFrameComplete _ completed ->
                  self
                    # payload
                    # signerHashes
                    # validityStart
                    # validityEnd
                    # 1
                    # cursor
                    # rest
                    # (depth - 1)
                    # nodeCount
                    # pfromData completed
      )
