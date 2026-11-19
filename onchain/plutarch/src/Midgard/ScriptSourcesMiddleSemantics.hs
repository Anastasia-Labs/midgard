{-# LANGUAGE OverloadedStrings #-}

-- | Target middle-stage predicates over the canonical authenticated frame.
module Midgard.ScriptSourcesMiddleSemantics (pstageTwoAdvance, pstageThreeReplay, pstageThreeFinish, pstageFourBegin, pstageFourFinish, pstageSixBegin, pstageSixAsset, pstageSixFinish) where

import Midgard.BoundedCollection (pmaxTxSizeDerivedItemCount)
import Midgard.BoundedItem qualified as BoundedItem
import Midgard.FraudProofs.NativeTx.Codec (pcborInt, pencodeDefiniteBytes, pdecodeDefiniteArrayHeaderAt, pdecodeCanonicalIntAt)
import Midgard.FraudProofs.NativeTx.Compact qualified as NativeCompact
import Midgard.FraudProofs.NativeTx.Types
import Midgard.LedgerOutput qualified as LedgerOutput
import Midgard.NativeTxFieldAccess qualified as NativeField
import Midgard.ScriptProof qualified as ScriptProof
import Midgard.ScriptSourcesRawFrame qualified as Raw
import Midgard.ValidationMachine
import Midgard.ValidationMachineFieldDoor qualified as FieldDoor
import Midgard.ValidationMerkle
import Midgard.ValidationTrace hiding (pcborInt)
import Plutarch.Core.Utils (pand'List, (#/=))
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.Prelude

baseControl :: forall s. Term s (Raw.PFrame :--> PScriptSourcesControlV1)
baseControl = phoistAcyclic $ plam $ \frame -> pmatch frame $ \f -> pscriptSourcesControlFromDataItems # Raw.pframe'items f # pconstant ""

pstageTwoAdvance, pstageThreeFinish, pstageFourFinish, pstageSixFinish :: forall s. Term s (PValidationMachineStateV1 :--> PValidationOneStepWitnessV1 :--> PBool)
pstageTwoAdvance = phoistAcyclic $ plam $ \pre witness -> pmatch witness $ \w ->
  plet (Raw.popenFrameV1 # pre # witness # 30 # 2) $ \frame ->
    pand'List
      [ Raw.pitemIntV1 # frame # 4 #> 0
      , Raw.pitemIntV1 # frame # 14 #== 0
      , Raw.pitemFrontierV1 # frame # 8 #== pnil
      , Raw.pitemBytesV1 # frame # 15 #== pinitialResolutionAccumulator
      , Raw.pitemBytesV1 # frame # 16 #== pemptyResolutionScheduleHash
      , Raw.pitemIntV1 # frame # 17 #== 0
      , Raw.pitemIntV1 # frame # 18 #== 0
      , Raw.pitemFrontierV1 # frame # 19 #== pnil
      , Raw.pitemBytesV1 # frame # 29 #/= pemptyResolutionScheduleHash
      , Raw.psuccessorIsExactV1 # pre # witness
          # (Raw.preplaceStageV1 # frame
              # (Raw.preplaceItemsV1 # frame # pfromData (poneStep'workWitnessCbor w) # 16 # 1
                  # (pencodeDefiniteBytes # (Raw.pitemBytesV1 # frame # 29))) # 3)
      ]

pstageThreeReplay :: forall s. Term s (PValidationMachineStateV1 :--> PValidationOneStepWitnessV1 :--> PInteger :--> PByteString :--> PByteString :--> PByteString :--> PBool)
pstageThreeReplay = phoistAcyclic $ plam $ \pre witness kind key next value ->
  plet (Raw.popenFrameV1 # pre # witness # 30 # 3) $ \frame ->
    Raw.pitemBytesV1 # frame # 16 #/= pemptyResolutionScheduleHash
      #&& pscriptSourcesReplayItem # pre # witness # (baseControl # frame) # kind # key # next # value

pstageThreeFinish = phoistAcyclic $ plam $ \pre witness -> pmatch witness $ \w ->
  plet (Raw.popenFrameV1 # pre # witness # 30 # 3) $ \frame ->
    pand'List
      [ Raw.pitemBytesV1 # frame # 16 #== pemptyResolutionScheduleHash
      , Raw.pitemIntV1 # frame # 14 #== Raw.pitemIntV1 # frame # 4
      , Raw.pitemBytesV1 # frame # 15 #== Raw.pitemBytesV1 # frame # 5
      , Raw.psuccessorIsExactV1 # pre # witness # (Raw.preplaceStageV1 # frame # pfromData (poneStep'workWitnessCbor w) # 4)
      ]

pstageFourFinish = phoistAcyclic $ plam $ \pre witness -> pmatch witness $ \w ->
  plet (Raw.popenFrameV1 # pre # witness # 30 # 4) $ \frame -> pmatch frame $ \f ->
  pmatch (NativeCompact.pdecodeNativeTxCompactV1 # Raw.pframe'compactCbor f) $ \compact -> pmatch (pcompact'body compact) $ \body ->
  pmatch (baseControl # frame) $ \c ->
    pand'List
      [ pif (pbodyCompact'outputsHash body #== NativeField.pemptyFieldCommitment)
          (pfromData (pscriptSources'outputCursor c) #== 0 #&& pfromData (pscriptSources'outputCount c) #== 0
            #&& pfromData (pscriptSources'outputTotalCount c) #== 0 #&& pfromData (pscriptSources'outputPeaks c) #== pnil)
          (pfromData (pscriptSources'outputTotalCount c) #> 0 #&& pfromData (pscriptSources'outputCount c) #== pfromData (pscriptSources'outputTotalCount c))
      , pfromData (pscriptSources'outputCursor c) #== 0
      , pfromData (pscriptSources'receiveScan c) #== pemptyReceivePurposeScanControl
      , Raw.psuccessorIsExactV1 # pre # witness # (Raw.preplaceStageV1 # frame # pfromData (poneStep'workWitnessCbor w) # 5)
      ]

pstageFourBegin :: forall s. Term s (PValidationMachineStateV1 :--> PValidationOneStepWitnessV1 :--> FieldDoor.PMachineFieldDoorV1 :--> NativeField.PFieldCarriageV1 :--> PBool)
pstageFourBegin = phoistAcyclic $ plam $ \pre witness door carriage -> pmatch pre $ \p -> pmatch witness $ \w ->
  plet (Raw.popenFrameV1 # pre # witness # 30 # 4) $ \frame -> pmatch frame $ \f ->
  pmatch (baseControl # frame) $ \c ->
  pmatch (NativeCompact.pverifyNativeTxProofSourceV1 # pfromData (pmachineState'transactionId p)
    # Raw.pframe'compactCbor f # Raw.pframe'witnessSetCompactCbor f # Raw.pframe'fieldPreimageLengthsCbor f) $ \(PPair verified ws) ->
  pmatch verified $ \v -> pmatch (pverified'txCompact v) $ \compact -> pmatch (pcompact'body compact) $ \body ->
    pif (pbodyCompact'outputsHash body #== NativeField.pemptyFieldCommitment
      #|| (pfromData (pscriptSources'outputTotalCount c) #> 0 #&& pfromData (pscriptSources'outputCount c) #== pfromData (pscriptSources'outputTotalCount c)))
      (pconstant False)
      (plet (FieldDoor.popenMachineFieldItem # door # verified # ws # 2 # pfromData (pscriptSources'outputCount c) # carriage) $ \item ->
       plet (FieldDoor.pmachineFieldItemCount # item) $ \count ->
       plet (pif (pfromData (pscriptSources'outputTotalCount c) #== 0) count (pfromData $ pscriptSources'outputTotalCount c)) $ \active ->
         pand'List
           [ pfromData (pscriptSources'outputCursor c) #== 0
           , pfromData (pscriptSources'receiveScan c) #== pemptyReceivePurposeScanControl
           , active #> 0
           , active #<= pmaxTxSizeDerivedItemCount
           , count #== active
           , FieldDoor.pmachineFieldItemLength # item #<= 16_384
           , Raw.psuccessorIsExactV1 # pre # witness
               # (Raw.preplaceItemsV1 # frame # pfromData (poneStep'workWitnessCbor w) # 21 # 3
                   # (pcborInt (pfromData (pscriptSources'outputCount c) + 1)
                     <> (pencodeFrontier # (pappendLeaf # pfromData (pscriptSources'outputCount c) # pfromData (pscriptSources'outputPeaks c)
                          # (ScriptProof.poutputItemLeafHash # pfromData (pscriptSources'outputCount c) # (FieldDoor.pmachineFieldItemCommitment # item))))
                     <> (pcborInt active)))
           ])

pstageSixFinish = phoistAcyclic $ plam $ \pre witness -> pmatch witness $ \w ->
  plet (Raw.popenFrameV1 # pre # witness # 30 # 6) $ \frame -> pmatch frame $ \f ->
  pmatch (baseControl # frame) $ \c -> plet (pfromData $ pscriptSources'mintFold c) $ \fold -> pmatch fold $ \m ->
  pmatch (NativeCompact.pdecodeNativeTxCompactV1 # Raw.pframe'compactCbor f) $ \compact -> pmatch (pcompact'body compact) $ \body ->
  plet (pfromData (pmintFold'policyCount m) #== -1 #&& pbodyCompact'mintHash body #== NativeField.pemptyFieldCommitment) $ \empty ->
  plet (pif empty (pcon m {pmintFold'policyCount = pdata 0}) fold) $ \next ->
    (empty #|| (pfromData (pmintFold'policyCount m) #>= 0 #&& pfromData (pmintFold'policyCursor m) #== pfromData (pmintFold'policyCount m)
      #&& pfromData (pmintFold'activePolicy m) #== pconstant ""))
      #&& Raw.psuccessorIsExactV1 # pre # witness
        # (Raw.preplaceStageV1 # frame
          # (Raw.preplaceItemsV1 # frame
            # (Raw.preplaceItemsV1 # frame # pfromData (poneStep'workWitnessCbor w) # 28 # 1 # (pencodeMintFoldControl # next))
            # 20 # 1 # pconstant "\x00") # 7)

pstageSixBegin ::
  forall s.
  Term
    s
    ( PValidationMachineStateV1
        :--> PValidationOneStepWitnessV1
        :--> FieldDoor.PMachineFieldDoorV1
        :--> PInteger
        :--> PInteger
        :--> NativeField.PFieldCarriageV1
        :--> PBool
    )
pstageSixBegin = phoistAcyclic $
  plam $
    \pre witness door fieldIndex itemIndex carriage ->
      pmatch pre $ \preState ->
        pmatch witness $ \stepWitness ->
          plet (Raw.popenFrameV1 # pre # witness # 30 # 6) $ \frame ->
          plet (baseControl # frame) $ \control ->
              pmatch control $ \c ->
                pmatch
                  ( NativeCompact.pverifyNativeTxProofSourceV1
                      # pfromData (pmachineState'transactionId preState)
                      # pfromData (pscriptSources'compactCbor c)
                      # pfromData (pscriptSources'witnessSetCompactCbor c)
                      # pfromData (pscriptSources'fieldPreimageLengthsCbor c)
                  )
                  $ \(PPair verifiedSource witnessSet) ->
                    plet verifiedSource $ \_ ->
                      pmatch (pfromData $ pscriptSources'mintFold c) $ \fold ->
                        plet
                          ( FieldDoor.popenMachineFieldItem
                              # door
                              # verifiedSource
                              # witnessSet
                              # 5
                              # pfromData (pmintFold'policyCursor fold)
                              # carriage
                          )
                          $ \item ->
                            plet (FieldDoor.pmachineFieldItemChunk # item # 0) $ \chunkBytes ->
                              plet (FieldDoor.pmachineFieldItemLength # item) $ \itemLength ->
                                pmatch (pdecodeDefiniteArrayHeaderAt # chunkBytes # 0) $ \(PPair policyOffset arrayItemCount) ->
                                  pif
                                    (arrayItemCount #== 2)
                                    ( pmatch (pdecodeCanonicalBytesAt # chunkBytes # policyOffset) $ \(PPair assetsHeaderOffset policyId) ->
                                        pmatch (pdecodeCanonicalMapHeaderAt # chunkBytes # assetsHeaderOffset) $ \(PPair assetsOffset assetCount) ->
                                          plet (FieldDoor.pmachineFieldItemCount # item) $ \policyItemCount ->
                                            plet
                                              ( pif
                                                  (pfromData (pmintFold'policyCount fold) #== -1)
                                                  policyItemCount
                                                  (pfromData $ pmintFold'policyCount fold)
                                              )
                                              $ \activePolicyCount ->
                                                plet
                                                  ( pand'List
                                                      [ pfromData (pmintFold'activePolicy fold) #== pconstant ""
                                                      , pfromData (pmintFold'policyCursor fold) #< activePolicyCount
                                                      , fieldIndex #== 5
                                                      , itemIndex #== pfromData (pmintFold'policyCursor fold)
                                                      , policyItemCount #== activePolicyCount
                                                      , policyItemCount #<= pmaxTxSizeDerivedItemCount
                                                      , itemLength #> assetsOffset
                                                      , itemLength #<= pmaxAggregateFieldPreimageBytes
                                                      , plengthBS # policyId #== 28
                                                      , assetCount #> 0
                                                      , pfromData (pmintFold'policyCursor fold)
                                                          #== 0
                                                          #|| pfromData (pmintFold'previousPolicy fold)
                                                          #< policyId
                                                      ]
                                                  )
                                                  $ \commonIsValid ->
                                                    plet
                                                      ( pcon $
                                                          PMintFoldControlV1
                                                            (pdata activePolicyCount)
                                                            (pmintFold'policyCursor fold)
                                                            (pmintFold'previousPolicy fold)
                                                            (pdata policyId)
                                                            (pdata itemLength)
                                                            (pdata $ FieldDoor.pmachineFieldItemCommitment # item)
                                                            (pdata assetsOffset)
                                                            (pdata assetCount)
                                                            (pdata 0)
                                                            (pdata $ pconstant "")
                                                            (pmintFold'assetCount fold)
                                                            (pmintFold'assetPeaks fold)
                                                      )
                                                      $ \nextFold ->
                                                        pif
                                                          ( commonIsValid
                                                              #&& pfromData (pmintFold'assetCount fold)
                                                              + assetCount
                                                                #> LedgerOutput.pmaxDistinctAssetCount
                                                          )
                                                          ( prejectedSuccessorIsExact
                                                              # pre
                                                              # pfromData (poneStep'claimedSuccessor stepWitness)
                                                              # pconstant "E_ASSET_COUNT"
                                                          )
                                                          ( commonIsValid
                                                              #&& Raw.psuccessorIsExactV1 # pre # witness
                                                                # (Raw.preplaceItemsV1 # frame
                                                                    # (Raw.preplaceItemsV1 # frame # pfromData (poneStep'workWitnessCbor stepWitness)
                                                                        # 28 # 1 # (pencodeMintFoldControl # nextFold))
                                                                    # 18 # 2
                                                                    # (pcborInt (pfromData (pscriptSources'purposeCount c) + 1)
                                                                        <> (pencodeFrontier # (pappendLeaf
                                                                            # pfromData (pscriptSources'purposeCount c)
                                                                            # pfromData (pscriptSources'purposePeaks c)
                                                                            # (ScriptProof.ppurposeLeafHash # 1 # pfromData (pmintFold'policyCursor fold) # policyId # policyId)))))
                                                          )
                                    )
                                    perror

pstageSixAsset ::
  forall s.
  Term
    s
    ( PValidationMachineStateV1
        :--> PValidationOneStepWitnessV1
        :--> BoundedItem.PChunkProofV1
        :--> PMaybeData BoundedItem.PChunkProofV1
        :--> PBool
    )
pstageSixAsset = phoistAcyclic $
  plam $
    \pre witness chunkProof nextChunkProof ->
      pmatch witness $ \stepWitness ->
      plet (Raw.popenFrameV1 # pre # witness # 30 # 6) $ \frame ->
      plet (baseControl # frame) $ \control -> pmatch control $ \c ->
                      plet (pfromData $ pscriptSources'mintFold c) $ \fold ->
                        pmatch fold $ \f ->
                          pmatch (pscriptSourcesMintChunkWindow # fold # chunkProof # nextChunkProof) $ \case
                            PNothing -> perror
                            PJust window ->
                              pmatch window $ \(PMintChunkWindowV1 bytes offset chunkIndex) ->
                                pmatch (pdecodeCanonicalBytesAt # bytes # offset) $ \(PPair quantityOffset assetName) ->
                                  pmatch (pdecodeCanonicalIntAt # bytes # quantityOffset) $ \(PPair nextOffset quantity) ->
                                    plet (chunkIndex * BoundedItem.pchunkBytes + nextOffset) $ \nextItemCursor ->
                                      plet (pfromData (pmintFold'assetCount f) + 1) $ \nextAssetCount ->
                                        plet
                                          ( pand'List
                                              [ plengthBS # pfromData (pmintFold'activePolicy f) #== 28
                                              , pfromData (pmintFold'assetsRemaining f) #> 0
                                              , pfromData (pmintFold'policyAssetCursor f) #>= 0
                                              , plengthBS # assetName #<= 32
                                              , quantity #/= 0
                                              , nextAssetCount #<= LedgerOutput.pmaxDistinctAssetCount
                                              , pfromData (pmintFold'policyAssetCursor f)
                                                  #== 0
                                                  #|| pscriptSourcesCanonicalBytesKeyPrecedes
                                                  # pfromData (pmintFold'previousAsset f)
                                                  # assetName
                                              , pif
                                                  (pfromData (pmintFold'assetsRemaining f) #== 1)
                                                  (nextItemCursor #== pfromData (pmintFold'itemLength f))
                                                  (nextItemCursor #< pfromData (pmintFold'itemLength f))
                                              ]
                                          )
                                          $ \commonIsValid ->
                                            plet
                                              ( pappendLeaf
                                                  # pfromData (pmintFold'assetCount f)
                                                  # pfromData (pmintFold'assetPeaks f)
                                                  # ( pmintAssetLeafHash
                                                        # pfromData (pmintFold'activePolicy f)
                                                        # assetName
                                                        # quantity
                                                    )
                                              )
                                              $ \nextAssetPeaks ->
                                                plet
                                                  ( pif
                                                      (pfromData (pmintFold'assetsRemaining f) #== 1)
                                                      ( pcon $
                                                          PMintFoldControlV1
                                                            (pmintFold'policyCount f)
                                                            (pdata $ pfromData (pmintFold'policyCursor f) + 1)
                                                            (pmintFold'activePolicy f)
                                                            (pdata $ pconstant "")
                                                            (pdata 0)
                                                            (pdata $ pconstant "")
                                                            (pdata 0)
                                                            (pdata 0)
                                                            (pdata 0)
                                                            (pdata $ pconstant "")
                                                            (pdata nextAssetCount)
                                                            (pdata nextAssetPeaks)
                                                      )
                                                      ( pcon $
                                                          PMintFoldControlV1
                                                            (pmintFold'policyCount f)
                                                            (pmintFold'policyCursor f)
                                                            (pmintFold'previousPolicy f)
                                                            (pmintFold'activePolicy f)
                                                            (pmintFold'itemLength f)
                                                            (pmintFold'itemCommitment f)
                                                            (pdata nextItemCursor)
                                                            (pdata $ pfromData (pmintFold'assetsRemaining f) - 1)
                                                            (pdata $ pfromData (pmintFold'policyAssetCursor f) + 1)
                                                            (pdata assetName)
                                                            (pdata nextAssetCount)
                                                            (pdata nextAssetPeaks)
                                                      )
                                                  )
                                                  $ \nextFold ->
                                                    commonIsValid
                                                      #&& Raw.psuccessorIsExactV1 # pre # witness
                                                        # (Raw.preplaceItemsV1 # frame # pfromData (poneStep'workWitnessCbor stepWitness)
                                                            # 28 # 1 # (pencodeMintFoldControl # nextFold))
