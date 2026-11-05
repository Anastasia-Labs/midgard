{-# LANGUAGE OverloadedStrings #-}

-- | Target stage-zero ScriptSources begin predicate over the authenticated raw frame.
module Midgard.ScriptSourcesStageZeroBeginSemantics (pverify) where

import Midgard.Blake2b224Trace qualified as Blake2b224
import Midgard.BoundedCollection (pmaxTxSizeDerivedItemCount)
import Midgard.FraudProofs.NativeTx.Codec (pcborInt, pencodeDefiniteBytes)
import Midgard.FraudProofs.NativeTx.Compact qualified as NativeCompact
import Midgard.FraudProofs.NativeTx.Types (PNativeTxWitnessSetCompact (..))
import Midgard.NativeScriptScan qualified as NativeScriptScan
import Midgard.NativeTxFieldAccess qualified as NativeField
import Midgard.ScriptSourcesRawFrame qualified as Raw
import Midgard.ValidationMachine (
  PInlineSourceHashControlV1 (..),
  PValidationOneStepWitnessV1 (..),
  pencodeInlineSourceHashControlV1,
  pmaxAggregateFieldPreimageBytes,
  prejectedSuccessorIsExact,
 )
import Midgard.ValidationMachineFieldDoor qualified as FieldDoor
import Midgard.ValidationTrace (PValidationMachineStateV1 (..))
import Plutarch.Core.Utils (pand'List)
import Plutarch.Prelude

pemptyReceiveWire :: forall s. Term s (PData :--> PBool)
pemptyReceiveWire = phoistAcyclic $ plam $ \dat ->
  plet (pasList # dat) $ \items ->
    pand'List
      [ plength # items #== 6
      , pasInt # (pelemAt # 0 # items) #== 0
      , pasList # (pelemAt # 1 # items) #== pnil
      , pasInt # (pelemAt # 2 # items) #== 0
      , pasByteStr # (pelemAt # 3 # items) #== pconstant ""
      , pasByteStr # (pelemAt # 4 # items) #== pconstant ""
      , pasList # (pelemAt # 5 # items) #== pnil
      ]

pverify ::
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
pverify = phoistAcyclic $
  plam $ \pre witness door fieldIndex itemIndex carriage ->
    pmatch pre $ \preState ->
      pmatch witness $ \stepWitness ->
        plet (Raw.popenFrameV1 # pre # witness # 30 # 0) $ \frame ->
          pmatch frame $ \f ->
            plet (Raw.pitemIntV1 # frame # 10) $ \sourceCount ->
              plet (Raw.pitemIntV1 # frame # 25) $ \sourceTotalCount ->
                pmatch (NativeCompact.pdecodeNativeTxWitnessSetCompact # Raw.pframe'witnessSetCompactCbor f) $ \witnessSet ->
                  plet
                    ( FieldDoor.popenMachineFieldItemByCommitment
                        # door
                        # pfromData (pmachineState'transactionId preState)
                        # pfromData (pwitnessSetCompact'scriptTxWitsHash witnessSet)
                        # 6
                        # sourceCount
                        # carriage
                    )
                    $ \item ->
                      plet (FieldDoor.pmachineFieldItemCount # item) $ \itemCount ->
                        plet (FieldDoor.pmachineFieldItemLength # item) $ \itemLength ->
                          plet (pif (sourceTotalCount #== 0) itemCount sourceTotalCount) $ \activeTotalCount ->
                            plet
                              ( NativeScriptScan.pversionedScriptHeaderV1
                                  # (FieldDoor.pmachineFieldItemChunk # item # 0)
                                  # itemLength
                              )
                              $ \header ->
                                plet
                                  ( pmatch header $ \case
                                      PNothing ->
                                        prejectedSuccessorIsExact
                                          # pre
                                          # pfromData (poneStep'claimedSuccessor stepWitness)
                                          # pconstant "E_INVALID_FIELD_TYPE"
                                      PJust itemHeader ->
                                        pmatch itemHeader $ \h ->
                                          plet
                                            ( pcon $
                                                PInlineSourceHashControlV1
                                                  (pdata 1)
                                                  (pdata sourceCount)
                                                  (pdata activeTotalCount)
                                                  (NativeScriptScan.pheader'languageTag h)
                                                  (NativeScriptScan.pheader'payloadOffset h)
                                                  (NativeScriptScan.pheader'payloadLength h)
                                                  (pdata itemLength)
                                                  (pdata $ FieldDoor.pmachineFieldItemCommitment # item)
                                                  ( pdata $
                                                      Blake2b224.pinitialControlV1
                                                        # (pfromData (NativeScriptScan.pheader'payloadLength h) + 1)
                                                  )
                                            )
                                            $ \pending ->
                                              plet
                                                ( pcborInt sourceTotalCount
                                                    <> pconstant "\x00"
                                                    <> Raw.pemptyObserverScanCbor
                                                    <> Raw.pemptyMintFoldCbor
                                                    <> (pencodeDefiniteBytes # (Raw.pitemBytesV1 # frame # 29))
                                                )
                                                $ \oldSuffix ->
                                                  plet
                                                    ( pcborInt activeTotalCount
                                                        <> pconstant "\x00"
                                                        <> Raw.pemptyObserverScanCbor
                                                        <> Raw.pemptyMintFoldCbor
                                                        <> (pencodeDefiniteBytes # (Raw.pitemBytesV1 # frame # 29))
                                                    )
                                                    $ \newSuffix ->
                                                      plet (pfromData $ poneStep'workWitnessCbor stepWitness) $ \workCbor ->
                                                        Raw.psuccessorIsExactV1
                                                          # pre
                                                          # witness
                                                          # ( Raw.pappendExtensionV1
                                                                # ( Raw.pspliceV1
                                                                      # workCbor
                                                                      # (plengthBS # workCbor - plengthBS # oldSuffix)
                                                                      # oldSuffix
                                                                      # newSuffix
                                                                  )
                                                                # (pencodeInlineSourceHashControlV1 # pending)
                                                            )
                                  )
                                  $ \headerResult ->
                                    pand'List
                                      [ pemptyReceiveWire # (pelemAt # 24 # Raw.pframe'items f)
                                      , Raw.pitemFrontierV1 # frame # 8 #== pnil
                                      , Raw.pitemIntV1 # frame # 12 #== 0
                                      , Raw.pitemFrontierV1 # frame # 13 #== pnil
                                      , Raw.pitemIntV1 # frame # 26 #== 0
                                      , Raw.pitemIntV1 # frame # 18 #== 0
                                      , Raw.pitemFrontierV1 # frame # 19 #== pnil
                                      , Raw.pitemIntV1 # frame # 20 #== 0
                                      , Raw.pitemIntV1 # frame # 21 #== 0
                                      , Raw.pitemFrontierV1 # frame # 22 #== pnil
                                      , Raw.pitemIntV1 # frame # 23 #== 0
                                      , activeTotalCount #> 0
                                      , activeTotalCount #<= pmaxTxSizeDerivedItemCount
                                      , itemLength #> 0
                                      , itemLength #<= pmaxAggregateFieldPreimageBytes
                                      , fieldIndex #== 6
                                      , itemIndex #== sourceCount
                                      , itemCount #== activeTotalCount
                                      , headerResult
                                      ]
