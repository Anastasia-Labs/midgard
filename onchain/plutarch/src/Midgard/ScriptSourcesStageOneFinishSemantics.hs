{-# LANGUAGE OverloadedStrings #-}

-- | Size-bounded target ScriptSources stage-one finish predicate.
module Midgard.ScriptSourcesStageOneFinishSemantics (pverify) where

import Midgard.BoundedCollection (pmaxTxSizeDerivedItemCount)
import Midgard.FraudProofs.NativeTx.Compact qualified as NativeCompact
import Midgard.FraudProofs.NativeTx.Types (PNativeTxWitnessSetCompact (..))
import Midgard.NativeTxFieldAccess qualified as NativeField
import Midgard.ScriptSourcesRawFrame qualified as Raw
import Midgard.ValidationMachine (PValidationOneStepWitnessV1 (..))
import Midgard.ValidationMerkle (pfrontierIsWellFormed)
import Midgard.ValidationTrace (PValidationMachineStateV1)
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

pcommonControlIsInitial :: forall s. Term s (Raw.PFrame :--> PBool)
pcommonControlIsInitial = phoistAcyclic $ plam $ \frame ->
  pand'List
    [ Raw.pitemIntV1 # frame # 25 #== Raw.pitemIntV1 # frame # 10
    , Raw.pitemFrontierV1 # frame # 8 #== pnil
    , Raw.pitemIntV1 # frame # 14 #== 0
    , Raw.pitemIntV1 # frame # 17 #== 0
    , Raw.pitemIntV1 # frame # 18 #== 0
    , Raw.pitemIntV1 # frame # 20 #== 0
    , Raw.pitemIntV1 # frame # 21 #== 0
    , pmatch frame $ \f -> pemptyReceiveWire # (pelemAt # 24 # Raw.pframe'items f)
    ]

pverify ::
  forall s.
  Term s (PValidationMachineStateV1 :--> PValidationOneStepWitnessV1 :--> PBool)
pverify = phoistAcyclic $ plam $ \pre witness ->
  pmatch witness $ \stepWitness ->
    plet (Raw.popenFrameV1 # pre # witness # 30 # 1) $ \frame ->
      pmatch frame $ \f ->
        pmatch (NativeCompact.pdecodeNativeTxWitnessSetCompact # Raw.pframe'witnessSetCompactCbor f) $ \witnessSet ->
          plet (pfromData $ pwitnessSetCompact'redeemerTxWitsHash witnessSet) $ \redeemerCommitment ->
            plet (Raw.pitemIntV1 # frame # 12) $ \redeemerCount ->
              plet (Raw.pitemFrontierV1 # frame # 13) $ \redeemerPeaks ->
                plet (Raw.pitemIntV1 # frame # 26) $ \redeemerTotalCount ->
                  plet
                    ( pif
                        (redeemerCommitment #== NativeField.pemptyFieldCommitment)
                        (redeemerCount #== 0 #&& redeemerTotalCount #== 0 #&& redeemerPeaks #== pnil)
                        ( redeemerTotalCount
                            #> 0
                            #&& redeemerCount
                            #== redeemerTotalCount
                            #&& pfrontierIsWellFormed
                            # redeemerCount
                            # redeemerPeaks
                        )
                    )
                    $ \scanIsComplete ->
                      pand'List
                        [ pcommonControlIsInitial # frame
                        , plengthBS # redeemerCommitment #== 32
                        , plengthBS # (Raw.pitemBytesV1 # frame # 29) #== 32
                        , redeemerTotalCount #<= pmaxTxSizeDerivedItemCount
                        , scanIsComplete
                        , Raw.psuccessorIsExactV1
                            # pre
                            # witness
                            # ( Raw.preplaceStageV1
                                  # frame
                                  # pfromData (poneStep'workWitnessCbor stepWitness)
                                  # 2
                              )
                        ]
