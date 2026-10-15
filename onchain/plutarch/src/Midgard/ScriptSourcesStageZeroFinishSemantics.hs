{-# LANGUAGE OverloadedStrings #-}

-- | Size-bounded target stage-zero ScriptSources finish predicate.
module Midgard.ScriptSourcesStageZeroFinishSemantics (pbaseIsBound, pverify) where

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

pemptyObserverWire :: forall s. Term s (PData :--> PBool)
pemptyObserverWire = phoistAcyclic $ plam $ \dat ->
  plet (pasList # dat) $ \items ->
    pand'List
      [ plength # items #== 3
      , pasInt # (pelemAt # 0 # items) #== 0
      , pasByteStr # (pelemAt # 1 # items) #== pconstant ""
      , pasInt # (pelemAt # 2 # items) #== 0
      ]

pemptyMintWire :: forall s. Term s (PData :--> PBool)
pemptyMintWire = phoistAcyclic $ plam $ \dat ->
  plet (pasList # dat) $ \items ->
    pand'List
      [ plength # items #== 12
      , pasInt # (pelemAt # 0 # items) #== (-1)
      , pasInt # (pelemAt # 1 # items) #== 0
      , pasByteStr # (pelemAt # 2 # items) #== pconstant ""
      , pasByteStr # (pelemAt # 3 # items) #== pconstant ""
      , pasInt # (pelemAt # 4 # items) #== 0
      , pasByteStr # (pelemAt # 5 # items) #== pconstant ""
      , pasInt # (pelemAt # 6 # items) #== 0
      , pasInt # (pelemAt # 7 # items) #== 0
      , pasInt # (pelemAt # 8 # items) #== 0
      , pasByteStr # (pelemAt # 9 # items) #== pconstant ""
      , pasInt # (pelemAt # 10 # items) #== 0
      , pasList # (pelemAt # 11 # items) #== pnil
      ]

pbaseIsBound :: forall s. Term s (Raw.PFrame :--> PBool)
pbaseIsBound = phoistAcyclic $ plam $ \frame ->
  pmatch frame $ \f ->
    plet (Raw.pitemIntV1 # frame # 10) $ \sourceCount ->
      plet (Raw.pitemIntV1 # frame # 25) $ \sourceTotalCount ->
        pand'List
          [ Raw.pitemIntV1 # frame # 4 #>= 0
          , plengthBS # (Raw.pitemBytesV1 # frame # 5) #== 32
          , Raw.pitemIntV1 # frame # 6 #>= 0
          , plengthBS # (Raw.pitemBytesV1 # frame # 7) #== 32
          , Raw.pitemFrontierV1 # frame # 8 #== pnil
          , Raw.pitemIntV1 # frame # 14 #== 0
          , plengthBS # (Raw.pitemBytesV1 # frame # 15) #== 32
          , plengthBS # (Raw.pitemBytesV1 # frame # 16) #== 32
          , plengthBS # (Raw.pitemBytesV1 # frame # 29) #== 32
          , Raw.pitemIntV1 # frame # 17 #== 0
          , pfrontierIsWellFormed # sourceCount # (Raw.pitemFrontierV1 # frame # 11)
          , sourceTotalCount #>= sourceCount
          , sourceTotalCount #<= pmaxTxSizeDerivedItemCount
          , Raw.pitemIntV1 # frame # 12 #== 0
          , Raw.pitemFrontierV1 # frame # 13 #== pnil
          , Raw.pitemIntV1 # frame # 26 #== 0
          , Raw.pitemIntV1 # frame # 18 #== 0
          , Raw.pitemFrontierV1 # frame # 19 #== pnil
          , Raw.pitemIntV1 # frame # 20 #== 0
          , Raw.pitemIntV1 # frame # 21 #== 0
          , Raw.pitemFrontierV1 # frame # 22 #== pnil
          , Raw.pitemIntV1 # frame # 23 #== 0
          , pemptyReceiveWire # (pelemAt # 24 # Raw.pframe'items f)
          , pemptyObserverWire # (pelemAt # 27 # Raw.pframe'items f)
          , pemptyMintWire # (pelemAt # 28 # Raw.pframe'items f)
          ]

pverify ::
  forall s.
  Term s (PValidationMachineStateV1 :--> PValidationOneStepWitnessV1 :--> PBool)
pverify = phoistAcyclic $ plam $ \pre witness ->
  pmatch witness $ \stepWitness ->
    plet (Raw.popenFrameV1 # pre # witness # 30 # 0) $ \frame ->
      pmatch frame $ \f ->
        plet (Raw.pitemIntV1 # frame # 10) $ \sourceCount ->
          plet (Raw.pitemIntV1 # frame # 25) $ \sourceTotalCount ->
            pmatch (NativeCompact.pdecodeNativeTxWitnessSetCompact # Raw.pframe'witnessSetCompactCbor f) $ \witnessSet ->
              plet (pfromData $ pwitnessSetCompact'scriptTxWitsHash witnessSet) $ \scriptCommitment ->
                plet
                  ( pif
                      (scriptCommitment #== NativeField.pemptyFieldCommitment)
                      ( sourceCount
                          #== 0
                          #&& sourceTotalCount
                          #== 0
                          #&& Raw.pitemFrontierV1
                          # frame
                          # 11
                          #== pnil
                      )
                      (sourceTotalCount #> 0 #&& sourceCount #== sourceTotalCount)
                  )
                  $ \sourceScanIsComplete ->
                    pand'List
                      [ pbaseIsBound # frame
                      , sourceScanIsComplete
                      , Raw.psuccessorIsExactV1
                          # pre
                          # witness
                          # ( Raw.preplaceStageV1
                                # frame
                                # pfromData (poneStep'workWitnessCbor stepWitness)
                                # 1
                            )
                      ]
