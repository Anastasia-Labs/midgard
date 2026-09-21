{-# LANGUAGE OverloadedStrings #-}

{- | Compact, raw-frame implementations of the split stage-seven observer work.

Keeping the canonical control as its 30-item CBOR frame avoids rebuilding the
entire record in each rewarding script.  The checked decoder below preserves
the target's field types, nested scan shapes, frontier shapes, stage guard and
32-byte resolution-schedule guard before either semantic predicate runs.
-}
module Midgard.ScriptSourcesStageSevenSemantics (pitemFacts, pbound, preceive, pfinish) where

import Aiken.Cbor (pdeserialise)
import Midgard.BoundedCollection (pmaxTxSizeDerivedItemCount)
import Midgard.FraudProofs.NativeTx.Codec (pcborInt, pencodeDefiniteArrayHeader, pencodeDefiniteBytes)
import Midgard.FraudProofs.NativeTx.Compact qualified as Native
import Midgard.FraudProofs.NativeTx.Types
import Midgard.LedgerOutput qualified as LedgerOutput
import Midgard.NativeTxFieldAccess qualified as Field
import Midgard.ScriptProof qualified as Script
import Midgard.ScriptSourcesRawFrame qualified as Raw
import Midgard.ValidationMachine (PValidationOneStepWitnessV1 (..), prejectedSuccessorIsExact)
import Midgard.ValidationMachineFieldDoor qualified as Door
import Midgard.ValidationMerkle (PFrontierPeak (..), pappendLeaf, pencodeFrontier, pfrontierIsWellFormed, pverifyMembership)
import Midgard.ValidationTrace (PValidationMachineStateV1 (..), phashValidationContext)
import Plutarch.Core.Utils (pand'List, (#/=))
import Plutarch.Prelude

pisInt, pisBytes, pisList :: forall s. Term s (PData :--> PBool)
pisInt = phoistAcyclic $ plam $ \dat -> pchooseData # dat # pconstant False # pconstant False # pconstant False # pconstant True # pconstant False
pisBytes = phoistAcyclic $ plam $ \dat -> pchooseData # dat # pconstant False # pconstant False # pconstant False # pconstant False # pconstant True
pisList = phoistAcyclic $ plam $ \dat -> pchooseData # dat # pconstant False # pconstant False # pconstant True # pconstant False # pconstant False

pat :: forall s. Term s (PBuiltinList PData) -> Term s PInteger -> Term s PData
pat fields index = pelemAt # index # fields

pint :: forall s. Term s (PBuiltinList PData) -> Term s PInteger -> Term s PInteger
pint fields index = pasInt # pat fields index

pbytes :: forall s. Term s (PBuiltinList PData) -> Term s PInteger -> Term s PByteString
pbytes fields index = pasByteStr # pat fields index

pfrontierShape :: forall s. Term s (PData :--> PBool)
pfrontierShape = phoistAcyclic $ plam $ \dat ->
  pisList
    # dat
    #&& pall
    # plam
      ( \peak ->
          pisList
            # peak
            #&& plet
              (pasList # peak)
              (\fields -> plength # fields #== 2 #&& pisInt # pat fields 0 #&& pisBytes # pat fields 1)
      )
    # (pasList # dat)

pdecodeFrontier :: forall s. Term s PData -> Term s (PBuiltinList (PAsData PFrontierPeak))
pdecodeFrontier dat =
  pmap
    # plam
      ( \peak ->
          plet (pasList # peak) $ \fields ->
            pdata $ pcon $ PFrontierPeak (pdata $ pasInt # pat fields 0) (pdata $ pasByteStr # pat fields 1)
      )
    # (pasList # dat)

preplaceAt :: forall s. Term s (PInteger :--> PData :--> PBuiltinList PData :--> PBuiltinList PData)
preplaceAt = phoistAcyclic $ pfix $ \self -> plam $ \index value items ->
  pif
    (index #== 0)
    (pcons # value # (ptail # items))
    (pcons # (phead # items) # (self # (index - 1) # value # (ptail # items)))

pdataFromCbor :: forall s. Term s (PByteString :--> PData)
pdataFromCbor = phoistAcyclic $ plam $ \cbor ->
  pmatch (pdeserialise # cbor) $ \case
    PNothing -> perror
    PJust dat -> dat

pobserverScanComplete :: forall s. Term s (PBuiltinList PData :--> PByteString :--> PBool)
pobserverScanComplete = phoistAcyclic $ plam $ \items observerCommitment ->
  plet (pasList # pat items 27) $ \observer ->
    pif
      (observerCommitment #== Field.pemptyFieldCommitment)
      (pint observer 0 #== 0 #&& pbytes observer 1 #== pconstant "" #&& pint observer 2 #== 0)
      (pint observer 0 #> 0 #&& pint observer 2 #== pint observer 0)

pcontrolShape :: forall s. Term s (PBuiltinList PData :--> PBool)
pcontrolShape = phoistAcyclic $ plam $ \items ->
  pif
    (plength # items #== 30)
    ( plet (pasList # pat items 24) $ \receive ->
        plet (pasList # pat items 27) $ \observer ->
          plet (pasList # pat items 28) $ \mint ->
            pand'List
              [ pall # plam (\index -> pisInt # pat items index) # pconstant @(PBuiltinList PInteger) [4, 6, 9, 10, 12, 14, 17, 18, 20, 21, 23, 25, 26]
              , pall # plam (\index -> pisBytes # pat items index) # pconstant @(PBuiltinList PInteger) [0, 1, 2, 3, 5, 7, 15, 16, 29]
              , pall # plam (\index -> pfrontierShape # pat items index) # pconstant @(PBuiltinList PInteger) [8, 11, 13, 19, 22]
              , plength # receive #== 6
              , pall # plam (\index -> pisInt # pat receive index) # pconstant @(PBuiltinList PInteger) [0, 2]
              , pall # plam (\index -> pisBytes # pat receive index) # pconstant @(PBuiltinList PInteger) [3, 4]
              , pfrontierShape # pat receive 1
              , pfrontierShape # pat receive 5
              , plength # observer #== 3
              , pisInt # pat observer 0
              , pisBytes # pat observer 1
              , pisInt # pat observer 2
              , plength # mint #== 12
              , pall # plam (\index -> pisInt # pat mint index) # pconstant @(PBuiltinList PInteger) [0, 1, 4, 6, 7, 8, 10]
              , pall # plam (\index -> pisBytes # pat mint index) # pconstant @(PBuiltinList PInteger) [2, 3, 5, 9]
              , pfrontierShape # pat mint 11
              , plengthBS # pbytes items 29 #== 32
              , pint items 9 #== 7
              ]
    )
    (pconstant False)

popen :: forall s. Term s (PByteString :--> Raw.PFrame)
popen = phoistAcyclic $ plam $ \cbor ->
  pmatch (pdeserialise # cbor) $ \case
    PNothing -> perror
    PJust dat ->
      plet (pasList # dat) $ \items ->
        pif
          (pcontrolShape # items)
          ( pcon $
              Raw.PFrame
                items
                30
                (pconstant "")
                0
                (pbytes items 0)
                (pbytes items 1)
                (pbytes items 2)
                (pbytes items 3)
          )
          perror

pencodeRaw :: forall s. Term s (PData :--> PByteString)
pencodeRaw = phoistAcyclic $ pfix $ \self -> plam $ \dat ->
  pforce $
    pchooseData
      # dat
      # pdelay perror
      # pdelay perror
      # pdelay
        ( plet (pasList # dat) $ \items ->
            pfoldl
              # plam (\encoded item -> encoded <> self # item)
              # (pencodeDefiniteArrayHeader # (plength # items))
              # items
        )
      # pdelay (pcborInt $ pasInt # dat)
      # pdelay (pencodeDefiniteBytes # (pasByteStr # dat))

pcontrolIsBound :: forall s. Term s (PValidationMachineStateV1 :--> PValidationOneStepWitnessV1 :--> Raw.PFrame :--> PBool)
pcontrolIsBound = phoistAcyclic $ plam $ \pre witness frame ->
  pmatch pre $ \preState ->
    pmatch witness $ \stepWitness ->
      pmatch frame $ \f ->
        plet (Raw.pframe'items f) $ \items ->
          plet (pasList # pat items 24) $ \receive ->
            plet (pasList # pat items 27) $ \observer ->
              plet (pasList # pat items 28) $ \mint ->
                plet (pint receive 0) $ \receiveSourceCount ->
                  plet (pint receive 2) $ \receiveCount ->
                    plet (pint observer 0) $ \observerTotal ->
                      plet (pint observer 2) $ \observerSeen ->
                        plet (pint mint 0) $ \policyCount ->
                          plet (pint mint 1) $ \policyCursor ->
                            plet (pint mint 10) $ \assetCount ->
                              pand'List
                                [ pencodeRaw # pforgetData (pdata $ Raw.pframe'items f) #== pfromData (poneStep'workWitnessCbor stepWitness)
                                , Native.pnativeTxProofCommitmentV1 # Raw.pframe'compactCbor f # Raw.pframe'witnessSetCompactCbor f # Raw.pframe'fieldPreimageLengthsCbor f #== pfromData (pmachineState'transactionCommitment preState)
                                , phashValidationContext # Raw.pframe'contextCbor f #== pfromData (pmachineState'validationContextHash preState)
                                , pint items 25 #== pint items 10
                                , pfrontierIsWellFormed # pint items 10 # pdecodeFrontier (pat items 11)
                                , pint items 26 #== pint items 12
                                , pfrontierIsWellFormed # pint items 12 # pdecodeFrontier (pat items 13)
                                , pfrontierIsWellFormed # pint items 18 # pdecodeFrontier (pat items 19)
                                , pint items 23 #== pint items 21
                                , pint items 21 #<= pmaxTxSizeDerivedItemCount
                                , pint items 20 #>= 0
                                , pint items 20 #<= receiveSourceCount
                                , receiveSourceCount #>= 0
                                , receiveSourceCount #<= pint items 21
                                , pfrontierIsWellFormed # pint items 21 # pdecodeFrontier (pat items 22)
                                , pfrontierIsWellFormed # receiveSourceCount # pdecodeFrontier (pat receive 1)
                                , pfrontierIsWellFormed # pint items 21 # pdecodeFrontier (pat receive 5)
                                , receiveCount #>= 0
                                , receiveCount #<= receiveSourceCount
                                , pif (receiveCount #== 0) (pbytes receive 3 #== pconstant "") (plengthBS # pbytes receive 3 #== 28)
                                , pbytes receive 4 #== pconstant "" #|| (plengthBS # pbytes receive 4 #== 28 #&& (pbytes receive 3 #== pconstant "" #|| pbytes receive 3 #< pbytes receive 4))
                                , observerTotal #>= 0
                                , observerTotal #<= pmaxTxSizeDerivedItemCount
                                , observerSeen #>= 0
                                , observerSeen #<= observerTotal
                                , pif (observerSeen #== 0) (pbytes observer 1 #== pconstant "") (plengthBS # pbytes observer 1 #== 28)
                                , policyCount #>= 0
                                , policyCount #<= pmaxTxSizeDerivedItemCount
                                , policyCursor #== policyCount
                                , pbytes mint 3 #== pconstant ""
                                , pint mint 4 #== 0
                                , pbytes mint 5 #== pconstant ""
                                , pint mint 6 #== 0
                                , pint mint 7 #== 0
                                , pint mint 8 #== 0
                                , pbytes mint 9 #== pconstant ""
                                , assetCount #>= 0
                                , assetCount #<= LedgerOutput.pmaxDistinctAssetCount
                                , pfrontierIsWellFormed # assetCount # pdecodeFrontier (pat mint 11)
                                , pif (policyCursor #== 0) (pbytes mint 2 #== pconstant "") (plengthBS # pbytes mint 2 #== 28)
                                ]

pitemFacts :: forall s. Term s (PValidationMachineStateV1 :--> PValidationOneStepWitnessV1 :--> Door.PMachineFieldDoorV1 :--> PInteger :--> PInteger :--> Field.PFieldCarriageV1 :--> PByteString :--> PInteger :--> PBool)
pitemFacts = phoistAcyclic $ plam $ \pre witness door fieldIndex itemIndex carriage claimedHash claimedCount ->
  pmatch pre $ \preState ->
    pmatch witness $ \stepWitness ->
      plet (popen # pfromData (poneStep'workWitnessCbor stepWitness)) $ \frame ->
        pmatch frame $ \f ->
          plet (Raw.pframe'items f) $ \items ->
            plet (pasList # pat items 27) $ \observer ->
              plet (pint observer 0) $ \total ->
                plet (pint observer 2) $ \seen ->
                  pmatch
                    (Native.pverifyNativeTxProofSourceV1 # pfromData (pmachineState'transactionId preState) # Raw.pframe'compactCbor f # Raw.pframe'witnessSetCompactCbor f # Raw.pframe'fieldPreimageLengthsCbor f)
                    $ \(PPair verified witnesses) ->
                      pmatch verified $ \source ->
                        pmatch (pverified'txCompact source) $ \compact ->
                          pmatch (pcompact'body compact) $ \body ->
                            plet (Door.popenMachineFieldItem # door # verified # witnesses # 3 # seen # carriage) $ \item ->
                              plet (Door.pmachineFieldItemCount # item) $ \itemCount ->
                                plet (pif (total #== 0) itemCount total) $ \activeCount ->
                                  plet (Door.pmachineFieldItemBytes # item) $ \observerHash ->
                                    pand'List
                                      [ pbodyCompact'requiredObserversHash body #/= Field.pemptyFieldCommitment
                                      , pnot # (total #> 0 #&& seen #== total)
                                      , activeCount #> 0
                                      , activeCount #<= pmaxTxSizeDerivedItemCount
                                      , fieldIndex #== 3
                                      , itemIndex #== seen
                                      , itemCount #== activeCount
                                      , Door.pmachineFieldItemLength # item #== 28
                                      , claimedHash #== observerHash
                                      , claimedCount #== activeCount
                                      ]

pbound :: forall s. Term s (PValidationMachineStateV1 :--> PValidationOneStepWitnessV1 :--> PByteString :--> PInteger :--> PBool)
pbound = phoistAcyclic $ plam $ \pre witness observerHash activeCount ->
  pmatch witness $ \stepWitness ->
    plet (pfromData $ poneStep'workWitnessCbor stepWitness) $ \workCbor ->
      plet (popen # workCbor) $ \frame ->
        pmatch frame $ \f ->
          plet (Raw.pframe'items f) $ \items ->
            plet (pasList # pat items 24) $ \receive ->
              plet (pasList # pat items 27) $ \observer ->
                plet (pint observer 2) $ \seen ->
                  plet
                    ( pand'List
                        [ pcontrolIsBound # pre # witness # frame
                        , pint items 20 #== 0
                        , pint receive 2 #== 0
                        , pbytes receive 3 #== pconstant ""
                        , pbytes receive 4 #== pconstant ""
                        , plengthBS # observerHash #== 28
                        , activeCount #> 0
                        , seen #< activeCount
                        ]
                    )
                    $ \common ->
                      pif
                        (common #&& seen #> 0 #&& pnot # (pbytes observer 1 #< observerHash))
                        (prejectedSuccessorIsExact # pre # pfromData (poneStep'claimedSuccessor stepWitness) # pconstant "E_INVALID_FIELD_TYPE")
                        ( plet
                            (pappendLeaf # pint items 18 # pdecodeFrontier (pat items 19) # (Script.ppurposeLeafHash # 2 # seen # observerHash # observerHash))
                            $ \nextPeaks ->
                              plet
                                ( Raw.preplaceItemsV1
                                    # frame
                                    # workCbor
                                    # 27
                                    # 1
                                    # (pencodeDefiniteArrayHeader # 3 <> pcborInt activeCount <> pencodeDefiniteBytes # observerHash <> pcborInt (seen + 1))
                                )
                                $ \withObserver ->
                                  common
                                    #&& Raw.psuccessorIsExactV1
                                    # pre
                                    # witness
                                    # ( Raw.preplaceItemsV1
                                          # frame
                                          # withObserver
                                          # 18
                                          # 2
                                          # (pcborInt (pint items 18 + 1) <> pencodeFrontier # nextPeaks)
                                      )
                        )

preceive ::
  forall s.
  Term
    s
    ( PValidationMachineStateV1
        :--> PValidationOneStepWitnessV1
        :--> PInteger
        :--> PInteger
        :--> PByteString
        :--> PByteString
        :--> PBuiltinList (PAsData PByteString)
        :--> PBool
    )
preceive = phoistAcyclic $ plam $ \pre witness purposeKind purposeIndex scriptHash subject siblings ->
  pmatch witness $ \stepWitness ->
    plet (pfromData $ poneStep'workWitnessCbor stepWitness) $ \workCbor ->
      plet (popen # workCbor) $ \frame ->
        pmatch frame $ \f ->
          plet (Raw.pframe'items f) $ \items ->
            plet (pasList # pat items 24) $ \receive ->
              pmatch (Native.pdecodeNativeTxCompactV1 # Raw.pframe'compactCbor f) $ \compact ->
                pmatch (pcompact'body compact) $ \body ->
                  plet (pbodyCompact'requiredObserversHash body) $ \observerCommitment ->
                    plet (pint receive 0) $ \sourceCount ->
                      plet (pint items 20) $ \outputCursor ->
                        plet (pbytes receive 3) $ \previousHash ->
                          plet (pbytes receive 4) $ \candidateHash ->
                            plet
                              ( pif
                                  ( previousHash
                                      #== pconstant ""
                                      #|| previousHash
                                      #< scriptHash
                                  )
                                  ( pif
                                      ( candidateHash
                                          #== pconstant ""
                                          #|| scriptHash
                                          #< candidateHash
                                      )
                                      scriptHash
                                      candidateHash
                                  )
                                  candidateHash
                              )
                              $ \nextCandidate ->
                                plet
                                  ( Raw.preplaceItemsV1
                                      # frame
                                      # workCbor
                                      # 24
                                      # 1
                                      # ( pencodeDefiniteArrayHeader
                                            # 6
                                            <> pcborInt sourceCount
                                            <> pencodeFrontier
                                              # pdecodeFrontier (pat receive 1)
                                            <> pcborInt (pint receive 2)
                                            <> pencodeDefiniteBytes
                                              # previousHash
                                            <> pencodeDefiniteBytes
                                              # nextCandidate
                                            <> pencodeFrontier
                                              # pdecodeFrontier (pat receive 5)
                                        )
                                  )
                                  $ \withReceive ->
                                    pand'List
                                      [ pcontrolIsBound # pre # witness # frame
                                      , pobserverScanComplete # items # observerCommitment
                                      , outputCursor #< sourceCount
                                      , purposeKind #== 3
                                      , purposeIndex #== outputCursor
                                      , plengthBS # scriptHash #== 28
                                      , subject #== scriptHash
                                      , pverifyMembership
                                          # sourceCount
                                          # pdecodeFrontier (pat receive 1)
                                          # purposeIndex
                                          # (Script.ppurposeLeafHash # purposeKind # purposeIndex # scriptHash # subject)
                                          # siblings
                                      , Raw.psuccessorIsExactV1
                                          # pre
                                          # witness
                                          # ( Raw.preplaceItemsV1
                                                # frame
                                                # withReceive
                                                # 20
                                                # 1
                                                # pcborInt (outputCursor + 1)
                                            )
                                      ]

pfinish ::
  forall s.
  Term s (PValidationMachineStateV1 :--> PValidationOneStepWitnessV1 :--> PBool)
pfinish = phoistAcyclic $ plam $ \pre witness ->
  pmatch witness $ \stepWitness ->
    plet (pfromData $ poneStep'workWitnessCbor stepWitness) $ \workCbor ->
      plet (popen # workCbor) $ \frame ->
        pmatch frame $ \f ->
          plet (Raw.pframe'items f) $ \items ->
            plet (pasList # pat items 24) $ \receive ->
              pmatch (Native.pdecodeNativeTxCompactV1 # Raw.pframe'compactCbor f) $ \compact ->
                pmatch (pcompact'body compact) $ \body ->
                  plet (pbodyCompact'requiredObserversHash body) $ \observerCommitment ->
                    plet (pint receive 0) $ \sourceCount ->
                      plet (pbytes receive 4) $ \candidateHash ->
                        plet
                          ( pcontrolIsBound
                              # pre
                              # witness
                              # frame
                              #&& pobserverScanComplete
                              # items
                              # observerCommitment
                              #&& pint items 20
                              #== sourceCount
                          )
                          $ \common ->
                            pif
                              (candidateHash #== pconstant "")
                              ( plet
                                  ( pencodeDefiniteArrayHeader
                                      # 6
                                      <> pcborInt 0
                                      <> pencodeFrontier
                                        # pnil
                                      <> pcborInt 0
                                      <> pencodeDefiniteBytes
                                        # pconstant ""
                                      <> pencodeDefiniteBytes
                                        # pconstant ""
                                      <> pencodeFrontier
                                        # pdecodeFrontier (pat receive 5)
                                  )
                                  $ \emptyReceive ->
                                    plet
                                      ( preplaceAt
                                          # 27
                                          # (pdataFromCbor # pconstant "\x83\x00\x40\x00")
                                          # items
                                      )
                                      $ \withObserver ->
                                        plet
                                          (preplaceAt # 24 # (pdataFromCbor # emptyReceive) # withObserver)
                                          $ \withReceive ->
                                            plet
                                              (preplaceAt # 20 # pforgetData (pdata $ pint items 21) # withReceive)
                                              $ \withCursor ->
                                                plet
                                                  (preplaceAt # 9 # pforgetData (pdata $ pconstant @PInteger 8) # withCursor)
                                                  $ \nextItems ->
                                                    common
                                                      #&& Raw.psuccessorIsExactV1
                                                      # pre
                                                      # witness
                                                      # ( Raw.pappendExtensionV1
                                                            # (pencodeRaw # (plistData # nextItems))
                                                            # pconstant "\x8f\x00\x00\x00\x20\x20\x40\x40\x20\x20\x40\x40\x40\x40\x00\x80"
                                                        )
                              )
                              ( plet
                                  ( pappendLeaf
                                      # pint items 18
                                      # pdecodeFrontier (pat items 19)
                                      # ( Script.ppurposeLeafHash
                                            # 3
                                            # pint receive 2
                                            # candidateHash
                                            # candidateHash
                                        )
                                  )
                                  $ \nextPurposePeaks ->
                                    plet
                                      ( pencodeDefiniteArrayHeader
                                          # 6
                                          <> pcborInt sourceCount
                                          <> pencodeFrontier
                                            # pdecodeFrontier (pat receive 1)
                                          <> pcborInt (pint receive 2 + 1)
                                          <> pencodeDefiniteBytes
                                            # candidateHash
                                          <> pencodeDefiniteBytes
                                            # pconstant ""
                                          <> pencodeFrontier
                                            # pdecodeFrontier (pat receive 5)
                                      )
                                      $ \nextReceive ->
                                        plet
                                          ( Raw.preplaceItemsV1
                                              # frame
                                              # workCbor
                                              # 24
                                              # 1
                                              # nextReceive
                                          )
                                          $ \withReceive ->
                                            common
                                              #&& Raw.psuccessorIsExactV1
                                              # pre
                                              # witness
                                              # ( Raw.preplaceItemsV1
                                                    # frame
                                                    # withReceive
                                                    # 18
                                                    # 3
                                                    # ( pcborInt (pint items 18 + 1)
                                                          <> pencodeFrontier
                                                            # nextPurposePeaks
                                                          <> pcborInt 0
                                                      )
                                                )
                              )
