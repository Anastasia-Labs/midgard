{-# LANGUAGE OverloadedStrings #-}

-- | Size-bounded raw witness checks used by the late ScriptSources resolvers.
module Midgard.ScriptSourcesLateRawSemantics (
  pverifyDescriptorStepClaim,
  pverifyDescriptorBegin,
  pverifyDescriptorMismatch,
  pverifyDescriptorUsed,
) where

import Aiken.Cbor (pdeserialise)
import Midgard.FraudProofs.NativeTx.Codec (pcborInt, pencodeDefiniteArrayHeader, pencodeDefiniteBytes)
import Midgard.FraudProofs.NativeTx.Compact qualified as Native
import Plutarch.Builtin.ByteString (pbyteStringToInteger, pmostSignificantFirst)
import Plutarch.Core.Utils (pand'List, (#/=))
import Plutarch.Prelude

import Midgard.RedeemerItemProof qualified as RedeemerItemProof
import Midgard.ScriptProof qualified as ScriptProof
import Midgard.ScriptSourcesDescriptor qualified as Descriptor
import Midgard.ScriptSourcesRawFrame qualified as Raw
import Midgard.ValidationMachine (PValidationOneStepWitnessV1 (..), prejectedSuccessorIsExact)
import Midgard.ValidationMerkle (PFrontierPeak (..), pfrontierIsWellFormed, pverifyMembership)
import Midgard.ValidationTrace (PValidationMachineStateV1 (..), phashValidationContext)

pisInt, pisBytes, pisList :: forall s. Term s (PData :--> PBool)
pisInt = phoistAcyclic $ plam $ \dat ->
  pchooseData # dat # pconstant False # pconstant False # pconstant False # pconstant True # pconstant False
pisBytes = phoistAcyclic $ plam $ \dat ->
  pchooseData # dat # pconstant False # pconstant False # pconstant False # pconstant False # pconstant True
pisList = phoistAcyclic $ plam $ \dat ->
  pchooseData # dat # pconstant False # pconstant False # pconstant True # pconstant False # pconstant False

pat :: forall s. Term s (PBuiltinList PData) -> Term s PInteger -> Term s PData
pat fields index = pelemAt # index # fields

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

pbitmapShape :: forall s. Term s (PData :--> PBool)
pbitmapShape = phoistAcyclic $ plam $ \dat ->
  pisBytes
    # dat
    #&& plet (plengthBS # (pasByteStr # dat)) (\length -> length #<= 2048 #&& (length #== 0 #|| pindexBS # (pasByteStr # dat) # 0 #/= (pintegerToByte # 0)))

pbaseShape :: forall s. Term s (PBuiltinList PData :--> PBool)
pbaseShape = phoistAcyclic $ plam $ \items ->
  plet (pasList # pat items 24) $ \receive ->
    plet (pasList # pat items 27) $ \observer ->
      plet (pasList # pat items 28) $ \mint ->
        pand'List
          [ pall # plam (\index -> pisInt # pat items index) # pconstant @(PBuiltinList PInteger) [4, 6, 9, 10, 12, 14, 17, 18, 20, 21, 23, 25, 26]
          , pall # plam (\index -> pisBytes # pat items index) # pconstant @(PBuiltinList PInteger) [0, 1, 2, 3, 5, 7, 15, 16, 29, 30]
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
          ]

pdiscoveryShape :: forall s. Term s (PBuiltinList PData :--> PBool)
pdiscoveryShape = phoistAcyclic $ plam $ \items ->
  pand'List
    [ plength # items #== 15
    , pall # plam (\index -> pisInt # pat items index) # pconstant @(PBuiltinList PInteger) [0, 1, 2, 3, 4, 7, 8, 13]
    , pall # plam (\index -> pisBytes # pat items index) # pconstant @(PBuiltinList PInteger) [5, 6, 9, 12]
    , pbitmapShape # pat items 10
    , pbitmapShape # pat items 11
    , pfrontierShape # pat items 14
    ]

pdecodeFrontier :: forall s. Term s PData -> Term s (PBuiltinList (PAsData PFrontierPeak))
pdecodeFrontier dat =
  pmap
    # plam
      ( \peak ->
          plet (pasList # peak) $ \fields ->
            pdata $ pcon $ PFrontierPeak (pdata $ pasInt # pat fields 0) (pdata $ pasByteStr # pat fields 1)
      )
    # (pasList # dat)

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

preplaceAt :: forall s. Term s (PInteger :--> PData :--> PBuiltinList PData :--> PBuiltinList PData)
preplaceAt = phoistAcyclic $ pfix $ \self -> plam $ \index value items ->
  pif
    (index #== 0)
    (pcons # value # (ptail # items))
    (pcons # (phead # items) # (self # (index - 1) # value # (ptail # items)))

pbitmap :: forall s. Term s PData -> Term s PInteger
pbitmap dat = pbyteStringToInteger # pmostSignificantFirst # (pasByteStr # dat)

ppow2 :: forall s. Term s (PInteger :--> PInteger)
ppow2 = phoistAcyclic $ pfix $ \self -> plam $ \exponent ->
  pif (exponent #== 0) 1 (2 * (self # (exponent - 1)))

pdiscoveryFrameIsExact :: forall s. Term s (Raw.PFrame :--> PValidationOneStepWitnessV1 :--> PBuiltinList PData :--> PBool)
pdiscoveryFrameIsExact = phoistAcyclic $ plam $ \frame witness discovery ->
  pmatch frame $ \f ->
    pmatch witness $ \w ->
      plet (pencodeDefiniteBytes # (pencodeRaw # pforgetData (pdata discovery))) $ \encoded ->
        plet (pfromData $ poneStep'workWitnessCbor w) $ \workCbor ->
          plet (plengthBS # workCbor - plengthBS # encoded) $ \offset ->
            offset
              #>= Raw.pframe'stageOffset f
              + 1
                #&& psliceBS
                # offset
                # (plengthBS # encoded)
                # workCbor
                #== encoded

pcontrolIsBound :: forall s. Term s (PInteger :--> PValidationMachineStateV1 :--> PValidationOneStepWitnessV1 :--> Raw.PFrame :--> PBuiltinList PData :--> PBool)
pcontrolIsBound = phoistAcyclic $ plam $ \stage pre witness frame discovery ->
  pmatch pre $ \preState ->
    pmatch frame $ \f ->
      plet (Raw.pframe'items f) $ \items ->
        plet (pasList # pat items 24) $ \receive ->
          plet (pasList # pat items 27) $ \observer ->
            plet (pasList # pat items 28) $ \mint ->
              plet (pasInt # pat discovery 0) $ \purposeCursor ->
                plet (pasInt # pat discovery 1) $ \sourceCursor ->
                  plet (pasInt # pat discovery 2) $ \redeemerCursor ->
                    plet (pasInt # pat discovery 13) $ \executionCount ->
                      plet (pasInt # pat items 10) $ \sourceCount ->
                        plet (pasInt # pat items 12) $ \redeemerCount ->
                          plet (pasInt # pat items 18) $ \purposeCount ->
                            pand'List
                              [ pbaseShape # items
                              , pdiscoveryShape # discovery
                              , pdiscoveryFrameIsExact # frame # witness # discovery
                              , plengthBS # (pasByteStr # pat items 29) #== 32
                              , Native.pnativeTxProofCommitmentV1 # Raw.pframe'compactCbor f # Raw.pframe'witnessSetCompactCbor f # Raw.pframe'fieldPreimageLengthsCbor f #== pfromData (pmachineState'transactionCommitment preState)
                              , phashValidationContext # Raw.pframe'contextCbor f #== pfromData (pmachineState'validationContextHash preState)
                              , pasInt # pat items 25 #== sourceCount
                              , pfrontierIsWellFormed # sourceCount # pdecodeFrontier (pat items 11)
                              , pasInt # pat items 26 #== redeemerCount
                              , pfrontierIsWellFormed # redeemerCount # pdecodeFrontier (pat items 13)
                              , pfrontierIsWellFormed # purposeCount # pdecodeFrontier (pat items 19)
                              , redeemerCursor #>= 0
                              , redeemerCursor #<= redeemerCount
                              , executionCount #== purposeCursor
                              , pfrontierIsWellFormed # executionCount # pdecodeFrontier (pat discovery 14)
                              , pbitmap (pat discovery 10) #< ppow2 # sourceCount
                              , pbitmap (pat discovery 11) #< ppow2 # redeemerCount
                              , pif
                                  (stage #== 10)
                                  ( pand'List
                                      [ purposeCursor #< purposeCount
                                      , sourceCursor #>= 0
                                      , sourceCursor #<= sourceCount
                                      , pasInt # pat discovery 3 #>= 0
                                      , pasInt # pat discovery 3 #<= 3
                                      , pasInt # pat discovery 4 #>= 0
                                      , plengthBS # (pasByteStr # pat discovery 5) #== 28
                                      , pasByteStr # pat discovery 6 #/= pconstant ""
                                      , pasInt # pat discovery 7 #>= 0
                                      , pasInt # pat discovery 7 #< sourceCount
                                      , pasInt # pat discovery 8 #== 3 #|| pasInt # pat discovery 8 #== 128
                                      , plengthBS # (pasByteStr # pat discovery 9) #== 32
                                      ]
                                  )
                                  ( pand'List
                                      [ stage #== 12
                                      , purposeCount #== purposeCursor
                                      , sourceCursor #== sourceCount
                                      , pasInt # pat discovery 3 #== -1
                                      , pasInt # pat discovery 4 #== -1
                                      , pasByteStr # pat discovery 5 #== pconstant ""
                                      , pasByteStr # pat discovery 6 #== pconstant ""
                                      , pasInt # pat discovery 7 #== -1
                                      , pasInt # pat discovery 8 #== -1
                                      , pasByteStr # pat discovery 9 #== pconstant ""
                                      ]
                                  )
                              , plength # receive #== 6
                              , plength # observer #== 3
                              , plength # mint #== 12
                              ]

psuccessorIsExact :: forall s. Term s (PInteger :--> PValidationMachineStateV1 :--> PValidationOneStepWitnessV1 :--> Raw.PFrame :--> PBuiltinList PData :--> PBool)
psuccessorIsExact = phoistAcyclic $ plam $ \nextStage pre witness frame nextDiscovery ->
  pmatch witness $ \w ->
    plet (pfromData $ poneStep'workWitnessCbor w) $ \workCbor ->
      plet
        ( Raw.preplaceItemsV1
            # frame
            # workCbor
            # 30
            # 1
            # (pencodeDefiniteBytes # (pencodeRaw # pforgetData (pdata nextDiscovery)))
        )
        $ \withDiscovery ->
          Raw.psuccessorIsExactV1
            # pre
            # witness
            # (Raw.preplaceStageV1 # frame # withDiscovery # nextStage)

pwithFrame ::
  forall s.
  Term s PInteger ->
  Term s PValidationMachineStateV1 ->
  Term s PValidationOneStepWitnessV1 ->
  (Term s Raw.PFrame -> Term s (PBuiltinList PData) -> Term s PBool) ->
  Term s PBool
pwithFrame stage pre witness use =
  plet (Raw.popenFrameV1 # pre # witness # 31 # stage) $ \frame ->
    pmatch frame $ \f ->
      plet (Raw.pframe'items f) $ \items ->
        pmatch (pdeserialise # (pasByteStr # pat items 30)) $ \case
          PNothing -> perror
          PJust discoveryData ->
            plet (pasList # discoveryData) $ \discovery ->
              pif (pcontrolIsBound # stage # pre # witness # frame # discovery) (use frame discovery) perror

pverifyDescriptorBegin ::
  forall s.
  Term
    s
    ( PInteger
        :--> PValidationMachineStateV1
        :--> PValidationOneStepWitnessV1
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PByteString
        :--> PBuiltinList (PAsData PByteString)
        :--> PBool
    )
pverifyDescriptorBegin = phoistAcyclic $ plam $ \stage pre witness itemIndex itemCount totalLength itemCommitment siblings ->
  pwithFrame stage pre witness $ \frame discovery ->
    pmatch frame $ \f ->
      plet (Raw.pframe'items f) $ \items ->
        plet
          ( RedeemerItemProof.pinitialControlV1
              # RedeemerItemProof.pmodeDescriptor
              # itemIndex
              # itemCount
              # totalLength
              # itemCommitment
              # (-1)
              # (-1)
          )
          $ \initial ->
            pand'List
              [ pasInt # pat discovery 2 #< pasInt # pat items 12
              , pasByteStr # pat discovery 12 #== pconstant ""
              , itemIndex #== pasInt # pat discovery 2
              , itemCount #== pasInt # pat items 12
              , pverifyMembership
                  # itemCount
                  # pdecodeFrontier (pat items 13)
                  # itemIndex
                  # (ScriptProof.predeemerItemLeafHash # itemIndex # itemCommitment)
                  # siblings
              , psuccessorIsExact
                  # stage
                  # pre
                  # witness
                  # frame
                  # ( preplaceAt
                        # 12
                        # pforgetData (pdata $ RedeemerItemProof.phashDescriptorControlV1 # initial)
                        # discovery
                    )
              ]

pverifyDescriptorMismatch ::
  forall s.
  Term s (PValidationMachineStateV1 :--> PValidationOneStepWitnessV1 :--> Descriptor.PDescriptorStepClaim :--> PBool)
pverifyDescriptorMismatch = phoistAcyclic $ plam $ \pre witness claim ->
  pwithFrame 10 pre witness $ \frame discovery ->
    pmatch frame $ \f ->
      plet (Raw.pframe'items f) $ \items ->
        pmatch claim $ \claimFields ->
          plet (Descriptor.pfullControl # pfromData (Descriptor.pdescriptorClaim'claimedNext claimFields)) $ \next ->
            pmatch next $ \nextFields ->
              plet (pfromData (RedeemerItemProof.predeemerControl'stage nextFields) #== RedeemerItemProof.pstageTerminal) $ \terminal ->
                plet
                  ( pif
                      (pasInt # pat discovery 3 #== 0)
                      0
                      ( pif
                          (pasInt # pat discovery 3 #== 1)
                          1
                          (pif (pasInt # pat discovery 3 #== 2) 3 (pif (pasInt # pat discovery 3 #== 3) 6 (-1)))
                      )
                  )
                  $ \expectedTag ->
                    plet
                      ( terminal
                          #&& pfromData
                            (RedeemerItemProof.predeemerControl'purposeTag nextFields)
                          #== expectedTag
                          #&& pfromData
                            (RedeemerItemProof.predeemerControl'pointerIndex nextFields)
                          #== pasInt
                          # pat discovery 4
                      )
                      $ \matches ->
                        plet
                          ( pif
                              terminal
                              ( preplaceAt
                                  # 12
                                  # pforgetData (pdata $ pconstant @PByteString "")
                                  # ( preplaceAt
                                        # 2
                                        # pforgetData (pdata $ pfromData (RedeemerItemProof.predeemerControl'itemIndex nextFields) + 1)
                                        # discovery
                                    )
                              )
                              ( preplaceAt
                                  # 12
                                  # pforgetData (pdata $ RedeemerItemProof.phashDescriptorControlV1 # next)
                                  # discovery
                              )
                          )
                          $ \after ->
                            pand'List
                              [ pasInt # pat discovery 2 #< pasInt # pat items 12
                              , pasByteStr # pat discovery 12 #/= pconstant ""
                              , pnot # matches
                              , psuccessorIsExact # 10 # pre # witness # frame # after
                              ]

pverifyDescriptorUsed ::
  forall s.
  Term s (PValidationMachineStateV1 :--> PValidationOneStepWitnessV1 :--> Descriptor.PDescriptorStepClaim :--> PBool)
pverifyDescriptorUsed = phoistAcyclic $ plam $ \pre witness claim ->
  pwithFrame 12 pre witness $ \frame discovery ->
    pmatch frame $ \f ->
      plet (Raw.pframe'items f) $ \items ->
        pmatch claim $ \claimFields ->
          plet (Descriptor.pfullControl # pfromData (Descriptor.pdescriptorClaim'claimedNext claimFields)) $ \next ->
            pmatch next $ \nextFields ->
              plet (pfromData (RedeemerItemProof.predeemerControl'stage nextFields) #== RedeemerItemProof.pstageTerminal) $ \terminal ->
                plet (pfromData $ RedeemerItemProof.predeemerControl'itemIndex nextFields) $ \nextIndex ->
                  plet
                    ( pif
                        terminal
                        ( preplaceAt
                            # 12
                            # pforgetData (pdata $ pconstant @PByteString "")
                            # (preplaceAt # 2 # pforgetData (pdata $ nextIndex + 1) # discovery)
                        )
                        ( preplaceAt
                            # 12
                            # pforgetData (pdata $ RedeemerItemProof.phashDescriptorControlV1 # next)
                            # discovery
                        )
                    )
                    $ \after ->
                      pand'List
                        [ pasInt # pat discovery 2 #< pasInt # pat items 12
                        , pasByteStr # pat discovery 12 #/= pconstant ""
                        , pif
                            ( terminal
                                #&& pmod
                                # (pdiv # pbitmap (pat discovery 11) # (ppow2 # nextIndex))
                                # 2
                                #== 0
                            )
                            ( pmatch witness $ \w ->
                                prejectedSuccessorIsExact
                                  # pre
                                  # pfromData (poneStep'claimedSuccessor w)
                                  # pconstant "E_INVALID_FIELD_TYPE"
                            )
                            (psuccessorIsExact # 12 # pre # witness # frame # after)
                        ]

pverifyDescriptorStepClaim ::
  forall s.
  Term s (PValidationOneStepWitnessV1 :--> Descriptor.PDescriptorStepClaim :--> PBool)
pverifyDescriptorStepClaim = phoistAcyclic $ plam $ \witness claim ->
  pmatch witness $ \stepWitness ->
    pmatch (pdeserialise # pfromData (poneStep'workWitnessCbor stepWitness)) $ \case
      PNothing -> perror
      PJust workData ->
        plet (pasList # workData) $ \items ->
          pif
            (plength # items #== 31 #&& pbaseShape # items)
            ( pmatch (pdeserialise # (pasByteStr # pat items 30)) $ \case
                PNothing -> perror
                PJust discoveryData ->
                  plet (pasList # discoveryData) $ \discovery ->
                    pif
                      (pdiscoveryShape # discovery)
                      ( pmatch claim $ \claimFields ->
                          plet (Descriptor.pfullControl # pfromData (Descriptor.pdescriptorClaim'control claimFields)) $ \current ->
                            plet (Descriptor.pfullControl # pfromData (Descriptor.pdescriptorClaim'claimedNext claimFields)) $ \claimedNext ->
                              pmatch current $ \currentFields ->
                                pand'List
                                  [ pasInt # pat items 9 #== 10 #|| pasInt # pat items 9 #== 12
                                  , pasInt # pat discovery 2 #< pasInt # pat items 12
                                  , pasByteStr # pat discovery 12 #/= pconstant ""
                                  , pfromData (RedeemerItemProof.predeemerControl'itemIndex currentFields) #== pasInt # pat discovery 2
                                  , pfromData (RedeemerItemProof.predeemerControl'itemCount currentFields) #== pasInt # pat items 12
                                  , RedeemerItemProof.phashDescriptorControlV1 # current #== pasByteStr # pat discovery 12
                                  , RedeemerItemProof.pdescriptorStepV1
                                      # current
                                      # pfromData (Descriptor.pdescriptorClaim'openTail claimFields)
                                      # pfromData (Descriptor.pdescriptorClaim'chunkProof claimFields)
                                      # pfromData (Descriptor.pdescriptorClaim'nextChunkProof claimFields)
                                      #== pcon (PJust $ pcon $ RedeemerItemProof.PRedeemerItemProofAdvanced $ pdata claimedNext)
                                  ]
                      )
                      perror
            )
            perror
