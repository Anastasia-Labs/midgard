{-# LANGUAGE OverloadedStrings #-}

module Testing.CekDataScan (tests) where

import Data.ByteString qualified as BS
import Data.Kind (Type)
import Plutarch.Builtin.Crypto (pblake2b_256)
import Plutarch.Core.Utils (pand'List)
import Plutarch.Prelude
import PlutusCore.Data qualified as PD
import Test.Tasty
import Test.Tasty.HUnit

import Midgard.CekData qualified as Data
import Midgard.CekDataScan qualified as Scan
import Midgard.ValidationMerkle qualified as Merkle
import Testing.Eval (passertEval)

tests :: TestTree
tests =
  testGroup
    "Midgard.CekDataScan"
    [ testCase "list children fold to the exact semantic data root" $ passertEval listFold
    , testCase "map pair folds to the exact semantic data root" $ passertEval mapFold
    , testCase "frame hash binds the parent stack link" $ passertEval frameHashBindsTail
    , testCase "raw list cursor authenticates each leaf and exact break" $ passertEval rawListCursor
    , testCase "raw small constructor and map headers are exact" $ passertEval exactHeaders
    , testCase "completed nested frame pops into its authenticated parent" $ passertEval completedFramePops
    , testCase "maximum three-chunk blob root matches offchain commitment" $ passertEval maximumBlobRoot
    , testCase "revealed large bytes leaf uses the exact chunked semantic root" $ passertEval revealedLargeBytes
    , testCase "direct and streamed large bytes use the same semantic root" $ passertEval directAndStreamed
    , testCase "large constructor header and summary match direct semantics" $ passertEval largeConstructor
    , testCase "nested control steps reconstruct the exact semantic root" $ passertEval nestedControl
    , testCase "cross-language data scan control hash vectors" $ passertEval controlHashVectors
    ]

pemptyListFrame :: forall (s :: S). Term s PInteger -> Term s Scan.PDataScanFrameV1
pemptyListFrame expectedChildren =
  pframe Scan.plistFrame expectedChildren Data.pemptyDataListSummaryV1 (pconstant "")

pemptyMapFrame :: forall (s :: S). Term s PInteger -> Term s Scan.PDataScanFrameV1
pemptyMapFrame expectedChildren =
  pframe Scan.pmapFrame expectedChildren Data.pemptyDataPairSummaryV1 (pconstant "")

pframe ::
  forall (s :: S).
  Term s PInteger ->
  Term s PInteger ->
  Term s Data.PDataSequenceSummaryV1 ->
  Term s PByteString ->
  Term s Scan.PDataScanFrameV1
pframe kind expectedChildren sequence tailRoot =
  pcon $
    Scan.PDataScanFrameV1
      { Scan.pframe'kind = pdata kind
      , Scan.pframe'constructor = pdata 0
      , Scan.pframe'tail = pdata tailRoot
      , Scan.pframe'expectedChildren = pdata expectedChildren
      , Scan.pframe'childCount = pdata 0
      , Scan.pframe'childPeaks = pdata Merkle.pemptyFrontier
      , Scan.pframe'foldCursor = pdata 0
      , Scan.pframe'sequence = pdata sequence
      }

poneSibling :: forall (s :: S). Term s PByteString -> Term s (PBuiltinList (PAsData PByteString))
poneSibling sibling = pcons # pdata sibling # pcon PNil

pexpectJust :: forall (s :: S) (a :: S -> Type). Term s (PMaybe a) -> Term s a
pexpectJust value = pmatch value $ \case PNothing -> perror; PJust exact -> exact

listFold :: forall (s :: S). Term s PBool
listFold =
  plet (pconstant (PD.I 1)) $ \firstData ->
    plet (pconstant (PD.B "\xab\xcd")) $ \secondData ->
      plet (Data.psemanticDataSummaryV1 # firstData) $ \first ->
        plet (Data.psemanticDataSummaryV1 # secondData) $ \second ->
          plet (pexpectJust (Scan.pappendChildV1 # pemptyListFrame 2 # first)) $ \withFirst ->
            plet (pexpectJust (Scan.pappendChildV1 # withFirst # second)) $ \complete ->
              plet
                (pexpectJust (Scan.pfoldListChildV1 # complete # 1 # second # poneSibling (Scan.pchildLeafHashV1 # 0 # first)))
                $ \foldedSecond ->
                  plet
                    (pexpectJust (Scan.pfoldListChildV1 # foldedSecond # 0 # first # poneSibling (Scan.pchildLeafHashV1 # 1 # second)))
                    $ \foldedFirst ->
                      pexpectJust (Scan.pfinalizedSummaryV1 # foldedFirst # pcon PNothing)
                        #== Data.psemanticDataSummaryV1 # pconstant (PD.List [PD.I 1, PD.B "\xab\xcd"])

mapFold :: forall (s :: S). Term s PBool
mapFold =
  plet (pconstant (PD.B "\x01")) $ \keyData ->
    plet (pconstant (PD.I (-7))) $ \valueData ->
      plet (Data.psemanticDataSummaryV1 # keyData) $ \key ->
        plet (Data.psemanticDataSummaryV1 # valueData) $ \value ->
          plet (pexpectJust (Scan.pappendChildV1 # pemptyMapFrame 2 # key)) $ \withKey ->
            plet (pexpectJust (Scan.pappendChildV1 # withKey # value)) $ \complete ->
              plet
                ( pexpectJust $
                    Scan.pfoldMapPairV1
                      # complete
                      # 0
                      # key
                      # value
                      # poneSibling (Scan.pchildLeafHashV1 # 1 # value)
                      # poneSibling (Scan.pchildLeafHashV1 # 0 # key)
                )
                $ \folded ->
                  pexpectJust (Scan.pfinalizedSummaryV1 # folded # pcon PNothing)
                    #== Data.psemanticDataSummaryV1 # pconstant (PD.Map [(PD.B "\x01", PD.I (-7))])

frameHashBindsTail :: forall (s :: S). Term s PBool
frameHashBindsTail =
  plet (pemptyListFrame 0) $ \frame ->
    pmatch frame $ \f ->
      pnot
        #$ Scan.phashFrameV1
        # frame
        #== Scan.phashFrameV1
        # pcon f {Scan.pframe'tail = pdata (phexByteStr (replicate 64 'a'))}

rawListCursor :: forall (s :: S). Term s PBool
rawListCursor =
  plet (phexByteStr "9f0142abcdff") $ \raw ->
    plet (pblake2b_256 # raw) $ \rawHash ->
      plet (pexpectJust (Scan.popenListAtV1 # raw # rawHash # 0 # 2 # pconstant "")) $ \opened ->
        pmatch opened $ \(Scan.POpenedDataFrameV1 nextOffset frame) ->
          plet (pexpectJust (Scan.psmallLeafAtV1 # raw # rawHash # 1 # phexByteStr "01")) $ \first ->
            pmatch first $ \(Scan.PRawDataLeafV1 firstNext firstSummary) ->
              plet (pexpectJust (Scan.psmallLeafAtV1 # raw # rawHash # firstNext # phexByteStr "42abcd")) $ \second ->
                pmatch second $ \(Scan.PRawDataLeafV1 secondNext secondSummary) ->
                  pmatch frame $ \f ->
                    plet
                      ( Merkle.pappendLeaf
                          # 1
                          # (Merkle.pappendLeaf # 0 # Merkle.pemptyFrontier # (Scan.pchildLeafHashV1 # 0 # firstSummary))
                          # (Scan.pchildLeafHashV1 # 1 # secondSummary)
                      )
                      $ \peaks ->
                        nextOffset #== 1
                          #&& pexpectJust
                            ( Scan.pcloseSequenceAtV1
                                # raw
                                # rawHash
                                # secondNext
                                # pcon f {Scan.pframe'childCount = pdata 2, Scan.pframe'childPeaks = pdata peaks}
                            )
                          #== 6

exactHeaders :: forall (s :: S). Term s PBool
exactHeaders =
  plet (phexByteStr "d8799f01ff") $ \constructorRaw ->
    plet (phexByteStr "a1410101") $ \mapRaw ->
      plet
        (pexpectJust (Scan.popenSmallConstructorAtV1 # constructorRaw # (pblake2b_256 # constructorRaw) # 0 # 0 # 1 # pconstant ""))
        $ \constructor ->
          plet (pexpectJust (Scan.popenMapAtV1 # mapRaw # (pblake2b_256 # mapRaw) # 0 # pconstant "")) $ \mapOpened ->
            pmatch constructor $ \(Scan.POpenedDataFrameV1 constructorOffset constructorFrame) ->
              pmatch mapOpened $ \(Scan.POpenedDataFrameV1 mapOffset mapFrame) ->
                pmatch constructorFrame $ \cf ->
                  pmatch mapFrame $ \mf ->
                    pand'List
                      [ constructorOffset #== 3
                      , pfromData (Scan.pframe'expectedChildren cf) #== 1
                      , mapOffset #== 1
                      , pfromData (Scan.pframe'expectedChildren mf) #== 2
                      ]

completedFramePops :: forall (s :: S). Term s PBool
completedFramePops =
  plet (pframe Scan.prootFrame 1 Data.pemptyDataListSummaryV1 (pconstant "")) $ \parent ->
    plet (pframe Scan.plistFrame 0 Data.pemptyDataListSummaryV1 (Scan.phashFrameV1 # parent)) $ \child ->
      plet (pexpectJust (Scan.pfinalizedSummaryV1 # child # pcon PNothing)) $ \summary ->
        plet (pexpectJust (Scan.pappendCompletedChildV1 # child # summary # parent)) $ \nextParent ->
          pexpectJust (Scan.pfinalizedSummaryV1 # nextParent # pcon (PJust summary)) #== summary

maximumBlobRoot :: forall (s :: S). Term s PBool
maximumBlobRoot =
  Scan.pboundedBlobRootV1 # pconstant (BS.replicate 9000 0)
    #== phexByteStr "69f84362d34689aee864cdf9e81bc71922e2d496c32bf8a81b72bcec0aec2db8"

largeBytes :: BS.ByteString
largeBytes = BS.replicate 8800 0

revealedLargeBytes :: forall (s :: S). Term s PBool
revealedLargeBytes =
  plet (pserialiseData # pconstant (PD.B largeBytes)) $ \raw ->
    plet (pexpectJust (Scan.prevealedLeafAtV1 # raw # (pblake2b_256 # raw) # 0 # (plengthBS # raw))) $ \leaf ->
      pmatch leaf $ \(Scan.PRawDataLeafV1 nextOffset summary) ->
        nextOffset #== plengthBS # raw
          #&& summary
          #== Data.pbytesDataSummaryV1
          # 8800
          # phexByteStr "def1a3dfbf8dfb6376d606cce8ee5eba4bc52f26dcd492343ed72517cb9e65d3"

directAndStreamed :: forall (s :: S). Term s PBool
directAndStreamed =
  plet (pconstant (PD.B largeBytes)) $ \d ->
    plet (pserialiseData # d) $ \raw ->
      pmatch (pexpectJust (Scan.prevealedLeafAtV1 # raw # (pblake2b_256 # raw) # 0 # (plengthBS # raw))) $ \(Scan.PRawDataLeafV1 _ summary) ->
        summary #== Data.psemanticDataSummaryV1 # d

largeConstructor :: forall (s :: S). Term s PBool
largeConstructor =
  plet (pconstant (PD.I 1)) $ \field ->
    plet (pconstant (PD.Constr 128 [PD.I 1])) $ \d ->
      plet (pserialiseData # d) $ \raw ->
        plet (pblake2b_256 # raw) $ \rawHash ->
          plet (pexpectJust (Scan.popenConstructorAtV1 # raw # rawHash # 0 # 128 # 1 # pconstant "")) $ \opened ->
            pmatch opened $ \(Scan.POpenedDataFrameV1 nextOffset frame) ->
              plet (pexpectJust (Scan.psmallLeafAtV1 # raw # rawHash # nextOffset # (pserialiseData # field))) $ \leaf ->
                pmatch leaf $ \(Scan.PRawDataLeafV1 leafNext summary) ->
                  plet (pexpectJust (Scan.pappendChildV1 # frame # summary)) $ \complete ->
                    plet (pexpectJust (Scan.pcloseSequenceAtV1 # raw # rawHash # leafNext # complete)) $ \afterBreak ->
                      plet (pexpectJust (Scan.pfoldListChildV1 # complete # 0 # summary # pcon PNil)) $ \folded ->
                        afterBreak #== plengthBS # raw
                          #&& pexpectJust (Scan.pfinalizedSummaryV1 # folded # pcon PNothing)
                          #== Data.psemanticDataSummaryV1 # d

nestedControl :: forall (s :: S). Term s PBool
nestedControl =
  plet (pconstant nestedData) $ \d ->
    plet (pserialiseData # d) $ \raw ->
      plet (pblake2b_256 # raw) $ \rawHash ->
        plet (Scan.pinitialControlV1 # rawHash # (plengthBS # raw)) $ \initial ->
          plet (pexpectJust (Scan.popenConstructorControlStepV1 # initial # raw # pcon PNothing # 0 # 2)) $ \afterRootOpen ->
            plet (pexpectJust (Scan.popenConstructorAtV1 # raw # rawHash # 0 # 0 # 2 # pconstant "")) $ \openedRoot ->
              pmatch openedRoot $ \(Scan.POpenedDataFrameV1 rootOffset rootFrame) ->
                plet (pexpectJust (Scan.popenListControlStepV1 # afterRootOpen # raw # pcon (PJust rootFrame) # 2)) $ \afterListOpen ->
                  plet (pexpectJust (Scan.popenListAtV1 # raw # rawHash # rootOffset # 2 # (Scan.phashFrameV1 # rootFrame))) $ \openedList ->
                    pmatch openedList $ \(Scan.POpenedDataFrameV1 _ listFrame) ->
                      plet (Data.psemanticDataSummaryV1 # pconstant (PD.I 1)) $ \firstSummary ->
                        plet (Data.psemanticDataSummaryV1 # pconstant (PD.I 2)) $ \secondSummary ->
                          plet (pexpectJust (Scan.prevealLeafControlStepV1 # afterListOpen # raw # pcon (PJust listFrame) # 1)) $ \afterFirst ->
                            plet (pexpectJust (Scan.pappendChildV1 # listFrame # firstSummary)) $ \listWithFirst ->
                              plet (pexpectJust (Scan.prevealLeafControlStepV1 # afterFirst # raw # pcon (PJust listWithFirst) # 1)) $ \afterSecond ->
                                plet (pexpectJust (Scan.pappendChildV1 # listWithFirst # secondSummary)) $ \listComplete ->
                                  plet (pexpectJust (Scan.pcloseSequenceControlStepV1 # afterSecond # raw # listComplete)) $ \afterListClose ->
                                    plet (pexpectJust (Scan.pfoldListControlStepV1 # afterListClose # listComplete # 1 # secondSummary # poneSibling (Scan.pchildLeafHashV1 # 0 # firstSummary))) $ \afterSecondFold ->
                                      plet (pexpectJust (Scan.pfoldListChildV1 # listComplete # 1 # secondSummary # poneSibling (Scan.pchildLeafHashV1 # 0 # firstSummary))) $ \listSecondFolded ->
                                        plet (pexpectJust (Scan.pfoldListControlStepV1 # afterSecondFold # listSecondFolded # 0 # firstSummary # poneSibling (Scan.pchildLeafHashV1 # 1 # secondSummary))) $ \afterFirstFold ->
                                          plet (pexpectJust (Scan.pfoldListChildV1 # listSecondFolded # 0 # firstSummary # poneSibling (Scan.pchildLeafHashV1 # 1 # secondSummary))) $ \listFolded ->
                                            plet (pexpectJust (Scan.pfinalizedSummaryV1 # listFolded # pcon PNothing)) $ \listSummary ->
                                              plet (pexpectJust (Scan.pfinalizeFrameControlStepV1 # afterFirstFold # listFolded # pcon (PJust rootFrame))) $ \afterListPop ->
                                                plet (pexpectJust (Scan.pappendCompletedChildV1 # listFolded # listSummary # rootFrame)) $ \rootWithList ->
                                                  pnestedMapHalf raw rawHash d afterListPop rootWithList listSummary

nestedData :: PD.Data
nestedData = PD.Constr 0 [PD.List [PD.I 1, PD.I 2], PD.Map [(PD.I 3, PD.I 4)]]

pnestedMapHalf ::
  forall (s :: S).
  Term s PByteString ->
  Term s PByteString ->
  Term s PData ->
  Term s Scan.PDataScanControlV1 ->
  Term s Scan.PDataScanFrameV1 ->
  Term s Data.PDataSummaryV1 ->
  Term s PBool
pnestedMapHalf raw rawHash d afterListPop rootWithList listSummary =
  plet (pexpectJust (Scan.popenMapControlStepV1 # afterListPop # raw # pcon (PJust rootWithList))) $ \afterMapOpen ->
    pmatch afterListPop $ \controlBeforeMap ->
      plet (pexpectJust (Scan.popenMapAtV1 # raw # rawHash # pfromData (Scan.pscan'offset controlBeforeMap) # (Scan.phashFrameV1 # rootWithList))) $ \openedMap ->
        pmatch openedMap $ \(Scan.POpenedDataFrameV1 _ mapFrame) ->
          plet (Data.psemanticDataSummaryV1 # pconstant (PD.I 3)) $ \keySummary ->
            plet (Data.psemanticDataSummaryV1 # pconstant (PD.I 4)) $ \valueSummary ->
              plet (pexpectJust (Scan.prevealLeafControlStepV1 # afterMapOpen # raw # pcon (PJust mapFrame) # 1)) $ \afterKey ->
                plet (pexpectJust (Scan.pappendChildV1 # mapFrame # keySummary)) $ \mapWithKey ->
                  plet (pexpectJust (Scan.prevealLeafControlStepV1 # afterKey # raw # pcon (PJust mapWithKey) # 1)) $ \afterValue ->
                    plet (pexpectJust (Scan.pappendChildV1 # mapWithKey # valueSummary)) $ \mapComplete ->
                      plet (pexpectJust (Scan.pfoldMapControlStepV1 # afterValue # mapComplete # 0 # keySummary # valueSummary # poneSibling (Scan.pchildLeafHashV1 # 1 # valueSummary) # poneSibling (Scan.pchildLeafHashV1 # 0 # keySummary))) $ \afterPairFold ->
                        plet (pexpectJust (Scan.pfoldMapPairV1 # mapComplete # 0 # keySummary # valueSummary # poneSibling (Scan.pchildLeafHashV1 # 1 # valueSummary) # poneSibling (Scan.pchildLeafHashV1 # 0 # keySummary))) $ \mapFolded ->
                          plet (pexpectJust (Scan.pfinalizedSummaryV1 # mapFolded # pcon PNothing)) $ \mapSummary ->
                            plet (pexpectJust (Scan.pfinalizeFrameControlStepV1 # afterPairFold # mapFolded # pcon (PJust rootWithList))) $ \afterMapPop ->
                              plet (pexpectJust (Scan.pappendCompletedChildV1 # mapFolded # mapSummary # rootWithList)) $ \rootComplete ->
                                plet (pexpectJust (Scan.pcloseSequenceControlStepV1 # afterMapPop # raw # rootComplete)) $ \afterRootClose ->
                                  plet (pexpectJust (Scan.pfoldListControlStepV1 # afterRootClose # rootComplete # 1 # mapSummary # poneSibling (Scan.pchildLeafHashV1 # 0 # listSummary))) $ \afterMapFold ->
                                    plet (pexpectJust (Scan.pfoldListChildV1 # rootComplete # 1 # mapSummary # poneSibling (Scan.pchildLeafHashV1 # 0 # listSummary))) $ \rootMapFolded ->
                                      plet (pexpectJust (Scan.pfoldListControlStepV1 # afterMapFold # rootMapFolded # 0 # listSummary # poneSibling (Scan.pchildLeafHashV1 # 1 # mapSummary))) $ \afterListFold ->
                                        plet (pexpectJust (Scan.pfoldListChildV1 # rootMapFolded # 0 # listSummary # poneSibling (Scan.pchildLeafHashV1 # 1 # mapSummary))) $ \rootFolded ->
                                          plet (pexpectJust (Scan.pfinalizeFrameControlStepV1 # afterListFold # rootFolded # pcon PNothing)) $ \completed ->
                                            pmatch completed $ \c ->
                                              pand'List
                                                [ pfromData (Scan.pscan'offset c) #== plengthBS # raw
                                                , pfromData (Scan.pscan'frameRoot c) #== pconstant ""
                                                , pnot # pfromData (Scan.pscan'frameClosed c)
                                                , pfromData (Scan.pscan'result c) #== Data.psemanticDataSummaryV1 # d
                                                ]

controlHashVectors :: forall (s :: S). Term s PBool
controlHashVectors =
  plet (phexByteStr "d8799f9f0102ffa10342abcdff") $ \raw ->
    plet (Scan.pinitialControlV1 # (pblake2b_256 # raw) # (plengthBS # raw)) $ \initial ->
      pmatch initial $ \c ->
        plet
          ( pcon
              c
                { Scan.pscan'offset = pdata (plengthBS # raw)
                , Scan.pscan'result =
                    pdata $
                      Data.psemanticDataSummaryV1
                        # pconstant
                          ( PD.Constr
                              0
                              [ PD.List [PD.I 1, PD.I 2]
                              , PD.Map [(PD.I 3, PD.B "\xab\xcd")]
                              ]
                          )
                }
          )
          $ \terminal ->
            pand'List
              [ Scan.phashControlV1 # initial
                  #== phexByteStr "6b5258f74c54a3932194e3087c5ce5652fb1bbffb042b71572aab47bea7d07e4"
              , Scan.phashControlV1 # terminal
                  #== phexByteStr "865b9c51d15222a3b33dbb422e212aa302d37e4a791268c0f22a8ff6b0a538fa"
              ]
