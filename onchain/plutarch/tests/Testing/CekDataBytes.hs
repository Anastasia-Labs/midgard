{-# LANGUAGE OverloadedStrings #-}

module Testing.CekDataBytes (tests) where

import Data.ByteString qualified as BS
import Data.Kind (Type)
import Plutarch.Core.Utils (pand'List)
import Plutarch.Prelude
import Test.Tasty
import Test.Tasty.HUnit

import Midgard.CekData (PDataSummaryV1 (..))
import Midgard.CekDataBytes (
  PCekDataBytesControlV1 (..),
  pdecodeControlV1,
  pencodeControlV1,
  pfinalizeV1,
  pinitialControlV1,
  pnextSourceSpanV1,
  pparseSyntaxV1,
  pstageBreak,
  pstageTerminal,
  pstepV1,
 )
import Midgard.CekSourceBlob (PCekSourceBlobSpanV1 (..))
import Midgard.FraudProofs.NativeTx.Codec (psliceLen)
import Testing.Eval (passertEval)

tests :: TestTree
tests =
  testGroup
    "Midgard.CekDataBytes"
    [ bytesV1ParityTests
    ]

bytesV1ParityTests :: TestTree
bytesV1ParityTests =
  testGroup
    "cek-data-bytes-v1 Aiken parity"
    [ testCase "proves_canonical_chunked_bytes_in_raw_and_virtual_coordinates" $
        passertEval provesChunkedBytes
    , testCase "decodes_the_typescript_terminal_control" $
        passertEval decodesTypescriptControl
    , testCase "canonical_cardano_framing_boundaries_derive_exact_content_lengths" $
        passertEval framingBoundaries
    , testCase "rejects_noncanonical_and_impossible_framing_lengths" $
        passertEval impossibleFraming
    , testCase "rejects_a_malformed_authenticated_chunk_header" $
        passertEval malformedChunkHeader
    , testCase "rejects_a_missing_or_wrong_indefinite_break" $
        passertEval wrongBreak
    ]

source65Bytes :: BS.ByteString
source65Bytes = BS.pack [0x5f, 0x58, 0x40] <> BS.replicate 64 106 <> BS.pack [0x41, 0x6a, 0xff]

source65 :: forall (s :: S). Term s PByteString
source65 = pconstant source65Bytes

typescriptTerminalControlCbor :: forall (s :: S). Term s PByteString
typescriptTerminalControlCbor =
  phexByteStr
    "8601031118461841d8799f86010100184184010118418183005820ebc3448dad1500c73547d17bd6e9e93387c20bc9422b3313e45b434bf26a967c1841d87a80ff"

summaryMatches :: forall (s :: S). Term s (PMaybe PDataSummaryV1 :--> PBool)
summaryMatches = plam $ \result ->
  pmatch result $ \case
    PNothing -> pconstant False
    PJust summary ->
      pmatch summary $ \s ->
        pand'List
          [ pfromData (psummary'root s)
              #== phexByteStr "e36bf656c2ebd1bb060fa2a5b3c8d515c595d7ddac821eab2ab29d5e97b29836"
          , pfromData (psummary'cborLength s) #== 70
          , pfromData (psummary'memory s) #== 69
          ]

provesChunkedBytes :: forall (s :: S). Term s PBool
provesChunkedBytes =
  plet (pfinishTrace # (pinitialControlV1 # 17 # 70) # source65 # 17) $ \terminal ->
    (summaryMatches # (pfinalizeV1 # terminal))
      #&& (pencodeControlV1 # terminal #== typescriptTerminalControlCbor)

decodesTypescriptControl :: forall (s :: S). Term s PBool
decodesTypescriptControl =
  plet (pdecodeControlV1 # typescriptTerminalControlCbor) $ \control ->
    pmatch control $ \c ->
      pand'List
        [ pfromData (pbytes'stage c) #== pstageTerminal
        , pfromData (pbytes'sourceStart c) #== 17
        , pfromData (pbytes'sourceLength c) #== 70
        , pfromData (pbytes'bytesLength c) #== 65
        , summaryMatches # (pfinalizeV1 # control)
        , pencodeControlV1 # control #== typescriptTerminalControlCbor
        ]

framingBoundaries :: forall (s :: S). Term s PBool
framingBoundaries =
  pand'List
    [ isJustWith 0 (pparseSyntaxV1 # phexByteStr "40" # 1)
    , isJustWith 1 (pparseSyntaxV1 # phexByteStr "416a" # 2)
    , isJustWith 23 (pparseSyntaxV1 # phexByteStr "576a" # 24)
    , isJustWith 24 (pparseSyntaxV1 # phexByteStr "5818" # 26)
    , isJustWith 63 (pparseSyntaxV1 # phexByteStr "583f" # 65)
    , isJustWith 64 (pparseSyntaxV1 # phexByteStr "5840" # 66)
    , isJustWith 65 (pparseSyntaxV1 # phexByteStr "5f58" # 70)
    , isJustWith 128 (pparseSyntaxV1 # phexByteStr "5f58" # 134)
    , isJustWith 129 (pparseSyntaxV1 # phexByteStr "5f58" # 136)
    ]

impossibleFraming :: forall (s :: S). Term s PBool
impossibleFraming =
  pand'List
    [ isNothing (pparseSyntaxV1 # phexByteStr "5817" # 25)
    , isNothing (pparseSyntaxV1 # phexByteStr "5841" # 67)
    , isNothing (pparseSyntaxV1 # phexByteStr "5f58" # 68)
    , isNothing (pparseSyntaxV1 # phexByteStr "5f58" # 69)
    , isNothing (pparseSyntaxV1 # phexByteStr "5f58" # 93)
    , isNothing (pparseSyntaxV1 # phexByteStr "596a" # 66)
    ]

malformedChunkHeader :: forall (s :: S). Term s PBool
malformedChunkHeader =
  plet (pinitialControlV1 # 17 # 70) $ \initial ->
    pmatch (pstepV1 # initial # pcon (PJust (psliceLen # source65 # 0 # 2))) $ \case
      PNothing -> perror
      PJust blobControl ->
        pmatch (pnextSourceSpanV1 # blobControl) $ \case
          PNothing -> perror
          PJust span ->
            pmatch span $ \(PCekSourceBlobSpanV1 absoluteStart spanLength) ->
              plet
                (psliceLen # source65 # (pfromData absoluteStart - 17) # pfromData spanLength)
                $ \sourceBytes ->
                  plet
                    (pconstant "\x00" <> psliceLen # sourceBytes # 1 # (plengthBS # sourceBytes - 1))
                    $ \malformed -> isNothing (pstepV1 # blobControl # pcon (PJust malformed))

wrongBreak :: forall (s :: S). Term s PBool
wrongBreak =
  plet (pfinishToBreak # (pinitialControlV1 # 17 # 70) # source65 # 17) $ \breakControl ->
    isNothing (pstepV1 # breakControl # pcon PNothing)
      #&& isNothing (pstepV1 # breakControl # pcon (PJust (pconstant "\x00")))

isNothing :: forall (s :: S) (a :: S -> Type). Term s (PMaybe a) -> Term s PBool
isNothing value = pmatch value $ \case PNothing -> pconstant True; PJust _ -> pconstant False

isJustWith :: forall (s :: S). Term s PInteger -> Term s (PMaybe PInteger) -> Term s PBool
isJustWith expected value = pmatch value $ \case PNothing -> pconstant False; PJust actual -> actual #== expected

pfinishTrace ::
  forall (s :: S).
  Term
    s
    ( PCekDataBytesControlV1
        :--> PByteString
        :--> PInteger
        :--> PCekDataBytesControlV1
    )
pfinishTrace = pfinishUntil pstageTerminal

pfinishToBreak ::
  forall (s :: S).
  Term
    s
    ( PCekDataBytesControlV1
        :--> PByteString
        :--> PInteger
        :--> PCekDataBytesControlV1
    )
pfinishToBreak = pfinishUntil pstageBreak

pfinishUntil ::
  forall (s :: S).
  Term s PInteger ->
  Term
    s
    ( PCekDataBytesControlV1
        :--> PByteString
        :--> PInteger
        :--> PCekDataBytesControlV1
    )
pfinishUntil target =
  pfix $ \self -> plam $ \control wholeSource sourceStart ->
    pmatch control $ \c ->
      pif
        (pfromData (pbytes'stage c) #== target)
        control
        $ pmatch (pnextSourceSpanV1 # control)
        $ \case
          PNothing ->
            pmatch (pstepV1 # control # pcon PNothing) $ \case
              PNothing -> perror
              PJust next -> self # next # wholeSource # sourceStart
          PJust span ->
            pmatch span $ \(PCekSourceBlobSpanV1 absoluteStart spanLength) ->
              plet
                ( psliceLen
                    # wholeSource
                    # (pfromData absoluteStart - sourceStart)
                    # pfromData spanLength
                )
                $ \sourceBytes ->
                  pmatch (pstepV1 # control # pcon (PJust sourceBytes)) $ \case
                    PNothing -> perror
                    PJust next -> self # next # wholeSource # sourceStart
