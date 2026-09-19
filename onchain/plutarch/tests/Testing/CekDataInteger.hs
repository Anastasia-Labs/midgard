{-# LANGUAGE OverloadedStrings #-}

module Testing.CekDataInteger (tests) where

import Data.Kind (Type)
import Plutarch.Core.Utils (pand'List)
import Plutarch.Prelude
import Test.Tasty
import Test.Tasty.HUnit

import Midgard.CekData (PDataSummaryV1 (..))
import Midgard.CekDataInteger (
  PCekDataIntegerControlV1 (..),
  pdecodeControlV1,
  pencodeControlV1,
  pfinalizeV1,
  pinitialControlV1,
  pnextSourceSpanV1,
  pparseLargeConstructorSyntaxV1,
  pparseSyntaxV1,
  pstageTerminal,
  pstepV1,
 )
import Midgard.CekSourceBlob (PCekSourceBlobSpanV1 (..))
import Midgard.FraudProofs.NativeTx.Codec (psliceLen)
import Testing.Eval (passertEval)

tests :: TestTree
tests =
  testGroup
    "Midgard.CekDataInteger"
    [ integerV1ParityTests
    ]

integerV1ParityTests :: TestTree
integerV1ParityTests =
  testGroup
    "cek-data-integer-v1 Aiken parity"
    [ testCase "proves_a_canonical_negative_bignum_without_materializing_it" $
        passertEval provesNegativeBignum
    , testCase "decodes_the_typescript_terminal_control" $
        passertEval decodesTypescriptControl
    , testCase "canonical_signed_boundaries_have_exact_cek_memory" $
        passertEval canonicalBoundaries
    , testCase "accepts_only_canonical_constructor_alternatives_above_127" $
        passertEval largeConstructorSyntax
    , testCase "rejects_malformed_and_noncanonical_integer_syntax" $
        passertEval malformedSyntax
    , testCase "rejects_missing_short_and_surplus_authenticated_syntax_windows" $
        passertEval malformedWindows
    ]

source :: forall (s :: S). Term s PByteString
source = phexByteStr "c349010000000000000000"

typescriptTerminalControlCbor :: forall (s :: S). Term s PByteString
typescriptTerminalControlCbor =
  phexByteStr
    "860102110b0dd8799f860101110b8401010b8183005820529618b73f1e990ed364ce58c08a76518a3f4ddaf2397ea92207a760422764840bd87a80ff"

summaryMatches :: forall (s :: S). Term s (PMaybe PDataSummaryV1 :--> PBool)
summaryMatches = plam $ \result ->
  pmatch result $ \case
    PNothing -> pconstant False
    PJust summary ->
      pmatch summary $ \s ->
        pand'List
          [ pfromData (psummary'root s)
              #== phexByteStr "720c28eb8291c0e25d860108458a13027f509d93b9c61296532fdb230063c691"
          , pfromData (psummary'cborLength s) #== 11
          , pfromData (psummary'memory s) #== 13
          ]

provesNegativeBignum :: forall (s :: S). Term s PBool
provesNegativeBignum =
  plet (pfinishTrace # (pinitialControlV1 # 17 # 11) # source # 17) $ \terminal ->
    (summaryMatches # (pfinalizeV1 # terminal))
      #&& (pencodeControlV1 # terminal #== typescriptTerminalControlCbor)

decodesTypescriptControl :: forall (s :: S). Term s PBool
decodesTypescriptControl =
  plet (pdecodeControlV1 # typescriptTerminalControlCbor) $ \control ->
    pmatch control $ \c ->
      pand'List
        [ pfromData (pint'stage c) #== pstageTerminal
        , pfromData (pint'sourceStart c) #== 17
        , pfromData (pint'sourceLength c) #== 11
        , pfromData (pint'memory c) #== 13
        , summaryMatches # (pfinalizeV1 # control)
        , pencodeControlV1 # control #== typescriptTerminalControlCbor
        ]

canonicalBoundaries :: forall (s :: S). Term s PBool
canonicalBoundaries =
  pand'List
    [ isJustWith 5 (pparseSyntaxV1 # phexByteStr "00" # 1)
    , isJustWith 5 (pparseSyntaxV1 # phexByteStr "20" # 1)
    , isJustWith 13 (pparseSyntaxV1 # phexByteStr "1bffffffffffffffff" # 9)
    , isJustWith 13 (pparseSyntaxV1 # phexByteStr "3bffffffffffffffff" # 9)
    , isJustWith 13 (pparseSyntaxV1 # phexByteStr "c249010000000000000000" # 11)
    , isJustWith 13 (pparseSyntaxV1 # phexByteStr "c349010000000000000000" # 11)
    ]

largeConstructorSyntax :: forall (s :: S). Term s PBool
largeConstructorSyntax =
  pand'List
    [ isJust (pparseLargeConstructorSyntaxV1 # phexByteStr "1880" # 2)
    , isJust (pparseLargeConstructorSyntaxV1 # phexByteStr "1bffffffffffffffff" # 9)
    , isJust (pparseLargeConstructorSyntaxV1 # phexByteStr "c249010000000000000000" # 11)
    , isNothing (pparseLargeConstructorSyntaxV1 # phexByteStr "1817" # 2)
    , isNothing (pparseLargeConstructorSyntaxV1 # phexByteStr "187f" # 2)
    , isNothing (pparseLargeConstructorSyntaxV1 # phexByteStr "3880" # 2)
    , isNothing (pparseLargeConstructorSyntaxV1 # phexByteStr "c349010000000000000000" # 11)
    ]

malformedSyntax :: forall (s :: S). Term s PBool
malformedSyntax =
  pand'List
    [ isNothing (pparseSyntaxV1 # phexByteStr "1817" # 2)
    , isNothing (pparseSyntaxV1 # phexByteStr "c248ffffffffffffffff" # 10)
    , isNothing (pparseSyntaxV1 # phexByteStr "c249000100000000000000" # 11)
    , isNothing (pparseSyntaxV1 # phexByteStr "c25809010000000000000000" # 12)
    , isNothing (pparseSyntaxV1 # phexByteStr "c25f490100000000000000ff" # 12)
    , isNothing (pparseSyntaxV1 # phexByteStr "40" # 1)
    ]

malformedWindows :: forall (s :: S). Term s PBool
malformedWindows =
  plet (pinitialControlV1 # 9 # 11) $ \initial ->
    pand'List
      [ isNothing (pstepV1 # initial # pcon PNothing)
      , isNothing (pstepV1 # initial # pcon (PJust (phexByteStr "0000000000")))
      , isNothing (pstepV1 # initial # pcon (PJust (phexByteStr "000000000000")))
      ]

isNothing :: forall (s :: S) (a :: S -> Type). Term s (PMaybe a) -> Term s PBool
isNothing value = pmatch value $ \case PNothing -> pconstant True; PJust _ -> pconstant False

isJust :: forall (s :: S) (a :: S -> Type). Term s (PMaybe a) -> Term s PBool
isJust value = pmatch value $ \case PNothing -> pconstant False; PJust _ -> pconstant True

isJustWith :: forall (s :: S). Term s PInteger -> Term s (PMaybe PInteger) -> Term s PBool
isJustWith expected value = pmatch value $ \case PNothing -> pconstant False; PJust actual -> actual #== expected

pfinishTrace ::
  forall (s :: S).
  Term
    s
    ( PCekDataIntegerControlV1
        :--> PByteString
        :--> PInteger
        :--> PCekDataIntegerControlV1
    )
pfinishTrace =
  pfix $ \self -> plam $ \control wholeSource sourceStart ->
    pmatch control $ \c ->
      pif
        (pfromData (pint'stage c) #== pstageTerminal)
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
