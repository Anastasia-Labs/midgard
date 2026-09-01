{-# LANGUAGE OverloadedStrings #-}

module Testing.NativeTxScriptPushdown (tests) where

import Data.Bits (shiftR)
import Data.ByteString qualified as BS
import Data.Word (Word8)
import Plutarch.Core.Utils ((#/=))
import Plutarch.Prelude
import Test.Tasty hiding (after)
import Test.Tasty.HUnit

import Midgard.NativeScript (
  PNativeScriptProofV1 (..),
  pcheckNativeScriptV1,
  pinspectNativeScriptV1,
 )
import Midgard.NativeTxScriptPushdown
import Testing.Eval (passertEvalNoTrace, pfails)

tests :: TestTree
tests = testGroup "Midgard.NativeTxScriptPushdown"
  [ testGroup "traversal"
      [ verdictCase "nested script" nestedScript True
      , verdictCase "other script" otherScript False
      , verdictCase "matching signature" (signature key1) True
      , verdictCase "missing signature" (signature key2) False
      , verdictCase "after" (after 100) True
      , verdictCase "before satisfied" (before 400) True
      , verdictCase "before unsatisfied" (before 100) False
      , verdictCase "all of empty" (allOf []) True
      , verdictCase "any of empty" (anyOf []) False
      , verdictCase "zero of empty" (atLeast 0 []) True
      , testCase "threshold above node bound is adjudicated" $ passertEvalNoTrace cappedThresholds
      , testCase "deepest admissible script traverses" $ passertEvalNoTrace deepestAdmissible
      , testCase "reads every node exactly once" $ passertEvalNoTrace readsEachNodeOnce
      , testCase "nested fixture takes exactly fifteen steps" $ passertEvalNoTrace exactStepCount
      , testCase "unfinished traversal has no verdict" $ passertEvalNoTrace unfinishedHasNoVerdict
      ]
  , testGroup "recursive checker equivalence"
      [ agreementCase "nested" nestedScript
      , agreementCase "other" otherScript
      , agreementCase "matching signature" (signature key1)
      , agreementCase "missing signature" (signature key2)
      , agreementCase "after" (after 100)
      , agreementCase "before" (before 400)
      , agreementCase "empty all" (allOf [])
      , agreementCase "empty any" (anyOf [])
      , agreementCase "empty zero-of" (atLeast 0 [])
      , agreementCase "depth 1" (nestedAlls 0 $ signature key1)
      , agreementCase "depth 2" (nestedAlls 1 $ signature key1)
      , agreementCase "depth 15" (nestedAlls 14 $ signature key1)
      , agreementCase "depth 16 true" (nestedAlls 15 $ signature key1)
      , agreementCase "depth 16 false" (nestedAlls 15 $ signature key2)
      , agreementCase "wide all at node bound" (allOf $ replicate 15 $ signature key1)
      , agreementCase "wide any at node bound" (anyOf $ replicate 15 $ signature key2)
      , agreementCase "two of three" (atLeast 2 $ replicate 3 $ signature key1)
      , agreementCase "one of one" (atLeast 1 [signature key1])
      , agreementCase "two of one" (atLeast 2 [signature key1])
      , agreementCase "four of three" (atLeast 4 $ replicate 3 $ signature key1)
      , agreementCase "threshold 32" (atLeast 32 [signature key1])
      , agreementCase "threshold 33" (atLeast 33 [signature key1])
      , agreementCase "threshold 33 three children" (atLeast 33 $ replicate 3 $ signature key1)
      , agreementCase "threshold uint16" (atLeast 0xffff [signature key1])
      , agreementCase "threshold uint64" (atLeast 0xffffffffffffffff [signature key1])
      , agreementCase "empty threshold one" (atLeast 1 [])
      , agreementCase "empty threshold 33" (atLeast 33 [])
      , testCase "depth boundary fixtures match the definition" $ passertEvalNoTrace depthBoundaryFixtures
      , testCase "threshold-above-bound fixture is within definition bounds" $ passertEvalNoTrace thresholdDefinition
      , testCase "bushy fixture exceeds only the node bound" $ passertEvalNoTrace bushyDefinitionCheck
      , testCase "recursive checker budget row" $ passertEvalNoTrace $ precursiveVerdict nestedScript True
      ]
  , testGroup "refusals"
      [ testCase "depth 17" $ pfails $ pfailRun (nestedAlls 16 $ signature key1) 64
      , testCase "payload substitution" $ pfails payloadSubstitution
      , testCase "negative budget" $ pfails $ pfailRun nestedScript (-1)
      , testCase "trailing bytes" $ pfails trailingBytesVerdict
      , testCase "wide node" $ pfails $ pfailRun wideScript 128
      , testCase "bushy node overflow" $ pfails $ pfailRun bushyScript 256
      , testCase "empty payload" $ pfails popenEmpty
      , testCase "wrong arity" $ pfails $ pfailRun wrongArityScript 64
      , testCase "wrong signature key width" $ pfails $ pfailRun wrongKeyWidthScript 64
      , testCase "unknown tag" $ pfails $ pfailRun (hexBytes [0x82, 0x06, 0x80]) 64
      ]
  , testGroup "checkpoint"
      [ testCase "resume carries a capped threshold" $ passertEvalNoTrace resumeCarriesCap
      , testCase "resume finishes with exactly the remaining steps" $ passertEvalNoTrace resumeFinishes
      , testCase "resume preserves every interruption verdict" $ passertEvalNoTrace resumeEveryBudget
      , testCase "cursor wire form has constant length" $ passertEvalNoTrace cursorConstantLength
      , testCase "cursor wire form declares seven CBOR byte strings" $ passertEvalNoTrace cursorCborShape
      , testCase "checkpoint and resume budget row" $ passertEvalNoTrace checkpointBudgetRow
      ]
  , testGroup "checkpoint refusals"
      [ testCase "uncommitted position" $ pfails resumeWrongCommitment
      , testCase "substituted frame" $ pfails resumeForgedFrame
      , testCase "shortened stack" $ pfails resumeShortenedStack
      , testCase "different script" $ pfails $ resumeWrongPayload otherScript
      , testCase "same-length different script" $ pfails $ resumeWrongPayload twinScript
      ]
  ]

verdictCase :: String -> BS.ByteString -> Bool -> TestTree
verdictCase name script expected = testCase name $ passertEvalNoTrace $
  pverdictIs script expected

agreementCase :: String -> BS.ByteString -> TestTree
agreementCase name script = testCase name $ passertEvalNoTrace $ pagreesWithRecursive script

pcontext :: forall s. Term s PNativeScriptContextV1
pcontext = pcon $ PNativeScriptContextV1
  (pdata $ pcons # pdata (pconstant key1) # pnil)
  (pdata 200)
  (pdata 300)

prun :: forall s. BS.ByteString -> Integer -> Term s PNativeScriptWalkV1
prun script budget =
  pnativeScriptRun
    # (popenNativeScriptWalk # pconstant script)
    # pconstant script
    # pcontext
    # pconstant budget

pverdictIs :: forall s. BS.ByteString -> Bool -> Term s PBool
pverdictIs script expected = pmatch (pnativeScriptVerdict # prun script 64) $ \case
  PNothing -> pconstant False
  PJust verdict -> verdict #== pconstant expected

pagreesWithRecursive :: forall s. BS.ByteString -> Term s PBool
pagreesWithRecursive script = pmatch
  (pcheckNativeScriptV1 # pconstant script # 200 # 300 # (pcons # pconstant key1 # pnil)) $ \case
    PNothing -> pconstant False
    PJust proof -> pmatch proof $ \checked ->
      pmatch (pnativeScriptVerdict # prun script 64) $ \case
        PNothing -> pconstant False
        PJust verdict -> verdict #== pfromData (pnativeProof'valid checked)

precursiveVerdict :: forall s. BS.ByteString -> Bool -> Term s PBool
precursiveVerdict script expected = pmatch
  (pcheckNativeScriptV1 # pconstant script # 200 # 300 # (pcons # pconstant key1 # pnil)) $ \case
    PNothing -> pconstant False
    PJust proof -> pmatch proof $ \checked ->
      pfromData (pnativeProof'valid checked) #== pconstant expected

pfailRun :: forall s. BS.ByteString -> Integer -> Term s PBool
pfailRun script budget = plet (prun script budget) $ \_ -> pconstant True

popenEmpty :: forall s. Term s PBool
popenEmpty = plet (popenNativeScriptWalk # pconstant ("" :: BS.ByteString)) $ \_ -> pconstant True

cappedThresholds :: forall s. Term s PBool
cappedThresholds =
  pverdictIs (atLeast 33 [signature key1]) False
    #&& pverdictIs (atLeast 33 $ replicate 3 $ signature key1) False
    #&& pverdictIs (atLeast 2 [signature key1]) False
    #&& pverdictIs (atLeast 1 [signature key1]) True
    #&& pverdictIs (atLeast 65_535 [signature key1]) False
    #&& pverdictIs (atLeast 0xffffffffffffffff [signature key1]) False

deepestAdmissible :: forall s. Term s PBool
deepestAdmissible =
  plet (prun (nestedAlls 15 $ signature key1) 64) $ \walk ->
    pmatch (pnativeScriptVerdict # walk) $ \case
      PNothing -> pconstant False
      PJust verdict -> verdict
        #&& pnativeScriptNodesVisited # walk #== 16
        #&& pverdictIs (nestedAlls 15 $ signature key2) False

readsEachNodeOnce :: forall s. Term s PBool
readsEachNodeOnce = plet (prun nestedScript 64) $ \walk ->
  pnativeScriptWalkIsComplete # walk
    #&& pnativeScriptNodesVisited # walk #== 8

exactStepCount :: forall s. Term s PBool
exactStepCount =
  pnativeScriptWalkIsComplete # prun nestedScript 15
    #&& pnot # (pnativeScriptWalkIsComplete # prun nestedScript 14)

unfinishedHasNoVerdict :: forall s. Term s PBool
unfinishedHasNoVerdict = plet (prun nestedScript 3) $ \walk ->
  pnot # (pnativeScriptWalkIsComplete # walk)
    #&& pmatch (pnativeScriptVerdict # walk) (\case PNothing -> pconstant True; PJust _ -> pconstant False)

payloadSubstitution :: forall s. Term s PBool
payloadSubstitution =
  plet (popenNativeScriptWalk # pconstant nestedScript) $ \walk ->
    plet (pnativeScriptRun # walk # pconstant otherScript # pcontext # 64) $ \_ -> pconstant True

trailingBytesVerdict :: forall s. Term s PBool
trailingBytesVerdict =
  plet (prun (signature key1 <> BS.singleton 0) 64) $ \walk ->
    plet (pnativeScriptVerdict # walk) $ \_ -> pconstant True

pstopped :: forall s. BS.ByteString -> Integer -> Term s PNativeScriptWalkV1
pstopped script budget = prun script budget

presume :: forall s. Term s PNativeScriptWalkV1 -> BS.ByteString -> Term s PNativeScriptWalkV1
presume stopped script =
  presumeNativeScriptWalkFromCommitment
    # (pnativeScriptCursorHash # stopped)
    # (pencodeNativeScriptCursor # stopped)
    # (pnativeScriptWalkFrames # stopped)
    # pconstant script

resumeCarriesCap :: forall s. Term s PBool
resumeCarriesCap =
  let script = atLeast 0xffffffffffffffff [signature key1, signature key2, signature key3]
   in plet (pstopped script 2) $ \stopped ->
        pmatch (pnativeScriptWalkFrames # stopped) $ \case
          PNil -> pconstant False
          PCons topData _ -> pmatch (pfromData topData) $ \top ->
            pfromData (pnativeFrame'required top) #== 33
              #&& plet (presume stopped script) (\resumed ->
                pmatch (pnativeScriptVerdict #$
                  pnativeScriptRun # resumed # pconstant script # pcontext # 64) $ \case
                    PNothing -> pconstant False
                    PJust verdict -> pnot # verdict)

resumeFinishes :: forall s. Term s PBool
resumeFinishes = plet (pstopped nestedScript 5) $ \stopped ->
  plet (presume stopped nestedScript) $ \resumed ->
    plet (pnativeScriptRun # resumed # pconstant nestedScript # pcontext # 10) $ \finished ->
      plet (pnativeScriptRun # resumed # pconstant nestedScript # pcontext # 9) $ \starved ->
        pmatch (pnativeScriptVerdict # finished) $ \case
          PNothing -> pconstant False
          PJust verdict -> verdict
            #&& plengthBS # (pencodeNativeScriptCursor # stopped) #== pnativeScriptCursorBytes
            #&& pnativeScriptNodesVisited # stopped #< 8
            #&& pnativeScriptNodesVisited # finished #== 8
            #&& pnot # (pnativeScriptWalkIsComplete # starved)

resumeEveryBudget :: forall s. Term s PBool
resumeEveryBudget = pfix (\self -> plam $ \budget ->
  pif (budget #> 12) (pconstant True) $
    plet (pnativeScriptRun
      # (popenNativeScriptWalk # pconstant nestedScript)
      # pconstant nestedScript # pcontext # budget) $ \stopped ->
        pif (pnativeScriptWalkIsComplete # stopped)
          (pmatch (pnativeScriptVerdict # stopped) $ \case
            PNothing -> pconstant False
            PJust verdict -> verdict #&& self # (budget + 1))
          (plet (presume stopped nestedScript) $ \resumed ->
            pmatch (pnativeScriptVerdict #$
              pnativeScriptRun # resumed # pconstant nestedScript # pcontext # 64) $ \case
                PNothing -> pconstant False
                PJust verdict -> verdict #&& self # (budget + 1))) # 1

cursorConstantLength :: forall s. Term s PBool
cursorConstantLength =
  plet (pencodeNativeScriptCursor # pstopped nestedScript 4) $ \first ->
    plet (pencodeNativeScriptCursor # pstopped otherScript 4) $ \second ->
      plengthBS # first #== pnativeScriptCursorBytes
        #&& plengthBS # second #== pnativeScriptCursorBytes
        #&& plengthBS # first #== plengthBS # second
        #&& first #/= second

-- The Aiken test decodes this with its generic CBOR decoder.  Pinning the
-- complete fixed-width layout here establishes the same property: array(7),
-- two bytes(32), four bytes(3), and bytes(1), with no trailing byte.
cursorCborShape :: forall s. Term s PBool
cursorCborShape = plet (pencodeNativeScriptCursor # pstopped nestedScript 5) $ \cursor ->
  plengthBS # cursor #== 87
    #&& psliceBS # 0 # 3 # cursor #== pconstant (hexBytes [0x87, 0x58, 0x20])
    #&& psliceBS # 35 # 2 # cursor #== pconstant (hexBytes [0x58, 0x20])
    #&& psliceBS # 69 # 1 # cursor #== pconstant (hexBytes [0x43])
    #&& psliceBS # 73 # 1 # cursor #== pconstant (hexBytes [0x43])
    #&& psliceBS # 77 # 1 # cursor #== pconstant (hexBytes [0x43])
    #&& psliceBS # 81 # 1 # cursor #== pconstant (hexBytes [0x43])
    #&& psliceBS # 85 # 1 # cursor #== pconstant (hexBytes [0x41])

checkpointBudgetRow :: forall s. Term s PBool
checkpointBudgetRow = plet (pstopped nestedScript 5) $ \stopped ->
  plet (presume stopped nestedScript) $ \resumed ->
    pmatch (pnativeScriptVerdict #$
      pnativeScriptRun # resumed # pconstant nestedScript # pcontext # 64) $ \case
        PNothing -> pconstant False
        PJust verdict -> verdict

resumeWrongCommitment :: forall s. Term s PBool
resumeWrongCommitment =
  plet (pstopped nestedScript 5) $ \stopped ->
    plet (pstopped nestedScript 6) $ \further ->
      plet
        (presumeNativeScriptWalkFromCommitment
          # (pnativeScriptCursorHash # further)
          # (pencodeNativeScriptCursor # stopped)
          # (pnativeScriptWalkFrames # stopped)
          # pconstant nestedScript)
        $ \_ -> pconstant True

resumeForgedFrame :: forall s. Term s PBool
resumeForgedFrame = plet (pstopped nestedScript 5) $ \stopped ->
  pmatch (pnativeScriptWalkFrames # stopped) $ \case
    PNil -> pconstant False
    PCons topData rest -> pmatch (pfromData topData) $ \top ->
      plet
        (pcons
          # pdata (pcon $ PNativeScriptFrameV1
            (pnativeFrame'kind top)
            (pnativeFrame'remaining top)
            (pdata $ pfromData (pnativeFrame'satisfied top) + 1)
            (pnativeFrame'required top))
          # rest)
        $ \forged ->
          plet
            (presumeNativeScriptWalkFromCommitment
              # (pnativeScriptCursorHash # stopped)
              # (pencodeNativeScriptCursor # stopped)
              # forged
              # pconstant nestedScript)
            $ \_ -> pconstant True

resumeShortenedStack :: forall s. Term s PBool
resumeShortenedStack = plet (pstopped nestedScript 5) $ \stopped ->
  pmatch (pnativeScriptWalkFrames # stopped) $ \case
    PNil -> pconstant False
    PCons _ rest -> plet
      (presumeNativeScriptWalkFromCommitment
        # (pnativeScriptCursorHash # stopped)
        # (pencodeNativeScriptCursor # stopped)
        # rest
        # pconstant nestedScript)
      $ \_ -> pconstant True

resumeWrongPayload :: forall s. BS.ByteString -> Term s PBool
resumeWrongPayload payload = plet (pstopped nestedScript 5) $ \stopped ->
  plet
    (presumeNativeScriptWalkFromCommitment
      # (pnativeScriptCursorHash # stopped)
      # (pencodeNativeScriptCursor # stopped)
      # (pnativeScriptWalkFrames # stopped)
      # pconstant payload)
    $ \_ -> pconstant True

depthBoundaryFixtures :: forall s. Term s PBool
depthBoundaryFixtures =
  pinspectedShape (nestedAlls 14 $ signature key1) 15 15
    #&& pinspectedShape (nestedAlls 15 $ signature key1) 16 16
    #&& pinspectedShape (nestedAlls 16 $ signature key1) 17 17
    #&& pisChecked (nestedAlls 15 $ signature key1)
    #&& pnot # pisChecked (nestedAlls 16 $ signature key1)

thresholdDefinition :: forall s. Term s PBool
thresholdDefinition =
  pinspectedShape (atLeast 33 [signature key1]) 2 2
    #&& pisChecked (atLeast 33 [signature key1])
    #&& precursiveVerdict (atLeast 33 [signature key1]) False
    #&& precursiveVerdict (atLeast 3 $ replicate 3 $ signature key1) True

bushyDefinitionCheck :: forall s. Term s PBool
bushyDefinitionCheck =
  pinspectedShape bushyScript 3 49
    #&& pnot # pisChecked bushyScript

pinspectedShape :: forall s. BS.ByteString -> Integer -> Integer -> Term s PBool
pinspectedShape script expectedDepth expectedNodes = pmatch
  (pinspectNativeScriptV1 # pconstant script # 200 # 300 # (pcons # pconstant key1 # pnil)) $ \case
    PNothing -> pconstant False
    PJust proof -> pmatch proof $ \checked ->
      pfromData (pnativeProof'depth checked) #== pconstant expectedDepth
        #&& pfromData (pnativeProof'nodeCount checked) #== pconstant expectedNodes

pisChecked :: forall s. BS.ByteString -> Term s PBool
pisChecked script = pmatch
  (pcheckNativeScriptV1 # pconstant script # 200 # 300 # (pcons # pconstant key1 # pnil))
  (\case PNothing -> pconstant False; PJust _ -> pconstant True)

key1, key2, key3 :: BS.ByteString
key1 = BS.replicate 28 0x11
key2 = BS.replicate 28 0x22
key3 = BS.replicate 28 0x33

signature :: BS.ByteString -> BS.ByteString
signature key = hexBytes [0x82, 0x00, 0x58, 0x1c] <> key

allOf, anyOf :: [BS.ByteString] -> BS.ByteString
allOf = compound 1
anyOf = compound 2

compound :: Word8 -> [BS.ByteString] -> BS.ByteString
compound tag children = hexBytes [0x82, tag, 0x80 + fromIntegral (length children)] <> BS.concat children

atLeast :: Integer -> [BS.ByteString] -> BS.ByteString
atLeast required children =
  hexBytes [0x83, 0x03] <> uintBytes required
    <> BS.singleton (0x80 + fromIntegral (length children)) <> BS.concat children

after, before :: Integer -> BS.ByteString
after slot = hexBytes [0x82, 0x04] <> uintBytes slot
before slot = hexBytes [0x82, 0x05] <> uintBytes slot

nestedScript, otherScript :: BS.ByteString
nestedScript = allOf
  [ anyOf [signature key1, signature key2]
  , atLeast 2 [signature key1, signature key3, after 100]
  ]
otherScript = allOf
  [ anyOf [signature key3, signature key2, before 200]
  , atLeast 1 [signature key2, after 50]
  ]

twinScript :: BS.ByteString
twinScript = allOf
  [ anyOf [signature key3, signature key2]
  , atLeast 2 [signature key1, signature key3, after 100]
  ]

nestedAlls :: Int -> BS.ByteString -> BS.ByteString
nestedAlls count inner
  | count <= 0 = inner
  | otherwise = nestedAlls (count - 1) (allOf [inner])

bushyScript :: BS.ByteString
bushyScript = allOf $ replicate 16 $ allOf [signature key1, signature key1]

wideScript :: BS.ByteString
wideScript = hexBytes [0x82, 0x02, 0x98, 0x21] <> BS.concat (replicate 33 $ signature key2)

wrongArityScript :: BS.ByteString
wrongArityScript = hexBytes [0x82, 0x03, 0x01, 0x81] <> signature key1

wrongKeyWidthScript :: BS.ByteString
wrongKeyWidthScript = hexBytes [0x82, 0x00, 0x58, 0x1b] <> BS.take 27 key1

uintBytes :: Integer -> BS.ByteString
uintBytes value
  | value <= 23 = BS.singleton $ fromIntegral value
  | value <= 0xff = hexBytes [0x18, byte 0 value]
  | value <= 0xffff = hexBytes [0x19, byte 8 value, byte 0 value]
  | value <= 0xffffffff = hexBytes [0x1a, byte 24 value, byte 16 value, byte 8 value, byte 0 value]
  | otherwise = hexBytes
      [ 0x1b, byte 56 value, byte 48 value, byte 40 value, byte 32 value
      , byte 24 value, byte 16 value, byte 8 value, byte 0 value
      ]

byte :: Int -> Integer -> Word8
byte shift value = fromIntegral (value `shiftR` shift)

hexBytes :: [Word8] -> BS.ByteString
hexBytes = BS.pack
