{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Testing.Blake2b224Trace
Description : Behavioural tests for the Plutarch port of
              @lib/midgard/blake2b-224-trace-v1.ak@.

=== The oracle is the builtin

This module reimplements BLAKE2b-224 in modular arithmetic over byte strings so
that a hash can be replayed one round at a time. The reference it is checked
against is @blake2b_224@ itself — the ledger's own implementation, in C, reached
through the builtin. For each message the trace is driven to completion, step by
step, and its digest compared against the builtin's.

There is nothing to get subtly right and hide: the IV, the parameter block, the
sigma permutation, the four rotation distances, the counter, the finalisation
flag and the padding either all agree with the reference implementation or the
digest is wrong. Messages are chosen either side of every block boundary,
because that is where the counter and the finalisation flag change.

=== A trace is a state machine, and the tests drive it wrong on purpose

Fourteen steps per block: one to absorb, twelve to mix, one to fold. The block
argument belongs to exactly one of them, and the refusal group offers a block
where none is wanted, withholds one where it is, and asks a finished trace for
another step.

=== Where the trace refuses to start

An empty message has no trace at all — @total_length > 0@ — even though
@blake2b_224("")@ is perfectly well defined. That is a deliberate narrowing, and
it is pinned here so that a later port cannot quietly widen it.
-}
module Testing.Blake2b224Trace (tests) where

import Data.Bits (xor)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.Word (Word8)
import PlutusCore.Data qualified as PD
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.Builtin.Crypto (pblake2b_224)
import Plutarch.Prelude

import Aiken.Cbor (pdeserialise)
import Midgard.Blake2b224Trace (
  PBlake2b224TraceControlV1 (..),
  pcontrolFromDataV1,
  pcontrolIsWellFormed,
  pdecodeControlV1,
  pdigestV1,
  pencodeControlV1,
  pinitialControlV1,
  pstepV1,
 )
import Midgard.FraudProofs.NativeTx.Types (PMidgardScriptLanguage (..), PMidgardVersionedScript (..))
import Midgard.ScriptProof (pversionedScriptHash)
import Testing.Eval (passertEval, pfails)

--------------------------------------------------------------------------------
-- The suite
--------------------------------------------------------------------------------

tests :: TestTree
tests =
  testGroup
    "Midgard.Blake2b224Trace"
    [ digestTests
    , aikenParityTests
    , stepDisciplineTests
    , wellFormednessTests
    , encodingTests
    ]

aikenParityTests :: TestTree
aikenParityTests = testGroup "blake2b-224-trace-v1 Aiken parity"
  [ testCase "complete_partial_block_trace_matches_the_builtin" $
      passertEval $ plet (traceDigest "\x03") $ \digest ->
        digest #== pblake2b_224 # pconstant "\x03"
          #&& digest #== pversionedScriptHash # pcon
            (PMidgardVersionedScript (pdata $ pcon PPlutusV3Script) (pdata $ pconstant ""))
  , testCase "complete_full_block_trace_matches_the_builtin" $
      passertEval $ traceDigest (BS.replicate 128 107) #== pblake2b_224 # pconstant (BS.replicate 128 107)
  , testCase "one_round_successor_is_bounded" $
      passertEval oneRoundSuccessorIsBounded
  , testCase "decodes_the_typescript_multiblock_terminal_state" $
      passertEval typescriptTerminalStateIsExact
  , testCase "decodes_the_typescript_active_state_with_chunked_byte_strings" $
      passertEval typescriptActiveStateIsExact
  , testCase "fails_closed_for_malformed_overlong_plutus_data_chunks" $
      passertEval $ pmatch (pdeserialise # pconstant malformedOverlongActiveControlCbor) $ \case
        PNothing -> pconstant @PBool True
        PJust _ -> pconstant @PBool False
  , testCase "fails_closed_for_wrong_boundaries_and_malformed_state" $
      passertEval $ pnot # (steps (pinitialControlV1 # 129) [Just $ BS.replicate 127 0])
  ]

oneRoundSuccessorIsBounded :: forall s. Term s PBool
oneRoundSuccessorIsBounded =
  plet (runSteps (pinitialControlV1 # 129) [Just $ BS.replicate 128 107, Nothing]) $ \control ->
    pmatch control $ \c ->
      pfromData (pctl'stage c) #== 1
        #&& pfromData (pctl'round c) #== 1
        #&& pfromData (pctl'cursor c) #== 0
        #&& plengthBS # pfromData (pctl'activeBlock c) #== 128
        #&& plengthBS # pfromData (pctl'workingValue c) #== 128

typescriptTerminalStateIsExact :: forall s. Term s PBool
typescriptTerminalStateIsExact =
  plet (pdecodeControlV1 # pconstant crossLanguageTerminalControlCbor) $ \control -> pmatch control $ \c ->
    pfromData (pctl'stage c) #== 3
      #&& pfromData (pctl'cursor c) #== 6_001
      #&& pfromData (pctl'totalLength c) #== 6_001
      #&& pdigestV1 # control #== pcon (PJust $ pconstant $ hex "634e9ca63abb532a52c53389db12d1514358f8ff155e3d82c0622098")
      #&& pencodeControlV1 # control #== pconstant crossLanguageTerminalControlCbor

typescriptActiveStateIsExact :: forall s. Term s PBool
typescriptActiveStateIsExact =
  plet (pdecodeControlV1 # pconstant crossLanguageActiveControlCbor) $ \control -> pmatch control $ \c ->
    pfromData (pctl'stage c) #== 1
      #&& pfromData (pctl'cursor c) #== 0
      #&& pfromData (pctl'totalLength c) #== 129
      #&& pfromData (pctl'activeBlockLength c) #== 128
      #&& plengthBS # pfromData (pctl'activeBlock c) #== 128
      #&& plengthBS # pfromData (pctl'workingValue c) #== 128
      #&& pencodeControlV1 # control #== pconstant crossLanguageActiveControlCbor

crossLanguageTerminalControlCbor, crossLanguageActiveControlCbor, malformedOverlongActiveControlCbor :: BS.ByteString
crossLanguageTerminalControlCbor = hex "8901031917711917715840634e9ca63abb532a52c53389db12d1514358f8ff155e3d82c0622098dbdd88d3a54a6646cce0bede0423668a5079fb08595004db249d66dbc8e10681056a775c40004000"
crossLanguageActiveControlCbor =
  hex "890101001881584014c9bdf267e6096a3ba7ca8485ae67bb2bf894fe72f36e3cf1361d5f3af54fa5d182e6ad7f520e511f6c3e2b8c68059b6bbd41fbabd9831f79217e1319cde05b5f5840"
    <> BS.replicate 64 107 <> hex "5840" <> BS.replicate 64 107
    <> hex "ff18805f584014c9bdf267e6096a3ba7ca8485ae67bb2bf894fe72f36e3cf1361d5f3af54fa5d182e6ad7f520e511f6c3e2b8c68059b6bbd41fbabd9831f79217e1319cde05b584008c9bcf367e6096a3ba7ca8485ae67bb2bf894fe72f36e3cf1361d5f3af54fa55182e6ad7f520e511f6c3e2b8c68059b6bbd41fbabd9831f79217e1319cde05bff00"
malformedOverlongActiveControlCbor =
  hex "890101001881584014c9bdf267e6096a3ba7ca8485ae67bb2bf894fe72f36e3cf1361d5f3af54fa5d182e6ad7f520e511f6c3e2b8c68059b6bbd41fbabd9831f79217e1319cde05b5f58406b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b58406b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6bff18805f584014c9bdf267e6096a3ba7ca8485ae67bb2bf894fe72f36e3cf1361d5f3af54fa5d182e6ad7f520e511f6c3e2b8c68059b6bbd41fbabd9831f79217e1319cde05b584008c9bcf367e6096a3ba7ca8485ae67bb2bf894fe72f36e3cf1361d5f3af54fa55182e6ad7f520e511f6c3e2b8c68059b6bbd41fbabd9831f79217e1319cde05bff00"

hex :: BS.ByteString -> BS.ByteString
hex = Base16.decodeLenient

--------------------------------------------------------------------------------
-- The digest, against the builtin
--------------------------------------------------------------------------------

{- | Message lengths either side of every boundary the trace has.

128 is the block size, so 127/128/129 exercise a short final block, an exactly
full one, and a full one followed by a one-byte block — which is where the
counter and the finalisation flag are most easily got wrong.
-}
messageLengths :: [Int]
messageLengths = [1, 2, 63, 64, 65, 127, 128, 129, 130, 200, 255, 256, 257]

message :: Int -> BS.ByteString
message n = BS.pack [fromIntegral (i * 7 + 3) | i <- [0 .. n - 1]]

digestTests :: TestTree
digestTests =
  testGroup
    "the replayed digest is the builtin's"
    [ testGroup
        "over a message of"
        [ testCase (show n <> " bytes") $
            passertEval $
              traceDigest (message n) #== (pblake2b_224 # pconstant (message n))
        | n <- messageLengths
        ]
    , testCase "a message of all zeros, where a dropped counter would still pass" $
        passertEval $
          traceDigest (BS.replicate 200 0) #== (pblake2b_224 # pconstant (BS.replicate 200 0))
    , testCase "two messages differing in one byte give different digests" $
        passertEval $
          pnot #$ traceDigest (message 130) #== traceDigest (BS.snoc (message 129) 0xff)
    , testCase "the empty message has no trace: total_length must be positive" $
        pfails $ pinitialControlV1 # pconstant 0
    , testCase "…and a negative length likewise" $
        pfails $ pinitialControlV1 # pconstant (-1)
    ]

--------------------------------------------------------------------------------
-- The state machine
--------------------------------------------------------------------------------

stepDisciplineTests :: TestTree
stepDisciplineTests =
  testGroup
    "the step is a state machine"
    [ testCase "ready accepts a block" $
        passertEval $ steps (pinitialControlV1 # pconstant 200) [Just (blockAt msg 0)]
    , testCase "…and refuses none" $
        passertEval $ pnot #$ steps (pinitialControlV1 # pconstant 200) [Nothing]
    , testCase "…and refuses a block of the wrong length" $
        passertEval $
          pnot
            #$ steps (pinitialControlV1 # pconstant 200) [Just (BS.take 127 (blockAt msg 0))]
    , testCase "…and refuses a block longer than the block size" $
        passertEval $
          pnot #$ steps (pinitialControlV1 # pconstant 200) [Just (BS.replicate 129 0)]
    , testCase "a short final block must be exactly what is left" $
        passertEval $
          steps
            (pinitialControlV1 # pconstant 200)
            (Just (blockAt msg 0) : replicate 13 Nothing <> [Just (blockAt msg 1)])
    , testCase "…and not a full block padded by the prover" $
        passertEval $
          pnot
            #$ steps
              (pinitialControlV1 # pconstant 200)
              ( Just (blockAt msg 0)
                  : replicate 13 Nothing
                  <> [Just (blockAt msg 1 <> BS.replicate 56 0)]
              )
    , testCase "round refuses a block" $
        passertEval $
          pnot #$ steps (pinitialControlV1 # pconstant 200) [Just (blockAt msg 0), Just "x"]
    , testCase "round accepts none, twelve times over" $
        passertEval $
          steps (pinitialControlV1 # pconstant 200) (Just (blockAt msg 0) : replicate 12 Nothing)
    , testCase "finish accepts none and returns to ready" $
        passertEval $
          steps (pinitialControlV1 # pconstant 200) (Just (blockAt msg 0) : replicate 13 Nothing)
    , testCase "terminal has no successor" $
        passertEval $ pnot #$ steps (finished (message 100)) [Nothing]
    , testCase "…not even with a block" $
        passertEval $ pnot #$ steps (finished (message 100)) [Just (BS.replicate 128 0)]
    , testCase "a trace short of terminal has no digest" $
        passertEval $
          noDigest $
            pdigestV1 # runSteps (pinitialControlV1 # pconstant 200) (init (stepsFor (message 200)))
    , testCase "…and one at terminal has one" $
        passertEval $
          pnot #$ noDigest (pdigestV1 # finished (message 200))
    ]
  where
    msg = message 200

--------------------------------------------------------------------------------
-- Well-formedness
--------------------------------------------------------------------------------

wellFormednessTests :: TestTree
wellFormednessTests =
  testGroup
    "a control is well formed only when"
    [ testCase "it is a fresh one" $
        passertEval $ pcontrolIsWellFormed # (pinitialControlV1 # pconstant 200)
    , testCase "it is one the trace reached" $
        passertEval $ pcontrolIsWellFormed # finished (message 200)
    , testGroup
        "and it is refused when"
        [ testCase "the version is not one" $ refuses fresh {cVersion = 2}
        , testCase "the stage is past terminal" $ refuses fresh {cStage = 4}
        , testCase "the stage is negative" $ refuses fresh {cStage = -1}
        , testCase "the total length is zero" $ refuses fresh {cTotalLength = 0}
        , testCase "the cursor is past the total length" $
            refuses fresh {cCursor = 300}
        , testCase "the cursor is negative" $ refuses fresh {cCursor = -1}
        , testCase "the chaining value is not 64 bytes" $
            refuses fresh {cChainingValue = BS.replicate 63 0}
        , testCase "the cursor is zero but the chaining value is not the canonical one" $
            refuses fresh {cChainingValue = BS.replicate 64 0}
        , testCase "a ready control has a cursor off a block boundary" $
            refuses fresh {cCursor = 1, cChainingValue = someChaining}
        , testCase "a ready control carries an active block" $
            refuses fresh {cActiveBlock = BS.replicate 128 0}
        , testCase "a ready control carries a working value" $
            refuses fresh {cWorkingValue = BS.replicate 128 0}
        , testCase "a ready control carries a round" $ refuses fresh {cRound = 1}
        , testCase "a ready control's cursor equals its total length" $
            refuses fresh {cCursor = 200, cChainingValue = someChaining}
        , testCase "a terminal control's cursor is short of the total length" $
            refuses fresh {cStage = 3, cCursor = 100, cChainingValue = someChaining}
        , testCase "a round control has a round past the count" $
            refuses (roundControl 12)
        , testCase "…and at the count it is a finish, not a round" $
            refuses (roundControl 12) {cStage = 1}
        , testCase "a finish control's round is not the count" $
            refuses (roundControl 11) {cStage = 2}
        , testCase "a round control's working value is not 128 bytes" $
            refuses (roundControl 0) {cWorkingValue = BS.replicate 127 0}
        , testCase "a round control's active block is not 128 bytes" $
            refuses (roundControl 0) {cActiveBlock = BS.replicate 127 0}
        , testCase "a short final block's padding is not zero" $
            refuses
              (roundControl 0)
                { cCursor = 128
                , cChainingValue = someChaining
                , cActiveBlockLength = 72
                , cActiveBlock = BS.replicate 72 0x11 <> BS.replicate 56 0xff
                }
        , testCase "…where the same block with zero padding is accepted" $
            passertEval $
              pcontrolIsWellFormed
                # controlT
                  (roundControl 0)
                    { cCursor = 128
                    , cChainingValue = someChaining
                    , cActiveBlockLength = 72
                    , cActiveBlock = BS.replicate 72 0x11 <> BS.replicate 56 0x00
                    }
        , testCase "the active block length disagrees with what is left" $
            refuses (roundControl 0) {cActiveBlockLength = 127}
        ]
    ]
  where
    refuses c = passertEval $ pnot #$ pcontrolIsWellFormed # controlT c
    someChaining = BS.replicate 64 0x5a

--------------------------------------------------------------------------------
-- Encoding
--------------------------------------------------------------------------------

encodingTests :: TestTree
encodingTests =
  testGroup
    "encoding"
    [ testCase "a fresh control round-trips" $
        passertEval $ roundTrips (pinitialControlV1 # pconstant 200)
    , testCase "a mid-block control round-trips" $
        passertEval $
          roundTrips (runSteps (pinitialControlV1 # pconstant 200) [Just (blockAt (message 200) 0)])
    , testCase "a terminal control round-trips" $
        passertEval $ roundTrips (finished (message 200))
    , testCase "the encoding of a malformed control aborts" $
        pfails $ pencodeControlV1 # controlT fresh {cVersion = 2}
    , testCase "decoding trailing bytes aborts" $
        pfails $
          pdecodeControlV1
            #$ (pencodeControlV1 # (pinitialControlV1 # pconstant 200)) <> pconstant "\x00"
    , testCase "decoding a control whose fields are malformed aborts" $
        pfails $ pcontrolFromDataV1 # pconstant (controlData fresh {cVersion = 2})
    , testCase "decoding an array of the wrong arity aborts" $
        pfails $ pcontrolFromDataV1 # pconstant (PD.List [PD.I 1])
    , testCase "decoding a non-array aborts" $
        pfails $ pcontrolFromDataV1 # pconstant (PD.I 1)
    ]

roundTrips ::
  forall (s :: S). (forall (s' :: S). Term s' PBlake2b224TraceControlV1) -> Term s PBool
roundTrips control =
  (pdecodeControlV1 #$ pencodeControlV1 # control) #== control

--------------------------------------------------------------------------------
-- Driving the trace
--------------------------------------------------------------------------------

blockSize :: Int
blockSize = 128

blocksOf :: BS.ByteString -> [BS.ByteString]
blocksOf bytes
  | BS.null bytes = []
  | otherwise = BS.take blockSize bytes : blocksOf (BS.drop blockSize bytes)

blockAt :: BS.ByteString -> Int -> BS.ByteString
blockAt bytes i = blocksOf bytes !! i

-- | Fourteen steps a block: one to absorb, twelve to mix, one to fold.
stepsFor :: BS.ByteString -> [Maybe BS.ByteString]
stepsFor bytes = concat [Just b : replicate 13 Nothing | b <- blocksOf bytes]

runSteps ::
  forall (s :: S).
  Term s PBlake2b224TraceControlV1 ->
  [Maybe BS.ByteString] ->
  Term s PBlake2b224TraceControlV1
runSteps = foldl unsafeStep

unsafeStep ::
  forall (s :: S).
  Term s PBlake2b224TraceControlV1 ->
  Maybe BS.ByteString ->
  Term s PBlake2b224TraceControlV1
unsafeStep control block =
  pmatch (pstepV1 # control # blockT block) $ \case
    PNothing -> perror
    PJust next -> next

blockT :: forall (s :: S). Maybe BS.ByteString -> Term s (PMaybe PByteString)
blockT = \case
  Nothing -> pcon PNothing
  Just bytes -> pcon (PJust (pconstant bytes))

-- | The control a whole message's trace ends in.
finished :: forall (s :: S). BS.ByteString -> Term s PBlake2b224TraceControlV1
finished msg =
  runSteps (pinitialControlV1 # pconstant (fromIntegral (BS.length msg))) (stepsFor msg)

traceDigest :: forall (s :: S). BS.ByteString -> Term s PByteString
traceDigest msg =
  pmatch (pdigestV1 # finished msg) $ \case
    PNothing -> perror
    PJust digest -> digest

-- | Whether every step in the sequence is accepted.
steps ::
  forall (s :: S).
  Term s PBlake2b224TraceControlV1 -> [Maybe BS.ByteString] -> Term s PBool
steps control blocks = go control blocks
  where
    go _ [] = pconstant @PBool True
    go c (b : rest) =
      pmatch (pstepV1 # c # blockT b) $ \case
        PNothing -> pconstant @PBool False
        PJust next -> go next rest

noDigest :: forall (s :: S). Term s (PMaybe PByteString) -> Term s PBool
noDigest t = pmatch t $ \case
  PNothing -> pconstant @PBool True
  PJust _ -> pconstant @PBool False

--------------------------------------------------------------------------------
-- Controls built field by field
--------------------------------------------------------------------------------

{- | A control assembled directly, for the shapes stepping cannot reach.

The chaining value defaults to the canonical initial one, so a test that only
means to disturb one field does not accidentally disturb that clause too.
-}
data RefControl = RefControl
  { cVersion :: Integer
  , cStage :: Integer
  , cCursor :: Integer
  , cTotalLength :: Integer
  , cChainingValue :: BS.ByteString
  , cActiveBlock :: BS.ByteString
  , cActiveBlockLength :: Integer
  , cWorkingValue :: BS.ByteString
  , cRound :: Integer
  }

fresh :: RefControl
fresh =
  RefControl
    { cVersion = 1
    , cStage = 0
    , cCursor = 0
    , cTotalLength = 200
    , cChainingValue = initialChainingValue
    , cActiveBlock = ""
    , cActiveBlockLength = 0
    , cWorkingValue = ""
    , cRound = 0
    }

-- | A round-stage control at the first block, with @round@ set as asked.
roundControl :: Integer -> RefControl
roundControl r =
  fresh
    { cStage = 1
    , cActiveBlock = BS.replicate 128 0x11
    , cActiveBlockLength = 128
    , cWorkingValue = BS.replicate 128 0x22
    , cRound = r
    }

controlT :: forall (s :: S). RefControl -> Term s PBlake2b224TraceControlV1
controlT c =
  pcon $
    PBlake2b224TraceControlV1
      (pdata (pconstant (cVersion c)))
      (pdata (pconstant (cStage c)))
      (pdata (pconstant (cCursor c)))
      (pdata (pconstant (cTotalLength c)))
      (pdata (pconstant (cChainingValue c)))
      (pdata (pconstant (cActiveBlock c)))
      (pdata (pconstant (cActiveBlockLength c)))
      (pdata (pconstant (cWorkingValue c)))
      (pdata (pconstant (cRound c)))

controlData :: RefControl -> PD.Data
controlData c =
  PD.List
    [ PD.I (cVersion c)
    , PD.I (cStage c)
    , PD.I (cCursor c)
    , PD.I (cTotalLength c)
    , PD.B (cChainingValue c)
    , PD.B (cActiveBlock c)
    , PD.I (cActiveBlockLength c)
    , PD.B (cWorkingValue c)
    , PD.I (cRound c)
    ]

--------------------------------------------------------------------------------
-- The reference: the one constant the tests need
--------------------------------------------------------------------------------

{- | The initial chaining value: the BLAKE2b IV with the parameter block folded
into its first word.

Written out here from the specification — eight little-endian words, then
@0x0101001c@ XORed into the low four bytes of the first — rather than taken from
the port, so a wrong IV or a wrong parameter block would show up in the
well-formedness tests as well as in the digest.
-}
initialChainingValue :: BS.ByteString
initialChainingValue =
  BS.pack (zipWith xorW8 (BS.unpack (BS.take 4 ivLe)) parameterBlockLe) <> BS.drop 4 ivLe
  where
    -- 0x0101001c little-endian: digest length 28, no key, fanout 1, depth 1.
    parameterBlockLe = [0x1c, 0x00, 0x01, 0x01]

xorW8 :: Word8 -> Word8 -> Word8
xorW8 = xor

ivLe :: BS.ByteString
ivLe =
  BS.pack
    [ 0x08, 0xc9, 0xbc, 0xf3, 0x67, 0xe6, 0x09, 0x6a
    , 0x3b, 0xa7, 0xca, 0x84, 0x85, 0xae, 0x67, 0xbb
    , 0x2b, 0xf8, 0x94, 0xfe, 0x72, 0xf3, 0x6e, 0x3c
    , 0xf1, 0x36, 0x1d, 0x5f, 0x3a, 0xf5, 0x4f, 0xa5
    , 0xd1, 0x82, 0xe6, 0xad, 0x7f, 0x52, 0x0e, 0x51
    , 0x1f, 0x6c, 0x3e, 0x2b, 0x8c, 0x68, 0x05, 0x9b
    , 0x6b, 0xbd, 0x41, 0xfb, 0xab, 0xd9, 0x83, 0x1f
    , 0x79, 0x21, 0x7e, 0x13, 0x19, 0xcd, 0xe0, 0x5b
    ]
