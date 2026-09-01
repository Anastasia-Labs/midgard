{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Testing.CekSourceBlob
Description : Behavioural tests for the Plutarch port of
              @lib/midgard/cek-source-blob-v1.ak@.

=== Driving both machines to the end

This module nests two state machines: a BLAKE2b trace that hashes one chunk a
round at a time, and a frontier that folds the chunk roots. The headline group
below runs the whole thing — every block absorbed, every round mixed, every
chunk folded — and asserts the root it finalises to is the one
@bounded_blob_root_v1@ and @root_from_chunks_v1@ produce for the same bytes.
Those two are independent implementations already pinned against explicit trees,
so agreement here is agreement across three code paths.

=== Why the blobs are small

A single 4,095-byte chunk is 33 trace blocks, or 463 outer steps, and each step
carries a full well-formedness check over both machines. The end-to-end group
therefore uses blobs of one chunk, sized to cross every boundary that matters:
the three CBOR header widths (1, 2 and 3 bytes) and the point where the framed
chunk stops fitting in one 128-byte block.

The multi-chunk arithmetic is checked separately and cheaply, by building a
control whose frontier already holds a chunk — appending a root costs nothing,
where hashing 4,095 bytes costs everything — and asking where the next span
falls.

=== The span is the interesting arithmetic

The first block of every chunk is mostly framing: 21 bytes of domain string and
one to three of length header. 'pnextSourceSpanV1' has to say how much of the
/source/ that block still needs, and where in the source it starts, while the
prefix is being consumed and after it has been. Both sides of that transition
are checked against a reference written from the format.
-}
module Testing.CekSourceBlob (tests) where

import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.Prelude

import Midgard.Blake2b256Trace qualified as Trace
import Midgard.CekBlobFrontier (
  PCekBlobFrontierV1 (..),
  pappendChunkRootV1,
  pemptyFrontierV1,
  prootFromChunksV1,
 )
import Midgard.CekProof (pboundedBlobRootV1)
import Midgard.CekSourceBlob (
  PCekSourceBlobControlV1 (..),
  PCekSourceBlobSpanV1 (..),
  pcontrolIsWellFormed,
  pdecodeControlV1,
  pencodeControlV1,
  pfinalizeV1,
  pinitialControlV1,
  pnextSourceSpanV1,
  pstepV1,
 )
import Testing.BoundedItem (blake2b256)
import Testing.Eval (passertEval, pfails)

--------------------------------------------------------------------------------
-- The suite
--------------------------------------------------------------------------------

tests :: TestTree
tests =
  testGroup
    "Midgard.CekSourceBlob"
    [ testGroup "Aiken parity" aikenParityTests
    , endToEndTests
    , spanTests
    , stepDisciplineTests
    , wellFormednessTests
    , encodingTests
    ]

aikenParityTests :: [TestTree]
aikenParityTests =
  [ testCase "streams_an_authenticated_source_span_into_the_canonical_blob_root" $
      passertEval $
        spanIs (freshFrom 73 (BS.length authenticatedSource)) (73, 64)
          #&& (streamedRootFrom 73 authenticatedSource #== pconstant (hashBlobChunk authenticatedSource))
  , testCase "decodes_the_typescript_multileaf_terminal_control" $
      passertEval typescriptTerminalControlMatches
  , testCase "rejects_missing_and_surplus_source_bytes" $
      passertEval $
        pnot # (accepts (freshFrom 9 129) [Nothing])
          #&& pnot # (accepts (freshFrom 9 129) [Just (BS.replicate 104 106)])
          #&& pnot # (accepts (freshFrom 9 129) [Just (BS.replicate 106 106)])
  ]

authenticatedSource :: BS.ByteString
authenticatedSource = BS.replicate 64 106

typescriptTerminalControlCbor, typescriptTerminalRoot :: BS.ByteString
typescriptTerminalControlCbor =
  hex "860101111910008401021910008183015820eeae7280d2825a069ee81fdde1b202e15766bb7bf1689a514224772d104bc59d191000d87a80"
typescriptTerminalRoot = hex "eeae7280d2825a069ee81fdde1b202e15766bb7bf1689a514224772d104bc59d"

typescriptTerminalControlMatches :: forall (s :: S). Term s PBool
typescriptTerminalControlMatches =
  plet (pdecodeControlV1 # pconstant typescriptTerminalControlCbor) $ \control ->
    pmatch control $ \c ->
      pmatch (pfromData (pblob'frontier c)) $ \(PCekBlobFrontierV1 count byteLength _) ->
        pfromData (pblob'stage c) #== 1
          #&& (pfromData (pblob'sourceStart c) #== 17)
          #&& (pfromData (pblob'sourceLength c) #== 4096)
          #&& (pfromData count #== 2)
          #&& (pfromData byteLength #== 4096)
          #&& rootIs (pfinalizeV1 # control) typescriptTerminalRoot
          #&& ((pencodeControlV1 # control) #== pconstant typescriptTerminalControlCbor)

--------------------------------------------------------------------------------
-- The whole machine
--------------------------------------------------------------------------------

chunkBytes :: Int
chunkBytes = 4095

domainLength :: Int
domainLength = BS.length "MidgardCekBlobChunkV1"

{- | Blob lengths that cross every boundary a one-chunk blob has.

@105@ is the one to keep: with a two-byte header the framed chunk is exactly 128
bytes, so it is the last size that fits one trace block.
-}
blobLengths :: [Int]
blobLengths = [0, 1, 23, 24, 104, 105, 106, 200, 255, 256, 300]

blob :: Int -> BS.ByteString
blob n = BS.pack [fromIntegral (i * 11 + 5) | i <- [0 .. n - 1]]

endToEndTests :: TestTree
endToEndTests =
  testGroup
    "the streamed root is the one the other two implementations produce"
    [ testGroup
        "against bounded_blob_root_v1"
        [ testCase (show n <> " bytes") $
            passertEval $
              streamedRoot (blob n) #== (pboundedBlobRootV1 # pconstant (blob n))
        | n <- blobLengths
        ]
    , testGroup
        "and against root_from_chunks_v1"
        [ testCase (show n <> " bytes") $
            passertEval $ streamedRoot (blob n) #== rootFromChunks (blob n)
        | n <- blobLengths
        ]
    , testCase "the empty blob is one chunk, not none" $
        passertEval $ streamedRoot "" #== pconstant (hashBlobChunk "")
    , testCase "…and that is what bounded_blob_root_v1 says too" $
        passertEval $ (pboundedBlobRootV1 # pconstant "") #== pconstant (hashBlobChunk "")
    , testCase "two blobs differing in one byte give different roots" $
        passertEval $
          pnot #$ streamedRoot (blob 100) #== streamedRoot (BS.snoc (BS.init (blob 100)) 0xff)
    , testCase "a blob still being hashed has no root" $
        passertEval $
          noRoot $ pfinalizeV1 # runSteps (freshFor 100) (init (stepsForBlob (blob 100)))
    , testCase "source_start does not change the root" $
        passertEval $
          streamedRootFrom 0 (blob 100) #== streamedRootFrom 9999 (blob 100)
    ]

--------------------------------------------------------------------------------
-- Spans
--------------------------------------------------------------------------------

spanTests :: TestTree
spanTests =
  testGroup
    "the next span"
    [ testGroup
        "at the start of a blob"
        [ testCase (show n <> " bytes") $
            passertEval $ spanIs (freshFor n) (refSpan 0 0 n 0)
        | n <- blobLengths
        ]
    , testCase "a short final block exposes all 23 source bytes" $
        refSpanLength 0 23 0 @?= 23
    , testCase "…23 for a medium one" $
        refSpanLength 0 200 0 @?= 128 - (domainLength + 2)
    , testCase "…and 24 for a long one" $
        refSpanLength 0 300 0 @?= 128 - (domainLength + 3)
    , testCase "the second block of a chunk is all source" $
        passertEval $
          spanIs (runSteps (freshFor 300) (take 14 (stepsForBlob (blob 300)))) (refSpan 0 0 300 128)
    , testCase "…and starts where the first block left off" $
        refSpanStart 0 0 300 128 @?= 128 - fromIntegral (domainLength + 3)
    , testCase "a span offset by source_start is offset in the answer" $
        passertEval $ spanIs (freshFrom 500 100) (refSpan 500 0 100 0)
    , testGroup
        "in the second chunk of a multi-chunk blob"
        [ testCase "starts one whole chunk into the source" $
            passertEval $ spanIs (secondChunkControl 5000) (refSpan 0 chunkBytes 5000 0)
        , testCase "…at an offset source_start moves" $
            passertEval $ spanIs (secondChunkControlFrom 77 5000) (refSpan 77 chunkBytes 5000 0)
        , testCase "…and asks for a full-length chunk when more than one remains" $
            passertEval $
              spanIs (secondChunkControl (3 * chunkBytes)) (refSpan 0 chunkBytes (3 * chunkBytes) 0)
        ]
    , testCase "a terminal control has no span" $
        passertEval $ noSpan $ pnextSourceSpanV1 # finishedFor (blob 100)
    , testCase "a control mid-round has no span: it wants no block" $
        passertEval $
          noSpan $ pnextSourceSpanV1 # runSteps (freshFor 100) [head (stepsForBlob (blob 100))]
    ]

--------------------------------------------------------------------------------
-- Step discipline
--------------------------------------------------------------------------------

stepDisciplineTests :: TestTree
stepDisciplineTests =
  testGroup
    "the step is a state machine"
    [ testCase "the first step wants source bytes" $
        passertEval $ accepts (freshFor 100) [head (stepsForBlob (blob 100))]
    , testCase "…and refuses none" $
        passertEval $ pnot #$ accepts (freshFor 100) [Nothing]
    , testCase "…and refuses bytes of the wrong length" $
        passertEval $ pnot #$ accepts (freshFor 100) [Just (BS.take 3 (blob 100))]
    , testCase "…and refuses bytes that are one too many" $
        passertEval $
          pnot #$ accepts (freshFor 100) [Just (blob 100 <> "\x00")]
    , testCase "a round refuses source bytes" $
        passertEval $
          pnot #$ accepts (freshFor 100) [head (stepsForBlob (blob 100)), Just "x"]
    , testCase "the fold that closes a chunk refuses source bytes" $
        passertEval $
          pnot #$ accepts (freshFor 100) (take 14 (stepsForBlob (blob 100)) <> [Just "x"])
    , testCase "…and accepts none" $
        passertEval $ accepts (freshFor 100) (take 15 (stepsForBlob (blob 100)))
    , testCase "a terminal control has no successor" $
        passertEval $ pnot #$ accepts (finishedFor (blob 100)) [Nothing]
    , testCase "…not even with bytes" $
        passertEval $ pnot #$ accepts (finishedFor (blob 100)) [Just "x"]
    , testCase "the whole sequence is accepted, step for step" $
        passertEval $ accepts (freshFor 200) (stepsForBlob (blob 200))
    ]

--------------------------------------------------------------------------------
-- Well-formedness
--------------------------------------------------------------------------------

wellFormednessTests :: TestTree
wellFormednessTests =
  testGroup
    "a control is well formed only when"
    [ testCase "it is a fresh one" $
        passertEval $ pcontrolIsWellFormed # freshFor 200
    , testCase "it is a terminal one" $
        passertEval $ pcontrolIsWellFormed # finishedFor (blob 200)
    , testCase "a negative source length aborts at construction" $
        pfails $ pinitialControlV1 # pconstant 0 # pconstant (-1)
    , testCase "a negative source start aborts at construction" $
        pfails $ pinitialControlV1 # pconstant (-1) # pconstant 100
    , testCase "the zero-length blob is allowed, because it is one chunk" $
        passertEval $ pcontrolIsWellFormed # (pinitialControlV1 # pconstant 0 # pconstant 0)
    , testCase "a control whose stage is past terminal is refused" $
        passertEval $ pnot #$ pcontrolIsWellFormed # withStage 2 (freshFor 200)
    , testCase "a control whose stage is negative is refused" $
        passertEval $ pnot #$ pcontrolIsWellFormed # withStage (-1) (freshFor 200)
    , testCase "an active control with no active hash is refused" $
        passertEval $ pnot #$ pcontrolIsWellFormed # withoutActiveHash (freshFor 200)
    , testCase "a terminal control that still carries an active hash is refused" $
        passertEval $ pnot #$ pcontrolIsWellFormed # withStage 1 (freshFor 200)
    , testCase "a terminal control whose frontier is short of the chunk count is refused" $
        passertEval $
          pnot #$ pcontrolIsWellFormed # withoutActiveHash (withStage 1 (freshFor 200))
    , testCase "an active hash sized for the wrong chunk length is refused" $
        passertEval $
          pnot #$ pcontrolIsWellFormed # withActiveHashLength 999 (freshFor 200)
    , testCase "…and at the right length it is accepted" $
        passertEval $
          pcontrolIsWellFormed # withActiveHashLength (framedLength 200) (freshFor 200)
    ]

--------------------------------------------------------------------------------
-- Encoding
--------------------------------------------------------------------------------

encodingTests :: TestTree
encodingTests =
  testGroup
    "encoding"
    [ testCase "a fresh control round-trips" $ passertEval $ roundTrips (freshFor 200)
    , testCase "a mid-chunk control round-trips" $
        passertEval $
          roundTrips (runSteps (freshFor 200) (take 5 (stepsForBlob (blob 200))))
    , testCase "a terminal control round-trips" $
        passertEval $ roundTrips (finishedFor (blob 200))
    , testCase "a control at a non-zero source start round-trips" $
        passertEval $ roundTrips (freshFrom 12345 200)
    , testCase "a terminal control encodes its absent hash as the None constructor" $
        passertEval $
          psuffixIs (pencodeControlV1 # finishedFor (blob 200)) "\xd8\x7a\x80"
    , testCase "an active control does not" $
        passertEval $
          pnot #$ psuffixIs (pencodeControlV1 # freshFor 200) "\xd8\x7a\x80"
    , testCase "…it ends with the indefinite-array break instead" $
        passertEval $ psuffixIs (pencodeControlV1 # freshFor 200) "\xff"
    , testCase "encoding a malformed control aborts" $
        pfails $ pencodeControlV1 # withStage 2 (freshFor 200)
    , testCase "decoding trailing bytes aborts" $
        pfails $
          pdecodeControlV1 #$ (pencodeControlV1 # freshFor 200) <> pconstant "\x00"
    , testCase "decoding a truncated encoding aborts" $
        pfails $
          pdecodeControlV1
            #$ psliceBS
            # 0
            # 10
            # (pencodeControlV1 # freshFor 200)
    ]

roundTrips ::
  forall (s :: S). (forall (s' :: S). Term s' PCekSourceBlobControlV1) -> Term s PBool
roundTrips control = (pdecodeControlV1 #$ pencodeControlV1 # control) #== control

psuffixIs :: forall (s :: S). Term s PByteString -> BS.ByteString -> Term s PBool
psuffixIs bytes suffix =
  plet (plengthBS # bytes) $ \len ->
    plet (pconstant (fromIntegral (BS.length suffix))) $ \n ->
      (psliceBS # (len - n) # n # bytes) #== pconstant suffix

--------------------------------------------------------------------------------
-- Driving the machine
--------------------------------------------------------------------------------

freshFor :: forall (s :: S). Int -> Term s PCekSourceBlobControlV1
freshFor = freshFrom 0

freshFrom :: forall (s :: S). Integer -> Int -> Term s PCekSourceBlobControlV1
freshFrom start n =
  pinitialControlV1 # pconstant start # pconstant (fromIntegral n)

runSteps ::
  forall (s :: S).
  Term s PCekSourceBlobControlV1 ->
  [Maybe BS.ByteString] ->
  Term s PCekSourceBlobControlV1
runSteps = foldl unsafeStep

unsafeStep ::
  forall (s :: S).
  Term s PCekSourceBlobControlV1 ->
  Maybe BS.ByteString ->
  Term s PCekSourceBlobControlV1
unsafeStep control bytes =
  pmatch (pstepV1 # control # bytesT bytes) $ \case
    PNothing -> perror
    PJust next -> next

bytesT :: forall (s :: S). Maybe BS.ByteString -> Term s (PMaybe PByteString)
bytesT = \case
  Nothing -> pcon PNothing
  Just b -> pcon (PJust (pconstant b))

finishedFor :: forall (s :: S). BS.ByteString -> Term s PCekSourceBlobControlV1
finishedFor src = runSteps (freshFor (BS.length src)) (stepsForBlob src)

streamedRoot :: forall (s :: S). BS.ByteString -> Term s PByteString
streamedRoot = streamedRootFrom 0

streamedRootFrom :: forall (s :: S). Integer -> BS.ByteString -> Term s PByteString
streamedRootFrom start src =
  pmatch (pfinalizeV1 # runSteps (freshFrom start (BS.length src)) (stepsForBlob src)) $ \case
    PNothing -> perror
    PJust root -> root

rootFromChunks :: forall (s :: S). BS.ByteString -> Term s PByteString
rootFromChunks src =
  pmatch (prootFromChunksV1 # chunkListT (chunksOfBlob src)) $ \case
    PNothing -> perror
    PJust root -> root

chunkListT ::
  forall (s :: S). [BS.ByteString] -> Term s (PBuiltinList (PAsData PByteString))
chunkListT = foldr (\c acc -> pcons # pdata (pconstant c) # acc) pnil

accepts ::
  forall (s :: S).
  Term s PCekSourceBlobControlV1 -> [Maybe BS.ByteString] -> Term s PBool
accepts control blocks = go control blocks
  where
    go _ [] = pconstant @PBool True
    go c (b : rest) =
      pmatch (pstepV1 # c # bytesT b) $ \case
        PNothing -> pconstant @PBool False
        PJust next -> go next rest

noRoot :: forall (s :: S). Term s (PMaybe PByteString) -> Term s PBool
noRoot t = pmatch t $ \case
  PNothing -> pconstant @PBool True
  PJust _ -> pconstant @PBool False

rootIs :: forall (s :: S). Term s (PMaybe PByteString) -> BS.ByteString -> Term s PBool
rootIs t expected = pmatch t $ \case
  PNothing -> pconstant @PBool False
  PJust root -> root #== pconstant expected

noSpan :: forall (s :: S). Term s (PMaybe PCekSourceBlobSpanV1) -> Term s PBool
noSpan t = pmatch t $ \case
  PNothing -> pconstant @PBool True
  PJust _ -> pconstant @PBool False

spanIs ::
  forall (s :: S).
  Term s PCekSourceBlobControlV1 -> (Integer, Integer) -> Term s PBool
spanIs control (start, len) =
  pmatch (pnextSourceSpanV1 # control) $ \case
    PNothing -> pconstant @PBool False
    PJust span ->
      pmatch span $ \(PCekSourceBlobSpanV1 gotStart gotLength) ->
        pfromData gotStart
          #== pconstant start
          #&& pfromData gotLength
          #== pconstant len

--------------------------------------------------------------------------------
-- Controls built by hand
--------------------------------------------------------------------------------

{- | A control whose frontier already holds one whole chunk.

Built by appending a chunk /root/ rather than by hashing 4,095 bytes, which is
what makes the multi-chunk span arithmetic testable at all.
-}
secondChunkControl :: forall (s :: S). Int -> Term s PCekSourceBlobControlV1
secondChunkControl = secondChunkControlFrom 0

secondChunkControlFrom ::
  forall (s :: S). Integer -> Int -> Term s PCekSourceBlobControlV1
secondChunkControlFrom start sourceLength =
  pmatch (freshFrom start sourceLength) $ \c ->
    pcon
      c
        { pblob'frontier = pdata oneChunkFrontier
        , pblob'activeHash =
            pdata $
              pcon $
                PDJust $
                  pdata $
                    Trace.pinitialControlV1
                      # pconstant (fromIntegral (framedLength (min chunkBytes (sourceLength - chunkBytes))))
        }

oneChunkFrontier :: forall (s :: S). Term s PCekBlobFrontierV1
oneChunkFrontier =
  pmatch
    ( pappendChunkRootV1
        # pemptyFrontierV1
        # pconstant (hashBlobChunk (BS.replicate chunkBytes 0))
        # pconstant (fromIntegral chunkBytes)
    )
    $ \case
      PNothing -> perror
      PJust frontier -> frontier

withStage ::
  forall (s :: S).
  Integer ->
  (forall (s' :: S). Term s' PCekSourceBlobControlV1) ->
  Term s PCekSourceBlobControlV1
withStage stage control =
  pmatch control $ \c -> pcon c {pblob'stage = pdata (pconstant stage)}

withoutActiveHash ::
  forall (s :: S).
  (forall (s' :: S). Term s' PCekSourceBlobControlV1) ->
  Term s PCekSourceBlobControlV1
withoutActiveHash control =
  pmatch control $ \c -> pcon c {pblob'activeHash = pdata (pcon PDNothing)}

withActiveHashLength ::
  forall (s :: S).
  Int ->
  (forall (s' :: S). Term s' PCekSourceBlobControlV1) ->
  Term s PCekSourceBlobControlV1
withActiveHashLength total control =
  pmatch control $ \c ->
    pcon
      c
        { pblob'activeHash =
            pdata $
              pcon $
                PDJust $
                  pdata (Trace.pinitialControlV1 # pconstant (fromIntegral total))
        }

--------------------------------------------------------------------------------
-- The reference
--------------------------------------------------------------------------------

-- | The CBOR length header a chunk of this length is framed with.
headerLength :: Int -> Int
headerLength n
  | n < 24 = 1
  | n <= 255 = 2
  | otherwise = 3

-- | What the trace actually hashes: the domain, the header, then the chunk.
framedLength :: Int -> Int
framedLength n = domainLength + headerLength n + n

{- | The chunks a source blob is committed in.

Unlike an ordinary chunking, the empty blob yields /one/ empty chunk — which is
@expected_chunk_count(0) == 1@, and is why the empty blob has a root at all.
-}
chunksOfBlob :: BS.ByteString -> [BS.ByteString]
chunksOfBlob bytes
  | BS.null bytes = [""]
  | otherwise = go bytes
  where
    go b
      | BS.null b = []
      | otherwise = BS.take chunkBytes b : go (BS.drop chunkBytes b)

{- | Aiken @next_source_span_v1@, written from the format.

@(absolute_start, length)@ for the block at @cursor@ of the chunk that starts
@consumed@ bytes into a source of @sourceLength@ beginning at @sourceStart@.
-}
refSpan :: Integer -> Int -> Int -> Int -> (Integer, Integer)
refSpan sourceStart consumed sourceLength cursor =
  (fromIntegral (refSpanStart sourceStart consumed sourceLength cursor'), fromIntegral spanLen)
  where
    cursor' = cursor
    spanLen = refSpanLength consumed sourceLength cursor

refSpanLength :: Int -> Int -> Int -> Int
refSpanLength consumed sourceLength cursor =
  blockLength - (prefixEnd - prefixStart)
  where
    chunkLength = if sourceLength == 0 then 0 else min chunkBytes (sourceLength - consumed)
    prefixLength = domainLength + headerLength chunkLength
    total = prefixLength + chunkLength
    blockLength = min 128 (total - cursor)
    prefixStart = min cursor prefixLength
    prefixEnd = min (cursor + blockLength) prefixLength

refSpanStart :: Integer -> Int -> Int -> Int -> Integer
refSpanStart sourceStart consumed sourceLength cursor =
  sourceStart
    + fromIntegral consumed
    + fromIntegral (if cursor > prefixLength then cursor - prefixLength else 0)
  where
    chunkLength = if sourceLength == 0 then 0 else min chunkBytes (sourceLength - consumed)
    prefixLength = domainLength + headerLength chunkLength

{- | The whole step sequence for a blob.

Fourteen steps a trace block — one to absorb, twelve to mix, one to fold the
block into the chaining value — and then one more to close the chunk.
-}
stepsForBlob :: BS.ByteString -> [Maybe BS.ByteString]
stepsForBlob src = concat (zipWith chunkSteps [0 ..] (chunksOfBlob src))
  where
    sourceLength = BS.length src
    chunkSteps index chunk =
      concat
        [ Just (spanBytesAt chunk consumed cursor) : replicate 13 Nothing
        | cursor <- takeWhile (< framedLength (BS.length chunk)) [0, 128 ..]
        ]
        <> [Nothing]
      where
        consumed = index * chunkBytes
        spanBytesAt c consumed' cursor =
          BS.take
            (refSpanLength consumed' sourceLength cursor)
            (BS.drop (sourceOffset consumed' cursor) c)
        sourceOffset consumed' cursor =
          let chunkLength =
                if sourceLength == 0 then 0 else min chunkBytes (sourceLength - consumed')
              prefixLength = domainLength + headerLength chunkLength
           in max 0 (cursor - prefixLength)

-- | The chunk leaf hash, which is what the trace is computing.
hashBlobChunk :: BS.ByteString -> BS.ByteString
hashBlobChunk chunk = blake2b256 ("MidgardCekBlobChunkV1" <> definiteBytesLong chunk)

definiteBytesLong :: BS.ByteString -> BS.ByteString
definiteBytesLong bytes
  | len < 24 = BS.pack [fromIntegral (64 + len)] <> bytes
  | len <= 255 = BS.pack [0x58, fromIntegral len] <> bytes
  | otherwise =
      BS.pack [0x59, fromIntegral (len `div` 256), fromIntegral (len `mod` 256)] <> bytes
  where
    len = BS.length bytes

hex :: BS.ByteString -> BS.ByteString
hex = either error id . Base16.decode
