{- |
Module      : Midgard.Blake2b256Trace
Description : Plutarch port of @lib/midgard/blake2b-256-trace-v1.ak@.

BLAKE2b-256 as a __replayable trace__: the hash is computed one step at a time —
one block absorbed, one round mixed, one block folded in — so a fault proof can
charge a single round rather than a whole digest, and can dispute a hash of a
message far larger than any proof could carry.

=== Four stages, and what each may do

@ready@ waits for the next block and is the only stage that accepts one.
@round@ applies one of the twelve mixing rounds. @finish@ folds the working
value back into the chaining value. @terminal@ is the end, and 'pdigestV1' is
the only thing that reads it. 'pstepV1' refuses a block in any stage but
@ready@, and refuses the absence of one in @ready@ — the block argument is part
of the transition, not a hint.

=== Every step is checked twice

'pstepV1' validates the control it is given and the control it produces. The
incoming check rejects a forged state; the outgoing one rejects a transition
that would have produced an unreachable one. Between them, no reachable trace
passes through a control that 'pcontrolIsWellFormed' would reject, so a proof
about any single step is a proof about a real hash.

=== The padding is part of the commitment

A final short block is padded to 128 bytes with zeros, and
'pactiveBlockPaddingIsZero' checks that the padding really is zero rather than
whatever the prover liked. Without it two different traces could absorb the same
final block and disagree about the digest.

=== The arithmetic is 64-bit, done in bytes

Plutus has no machine words, so a 64-bit word is eight bytes little-endian and
every operation goes through them: addition is modular arithmetic, XOR is
'pxorBS' over two eight-byte strings, and rotation is a divide and a multiply.
The four rotation distances BLAKE2b uses — 32, 24, 16 and 63 — are the only ones
accepted, and any other aborts, exactly as the Aiken's @expect@ does.
-}
module Midgard.Blake2b256Trace (
  -- * Constants
  pblake2b256TraceVersion,
  pstageReady,
  pstageRound,
  pstageFinish,
  pstageTerminal,
  pblockBytes,
  proundCount,
  pdigestBytes,

  -- * The control
  PBlake2b256TraceControlV1 (..),
  pcontrolIsWellFormed,
  pinitialControlV1,

  -- * Stepping
  pstepV1,
  pdigestV1,

  -- * Encoding
  pencodeControlV1,
  pcontrolFromDataV1,
  pdecodeControlV1,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP

import Plutarch.Builtin.ByteString (pbyteStringToInteger, pintegerToByteString, pmostSignificantLast)
import Plutarch.Core.Internal.Builtins (pindexBS')
import Plutarch.Core.Utils (pand'List)
import Plutarch.Prelude

import Aiken.Cbor (pdeserialise)
import Midgard.FraudProofs.NativeTx.Codec (pcborInt, pencodeDefiniteArrayHeader)

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

-- | Aiken @version@.
pblake2b256TraceVersion :: forall (s :: S). Term s PInteger
pblake2b256TraceVersion = 1

-- | Aiken @stage_ready@ and its three siblings.
pstageReady, pstageRound, pstageFinish, pstageTerminal :: forall (s :: S). Term s PInteger
pstageReady = 0
pstageRound = 1
pstageFinish = 2
pstageTerminal = 3

-- | Aiken @block_bytes@, @round_count@, @digest_bytes@.
pblockBytes, proundCount, pdigestBytes :: forall (s :: S). Term s PInteger
pblockBytes = 128
proundCount = 12
pdigestBytes = 32

pwordModulus, pwordMask :: forall (s :: S). Term s PInteger
pwordModulus = 18446744073709551616
pwordMask = 18446744073709551615

{- | Aiken @parameter_block_v1@.

@0x01010020@ read as a little-endian word: one byte of digest length (32), one
of key length (0), fanout 1 and depth 1. It is XORed into the first word of the
IV, which is what makes this BLAKE2b-/256/ rather than BLAKE2b-512.
-}
pparameterBlockV1 :: forall (s :: S). Term s PInteger
pparameterBlockV1 = 16842784

{- | The eight BLAKE2b IV words, little-endian, back to back.

These are the fractional parts of the square roots of the first eight primes —
the same constants SHA-512 uses.
-}
pivLe :: forall (s :: S). Term s PByteString
pivLe =
  pconstant
    "\x08\xc9\xbc\xf3\x67\xe6\x09\x6a\x3b\xa7\xca\x84\x85\xae\x67\xbb\x2b\xf8\x94\xfe\x72\xf3\x6e\x3c\xf1\x36\x1d\x5f\x3a\xf5\x4f\xa5\xd1\x82\xe6\xad\x7f\x52\x0e\x51\x1f\x6c\x3e\x2b\x8c\x68\x05\x9b\x6b\xbd\x41\xfb\xab\xd9\x83\x1f\x79\x21\x7e\x13\x19\xcd\xe0\x5b"

{- | The ten @sigma@ permutation rows, flattened to 160 bytes.

Aiken carries them as @List<List<Int>>@ and reads one with @list.at@. Every
entry is in @0..15@, so one byte each is exact, and a flat byte string turns two
list traversals into two index operations. The row is @round % 10@ and the word
is @row * 16 + index@; both are bounded by their callers, and an out-of-range
index aborts here exactly as @expect Some(word)@ does there.
-}
psigmaRows :: forall (s :: S). Term s PByteString
psigmaRows =
  pconstant
    "\x00\x01\x02\x03\x04\x05\x06\x07\x08\x09\x0a\x0b\x0c\x0d\x0e\x0f\x0e\x0a\x04\x08\x09\x0f\x0d\x06\x01\x0c\x00\x02\x0b\x07\x05\x03\x0b\x08\x0c\x00\x05\x02\x0f\x0d\x0a\x0e\x03\x06\x07\x01\x09\x04\x07\x09\x03\x01\x0d\x0c\x0b\x0e\x02\x06\x05\x0a\x04\x00\x0f\x08\x09\x00\x05\x07\x02\x04\x0a\x0f\x0e\x01\x0b\x0c\x06\x08\x03\x0d\x02\x0c\x06\x0a\x00\x0b\x08\x03\x04\x0d\x07\x05\x0f\x0e\x01\x09\x0c\x05\x01\x0f\x0e\x0d\x04\x0a\x00\x07\x06\x03\x09\x02\x08\x0b\x0d\x0b\x07\x0e\x0c\x01\x03\x09\x05\x00\x0f\x04\x08\x06\x02\x0a\x06\x0f\x0e\x09\x0b\x03\x00\x08\x0c\x02\x0d\x07\x01\x04\x0a\x05\x0a\x02\x08\x04\x07\x06\x01\x05\x0f\x0b\x09\x0e\x03\x0c\x0d\x00"

--------------------------------------------------------------------------------
-- The control
--------------------------------------------------------------------------------

{- | Aiken @Blake2b256TraceControlV1@.

Everything a replay needs between steps. @active_block@, @working_value@ and
@round@ are the /within-block/ state and are empty outside @round@ and @finish@;
@cursor@ and @chaining_value@ are the /across-block/ state and persist.
-}
data PBlake2b256TraceControlV1 (s :: S) = PBlake2b256TraceControlV1
  { pctl'version :: Term s (PAsData PInteger)
  , pctl'stage :: Term s (PAsData PInteger)
  , pctl'cursor :: Term s (PAsData PInteger)
  , pctl'totalLength :: Term s (PAsData PInteger)
  , pctl'chainingValue :: Term s (PAsData PByteString)
  , pctl'activeBlock :: Term s (PAsData PByteString)
  , pctl'activeBlockLength :: Term s (PAsData PInteger)
  , pctl'workingValue :: Term s (PAsData PByteString)
  , pctl'round :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PBlake2b256TraceControlV1)

--------------------------------------------------------------------------------
-- 64-bit words, in bytes
--------------------------------------------------------------------------------

-- | Aiken @word_le@ — one 64-bit word as eight little-endian bytes.
pwordLe :: forall (s :: S). Term s (PInteger :--> PByteString)
pwordLe = phoistAcyclic $
  plam $ \value ->
    pintegerToByteString # pmostSignificantLast # 8 # (pmod # value # pwordModulus)

-- | Aiken @word_at_le@ — the @index@th word of a byte string.
pwordAtLe :: forall (s :: S). Term s (PByteString :--> PInteger :--> PInteger)
pwordAtLe = phoistAcyclic $
  plam $ \bytes index ->
    pbyteStringToInteger # pmostSignificantLast #$ psliceBS # (index * 8) # 8 # bytes

-- | Aiken @replace_word_le@ — the same string with one word overwritten.
preplaceWordLe ::
  forall (s :: S). Term s (PByteString :--> PInteger :--> PInteger :--> PByteString)
preplaceWordLe = phoistAcyclic $
  plam $ \bytes index value ->
    plet (index * 8) $ \start ->
      plet (start + 8) $ \suffixStart ->
        (psliceBS # 0 # start # bytes)
          <> (pwordLe # value)
          <> (psliceBS # suffixStart # (plengthBS # bytes - suffixStart) # bytes)

-- | Aiken @add64@ — three-way modular addition, because that is how @mix@ uses it.
padd64 :: forall (s :: S). Term s (PInteger :--> PInteger :--> PInteger :--> PInteger)
padd64 = phoistAcyclic $
  plam $ \first second third -> pmod # (first + second + third) # pwordModulus

{- | Aiken @xor64@.

Through bytes, because Plutus has no integer XOR. Truncation semantics rather
than padding, as the Aiken passes — immaterial here, since both operands are
exactly eight bytes, but it is what the original says.
-}
pxor64 :: forall (s :: S). Term s (PInteger :--> PInteger :--> PInteger)
pxor64 = phoistAcyclic $
  plam $ \first second ->
    pbyteStringToInteger
      # pmostSignificantLast
      #$ pxorBS
      # ptruncation
      # (pwordLe # first)
      # (pwordLe # second)

{- | Aiken @rotate_right64@.

A rotation is a divide and a multiply: the low @bits@ move to the top and the
rest shift down. Only BLAKE2b's four distances are accepted; anything else
aborts, which is the @expect bits == 63@ at the bottom of the Aiken's chain.
-}
protateRight64 :: forall (s :: S). Term s (PInteger :--> PInteger :--> PInteger)
protateRight64 = phoistAcyclic $
  plam $ \value bits ->
    plet
      ( pif (bits #== 32) 4294967296 $
          pif (bits #== 24) 16777216 $
            pif (bits #== 16) 65536 $
              pif (bits #== 63) 9223372036854775808 perror
      )
      $ \divisor ->
        plet
          ( pif (bits #== 32) 4294967296 $
              pif (bits #== 24) 1099511627776 $
                pif (bits #== 16) 281474976710656 2
          )
          $ \multiplier ->
            (pquot # value # divisor) + (prem # value # divisor) * multiplier

--------------------------------------------------------------------------------
-- The compression function
--------------------------------------------------------------------------------

{- | Aiken @mix@ — BLAKE2b's @G@.

Four words of the working value are updated in place, twice each, with the two
message words woven in between. Written exactly as the Aiken writes it, in
sequence, because the intermediate values are what the rotations act on.
-}
pmix ::
  forall (s :: S).
  Term
    s
    ( PByteString
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PByteString
    )
pmix = phoistAcyclic $
  plam $ \words a b c d x y ->
    plet (padd64 # (pwordAtLe # words # a) # (pwordAtLe # words # b) # x) $ \firstA ->
      plet (protateRight64 # (pxor64 # (pwordAtLe # words # d) # firstA) # 32) $ \firstD ->
        plet (padd64 # (pwordAtLe # words # c) # firstD # 0) $ \firstC ->
          plet (protateRight64 # (pxor64 # (pwordAtLe # words # b) # firstC) # 24) $ \firstB ->
            plet (padd64 # firstA # firstB # y) $ \secondA ->
              plet (protateRight64 # (pxor64 # firstD # secondA) # 16) $ \secondD ->
                plet (padd64 # firstC # secondD # 0) $ \secondC ->
                  plet (protateRight64 # (pxor64 # firstB # secondC) # 63) $ \secondB ->
                    preplaceWordLe
                      # ( preplaceWordLe
                            # ( preplaceWordLe
                                  # (preplaceWordLe # words # a # secondA)
                                  # b
                                  # secondB
                              )
                            # c
                            # secondC
                        )
                      # d
                      # secondD

{- | Aiken @sigma_word@ — the @index@th entry of the @row@th permutation.

The rows are flattened, so this is one index rather than two list walks. Reading
past the table aborts, which is the @expect Some(word)@ it replaces.
-}
psigmaWord :: forall (s :: S). Term s (PInteger :--> PInteger :--> PInteger)
psigmaWord = phoistAcyclic $
  plam $ \row index -> pindexBS' # psigmaRows # (row * 16 + index)

-- | Aiken @apply_round@ — four column mixes, then four diagonal ones.
papplyRound ::
  forall (s :: S). Term s (PByteString :--> PByteString :--> PInteger :--> PByteString)
papplyRound = phoistAcyclic $
  plam $ \words message row ->
    plet (plam $ \i -> pwordAtLe # message # (psigmaWord # row # i)) $ \msg ->
      pmixAt
        ( pmixAt
            ( pmixAt
                ( pmixAt
                    ( pmixAt
                        ( pmixAt
                            ( pmixAt
                                (pmixAt words 0 4 8 12 (msg # 0) (msg # 1))
                                1
                                5
                                9
                                13
                                (msg # 2)
                                (msg # 3)
                            )
                            2
                            6
                            10
                            14
                            (msg # 4)
                            (msg # 5)
                        )
                        3
                        7
                        11
                        15
                        (msg # 6)
                        (msg # 7)
                    )
                    0
                    5
                    10
                    15
                    (msg # 8)
                    (msg # 9)
                )
                1
                6
                11
                12
                (msg # 10)
                (msg # 11)
            )
            2
            7
            8
            13
            (msg # 12)
            (msg # 13)
        )
        3
        4
        9
        14
        (msg # 14)
        (msg # 15)

pmixAt ::
  forall (s :: S).
  Term s PByteString ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PByteString
pmixAt words a b c d x y = pmix # words # a # b # c # d # x # y

{- | Aiken @fold_chaining_value@.

The two halves of the working value are XORed into the chaining value, word by
word — the step that makes the compression irreversible.
-}
pfoldChainingValue ::
  forall (s :: S). Term s (PByteString :--> PByteString :--> PInteger :--> PByteString)
pfoldChainingValue = phoistAcyclic $
  pfix $ \self -> plam $ \chainingValue words index ->
    pif (index #== 8) (pconstant "") $
      ( pwordLe
          #$ pxor64
          # ( pxor64
                # (pwordAtLe # chainingValue # index)
                # (pwordAtLe # words # index)
            )
          # (pwordAtLe # words # (index + 8))
      )
        <> (self # chainingValue # words # (index + 1))

-- | Aiken @initial_chaining_value_v1@ — the IV with the parameter block folded in.
pinitialChainingValueV1 :: forall (s :: S). Term s PByteString
pinitialChainingValueV1 =
  preplaceWordLe # pivLe # 0 #$ pxor64 # (pwordAtLe # pivLe # 0) # pparameterBlockV1

{- | Aiken @initialize_working_value@.

Sixteen words: the chaining value, then the IV with the byte counter XORed into
words 12 and 13 and — on the final block only — word 14 inverted. That
inversion is the whole of BLAKE2b's finalisation flag, and getting it wrong
would make every message hash like a prefix of itself.
-}
pinitializeWorkingValue ::
  forall (s :: S). Term s (PByteString :--> PInteger :--> PBool :--> PByteString)
pinitializeWorkingValue = phoistAcyclic $
  plam $ \chainingValue bytesCompressed final ->
    plet (chainingValue <> pivLe) $ \initialWords ->
      plet
        ( preplaceWordLe
            # initialWords
            # 12
            #$ pxor64
            # (pwordAtLe # initialWords # 12)
            # (pmod # bytesCompressed # pwordModulus)
        )
        $ \withLowCounter ->
          plet
            ( preplaceWordLe
                # withLowCounter
                # 13
                #$ pxor64
                # (pwordAtLe # withLowCounter # 13)
                # (pquot # bytesCompressed # pwordModulus)
            )
            $ \withCounter ->
              pif
                final
                ( preplaceWordLe
                    # withCounter
                    # 14
                    #$ pxor64
                    # (pwordAtLe # withCounter # 14)
                    # pwordMask
                )
                withCounter

--------------------------------------------------------------------------------
-- Well-formedness
--------------------------------------------------------------------------------

-- | Aiken @expected_active_block_length@ — a full block, or whatever is left.
pexpectedActiveBlockLength ::
  forall (s :: S). Term s (PBlake2b256TraceControlV1 :--> PInteger)
pexpectedActiveBlockLength = phoistAcyclic $
  plam $ \control ->
    pmatch control $ \c ->
      plet (pfromData (pctl'totalLength c) - pfromData (pctl'cursor c)) $ \remaining ->
        pif (remaining #< pblockBytes) remaining pblockBytes

-- | Aiken @active_state_is_empty@ — the within-block fields, all absent.
pactiveStateIsEmpty :: forall (s :: S). Term s (PBlake2b256TraceControlV1 :--> PBool)
pactiveStateIsEmpty = phoistAcyclic $
  plam $ \control ->
    pmatch control $ \c ->
      pand'List
        [ plengthBS # pfromData (pctl'activeBlock c) #== 0
        , pfromData (pctl'activeBlockLength c) #== 0
        , plengthBS # pfromData (pctl'workingValue c) #== 0
        , pfromData (pctl'round c) #== 0
        ]

{- | Aiken @active_block_padding_is_zero@.

The prover supplies the block, and a short final block is padded to 128 bytes.
If the padding were not checked, two traces could absorb the same message bytes
and disagree about the digest.
-}
pactiveBlockPaddingIsZero ::
  forall (s :: S). Term s (PBlake2b256TraceControlV1 :--> PBool)
pactiveBlockPaddingIsZero = phoistAcyclic $
  plam $ \control ->
    pmatch control $ \c ->
      plet (pfromData (pctl'activeBlockLength c)) $ \len ->
        pif (len #== pblockBytes) (pconstant True) $
          plet (pblockBytes - len) $ \paddingLength ->
            (psliceBS # len # paddingLength # pfromData (pctl'activeBlock c))
              #== (preplicateBS # paddingLength # (pintegerToByte # 0))

{- | Aiken @control_is_well_formed@.

Three groups of clauses: the invariants that hold in every stage, the
stage-specific ones, and — the one that anchors the whole trace — that a cursor
of zero implies the /canonical/ initial chaining value. Without that last clause
a prover could start from any chaining value it liked and produce any digest.
-}
pcontrolIsWellFormed :: forall (s :: S). Term s (PBlake2b256TraceControlV1 :--> PBool)
pcontrolIsWellFormed = phoistAcyclic $
  plam $ \control ->
    pmatch control $ \c ->
      plet (pfromData (pctl'stage c)) $ \stage ->
        plet (pfromData (pctl'cursor c)) $ \cursor ->
          plet (pfromData (pctl'totalLength c)) $ \totalLength ->
            pand'List
              [ pfromData (pctl'version c) #== pblake2b256TraceVersion
              , pstageReady #<= stage
              , stage #<= pstageTerminal
              , 0 #<= cursor
              , 0 #< totalLength
              , cursor #<= totalLength
              , plengthBS # pfromData (pctl'chainingValue c) #== 64
              , pif
                  (cursor #== 0)
                  (pfromData (pctl'chainingValue c) #== pinitialChainingValueV1)
                  (pconstant True)
              ]
              #&& pstageIsWellFormed control stage cursor totalLength

pstageIsWellFormed ::
  forall (s :: S).
  Term s PBlake2b256TraceControlV1 ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PBool
pstageIsWellFormed control stage cursor totalLength =
  pmatch control $ \c ->
    pif
      (stage #== pstageReady)
      ( pand'List
          [ cursor #< totalLength
          , prem # cursor # pblockBytes #== 0
          , pactiveStateIsEmpty # control
          ]
      )
      $ pif
        (stage #== pstageTerminal)
        (cursor #== totalLength #&& (pactiveStateIsEmpty # control))
        ( pand'List
            [ cursor #< totalLength
            , prem # cursor # pblockBytes #== 0
            , plengthBS # pfromData (pctl'activeBlock c) #== pblockBytes
            , pfromData (pctl'activeBlockLength c)
                #== (pexpectedActiveBlockLength # control)
            , pactiveBlockPaddingIsZero # control
            , plengthBS # pfromData (pctl'workingValue c) #== 128
            , pif
                (stage #== pstageRound)
                ( 0
                    #<= pfromData (pctl'round c)
                    #&& pfromData (pctl'round c)
                    #< proundCount
                )
                (pfromData (pctl'round c) #== proundCount)
            ]
        )

-- | Aiken @initial_control_v1@ — aborting rather than declining, as the Aiken does.
pinitialControlV1 ::
  forall (s :: S). Term s (PInteger :--> PBlake2b256TraceControlV1)
pinitialControlV1 = phoistAcyclic $
  plam $ \totalLength ->
    plet
      ( pcon $
          PBlake2b256TraceControlV1
            { pctl'version = pdata pblake2b256TraceVersion
            , pctl'stage = pdata pstageReady
            , pctl'cursor = pdata 0
            , pctl'totalLength = pdata totalLength
            , pctl'chainingValue = pdata pinitialChainingValueV1
            , pctl'activeBlock = pdata (pconstant "")
            , pctl'activeBlockLength = pdata 0
            , pctl'workingValue = pdata (pconstant "")
            , pctl'round = pdata 0
            }
      )
      $ \control -> pif (pcontrolIsWellFormed # control) control perror

--------------------------------------------------------------------------------
-- Encoding
--------------------------------------------------------------------------------

{- | Aiken @encode_control_v1@.

Nine items. The three byte-string fields go through @cbor.serialise@ of a
@b_data@, so they carry the chunked encoding Plutus emits above 64 bytes — which
both the 64-byte chaining value and the 128-byte block do.
-}
pencodeControlV1 ::
  forall (s :: S). Term s (PBlake2b256TraceControlV1 :--> PByteString)
pencodeControlV1 = phoistAcyclic $
  plam $ \control ->
    pif (pnot # (pcontrolIsWellFormed # control)) perror $
      pmatch control $ \c ->
        (pencodeDefiniteArrayHeader # 9)
          <> pcborInt pblake2b256TraceVersion
          <> pcborInt (pfromData (pctl'stage c))
          <> pcborInt (pfromData (pctl'cursor c))
          <> pcborInt (pfromData (pctl'totalLength c))
          <> (pserialiseData #$ pforgetData (pctl'chainingValue c))
          <> (pserialiseData #$ pforgetData (pctl'activeBlock c))
          <> pcborInt (pfromData (pctl'activeBlockLength c))
          <> (pserialiseData #$ pforgetData (pctl'workingValue c))
          <> pcborInt (pfromData (pctl'round c))

-- | Aiken @control_from_data_v1@ — a nine-item array, checked as it is read.
pcontrolFromDataV1 ::
  forall (s :: S). Term s (PData :--> PBlake2b256TraceControlV1)
pcontrolFromDataV1 = phoistAcyclic $
  plam $ \d ->
    plet (pasList # d) $ \items ->
      pif (pnot # (plength # items #== 9)) perror $
        plet
          ( pcon $
              PBlake2b256TraceControlV1
                { pctl'version = pdata (pasInt # pitemAt items 0)
                , pctl'stage = pdata (pasInt # pitemAt items 1)
                , pctl'cursor = pdata (pasInt # pitemAt items 2)
                , pctl'totalLength = pdata (pasInt # pitemAt items 3)
                , pctl'chainingValue = pdata (pasByteStr # pitemAt items 4)
                , pctl'activeBlock = pdata (pasByteStr # pitemAt items 5)
                , pctl'activeBlockLength = pdata (pasInt # pitemAt items 6)
                , pctl'workingValue = pdata (pasByteStr # pitemAt items 7)
                , pctl'round = pdata (pasInt # pitemAt items 8)
                }
          )
          $ \control -> pif (pcontrolIsWellFormed # control) control perror

pitemAt :: forall (s :: S). Term s (PBuiltinList PData) -> Term s PInteger -> Term s PData
pitemAt items index = pelemAt # index # items

{- | Aiken @decode_control_v1@.

Decodes, rebuilds, and re-encodes to compare — the same canonicity gate the rest
of the tree uses, and for the same reason: "Aiken.Cbor" reads non-canonical CBOR
deliberately, so the re-encoding is where canonicity is asked.
-}
pdecodeControlV1 ::
  forall (s :: S). Term s (PByteString :--> PBlake2b256TraceControlV1)
pdecodeControlV1 = phoistAcyclic $
  plam $ \controlCbor ->
    pmatch (pdeserialise # controlCbor) $ \case
      PNothing -> perror
      PJust d ->
        plet (pcontrolFromDataV1 # d) $ \control ->
          pif (pencodeControlV1 # control #== controlCbor) control perror

--------------------------------------------------------------------------------
-- Stepping
--------------------------------------------------------------------------------

{- | Aiken @begin_block_v1@.

The block's length must be exactly what the cursor says is left, so a prover
cannot absorb a short block early or a long one late. The finalisation flag is
set when this block reaches the total length — computed here, not supplied.
-}
pbeginBlockV1 ::
  forall (s :: S).
  Term
    s
    ( PBlake2b256TraceControlV1
        :--> PByteString
        :--> PMaybe PBlake2b256TraceControlV1
    )
pbeginBlockV1 = phoistAcyclic $
  plam $ \control block ->
    plet (pexpectedActiveBlockLength # control) $ \expectedLength ->
      pif (pnot # (plengthBS # block #== expectedLength)) (pcon PNothing) $
        pmatch control $ \c ->
          plet (pfromData (pctl'cursor c) + expectedLength) $ \bytesCompressed ->
            pcon $
              PJust $
                pcon
                  c
                    { pctl'stage = pdata pstageRound
                    , pctl'activeBlock =
                        pdata $
                          block
                            <> ( preplicateBS
                                  # (pblockBytes - expectedLength)
                                  # (pintegerToByte # 0)
                               )
                    , pctl'activeBlockLength = pdata expectedLength
                    , pctl'workingValue =
                        pdata $
                          pinitializeWorkingValue
                            # pfromData (pctl'chainingValue c)
                            # bytesCompressed
                            # (bytesCompressed #== pfromData (pctl'totalLength c))
                    , pctl'round = pdata 0
                    }

-- | Aiken @round_v1@ — one of the twelve mixing rounds.
proundV1 ::
  forall (s :: S).
  Term s (PBlake2b256TraceControlV1 :--> PBlake2b256TraceControlV1)
proundV1 = phoistAcyclic $
  plam $ \control ->
    pmatch control $ \c ->
      plet (pfromData (pctl'round c)) $ \round' ->
        plet (round' + 1) $ \nextRound ->
          pcon
            c
              { pctl'stage =
                  pdata (pif (nextRound #== proundCount) pstageFinish pstageRound)
              , pctl'workingValue =
                  pdata $
                    papplyRound
                      # pfromData (pctl'workingValue c)
                      # pfromData (pctl'activeBlock c)
                      # (pmod # round' # 10)
              , pctl'round = pdata nextRound
              }

-- | Aiken @finish_block_v1@ — fold the working value in and clear the block.
pfinishBlockV1 ::
  forall (s :: S).
  Term s (PBlake2b256TraceControlV1 :--> PBlake2b256TraceControlV1)
pfinishBlockV1 = phoistAcyclic $
  plam $ \control ->
    pmatch control $ \c ->
      plet (pfromData (pctl'cursor c) + pfromData (pctl'activeBlockLength c)) $ \cursor ->
        pcon
          c
            { pctl'stage =
                pdata
                  ( pif
                      (cursor #== pfromData (pctl'totalLength c))
                      pstageTerminal
                      pstageReady
                  )
            , pctl'cursor = pdata cursor
            , pctl'chainingValue =
                pdata $
                  pfoldChainingValue
                    # pfromData (pctl'chainingValue c)
                    # pfromData (pctl'workingValue c)
                    # 0
            , pctl'activeBlock = pdata (pconstant "")
            , pctl'activeBlockLength = pdata 0
            , pctl'workingValue = pdata (pconstant "")
            , pctl'round = pdata 0
            }

{- | Aiken @step_v1@.

The block argument is part of the transition: @ready@ demands one, every other
stage refuses one, and @terminal@ has no successor at all. Both the incoming and
the outgoing control are checked, so a step is only ever between two states a
real hash passes through.
-}
pstepV1 ::
  forall (s :: S).
  Term
    s
    ( PBlake2b256TraceControlV1
        :--> PMaybe PByteString
        :--> PMaybe PBlake2b256TraceControlV1
    )
pstepV1 = phoistAcyclic $
  plam $ \control block ->
    pif (pnot # (pcontrolIsWellFormed # control)) (pcon PNothing) $
      pmatch control $ \c ->
        plet (pfromData (pctl'stage c)) $ \stage ->
          plet
            ( pif
                (stage #== pstageReady)
                ( pmatch block $ \case
                    PNothing -> pcon PNothing
                    PJust bytes -> pbeginBlockV1 # control # bytes
                )
                $ pmatch block
                $ \case
                  PJust _ -> pcon PNothing
                  PNothing ->
                    pif (stage #== pstageRound) (pcon (PJust (proundV1 # control))) $
                      pif
                        (stage #== pstageFinish)
                        (pcon (PJust (pfinishBlockV1 # control)))
                        (pcon PNothing)
            )
            $ \result ->
              pmatch result $ \case
                PNothing -> pcon PNothing
                PJust next ->
                  pif
                    (pcontrolIsWellFormed # next)
                    (pcon (PJust next))
                    (pcon PNothing)

{- | Aiken @digest_v1@.

The first 32 bytes of the chaining value, and only at @terminal@. A trace that
has not finished has no digest — not a partial one.
-}
pdigestV1 ::
  forall (s :: S). Term s (PBlake2b256TraceControlV1 :--> PMaybe PByteString)
pdigestV1 = phoistAcyclic $
  plam $ \control ->
    pmatch control $ \c ->
      pif
        ( pcontrolIsWellFormed
            # control
            #&& pfromData (pctl'stage c)
            #== pstageTerminal
        )
        ( pcon $
            PJust $
              psliceBS # 0 # pdigestBytes # pfromData (pctl'chainingValue c)
        )
        (pcon PNothing)
