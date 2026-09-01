{- |
Module      : Midgard.CekSourceBlob
Description : Plutarch port of @lib/midgard/cek-source-blob-v1.ak@.

The whole streaming commitment, end to end: a byte string of any length is
committed by hashing it in 4,095-byte chunks and folding the chunk roots into a
Merkle frontier — and each chunk's hash is itself computed __one BLAKE2b round
at a time__ through "Midgard.Blake2b256Trace", so no step of the whole scheme
needs more than one 128-byte block of the source in hand.

That is what makes the commitment disputable at any size. A proof about a
megabyte blob is a proof about one round of one block of one chunk, and the
state between steps is this control.

=== Two machines, nested

The outer machine has two stages: @active@, which is hashing some chunk, and
@terminal@, which has hashed them all. The inner machine is the BLAKE2b trace,
and it runs to /its/ terminal once per chunk. 'pstepV1' dispatches on the inner
stage: while the trace is unfinished it advances the trace, and when the trace
reaches terminal the next step folds the digest into the frontier and opens a
new trace for the next chunk.

=== The prover is told what to fetch, not asked what it has

'pnextSourceSpanV1' returns the absolute range of the /source/ the next step
needs. The block the trace actually absorbs is that range with the relevant part
of the chunk's hash prefix — the domain string and the CBOR length header —
prepended, and 'pactiveMessageBlock' reconstructs it rather than accepting it.
So a prover can supply bytes but never the framing they are hashed under.

=== An empty source still has one chunk

@expected_chunk_count(0)@ is one, not zero: the empty blob commits to the hash
of the empty chunk. That is what keeps 'pfinalizeV1' total — a frontier with no
peaks has no root at all — and it matches 'Midgard.CekProof.pboundedBlobRootV1',
which hashes the empty string as a single chunk too.
-}
module Midgard.CekSourceBlob (
  -- * Constants
  pcekSourceBlobVersion,
  pstageActive,
  pstageTerminal,

  -- * Types
  PCekSourceBlobControlV1 (..),
  PCekSourceBlobSpanV1 (..),

  -- * Building and checking
  pcontrolIsWellFormed,
  pinitialControlV1,

  -- * Stepping
  pnextSourceSpanV1,
  pstepV1,
  pfinalizeV1,

  -- * Encoding
  pencodeControlV1,
  pcontrolFromDataV1,
  pdecodeControlV1,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP

import Plutarch.Builtin.ByteString (pintegerToByteString, pmostSignificantFirst)
import Plutarch.Core.Utils (pand'List)
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.Prelude

import Aiken.Cbor (pdeserialise)
import Midgard.Blake2b256Trace qualified as Trace
import Midgard.CekBlobFrontier (
  PCekBlobFrontierPeakV1 (..),
  PCekBlobFrontierV1 (..),
  pappendChunkRootV1,
  pcekBlobFrontierVersion,
  pemptyFrontierV1,
  pencodeFrontierV1,
  pfrontierIsWellFormedV1,
 )
import Midgard.CekBlobFrontier qualified as Frontier
import Midgard.CekProof (pmaxBlobChunkBytesV1)
import Midgard.FraudProofs.NativeTx.Codec (
  pcborInt,
  pencodeDefiniteArrayHeader,
  psliceLen,
 )
import Midgard.ValidationMerkle (pmaximumLeafCount)

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

-- | Aiken @version@.
pcekSourceBlobVersion :: forall (s :: S). Term s PInteger
pcekSourceBlobVersion = 1

-- | Aiken @stage_active@ and @stage_terminal@.
pstageActive, pstageTerminal :: forall (s :: S). Term s PInteger
pstageActive = 0
pstageTerminal = 1

-- | The same domain "Midgard.CekProof" hashes a chunk under.
pblobChunkDomain :: forall (s :: S). Term s PByteString
pblobChunkDomain = pconstant "MidgardCekBlobChunkV1"

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

{- | Aiken @CekSourceBlobControlV1@.

@source_start@ is where this blob begins in whatever the prover is reading from,
and is carried only so 'pnextSourceSpanV1' can name absolute positions; nothing
here interprets it.
-}
data PCekSourceBlobControlV1 (s :: S) = PCekSourceBlobControlV1
  { pblob'version :: Term s (PAsData PInteger)
  , pblob'stage :: Term s (PAsData PInteger)
  , pblob'sourceStart :: Term s (PAsData PInteger)
  , pblob'sourceLength :: Term s (PAsData PInteger)
  , pblob'frontier :: Term s (PAsData PCekBlobFrontierV1)
  , pblob'activeHash :: Term s (PAsData (PMaybeData Trace.PBlake2b256TraceControlV1))
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PCekSourceBlobControlV1)

-- | Aiken @CekSourceBlobSpanV1@ — the range of the source the next step needs.
data PCekSourceBlobSpanV1 (s :: S) = PCekSourceBlobSpanV1
  { pspan'absoluteStart :: Term s (PAsData PInteger)
  , pspan'length :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PCekSourceBlobSpanV1)

--------------------------------------------------------------------------------
-- Arithmetic
--------------------------------------------------------------------------------

pminimum :: forall (s :: S). Term s (PInteger :--> PInteger :--> PInteger)
pminimum = phoistAcyclic $ plam $ \first second -> pif (first #< second) first second

{- | Aiken @expected_chunk_count@.

Ceiling division, except that zero gives /one/. The empty blob is committed as
the hash of the empty chunk rather than as an empty tree, which is what keeps
the frontier non-empty and 'pfinalizeV1' total.
-}
pexpectedChunkCount :: forall (s :: S). Term s (PInteger :--> PInteger)
pexpectedChunkCount = phoistAcyclic $
  plam $ \sourceLength ->
    pif (sourceLength #== 0) 1 $
      pquot
        # (sourceLength + pmaxBlobChunkBytesV1 - 1)
        # pmaxBlobChunkBytesV1

-- | Aiken @expected_frontier_byte_length@ — every chunk full but possibly the last.
pexpectedFrontierByteLength ::
  forall (s :: S). Term s (PInteger :--> PInteger :--> PInteger)
pexpectedFrontierByteLength = phoistAcyclic $
  plam $ \sourceLength count ->
    pminimum # sourceLength # (count * pmaxBlobChunkBytesV1)

{- | Aiken @definite_bytes_header@ — the CBOR header a chunk is hashed under.

Written out rather than taken from the codec because the trace hashes the header
/incrementally/, so its length has to be computable before any of it is read.
-}
pdefiniteBytesHeader :: forall (s :: S). Term s (PInteger :--> PByteString)
pdefiniteBytesHeader = phoistAcyclic $
  plam $ \len ->
    pif (pnot # (0 #<= len #&& len #<= pmaxBlobChunkBytesV1)) perror $
      pif (len #< 24) (pbigEndian 1 (64 + len)) $
        pif
          (len #<= 255)
          (pconstant "\x58" <> pbigEndian 1 len)
          (pconstant "\x59" <> pbigEndian 2 len)

pbigEndian :: forall (s :: S). Term s PInteger -> Term s PInteger -> Term s PByteString
pbigEndian width value = pintegerToByteString # pmostSignificantFirst # width # value

-- | Aiken @chunk_prefix@ — the domain and the header, which precede the chunk's bytes.
pchunkPrefix :: forall (s :: S). Term s (PInteger :--> PByteString)
pchunkPrefix = phoistAcyclic $
  plam $ \len -> pblobChunkDomain <> (pdefiniteBytesHeader # len)

-- | Aiken @active_chunk_length@ — how many source bytes the current chunk holds.
pactiveChunkLength :: forall (s :: S). Term s (PCekSourceBlobControlV1 :--> PInteger)
pactiveChunkLength = phoistAcyclic $
  plam $ \control ->
    pmatch control $ \c ->
      plet (pfromData (pblob'sourceLength c)) $ \sourceLength ->
        pif (sourceLength #== 0) 0 $
          pminimum
            # pmaxBlobChunkBytesV1
            # (sourceLength - pfrontierByteLength control)

pfrontierByteLength :: forall (s :: S). Term s PCekSourceBlobControlV1 -> Term s PInteger
pfrontierByteLength control =
  pmatch control $ \c ->
    pmatch (pfromData (pblob'frontier c)) $ \(PCekBlobFrontierV1 _ byteLength _) ->
      pfromData byteLength

pfrontierCount :: forall (s :: S). Term s PCekSourceBlobControlV1 -> Term s PInteger
pfrontierCount control =
  pmatch control $ \c ->
    pmatch (pfromData (pblob'frontier c)) $ \(PCekBlobFrontierV1 count _ _) ->
      pfromData count

{- | Aiken @initial_active_hash@.

The trace's total length is the prefix plus the chunk, because what is hashed is
the /framed/ chunk, not its bytes alone.
-}
pinitialActiveHash ::
  forall (s :: S). Term s (PInteger :--> Trace.PBlake2b256TraceControlV1)
pinitialActiveHash = phoistAcyclic $
  plam $ \chunkLength ->
    Trace.pinitialControlV1 #$ (plengthBS #$ pchunkPrefix # chunkLength) + chunkLength

--------------------------------------------------------------------------------
-- Well-formedness
--------------------------------------------------------------------------------

{- | Aiken @control_is_well_formed@.

The clause that ties the two machines together is the last one: the active
trace's @total_length@ must be exactly the prefix plus /this/ chunk's length, so
a prover cannot hash a chunk of one length and claim it stood for another.
-}
pcontrolIsWellFormed :: forall (s :: S). Term s (PCekSourceBlobControlV1 :--> PBool)
pcontrolIsWellFormed = phoistAcyclic $
  plam $ \control ->
    pmatch control $ \c ->
      plet (pfromData (pblob'sourceLength c)) $ \sourceLength ->
        plet (pexpectedChunkCount # sourceLength) $ \chunkCount ->
          plet (pfrontierCount control) $ \count ->
            pand'List
              [ pfromData (pblob'version c) #== pcekSourceBlobVersion
              , pstageActive #<= pfromData (pblob'stage c)
              , pfromData (pblob'stage c) #<= pstageTerminal
              , 0 #<= pfromData (pblob'sourceStart c)
              , 0 #<= sourceLength
              , chunkCount #<= pmaximumLeafCount
              , pfrontierIsWellFormedV1 # pfromData (pblob'frontier c)
              , count #<= chunkCount
              , pfrontierByteLength control
                  #== (pexpectedFrontierByteLength # sourceLength # count)
              ]
              #&& pstageIsWellFormed control chunkCount count

pstageIsWellFormed ::
  forall (s :: S).
  Term s PCekSourceBlobControlV1 ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PBool
pstageIsWellFormed control chunkCount count =
  pmatch control $ \c ->
    plet (pfromData (pblob'activeHash c)) $ \activeHash ->
      pif
        (pfromData (pblob'stage c) #== pstageTerminal)
        ( count
            #== chunkCount
            #&& pmatch activeHash (\case PDNothing -> pconstant True; PDJust _ -> pconstant False)
        )
        $ pmatch activeHash
        $ \case
          PDNothing -> pconstant False
          PDJust hashControl ->
            plet (pfromData hashControl) $ \hashCtl ->
              plet (pactiveChunkLength # control) $ \chunkLength ->
                pand'List
                  [ count #< chunkCount
                  , 0 #<= chunkLength
                  , Trace.pcontrolIsWellFormed # hashCtl
                  , ptraceTotalLength hashCtl
                      #== (plengthBS #$ pchunkPrefix # chunkLength) + chunkLength
                  ]

ptraceTotalLength ::
  forall (s :: S). Term s Trace.PBlake2b256TraceControlV1 -> Term s PInteger
ptraceTotalLength hashControl =
  pmatch hashControl $ \h -> pfromData (Trace.pctl'totalLength h)

ptraceCursor :: forall (s :: S). Term s Trace.PBlake2b256TraceControlV1 -> Term s PInteger
ptraceCursor hashControl =
  pmatch hashControl $ \h -> pfromData (Trace.pctl'cursor h)

ptraceStage :: forall (s :: S). Term s Trace.PBlake2b256TraceControlV1 -> Term s PInteger
ptraceStage hashControl =
  pmatch hashControl $ \h -> pfromData (Trace.pctl'stage h)

-- | Aiken @initial_control_v1@.
pinitialControlV1 ::
  forall (s :: S). Term s (PInteger :--> PInteger :--> PCekSourceBlobControlV1)
pinitialControlV1 = phoistAcyclic $
  plam $ \sourceStart sourceLength ->
    plet
      ( pcon $
          PCekSourceBlobControlV1
            { pblob'version = pdata pcekSourceBlobVersion
            , pblob'stage = pdata pstageActive
            , pblob'sourceStart = pdata sourceStart
            , pblob'sourceLength = pdata sourceLength
            , pblob'frontier = pdata pemptyFrontierV1
            , pblob'activeHash =
                pdata $
                  pcon $
                    PDJust $
                      pdata $
                        pinitialActiveHash
                          #$ pminimum # sourceLength # pmaxBlobChunkBytesV1
            }
      )
      $ \control -> pif (pcontrolIsWellFormed # control) control perror

--------------------------------------------------------------------------------
-- Encoding
--------------------------------------------------------------------------------

{- | Aiken @encode_optional_active_hash@.

@0xd87a80@ is @None@ — constructor 1, no fields — and @Some@ is constructor 0
with its field in an /indefinite/ array, which is what @serialiseData@ emits for
a constructor with fields.
-}
pencodeOptionalActiveHash ::
  forall (s :: S).
  Term s (PMaybeData Trace.PBlake2b256TraceControlV1 :--> PByteString)
pencodeOptionalActiveHash = phoistAcyclic $
  plam $ \activeHash ->
    pmatch activeHash $ \case
      PDNothing -> pconstant "\xd8\x7a\x80"
      PDJust value ->
        pconstant "\xd8\x79\x9f"
          <> (Trace.pencodeControlV1 # pfromData value)
          <> pconstant "\xff"

-- | Aiken @encode_control_v1@.
pencodeControlV1 :: forall (s :: S). Term s (PCekSourceBlobControlV1 :--> PByteString)
pencodeControlV1 = phoistAcyclic $
  plam $ \control ->
    pif (pnot # (pcontrolIsWellFormed # control)) perror $
      pmatch control $ \c ->
        (pencodeDefiniteArrayHeader # 6)
          <> pcborInt pcekSourceBlobVersion
          <> pcborInt (pfromData (pblob'stage c))
          <> pcborInt (pfromData (pblob'sourceStart c))
          <> pcborInt (pfromData (pblob'sourceLength c))
          <> (pencodeFrontierV1 # pfromData (pblob'frontier c))
          <> (pencodeOptionalActiveHash # pfromData (pblob'activeHash c))

-- | Aiken @peak_from_data@.
ppeakFromData :: forall (s :: S). Term s (PData :--> PCekBlobFrontierPeakV1)
ppeakFromData = phoistAcyclic $
  plam $ \d ->
    plet (pasList # d) $ \items ->
      pif (pnot # (plength # items #== 3)) perror $
        pcon $
          PCekBlobFrontierPeakV1
            (pdata (pasInt # (pelemAt # 0 # items)))
            (pdata (pasByteStr # (pelemAt # 1 # items)))
            (pdata (pasInt # (pelemAt # 2 # items)))

-- | Aiken @peaks_from_data@.
ppeaksFromData ::
  forall (s :: S).
  Term s (PBuiltinList PData :--> PBuiltinList (PAsData PCekBlobFrontierPeakV1))
ppeaksFromData = phoistAcyclic $
  pfix $ \self -> plam $ \items ->
    pelimList
      (\item rest -> pcons # pdata (ppeakFromData # item) # (self # rest))
      (pcon PNil)
      items

-- | Aiken @frontier_from_data@.
pfrontierFromData :: forall (s :: S). Term s (PData :--> PCekBlobFrontierV1)
pfrontierFromData = phoistAcyclic $
  plam $ \d ->
    plet (pasList # d) $ \items ->
      pif (pnot # (plength # items #== 4)) perror $
        pif
          (pnot # (pasInt # (pelemAt # 0 # items) #== pcekBlobFrontierVersion))
          perror
          $ plet
            ( pcon $
                PCekBlobFrontierV1
                  (pdata (pasInt # (pelemAt # 1 # items)))
                  (pdata (pasInt # (pelemAt # 2 # items)))
                  (pdata (ppeaksFromData #$ pasList # (pelemAt # 3 # items)))
            )
          $ \frontier ->
            pif (pfrontierIsWellFormedV1 # frontier) frontier perror

-- | Aiken @optional_active_hash_from_data@.
poptionalActiveHashFromData ::
  forall (s :: S).
  Term s (PData :--> PMaybeData Trace.PBlake2b256TraceControlV1)
poptionalActiveHashFromData = phoistAcyclic $
  plam $ \d ->
    pmatch (pasConstr # d) $ \(PBuiltinPair index fields) ->
      pif
        (index #== 0)
        ( pif (pnot # (plength # fields #== 1)) perror $
            pcon $
              PDJust $
                pdata $
                  Trace.pcontrolFromDataV1 # (pelemAt # 0 # fields)
        )
        $ pif
          (index #== 1 #&& pnull # fields)
          (pcon PDNothing)
          perror

-- | Aiken @control_from_data_v1@.
pcontrolFromDataV1 :: forall (s :: S). Term s (PData :--> PCekSourceBlobControlV1)
pcontrolFromDataV1 = phoistAcyclic $
  plam $ \d ->
    plet (pasList # d) $ \items ->
      pif (pnot # (plength # items #== 6)) perror $
        plet
          ( pcon $
              PCekSourceBlobControlV1
                { pblob'version = pdata (pasInt # (pelemAt # 0 # items))
                , pblob'stage = pdata (pasInt # (pelemAt # 1 # items))
                , pblob'sourceStart = pdata (pasInt # (pelemAt # 2 # items))
                , pblob'sourceLength = pdata (pasInt # (pelemAt # 3 # items))
                , pblob'frontier = pdata (pfrontierFromData # (pelemAt # 4 # items))
                , pblob'activeHash =
                    pdata (poptionalActiveHashFromData # (pelemAt # 5 # items))
                }
          )
          $ \control -> pif (pcontrolIsWellFormed # control) control perror

-- | Aiken @decode_control_v1@ — decode, rebuild, re-encode and compare.
pdecodeControlV1 :: forall (s :: S). Term s (PByteString :--> PCekSourceBlobControlV1)
pdecodeControlV1 = phoistAcyclic $
  plam $ \controlCbor ->
    pmatch (pdeserialise # controlCbor) $ \case
      PNothing -> perror
      PJust d ->
        plet (pcontrolFromDataV1 # d) $ \control ->
          pif (pencodeControlV1 # control #== controlCbor) control perror

--------------------------------------------------------------------------------
-- Spans
--------------------------------------------------------------------------------

-- | Aiken @expected_block_length@ — a full trace block, or what is left of it.
pexpectedBlockLength ::
  forall (s :: S). Term s (Trace.PBlake2b256TraceControlV1 :--> PInteger)
pexpectedBlockLength = phoistAcyclic $
  plam $ \hashControl ->
    pminimum
      # Trace.pblockBytes
      # (ptraceTotalLength hashControl - ptraceCursor hashControl)

{- | Aiken @next_source_span_v1@.

What the next step needs from the source, in absolute positions. The first block
of a chunk is mostly prefix — the domain string is 21 bytes and the header one
to three more — so the span it asks for is that much shorter than a full block.
Only available while both machines are ready for a block.
-}
pnextSourceSpanV1 ::
  forall (s :: S).
  Term s (PCekSourceBlobControlV1 :--> PMaybe PCekSourceBlobSpanV1)
pnextSourceSpanV1 = phoistAcyclic $
  plam $ \control ->
    pif (pnot # (pcontrolIsWellFormed # control)) (pcon PNothing) $
      pmatch control $ \c ->
        pmatch (pfromData (pblob'activeHash c)) $ \case
          PDNothing -> pcon PNothing
          PDJust hashControlData ->
            plet (pfromData hashControlData) $ \hashControl ->
              pif
                ( pnot
                    #$ pfromData (pblob'stage c)
                    #== pstageActive
                    #&& ptraceStage hashControl
                    #== Trace.pstageReady
                )
                (pcon PNothing)
                $ pspanOf control hashControl

pspanOf ::
  forall (s :: S).
  Term s PCekSourceBlobControlV1 ->
  Term s Trace.PBlake2b256TraceControlV1 ->
  Term s (PMaybe PCekSourceBlobSpanV1)
pspanOf control hashControl =
  pmatch control $ \c ->
    plet (plengthBS #$ pchunkPrefix #$ pactiveChunkLength # control) $ \prefixLength ->
      plet (pexpectedBlockLength # hashControl) $ \blockLength ->
        plet (ptraceCursor hashControl) $ \cursor ->
          plet (pminimum # cursor # prefixLength) $ \prefixStart ->
            plet (pminimum # (cursor + blockLength) # prefixLength) $ \prefixEnd ->
              pcon $
                PJust $
                  pcon $
                    PCekSourceBlobSpanV1
                      { pspan'absoluteStart =
                          pdata $
                            pfromData (pblob'sourceStart c)
                              + pfrontierByteLength control
                              + pif (prefixLength #< cursor) (cursor - prefixLength) 0
                      , pspan'length = pdata (blockLength - (prefixEnd - prefixStart))
                      }

{- | Aiken @active_message_block@.

The block the trace absorbs: whatever part of the chunk prefix falls in this
block, then the source bytes. The prefix is /reconstructed/, never supplied, so
a prover can choose the bytes but not the framing they are hashed under.
-}
pactiveMessageBlock ::
  forall (s :: S).
  Term s (PCekSourceBlobControlV1 :--> PByteString :--> PMaybe PByteString)
pactiveMessageBlock = phoistAcyclic $
  plam $ \control sourceBytes ->
    pmatch control $ \c ->
      pmatch (pfromData (pblob'activeHash c)) $ \case
        PDNothing -> perror
        PDJust hashControlData ->
          plet (pfromData hashControlData) $ \hashControl ->
            pmatch (pnextSourceSpanV1 # control) $ \case
              PNothing -> perror
              PJust span ->
                pmatch span $ \(PCekSourceBlobSpanV1 _ spanLength) ->
                  pif
                    (pnot # (plengthBS # sourceBytes #== pfromData spanLength))
                    (pcon PNothing)
                    $ plet (pchunkPrefix #$ pactiveChunkLength # control)
                    $ \prefix ->
                      plet (pexpectedBlockLength # hashControl) $ \blockLength ->
                        plet (plengthBS # prefix) $ \prefixLength ->
                          plet (ptraceCursor hashControl) $ \cursor ->
                            plet (pminimum # cursor # prefixLength) $ \prefixStart ->
                              plet
                                (pminimum # (cursor + blockLength) # prefixLength)
                                $ \prefixEnd ->
                                  pcon $
                                    PJust $
                                      (psliceLen # prefix # prefixStart # (prefixEnd - prefixStart))
                                        <> sourceBytes

--------------------------------------------------------------------------------
-- Stepping
--------------------------------------------------------------------------------

{- | Aiken @finish_active_chunk@.

The trace's digest becomes a chunk root, the frontier takes it, and — unless
that was the last chunk — a fresh trace opens for the next one, sized to
whatever is left.
-}
pfinishActiveChunk ::
  forall (s :: S).
  Term s (PCekSourceBlobControlV1 :--> PMaybe PCekSourceBlobControlV1)
pfinishActiveChunk = phoistAcyclic $
  plam $ \control ->
    pmatch control $ \c ->
      pmatch (pfromData (pblob'activeHash c)) $ \case
        PDNothing -> perror
        PDJust hashControlData ->
          pmatch (Trace.pdigestV1 # pfromData hashControlData) $ \case
            PNothing -> pcon PNothing
            PJust root ->
              pmatch
                ( pappendChunkRootV1
                    # pfromData (pblob'frontier c)
                    # root
                    # (pactiveChunkLength # control)
                )
                $ \case
                  PNothing -> pcon PNothing
                  PJust frontier -> pafterChunk control frontier

pafterChunk ::
  forall (s :: S).
  Term s PCekSourceBlobControlV1 ->
  Term s PCekBlobFrontierV1 ->
  Term s (PMaybe PCekSourceBlobControlV1)
pafterChunk control frontier =
  pmatch control $ \c ->
    plet (pfromData (pblob'sourceLength c)) $ \sourceLength ->
      pmatch frontier $ \(PCekBlobFrontierV1 count byteLength _) ->
        plet (pfromData count #== (pexpectedChunkCount # sourceLength)) $ \terminal ->
          plet
            ( pcon
                c
                  { pblob'stage = pdata (pif terminal pstageTerminal pstageActive)
                  , pblob'frontier = pdata frontier
                  , pblob'activeHash =
                      pdata $
                        pif
                          terminal
                          (pcon PDNothing)
                          ( pcon $
                              PDJust $
                                pdata $
                                  pinitialActiveHash
                                    #$ pminimum
                                    # pmaxBlobChunkBytesV1
                                    # (sourceLength - pfromData byteLength)
                          )
                  }
            )
            $ \next ->
              pif
                (pcontrolIsWellFormed # next)
                (pcon (PJust next))
                (pcon PNothing)

-- | Aiken @advance_active_hash@ — one step of the inner machine.
padvanceActiveHash ::
  forall (s :: S).
  Term
    s
    ( PCekSourceBlobControlV1
        :--> PMaybe PByteString
        :--> PMaybe PCekSourceBlobControlV1
    )
padvanceActiveHash = phoistAcyclic $
  plam $ \control sourceBytes ->
    pmatch control $ \c ->
      pmatch (pfromData (pblob'activeHash c)) $ \case
        PDNothing -> perror
        PDJust hashControlData ->
          plet (pfromData hashControlData) $ \hashControl ->
            pmatch (pnextHash control hashControl sourceBytes) $ \case
              PNothing -> pcon PNothing
              PJust hash ->
                plet
                  (pcon c {pblob'activeHash = pdata (pcon (PDJust (pdata hash)))})
                  $ \next ->
                    pif
                      (pcontrolIsWellFormed # next)
                      (pcon (PJust next))
                      (pcon PNothing)

pnextHash ::
  forall (s :: S).
  Term s PCekSourceBlobControlV1 ->
  Term s Trace.PBlake2b256TraceControlV1 ->
  Term s (PMaybe PByteString) ->
  Term s (PMaybe Trace.PBlake2b256TraceControlV1)
pnextHash control hashControl sourceBytes =
  pif
    (ptraceStage hashControl #== Trace.pstageReady)
    ( pmatch sourceBytes $ \case
        PNothing -> pcon PNothing
        PJust bytes ->
          pmatch (pactiveMessageBlock # control # bytes) $ \case
            PNothing -> pcon PNothing
            PJust block -> Trace.pstepV1 # hashControl # pcon (PJust block)
    )
    $ pmatch sourceBytes
    $ \case
      PJust _ -> pcon PNothing
      PNothing -> Trace.pstepV1 # hashControl # pcon PNothing

{- | Aiken @step_v1@.

One step of the outer machine, which is one step of the inner one — except when
the inner machine has finished, and then it is the fold that closes a chunk. The
source bytes belong to exactly one of those: a finished trace takes none.
-}
pstepV1 ::
  forall (s :: S).
  Term
    s
    ( PCekSourceBlobControlV1
        :--> PMaybe PByteString
        :--> PMaybe PCekSourceBlobControlV1
    )
pstepV1 = phoistAcyclic $
  plam $ \control sourceBytes ->
    pif
      ( pnot
          #$ pcontrolIsWellFormed
          # control
          #&& pblobStage control
          #== pstageActive
      )
      (pcon PNothing)
      $ pmatch control
      $ \c ->
        pmatch (pfromData (pblob'activeHash c)) $ \case
          PDNothing -> perror
          PDJust hashControlData ->
            pif
              (ptraceStage (pfromData hashControlData) #== Trace.pstageTerminal)
              ( pmatch sourceBytes $ \case
                  PJust _ -> pcon PNothing
                  PNothing -> pfinishActiveChunk # control
              )
              (padvanceActiveHash # control # sourceBytes)

pblobStage :: forall (s :: S). Term s PCekSourceBlobControlV1 -> Term s PInteger
pblobStage control = pmatch control $ \c -> pfromData (pblob'stage c)

{- | Aiken @finalize_v1@.

The frontier's root, once every chunk is in. This is the same value
'Midgard.CekBlobFrontier.prootFromChunksV1' produces from the chunks directly,
and — for a blob of at most three chunks — the same value
'Midgard.CekProof.pboundedBlobRootV1' produces without a frontier at all.
-}
pfinalizeV1 ::
  forall (s :: S). Term s (PCekSourceBlobControlV1 :--> PMaybe PByteString)
pfinalizeV1 = phoistAcyclic $
  plam $ \control ->
    pmatch control $ \c ->
      pif
        ( pcontrolIsWellFormed
            # control
            #&& pfromData (pblob'stage c)
            #== pstageTerminal
        )
        (Frontier.pfinalizeV1 # pfromData (pblob'frontier c))
        (pcon PNothing)
