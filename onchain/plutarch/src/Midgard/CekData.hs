{- |
Module      : Midgard.CekData
Description : Plutarch port of @lib/midgard/cek-data-v1.ak@.

The authenticated @Data@ node format: a Merkle-shaped commitment to a Plutus
@Data@ value in which every node carries not only its children's roots but the
two numbers a CEK replay needs about it — its __CBOR length__ and its
__ExMemory__ — so a fault proof can charge a step without materialising the value
it operates on.

That is the whole point of the format. A machine step that pushes a @Data@ onto
the stack is charged by the value's memory, and one that serialises it by its
length; a proof that had to reveal the value to establish either would be bounded
by the value's size rather than by the step's. Here both travel with the root, so
a step is proved against three 32-byte hashes and a handful of integers however
large the datum is.

=== Three node families, three domains

Nodes ('PDataNodeV1'), list links ('PDataListNodeV1') and map links
('PDataPairNodeV1') hash under separate domain tags, so a preimage of one shape
can never be read as another. The empty list and the empty map are the hashes of
a bare @0x80@ under their own domains, which is what lets a length-zero sequence
have a root at all.

=== Every inspector re-encodes

'pinspectDataNodePreimageV1' and its two siblings decode a preimage, range-check
every field, rebuild the node, and then __re-encode it and compare against the
bytes they were given__. The decode alone would accept indefinite-length CBOR,
non-minimal integer headers, reordered map keys and trailing bytes — all of which
"Aiken.Cbor" reads, deliberately, because canonicity is a separate question. The
re-encoding is where it is asked and answered.

=== The accessors read tags, not patterns

'pdataNodeCborLengthV1', 'pdataNodeMemoryV1' and 'pdataNodeChildRootsV1' would
each be a six-armed @pmatch@ whose arms are largely identical — the exact shape
that triggers Plutarch's branch-selection hazard, where no arm is selected and a
valid node is silently rejected. They read the constructor tag with 'pconstrOf'
and index the field vector instead. The field /positions/ are therefore
load-bearing and are written next to each accessor.
-}
module Midgard.CekData (
  -- * The node types
  PDataNodeV1 (..),
  PDataListNodeV1 (..),
  PDataPairNodeV1 (..),
  PDataSummaryV1 (..),
  PDataSequenceSummaryV1 (..),

  -- * Encoding and hashing
  pencodeDataNodePreimageV1,
  phashDataNodeV1,
  pencodeDataListNodePreimageV1,
  phashDataListNodeV1,
  pencodeDataPairNodePreimageV1,
  phashDataPairNodeV1,
  pemptyDataListRootV1,
  pemptyDataPairRootV1,

  -- * Inspection
  pinspectDataNodePreimageV1,
  pinspectDataListNodePreimageV1,
  pinspectDataPairNodePreimageV1,

  -- * Accessors
  pdataNodeCborLengthV1,
  pdataNodeMemoryV1,
  pdataNodeChildRootsV1,
  pdataListNodeChildRootsV1,
  pdataPairNodeChildRootsV1,
  pbytesDataCborLengthV1,

  -- * Verification
  pverifyDataNodeV1,
  pverifyDataListLinkV1,
  pverifyDataPairLinkV1,

  -- * Summaries
  pemptyDataListSummaryV1,
  pprependDataListSummaryV1,
  pemptyDataPairSummaryV1,
  pprependDataPairSummaryV1,
  psmallConstrDataSummaryV1,
  plargeConstrDataSummaryV1,
  plargeConstrDataSummaryFromCborV1,
  plistDataSummaryV1,
  pmapDataSummaryV1,
  pintegerDataSummaryV1,
  pbytesDataSummaryV1,
  psemanticDataSummaryV1,
  phashSemanticDataV1,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP

import Plutarch.Builtin.Crypto (pblake2b_256)
import Plutarch.Core.Utils (pand'List)
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.Prelude

import Aiken.Cbor (pdeserialise)
import Midgard.Common.Utils (pconstrOf)
import Midgard.FraudProofs.NativeTx.Codec (
  pcborInt,
  pencodeDefiniteArrayHeader,
  pencodeDefiniteBytes,
  psliceLen,
 )

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

pdataNodeDomain, pdataListNodeDomain, pdataPairNodeDomain :: forall (s :: S). Term s PByteString
pdataNodeDomain = pconstant "MidgardCekDataNodeV1"
pdataListNodeDomain = pconstant "MidgardCekDataListNodeV1"
pdataPairNodeDomain = pconstant "MidgardCekDataPairNodeV1"

pblobChunkDomain, pblobBranchDomain :: forall (s :: S). Term s PByteString
pblobChunkDomain = pconstant "MidgardCekBlobChunkV1"
pblobBranchDomain = pconstant "MidgardCekBlobBranchV1"

puint32Max, puint64Max :: forall (s :: S). Term s PInteger
puint32Max = 4294967295
puint64Max = 18446744073709551615

pmaxBlobChunkBytes, pmaxBoundedBlobBytes :: forall (s :: S). Term s PInteger
pmaxBlobChunkBytes = 4095
pmaxBoundedBlobBytes = 9215

--------------------------------------------------------------------------------
-- The node types
--------------------------------------------------------------------------------

{- | Aiken @DataNodeV1@ — one authenticated @Data@ node.

The two constructor forms exist because a constructor index above 127 does not
fit the compact tag encoding, so it travels as its own committed CBOR blob rather
than as an integer field. Everything else is a count, a root, and the two
replay numbers.
-}
data PDataNodeV1 (s :: S)
  = PConstrSmallData
      { pnode'constructor :: Term s (PAsData PInteger)
      , pnode'fieldsCount :: Term s (PAsData PInteger)
      , pnode'fieldsRoot :: Term s (PAsData PByteString)
      , pnode'cborLength :: Term s (PAsData PInteger)
      , pnode'memory :: Term s (PAsData PInteger)
      }
  | PConstrLargeData
      { pnode'constructorCborRoot :: Term s (PAsData PByteString)
      , pnode'constructorCborLength :: Term s (PAsData PInteger)
      , pnode'constructorMemory :: Term s (PAsData PInteger)
      , pnode'fieldsCount :: Term s (PAsData PInteger)
      , pnode'fieldsRoot :: Term s (PAsData PByteString)
      , pnode'cborLength :: Term s (PAsData PInteger)
      , pnode'memory :: Term s (PAsData PInteger)
      }
  | PMapDataNode
      { pnode'entriesCount :: Term s (PAsData PInteger)
      , pnode'entriesRoot :: Term s (PAsData PByteString)
      , pnode'cborLength :: Term s (PAsData PInteger)
      , pnode'memory :: Term s (PAsData PInteger)
      }
  | PListDataNode
      { pnode'itemsCount :: Term s (PAsData PInteger)
      , pnode'itemsRoot :: Term s (PAsData PByteString)
      , pnode'cborLength :: Term s (PAsData PInteger)
      , pnode'memory :: Term s (PAsData PInteger)
      }
  | PIntegerDataNode
      { pnode'cborRoot :: Term s (PAsData PByteString)
      , pnode'cborLength :: Term s (PAsData PInteger)
      , pnode'memory :: Term s (PAsData PInteger)
      }
  | PBytesDataNode
      { pnode'bytesRoot :: Term s (PAsData PByteString)
      , pnode'bytesLength :: Term s (PAsData PInteger)
      , pnode'cborLength :: Term s (PAsData PInteger)
      , pnode'memory :: Term s (PAsData PInteger)
      }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PDataNodeV1)

-- | Aiken @DataListNodeV1@ — one link of an authenticated item sequence.
data PDataListNodeV1 (s :: S) = PDataListNodeV1
  { plistNode'head :: Term s (PAsData PByteString)
  , plistNode'headCborLength :: Term s (PAsData PInteger)
  , plistNode'headMemory :: Term s (PAsData PInteger)
  , plistNode'tail :: Term s (PAsData PByteString)
  , plistNode'length :: Term s (PAsData PInteger)
  , plistNode'payloadCborLength :: Term s (PAsData PInteger)
  , plistNode'memory :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PDataListNodeV1)

-- | Aiken @DataPairNodeV1@ — one link of an authenticated key/value sequence.
data PDataPairNodeV1 (s :: S) = PDataPairNodeV1
  { ppairNode'key :: Term s (PAsData PByteString)
  , ppairNode'keyCborLength :: Term s (PAsData PInteger)
  , ppairNode'keyMemory :: Term s (PAsData PInteger)
  , ppairNode'value :: Term s (PAsData PByteString)
  , ppairNode'valueCborLength :: Term s (PAsData PInteger)
  , ppairNode'valueMemory :: Term s (PAsData PInteger)
  , ppairNode'tail :: Term s (PAsData PByteString)
  , ppairNode'length :: Term s (PAsData PInteger)
  , ppairNode'payloadCborLength :: Term s (PAsData PInteger)
  , ppairNode'memory :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PDataPairNodeV1)

-- | Aiken @DataSummaryV1@ — a node's root and its two replay numbers.
data PDataSummaryV1 (s :: S) = PDataSummaryV1
  { psummary'root :: Term s (PAsData PByteString)
  , psummary'cborLength :: Term s (PAsData PInteger)
  , psummary'memory :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PDataSummaryV1)

-- | Aiken @DataSequenceSummaryV1@ — the same for a sequence, plus its length.
data PDataSequenceSummaryV1 (s :: S) = PDataSequenceSummaryV1
  { pseq'root :: Term s (PAsData PByteString)
  , pseq'length :: Term s (PAsData PInteger)
  , pseq'payloadCborLength :: Term s (PAsData PInteger)
  , pseq'memory :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PDataSequenceSummaryV1)

--------------------------------------------------------------------------------
-- Range assertions
--------------------------------------------------------------------------------

-- | Aiken @expect_hash@ — 32 bytes, aborting otherwise.
pexpectHash :: forall (s :: S). Term s (PByteString :--> PByteString)
pexpectHash = phoistAcyclic $
  plam $ \value -> pif (plengthBS # value #== 32) value perror

-- | Aiken @expect_uint32@.
pexpectUint32 :: forall (s :: S). Term s (PInteger :--> PInteger)
pexpectUint32 = phoistAcyclic $
  plam $ \value -> pif (0 #<= value #&& value #<= puint32Max) value perror

-- | Aiken @expect_uint64@.
pexpectUint64 :: forall (s :: S). Term s (PInteger :--> PInteger)
pexpectUint64 = phoistAcyclic $
  plam $ \value -> pif (0 #<= value #&& value #<= puint64Max) value perror

-- | Aiken @uint32@ — the same range as a predicate rather than an assertion.
puint32 :: forall (s :: S). Term s (PInteger :--> PBool)
puint32 = phoistAcyclic $ plam $ \value -> 0 #<= value #&& value #<= puint32Max

-- | Aiken @uint64@.
puint64 :: forall (s :: S). Term s (PInteger :--> PBool)
puint64 = phoistAcyclic $ plam $ \value -> 0 #<= value #&& value #<= puint64Max

--------------------------------------------------------------------------------
-- The bounded blob root
--------------------------------------------------------------------------------

-- | Aiken @hash_blob_chunk_v1@.
phashBlobChunkV1 :: forall (s :: S). Term s (PByteString :--> PByteString)
phashBlobChunkV1 = phoistAcyclic $
  plam $ \chunk ->
    pif (plengthBS # chunk #<= pmaxBlobChunkBytes) `flip` perror $
      pblake2b_256 #$ pblobChunkDomain <> (pencodeDefiniteBytes # chunk)

-- | Aiken @hash_blob_branch_v1@.
phashBlobBranchV1 ::
  forall (s :: S).
  Term s (PByteString :--> PByteString :--> PInteger :--> PByteString)
phashBlobBranchV1 = phoistAcyclic $
  plam $ \left right byteLength ->
    pblake2b_256
      #$ pblobBranchDomain
      <> ( pconstant "\x83"
            <> (pencodeDefiniteBytes #$ pexpectHash # left)
            <> (pencodeDefiniteBytes #$ pexpectHash # right)
            <> pcborInt (pexpectUint64 # byteLength)
         )

{- | Aiken @bounded_blob_root_v1@.

At most three chunks, folded left-heavy: one chunk is its own root, two are a
branch, and three are a branch of that branch with the tail. The shape is fixed
rather than general because the bound is — 9,215 bytes is three 4,095-byte chunks
minus nothing, and a fourth would need a different tree.
-}
pboundedBlobRootV1 :: forall (s :: S). Term s (PByteString :--> PByteString)
pboundedBlobRootV1 = phoistAcyclic $
  plam $ \bytes ->
    plet (plengthBS # bytes) $ \len ->
      pif (len #<= pmaxBoundedBlobBytes) `flip` perror $
        pif (len #<= pmaxBlobChunkBytes) (phashBlobChunkV1 # bytes) $
          plet (psliceLen # bytes # 0 # pmaxBlobChunkBytes) $ \first ->
            plet (len - pmaxBlobChunkBytes) $ \remaining ->
              plet
                ( pif
                    (remaining #<= pmaxBlobChunkBytes)
                    remaining
                    pmaxBlobChunkBytes
                )
                $ \secondLength ->
                  plet (psliceLen # bytes # pmaxBlobChunkBytes # secondLength) $ \second ->
                    plet
                      ( phashBlobBranchV1
                          # (phashBlobChunkV1 # first)
                          # (phashBlobChunkV1 # second)
                          # (pmaxBlobChunkBytes + secondLength)
                      )
                      $ \left ->
                        pif (remaining #<= pmaxBlobChunkBytes) left $
                          plet
                            ( psliceLen
                                # bytes
                                # (pmaxBlobChunkBytes + secondLength)
                                # (remaining - secondLength)
                            )
                            $ \third ->
                              phashBlobBranchV1 # left # (phashBlobChunkV1 # third) # len

--------------------------------------------------------------------------------
-- Encoding
--------------------------------------------------------------------------------

-- | The domain-tagged hash of a node preimage.
phashUnder ::
  forall (s :: S). Term s PByteString -> Term s PByteString -> Term s PByteString
phashUnder domain preimage = pblake2b_256 # (domain <> preimage)

{- | Aiken @encode_data_node_preimage_v1@.

Six shapes, distinguished by a leading kind integer and by their arity: 6 fields
for a small constructor, 8 for a large one, 5 for a map, a list or a byte string,
and 4 for an integer. The arity is what the inspector dispatches on, so it is
part of the format rather than an artefact of it.

Note the one asymmetry, faithfully kept: @IntegerData@ and @BytesData@ bound
@cbor_length@ to 32 bits where every other shape allows 64.
-}
pencodeDataNodePreimageV1 :: forall (s :: S). Term s (PDataNodeV1 :--> PByteString)
pencodeDataNodePreimageV1 = phoistAcyclic $
  plam $ \node ->
    pmatch node $ \case
      PConstrSmallData
        { pnode'constructor
        , pnode'fieldsCount
        , pnode'fieldsRoot
        , pnode'cborLength
        , pnode'memory
        } ->
          plet (pfromData pnode'constructor) $ \constructor ->
            pif (0 #<= constructor #&& constructor #<= 127) `flip` perror $
              (pencodeDefiniteArrayHeader # 6)
                <> pcborInt 0
                <> pcborInt constructor
                <> pcborInt (pexpectUint32 # pfromData pnode'fieldsCount)
                <> (pencodeDefiniteBytes #$ pexpectHash # pfromData pnode'fieldsRoot)
                <> pcborInt (pexpectUint64 # pfromData pnode'cborLength)
                <> pcborInt (pexpectUint64 # pfromData pnode'memory)
      PConstrLargeData
        { pnode'constructorCborRoot
        , pnode'constructorCborLength
        , pnode'constructorMemory
        , pnode'fieldsCount
        , pnode'fieldsRoot
        , pnode'cborLength
        , pnode'memory
        } ->
          (pencodeDefiniteArrayHeader # 8)
            <> pcborInt 1
            <> (pencodeDefiniteBytes #$ pexpectHash # pfromData pnode'constructorCborRoot)
            <> pcborInt (pexpectUint32 # pfromData pnode'constructorCborLength)
            <> pcborInt (pexpectUint64 # pfromData pnode'constructorMemory)
            <> pcborInt (pexpectUint32 # pfromData pnode'fieldsCount)
            <> (pencodeDefiniteBytes #$ pexpectHash # pfromData pnode'fieldsRoot)
            <> pcborInt (pexpectUint64 # pfromData pnode'cborLength)
            <> pcborInt (pexpectUint64 # pfromData pnode'memory)
      PMapDataNode {pnode'entriesCount, pnode'entriesRoot, pnode'cborLength, pnode'memory} ->
        pcountedNode 2 (pfromData pnode'entriesCount) (pfromData pnode'entriesRoot) $
          pcborInt (pexpectUint64 # pfromData pnode'cborLength)
            <> pcborInt (pexpectUint64 # pfromData pnode'memory)
      PListDataNode {pnode'itemsCount, pnode'itemsRoot, pnode'cborLength, pnode'memory} ->
        pcountedNode 3 (pfromData pnode'itemsCount) (pfromData pnode'itemsRoot) $
          pcborInt (pexpectUint64 # pfromData pnode'cborLength)
            <> pcborInt (pexpectUint64 # pfromData pnode'memory)
      PIntegerDataNode {pnode'cborRoot, pnode'cborLength, pnode'memory} ->
        (pencodeDefiniteArrayHeader # 4)
          <> pcborInt 4
          <> (pencodeDefiniteBytes #$ pexpectHash # pfromData pnode'cborRoot)
          <> pcborInt (pexpectUint32 # pfromData pnode'cborLength)
          <> pcborInt (pexpectUint64 # pfromData pnode'memory)
      PBytesDataNode {pnode'bytesRoot, pnode'bytesLength, pnode'cborLength, pnode'memory} ->
        (pencodeDefiniteArrayHeader # 5)
          <> pcborInt 5
          <> (pencodeDefiniteBytes #$ pexpectHash # pfromData pnode'bytesRoot)
          <> pcborInt (pexpectUint32 # pfromData pnode'bytesLength)
          <> pcborInt (pexpectUint32 # pfromData pnode'cborLength)
          <> pcborInt (pexpectUint64 # pfromData pnode'memory)

{- | The shape shared by the map and list nodes: kind, count, root, then a tail.

Written once because the two encodings are byte-identical but for the kind, and a
second copy would be a place for them to drift apart. The @BytesData@ node has
the same arity but a different field order — root before length — so it is not
folded in here.
-}
pcountedNode ::
  forall (s :: S).
  Term s PInteger ->
  Term s PInteger ->
  Term s PByteString ->
  Term s PByteString ->
  Term s PByteString
pcountedNode kind count root rest =
  (pencodeDefiniteArrayHeader # 5)
    <> pcborInt kind
    <> pcborInt (pexpectUint32 # count)
    <> (pencodeDefiniteBytes #$ pexpectHash # root)
    <> rest

-- | Aiken @hash_data_node_v1@.
phashDataNodeV1 :: forall (s :: S). Term s (PDataNodeV1 :--> PByteString)
phashDataNodeV1 = phoistAcyclic $
  plam $ \node -> phashUnder pdataNodeDomain (pencodeDataNodePreimageV1 # node)

-- | Aiken @encode_data_list_node_preimage_v1@.
pencodeDataListNodePreimageV1 :: forall (s :: S). Term s (PDataListNodeV1 :--> PByteString)
pencodeDataListNodePreimageV1 = phoistAcyclic $
  plam $ \node ->
    pmatch node $
      \PDataListNodeV1
        { plistNode'head
        , plistNode'headCborLength
        , plistNode'headMemory
        , plistNode'tail
        , plistNode'length
        , plistNode'payloadCborLength
        , plistNode'memory
        } ->
          (pencodeDefiniteArrayHeader # 7)
            <> (pencodeDefiniteBytes #$ pexpectHash # pfromData plistNode'head)
            <> pcborInt (pexpectUint32 # pfromData plistNode'headCborLength)
            <> pcborInt (pexpectUint64 # pfromData plistNode'headMemory)
            <> (pencodeDefiniteBytes #$ pexpectHash # pfromData plistNode'tail)
            <> pcborInt (pexpectUint32 # pfromData plistNode'length)
            <> pcborInt (pexpectUint64 # pfromData plistNode'payloadCborLength)
            <> pcborInt (pexpectUint64 # pfromData plistNode'memory)

-- | Aiken @hash_data_list_node_v1@.
phashDataListNodeV1 :: forall (s :: S). Term s (PDataListNodeV1 :--> PByteString)
phashDataListNodeV1 = phoistAcyclic $
  plam $ \node ->
    phashUnder pdataListNodeDomain (pencodeDataListNodePreimageV1 # node)

-- | Aiken @encode_data_pair_node_preimage_v1@.
pencodeDataPairNodePreimageV1 :: forall (s :: S). Term s (PDataPairNodeV1 :--> PByteString)
pencodeDataPairNodePreimageV1 = phoistAcyclic $
  plam $ \node ->
    pmatch node $
      \PDataPairNodeV1
        { ppairNode'key
        , ppairNode'keyCborLength
        , ppairNode'keyMemory
        , ppairNode'value
        , ppairNode'valueCborLength
        , ppairNode'valueMemory
        , ppairNode'tail
        , ppairNode'length
        , ppairNode'payloadCborLength
        , ppairNode'memory
        } ->
          (pencodeDefiniteArrayHeader # 10)
            <> (pencodeDefiniteBytes #$ pexpectHash # pfromData ppairNode'key)
            <> pcborInt (pexpectUint32 # pfromData ppairNode'keyCborLength)
            <> pcborInt (pexpectUint64 # pfromData ppairNode'keyMemory)
            <> (pencodeDefiniteBytes #$ pexpectHash # pfromData ppairNode'value)
            <> pcborInt (pexpectUint32 # pfromData ppairNode'valueCborLength)
            <> pcborInt (pexpectUint64 # pfromData ppairNode'valueMemory)
            <> (pencodeDefiniteBytes #$ pexpectHash # pfromData ppairNode'tail)
            <> pcborInt (pexpectUint32 # pfromData ppairNode'length)
            <> pcborInt (pexpectUint64 # pfromData ppairNode'payloadCborLength)
            <> pcborInt (pexpectUint64 # pfromData ppairNode'memory)

-- | Aiken @hash_data_pair_node_v1@.
phashDataPairNodeV1 :: forall (s :: S). Term s (PDataPairNodeV1 :--> PByteString)
phashDataPairNodeV1 = phoistAcyclic $
  plam $ \node ->
    phashUnder pdataPairNodeDomain (pencodeDataPairNodePreimageV1 # node)

-- | Aiken @empty_data_list_root_v1@ — the hash of a bare empty array.
pemptyDataListRootV1 :: forall (s :: S). Term s PByteString
pemptyDataListRootV1 = phashUnder pdataListNodeDomain (pconstant "\x80")

-- | Aiken @empty_data_pair_root_v1@.
pemptyDataPairRootV1 :: forall (s :: S). Term s PByteString
pemptyDataPairRootV1 = phashUnder pdataPairNodeDomain (pconstant "\x80")

--------------------------------------------------------------------------------
-- Accessors
--------------------------------------------------------------------------------

{- | Aiken @data_node_cbor_length_v1@.

@cbor_length@ sits at a different position in each shape — 3, 5, 2, 2, 1, 2 for
the six kinds in tag order — so this is a tag read and an index rather than a
@pmatch@. See the module header for why that matters here specifically.
-}
pdataNodeCborLengthV1 :: forall (s :: S). Term s (PDataNodeV1 :--> PInteger)
pdataNodeCborLengthV1 = phoistAcyclic $
  plam $ \node ->
    let (tag, fields) = pconstrOf (pdata node)
     in plet tag $ \kind ->
          pasInt
            #$ pfieldAt fields
            $ pif (kind #== 0) 3 $
              pif (kind #== 1) 5 $
                pif (kind #== 4) 1 2

{- | Aiken @data_node_memory_v1@.

@memory@ is the last field of every shape, so this is the only accessor that
could have been written as a single pattern — and it still is not, because the
arity differs and "the last one" is not a position.
-}
pdataNodeMemoryV1 :: forall (s :: S). Term s (PDataNodeV1 :--> PInteger)
pdataNodeMemoryV1 = phoistAcyclic $
  plam $ \node ->
    let (tag, fields) = pconstrOf (pdata node)
     in plet tag $ \kind ->
          pasInt
            #$ pfieldAt fields
            $ pif (kind #== 0) 4 $
              pif (kind #== 1) 6 $
                pif (kind #== 4) 2 3

{- | Aiken @data_node_child_roots_v1@.

The roots a proof must open beneath this node: one for most shapes, two for a
large constructor, and none is not a case — every shape commits to at least one
child.
-}
pdataNodeChildRootsV1 ::
  forall (s :: S). Term s (PDataNodeV1 :--> PBuiltinList (PAsData PByteString))
pdataNodeChildRootsV1 = phoistAcyclic $
  plam $ \node ->
    let (tag, fields) = pconstrOf (pdata node)
     in plet tag $ \kind ->
          pif
            (kind #== 1)
            ( pcons
                # pbytesAt fields 0
                # (pcons # pbytesAt fields 4 # pnil)
            )
            $ pcons
              # ( pbytesAt fields $
                    pif (kind #== 0) 2 $
                      pif (kind #== 2 #|| kind #== 3) 1 0
                )
              # pnil

-- | Aiken @data_list_node_child_roots_v1@.
pdataListNodeChildRootsV1 ::
  forall (s :: S). Term s (PDataListNodeV1 :--> PBuiltinList (PAsData PByteString))
pdataListNodeChildRootsV1 = phoistAcyclic $
  plam $ \node ->
    pmatch node $ \PDataListNodeV1 {plistNode'head, plistNode'tail} ->
      pcons # plistNode'head # (pcons # plistNode'tail # pnil)

-- | Aiken @data_pair_node_child_roots_v1@.
pdataPairNodeChildRootsV1 ::
  forall (s :: S). Term s (PDataPairNodeV1 :--> PBuiltinList (PAsData PByteString))
pdataPairNodeChildRootsV1 = phoistAcyclic $
  plam $ \node ->
    pmatch node $ \PDataPairNodeV1 {ppairNode'key, ppairNode'value, ppairNode'tail} ->
      pcons # ppairNode'key # (pcons # ppairNode'value # (pcons # ppairNode'tail # pnil))

-- | The field at a position in a constructor's field vector.
pfieldAt ::
  forall (s :: S). Term s (PBuiltinList PData) -> Term s PInteger -> Term s PData
pfieldAt fields index = pelemAt # index # fields

-- | The same, read as a byte string.
pbytesAt ::
  forall (s :: S).
  Term s (PBuiltinList PData) -> Term s PInteger -> Term s (PAsData PByteString)
pbytesAt fields index = pdata (pasByteStr # pfieldAt fields index)

--------------------------------------------------------------------------------
-- CBOR lengths
--------------------------------------------------------------------------------

-- | Aiken @list_cbor_length@ — an indefinite array, or @0x80@ when empty.
plistCborLength :: forall (s :: S). Term s (PInteger :--> PInteger :--> PInteger)
plistCborLength = phoistAcyclic $
  plam $ \len payloadCborLength ->
    pif (0 #<= (pexpectUint32 # len)) `flip` perror $
      pif (0 #<= (pexpectUint64 # payloadCborLength)) `flip` perror $
        pif (len #== 0) 1 (2 + payloadCborLength)

-- | Aiken @map_cbor_length@ — a definite map, so the header grows with the count.
pmapCborLength :: forall (s :: S). Term s (PInteger :--> PInteger :--> PInteger)
pmapCborLength = phoistAcyclic $
  plam $ \len payloadCborLength ->
    pif (0 #<= (pexpectUint32 # len)) `flip` perror $
      pif (0 #<= (pexpectUint64 # payloadCborLength)) `flip` perror $
        pheaderLength len + payloadCborLength

-- | Aiken @bytes_header_length@, and the same arithmetic the map header uses.
pheaderLength :: forall (s :: S). Term s PInteger -> Term s PInteger
pheaderLength len =
  pif (len #< 24) 1 $
    pif (len #<= 255) 2 $
      pif (len #<= 65535) 3 5

-- | Aiken @bytes_header_length@.
pbytesHeaderLength :: forall (s :: S). Term s (PInteger :--> PInteger)
pbytesHeaderLength = phoistAcyclic $
  plam $ \len ->
    pif (0 #<= (pexpectUint32 # len)) (pheaderLength len) perror

{- | Aiken @bytes_data_cbor_length_v1@.

Above 64 bytes Plutus emits the indefinite chunked form — @0x5f@, then 64-byte
chunks, then a break — so the length is not the byte count plus a header. The
@66@ is a 64-byte chunk plus its two-byte header, and the @2@ is the @0x5f@ and
the break.
-}
pbytesDataCborLengthV1 :: forall (s :: S). Term s (PInteger :--> PInteger)
pbytesDataCborLengthV1 = phoistAcyclic $
  plam $ \bytesLength ->
    pif (0 #<= (pexpectUint32 # bytesLength)) `flip` perror $
      pif
        (bytesLength #<= 64)
        (pbytesHeaderLength # bytesLength + bytesLength)
        $ plet (pquot # bytesLength # 64) $ \fullChunks ->
          plet (prem # bytesLength # 64) $ \remainder ->
            2
              + fullChunks * 66
              + pif
                (remainder #== 0)
                0
                (pbytesHeaderLength # remainder + remainder)

--------------------------------------------------------------------------------
-- Integer sizing
--------------------------------------------------------------------------------

-- | Aiken @unsigned_byte_size@.
punsignedByteSize :: forall (s :: S). Term s (PInteger :--> PInteger)
punsignedByteSize = phoistAcyclic $
  pfix $ \self -> plam $ \value ->
    pif (value #< 256) 1 (1 + (self #$ pquot # value # 256))

{- | Aiken @integer_memory_size@.

The doubling is Plutus's: an integer's @ExMemory@ counts 64-bit words of the
/doubled/ magnitude, which is how the sign bit is paid for.
-}
pintegerMemorySize :: forall (s :: S). Term s (PInteger :--> PInteger)
pintegerMemorySize = phoistAcyclic $
  plam $ \value ->
    punsignedByteSize
      #$ pif (value #< 0) ((negate value - 1) * 2) (value * 2)

--------------------------------------------------------------------------------
-- Verification
--------------------------------------------------------------------------------

-- | Aiken @empty_or_list_summary_matches@.
pemptyOrListSummaryMatches ::
  forall (s :: S).
  Term s PByteString ->
  Term s PInteger ->
  Term s (PMaybeData PDataListNodeV1) ->
  Term s PBool
pemptyOrListSummaryMatches root len summary =
  pif
    (len #== 0)
    ( root
        #== pemptyDataListRootV1
        #&& pmatch summary (\case PDNothing -> pconstant True; PDJust _ -> pconstant False)
    )
    $ pmatch summary
    $ \case
      PDNothing -> perror
      PDJust exact ->
        plet (pfromData exact) $ \node ->
          root
            #== (phashDataListNodeV1 # node)
            #&& (plistLength node #== len)

-- | Aiken @empty_or_pair_summary_matches@.
pemptyOrPairSummaryMatches ::
  forall (s :: S).
  Term s PByteString ->
  Term s PInteger ->
  Term s (PMaybeData PDataPairNodeV1) ->
  Term s PBool
pemptyOrPairSummaryMatches root len summary =
  pif
    (len #== 0)
    ( root
        #== pemptyDataPairRootV1
        #&& pmatch summary (\case PDNothing -> pconstant True; PDJust _ -> pconstant False)
    )
    $ pmatch summary
    $ \case
      PDNothing -> perror
      PDJust exact ->
        plet (pfromData exact) $ \node ->
          root
            #== (phashDataPairNodeV1 # node)
            #&& (ppairLength node #== len)

plistLength :: forall (s :: S). Term s PDataListNodeV1 -> Term s PInteger
plistLength node = pmatch node $ \PDataListNodeV1 {plistNode'length} -> pfromData plistNode'length

ppairLength :: forall (s :: S). Term s PDataPairNodeV1 -> Term s PInteger
ppairLength node = pmatch node $ \PDataPairNodeV1 {ppairNode'length} -> pfromData ppairNode'length

-- | Aiken @list_payload_length@ and @list_memory@, at their absent default.
plistSummaryField ::
  forall (s :: S).
  (Term s PDataListNodeV1 -> Term s PInteger) ->
  Term s (PMaybeData PDataListNodeV1) ->
  Term s PInteger
plistSummaryField field summary =
  pmatch summary $ \case
    PDNothing -> 0
    PDJust exact -> field (pfromData exact)

ppairSummaryField ::
  forall (s :: S).
  (Term s PDataPairNodeV1 -> Term s PInteger) ->
  Term s (PMaybeData PDataPairNodeV1) ->
  Term s PInteger
ppairSummaryField field summary =
  pmatch summary $ \case
    PDNothing -> 0
    PDJust exact -> field (pfromData exact)

plistPayload, plistMemory :: forall (s :: S). Term s PDataListNodeV1 -> Term s PInteger
plistPayload node =
  pmatch node $ \PDataListNodeV1 {plistNode'payloadCborLength} -> pfromData plistNode'payloadCborLength
plistMemory node =
  pmatch node $ \PDataListNodeV1 {plistNode'memory} -> pfromData plistNode'memory

ppairPayload, ppairMemory :: forall (s :: S). Term s PDataPairNodeV1 -> Term s PInteger
ppairPayload node =
  pmatch node $ \PDataPairNodeV1 {ppairNode'payloadCborLength} -> pfromData ppairNode'payloadCborLength
ppairMemory node =
  pmatch node $ \PDataPairNodeV1 {ppairNode'memory} -> pfromData ppairNode'memory

pisNothingList :: forall (s :: S). Term s (PMaybeData PDataListNodeV1) -> Term s PBool
pisNothingList summary =
  pmatch summary $ \case PDNothing -> pconstant True; PDJust _ -> pconstant False

pisNothingPair :: forall (s :: S). Term s (PMaybeData PDataPairNodeV1) -> Term s PBool
pisNothingPair summary =
  pmatch summary $ \case PDNothing -> pconstant True; PDJust _ -> pconstant False

{- | Aiken @verify_data_node_v1@.

What one node claims about itself, checked against the sequence summary beneath
it. Every shape asserts that the summary it does /not/ use is absent — a list
node with a pair summary attached would be a proof of two different things at
once.
-}
pverifyDataNodeV1 ::
  forall (s :: S).
  Term
    s
    ( PDataNodeV1
        :--> PMaybeData PDataListNodeV1
        :--> PMaybeData PDataPairNodeV1
        :--> PBool
    )
pverifyDataNodeV1 = phoistAcyclic $
  plam $ \node listSummary pairSummary ->
    pmatch node $ \case
      PConstrSmallData
        { pnode'constructor
        , pnode'fieldsCount
        , pnode'fieldsRoot
        , pnode'cborLength
        , pnode'memory
        } ->
          plet (pfromData pnode'constructor) $ \constructor ->
            pif (0 #<= constructor #&& constructor #<= 127) `flip` perror $
              plet (pfromData pnode'fieldsCount) $ \fieldsCount ->
                plet
                  ( plistCborLength
                      # fieldsCount
                      # plistSummaryField plistPayload listSummary
                  )
                  $ \fieldsCborLength ->
                    pemptyOrListSummaryMatches (pfromData pnode'fieldsRoot) fieldsCount listSummary
                      #&& pisNothingPair pairSummary
                      #&& ( pfromData pnode'cborLength
                              #== pif
                                (constructor #<= 6)
                                (2 + fieldsCborLength)
                                (3 + fieldsCborLength)
                          )
                      #&& ( pfromData pnode'memory
                              #== 4 + plistSummaryField plistMemory listSummary
                          )
      PConstrLargeData
        { pnode'constructorCborRoot
        , pnode'constructorCborLength
        , pnode'constructorMemory
        , pnode'fieldsCount
        , pnode'fieldsRoot
        , pnode'cborLength
        , pnode'memory
        } ->
          plet (pfromData pnode'fieldsCount) $ \fieldsCount ->
            pemptyOrListSummaryMatches (pfromData pnode'fieldsRoot) fieldsCount listSummary
              #&& pisNothingPair pairSummary
              #&& (plengthBS # pfromData pnode'constructorCborRoot #== 32)
              #&& (0 #< pfromData pnode'constructorCborLength)
              #&& (5 #<= pfromData pnode'constructorMemory)
              #&& ( pfromData pnode'cborLength
                      #== 3
                      + pfromData pnode'constructorCborLength
                      + ( plistCborLength
                            # fieldsCount
                            # plistSummaryField plistPayload listSummary
                        )
                  )
              #&& (pfromData pnode'memory #== 4 + plistSummaryField plistMemory listSummary)
      PMapDataNode {pnode'entriesCount, pnode'entriesRoot, pnode'cborLength, pnode'memory} ->
        plet (pfromData pnode'entriesCount) $ \entriesCount ->
          pemptyOrPairSummaryMatches (pfromData pnode'entriesRoot) entriesCount pairSummary
            #&& pisNothingList listSummary
            #&& ( pfromData pnode'cborLength
                    #== ( pmapCborLength
                            # entriesCount
                            # ppairSummaryField ppairPayload pairSummary
                        )
                )
            #&& (pfromData pnode'memory #== 4 + ppairSummaryField ppairMemory pairSummary)
      PListDataNode {pnode'itemsCount, pnode'itemsRoot, pnode'cborLength, pnode'memory} ->
        plet (pfromData pnode'itemsCount) $ \itemsCount ->
          pemptyOrListSummaryMatches (pfromData pnode'itemsRoot) itemsCount listSummary
            #&& pisNothingPair pairSummary
            #&& ( pfromData pnode'cborLength
                    #== ( plistCborLength
                            # itemsCount
                            # plistSummaryField plistPayload listSummary
                        )
                )
            #&& (pfromData pnode'memory #== 4 + plistSummaryField plistMemory listSummary)
      PIntegerDataNode {pnode'cborRoot, pnode'cborLength, pnode'memory} ->
        pisNothingList listSummary
          #&& pisNothingPair pairSummary
          #&& (plengthBS # pfromData pnode'cborRoot #== 32)
          #&& (0 #< pfromData pnode'cborLength)
          #&& (5 #<= pfromData pnode'memory)
      PBytesDataNode {pnode'bytesRoot, pnode'bytesLength, pnode'cborLength, pnode'memory} ->
        plet (pfromData pnode'bytesLength) $ \bytesLength ->
          pisNothingList listSummary
            #&& pisNothingPair pairSummary
            #&& (plengthBS # pfromData pnode'bytesRoot #== 32)
            #&& (0 #<= bytesLength)
            #&& (pfromData pnode'cborLength #== pbytesDataCborLengthV1 # bytesLength)
            #&& ( pfromData pnode'memory
                    #== 4 + pif (bytesLength #== 0) 1 bytesLength
                )

-- | Aiken @verify_data_list_link_v1@.
pverifyDataListLinkV1 ::
  forall (s :: S).
  Term
    s
    ( PDataListNodeV1
        :--> PDataNodeV1
        :--> PMaybeData PDataListNodeV1
        :--> PBool
    )
pverifyDataListLinkV1 = phoistAcyclic $
  plam $ \node headNode tail ->
    pmatch node $
      \PDataListNodeV1
        { plistNode'head
        , plistNode'headCborLength
        , plistNode'headMemory
        , plistNode'tail
        , plistNode'length
        , plistNode'payloadCborLength
        , plistNode'memory
        } ->
          plet
            ( pmatch tail $ \case
                PDNothing -> pemptyDataListRootV1
                PDJust exact -> phashDataListNodeV1 # pfromData exact
            )
            $ \tailRoot ->
              plet (plistSummaryField plistLength tail) $ \tailLength ->
                plet (pfromData plistNode'headCborLength) $ \headCborLength ->
                  plet (pfromData plistNode'headMemory) $ \headMemory ->
                    (pfromData plistNode'head #== phashDataNodeV1 # headNode)
                      #&& (headCborLength #== pdataNodeCborLengthV1 # headNode)
                      #&& (headMemory #== pdataNodeMemoryV1 # headNode)
                      #&& (pfromData plistNode'tail #== tailRoot)
                      #&& (pfromData plistNode'length #== tailLength + 1)
                      #&& ( pfromData plistNode'payloadCborLength
                              #== headCborLength + plistSummaryField plistPayload tail
                          )
                      #&& ( pfromData plistNode'memory
                              #== headMemory + plistSummaryField plistMemory tail
                          )

-- | Aiken @verify_data_pair_link_v1@.
pverifyDataPairLinkV1 ::
  forall (s :: S).
  Term
    s
    ( PDataPairNodeV1
        :--> PDataNodeV1
        :--> PDataNodeV1
        :--> PMaybeData PDataPairNodeV1
        :--> PBool
    )
pverifyDataPairLinkV1 = phoistAcyclic $
  plam $ \node keyNode valueNode tail ->
    pmatch node $
      \PDataPairNodeV1
        { ppairNode'key
        , ppairNode'keyCborLength
        , ppairNode'keyMemory
        , ppairNode'value
        , ppairNode'valueCborLength
        , ppairNode'valueMemory
        , ppairNode'tail
        , ppairNode'length
        , ppairNode'payloadCborLength
        , ppairNode'memory
        } ->
          plet
            ( pmatch tail $ \case
                PDNothing -> pemptyDataPairRootV1
                PDJust exact -> phashDataPairNodeV1 # pfromData exact
            )
            $ \tailRoot ->
              plet (ppairSummaryField ppairLength tail) $ \tailLength ->
                plet (pfromData ppairNode'keyCborLength) $ \keyCborLength ->
                  plet (pfromData ppairNode'valueCborLength) $ \valueCborLength ->
                    plet (pfromData ppairNode'keyMemory) $ \keyMemory ->
                      plet (pfromData ppairNode'valueMemory) $ \valueMemory ->
                        (pfromData ppairNode'key #== phashDataNodeV1 # keyNode)
                          #&& (keyCborLength #== pdataNodeCborLengthV1 # keyNode)
                          #&& (keyMemory #== pdataNodeMemoryV1 # keyNode)
                          #&& (pfromData ppairNode'value #== phashDataNodeV1 # valueNode)
                          #&& (valueCborLength #== pdataNodeCborLengthV1 # valueNode)
                          #&& (valueMemory #== pdataNodeMemoryV1 # valueNode)
                          #&& (pfromData ppairNode'tail #== tailRoot)
                          #&& (pfromData ppairNode'length #== tailLength + 1)
                          #&& ( pfromData ppairNode'payloadCborLength
                                  #== keyCborLength
                                  + valueCborLength
                                  + ppairSummaryField ppairPayload tail
                              )
                          #&& ( pfromData ppairNode'memory
                                  #== keyMemory
                                  + valueMemory
                                  + ppairSummaryField ppairMemory tail
                              )

--------------------------------------------------------------------------------
-- Data shape predicates
--------------------------------------------------------------------------------

{- | Aiken @data_is_integer@ and its two siblings.

The branches are constants, so the eager 'pchooseData' is safe here — unlike in
'psemanticDataSummaryV1', where each branch is a whole subtree commitment and
must be delayed.
-}
pdataIsInteger, pdataIsBytes, pdataIsList :: forall (s :: S). Term s (PData :--> PBool)
pdataIsInteger = phoistAcyclic $ plam $ \d -> pchoose5 d pfalse pfalse pfalse ptrue pfalse
pdataIsBytes = phoistAcyclic $ plam $ \d -> pchoose5 d pfalse pfalse pfalse pfalse ptrue
pdataIsList = phoistAcyclic $ plam $ \d -> pchoose5 d pfalse pfalse ptrue pfalse pfalse

pchoose5 ::
  forall (s :: S).
  Term s PData ->
  Term s PBool ->
  Term s PBool ->
  Term s PBool ->
  Term s PBool ->
  Term s PBool ->
  Term s PBool
pchoose5 d a b c e f = pchooseData # d # a # b # c # e # f

ptrue, pfalse :: forall (s :: S). Term s PBool
ptrue = pconstant @PBool True
pfalse = pconstant @PBool False

--------------------------------------------------------------------------------
-- Inspection
--------------------------------------------------------------------------------

{- | Aiken @inspect_data_node_preimage_v1@.

Decodes one preimage, dispatches on the decoded array's /arity/ — 6 for a small
constructor, 8 for a large one, 5 for the counted shapes (map, list, bytes) and
4 for an integer — shape- and range-checks every field, rebuilds the node, and
then re-encodes it and compares against the bytes it was handed. That last step
is what makes indefinite-length arrays, non-minimal headers and trailing bytes
fail closed: "Aiken.Cbor" reads all of them, and only the re-encoding rejects
them.
-}
pinspectDataNodePreimageV1 ::
  forall (s :: S). Term s (PByteString :--> PMaybe PDataNodeV1)
pinspectDataNodePreimageV1 = phoistAcyclic $
  plam $ \preimage ->
    pmatch (pdeserialise # preimage) $ \case
      PNothing -> pcon PNothing
      PJust decoded ->
        pif (pnot # (pdataIsList # decoded)) (pcon PNothing) $
          plet (pasList # decoded) $ \items ->
            plet (plength # items) $ \arity ->
              pif (arity #== 6) (pinspectSmallNode preimage items) $
                pif (arity #== 8) (pinspectLargeNode preimage items) $
                  pif (arity #== 5) (pinspectCountedNode preimage items) $
                    pif (arity #== 4) (pinspectIntegerNode preimage items) $
                      pcon PNothing

-- | The re-encoding gate every inspector ends with.
pfinishNode ::
  forall (s :: S).
  Term s PByteString ->
  Term s PDataNodeV1 ->
  Term s (PMaybe PDataNodeV1)
pfinishNode preimage node =
  plet node $ \n ->
    pif
      (pencodeDataNodePreimageV1 # n #== preimage)
      (pcon (PJust n))
      (pcon PNothing)

-- | Kind 0 — a constructor whose index fits the compact tag encoding.
pinspectSmallNode ::
  forall (s :: S).
  Term s PByteString ->
  Term s (PBuiltinList PData) ->
  Term s (PMaybe PDataNodeV1)
pinspectSmallNode preimage items =
  plet (pfieldAt items 0) $ \kindD ->
    plet (pfieldAt items 1) $ \constructorD ->
      plet (pfieldAt items 2) $ \fieldsCountD ->
        plet (pfieldAt items 3) $ \fieldsRootD ->
          plet (pfieldAt items 4) $ \cborLengthD ->
            plet (pfieldAt items 5) $ \memoryD ->
              pif
                ( pand'List
                    [ pdataIsInteger # kindD
                    , pdataIsInteger # constructorD
                    , pdataIsInteger # fieldsCountD
                    , pdataIsBytes # fieldsRootD
                    , pdataIsInteger # cborLengthD
                    , pdataIsInteger # memoryD
                    ]
                )
                ( plet (pasInt # constructorD) $ \constructor ->
                    plet (pasInt # fieldsCountD) $ \fieldsCount ->
                      plet (pasByteStr # fieldsRootD) $ \fieldsRoot ->
                        plet (pasInt # cborLengthD) $ \cborLength ->
                          plet (pasInt # memoryD) $ \memory ->
                            pif
                              ( pand'List
                                  [ pasInt # kindD #== 0
                                  , 0 #<= constructor
                                  , constructor #<= 127
                                  , puint32 # fieldsCount
                                  , plengthBS # fieldsRoot #== 32
                                  , puint64 # cborLength
                                  , puint64 # memory
                                  ]
                              )
                              ( pfinishNode preimage $
                                  pcon $
                                    PConstrSmallData
                                      { pnode'constructor = pdata constructor
                                      , pnode'fieldsCount = pdata fieldsCount
                                      , pnode'fieldsRoot = pdata fieldsRoot
                                      , pnode'cborLength = pdata cborLength
                                      , pnode'memory = pdata memory
                                      }
                              )
                              (pcon PNothing)
                )
                (pcon PNothing)

-- | Kind 1 — a constructor index too large to be a tag, carried as its own blob.
pinspectLargeNode ::
  forall (s :: S).
  Term s PByteString ->
  Term s (PBuiltinList PData) ->
  Term s (PMaybe PDataNodeV1)
pinspectLargeNode preimage items =
  plet (pfieldAt items 0) $ \kindD ->
    plet (pfieldAt items 1) $ \constructorRootD ->
      plet (pfieldAt items 2) $ \constructorLengthD ->
        plet (pfieldAt items 3) $ \constructorMemoryD ->
          plet (pfieldAt items 4) $ \fieldsCountD ->
            plet (pfieldAt items 5) $ \fieldsRootD ->
              plet (pfieldAt items 6) $ \cborLengthD ->
                plet (pfieldAt items 7) $ \memoryD ->
                  pif
                    ( pand'List
                        [ pdataIsInteger # kindD
                        , pdataIsBytes # constructorRootD
                        , pdataIsInteger # constructorLengthD
                        , pdataIsInteger # constructorMemoryD
                        , pdataIsInteger # fieldsCountD
                        , pdataIsBytes # fieldsRootD
                        , pdataIsInteger # cborLengthD
                        , pdataIsInteger # memoryD
                        ]
                    )
                    ( plet (pasByteStr # constructorRootD) $ \constructorCborRoot ->
                        plet (pasInt # constructorLengthD) $ \constructorCborLength ->
                          plet (pasInt # constructorMemoryD) $ \constructorMemory ->
                            plet (pasInt # fieldsCountD) $ \fieldsCount ->
                              plet (pasByteStr # fieldsRootD) $ \fieldsRoot ->
                                plet (pasInt # cborLengthD) $ \cborLength ->
                                  plet (pasInt # memoryD) $ \memory ->
                                    pif
                                      ( pand'List
                                          [ pasInt # kindD #== 1
                                          , plengthBS # constructorCborRoot #== 32
                                          , puint32 # constructorCborLength
                                          , puint64 # constructorMemory
                                          , puint32 # fieldsCount
                                          , plengthBS # fieldsRoot #== 32
                                          , puint64 # cborLength
                                          , puint64 # memory
                                          ]
                                      )
                                      ( pfinishNode preimage $
                                          pcon $
                                            PConstrLargeData
                                              { pnode'constructorCborRoot = pdata constructorCborRoot
                                              , pnode'constructorCborLength = pdata constructorCborLength
                                              , pnode'constructorMemory = pdata constructorMemory
                                              , pnode'fieldsCount = pdata fieldsCount
                                              , pnode'fieldsRoot = pdata fieldsRoot
                                              , pnode'cborLength = pdata cborLength
                                              , pnode'memory = pdata memory
                                              }
                                      )
                                      (pcon PNothing)
                    )
                    (pcon PNothing)

{- | Kinds 2, 3 and 5 — map, list and bytes, which share a shape.

All three are a count, a root and the two replay numbers; only the kind tag and
the range on @cbor_length@ tell them apart. Bytes is the narrow one: its length
is a @uint32@ because a byte string that long would not fit a proof anyway.
-}
pinspectCountedNode ::
  forall (s :: S).
  Term s PByteString ->
  Term s (PBuiltinList PData) ->
  Term s (PMaybe PDataNodeV1)
pinspectCountedNode preimage items =
  plet (pfieldAt items 0) $ \kindD ->
    plet (pfieldAt items 1) $ \countD ->
      plet (pfieldAt items 2) $ \rootD ->
        plet (pfieldAt items 3) $ \cborLengthD ->
          plet (pfieldAt items 4) $ \memoryD ->
            pif
              ( pand'List
                  [ pdataIsInteger # kindD
                  , pdataIsInteger # countD
                  , pdataIsBytes # rootD
                  , pdataIsInteger # cborLengthD
                  , pdataIsInteger # memoryD
                  ]
              )
              ( plet (pasInt # kindD) $ \kind ->
                  plet (pasInt # countD) $ \count ->
                    plet (pasByteStr # rootD) $ \root ->
                      plet (pasInt # cborLengthD) $ \cborLength ->
                        plet (pasInt # memoryD) $ \memory ->
                          pif
                            ( pand'List
                                [ kind #== 2 #|| kind #== 3 #|| kind #== 5
                                , puint32 # count
                                , plengthBS # root #== 32
                                , pif
                                    (kind #== 5)
                                    (puint32 # cborLength)
                                    (puint64 # cborLength)
                                , puint64 # memory
                                ]
                            )
                            ( pfinishNode preimage $
                                pif
                                  (kind #== 2)
                                  ( pcon $
                                      PMapDataNode
                                        { pnode'entriesCount = pdata count
                                        , pnode'entriesRoot = pdata root
                                        , pnode'cborLength = pdata cborLength
                                        , pnode'memory = pdata memory
                                        }
                                  )
                                  $ pif
                                    (kind #== 3)
                                    ( pcon $
                                        PListDataNode
                                          { pnode'itemsCount = pdata count
                                          , pnode'itemsRoot = pdata root
                                          , pnode'cborLength = pdata cborLength
                                          , pnode'memory = pdata memory
                                          }
                                    )
                                    ( pcon $
                                        PBytesDataNode
                                          { pnode'bytesRoot = pdata root
                                          , pnode'bytesLength = pdata count
                                          , pnode'cborLength = pdata cborLength
                                          , pnode'memory = pdata memory
                                          }
                                    )
                            )
                            (pcon PNothing)
              )
              (pcon PNothing)

-- | Kind 4 — an integer, which has no count because its blob root is the value.
pinspectIntegerNode ::
  forall (s :: S).
  Term s PByteString ->
  Term s (PBuiltinList PData) ->
  Term s (PMaybe PDataNodeV1)
pinspectIntegerNode preimage items =
  plet (pfieldAt items 0) $ \kindD ->
    plet (pfieldAt items 1) $ \rootD ->
      plet (pfieldAt items 2) $ \cborLengthD ->
        plet (pfieldAt items 3) $ \memoryD ->
          pif
            ( pand'List
                [ pdataIsInteger # kindD
                , pdataIsBytes # rootD
                , pdataIsInteger # cborLengthD
                , pdataIsInteger # memoryD
                ]
            )
            ( plet (pasByteStr # rootD) $ \root ->
                plet (pasInt # cborLengthD) $ \cborLength ->
                  plet (pasInt # memoryD) $ \memory ->
                    pif
                      ( pand'List
                          [ pasInt # kindD #== 4
                          , plengthBS # root #== 32
                          , puint32 # cborLength
                          , puint64 # memory
                          ]
                      )
                      ( pfinishNode preimage $
                          pcon $
                            PIntegerDataNode
                              { pnode'cborRoot = pdata root
                              , pnode'cborLength = pdata cborLength
                              , pnode'memory = pdata memory
                              }
                      )
                      (pcon PNothing)
            )
            (pcon PNothing)

-- | Aiken @inspect_data_list_node_preimage_v1@ — one item-sequence link.
pinspectDataListNodePreimageV1 ::
  forall (s :: S). Term s (PByteString :--> PMaybe PDataListNodeV1)
pinspectDataListNodePreimageV1 = phoistAcyclic $
  plam $ \preimage ->
    pmatch (pdeserialise # preimage) $ \case
      PNothing -> pcon PNothing
      PJust decoded ->
        pif (pnot # (pdataIsList # decoded)) (pcon PNothing) $
          plet (pasList # decoded) $ \items ->
            pif (pnot #$ plength # items #== 7) (pcon PNothing) $
              plet (pfieldAt items 0) $ \headD ->
                plet (pfieldAt items 1) $ \headLengthD ->
                  plet (pfieldAt items 2) $ \headMemoryD ->
                    plet (pfieldAt items 3) $ \tailD ->
                      plet (pfieldAt items 4) $ \lengthD ->
                        plet (pfieldAt items 5) $ \payloadD ->
                          plet (pfieldAt items 6) $ \memoryD ->
                            pif
                              ( pand'List
                                  [ pdataIsBytes # headD
                                  , pdataIsInteger # headLengthD
                                  , pdataIsInteger # headMemoryD
                                  , pdataIsBytes # tailD
                                  , pdataIsInteger # lengthD
                                  , pdataIsInteger # payloadD
                                  , pdataIsInteger # memoryD
                                  ]
                              )
                              ( plet (pasByteStr # headD) $ \headRoot ->
                                  plet (pasInt # headLengthD) $ \headCborLength ->
                                    plet (pasInt # headMemoryD) $ \headMemory ->
                                      plet (pasByteStr # tailD) $ \tailRoot ->
                                        plet (pasInt # lengthD) $ \len ->
                                          plet (pasInt # payloadD) $ \payloadCborLength ->
                                            plet (pasInt # memoryD) $ \memory ->
                                              pif
                                                ( pand'List
                                                    [ plengthBS # headRoot #== 32
                                                    , puint32 # headCborLength
                                                    , puint64 # headMemory
                                                    , plengthBS # tailRoot #== 32
                                                    , 0 #< len
                                                    , puint32 # len
                                                    , puint64 # payloadCborLength
                                                    , puint64 # memory
                                                    ]
                                                )
                                                ( plet
                                                    ( pcon $
                                                        PDataListNodeV1
                                                          { plistNode'head = pdata headRoot
                                                          , plistNode'headCborLength = pdata headCborLength
                                                          , plistNode'headMemory = pdata headMemory
                                                          , plistNode'tail = pdata tailRoot
                                                          , plistNode'length = pdata len
                                                          , plistNode'payloadCborLength = pdata payloadCborLength
                                                          , plistNode'memory = pdata memory
                                                          }
                                                    )
                                                    $ \node ->
                                                      pif
                                                        (pencodeDataListNodePreimageV1 # node #== preimage)
                                                        (pcon (PJust node))
                                                        (pcon PNothing)
                                                )
                                                (pcon PNothing)
                              )
                              (pcon PNothing)

-- | Aiken @inspect_data_pair_node_preimage_v1@ — one key/value-sequence link.
pinspectDataPairNodePreimageV1 ::
  forall (s :: S). Term s (PByteString :--> PMaybe PDataPairNodeV1)
pinspectDataPairNodePreimageV1 = phoistAcyclic $
  plam $ \preimage ->
    pmatch (pdeserialise # preimage) $ \case
      PNothing -> pcon PNothing
      PJust decoded ->
        pif (pnot # (pdataIsList # decoded)) (pcon PNothing) $
          plet (pasList # decoded) $ \items ->
            pif (pnot #$ plength # items #== 10) (pcon PNothing) $
              plet (pfieldAt items 0) $ \keyD ->
                plet (pfieldAt items 1) $ \keyLengthD ->
                  plet (pfieldAt items 2) $ \keyMemoryD ->
                    plet (pfieldAt items 3) $ \valueD ->
                      plet (pfieldAt items 4) $ \valueLengthD ->
                        plet (pfieldAt items 5) $ \valueMemoryD ->
                          plet (pfieldAt items 6) $ \tailD ->
                            plet (pfieldAt items 7) $ \lengthD ->
                              plet (pfieldAt items 8) $ \payloadD ->
                                plet (pfieldAt items 9) $ \memoryD ->
                                  pif
                                    ( pand'List
                                        [ pdataIsBytes # keyD
                                        , pdataIsInteger # keyLengthD
                                        , pdataIsInteger # keyMemoryD
                                        , pdataIsBytes # valueD
                                        , pdataIsInteger # valueLengthD
                                        , pdataIsInteger # valueMemoryD
                                        , pdataIsBytes # tailD
                                        , pdataIsInteger # lengthD
                                        , pdataIsInteger # payloadD
                                        , pdataIsInteger # memoryD
                                        ]
                                    )
                                    (pinspectPairFields preimage keyD keyLengthD keyMemoryD valueD valueLengthD valueMemoryD tailD lengthD payloadD memoryD)
                                    (pcon PNothing)

-- | The range checks and rebuild of 'pinspectDataPairNodePreimageV1'.
pinspectPairFields ::
  forall (s :: S).
  Term s PByteString ->
  Term s PData ->
  Term s PData ->
  Term s PData ->
  Term s PData ->
  Term s PData ->
  Term s PData ->
  Term s PData ->
  Term s PData ->
  Term s PData ->
  Term s PData ->
  Term s (PMaybe PDataPairNodeV1)
pinspectPairFields preimage keyD keyLengthD keyMemoryD valueD valueLengthD valueMemoryD tailD lengthD payloadD memoryD =
  plet (pasByteStr # keyD) $ \key ->
    plet (pasInt # keyLengthD) $ \keyCborLength ->
      plet (pasInt # keyMemoryD) $ \keyMemory ->
        plet (pasByteStr # valueD) $ \value ->
          plet (pasInt # valueLengthD) $ \valueCborLength ->
            plet (pasInt # valueMemoryD) $ \valueMemory ->
              plet (pasByteStr # tailD) $ \tailRoot ->
                plet (pasInt # lengthD) $ \len ->
                  plet (pasInt # payloadD) $ \payloadCborLength ->
                    plet (pasInt # memoryD) $ \memory ->
                      pif
                        ( pand'List
                            [ plengthBS # key #== 32
                            , puint32 # keyCborLength
                            , puint64 # keyMemory
                            , plengthBS # value #== 32
                            , puint32 # valueCborLength
                            , puint64 # valueMemory
                            , plengthBS # tailRoot #== 32
                            , 0 #< len
                            , puint32 # len
                            , puint64 # payloadCborLength
                            , puint64 # memory
                            ]
                        )
                        ( plet
                            ( pcon $
                                PDataPairNodeV1
                                  { ppairNode'key = pdata key
                                  , ppairNode'keyCborLength = pdata keyCborLength
                                  , ppairNode'keyMemory = pdata keyMemory
                                  , ppairNode'value = pdata value
                                  , ppairNode'valueCborLength = pdata valueCborLength
                                  , ppairNode'valueMemory = pdata valueMemory
                                  , ppairNode'tail = pdata tailRoot
                                  , ppairNode'length = pdata len
                                  , ppairNode'payloadCborLength = pdata payloadCborLength
                                  , ppairNode'memory = pdata memory
                                  }
                            )
                            $ \node ->
                              pif
                                (pencodeDataPairNodePreimageV1 # node #== preimage)
                                (pcon (PJust node))
                                (pcon PNothing)
                        )
                        (pcon PNothing)

--------------------------------------------------------------------------------
-- Summaries
--------------------------------------------------------------------------------

-- | A node's summary: its root and the two numbers a replay charges it by.
psummaryOfNode :: forall (s :: S). Term s PDataNodeV1 -> Term s PDataSummaryV1
psummaryOfNode node =
  plet node $ \n ->
    pcon $
      PDataSummaryV1
        { psummary'root = pdata (phashDataNodeV1 # n)
        , psummary'cborLength = pdata (pdataNodeCborLengthV1 # n)
        , psummary'memory = pdata (pdataNodeMemoryV1 # n)
        }

-- | Aiken @empty_data_list_summary_v1@.
pemptyDataListSummaryV1 :: forall (s :: S). Term s PDataSequenceSummaryV1
pemptyDataListSummaryV1 =
  pcon $
    PDataSequenceSummaryV1
      { pseq'root = pdata pemptyDataListRootV1
      , pseq'length = pdata 0
      , pseq'payloadCborLength = pdata 0
      , pseq'memory = pdata 0
      }

-- | Aiken @prepend_data_list_summary_v1@.
pprependDataListSummaryV1 ::
  forall (s :: S).
  Term s (PDataSummaryV1 :--> PDataSequenceSummaryV1 :--> PDataSequenceSummaryV1)
pprependDataListSummaryV1 = phoistAcyclic $
  plam $ \headSummary tailSummary ->
    pmatch headSummary $ \(PDataSummaryV1 headRoot headCborLength headMemory) ->
      pmatch tailSummary $ \(PDataSequenceSummaryV1 tailRoot tailLength tailPayload tailMemory) ->
        plet (pfromData tailLength + 1) $ \len ->
          plet (pfromData headCborLength + pfromData tailPayload) $ \payload ->
            plet (pfromData headMemory + pfromData tailMemory) $ \memory ->
              plet
                ( pcon $
                    PDataListNodeV1
                      { plistNode'head = headRoot
                      , plistNode'headCborLength = headCborLength
                      , plistNode'headMemory = headMemory
                      , plistNode'tail = tailRoot
                      , plistNode'length = pdata len
                      , plistNode'payloadCborLength = pdata payload
                      , plistNode'memory = pdata memory
                      }
                )
                $ \node ->
                  pcon $
                    PDataSequenceSummaryV1
                      { pseq'root = pdata (phashDataListNodeV1 # node)
                      , pseq'length = pdata len
                      , pseq'payloadCborLength = pdata payload
                      , pseq'memory = pdata memory
                      }

-- | Aiken @empty_data_pair_summary_v1@.
pemptyDataPairSummaryV1 :: forall (s :: S). Term s PDataSequenceSummaryV1
pemptyDataPairSummaryV1 =
  pcon $
    PDataSequenceSummaryV1
      { pseq'root = pdata pemptyDataPairRootV1
      , pseq'length = pdata 0
      , pseq'payloadCborLength = pdata 0
      , pseq'memory = pdata 0
      }

-- | Aiken @prepend_data_pair_summary_v1@.
pprependDataPairSummaryV1 ::
  forall (s :: S).
  Term
    s
    ( PDataSummaryV1
        :--> PDataSummaryV1
        :--> PDataSequenceSummaryV1
        :--> PDataSequenceSummaryV1
    )
pprependDataPairSummaryV1 = phoistAcyclic $
  plam $ \keySummary valueSummary tailSummary ->
    pmatch keySummary $ \(PDataSummaryV1 keyRoot keyCborLength keyMemory) ->
      pmatch valueSummary $ \(PDataSummaryV1 valueRoot valueCborLength valueMemory) ->
        pmatch tailSummary $ \(PDataSequenceSummaryV1 tailRoot tailLength tailPayload tailMemory) ->
          plet (pfromData tailLength + 1) $ \len ->
            plet
              ( pfromData keyCborLength
                  + pfromData valueCborLength
                  + pfromData tailPayload
              )
              $ \payload ->
                plet
                  (pfromData keyMemory + pfromData valueMemory + pfromData tailMemory)
                  $ \memory ->
                    plet
                      ( pcon $
                          PDataPairNodeV1
                            { ppairNode'key = keyRoot
                            , ppairNode'keyCborLength = keyCborLength
                            , ppairNode'keyMemory = keyMemory
                            , ppairNode'value = valueRoot
                            , ppairNode'valueCborLength = valueCborLength
                            , ppairNode'valueMemory = valueMemory
                            , ppairNode'tail = tailRoot
                            , ppairNode'length = pdata len
                            , ppairNode'payloadCborLength = pdata payload
                            , ppairNode'memory = pdata memory
                            }
                      )
                      $ \node ->
                        pcon $
                          PDataSequenceSummaryV1
                            { pseq'root = pdata (phashDataPairNodeV1 # node)
                            , pseq'length = pdata len
                            , pseq'payloadCborLength = pdata payload
                            , pseq'memory = pdata memory
                            }

{- | Aiken @small_constr_data_summary_v1@.

The @2@/@3@ split is the CBOR tag: alternatives 0–6 encode as tags 121–127, one
byte, and everything above that reaches into the 1280+ range, two bytes.
-}
psmallConstrDataSummaryV1 ::
  forall (s :: S).
  Term s (PInteger :--> PDataSequenceSummaryV1 :--> PDataSummaryV1)
psmallConstrDataSummaryV1 = phoistAcyclic $
  plam $ \constructor fields ->
    pif (0 #<= constructor #&& constructor #<= 127) `flip` perror $
      pmatch fields $ \(PDataSequenceSummaryV1 fieldsRoot fieldsLength fieldsPayload fieldsMemory) ->
        plet (plistCborLength # pfromData fieldsLength # pfromData fieldsPayload) $ \fieldsCborLength ->
          psummaryOfNode $
            pcon $
              PConstrSmallData
                { pnode'constructor = pdata constructor
                , pnode'fieldsCount = fieldsLength
                , pnode'fieldsRoot = fieldsRoot
                , pnode'cborLength =
                    pdata $
                      pif (constructor #<= 6) (2 + fieldsCborLength) (3 + fieldsCborLength)
                , pnode'memory = pdata (4 + pfromData fieldsMemory)
                }

-- | Aiken @large_constr_data_summary_v1@.
plargeConstrDataSummaryV1 ::
  forall (s :: S).
  Term
    s
    ( PInteger
        :--> PByteString
        :--> PDataSequenceSummaryV1
        :--> PDataSummaryV1
    )
plargeConstrDataSummaryV1 = phoistAcyclic $
  plam $ \constructor constructorCborRoot fields ->
    pif (127 #< constructor) `flip` perror $
      plargeConstrDataSummaryFromCborV1
        # constructorCborRoot
        # (plengthBS #$ pserialiseData #$ pforgetData (pdata constructor))
        # (4 + pintegerMemorySize # constructor)
        # fields

{- | Aiken @large_constr_data_summary_from_cbor_v1@.

Builds the same node from the integer submachine's fixed-size summary rather
than from the constructor index itself, so a frame proof never has to reveal an
arbitrarily large alternative.
-}
plargeConstrDataSummaryFromCborV1 ::
  forall (s :: S).
  Term
    s
    ( PByteString
        :--> PInteger
        :--> PInteger
        :--> PDataSequenceSummaryV1
        :--> PDataSummaryV1
    )
plargeConstrDataSummaryFromCborV1 = phoistAcyclic $
  plam $ \constructorCborRoot constructorCborLength constructorMemory fields ->
    pif (plengthBS # constructorCborRoot #== 32) `flip` perror $
      pif (0 #< constructorCborLength) `flip` perror $
        pif (constructorCborLength #<= puint32Max) `flip` perror $
          pif (5 #<= constructorMemory) `flip` perror $
            pif (constructorMemory #<= puint64Max) `flip` perror $
              pmatch fields $ \(PDataSequenceSummaryV1 fieldsRoot fieldsLength fieldsPayload fieldsMemory) ->
                psummaryOfNode $
                  pcon $
                    PConstrLargeData
                      { pnode'constructorCborRoot = pdata constructorCborRoot
                      , pnode'constructorCborLength = pdata constructorCborLength
                      , pnode'constructorMemory = pdata constructorMemory
                      , pnode'fieldsCount = fieldsLength
                      , pnode'fieldsRoot = fieldsRoot
                      , pnode'cborLength =
                          pdata $
                            3
                              + constructorCborLength
                              + (plistCborLength # pfromData fieldsLength # pfromData fieldsPayload)
                      , pnode'memory = pdata (4 + pfromData fieldsMemory)
                      }

-- | Aiken @list_data_summary_v1@.
plistDataSummaryV1 ::
  forall (s :: S). Term s (PDataSequenceSummaryV1 :--> PDataSummaryV1)
plistDataSummaryV1 = phoistAcyclic $
  plam $ \items ->
    pmatch items $ \(PDataSequenceSummaryV1 root len payload memory) ->
      psummaryOfNode $
        pcon $
          PListDataNode
            { pnode'itemsCount = len
            , pnode'itemsRoot = root
            , pnode'cborLength = pdata (plistCborLength # pfromData len # pfromData payload)
            , pnode'memory = pdata (4 + pfromData memory)
            }

-- | Aiken @map_data_summary_v1@.
pmapDataSummaryV1 ::
  forall (s :: S). Term s (PDataSequenceSummaryV1 :--> PDataSummaryV1)
pmapDataSummaryV1 = phoistAcyclic $
  plam $ \entries ->
    pmatch entries $ \(PDataSequenceSummaryV1 root len payload memory) ->
      psummaryOfNode $
        pcon $
          PMapDataNode
            { pnode'entriesCount = len
            , pnode'entriesRoot = root
            , pnode'cborLength = pdata (pmapCborLength # pfromData len # pfromData payload)
            , pnode'memory = pdata (4 + pfromData memory)
            }

-- | Aiken @integer_data_summary_v1@.
pintegerDataSummaryV1 ::
  forall (s :: S). Term s (PInteger :--> PByteString :--> PDataSummaryV1)
pintegerDataSummaryV1 = phoistAcyclic $
  plam $ \integer cborRoot ->
    pif (plengthBS # cborRoot #== 32) `flip` perror $
      plet (plengthBS #$ pserialiseData #$ pforgetData (pdata integer)) $ \cborLength ->
        plet
          ( pcon $
              PIntegerDataNode
                { pnode'cborRoot = pdata cborRoot
                , pnode'cborLength = pdata cborLength
                , pnode'memory = pdata (4 + pintegerMemorySize # integer)
                }
          )
          $ \node ->
            pcon $
              PDataSummaryV1
                { psummary'root = pdata (phashDataNodeV1 # node)
                , psummary'cborLength = pdata cborLength
                , psummary'memory = pdata (pdataNodeMemoryV1 # node)
                }

{- | Aiken @bytes_data_summary_v1@.

The empty byte string costs one word, not none: Plutus charges a minimum of one
for a byte string of any length, which is why the zero case is written out.
-}
pbytesDataSummaryV1 ::
  forall (s :: S). Term s (PInteger :--> PByteString :--> PDataSummaryV1)
pbytesDataSummaryV1 = phoistAcyclic $
  plam $ \bytesLength bytesRoot ->
    pif (0 #<= bytesLength) `flip` perror $
      pif (plengthBS # bytesRoot #== 32) `flip` perror $
        psummaryOfNode $
          pcon $
            PBytesDataNode
              { pnode'bytesRoot = pdata bytesRoot
              , pnode'bytesLength = pdata bytesLength
              , pnode'cborLength = pdata (pbytesDataCborLengthV1 # bytesLength)
              , pnode'memory = pdata (4 + pif (bytesLength #== 0) 1 bytesLength)
              }

{- | Aiken @commit_data_list@.

Takes the semantic summariser as a parameter rather than calling it by name.
The two are mutually recursive, and a cycle among top-level Plutarch term
definitions is an infinite value: it type-checks, compiles, and then exhausts
memory the moment the term is built. The knot is tied once, in
'psemanticDataSummaryV1', with 'pfix'.
-}
pcommitDataList ::
  forall (s :: S).
  Term
    s
    ( (PData :--> PDataSummaryV1)
        :--> PBuiltinList PData
        :--> PDataSequenceSummaryV1
    )
pcommitDataList = phoistAcyclic $
  pfix $ \self -> plam $ \summarise items ->
    pelimList
      ( \headItem tailItems ->
          pprependDataListSummaryV1
            # (summarise # headItem)
            # (self # summarise # tailItems)
      )
      pemptyDataListSummaryV1
      items

-- | Aiken @commit_data_pairs@ — the same for a map's entries.
pcommitDataPairs ::
  forall (s :: S).
  Term
    s
    ( (PData :--> PDataSummaryV1)
        :--> PBuiltinList (PBuiltinPair PData PData)
        :--> PDataSequenceSummaryV1
    )
pcommitDataPairs = phoistAcyclic $
  pfix $ \self -> plam $ \summarise entries ->
    pelimList
      ( \entry rest ->
          pmatch entry $ \(PBuiltinPair key value) ->
            pprependDataPairSummaryV1
              # (summarise # key)
              # (summarise # value)
              # (self # summarise # rest)
      )
      pemptyDataPairSummaryV1
      entries

{- | Aiken @semantic_data_summary_v1@ — the whole-value commitment.

Unlike the shape predicates, the five branches here are entire subtree
commitments, so they are delayed: 'pchooseData' evaluates all six of its
arguments, and an eager map branch would run on an integer.

This is the direct commitment, for a payload that already fits one proof. Larger
byte and integer leaves are committed by the streaming blob proof instead, so
this helper never becomes a whole-value consensus cap.
-}
psemanticDataSummaryV1 :: forall (s :: S). Term s (PData :--> PDataSummaryV1)
psemanticDataSummaryV1 = phoistAcyclic $
  pfix $ \self -> plam $ \d ->
    plet (plengthBS #$ pserialiseData # d) $ \cborLength ->
      plet (psemanticNode self d cborLength) $ \node ->
        pcon $
          PDataSummaryV1
            { psummary'root = pdata (phashDataNodeV1 # node)
            , psummary'cborLength = pdata cborLength
            , psummary'memory = pdata (pdataNodeMemoryV1 # node)
            }

-- | The five-way dispatch of 'psemanticDataSummaryV1'.
psemanticNode ::
  forall (s :: S).
  Term s (PData :--> PDataSummaryV1) ->
  Term s PData ->
  Term s PInteger ->
  Term s PDataNodeV1
psemanticNode summarise d cborLength =
  pforce $
    pchooseData
      # d
      # pdelay
        ( pmatch (pasConstr # d) $ \(PBuiltinPair constructor fields) ->
            pmatch (pcommitDataList # summarise # fields) $ \(PDataSequenceSummaryV1 fieldsRoot fieldsLength _ fieldsMemory) ->
              plet (pdata (4 + pfromData fieldsMemory)) $ \memory ->
                pif
                  (constructor #<= 127)
                  ( pcon $
                      PConstrSmallData
                        { pnode'constructor = pdata constructor
                        , pnode'fieldsCount = fieldsLength
                        , pnode'fieldsRoot = fieldsRoot
                        , pnode'cborLength = pdata cborLength
                        , pnode'memory = memory
                        }
                  )
                  ( plet (pserialiseData #$ pforgetData (pdata constructor)) $ \constructorCbor ->
                      pcon $
                        PConstrLargeData
                          { pnode'constructorCborRoot = pdata (pboundedBlobRootV1 # constructorCbor)
                          , pnode'constructorCborLength = pdata (plengthBS # constructorCbor)
                          , pnode'constructorMemory = pdata (4 + pintegerMemorySize # constructor)
                          , pnode'fieldsCount = fieldsLength
                          , pnode'fieldsRoot = fieldsRoot
                          , pnode'cborLength = pdata cborLength
                          , pnode'memory = memory
                          }
                  )
        )
      # pdelay
        ( pmatch (pcommitDataPairs # summarise # (pasMap # d)) $ \(PDataSequenceSummaryV1 root len _ memory) ->
            pcon $
              PMapDataNode
                { pnode'entriesCount = len
                , pnode'entriesRoot = root
                , pnode'cborLength = pdata cborLength
                , pnode'memory = pdata (4 + pfromData memory)
                }
        )
      # pdelay
        ( pmatch (pcommitDataList # summarise # (pasList # d)) $ \(PDataSequenceSummaryV1 root len _ memory) ->
            pcon $
              PListDataNode
                { pnode'itemsCount = len
                , pnode'itemsRoot = root
                , pnode'cborLength = pdata cborLength
                , pnode'memory = pdata (4 + pfromData memory)
                }
        )
      # pdelay
        ( pcon $
            PIntegerDataNode
              { pnode'cborRoot = pdata (pboundedBlobRootV1 #$ pserialiseData # d)
              , pnode'cborLength = pdata cborLength
              , pnode'memory = pdata (4 + pintegerMemorySize # (pasInt # d))
              }
        )
      # pdelay
        ( plet (pasByteStr # d) $ \bytes ->
            plet (plengthBS # bytes) $ \bytesLength ->
              pcon $
                PBytesDataNode
                  { pnode'bytesRoot = pdata (pboundedBlobRootV1 # bytes)
                  , pnode'bytesLength = pdata bytesLength
                  , pnode'cborLength = pdata cborLength
                  , pnode'memory = pdata (4 + pif (bytesLength #== 0) 1 bytesLength)
                  }
        )

-- | Aiken @hash_semantic_data_v1@.
phashSemanticDataV1 :: forall (s :: S). Term s (PData :--> PByteString)
phashSemanticDataV1 = phoistAcyclic $
  plam $ \d ->
    pmatch (psemanticDataSummaryV1 # d) $ \(PDataSummaryV1 root _ _) ->
      pfromData root
