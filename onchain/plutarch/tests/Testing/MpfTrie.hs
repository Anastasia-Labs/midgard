{- | The MPF trie, rebuilt from the format.

@plutarch-onchain-lib@ implements the Merkle Patricia Forestry walk that every
counted-root proof in Midgard ultimately bottoms out in. This module is the
independent reference the tests check it against: the branch hashes, the path
nibbling, the suffix sentinels, and the roots of one- and two-entry tries — all
derived from the format rather than from the implementation.

It lives on its own because more than one test module needs a real tree. A fault
about /adjacency/ cannot be stated over two separate one-entry trees, and a fault
about a source cannot be stated over a tree the header does not commit, so the
fixtures those tests need are real tries or nothing.
-}
module Testing.MpfTrie (
  -- * Roots
  singleEntryRoot,
  twoLeafRoot,
  twoLeafProof,
  emptyMerkleRoot,
  libraryNullHash,

  -- * The walk
  combine,
  nibble,
  nibbles,
  commonNibbles,
  suffix,
  sparseMerkle16,
) where

import Data.ByteString qualified as BS
import PlutusCore.Data qualified as PD
import PlutusTx.Builtins qualified as Builtins
import PlutusTx.Builtins (fromBuiltin, toBuiltin)

blake2b256 :: BS.ByteString -> BS.ByteString
blake2b256 = fromBuiltin . Builtins.blake2b_256 . toBuiltin

{- | The raw MPF root of a one-entry tree.

With an empty proof the walk reduces to @combine(suffix(path, 0), blake2b_256
(value))@, and @suffix@ at cursor 0 is @0xff@ followed by the whole path.
-}
singleEntryRoot :: BS.ByteString -> BS.ByteString -> BS.ByteString
singleEntryRoot keyBytes valueBytes =
  blake2b256 (BS.cons 0xff (blake2b256 keyBytes <> blake2b256 valueBytes))


{- | @env.empty_merkle_tree_root@.

__Not__ the MPF library's null hash. Midgard's empty-tree sentinel is
@blake2b_256("")@, while @plutarch-onchain-lib@'s @empty@ carries 32 zero bytes —
which is exactly why @transition_trace.mpf_from_midgard_root@ exists, and why a
verifier that compared a walk's output against the /Midgard/ root rather than the
translated one would reject every proof about an empty tree. It did, until this
suite was written.
-}
emptyMerkleRoot :: BS.ByteString
emptyMerkleRoot = blake2b256 ""

-- | The library's own empty root, for the contrast above.
libraryNullHash :: BS.ByteString
libraryNullHash = BS.replicate 32 0x00


{- | The root of a trie holding exactly two entries.

Derived by running the @including@ walk for one leaf by hand: with a single
@Leaf@ step skipping the @d@ nibbles the two paths share,

@
root = combine(nibbles(path, 0, d),
               sparse_merkle_16(nibble(path, d),
                                combine(suffix(path, d+1), blake2b_256(value)),
                                nibble(other, d),
                                combine(suffix(other, d+1), blake2b_256(otherValue))))
@

and it comes out the same whichever leaf you start from, which is the property a
two-leaf trie has to have.
-}
twoLeafRoot :: (BS.ByteString, BS.ByteString) -> (BS.ByteString, BS.ByteString) -> BS.ByteString
twoLeafRoot (keyA, valueA) (keyB, valueB) =
  combine
    (nibbles pathA 0 d)
    ( sparseMerkle16
        (nibble pathA d)
        (combine (suffix pathA (d + 1)) (blake2b256 valueA))
        (nibble pathB d)
        (combine (suffix pathB (d + 1)) (blake2b256 valueB))
    )
  where
    pathA = blake2b256 keyA
    pathB = blake2b256 keyB
    d = commonNibbles pathA pathB

-- | The number of leading nibbles two paths share.
commonNibbles :: BS.ByteString -> BS.ByteString -> Int
commonNibbles a b = length (takeWhile id [nibble a i == nibble b i | i <- [0 .. 63]])

combine :: BS.ByteString -> BS.ByteString -> BS.ByteString
combine left right = blake2b256 (left <> right)

-- | One nibble of a path, high half first.
nibble :: BS.ByteString -> Int -> Int
nibble path i
  | even i = fromIntegral (BS.index path (i `div` 2)) `div` 16
  | otherwise = fromIntegral (BS.index path (i `div` 2)) `mod` 16

-- | A run of nibbles, one per byte.
nibbles :: BS.ByteString -> Int -> Int -> BS.ByteString
nibbles path start end = BS.pack [fromIntegral (nibble path i) | i <- [start .. end - 1]]

{- | The remainder of a path from a nibble cursor.

@0xff@ then the remaining bytes when the cursor is even; @0x00@, the odd nibble,
then the remaining bytes when it is odd. The two sentinels are what keep an
odd-aligned suffix from colliding with an even-aligned one.
-}
suffix :: BS.ByteString -> Int -> BS.ByteString
suffix path cursor
  | even cursor = BS.cons 0xff (BS.drop (cursor `div` 2) path)
  | otherwise =
      BS.cons 0x00 (BS.cons (fromIntegral (nibble path cursor)) (BS.drop ((cursor + 1) `div` 2) path))

nullHash :: BS.ByteString
nullHash = BS.replicate 32 0x00

nullHash2, nullHash4, nullHash8 :: BS.ByteString
nullHash2 = combine nullHash nullHash
nullHash4 = combine nullHash2 nullHash2
nullHash8 = combine nullHash4 nullHash4

-- | A sixteen-way branch holding exactly two children, at @me@ and @neighbour@.
sparseMerkle16 :: Int -> BS.ByteString -> Int -> BS.ByteString -> BS.ByteString
sparseMerkle16 me meHash neighbour neighbourHash
  | me < 8 && neighbour < 8 = combine (sparseMerkle8 me meHash neighbour neighbourHash) nullHash8
  | me < 8 =
      combine
        (merkle8 me meHash nullHash4 nullHash2 nullHash)
        (merkle8 (neighbour - 8) neighbourHash nullHash4 nullHash2 nullHash)
  | neighbour >= 8 =
      combine nullHash8 (sparseMerkle8 (me - 8) meHash (neighbour - 8) neighbourHash)
  | otherwise =
      combine
        (merkle8 neighbour neighbourHash nullHash4 nullHash2 nullHash)
        (merkle8 (me - 8) meHash nullHash4 nullHash2 nullHash)

sparseMerkle8 :: Int -> BS.ByteString -> Int -> BS.ByteString -> BS.ByteString
sparseMerkle8 me meHash neighbour neighbourHash
  | me < 4 && neighbour < 4 = combine (sparseMerkle4 me meHash neighbour neighbourHash) nullHash4
  | me < 4 =
      combine
        (merkle4 me meHash nullHash2 nullHash)
        (merkle4 (neighbour - 4) neighbourHash nullHash2 nullHash)
  | neighbour >= 4 =
      combine nullHash4 (sparseMerkle4 (me - 4) meHash (neighbour - 4) neighbourHash)
  | otherwise =
      combine
        (merkle4 neighbour neighbourHash nullHash2 nullHash)
        (merkle4 (me - 4) meHash nullHash2 nullHash)

sparseMerkle4 :: Int -> BS.ByteString -> Int -> BS.ByteString -> BS.ByteString
sparseMerkle4 me meHash neighbour neighbourHash
  | me < 2 && neighbour < 2 = combine (merkle2 me meHash neighbourHash) nullHash2
  | me < 2 = combine (merkle2 me meHash nullHash) (merkle2 (neighbour - 2) neighbourHash nullHash)
  | neighbour >= 2 = combine nullHash2 (merkle2 (me - 2) meHash neighbourHash)
  | otherwise =
      combine (merkle2 neighbour neighbourHash nullHash) (merkle2 (me - 2) meHash nullHash)

merkle8 :: Int -> BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString
merkle8 branch root n4 n2 n1
  | branch <= 3 = combine (merkle4 branch root n2 n1) n4
  | otherwise = combine n4 (merkle4 (branch - 4) root n2 n1)

merkle4 :: Int -> BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString
merkle4 branch root n2 n1
  | branch <= 1 = combine (merkle2 branch root n1) n2
  | otherwise = combine n2 (merkle2 (branch - 2) root n1)

merkle2 :: Int -> BS.ByteString -> BS.ByteString -> BS.ByteString
merkle2 branch root neighbour
  | branch <= 0 = combine root neighbour
  | otherwise = combine neighbour root

{- | The one-step inclusion proof for one leaf of a two-leaf trie.

A single @Leaf@ step naming the /other/ leaf's path and value digest, skipping
the nibbles the two paths share. This is what makes a two-entry fixture worth
building: with one entry per tree every proof verifies against its own root, which
is exactly the forgery a same-tree check exists to stop.
-}
twoLeafProof ::
  (BS.ByteString, BS.ByteString) -> (BS.ByteString, BS.ByteString) -> PD.Data
twoLeafProof (keyA, _) (keyB, valueB) =
  PD.List
    [ PD.Constr
        2
        [ PD.I (fromIntegral (commonNibbles pathA pathB))
        , PD.B pathB
        , PD.B (blake2b256 valueB)
        ]
    ]
  where
    pathA = blake2b256 keyA
    pathB = blake2b256 keyB
