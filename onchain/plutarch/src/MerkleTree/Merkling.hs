{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module MerkleTree.Merkling where 

import Plutarch.Prelude
import Data.ByteString qualified as BS
import MerkleTree.Helpers
import Midgard.Utils ((#>=))

-- Constants

pnull_hash :: Term s PByteString
pnull_hash = pconstant $ BS.pack $ replicate 32 0

pnull_hash_2 :: Term s PByteString
pnull_hash_2 = pcombine # pnull_hash # pnull_hash

pnull_hash_4 :: Term s PByteString
pnull_hash_4 = pcombine # pnull_hash_2 # pnull_hash_2

pnull_hash_8 :: Term s PByteString
pnull_hash_8 = pcombine # pnull_hash_4 # pnull_hash_4

-- Merkle functions

pmerkle_16 :: Term s (PInteger :--> PByteString :--> PByteString :--> PByteString :--> PByteString :--> PByteString :--> PByteString)
pmerkle_16 = phoistAcyclic $ plam $ \branch root neighbor_8 neighbor_4 neighbor_2 neighbor_1 ->
  pif (branch #<= 7)
    (pcombine # (pmerkle_8 # branch # root # neighbor_4 # neighbor_2 # neighbor_1) # neighbor_8)
    (pcombine # neighbor_8 # (pmerkle_8 # (branch - 8) # root # neighbor_4 # neighbor_2 # neighbor_1))

pmerkle_8 :: Term s (PInteger :--> PByteString :--> PByteString :--> PByteString :--> PByteString :--> PByteString)
pmerkle_8 = phoistAcyclic $ plam $ \branch root neighbor_4 neighbor_2 neighbor_1 ->
  pif (branch #<= 3)
    (pcombine # (pmerkle_4 # branch # root # neighbor_2 # neighbor_1) # neighbor_4)
    (pcombine # neighbor_4 # (pmerkle_4 # (branch - 4) # root # neighbor_2 # neighbor_1))

pmerkle_4 :: Term s (PInteger :--> PByteString :--> PByteString :--> PByteString :--> PByteString)
pmerkle_4 = phoistAcyclic $ plam $ \branch root neighbor_2 neighbor_1 ->
  pif (branch #<= 1)
    (pcombine # (pmerkle_2 # branch # root # neighbor_1) # neighbor_2)
    (pcombine # neighbor_2 # (pmerkle_2 # (branch - 2) # root # neighbor_1))

pmerkle_2 :: Term s (PInteger :--> PByteString :--> PByteString :--> PByteString)
pmerkle_2 = phoistAcyclic $ plam $ \branch root neighbor ->
  pif (branch #<= 0)
    (pcombine # root # neighbor)
    (pcombine # neighbor # root)

-- Sparse Merkle functions

psparse_merkle_16 :: Term s (PInteger :--> PByteString :--> PInteger :--> PByteString :--> PByteString)
psparse_merkle_16 = phoistAcyclic $ plam $ \me meHash neighbor neighborHash ->
  pif (me #< 8)
    (pif (neighbor #< 8)
      (pcombine # (psparse_merkle_8 # me # meHash # neighbor # neighborHash) # pnull_hash_8)
      (pcombine 
        # (pmerkle_8 # me # meHash # pnull_hash_4 # pnull_hash_2 # pnull_hash)
        # (pmerkle_8 # (neighbor - 8) # neighborHash # pnull_hash_4 # pnull_hash_2 # pnull_hash)))
    (pif (neighbor #>= 8)
      (pcombine # pnull_hash_8 # (psparse_merkle_8 # (me - 8) # meHash # (neighbor - 8) # neighborHash))
      (pcombine
        # (pmerkle_8 # neighbor # neighborHash # pnull_hash_4 # pnull_hash_2 # pnull_hash)
        # (pmerkle_8 # (me - 8) # meHash # pnull_hash_4 # pnull_hash_2 # pnull_hash)))

psparse_merkle_8 :: Term s (PInteger :--> PByteString :--> PInteger :--> PByteString :--> PByteString)
psparse_merkle_8 = phoistAcyclic $ plam $ \me meHash neighbor neighborHash ->
  pif (me #< 4)
    (pif (neighbor #< 4)
      (pcombine # (psparse_merkle_4 # me # meHash # neighbor # neighborHash) # pnull_hash_4)
      (pcombine
        # (pmerkle_4 # me # meHash # pnull_hash_2 # pnull_hash)
        # (pmerkle_4 # (neighbor - 4) # neighborHash # pnull_hash_2 # pnull_hash)))
    (pif (neighbor #>= 4)
      (pcombine # pnull_hash_4 # (psparse_merkle_4 # (me - 4) # meHash # (neighbor - 4) # neighborHash))
      (pcombine
        # (pmerkle_4 # neighbor # neighborHash # pnull_hash_2 # pnull_hash)
        # (pmerkle_4 # (me - 4) # meHash # pnull_hash_2 # pnull_hash)))

psparse_merkle_4 :: Term s (PInteger :--> PByteString :--> PInteger :--> PByteString :--> PByteString)
psparse_merkle_4 = phoistAcyclic $ plam $ \me meHash neighbor neighborHash ->
  pif (me #< 2)
    (pif (neighbor #< 2)
      (pcombine # (pmerkle_2 # me # meHash # neighborHash) # pnull_hash_2)
      (pcombine
        # (pmerkle_2 # me # meHash # pnull_hash)
        # (pmerkle_2 # (neighbor - 2) # neighborHash # pnull_hash)))
    (pif (neighbor #>= 2)
      (pcombine # pnull_hash_2 # (pmerkle_2 # (me - 2) # meHash # neighborHash))
      (pcombine
        # (pmerkle_2 # neighbor # neighborHash # pnull_hash)
        # (pmerkle_2 # (me - 2) # meHash # pnull_hash)))