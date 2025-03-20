{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module MerkleTree.Helpers where

import Plutarch.Crypto (pblake2b_256)
import Plutarch.Prelude
import Data.ByteString qualified as BS 
import Midgard.Utils ((#>=))

-- Combine two ByteArrays using blake2b_256 hash
pcombine :: Term s (PByteString :--> PByteString :--> PByteString)
pcombine = phoistAcyclic $ plam $ \left right ->
  pblake2b_256 # (left <> right)

-- Calculate suffix of a path
psuffix :: Term s (PByteString :--> PInteger :--> PByteString)
psuffix = phoistAcyclic $ plam $ \path cursor -> 
  pif 
    (pmod # cursor # 2 #== 0)
    (pconsBS # 0xff # (pdropBS # (pdiv # cursor # 2) # path))
    (
      pconsBS # 0 # (pconsBS # (pnibble # path # cursor) # (pdropBS # (pdiv # (cursor + 1) # 2) # path))
    )

-- Calculate nibbles for a branch node
pnibbles :: Term s (PByteString :--> PInteger :--> PInteger :--> PByteString)
pnibbles = phoistAcyclic $ plam $ \path start end ->
  (pfix #$ plam (\self s ->
      pif 
        (s #>= end)
        (pconstant BS.empty)
        (pconsBS # (pnibble # path # s) # (self # (s + 1)))
      )
  ) # start

-- Calculate a single nibble
pnibble :: Term s (PByteString :--> PInteger :--> PInteger)
pnibble = phoistAcyclic $ plam $ \self index ->
  pif 
    (pmod # index # 2 #== 0)
    (pdiv # (pindexBS # self # (pdiv # index # 2)) # 16)
    (pmod # (pindexBS # self # (pdiv # index # 2)) # 16)

-- Helper functions

pdropBS :: Term s (PInteger :--> PByteString :--> PByteString)
pdropBS = phoistAcyclic $ plam $ \n bs -> 
  psliceBS # n # (plengthBS # bs - n) # bs 
