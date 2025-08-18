{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# LANGUAGE TemplateHaskell #-}
module MerkleTree.MerklePatriciaForestry where

import Plutarch.Prelude
import Plutarch.Lift
import Plutarch.DataRepr 
import Plutarch.Crypto (pblake2b_256)
import MerkleTree.Helpers
import MerkleTree.Merkling 
import PlutusTx qualified
import PlutusLedgerApi.V2 (BuiltinByteString)

-- Constants

pblake2b_256_digest_size :: Term s PInteger
pblake2b_256_digest_size = pconstant 32

-- Merkle Patricia Forestry

newtype MerklePatriciaForestry = MerklePatriciaForestry BuiltinByteString
  deriving stock (Show, Eq, Ord, Generic)
PlutusTx.unstableMakeIsData ''MerklePatriciaForestry
PlutusTx.makeLift ''MerklePatriciaForestry

newtype PMerklePatriciaForestry (s :: S) = PMerklePatriciaForestry (Term s PByteString)
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq, PShow)

instance DerivePlutusType PMerklePatriciaForestry where type DPTStrat _ = PlutusTypeNewtype

deriving via
  (DerivePConstantViaBuiltin MerklePatriciaForestry PMerklePatriciaForestry PByteString)
  instance
    PConstantDecl MerklePatriciaForestry

instance PUnsafeLiftDecl PMerklePatriciaForestry where
  type PLifted PMerklePatriciaForestry = MerklePatriciaForestry

pfrom_root :: Term s (PByteString :--> PMerklePatriciaForestry)
pfrom_root = phoistAcyclic $ plam $ \root ->
  pif (plengthBS # root #== pblake2b_256_digest_size)
    (pcon $ PMerklePatriciaForestry root)
    perror

pempty :: Term s PMerklePatriciaForestry
pempty = pcon $ PMerklePatriciaForestry pnull_hash

pis_empty :: Term s (PMerklePatriciaForestry :--> PBool)
pis_empty = phoistAcyclic $ plam $ \self ->
  pmatch self $ \(PMerklePatriciaForestry root) ->
    root #== pnull_hash

data Neighbor = Neighbor
  { nibble :: Integer
  , prefix :: BuiltinByteString
  , root :: BuiltinByteString
  }
  deriving stock (Show, Eq, Generic)
PlutusTx.unstableMakeIsData ''Neighbor

data ProofStep
  = Branch
      { skip :: Integer
      , neighbors :: BuiltinByteString
      }
  | Fork
      { skip :: Integer
      , neighbor :: Neighbor
      }
  | Leaf
      { skip :: Integer
      , key :: BuiltinByteString
      , value :: BuiltinByteString
      }
  deriving stock (Show, Eq, Generic)
PlutusTx.unstableMakeIsData ''ProofStep 

data PProofStep (s :: S)
  = PBranch (Term s (PDataRecord '["skip" ':= PInteger, "neighbors" ':= PByteString]))
  | PFork (Term s (PDataRecord '["skip" ':= PInteger, "neighbor" ':= PNeighbor]))
  | PLeaf (Term s (PDataRecord '["skip" ':= PInteger, "key" ':= PByteString, "value" ':= PByteString]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq, PShow)

instance DerivePlutusType PProofStep where type DPTStrat _ = PlutusTypeData
instance PUnsafeLiftDecl PProofStep where
  type PLifted PProofStep = ProofStep

deriving via
  (DerivePConstantViaData ProofStep PProofStep)
  instance
    PConstantDecl ProofStep


newtype PProof (s :: S) = PProof (Term s (PBuiltinList PProofStep))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq, PShow)

instance DerivePlutusType PProof where type DPTStrat _ = PlutusTypeNewtype

data PNeighbor (s :: S) = PNeighbor 
  (Term s (PDataRecord '["nibble" ':= PInteger, "prefix" ':= PByteString, "root" ':= PByteString]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PDataFields, PIsData, PEq, PShow)

instance DerivePlutusType PNeighbor where type DPTStrat _ = PlutusTypeData
instance PUnsafeLiftDecl PNeighbor where
  type PLifted PNeighbor = Neighbor

deriving via
  (DerivePConstantViaData Neighbor PNeighbor)
  instance
    PConstantDecl Neighbor

-- Test whether an element is present in the trie with a specific value. This
-- requires a Proof of inclusion for the element. The latter can be
-- obtained off-chain from the whole trie containing the element.
--
-- Returns `False` when the element isn't in the tree.
phas :: Term s (PMerklePatriciaForestry :--> PByteString :--> PByteString :--> PProof :--> PBool)
phas = phoistAcyclic $ plam $ \self key value proof ->
  pmatch self $ \(PMerklePatriciaForestry root) ->
    pincluding # key # value # proof #== root

-- Insert an element in the trie. This requires a [Proof](#Proof) of inclusion
-- for the element. The latter can be obtained off-chain from the whole trie
-- containing the element.
--
-- #### Fails when
--
-- - The Proof is invalid.
-- - There's already an element in the trie at the given key.
pinsert :: Term s (PMerklePatriciaForestry :--> PByteString :--> PByteString :--> PProof :--> PMerklePatriciaForestry)
pinsert = phoistAcyclic $ plam $ \self key value proof ->
  pmatch self $ \(PMerklePatriciaForestry root) ->
    pif
      ((pexcluding # key # proof) #== root)
      (pcon $ PMerklePatriciaForestry (pincluding # key # value # proof))
      perror

pdelete :: Term s (PMerklePatriciaForestry :--> PByteString :--> PByteString :--> PProof :--> PMerklePatriciaForestry)
pdelete = phoistAcyclic $ plam $ \self key value proof ->
  pif
    ((pincluding # key # value # proof) #== (pto self))
    (pcon $ PMerklePatriciaForestry (pexcluding # key # proof))
    perror

pupdate :: Term s (PMerklePatriciaForestry :--> PByteString :--> PProof :--> PByteString :--> PByteString :--> PMerklePatriciaForestry)
pupdate = phoistAcyclic $ plam $ \self key proof oldValue newValue ->
  pif
    ((pincluding # key # oldValue # proof) #== (pto self))
    (pcon $ PMerklePatriciaForestry (pincluding # key # newValue # proof))
    perror

pexcluding :: Term s (PByteString  :--> PProof :--> PByteString)
pexcluding = phoistAcyclic $ plam $ \((pblake2b_256 #) -> path) proof ->
  let go :: Term _ (PInteger :--> PBuiltinList PProofStep :--> PByteString)
      go = pfix #$ plam $ \self cursor steps ->
        pmatch steps $ \case
          PNil -> pnull_hash
          PCons x xs ->
            pmatch x $ \case
              PBranch fields ->
                pletFields @'["skip", "neighbors"] fields $ \branchF ->
                  plet (cursor + 1 + branchF.skip) $ \nextCursor ->
                  let root = (self # nextCursor # xs) 
                  in pdo_branch # path # cursor # nextCursor # root # branchF.neighbors
              
              PFork fields ->
                pletFields @'["skip", "neighbor"] fields $ \forkF ->
                  pmatch xs $ \case
                    PNil ->
                      pletFields @'["prefix", "nibble", "root"] forkF.neighbor $ \neighborF ->
                        let prefix = pconsBS # neighborF.nibble # neighborF.prefix 
                        in pcombine # prefix # neighborF.root
                    PCons _ _ ->
                      plet (cursor + 1 + forkF.skip) $ \nextCursor ->
                        let root = (self # nextCursor # xs) 
                        in pdo_fork # path # cursor # nextCursor # root # forkF.neighbor
              
              PLeaf fields ->
                pletFields @'["skip", "key", "value"] fields $ \leafF ->
                  pmatch xs $ \case
                    PNil ->
                      pcombine # (psuffix # leafF.key # cursor) # leafF.value
                    PCons _ _ ->
                      plet (cursor + 1 + leafF.skip) $ \nextCursor ->
                      plet leafF.key $ \leafKey ->
                      let root = (self # nextCursor # xs) 
                          neighbor = ( pcon $ PNeighbor $
                            pdcons @"nibble" # pdata (pnibble # leafKey # cursor)
                              #$ pdcons @"prefix" # pdata (psuffix # leafKey # nextCursor)
                              #$ pdcons @"root" # leafF.value
                              #$ pdnil
                            )
                       in pdo_fork # path # cursor # nextCursor # root # neighbor
  in go # 0 # (pto proof)

-- | Compute the resulting hash digest from a 'Proof' associated with an
-- arbitrary value. If the proof is valid, the result is the root hash of
-- the target trie.
-- 
-- This can be used to check for membership of an element in a trie.
--  
pincluding :: Term s (PByteString :--> PByteString :--> PProof :--> PByteString) 
pincluding = phoistAcyclic $ plam $ \((pblake2b_256 #) -> path) ((pblake2b_256 #) -> value) proof -> 
  let go :: Term _ (PInteger :--> PBuiltinList PProofStep :--> PByteString)
      go = pfix #$ plam $ \self cursor steps ->
        pelimList (\proofStep ys -> 
          pmatch proofStep $ \case 
            PBranch fields -> 
              ptraceInfo ("branch" <> pshow cursor) $
                pletFields @'["skip", "neighbors"] fields $ \branchF -> 
                  plet (cursor + 1 + branchF.skip) $ \nextCursor ->
                    let root = self # nextCursor # ys 
                    in pdo_branch # path # cursor # nextCursor # root # branchF.neighbors 
            PFork fields ->
              pletFields @'["skip", "neighbor"] fields $ \forkF -> 
                plet (cursor + 1 + forkF.skip) $ \nextCursor ->
                  let root = self # nextCursor # ys 
                  in pdo_fork # path # cursor # nextCursor # root # forkF.neighbor 
            PLeaf fields -> 
              ptraceInfo ("leaf" <> pshow cursor) $
                pletFields @'["skip", "key", "value"] fields $ \leafF ->
                  plet (cursor + 1 + leafF.skip) $ \nextCursor -> 
                    plet leafF.key $ \key ->
                      let neighbor = pcon $
                            PNeighbor $
                              pdcons @"nibble" # pdata (pnibble # key # (nextCursor - 1))
                                #$ pdcons @"prefix" # pdata (psuffix # key # nextCursor)
                                #$ pdcons @"root" # leafF.value 
                                #$ pdnil
                          root = self # nextCursor # ys 
                      in pdo_fork # path # cursor # nextCursor # root # neighbor 
                       
        ) 
        (pcombine # (psuffix # path # cursor) # value)
        steps
   in go # 0 # (pto proof)    

pdo_branch :: Term s (PByteString :--> PInteger :--> PInteger :--> PByteString :--> PByteString :--> PByteString)
pdo_branch = phoistAcyclic $ plam $ \path cursor nextCursor root neighbors ->
  plet (pnibble # path # (nextCursor - 1)) $ \branch ->
  plet (pnibbles # path # cursor # (nextCursor - 1)) $ \prefix ->
  pcombine # prefix # 
    (pmerkle_16 # branch # root
      # (psliceBS # 0 # pblake2b_256_digest_size # neighbors)
      # (psliceBS # 32 # pblake2b_256_digest_size # neighbors)
      # (psliceBS # 64 # pblake2b_256_digest_size # neighbors)
      # (psliceBS # 96 # pblake2b_256_digest_size # neighbors))

pdo_fork :: Term s (PByteString :--> PInteger :--> PInteger :--> PByteString :--> PNeighbor :--> PByteString)
pdo_fork = phoistAcyclic $ plam $ \path cursor nextCursor root neighbor ->
  pletFields @'["nibble", "prefix", "root"] neighbor $ \neighborF -> 
  plet (pnibble # path # (nextCursor - 1)) $ \branch ->
  plet (pnibbles # path # cursor # (nextCursor - 1)) $ \prefix ->
  plet neighborF.nibble $ \neighborNibble -> 
  pif (branch #== neighborNibble)
    perror
    (pcombine # prefix #
      (psparse_merkle_16 # branch # root # 
        neighborNibble # 
        (pcombine # neighborF.prefix # neighborF.root)))



