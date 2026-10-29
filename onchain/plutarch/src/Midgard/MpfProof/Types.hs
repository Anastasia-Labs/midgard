{- |
Module      : Midgard.MpfProof.Types
Description : Aiken-compatible Merkle-Patricia-Forestry proof data.

The upstream Plutarch MPF library encodes a neighbour as a bare data list.
Aiken's @mpf.Neighbor@ is an ordinary single-constructor record, so its wire
format is @Constr 0 [nibble, prefix, root]@. Midgard owns these types to keep
proof-carrying redeemers and datums byte-for-byte compatible with Aiken and the
TypeScript tooling.
-}
module Midgard.MpfProof.Types (
  PNeighbor (..),
  PProofStep (..),
  PProof (..),
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Prelude

-- | Aiken @mpf.Neighbor@. The single constructor is part of the wire ABI.
data PNeighbor (s :: S) = PNeighbor
  { pneighbor'nibble :: Term s (PAsData PInteger)
  , pneighbor'prefix :: Term s (PAsData PByteString)
  , pneighbor'root :: Term s (PAsData PByteString)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PNeighbor)

-- | Aiken @mpf.ProofStep@: Branch tag 0, Fork tag 1, Leaf tag 2.
data PProofStep (s :: S)
  = PBranch
      { pproofStep'skip :: Term s (PAsData PInteger)
      , pproofStep'neighbors :: Term s (PAsData PByteString)
      }
  | PFork
      { pproofStep'skip :: Term s (PAsData PInteger)
      , pproofStep'neighbor :: Term s (PAsData PNeighbor)
      }
  | PLeaf
      { pproofStep'skip :: Term s (PAsData PInteger)
      , pproofStep'key :: Term s (PAsData PByteString)
      , pproofStep'value :: Term s (PAsData PByteString)
      }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PShow, PEq)
  deriving (PlutusType) via (DeriveAsDataStruct PProofStep)

-- | Aiken @mpf.Proof = List<ProofStep>@, transparently wrapped for Plutarch.
newtype PProof (s :: S) = PProof (Term s (PBuiltinList (PAsData PProofStep)))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PShow)
  deriving PlutusType via (DeriveNewtypePlutusType PProof)
