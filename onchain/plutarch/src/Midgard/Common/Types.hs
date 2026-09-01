{- |
Module      : Midgard.Common.Types
Description : Plutarch port of @lib/midgard/common/types.ak@.

Every declaration in the Aiken original is a type alias, so each one becomes a
Haskell type synonym here. Keeping them as synonyms rather than newtypes is
deliberate: a newtype would change nothing at the Plutus data level but would
force conversions at every use site, and the whole point of these names is that
they are transparent to the wire format.
-}
module Midgard.Common.Types (
  PEmpty,
  PPosixTime,
  PPosixTimeDuration,
  PValuePairs,
  PH28,
  PH32,
  PMerkleRoot,
  PProof,
) where

import Plutarch.LedgerApi.AssocMap (PAssocMap)
import Plutarch.LedgerApi.Value (PCurrencySymbol, PTokenName)
import Plutarch.Prelude

import Midgard.MpfProof.Types (PProof)

-- | Aiken @Empty = ByteArray@.
type PEmpty = PByteString

{- | Aiken @PosixTime = Int@: milliseconds since 1970-01-01T00:00:00Z.

Note this is a bare integer, not @Plutarch.LedgerApi.V3.PPosixTime@. Aiken's
alias carries no newtype wrapper, and using the ledger newtype here would be a
silent departure from the Aiken encoding at every call site.
-}
type PPosixTime = PInteger

-- | Aiken @PosixTimeDuration = Int@: a millisecond delta applied to a 'PPosixTime'.
type PPosixTimeDuration = PInteger

-- | Aiken @ValuePairs = Pairs<PolicyId, Pairs<AssetName, Int>>@.
type PValuePairs = PAssocMap PCurrencySymbol (PAssocMap PTokenName PInteger)

-- | Aiken @H28<data>@: a 28-byte Blake2b-224 digest.
type PH28 = PByteString

-- | Aiken @H32<data>@: a 32-byte Blake2b-256 digest.
type PH32 = PByteString

-- | Aiken @MerkleRoot<key, value>@: a 32-byte Blake2b-256 Merkle root.
type PMerkleRoot = PH32
