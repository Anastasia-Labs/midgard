module Types.Membership (
  PMerkleMembershipRedeemer (..),
  MerkleMembershipRedeemer (..),
  PMerkleNonMembershipRedeemer (..),
  MerkleNonMembershipRedeemer (..),
) where

import Control.Monad (guard)
import Data.List (uncons)
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP

import Plutarch.MerkleTree.PatriciaForestry (
  MerklePatriciaForestry,
  PMerklePatriciaForestry,
  PProof,
  ProofStep (..),
 )
import Plutarch.Prelude
import PlutusCore.Data qualified as PD
import PlutusLedgerApi.V3 (
  BuiltinByteString,
  FromData (fromBuiltinData),
  ToData (toBuiltinData),
  builtinDataToData,
  dataToBuiltinData,
 )
import PlutusTx qualified

data MerkleMembershipRedeemer = MerkleMembershipRedeemer
  { mmInputRoot :: MerklePatriciaForestry
  , mmInputKey :: BuiltinByteString
  , mmInputValue :: BuiltinByteString
  , mmInputProof :: [ProofStep]
  }
  deriving stock (Show, Eq, Generic)

instance PlutusTx.ToData MerkleMembershipRedeemer where
  toBuiltinData MerkleMembershipRedeemer {mmInputRoot, mmInputKey, mmInputValue, mmInputProof} =
    dataToBuiltinData $
      PD.List
        [ PlutusTx.toData mmInputRoot
        , PlutusTx.toData mmInputKey
        , PlutusTx.toData mmInputValue
        , PlutusTx.toData mmInputProof
        ]

instance PlutusTx.FromData MerkleMembershipRedeemer where
  fromBuiltinData :: PlutusTx.BuiltinData -> Maybe MerkleMembershipRedeemer
  fromBuiltinData (builtinDataToData -> PD.List dataList) = do
    (rawInputRoot, rest) <- uncons dataList
    mmInputRoot <- PlutusTx.fromData rawInputRoot
    (rawInputKey, rest) <- uncons rest
    mmInputKey <- PlutusTx.fromData rawInputKey
    (rawInputValue, rest) <- uncons rest
    mmInputValue <- PlutusTx.fromData rawInputValue
    (rawInputProof, rest) <- uncons rest
    mmInputProof <- PlutusTx.fromData rawInputProof
    guard $ null rest
    pure $ MerkleMembershipRedeemer {mmInputRoot, mmInputKey, mmInputValue, mmInputProof}
  fromBuiltinData _ = Nothing

data MerkleNonMembershipRedeemer = MerkleNonMembershipRedeemer
  { mnmInputRoot :: MerklePatriciaForestry
  , mnmInputKey :: BuiltinByteString
  , mnmInputProof :: [ProofStep]
  }
  deriving stock (Show, Eq, Generic)

instance PlutusTx.ToData MerkleNonMembershipRedeemer where
  toBuiltinData MerkleNonMembershipRedeemer {mnmInputRoot, mnmInputKey, mnmInputProof} =
    dataToBuiltinData $
      PD.List
        [ PlutusTx.toData mnmInputRoot
        , PlutusTx.toData mnmInputKey
        , PlutusTx.toData mnmInputProof
        ]

instance PlutusTx.FromData MerkleNonMembershipRedeemer where
  fromBuiltinData :: PlutusTx.BuiltinData -> Maybe MerkleNonMembershipRedeemer
  fromBuiltinData (builtinDataToData -> PD.List dataList) = do
    (rawInputRoot, rest) <- uncons dataList
    mnmInputRoot <- PlutusTx.fromData rawInputRoot
    (rawInputKey, rest) <- uncons rest
    mnmInputKey <- PlutusTx.fromData rawInputKey
    (rawInputProof, rest) <- uncons rest
    mnmInputProof <- PlutusTx.fromData rawInputProof
    guard $ null rest
    pure $ MerkleNonMembershipRedeemer {mnmInputRoot, mnmInputKey, mnmInputProof}
  fromBuiltinData _ = Nothing

data PMerkleMembershipRedeemer (s :: S) = PMerkleMembershipRedeemer
  { pmmInputRoot :: Term s (PAsData PMerklePatriciaForestry)
  , pmmInputKey :: Term s (PAsData PByteString)
  , pmmInputValue :: Term s (PAsData PByteString)
  , pmmInputProof :: Term s (PAsData PProof)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PShow, PEq)
  deriving (PlutusType) via (DeriveAsDataRec PMerkleMembershipRedeemer)

deriving via
  DeriveDataPLiftable (PAsData PMerkleMembershipRedeemer) MerkleMembershipRedeemer
  instance
    PLiftable PMerkleMembershipRedeemer

data PMerkleNonMembershipRedeemer (s :: S) = PMerkleNonMembershipRedeemer
  { pmnmInputRoot :: Term s (PAsData PMerklePatriciaForestry)
  , pmnmInputKey :: Term s (PAsData PByteString)
  , pmnmInputProof :: Term s (PAsData PProof)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PShow, PEq)
  deriving (PlutusType) via (DeriveAsDataRec PMerkleNonMembershipRedeemer)

deriving via
  DeriveDataPLiftable (PAsData PMerkleNonMembershipRedeemer) MerkleNonMembershipRedeemer
  instance
    PLiftable PMerkleNonMembershipRedeemer
