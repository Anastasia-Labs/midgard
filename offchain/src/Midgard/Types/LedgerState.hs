{-# LANGUAGE TemplateHaskell #-}

module Midgard.Types.LedgerState (
  HeaderHash,
  MerkleRoot,
  ConfirmedState (..),
  Header (..),
  genesisHeaderHash,
  genesisUtxoRoot,
  genesisProtocolVersion,
) where

import Data.ByteString qualified as BS
import GHC.Generics (Generic)

import PlutusLedgerApi.V3 (BuiltinByteString, POSIXTime, PubKeyHash)
import PlutusTx.Blueprint (HasBlueprintDefinition, definitionRef)
import PlutusTx.Blueprint.TH (makeIsDataSchemaIndexed)
import PlutusTx.Builtins qualified as PlutusTx

import Midgard.Constants (emptyMerkleTreeRoot)

type HeaderHash = BuiltinByteString

type MerkleRoot = BuiltinByteString

data Header = Header
  { prevUtxosRoot :: MerkleRoot
  , utxosRoot :: MerkleRoot
  , transactionsRoot :: MerkleRoot
  , depositsRoot :: MerkleRoot
  , withdrawalsRoot :: MerkleRoot
  , startTime :: POSIXTime
  , endTime :: POSIXTime
  , prevHeaderHash :: HeaderHash
  , operatorVkey :: PubKeyHash
  , protocolVersion :: Integer
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (HasBlueprintDefinition)

$( makeIsDataSchemaIndexed
     ''Header
     [ ('Header, 0)
     ]
 )

data ConfirmedState = ConfirmedState
  { confirmedHeaderHash :: HeaderHash
  , confirmedPrevHeaderHash :: HeaderHash
  , confirmedUtxoRoot :: MerkleRoot
  , confirmedStartTime :: POSIXTime
  , confirmedEndTime :: POSIXTime
  , confirmedProtocolVersion :: Integer
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (HasBlueprintDefinition)

$( makeIsDataSchemaIndexed
     ''ConfirmedState
     [ ('ConfirmedState, 0)
     ]
 )

genesisHeaderHash :: HeaderHash
genesisHeaderHash = PlutusTx.toBuiltin $ BS.replicate 28 0

genesisUtxoRoot :: MerkleRoot
genesisUtxoRoot = PlutusTx.toBuiltin emptyMerkleTreeRoot

genesisProtocolVersion :: Integer
genesisProtocolVersion = 0
