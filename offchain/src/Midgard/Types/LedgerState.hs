{-# LANGUAGE TemplateHaskell #-}

module Midgard.Types.LedgerState (
  HeaderHash,
  MerkleRoot,
  MidgardLedgerRoot,
  WithdrawalsRoot,
  ForcedTransactionsRoot,
  MidgardTxsRoot,
  DepositsRoot,
  EventToStepRoot,
  TransitionTraceRoot,
  ConfirmedState (..),
  Header (..),
  genesisHeaderHash,
  genesisUtxoRoot,
  genesisProtocolVersion,
) where

import Data.ByteString qualified as BS
import GHC.Generics (Generic)

import PlutusLedgerApi.Common (BuiltinData)
import PlutusLedgerApi.V3 (
  Address,
  BuiltinByteString,
  Credential,
  CurrencySymbol,
  POSIXTime,
  PubKeyHash,
  ScriptHash,
  TokenName,
  TxOutRef,
 )
import PlutusTx.Blueprint (HasBlueprintDefinition, definitionRef)
import PlutusTx.Blueprint.TH (makeIsDataSchemaIndexed)
import PlutusTx.Builtins qualified as PlutusTx

import Midgard.Constants (emptyMerkleTreeRoot)

type HeaderHash = BuiltinByteString

type MerkleRoot key value = BuiltinByteString

type MidgardAddress = Credential

type MidgardValue = [(CurrencySymbol, TokenName, Integer)]

data MidgardOutput = MidgardOutput
  { address :: MidgardAddress
  , value :: MidgardValue
  , datum :: Maybe BuiltinData
  , referenceScript :: Maybe ScriptHash
  }
  deriving stock (Eq, Show, Generic)

type MidgardLedgerRoot = MerkleRoot TxOutRef MidgardOutput

type DepositId = TxOutRef

data DepositInfo = DepositInfo
  { l2Address :: Address
  , l2NetworkId :: Integer
  , l2Datum :: Maybe BuiltinData
  }
  deriving stock (Eq, Show, Generic)

type DepositsRoot = MerkleRoot DepositId DepositInfo

type WithdrawalId = TxOutRef

data WithdrawalBody = WithdrawalBody
  { l2Outref :: TxOutRef
  , l2Owner :: PubKeyHash
  , l2Value :: MidgardValue
  , l1Address :: Address
  , l1Datum :: BuiltinData
  }
  deriving stock (Eq, Show, Generic)

type VerificationKey = BuiltinByteString

type Signature = BuiltinByteString

type WithdrawalSignature = (VerificationKey, Signature)

data WithdrawalValidity
  = WithdrawalIsValid
  | NonExistentWithdrawalUtxo
  | SpentWithdrawalUtxo {l2TxId :: MidgardTxId}
  | IncorrectWithdrawalOwner
  | IncorrectWithdrawalValue
  | IncorrectWithdrawalSignature
  | TooManyTokensInWithdrawal
  | UnpayableWithdrawalValue
  deriving stock (Eq, Show, Generic)

data WithdrawalInfo = WithdrawalInfo
  { body :: WithdrawalBody
  , signature :: WithdrawalSignature
  , validity :: WithdrawalValidity
  }
  deriving stock (Eq, Show, Generic)

type WithdrawalsRoot = MerkleRoot WithdrawalId WithdrawalInfo

type TxOrderId = TxOutRef

type MidgardTxId = BuiltinByteString

type MidgardTxWitsHash = BuiltinByteString

data MidgardTxValidity
  = TxIsValid
  | NonExistentInputUtxo
  | InvalidSignature
  | FailedScript
  | FeeTooLow
  | UnbalancedTx
  deriving stock (Eq, Show, Generic)

data MidgardNetworkId
  = Mainnet
  | Testnet
  deriving stock (Eq, Show, Generic)

data MidgardTxBodyCompact = MidgardTxBodyCompact
  { spendInputs :: BuiltinByteString
  , referenceInputs :: BuiltinByteString
  , outputs :: BuiltinByteString
  , fee :: Integer
  , validityInterval :: BuiltinData
  , requiredObservers :: BuiltinByteString
  , requiredSignerHashes :: BuiltinByteString
  , mint :: BuiltinByteString
  , scriptIntegrityHash :: BuiltinByteString
  , auxiliaryDataHash :: BuiltinByteString
  , networkId :: MidgardNetworkId
  }
  deriving stock (Eq, Show, Generic)

data MidgardTxCompactWithoutValidity = MidgardTxCompactWithoutValidity
  { body :: MidgardTxBodyCompact
  , wits :: MidgardTxWitsHash
  }
  deriving stock (Eq, Show, Generic)

data MidgardTxCompact = MidgardTxCompact
  { body :: MidgardTxBodyCompact
  , wits :: MidgardTxWitsHash
  , validity :: MidgardTxValidity
  }
  deriving stock (Eq, Show, Generic)

data ForcedInclusionTx = ForcedInclusionTx
  { txCompact :: MidgardTxCompactWithoutValidity
  , operatorValidity :: MidgardTxValidity
  }
  deriving stock (Eq, Show, Generic)

type ForcedTransactionsRoot = MerkleRoot TxOutRef ForcedInclusionTx

type MidgardTxsRoot = MerkleRoot MidgardTxId MidgardTxCompact

data TransitionPhase
  = Withdrawal
  | ForcedTransaction
  | L2Transaction
  | Deposit
  deriving stock (Eq, Show, Generic)

data EventKey
  = WithdrawalEventKey {withdrawalId :: WithdrawalId}
  | ForcedTransactionEventKey {txOrderId :: TxOrderId}
  | L2TransactionEventKey {txId :: MidgardTxId}
  | DepositEventKey {depositId :: DepositId}
  deriving stock (Eq, Show, Generic)

data EventToStepValue = EventToStepValue
  { stepIndex :: Integer
  , phase :: TransitionPhase
  }
  deriving stock (Eq, Show, Generic)

type EventToStepRoot = MerkleRoot EventKey EventToStepValue

data TransitionStep = TransitionStep
  { schemaVersion :: Integer
  , stepIndex :: Integer
  , eventKey :: EventKey
  , phase :: TransitionPhase
  , preUtxosRoot :: MidgardLedgerRoot
  , postUtxosRoot :: MidgardLedgerRoot
  }
  deriving stock (Eq, Show, Generic)

type TransitionTraceRoot = MerkleRoot Integer TransitionStep

data Header = Header
  { prevUtxosRoot :: MidgardLedgerRoot
  , utxosRoot :: MidgardLedgerRoot
  , withdrawalsRoot :: WithdrawalsRoot
  , forcedTransactionsRoot :: ForcedTransactionsRoot
  , transactionsRoot :: MidgardTxsRoot
  , depositsRoot :: DepositsRoot
  , transitionTraceRoot :: TransitionTraceRoot
  , eventToStepRoot :: EventToStepRoot
  , withdrawalCount :: Integer
  , forcedTransactionCount :: Integer
  , l2TransactionCount :: Integer
  , depositCount :: Integer
  , totalEventCount :: Integer
  , transitionStepCount :: Integer
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
  , confirmedUtxoRoot :: MidgardLedgerRoot
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

genesisUtxoRoot :: MidgardLedgerRoot
genesisUtxoRoot = PlutusTx.toBuiltin emptyMerkleTreeRoot

genesisProtocolVersion :: Integer
genesisProtocolVersion = 0
