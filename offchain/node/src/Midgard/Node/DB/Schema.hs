{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Midgard.Node.DB.Schema where

import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Int (Int32, Int64)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Time (UTCTime)

import Data.Aeson (Value, decodeStrict', encode)
import Database.Persist.Class (PersistField (fromPersistValue, toPersistValue))
import Database.Persist.Sql (PersistFieldSql (sqlType), PersistValue (PersistByteString, PersistText), SqlType (SqlOther))
import Database.Persist.TH (
  mkMigrate,
  mkPersist,
  persistLowerCase,
  share,
  sqlSettings,
 )

import Midgard.Node.DB.Schema.DepositConfirmationStatus
import Midgard.Node.DB.Schema.DepositsUtxosStatus
import Midgard.Node.DB.Schema.EventProjectionStatus
import Midgard.Node.DB.Schema.ForcedOperatorValidity
import Midgard.Node.DB.Schema.LocalMutationJobKind
import Midgard.Node.DB.Schema.LocalMutationJobStatus
import Midgard.Node.DB.Schema.PendingBlockFinalizationStatus
import Midgard.Node.DB.Schema.StateQueueLeaseStatus
import Midgard.Node.DB.Schema.StateQueueScope
import Midgard.Node.DB.Schema.SubmitSource
import Midgard.Node.DB.Schema.TxAdmissionStatus
import Midgard.Node.DB.Schema.WithdrawalValidity
import Midgard.Node.DB.Types

newtype Jsonb = Jsonb
  { unJsonb :: Value
  }
  deriving stock (Eq, Show)

instance PersistField Jsonb where
  toPersistValue = PersistByteString . LBS.toStrict . encode . unJsonb
  fromPersistValue = \case
    PersistByteString bytes ->
      maybe (Left "Invalid JSONB bytes") (Right . Jsonb) (decodeStrict' bytes)
    PersistText text ->
      maybe (Left "Invalid JSONB text") (Right . Jsonb) (decodeStrict' (Text.encodeUtf8 text))
    other ->
      Left ("Expected JSONB value, got " <> Text.pack (show other))

instance PersistFieldSql Jsonb where
  sqlType _ = SqlOther "JSONB"

{- | Persistent's entity syntax is the Haskell-side skeleton for the Midgard
node schema. The fields are based on the current SQL migration end-state in:

  ../demo/midgard-node/src/database/migrations/sql

Persistent covers table/column drift well. PostgreSQL-specific details that do
not fit entity declarations cleanly, such as partial indexes, enum types,
foreign-key actions, and complex CHECK constraints, should remain explicit SQL
adjunct migrations beside this schema.
-}
share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
AddressHistory
    txId TxIdPersist
    address AddressPersist
    createdAt UTCTime default=now()
    UniqueAddressHistory txId address
    deriving Eq Show

Blocks
    height Int32 sqlType=SERIAL
    headerHash HeaderHashPersist
    txId TxIdPersist
    timeStampTz UTCTime default=now()
    Primary height
    UniqueBlocksTxId txId
    deriving Eq Show

ConfirmedLedger
    txId TxIdPersist
    outref TxOutRefPersist
    output TxOutPersist
    address AddressPersist
    timeStampTz UTCTime default=now()
    Primary outref
    deriving Eq Show

LatestLedger
    txId TxIdPersist
    outref TxOutRefPersist
    output TxOutPersist
    address AddressPersist
    timeStampTz UTCTime default=now()
    Primary outref
    deriving Eq Show

DepositsUtxos
    eventId TxOutRefPersist
    eventInfo ByteString
    inclusionTime UTCTime
    depositL1TxHash TxIdPersist
    ledgerTxId TxIdPersist
    ledgerOutput TxOutPersist
    ledgerAddress AddressPersist
    projectedHeaderHash HeaderHashPersist Maybe
    status DepositsUtxosStatus
    Primary eventId
    deriving Eq Show

Immutable
    txId TxIdPersist
    tx ByteString
    timeStampTz UTCTime default=now()
    Primary txId
    deriving Eq Show

Mempool
    txId TxIdPersist
    tx ByteString
    timeStampTz UTCTime default=now()
    Primary txId
    deriving Eq Show

ProcessedMempool
    txId TxIdPersist
    tx ByteString
    timeStampTz UTCTime default=now()
    Primary txId
    deriving Eq Show

MempoolLedger
    txId TxIdPersist
    outref TxOutRefPersist
    output TxOutPersist
    address AddressPersist
    sourceEventId TxOutRefPersist Maybe
    timeStampTz UTCTime default=now()
    Primary outref
    deriving Eq Show

MempoolTxDeltas
    txId TxIdPersist
    spentCbor ByteString
    producedCbor ByteString
    Primary txId
    deriving Eq Show

TxRejections
    txId TxIdPersist
    rejectCode Text
    rejectDetail Text Maybe
    createdAt UTCTime default=now()
    deriving Eq Show

DepositIngestionCursor
    cursorName Text
    stableTipHash Text
    stableTipSlot Int64
    stableTipTimeMs Int64
    scanUpperBoundTimeMs Int64
    lastScannedEventId TxOutRefPersist
    updatedAt UTCTime default=now()
    Primary cursorName
    deriving Eq Show

PendingBlockFinalizations
    headerHash HeaderHashPersist
    submittedTxHash TxIdPersist Maybe
    blockEndTime UTCTime
    status PendingBlockFinalizationStatus
    observedConfirmedAtMs Int64 Maybe
    createdAt UTCTime default=now()
    updatedAt UTCTime default=now()
    stateQueueLeaseToken Text
    baseSnapshotId Text
    baseTailOutRef Text
    baseTailHeaderHash HeaderHashPersist
    baseTailDatumCbor Text
    baseUtxosRoot Text
    baseTransactionsRoot Text
    baseDepositsRoot Text
    baseWithdrawalsRoot Text
    blockStartTime UTCTime
    expectedUtxosRoot Text
    expectedTransactionsRoot Text
    expectedDepositsRoot Text
    expectedWithdrawalsRoot Text
    baseForcedTransactionsRoot Text
    expectedForcedTransactionsRoot Text
    headerCbor ByteString
    expectedTransitionTraceRoot Text
    expectedEventToStepRoot Text
    expectedWithdrawalCount Int64
    expectedForcedTransactionCount Int64
    expectedL2TransactionCount Int64
    expectedDepositCount Int64
    expectedTotalEventCount Int64
    expectedTransitionStepCount Int64
    Primary headerHash
    UniquePendingBlockFinalizationsSubmittedTxHash submittedTxHash !force
    deriving Eq Show

PendingBlockFinalizationDeposits
    headerHash HeaderHashPersist
    memberId TxOutRefPersist
    ordinal Int32
    payloadCbor ByteString
    payloadSha256 ByteString
    sourceTable Text
    sourceId ByteString
    sourceTimeStampTz UTCTime
    Primary headerHash memberId
    UniquePendingBlockFinalizationDepositOrdinal headerHash ordinal
    deriving Eq Show

PendingBlockFinalizationTxs
    headerHash HeaderHashPersist
    memberId TxIdPersist
    ordinal Int32
    payloadCbor ByteString
    payloadSha256 ByteString
    sourceTable Text
    sourceId ByteString
    sourceTimeStampTz UTCTime
    Primary headerHash memberId
    UniquePendingBlockFinalizationTxOrdinal headerHash ordinal
    deriving Eq Show

TxAdmissions
    txId TxIdPersist
    txCanonicalCbor ByteString
    txCanonicalCborSha256 ByteString
    arrivalSeq Int64
    status TxAdmissionStatus
    firstSeenAt UTCTime default=now()
    lastSeenAt UTCTime default=now()
    updatedAt UTCTime default=now()
    validationStartedAt UTCTime Maybe
    terminalAt UTCTime Maybe
    leaseOwner Text Maybe
    leaseExpiresAt UTCTime Maybe
    attemptCount Int32 default=0
    nextAttemptAt UTCTime default=now()
    rejectCode Text Maybe
    rejectDetail Text Maybe
    submitSource SubmitSource
    requestCount Int64 default=1
    Primary txId
    UniqueTxAdmissionArrivalSeq arrivalSeq
    deriving Eq Show

LocalMutationJobs
    jobId Text
    kind LocalMutationJobKind
    status LocalMutationJobStatus
    planHash ByteString Maybe
    payload Jsonb default='{}'::jsonb
    attempts Int32 default=0
    lastError Text Maybe
    createdAt UTCTime default=now()
    updatedAt UTCTime default=now()
    completedAt UTCTime Maybe
    Primary jobId
    deriving Eq Show

WithdrawalUtxos
    eventId TxOutRefPersist
    rawEventInfo ByteString
    settlementEventInfo ByteString Maybe
    inclusionTime UTCTime
    withdrawalL1TxHash TxIdPersist
    withdrawalL1OutputIndex Int32
    assetName ByteString
    l2Outref ByteString
    l2Owner ByteString
    l2Value ByteString
    l1Address ByteString
    l1Datum ByteString
    refundAddress ByteString
    refundDatum ByteString
    validity WithdrawalValidity Maybe
    validityDetail Jsonb default='{}'::jsonb
    projectedHeaderHash HeaderHashPersist Maybe
    status EventProjectionStatus
    createdAt UTCTime default=now()
    updatedAt UTCTime default=now()
    Primary eventId
    UniqueWithdrawalUtxoL1Ref withdrawalL1TxHash withdrawalL1OutputIndex
    deriving Eq Show

PendingBlockFinalizationWithdrawals
    headerHash HeaderHashPersist
    memberId TxOutRefPersist
    ordinal Int32
    payloadCbor ByteString
    payloadSha256 ByteString
    sourceTable Text
    sourceId ByteString
    sourceTimeStampTz UTCTime
    Primary headerHash memberId
    UniquePendingBlockFinalizationWithdrawalOrdinal headerHash ordinal
    deriving Eq Show

StateQueueMutationLeases
    token Text
    scope StateQueueScope
    holder Text
    status StateQueueLeaseStatus
    acquiredAt UTCTime default=now()
    expiresAt UTCTime
    releasedAt UTCTime Maybe
    lastError Text Maybe
    Primary token
    deriving Eq Show

DaPayloads
    headerHash HeaderHashPersist
    version Int32
    payloadCbor ByteString
    payloadSha256 ByteString
    utxosRoot Text
    transactionsRoot Text
    depositsRoot Text
    withdrawalsRoot Text
    blockStartTime UTCTime
    blockEndTime UTCTime
    createdAt UTCTime default=now()
    updatedAt UTCTime default=now()
    forcedTransactionsRoot Text
    transitionTraceRoot Text
    eventToStepRoot Text
    withdrawalCount Int64
    forcedTransactionCount Int64
    l2TransactionCount Int64
    depositCount Int64
    totalEventCount Int64
    transitionStepCount Int64
    Primary headerHash
    deriving Eq Show

PendingBlockFinalizationUtxos
    headerHash HeaderHashPersist
    outref TxOutRefPersist
    ordinal Int32
    output TxOutPersist
    Primary headerHash outref
    UniquePendingBlockFinalizationUtxoOrdinal headerHash ordinal
    deriving Eq Show

DepositSubmissionAttempts
    txHash TxIdPersist
    depositEventId TxOutRefPersist
    expectedDepositOutRef Text
    expectedL2Address Text
    expectedLovelace Text
    expectedAssets Jsonb
    metadata Jsonb
    fundingOutRefs Jsonb
    submittedAt UTCTime default=now()
    confirmationStatus DepositConfirmationStatus
    confirmedAt UTCTime Maybe
    lastReconciledAt UTCTime Maybe
    lastError Text Maybe
    updatedAt UTCTime default=now()
    Primary txHash
    deriving Eq Show

ForcedTransactionUtxos
    txOrderId TxOutRefPersist
    txOrderL1TxHash TxIdPersist
    txOrderL1OutputIndex Int32
    assetName ByteString
    rawDatum ByteString
    txId TxIdPersist
    txCompact ByteString
    forcedInclusionValue ByteString
    operatorValidity ForcedOperatorValidity
    inclusionTime UTCTime
    projectedHeaderHash HeaderHashPersist Maybe
    status EventProjectionStatus
    createdAt UTCTime default=now()
    updatedAt UTCTime default=now()
    Primary txOrderId
    UniqueForcedTransactionL1Ref txOrderL1TxHash txOrderL1OutputIndex
    deriving Eq Show

PendingBlockFinalizationForcedTransactions
    headerHash HeaderHashPersist
    memberId TxOutRefPersist
    ordinal Int32
    payloadCbor ByteString
    payloadSha256 ByteString
    sourceTable Text
    sourceId ByteString
    sourceTimeStampTz UTCTime
    Primary headerHash memberId
    UniquePendingBlockFinalizationForcedTransactionOrdinal headerHash ordinal
    deriving Eq Show

PendingBlockFinalizationTransitionTrace
    headerHash HeaderHashPersist
    memberId RetainedRootMemberIdPersist
    ordinal Int32
    payloadCbor ByteString
    payloadSha256 ByteString
    sourceTable Text
    sourceId ByteString
    sourceTimeStampTz UTCTime
    Primary headerHash memberId
    UniquePendingBlockFinalizationTransitionTraceOrdinal headerHash ordinal
    deriving Eq Show

PendingBlockFinalizationEventToStep
    headerHash HeaderHashPersist
    memberId RetainedRootMemberIdPersist
    ordinal Int32
    payloadCbor ByteString
    payloadSha256 ByteString
    sourceTable Text
    sourceId ByteString
    sourceTimeStampTz UTCTime
    Primary headerHash memberId
    UniquePendingBlockFinalizationEventToStepOrdinal headerHash ordinal
    deriving Eq Show
|]
