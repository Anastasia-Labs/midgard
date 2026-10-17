import { MIDGARD_CONSENSUS_PROFILE_ID } from "@al-ft/midgard-core/consensus-profile";
import { parseDeploymentMarker } from "@al-ft/midgard-core/deployment-manifest-identity";

import {
  assertNativeMpfReplay,
  exactBytes,
  exactDate,
  exactHex,
  exactNonEmptyString,
  exactNonNegativeBigInt,
  type NativeMpfReplayInput,
  type PendingBlockFinalizationMetadata,
} from "./pendingBlockFinalizations.parse-ledger-delta.js";
import { exactRecord } from "./utils/exact-record.js";

export const parseNativeMpfReplay = (value: unknown): NativeMpfReplayInput => {
  const candidate = exactRecord(
    value,
    [
      "schema",
      "ownerBinarySha256",
      "baseRoot",
      "candidateRoot",
      "eventLog",
      "eventLogDigest",
      "eventRoots",
      "eventCount",
    ],
    "PendingBlockFinalizationV1 nativeMpfReplay",
  );
  if (candidate.schema !== 1) {
    throw new Error(
      "PendingBlockFinalizationV1 nativeMpfReplay.schema must equal 1",
    );
  }
  if (
    typeof candidate.eventCount !== "number" ||
    !Number.isSafeInteger(candidate.eventCount)
  ) {
    throw new Error(
      "PendingBlockFinalizationV1 nativeMpfReplay.eventCount must be a safe integer",
    );
  }
  const replay: NativeMpfReplayInput = {
    schema: 1,
    ownerBinarySha256: exactBytes(
      candidate.ownerBinarySha256,
      "PendingBlockFinalizationV1 nativeMpfReplay.ownerBinarySha256",
      32,
    ),
    baseRoot: exactBytes(
      candidate.baseRoot,
      "PendingBlockFinalizationV1 nativeMpfReplay.baseRoot",
      32,
    ),
    candidateRoot: exactBytes(
      candidate.candidateRoot,
      "PendingBlockFinalizationV1 nativeMpfReplay.candidateRoot",
      32,
    ),
    eventLog: exactBytes(
      candidate.eventLog,
      "PendingBlockFinalizationV1 nativeMpfReplay.eventLog",
    ),
    eventLogDigest: exactBytes(
      candidate.eventLogDigest,
      "PendingBlockFinalizationV1 nativeMpfReplay.eventLogDigest",
      32,
    ),
    eventRoots: exactBytes(
      candidate.eventRoots,
      "PendingBlockFinalizationV1 nativeMpfReplay.eventRoots",
      candidate.eventCount * 32,
    ),
    eventCount: candidate.eventCount,
  };
  assertNativeMpfReplay(replay);
  return replay;
};

export const parsePendingBlockFinalizationMetadata = (
  value: unknown,
): PendingBlockFinalizationMetadata => {
  const candidate = exactRecord(
    value,
    [
      "deploymentMarker",
      "consensusProfileId",
      "stateQueueLeaseToken",
      "baseSnapshotId",
      "baseTailOutRef",
      "baseTailHeaderHash",
      "baseTailDatumCbor",
      "baseRoots",
      "blockStartTime",
      "expectedRoots",
      "expectedCounts",
    ],
    "PendingBlockFinalizationV1 metadata",
  );
  const baseRoots = exactRecord(
    candidate.baseRoots,
    [
      "utxosRoot",
      "forcedTransactionsRoot",
      "transactionsRoot",
      "depositsRoot",
      "withdrawalsRoot",
    ],
    "PendingBlockFinalizationV1 metadata.baseRoots",
  );
  const expectedRoots = exactRecord(
    candidate.expectedRoots,
    [
      "utxosRoot",
      "forcedTransactionsRoot",
      "transactionsRoot",
      "depositsRoot",
      "withdrawalsRoot",
      "transitionTraceRoot",
      "eventToStepRoot",
      "validationTracesRoot",
    ],
    "PendingBlockFinalizationV1 metadata.expectedRoots",
  );
  const expectedCounts = exactRecord(
    candidate.expectedCounts,
    [
      "withdrawalCount",
      "forcedTransactionCount",
      "l2TransactionCount",
      "depositCount",
      "totalEventCount",
      "transitionStepCount",
      "validationTraceCount",
    ],
    "PendingBlockFinalizationV1 metadata.expectedCounts",
  );
  const deploymentMarker = parseDeploymentMarker(candidate.deploymentMarker);
  if (candidate.consensusProfileId !== MIDGARD_CONSENSUS_PROFILE_ID) {
    throw new Error(
      `PendingBlockFinalizationV1 metadata.consensusProfileId must equal ${MIDGARD_CONSENSUS_PROFILE_ID}`,
    );
  }
  const counts = {
    withdrawalCount: exactNonNegativeBigInt(
      expectedCounts.withdrawalCount,
      "PendingBlockFinalizationV1 metadata.expectedCounts.withdrawalCount",
    ),
    forcedTransactionCount: exactNonNegativeBigInt(
      expectedCounts.forcedTransactionCount,
      "PendingBlockFinalizationV1 metadata.expectedCounts.forcedTransactionCount",
    ),
    l2TransactionCount: exactNonNegativeBigInt(
      expectedCounts.l2TransactionCount,
      "PendingBlockFinalizationV1 metadata.expectedCounts.l2TransactionCount",
    ),
    depositCount: exactNonNegativeBigInt(
      expectedCounts.depositCount,
      "PendingBlockFinalizationV1 metadata.expectedCounts.depositCount",
    ),
    totalEventCount: exactNonNegativeBigInt(
      expectedCounts.totalEventCount,
      "PendingBlockFinalizationV1 metadata.expectedCounts.totalEventCount",
    ),
    transitionStepCount: exactNonNegativeBigInt(
      expectedCounts.transitionStepCount,
      "PendingBlockFinalizationV1 metadata.expectedCounts.transitionStepCount",
    ),
    validationTraceCount: exactNonNegativeBigInt(
      expectedCounts.validationTraceCount,
      "PendingBlockFinalizationV1 metadata.expectedCounts.validationTraceCount",
    ),
  };
  if (
    counts.totalEventCount !==
      counts.withdrawalCount +
        counts.forcedTransactionCount +
        counts.l2TransactionCount +
        counts.depositCount ||
    counts.transitionStepCount !== counts.totalEventCount ||
    counts.validationTraceCount !==
      counts.forcedTransactionCount + counts.l2TransactionCount
  ) {
    throw new Error(
      "PendingBlockFinalizationV1 metadata expected counts are inconsistent",
    );
  }
  return {
    deploymentMarker,
    consensusProfileId: MIDGARD_CONSENSUS_PROFILE_ID,
    stateQueueLeaseToken: exactNonEmptyString(
      candidate.stateQueueLeaseToken,
      "PendingBlockFinalizationV1 metadata.stateQueueLeaseToken",
    ),
    baseSnapshotId: exactNonEmptyString(
      candidate.baseSnapshotId,
      "PendingBlockFinalizationV1 metadata.baseSnapshotId",
    ),
    baseTailOutRef: exactNonEmptyString(
      candidate.baseTailOutRef,
      "PendingBlockFinalizationV1 metadata.baseTailOutRef",
    ),
    baseTailHeaderHash: exactBytes(
      candidate.baseTailHeaderHash,
      "PendingBlockFinalizationV1 metadata.baseTailHeaderHash",
      28,
    ),
    baseTailDatumCbor: exactNonEmptyString(
      candidate.baseTailDatumCbor,
      "PendingBlockFinalizationV1 metadata.baseTailDatumCbor",
    ),
    baseRoots: {
      utxosRoot: exactHex(
        baseRoots.utxosRoot,
        32,
        "PendingBlockFinalizationV1 metadata.baseRoots.utxosRoot",
      ),
      forcedTransactionsRoot: exactHex(
        baseRoots.forcedTransactionsRoot,
        32,
        "PendingBlockFinalizationV1 metadata.baseRoots.forcedTransactionsRoot",
      ),
      transactionsRoot: exactHex(
        baseRoots.transactionsRoot,
        32,
        "PendingBlockFinalizationV1 metadata.baseRoots.transactionsRoot",
      ),
      depositsRoot: exactHex(
        baseRoots.depositsRoot,
        32,
        "PendingBlockFinalizationV1 metadata.baseRoots.depositsRoot",
      ),
      withdrawalsRoot: exactHex(
        baseRoots.withdrawalsRoot,
        32,
        "PendingBlockFinalizationV1 metadata.baseRoots.withdrawalsRoot",
      ),
    },
    blockStartTime: exactDate(
      candidate.blockStartTime,
      "PendingBlockFinalizationV1 metadata.blockStartTime",
    ),
    expectedRoots: {
      utxosRoot: exactHex(
        expectedRoots.utxosRoot,
        32,
        "PendingBlockFinalizationV1 metadata.expectedRoots.utxosRoot",
      ),
      forcedTransactionsRoot: exactHex(
        expectedRoots.forcedTransactionsRoot,
        32,
        "PendingBlockFinalizationV1 metadata.expectedRoots.forcedTransactionsRoot",
      ),
      transactionsRoot: exactHex(
        expectedRoots.transactionsRoot,
        32,
        "PendingBlockFinalizationV1 metadata.expectedRoots.transactionsRoot",
      ),
      depositsRoot: exactHex(
        expectedRoots.depositsRoot,
        32,
        "PendingBlockFinalizationV1 metadata.expectedRoots.depositsRoot",
      ),
      withdrawalsRoot: exactHex(
        expectedRoots.withdrawalsRoot,
        32,
        "PendingBlockFinalizationV1 metadata.expectedRoots.withdrawalsRoot",
      ),
      transitionTraceRoot: exactHex(
        expectedRoots.transitionTraceRoot,
        32,
        "PendingBlockFinalizationV1 metadata.expectedRoots.transitionTraceRoot",
      ),
      eventToStepRoot: exactHex(
        expectedRoots.eventToStepRoot,
        32,
        "PendingBlockFinalizationV1 metadata.expectedRoots.eventToStepRoot",
      ),
      validationTracesRoot: exactHex(
        expectedRoots.validationTracesRoot,
        32,
        "PendingBlockFinalizationV1 metadata.expectedRoots.validationTracesRoot",
      ),
    },
    expectedCounts: counts,
  };
};
