import { isMidgardConsensusProfile } from "@al-ft/midgard-core/consensus-profile";
import * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";
import { exactObjectKeys } from "midgard-node/exact-object-keys";
import {
  Lucid,
  MidgardContracts,
  NodeConfig,
} from "midgard-node/services/index";
import {
  fetchLatestCommittedBlockLocal,
  getConfirmedStateFromStateQueueDatumLocal,
  getHeaderFromStateQueueDatumLocal,
  hashBlockHeaderLocal,
  localizeSdkEffect,
  stateQueueBaseHeaderHash,
  stateQueueOutRef,
} from "midgard-node/workers/commit-block-header/state-queue";

import {
  assertPhase4T1Gate,
  decodePhase4T1CanonicalTip,
  L2_HEADER_HASH,
  PHASE4_T1_PROBE_SCHEMA,
  type Phase4T1CanonicalTip,
  type Phase4T1Gate,
  type Phase4T1ProbeEvidence,
  requireL2HeaderHash,
  SAFE_ATTEMPT_ID,
  SHA256,
} from "./phase4-t1-recovery.decode-phase4-t1-canonical-tip.js";

export const decodePhase4T1ProbeEvidence = (
  value: unknown,
): Phase4T1ProbeEvidence => {
  if (
    !exactObjectKeys(value, [
      "schemaVersion",
      "snapshotIdentitySha256",
      "attemptId",
      "canonicalHeaderHashes",
      "canonicalTip",
    ])
  ) {
    throw new Error("Phase 4 T1 probe fields do not match the exact V1 schema");
  }
  if (
    value.schemaVersion !== PHASE4_T1_PROBE_SCHEMA ||
    typeof value.snapshotIdentitySha256 !== "string" ||
    !SHA256.test(value.snapshotIdentitySha256) ||
    typeof value.attemptId !== "string" ||
    !SAFE_ATTEMPT_ID.test(value.attemptId) ||
    !Array.isArray(value.canonicalHeaderHashes) ||
    value.canonicalHeaderHashes.length === 0 ||
    value.canonicalHeaderHashes.some(
      (hash) => typeof hash !== "string" || !L2_HEADER_HASH.test(hash),
    ) ||
    new Set(value.canonicalHeaderHashes).size !==
      value.canonicalHeaderHashes.length
  ) {
    throw new Error("Phase 4 T1 probe contains a noncanonical V1 value");
  }
  const canonicalTip = decodePhase4T1CanonicalTip(value.canonicalTip);
  if (!value.canonicalHeaderHashes.includes(canonicalTip.headerHash)) {
    throw new Error("Phase 4 T1 probe canonical tip is absent from its chain");
  }
  return value as Phase4T1ProbeEvidence;
};

const safeTimeMs = (value: bigint, label: string): number => {
  const result = Number(value);
  if (!Number.isSafeInteger(result) || result < 0) {
    throw new Error(
      `${label} is not a nonnegative safe POSIX millisecond value`,
    );
  }
  return result;
};

const fetchPhase4T1CanonicalState = Effect.gen(function* () {
  const lucid = yield* Lucid;
  const contracts = yield* MidgardContracts;
  if (!isMidgardConsensusProfile(contracts.consensusProfile)) {
    return yield* Effect.fail(
      new Error(
        "The phase4-t1-v1 recovery probe is launch-profile-specific and refuses V1 state-queue data",
      ),
    );
  }
  const fetchConfig: SDK.StateQueueFetchConfig = {
    stateQueueAddress: contracts.stateQueue.spendingScriptAddress,
    stateQueuePolicyId: contracts.stateQueue.policyId,
  };
  const sorted = yield* localizeSdkEffect<
    readonly SDK.StateQueueUTxO[],
    SDK.StateQueueError | SDK.LucidError
  >(SDK.fetchSortedStateQueueUTxOsProgram(lucid.api, fetchConfig));
  const latest = yield* fetchLatestCommittedBlockLocal(lucid.api, fetchConfig);
  const latestHeaderHash = yield* stateQueueBaseHeaderHash(latest);
  const canonicalHeaderHashes: string[] = [];
  for (const block of sorted) {
    if (block.datum.key === "Empty") {
      const { data } = yield* getConfirmedStateFromStateQueueDatumLocal(
        block.datum,
      );
      canonicalHeaderHashes.push(
        requireL2HeaderHash(data.headerHash, "confirmed header hash"),
      );
      continue;
    }
    const header = yield* getHeaderFromStateQueueDatumLocal(block.datum);
    canonicalHeaderHashes.push(
      requireL2HeaderHash(
        yield* hashBlockHeaderLocal(header),
        "canonical header hash",
      ),
    );
  }
  const uniqueCanonicalHeaderHashes = [...new Set(canonicalHeaderHashes)];
  if (!uniqueCanonicalHeaderHashes.includes(latestHeaderHash)) {
    throw new Error(
      "Latest state_queue tail is absent from the canonical hash set",
    );
  }

  let canonicalTip: Phase4T1CanonicalTip;
  if (latest.datum.key === "Empty") {
    const { data } = yield* getConfirmedStateFromStateQueueDatumLocal(
      latest.datum,
    );
    canonicalTip = {
      headerHash: requireL2HeaderHash(latestHeaderHash, "canonical tip hash"),
      outRef: stateQueueOutRef(latest),
      datumKind: "confirmed",
      prevHeaderHash: requireL2HeaderHash(
        data.prevHeaderHash,
        "canonical confirmed previous header hash",
      ),
      prevUtxosRoot: null,
      utxosRoot: data.utxoRoot,
      transactionsRoot: null,
      depositsRoot: null,
      withdrawalsRoot: null,
      forcedTransactionsRoot: null,
      transitionTraceRoot: null,
      eventToStepRoot: null,
      withdrawalCount: null,
      forcedTransactionCount: null,
      l2TransactionCount: null,
      depositCount: null,
      totalEventCount: null,
      transitionStepCount: null,
      startTimeMs: safeTimeMs(data.startTime, "confirmed start time"),
      endTimeMs: safeTimeMs(data.endTime, "confirmed end time"),
    };
  } else {
    const header = yield* getHeaderFromStateQueueDatumLocal(latest.datum);
    canonicalTip = {
      headerHash: requireL2HeaderHash(latestHeaderHash, "canonical tip hash"),
      outRef: stateQueueOutRef(latest),
      datumKind: "header",
      prevHeaderHash: requireL2HeaderHash(
        header.prevHeaderHash,
        "canonical previous header hash",
      ),
      prevUtxosRoot: header.prevUtxosRoot,
      utxosRoot: header.utxosRoot,
      transactionsRoot: header.transactionsRoot,
      depositsRoot: header.depositsRoot,
      withdrawalsRoot: header.withdrawalsRoot,
      forcedTransactionsRoot: header.forcedTransactionsRoot,
      transitionTraceRoot: header.transitionTraceRoot,
      eventToStepRoot: header.eventToStepRoot,
      withdrawalCount: header.withdrawalCount.toString(),
      forcedTransactionCount: header.forcedTransactionCount.toString(),
      l2TransactionCount: header.l2TransactionCount.toString(),
      depositCount: header.depositCount.toString(),
      totalEventCount: header.totalEventCount.toString(),
      transitionStepCount: header.transitionStepCount.toString(),
      startTimeMs: safeTimeMs(header.startTime, "header start time"),
      endTimeMs: safeTimeMs(header.endTime, "header end time"),
    };
  }
  return { canonicalHeaderHashes: uniqueCanonicalHeaderHashes, canonicalTip };
});

export type Phase4T1ProbeOptions = Phase4T1Gate & {
  readonly expectedTipHeaderHash?: string;
  readonly expectedPresentHeaderHash?: string;
  readonly expectedAbsentHeaderHash?: string;
};

export const phase4T1ProbeProgram = (
  options: Phase4T1ProbeOptions,
): Effect.Effect<
  Phase4T1ProbeEvidence,
  unknown,
  Lucid | MidgardContracts | NodeConfig
> =>
  Effect.gen(function* () {
    assertPhase4T1Gate({ ...options, env: process.env });
    const state = yield* fetchPhase4T1CanonicalState;
    const expectedTip =
      options.expectedTipHeaderHash === undefined
        ? undefined
        : requireL2HeaderHash(
            options.expectedTipHeaderHash,
            "expected tip header hash",
          );
    const expectedPresent =
      options.expectedPresentHeaderHash === undefined
        ? undefined
        : requireL2HeaderHash(
            options.expectedPresentHeaderHash,
            "expected present header hash",
          );
    const expectedAbsent =
      options.expectedAbsentHeaderHash === undefined
        ? undefined
        : requireL2HeaderHash(
            options.expectedAbsentHeaderHash,
            "expected absent header hash",
          );
    if (
      expectedTip !== undefined &&
      state.canonicalTip.headerHash !== expectedTip
    ) {
      throw new Error(
        `Canonical L2 tip mismatch: expected=${expectedTip},actual=${state.canonicalTip.headerHash}`,
      );
    }
    if (
      expectedPresent !== undefined &&
      !state.canonicalHeaderHashes.includes(expectedPresent)
    ) {
      throw new Error(
        `Required canonical L2 header is absent: ${expectedPresent}`,
      );
    }
    if (
      expectedAbsent !== undefined &&
      state.canonicalHeaderHashes.includes(expectedAbsent)
    ) {
      throw new Error(
        `Forbidden canonical L2 header is still present: ${expectedAbsent}`,
      );
    }
    return decodePhase4T1ProbeEvidence({
      schemaVersion: PHASE4_T1_PROBE_SCHEMA,
      snapshotIdentitySha256: options.snapshotIdentitySha256,
      attemptId: options.attemptId,
      ...state,
    });
  });

export type Phase4T1NoopAdvanceAssertion = {
  readonly baseHeaderHash: string;
  readonly recoveredTipHeaderHash: string;
  readonly abandonedHeaderHash: string;
  readonly baseEndTimeMs: number;
  readonly recoveredEndTimeMs: number;
  readonly minimumRecoveredEndTimeMs: number;
  readonly rootsPreserved: true;
  readonly transitionIsEmpty: true;
};

export const decodePhase4T1NoopAdvanceAssertion = (
  value: unknown,
): Phase4T1NoopAdvanceAssertion => {
  if (
    !exactObjectKeys(value, [
      "baseHeaderHash",
      "recoveredTipHeaderHash",
      "abandonedHeaderHash",
      "baseEndTimeMs",
      "recoveredEndTimeMs",
      "minimumRecoveredEndTimeMs",
      "rootsPreserved",
      "transitionIsEmpty",
    ])
  ) {
    throw new Error(
      "Phase 4 T1 invariant fields do not match the exact V1 schema",
    );
  }
  if (
    typeof value.baseHeaderHash !== "string" ||
    !L2_HEADER_HASH.test(value.baseHeaderHash) ||
    typeof value.recoveredTipHeaderHash !== "string" ||
    !L2_HEADER_HASH.test(value.recoveredTipHeaderHash) ||
    typeof value.abandonedHeaderHash !== "string" ||
    !L2_HEADER_HASH.test(value.abandonedHeaderHash) ||
    !Number.isSafeInteger(value.baseEndTimeMs) ||
    (value.baseEndTimeMs as number) < 0 ||
    !Number.isSafeInteger(value.recoveredEndTimeMs) ||
    (value.recoveredEndTimeMs as number) <= (value.baseEndTimeMs as number) ||
    !Number.isSafeInteger(value.minimumRecoveredEndTimeMs) ||
    (value.minimumRecoveredEndTimeMs as number) <= 0 ||
    (value.recoveredEndTimeMs as number) <
      (value.minimumRecoveredEndTimeMs as number) ||
    value.rootsPreserved !== true ||
    value.transitionIsEmpty !== true
  ) {
    throw new Error("Phase 4 T1 invariant evidence is noncanonical");
  }
  return value as Phase4T1NoopAdvanceAssertion;
};
