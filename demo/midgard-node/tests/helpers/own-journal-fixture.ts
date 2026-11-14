import { MIDGARD_CONSENSUS_PROFILE_ID } from "@al-ft/midgard-core/consensus-profile";
import { makeDeploymentMarker } from "@al-ft/midgard-core/deployment-manifest-identity";
import * as SDK from "@al-ft/midgard-sdk";
import { Data as LucidData } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import type * as PendingBlockFinalizationsDB from "../../src/database/pendingBlockFinalizations.js";
import {
  deterministicFixtureBytes,
  provideDatabaseLayers,
  resetApplicationTables,
} from "../utils.js";

/** Runs `effect` on a freshly reset database: the fixture producer gate
 * refuses any acquired owner row an earlier file on the same worker shard
 * left behind. */
export const isolatedDb = <A, E, R>(effect: Effect.Effect<A, E, R>) =>
  provideDatabaseLayers(
    Effect.gen(function* () {
      yield* resetApplicationTables;
      return yield* effect;
    }),
  );

export const header = (label: string) =>
  deterministicFixtureBytes(`own-journal:${label}`, 28);

/** An empty-block journal for `headerHash`, ready for preparePendingSubmission. */
export const journalFixture = (
  headerHash: Buffer,
): PendingBlockFinalizationsDB.PrepareInput => {
  const blockStartTime = new Date("2026-06-12T00:00:00.000Z");
  const emptyRoots = {
    utxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    forcedTransactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    transactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    depositsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    withdrawalsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  };
  const expectedRoots = {
    ...emptyRoots,
    transitionTraceRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    eventToStepRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    validationTracesRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  };
  const expectedCounts = {
    withdrawalCount: 0n,
    forcedTransactionCount: 0n,
    l2TransactionCount: 0n,
    depositCount: 0n,
    totalEventCount: 0n,
    transitionStepCount: 0n,
    validationTraceCount: 0n,
  };
  const blockHeader: SDK.Header = {
    prevUtxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    ...expectedRoots,
    ...expectedCounts,
    startTime: 1n,
    endTime: 2n,
    blockSlot: 0n,
    expectedNetworkId: 0n,
    minFeeA: 0n,
    minFeeB: 0n,
    prevHeaderHash: "11".repeat(28),
    operatorVkey: "22".repeat(28),
    protocolVersion: 1n,
  };
  return {
    headerHash,
    headerCbor: Buffer.from(
      LucidData.to(blockHeader as never, SDK.Header as never),
      "hex",
    ),
    metadata: {
      deploymentMarker: makeDeploymentMarker("de".repeat(32)),
      consensusProfileId: MIDGARD_CONSENSUS_PROFILE_ID,
      stateQueueLeaseToken: "lease-token",
      baseSnapshotId: "snapshot",
      baseTailOutRef: "base#0",
      baseTailHeaderHash: header("base-tail"),
      baseTailDatumCbor: "d87980",
      baseRoots: emptyRoots,
      blockStartTime,
      expectedRoots,
      expectedCounts,
    },
    blockEndTime: new Date(blockStartTime.getTime() + 60_000),
    blockTime: new Date(blockStartTime.getTime() + 60_000),
    depositEventIds: [],
    depositEntries: [],
    forcedTransactionEventIds: [],
    forcedTransactionEntries: [],
    withdrawalEventIds: [],
    withdrawalEntries: [],
    mempoolTxIds: [],
    mempoolTxs: [],
    mempoolTxSourceTable: "none",
    transitionTraceMembers: [],
    eventToStepMembers: [],
    validationTraceMembers: [],
    ledgerDelta: { spent: [], produced: [] },
  };
};
