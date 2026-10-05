import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { SqlClient } from "@effect/sql";
import { Effect, Logger } from "effect";
import { describe, expect, it, vi } from "vitest";

import { UnownedHistoryFixture } from "../src/services/event-history-producer.js";
import {
  ContractDeploymentIdentity,
  NodeConfig,
} from "../src/services/index.js";
import { databaseOperationsProgram } from "../src/workers/commit-block-header.database-operations-program.js";
import type { WorkerInput } from "../src/workers/utils/commit-block-header.js";

// The commit program reaches the foreign-tip gate after reading its mempool
// and its ingestion barriers; the gate itself records how it was asked.
const seams = vi.hoisted(() => ({ gateCalls: [] as unknown[] }));
vi.mock("../src/workers/t2-foreign-event-reconciliation.js", async () => {
  const actual = await vi.importActual<
    typeof import("../src/workers/t2-foreign-event-reconciliation.js")
  >("../src/workers/t2-foreign-event-reconciliation.js");
  return {
    ...actual,
    gateCommitOnRetainedForeignTips: vi.fn((request: unknown) => {
      seams.gateCalls.push(request);
      return Effect.succeed({
        type: "AwaitingForeignDa",
        foreignHeaderHash: "ee".repeat(28),
        reason: "missing",
        detail: "call-edge",
        present: { deposits: [], forcedTransactions: [], withdrawals: [] },
      });
    }),
  };
});
vi.mock("../src/database/index.js", async () => {
  const actual = await vi.importActual<
    typeof import("../src/database/index.js")
  >("../src/database/index.js");
  return {
    ...actual,
    MempoolDB: {
      ...actual.MempoolDB,
      retrievePage: vi.fn(() =>
        Effect.succeed({ entries: [], nextCursor: null }),
      ),
    },
    MpfEngineStateDB: {
      ...actual.MpfEngineStateDB,
      assertLedgerAuditHealthy: Effect.void,
    },
    ProcessedMempoolDB: {
      ...actual.ProcessedMempoolDB,
      retrieve: Effect.succeed([]),
    },
  };
});
vi.mock("../src/mpf/index.js", async () => {
  const actual = await vi.importActual<typeof import("../src/mpf/index.js")>(
    "../src/mpf/index.js",
  );
  return { ...actual, configureCommitMpfRuntime: vi.fn(() => Effect.void) };
});

const nodeConfig = {
  MEMPOOL_RETRIEVE_PAGE_SIZE: 100,
  COMMIT_BUILD_COST_MODEL: "static",
  COMMIT_MAX_L2_TX_COUNT: 100,
  COMMIT_MAX_LEDGER_OP_COUNT: 1_000,
  COMMIT_MAX_TRANSITION_STEP_COUNT: 1_000,
} as never;
const deploymentIdentity = ContractDeploymentIdentity.make({
  kind: "derived",
  deploymentMarker: {
    schemaVersion: "midgard-deployment-marker-v1",
    manifestId: "test-manifest",
  } as never,
  consensusProfile: MIDGARD_CONSENSUS_PROFILE,
});
const fakeSql = Object.assign(
  ((..._args: readonly unknown[]) =>
    Effect.succeed([])) as unknown as SqlClient.SqlClient,
  {
    withTransaction: <A, E, R>(effect: Effect.Effect<A, E, R>) => effect,
  },
) as unknown as SqlClient.SqlClient;

// Three distinct barriers; the withdrawal one is the earliest.
const WATERMARK_MS = Date.parse("2026-01-01T00:07:00.000Z");
const speculativeInput = {
  nativeMpf: {
    port: {} as MessagePort,
    durableRoot: "33".repeat(32),
    ownerBinarySha256: "ab".repeat(32),
  },
  data: {
    availableConfirmedBlock: "",
    availableLocalFinalizationBlock: "",
    currentBlockStartTimeMs: Date.parse("2026-01-01T00:00:00.000Z"),
    localFinalizationPending: false,
    mempoolTxsCountSoFar: 0,
    sizeOfProcessedTxsSoFar: 0,
    stateQueueHasUnmergedTail: false,
    speculativeBuild: {
      base: {
        headerHash: "aa".repeat(28),
        utxosRoot: "33".repeat(32),
        blockEndTimeMs: Date.parse("2026-01-01T00:05:00.000Z"),
        submittedTxHash: "bb".repeat(32),
      },
      watermarks: {
        depositMs: WATERMARK_MS + 3_000,
        withdrawalMs: WATERMARK_MS + 1_000,
        txOrderMs: WATERMARK_MS + 2_000,
        refreshedAtMs: WATERMARK_MS + 3_000,
      },
      excludedMempoolTxIds: [],
      excludedDepositEventIds: [],
      excludedForcedTransactionEventIds: [],
      excludedWithdrawalEventIds: [],
    },
  },
} as unknown as WorkerInput;

describe("commit program call edge into the foreign-tip gate", () => {
  it("asks a speculative build for the read-only gate, through the earliest ingestion barrier, and hands back its refusal", async () => {
    seams.gateCalls.length = 0;
    const output = await Effect.runPromise(
      databaseOperationsProgram(
        speculativeInput,
        {} as never,
        undefined,
        undefined,
        undefined,
        undefined,
        undefined,
        {} as never,
      ).pipe(
        Effect.provideService(NodeConfig, nodeConfig),
        Effect.provideService(ContractDeploymentIdentity, deploymentIdentity),
        Effect.provideService(UnownedHistoryFixture, true),
        Effect.provideService(SqlClient.SqlClient, fakeSql),
        Effect.provide(Logger.remove(Logger.defaultLogger)),
      ) as Effect.Effect<unknown, unknown, never>,
    );
    expect(seams.gateCalls).toEqual([
      {
        speculative: true,
        eventsIngestedThrough: new Date(WATERMARK_MS + 1_000),
      },
    ]);
    expect(output).toEqual({
      type: "AwaitingForeignDaOutput",
      foreignHeaderHash: "ee".repeat(28),
      reason: "missing:call-edge",
    });
  });
});
