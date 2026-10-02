import { encodeMidgardCekProgramMaterialSidecar } from "@al-ft/midgard-core/cek-proof";
import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { SqlClient } from "@effect/sql";
import { Effect, Logger, Option } from "effect";
import { describe, expect, it, vi } from "vitest";

import { TxUtils as TxTable } from "../src/database/index.js";
import type { EntryWithTimeStamp } from "../src/database/utils/tx.js";
import {
  type NativeMpfBuildContext,
  processMpfs,
  type UtxoPayloadSizeAggregate,
} from "../src/mpf/index.js";
import { UnownedHistoryFixture } from "../src/services/event-history-producer.js";
import {
  ContractDeploymentIdentity,
  NodeConfig,
} from "../src/services/index.js";
import { runCommitBlockHeaderWorkerProgram } from "../src/workers/commit-block-header.js";
import type {
  SpeculativeCandidateReadyOutput,
  WorkerInput,
} from "../src/workers/utils/commit-block-header.js";
import {
  blockContentFor,
  type BlockShape,
  CANONICAL_TX,
  LC1_BASE_LEDGER,
} from "./helpers/commit-da-frame-fixtures.js";

// The worker reads its candidates, its speculative base journal and the
// native owner through these seams; the block build itself is faked below.
const seams = vi.hoisted(() => ({
  candidates: [] as unknown[],
  baseAggregate: { entryCount: 0, encodedTupleBytes: 0 },
  owner: [] as string[],
}));

vi.mock("../src/database/index.js", async () => {
  const actual = await vi.importActual<
    typeof import("../src/database/index.js")
  >("../src/database/index.js");
  return {
    ...actual,
    DepositsDB: {
      ...actual.DepositsDB,
      retrievePendingHeaderEntriesUpTo: vi.fn(() => Effect.succeed([])),
    },
    ForcedTransactionsDB: {
      ...actual.ForcedTransactionsDB,
      retrievePendingHeaderEntriesUpTo: vi.fn(() => Effect.succeed([])),
    },
    MempoolDB: {
      ...actual.MempoolDB,
      retrievePage: vi.fn(() =>
        Effect.succeed({ entries: [], nextCursor: null }),
      ),
    },
    MpfEngineStateDB: {
      ...actual.MpfEngineStateDB,
      assertLedgerAuditHealthy: Effect.void,
      releaseLedgerStoreLease: vi.fn(() => Effect.void),
      revalidateLedgerStoreLease: vi.fn(() => Effect.void),
      stampLedgerPayloadAggregate: vi.fn(() => Effect.void),
      tryWithLedgerStoreLease: vi.fn(
        (
          owner: string,
          program: (activeOwner: string) => Effect.Effect<unknown>,
        ) =>
          program(owner).pipe(
            Effect.map((value) => ({ _tag: "Ran" as const, value })),
          ),
      ),
    },
    PendingBlockFinalizationsDB: {
      ...actual.PendingBlockFinalizationsDB,
      retrieveByHeaderHash: vi.fn(() =>
        Effect.succeed(
          Option.some({
            [actual.PendingBlockFinalizationsDB.Columns.EXPECTED_UTXOS_ROOT]:
              "33".repeat(32),
            utxoPayloadAggregate: seams.baseAggregate,
          }),
        ),
      ),
    },
    ProcessedMempoolDB: {
      ...actual.ProcessedMempoolDB,
      retrieve: Effect.suspend(() => Effect.succeed(seams.candidates)),
    },
    TxAdmissionsDB: {
      ...actual.TxAdmissionsDB,
      retrieveProgramMaterialSidecars: vi.fn((txIds: readonly Buffer[]) =>
        Effect.succeed(
          txIds.map((txId) => ({
            txId,
            sidecarCbor: encodeMidgardCekProgramMaterialSidecar([]),
          })),
        ),
      ),
    },
    WithdrawalsDB: {
      ...actual.WithdrawalsDB,
      retrievePendingHeaderEntriesUpTo: vi.fn(() => Effect.succeed([])),
    },
  };
});
vi.mock(
  "../src/transactions/state-queue/confirmed-ledger-snapshot.js",
  async () => {
    const actual = await vi.importActual<
      typeof import("../src/transactions/state-queue/confirmed-ledger-snapshot.js")
    >("../src/transactions/state-queue/confirmed-ledger-snapshot.js");
    return {
      ...actual,
      materializeConfirmedLedgerSnapshot: vi.fn(() =>
        Effect.succeed({
          entries: [],
          baseRoot: "33".repeat(32),
          root: "33".repeat(32),
          deltaChain: [],
          delta: { spent: [], produced: [] },
        }),
      ),
    };
  },
);
vi.mock("../src/fibers/fetch-and-insert-deposit-utxos.js", () => ({
  fetchAndInsertDepositUTxOsForCommitBarrier: vi.fn((end: Date) =>
    Effect.succeed(end),
  ),
}));
vi.mock("../src/fibers/fetch-and-insert-withdrawal-utxos.js", () => ({
  fetchAndInsertWithdrawalUTxOsForCommitBarrier: vi.fn((end: Date) =>
    Effect.succeed(end),
  ),
}));
vi.mock("../src/fibers/fetch-and-insert-tx-order-utxos.js", () => ({
  fetchAndInsertTxOrderUTxOsForCommitBarrier: vi.fn((end: Date) =>
    Effect.succeed(end),
  ),
}));
vi.mock("../src/e2e/pipelined-commit-crash-checkpoint.js", () => ({
  reachPipelinedCommitCrashCheckpoint: vi.fn(() => Effect.void),
}));
vi.mock("../src/workers/commit-block-header/event-roots.js", () => ({
  resolveDepositsRoot: vi.fn(() => Effect.succeed(Option.none())),
  resolveForcedTransactionsRoot: vi.fn(() => Effect.succeed(Option.none())),
  resolveWithdrawalsRoot: vi.fn(() => Effect.succeed(Option.none())),
}));
vi.mock("../src/mpf/index.js", async () => {
  const actual = await vi.importActual<typeof import("../src/mpf/index.js")>(
    "../src/mpf/index.js",
  );
  return {
    ...actual,
    configureCommitMpfRuntime: vi.fn(() => Effect.void),
    processMpfs: vi.fn(),
  };
});
// The native owner records every fork and discard in order.
vi.mock("../src/services/mpf-native-owner/client.js", () => ({
  NativeMpfWorkerPortClient: class {
    private forks = 0;
    fork = vi.fn(async (baseRoot: string) => {
      this.forks += 1;
      seams.owner.push(`fork:${this.forks.toString()}`);
      return { id: this.forks, baseRoot };
    });
    discard = vi.fn(async (handle: { id: number }) => {
      seams.owner.push(`discard:${handle.id.toString()}`);
    });
    retainForJournal = vi.fn(async () => undefined);
    close = vi.fn();
  },
}));

const BLOCK_TIME = Date.parse("2026-01-01T00:06:00.000Z");
const candidateTx = (seed: number): EntryWithTimeStamp => ({
  [TxTable.Columns.TX_ID]: Buffer.from(
    seed.toString(16).padStart(64, "0"),
    "hex",
  ),
  [TxTable.Columns.TX]: CANONICAL_TX,
  [TxTable.Columns.TIMESTAMPTZ]: new Date(BLOCK_TIME + seed),
});

const nodeConfig = {
  MPF_PAYLOAD_ROOT_CHECK: "off",
  MPF_RECORD_CORPUS: "",
  MEMPOOL_RETRIEVE_PAGE_SIZE: 100,
  COMMIT_BUILD_COST_MODEL: "static",
  COMMIT_MAX_L2_TX_COUNT: 100,
  COMMIT_MAX_LEDGER_OP_COUNT: 1_000,
  COMMIT_MAX_TRANSITION_STEP_COUNT: 1_000,
  NETWORK: "Testnet",
  MIN_FEE_A: 0n,
  MIN_FEE_B: 0n,
  VALIDATION_G4_BUCKET_CONCURRENCY: 1,
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
    array: vi.fn((values: readonly unknown[]) => values),
    withTransaction: <A, E, R>(effect: Effect.Effect<A, E, R>) => effect,
  },
) as unknown as SqlClient.SqlClient;
const watermark = Date.parse("2026-01-01T00:07:00.999Z");
const workerInput = {
  nativeMpf: {
    port: {} as MessagePort,
    durableRoot: "33".repeat(32),
    ownerBinarySha256: "ab".repeat(32),
  },
  data: {
    availableConfirmedBlock: "",
    availableLocalFinalizationBlock: "",
    currentBlockStartTimeMs: Date.parse("2026-01-01T00:00:00.000Z"),
    forcedValidationSlotConfig: {
      zeroTime: Date.parse("2026-01-01T00:06:50.999Z"),
      zeroSlot: 100,
      slotLength: 1_000,
    },
    ledgerStoreLeaseOwner: "commit:12345678-1234-4123-8123-123456789abc",
    localFinalizationPending: false,
    mempoolTxsCountSoFar: 0,
    sizeOfProcessedTxsSoFar: 0,
    baseSnapshotId: "test",
    stateQueueHasUnmergedTail: false,
    speculativeBuild: {
      base: {
        headerHash: "aa".repeat(28),
        utxosRoot: "33".repeat(32),
        blockEndTimeMs: Date.parse("2026-01-01T00:05:00.000Z"),
        submittedTxHash: "bb".repeat(32),
      },
      watermarks: {
        depositMs: watermark,
        withdrawalMs: watermark,
        txOrderMs: watermark,
        refreshedAtMs: watermark,
      },
      excludedMempoolTxIds: [],
      excludedDepositEventIds: [],
      excludedForcedTransactionEventIds: [],
      excludedWithdrawalEventIds: [],
    },
  },
} as unknown as WorkerInput;

/**
 * One commit worker run over `txCount` transactions of `shape`, from a base
 * ledger of `baseAggregate`. Each build returns the DA content its selection
 * would carry; the run ends at the speculative candidate the parent sees.
 */
const runWorker = async (
  txCount: number,
  shape: BlockShape,
  baseAggregate: UtxoPayloadSizeAggregate = LC1_BASE_LEDGER,
) => {
  seams.candidates = Array.from({ length: txCount }, (_, index) =>
    candidateTx(index + 1),
  );
  seams.baseAggregate = baseAggregate;
  seams.owner = [];
  const builds: string[][] = [];
  vi.mocked(processMpfs).mockReset();
  vi.mocked(processMpfs).mockImplementation(((
    _transactionsMpf: unknown,
    txs: readonly EntryWithTimeStamp[],
    config: { readonly nativeMpf?: NativeMpfBuildContext },
  ) =>
    Effect.sync(() => {
      const handle = config.nativeMpf?.handle as unknown as { id: number };
      seams.owner.push(`build:${handle.id.toString()}`);
      builds.push(
        txs.map((entry) => entry[TxTable.Columns.TX_ID].toString("hex")),
      );
      return {
        ...blockContentFor(txs, { ...shape, base: baseAggregate }),
        utxoRoot: "33".repeat(32),
        rawTxRoot: "44".repeat(32),
        txRoot: "44".repeat(32),
        transitionTraceRoot: "55".repeat(32),
        eventToStepRoot: "66".repeat(32),
        validationTracesRoot: "77".repeat(32),
        transitionStepCount: txs.length,
        validationTraceCount: txs.length,
        utxoPayloadEntries: [],
        ledgerDelta: { spent: [], produced: [] },
        rejectedMempoolTxsCount: 0,
        rejectedMempoolTxHashes: [],
        rejectionEntries: [],
        includedDepositEntriesCount: 0,
        includedDepositEventIds: [],
        includedForcedTransactionEntriesCount: 0,
        includedForcedTransactionEventIds: [],
        includedWithdrawalEntriesCount: 0,
        includedWithdrawalEventIds: [],
        nativeMpfReplay: undefined,
        nativeMpfHandle: config.nativeMpf?.handle,
        mempoolTxHashes: txs.map((entry) => entry[TxTable.Columns.TX_ID]),
        sizeOfProcessedTxs: txs.length * CANONICAL_TX.length,
      };
    })) as unknown as typeof processMpfs);
  const candidates: SpeculativeCandidateReadyOutput["candidate"][] = [];
  const output = await Effect.runPromise(
    runCommitBlockHeaderWorkerProgram(workerInput, (candidate) => {
      candidates.push(candidate);
      return Effect.succeed({
        type: "InvalidateSpeculativeCandidate",
        reason: "T1",
      });
    }).pipe(
      Effect.provideService(NodeConfig, nodeConfig),
      Effect.provideService(ContractDeploymentIdentity, deploymentIdentity),
      Effect.provideService(UnownedHistoryFixture, true),
      Effect.provideService(SqlClient.SqlClient, fakeSql),
      Effect.provide(Logger.remove(Logger.defaultLogger)),
    ) as Effect.Effect<unknown, unknown, never>,
  );
  return { output, builds, candidates, owner: seams.owner };
};

// About 925 KB of retained validation trace per transaction: a hundred of them
// overflow one V1 frame.
const DEEP_LEDGER_TRANSFER: BlockShape = { witnessValueBytes: 8_000 };

describe("commit worker DA frame step-down", () => {
  it("rebuilds an overflowing block once from a fresh fork and hands on the smaller one", async () => {
    const run = await runWorker(100, DEEP_LEDGER_TRANSFER);
    expect(run.output).toMatchObject({
      type: "SpeculativeCandidateInvalidatedOutput",
    });
    expect(run.builds).toHaveLength(2);
    const [first, second] = run.builds;
    expect(first).toHaveLength(100);
    expect(second!.length).toBeGreaterThan(0);
    expect(second!.length).toBeLessThan(100);
    expect(second).toEqual(first!.slice(0, second!.length));
    // The superseded fork is discarded before the rebuild forks again, and
    // the rebuild runs on the new fork.
    expect(run.owner.slice(0, 5)).toEqual([
      "fork:1",
      "build:1",
      "discard:1",
      "fork:2",
      "build:2",
    ]);
    expect(run.candidates).toHaveLength(1);
    expect(run.candidates[0]?.expectedL2TransactionCount).toBe(second!.length);
  });

  it("builds a block that fits once", async () => {
    const run = await runWorker(3, {});
    expect(run.builds).toHaveLength(1);
    expect(run.owner.filter((event) => event.startsWith("fork"))).toEqual([
      "fork:1",
    ]);
    expect(run.candidates[0]?.expectedL2TransactionCount).toBe(3);
  });

  it("drops every transaction when the base ledger alone cannot fit", async () => {
    const run = await runWorker(
      3,
      {},
      {
        entryCount: 500_000,
        encodedTupleBytes: 500_000 * 148,
      },
    );
    expect(run.builds.map((build) => build.length)).toEqual([3, 0]);
    expect(run.candidates[0]?.expectedL2TransactionCount).toBe(0);
  });
});
