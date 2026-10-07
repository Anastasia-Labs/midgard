import { encodeMidgardCekProgramMaterialSidecar } from "@al-ft/midgard-core/cek-proof";
import { maxDaPayloadInnerBytes } from "@al-ft/midgard-core/da-payload-sizing";
import { SqlClient } from "@effect/sql";
import { Effect, Logger, Option } from "effect";
import { describe, expect, it, vi } from "vitest";

import { readDaHardeningConfig } from "../src/da/hardening-config.js";
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
  type Lucid,
  MidgardContracts,
  NodeConfig,
} from "../src/services/index.js";
import { runCommitBlockHeaderWorkerProgram } from "../src/workers/commit-block-header.js";
import { captureCommitWorkerFailure } from "../src/workers/commit-block-header.run-commit-block-header-worker-program.js";
import type { WorkerOutput } from "../src/workers/utils/commit-block-header.js";
import {
  COMMIT_DA_FRAME_IDLE_NOTICE,
  type CommitDaFrameNotice,
} from "../src/workers/utils/commit-block-planner.commit-da-frame-notice.js";
import {
  DEFAULT_COMMIT_BATCH_BUDGET_LIMITS,
  estimatedTxDaPayloadBytes,
} from "../src/workers/utils/commit-block-planner.js";
import {
  blockContentFor,
  type BlockShape,
  CANONICAL_TX,
  LC1_BASE_LEDGER,
} from "./helpers/commit-da-frame-fixtures.js";
import {
  deploymentIdentity,
  fakeSql,
  nodeConfig,
  workerInput,
} from "./helpers/commit-da-frame-worker-fixture.js";

// The worker reads its candidates, its confirmed_ledger base and the native
// owner through these seams; the block build itself is faked below.
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
    ConfirmedLedgerDB: {
      ...actual.ConfirmedLedgerDB,
      retrieve: Effect.succeed([]),
    },
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
        Effect.succeed({ entries: seams.candidates, nextCursor: null }),
      ),
      clearTxs: vi.fn(() => Effect.void),
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
    ProcessedMempoolDB: {
      ...actual.ProcessedMempoolDB,
      retrieve: Effect.succeed([]),
      insertTxs: vi.fn(() => Effect.void),
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
// The follower-change driver has ingested past any planned end time.
vi.mock("../src/database/follower-events.js", async (importOriginal) => ({
  ...(await importOriginal<
    typeof import("../src/database/follower-events.js")
  >()),
  followerEligibilityHorizon: Effect.succeed(Number.MAX_SAFE_INTEGER),
}));
vi.mock("../src/fibers/fetch-and-insert-tx-order-utxos.js", () => ({
  fetchAndInsertTxOrderUTxOsForCommitBarrier: vi.fn((end: Date) =>
    Effect.succeed(end),
  ),
}));
vi.mock("../src/e2e/commit-crash-checkpoint.js", () => ({
  reachCommitCrashCheckpoint: vi.fn(() => Effect.void),
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
    // The confirmed_ledger base carries the durable root and the aggregate
    // the case selects.
    computeLedgerMpfRootFromLedgerEntries: vi.fn(() =>
      Effect.succeed("33".repeat(32)),
    ),
    ledgerPayloadAggregateFromEntries: vi.fn(() => seams.baseAggregate),
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

/**
 * One commit worker run over `txCount` transactions of `shape`, from a base
 * ledger of `baseAggregate`. Each build returns the DA content its selection
 * would carry; with no confirmed block the run ends at the deferred candidate.
 */
const runWorker = async (
  txCount: number,
  shape: BlockShape,
  baseAggregate: UtxoPayloadSizeAggregate = LC1_BASE_LEDGER,
  invalidateOnRebuild = false,
) => {
  seams.candidates = Array.from({ length: txCount }, (_, index) =>
    candidateTx(index + 1),
  );
  seams.baseAggregate = baseAggregate;
  seams.owner = [];
  const builds: string[][] = [];
  const sourceWindows: (number | undefined)[] = [];
  vi.mocked(processMpfs).mockReset();
  vi.mocked(processMpfs).mockImplementation(((
    _transactionsMpf: unknown,
    txs: readonly EntryWithTimeStamp[],
    config: {
      readonly nativeMpf?: NativeMpfBuildContext;
      readonly fixedBlockEndTime?: Date;
    },
  ) =>
    Effect.sync(() => {
      sourceWindows.push(config.fixedBlockEndTime?.getTime());
      const handle = config.nativeMpf?.handle as unknown as { id: number };
      seams.owner.push(`build:${handle.id.toString()}`);
      builds.push(
        txs.map((entry) => entry[TxTable.Columns.TX_ID].toString("hex")),
      );
      return {
        ...blockContentFor(txs, {
          ...shape,
          base: baseAggregate,
          ...(invalidateOnRebuild && builds.length > 1
            ? { witnessValueBytes: (shape.witnessValueBytes ?? 1470) + 1 }
            : {}),
        }),
        effectiveBlockEndTime:
          config.fixedBlockEndTime ??
          txs[txs.length - 1]?.[TxTable.Columns.TIMESTAMPTZ],
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
        includedDepositEntriesCount: 0,
        includedDepositEventIds: [],
        includedForcedTransactionEntriesCount: 0,
        includedForcedTransactionEventIds: [],
        includedWithdrawalEntriesCount: 0,
        includedWithdrawalEventIds: [],
        nativeMpfReplay: undefined,
        nativeMpfHandle: config.nativeMpf?.handle,
        processedMempoolTxs: [...txs],
        mempoolTxHashes: txs.map((entry) => entry[TxTable.Columns.TX_ID]),
        sizeOfProcessedTxs: txs.length * CANONICAL_TX.length,
      };
    })) as unknown as typeof processMpfs);
  const notices: unknown[] = [];
  // The ingestion barriers are faked, so the provider is never read.
  const output = (await Effect.runPromise(
    captureCommitWorkerFailure(
      runCommitBlockHeaderWorkerProgram(
        workerInput,
        (message) => Effect.sync(() => notices.push(message)),
        () => Effect.succeed({} as Lucid),
      ),
    ).pipe(
      Effect.provideService(NodeConfig, nodeConfig),
      Effect.provideService(MidgardContracts, {} as never),
      Effect.provideService(ContractDeploymentIdentity, deploymentIdentity),
      Effect.provideService(UnownedHistoryFixture, true),
      Effect.provideService(SqlClient.SqlClient, fakeSql),
      Effect.provide(Logger.remove(Logger.defaultLogger)),
    ) as unknown as Effect.Effect<WorkerOutput, unknown, never>,
  )) as WorkerOutput;
  const daFrameNotices = notices.filter(
    (notice): notice is CommitDaFrameNotice =>
      (notice as { type?: string }).type === "CommitDaFrameNotice",
  );
  // The deferred block's L2 transaction count; none when no block was built.
  const deferredTxCounts =
    output.type === "SkippedSubmissionOutput" && output.candidate !== undefined
      ? [output.mempoolTxsCount]
      : [];
  return {
    output,
    builds,
    deferredTxCounts,
    owner: seams.owner,
    daFrameNotices,
    sourceWindows,
  };
};

// About 925 KB of retained validation trace per transaction: a hundred of them
// overflow one V1 frame.
const DEEP_LEDGER_TRANSFER: BlockShape = { witnessValueBytes: 8_000 };

describe("commit worker DA frame step-down", () => {
  it("rebuilds an overflowing block once from a fresh fork and hands on the smaller one", async () => {
    const run = await runWorker(100, DEEP_LEDGER_TRANSFER);
    expect(run.output).toMatchObject({ type: "SkippedSubmissionOutput" });
    expect(run.builds).toHaveLength(2);
    expect(run.sourceWindows[0]).toBeUndefined();
    expect(run.sourceWindows[1]).toBe(BLOCK_TIME + 100);
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
    expect(run.deferredTxCounts).toEqual([second!.length]);
    expect(run.daFrameNotices[0]?.pressure).toMatchObject({
      passes: 2,
      requiredWorkInnerBytesUpperBound: null,
    });
    expect(
      run.daFrameNotices[0]?.pressure?.initialCandidateInnerBytesUpperBound,
    ).toBeGreaterThan(
      maxDaPayloadInnerBytes(readDaHardeningConfig().envelopeMode),
    );
    expect(
      run.daFrameNotices[0]?.pressure?.candidateInnerBytesUpperBound,
    ).toBeLessThanOrEqual(
      maxDaPayloadInnerBytes(readDaHardeningConfig().envelopeMode),
    );
  });

  it("builds a block that fits once", async () => {
    const run = await runWorker(3, {});
    expect(run.builds).toHaveLength(1);
    expect(run.owner.filter((event) => event.startsWith("fork"))).toEqual([
      "fork:1",
    ]);
    expect(run.deferredTxCounts).toEqual([3]);
  });

  it("keeps a legal ordinary candidate for final exact authority when its base ledger upper bound overflows", async () => {
    const run = await runWorker(
      3,
      {},
      {
        entryCount: 500_000,
        encodedTupleBytes: 500_000 * 148,
      },
    );
    expect(run.builds.map((build) => build.length)).toEqual([3, 1]);
    expect(run.deferredTxCounts).toEqual([1]);
  });

  it("plans the selection against the base ledger before the first build", async () => {
    // A base ledger that leaves the planner room for about forty plain
    // transfers of the hundred the base-free plan admits.
    const limit = maxDaPayloadInnerBytes(readDaHardeningConfig().envelopeMode);
    const perTx = estimatedTxDaPayloadBytes(
      CANONICAL_TX.length,
      DEFAULT_COMMIT_BATCH_BUDGET_LIMITS,
    );
    const entryCount = Math.floor((limit - 40.5 * perTx) / 148);
    const run = await runWorker(
      100,
      {},
      {
        entryCount,
        encodedTupleBytes: entryCount * 148,
      },
    );
    expect(run.builds).toHaveLength(1);
    const [only] = run.builds;
    expect(only!.length).toBeGreaterThan(0);
    expect(only!.length).toBeLessThan(100);
    expect(run.deferredTxCounts).toEqual([only!.length]);
    expect(run.daFrameNotices.map((notice) => notice.status)).toEqual(["fits"]);
  });
});

describe("commit worker DA frame notices", () => {
  it("posts fits for a block the frame admits", async () => {
    const run = await runWorker(3, {});
    expect(run.daFrameNotices).toEqual([
      expect.objectContaining({
        type: "CommitDaFrameNotice",
        status: "fits",
        pressure: expect.objectContaining({
          candidateStagePercent: 0,
          acceptedTxCount: 3,
          requiredWorkInnerBytesUpperBound: null,
          effectiveInnerLimit: maxDaPayloadInnerBytes(
            readDaHardeningConfig().envelopeMode,
          ),
        }),
      }),
    ]);
  });

  it("posts provisional exact_check_required when mandatory event upper bounds overflow", async () => {
    const mode = readDaHardeningConfig().envelopeMode;
    const run = await runWorker(3, {
      eventBytes: maxDaPayloadInnerBytes(mode),
    });
    expect(run.builds.at(-1)).toEqual([]);
    expect(run.daFrameNotices.map((notice) => notice.status)).toEqual([
      "exact_check_required",
    ]);
    expect(run.daFrameNotices[0]?.detail).toContain(
      `effective_inner_limit=${maxDaPayloadInnerBytes(mode).toString()}`,
    );
    expect(run.daFrameNotices[0]?.pressure).toMatchObject({
      acceptedTxCount: 0,
      requiredWorkStagePercent: 90,
    });
    expect(
      run.daFrameNotices[0]?.pressure?.requiredWorkInnerBytesUpperBound,
    ).toBe(run.daFrameNotices[0]?.pressure?.candidateInnerBytesUpperBound);
  });

  it("posts the idle notice from a tick with no transaction or user event pending", async () => {
    const run = await runWorker(0, {});
    expect(run.output).toEqual({ type: "NothingToCommitOutput" });
    expect(run.builds).toEqual([]);
    expect(run.daFrameNotices).toEqual([COMMIT_DA_FRAME_IDLE_NOTICE]);
  });

  it("does not infer an exact ledger ceiling from a maximum-header overflow", async () => {
    const run = await runWorker(
      3,
      {},
      { entryCount: 500_000, encodedTupleBytes: 500_000 * 148 },
    );
    expect(run.daFrameNotices.map((notice) => notice.status)).toEqual([
      "exact_check_required",
    ]);
  });
});

// A changed rebuild must fail the worker before any block is deferred.
it("fails the production worker and discards its forks when rebuilt validation material changes", async () => {
  const run = await runWorker(100, DEEP_LEDGER_TRANSFER, LC1_BASE_LEDGER, true);
  expect(run.output).toMatchObject({ type: "FailureOutput" });
  expect(run.builds).toHaveLength(2);
  expect(run.deferredTxCounts).toEqual([]);
  expect(run.daFrameNotices.map((notice) => notice.status)).toEqual([
    "incomplete",
  ]);
  expect(run.owner.filter((entry) => entry.startsWith("discard:"))).toEqual([
    "discard:1",
    "discard:2",
  ]);
});
