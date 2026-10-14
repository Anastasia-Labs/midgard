import { encodeMidgardCekProgramMaterialSidecar } from "@al-ft/midgard-core/cek-proof";
import { maxDaPayloadInnerBytes } from "@al-ft/midgard-core/da-payload-sizing";
import { SqlClient } from "@effect/sql";
import { Effect, Logger, Option } from "effect";
import { describe, expect, it, vi } from "vitest";

import { readDaHardeningConfig } from "../src/da/hardening-config.js";
import { TxUtils as TxTable } from "../src/database/index.js";
import type { EntryWithTimeStamp } from "../src/database/utils/tx.js";
import { type NativeMpfBuildContext, processMpfs } from "../src/mpf/index.js";
import { FollowerWriteFixture } from "../src/services/follower-write-gate.js";
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
  blockContentFor,
  type BlockShape,
  CANONICAL_TX,
  SYNTHETIC_TX_DA_BYTES,
} from "./helpers/commit-da-frame-fixtures.js";
import {
  deploymentIdentity,
  fakeSelectedEventPlan,
  fakeSql,
  nodeConfig,
  workerInput,
} from "./helpers/commit-da-frame-worker-fixture.js";
import { withoutFollowerJournal } from "./helpers/intent-journal.js";

// The worker reads its candidates, its confirmed_ledger base and the native
// owner through these seams; the block build itself is faked below.
const seams = vi.hoisted(() => ({
  candidates: [] as unknown[],
  owner: [] as string[],
  catchUp: false,
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
// The catch-up clock fixture supplies a modeled complete source instant. The
// real source encoding/budget and material-prefix counterexamples are tested in
// commit-additive-da-material; normal worker fixtures use the production planner.
vi.mock(
  "../src/workers/commit-block-header.plan-additive-da.js",
  async (importOriginal) => {
    const actual =
      await importOriginal<
        typeof import("../src/workers/commit-block-header.plan-additive-da.js")
      >();
    return {
      ...actual,
      planAdditiveCommitDa: (
        input: Parameters<typeof actual.planAdditiveCommitDa>[0],
      ) =>
        seams.catchUp
          ? Effect.succeed({
              kind: "selected",
              window: input.window,
              candidateCount: 0,
              upperBoundBytes: BigInt(input.maxInnerBytes),
            } as const)
          : actual.planAdditiveCommitDa(input),
    };
  },
);
// The commit anchor caps no planned end time.
vi.mock("../src/services/history-commit-window.js", async (importOriginal) => ({
  ...(await importOriginal<
    typeof import("../src/services/history-commit-window.js")
  >()),
  commitEventHorizon: () =>
    Effect.succeed({
      horizonMs: Number.MAX_SAFE_INTEGER,
      anchor: { hash: Buffer.alloc(32), height: 0, slot: 0 },
    }),
}));
// The worker composition fixture supplies an independent selected clock/window;
// real clock resolution and grouped SQL counts have their own causal suites.
vi.mock("../src/workers/commit-block-header.plan-event-window.js", () => ({
  planHistoryCommitEventWindow: () => fakeSelectedEventPlan(seams.catchUp),
}));
vi.mock("../src/forced-orders/horizon.js", () => ({
  forcedOrderHorizon: Effect.succeed(null),
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
    // The confirmed_ledger base carries the durable root.
    computeLedgerMpfRootFromLedgerEntries: vi.fn(() =>
      Effect.succeed("33".repeat(32)),
    ),
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
 * One commit worker run over `txCount` transactions of `shape`. Each build returns the DA content its selection
 * would carry; with no confirmed block the run ends at the deferred candidate.
 */
const runWorker = async (
  txCount: number,
  shape: BlockShape,
  invalidateOnRebuild = false,
  catchUp = false,
) => {
  seams.candidates = Array.from({ length: txCount }, (_, index) =>
    candidateTx(index + 1),
  );
  seams.owner = [];
  seams.catchUp = catchUp;
  const builds: string[][] = [];
  const sourceWindows: (number | undefined)[] = [];
  const commitClocks: (number | undefined)[] = [];
  vi.mocked(processMpfs).mockReset();
  vi.mocked(processMpfs).mockImplementation(((
    _transactionsMpf: unknown,
    txs: readonly EntryWithTimeStamp[],
    config: {
      readonly nativeMpf?: NativeMpfBuildContext;
      readonly fixedBlockEndTime?: Date;
      readonly fixedBlockTime?: Date;
    },
  ) =>
    Effect.sync(() => {
      sourceWindows.push(config.fixedBlockEndTime?.getTime());
      commitClocks.push(config.fixedBlockTime?.getTime());
      const handle = config.nativeMpf?.handle as unknown as { id: number };
      seams.owner.push(`build:${handle.id.toString()}`);
      builds.push(
        txs.map((entry) => entry[TxTable.Columns.TX_ID].toString("hex")),
      );
      return {
        ...blockContentFor(txs, {
          ...shape,
          ...(invalidateOnRebuild && builds.length > 1
            ? {
                descriptorBytes:
                  (shape.descriptorBytes ?? SYNTHETIC_TX_DA_BYTES) + 112,
              }
            : {}),
        }),
        effectiveBlockTime: config.fixedBlockTime,
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
      Effect.provideService(FollowerWriteFixture, true),
      Effect.provideService(SqlClient.SqlClient, fakeSql),
      Effect.provide(Logger.remove(Logger.defaultLogger)),
      withoutFollowerJournal,
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
    commitClocks,
  };
};

// About 900 KB of DA weight per transaction (see SYNTHETIC_TX_DA_BYTES).
const HEAVY_TRANSFER: BlockShape = { descriptorBytes: 896_000 };

describe("commit worker DA frame step-down", () => {
  it("keeps the commit clock current when catch-up removes normal transactions", async () => {
    const run = await runWorker(3, { eventBytes: 100 }, false, true);
    expect(run.builds).toEqual([[]]);
    expect(run.sourceWindows).toEqual([BLOCK_TIME - 299_000]);
    expect(run.commitClocks).toEqual([BLOCK_TIME + 1_000]);
    expect(run.commitClocks[0]).toBeGreaterThan(run.sourceWindows[0]!);
  });

  it("holds and alarms an underestimated source after one build without descending replay", async () => {
    const run = await runWorker(100, HEAVY_TRANSFER);
    expect(run.output).toMatchObject({ type: "FailureOutput" });
    expect(run.builds).toHaveLength(1);
    expect(run.deferredTxCounts).toEqual([]);
    expect(run.owner.filter((event) => event.startsWith("fork:"))).toEqual([
      "fork:1",
    ]);
    expect(run.daFrameNotices.map((notice) => notice.status)).toEqual([
      "incomplete",
    ]);
  });

  it("builds a block that fits once", async () => {
    const run = await runWorker(3, {});
    expect(run.builds).toHaveLength(1);
    expect(run.owner.filter((event) => event.startsWith("fork"))).toEqual([
      "fork:1",
    ]);
    expect(run.deferredTxCounts).toEqual([3]);
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

  it("posts the idle notice from a tick with no transaction or user event pending", async () => {
    const run = await runWorker(0, {});
    expect(run.output).toEqual({ type: "NothingToCommitOutput" });
    expect(run.builds).toEqual([]);
    expect(run.daFrameNotices).toEqual([COMMIT_DA_FRAME_IDLE_NOTICE]);
  });
});
