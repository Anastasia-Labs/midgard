import { encodeMidgardCekProgramMaterialSidecar } from "@al-ft/midgard-core/cek-proof";
import { maxDaPayloadInnerBytes } from "@al-ft/midgard-core/da-payload-sizing";
import { SqlClient } from "@effect/sql";
import { Effect, Logger, Option } from "effect";
import { describe, expect, it, vi } from "vitest";

import { readDaHardeningConfig } from "../src/da/hardening-config.js";
import { TxUtils as TxTable } from "../src/database/index.js";
import type { EntryWithTimeStamp } from "../src/database/utils/tx.js";
import { COMMIT_REJECT_CODE_DA_OVERSIZE } from "../src/mpf/commit-rejection.js";
import {
  type NativeMpfBuildContext,
  processMpfs,
  type UtxoPayloadSizeAggregate,
} from "../src/mpf/index.js";
import { FollowerWriteFixture } from "../src/services/follower-write-gate.js";
import {
  ContractDeploymentIdentity,
  type Lucid,
  MidgardContracts,
  NodeConfig,
} from "../src/services/index.js";
import { runCommitBlockHeaderWorkerProgram } from "../src/workers/commit-block-header.js";
import { captureCommitWorkerFailure } from "../src/workers/commit-block-header.run-commit-block-header-worker-program.js";
import {
  DA_PAYLOAD_HEADER_SLACK_BYTES,
  oversizeSingleNormalTx,
} from "../src/workers/commit-block-header/commit-da-oversize-eviction.js";
import { measureCommitDaPayloadUpperBound } from "../src/workers/commit-block-header/submission.assert-pre-submit-da-payload-size.js";
import type { WorkerOutput } from "../src/workers/utils/commit-block-header.js";
import {
  blockContentFor,
  CANONICAL_TX,
  LC1_BASE_LEDGER,
  PLACEHOLDER_DESCRIPTOR_BYTES,
  preSubmit,
  REFUSAL,
  syntheticCommitPrefixMeasurement,
  traceFor,
} from "./helpers/commit-da-frame-fixtures.js";
import {
  deploymentIdentity,
  fakeSql,
  nodeConfig,
  workerInput,
} from "./helpers/commit-da-frame-worker-fixture.js";
import { withoutFollowerJournal } from "./helpers/intent-journal.js";

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
const MODE = readDaHardeningConfig().envelopeMode;
const LIMIT = maxDaPayloadInnerBytes(MODE);
const seedOf = (entry: EntryWithTimeStamp) =>
  Number.parseInt(entry[TxTable.Columns.TX_ID].toString("hex"), 16);
// A descriptor as large as the whole DA frame.
const OVERSIZE_DESCRIPTOR_BYTES = LIMIT;

/**
 * One commit worker run over transactions 1..`txCount` from a base ledger of
 * `baseAggregate`; the transactions in `oversize` carry a descriptor as large
 * as the DA frame. Each build drops the transactions the worker evicts, as the
 * real build rejects them before validation.
 */
const runWorker = async (
  txCount: number,
  oversize: ReadonlySet<number>,
  baseAggregate: UtxoPayloadSizeAggregate = LC1_BASE_LEDGER,
) => {
  seams.candidates = Array.from({ length: txCount }, (_, index) =>
    candidateTx(index + 1),
  );
  seams.baseAggregate = baseAggregate;
  seams.owner = [];
  const builds: {
    readonly selected: readonly number[];
    readonly evicted: Readonly<Record<number, string>>;
  }[] = [];
  vi.mocked(processMpfs).mockReset();
  vi.mocked(processMpfs).mockImplementation(((
    _transactionsMpf: unknown,
    txs: readonly EntryWithTimeStamp[],
    config: {
      readonly nativeMpf?: NativeMpfBuildContext;
      readonly fixedBlockEndTime?: Date;
      readonly commitEvictions?: ReadonlyMap<string, { code: string }>;
    },
  ) =>
    Effect.sync(() => {
      const handle = config.nativeMpf?.handle as unknown as { id: number };
      seams.owner.push(`build:${handle.id.toString()}`);
      const evictions = config.commitEvictions ?? new Map();
      const isEvicted = (entry: EntryWithTimeStamp) =>
        evictions.has(entry[TxTable.Columns.TX_ID].toString("hex"));
      const kept = txs.filter((entry) => !isEvicted(entry));
      builds.push({
        selected: txs.map(seedOf),
        evicted: Object.fromEntries(
          [...evictions].map(([id, eviction]) => [
            Number.parseInt(id, 16),
            eviction.code,
          ]),
        ),
      });
      return {
        ...blockContentFor(kept, { base: baseAggregate }),
        validationTraceMembers: kept.map((entry) =>
          traceFor(
            seedOf(entry),
            oversize.has(seedOf(entry))
              ? OVERSIZE_DESCRIPTOR_BYTES
              : PLACEHOLDER_DESCRIPTOR_BYTES,
          ),
        ),
        effectiveBlockEndTime:
          config.fixedBlockEndTime ??
          txs[txs.length - 1]?.[TxTable.Columns.TIMESTAMPTZ],
        utxoRoot: "33".repeat(32),
        rawTxRoot: "44".repeat(32),
        txRoot: "44".repeat(32),
        transitionTraceRoot: "55".repeat(32),
        eventToStepRoot: "66".repeat(32),
        validationTracesRoot: "77".repeat(32),
        transitionStepCount: kept.length,
        validationTraceCount: kept.length,
        utxoPayloadEntries: [],
        ledgerDelta: { spent: [], produced: [] },
        rejectedMempoolTxsCount: txs.length - kept.length,
        rejectedMempoolTxHashes: txs
          .filter(isEvicted)
          .map((entry) => entry[TxTable.Columns.TX_ID]),
        includedDepositEntriesCount: 0,
        includedDepositEventIds: [],
        includedForcedTransactionEntriesCount: 0,
        includedForcedTransactionEventIds: [],
        includedWithdrawalEntriesCount: 0,
        includedWithdrawalEventIds: [],
        nativeMpfReplay: undefined,
        nativeMpfHandle: config.nativeMpf?.handle,
        processedMempoolTxs: [...kept],
        mempoolTxHashes: kept.map((entry) => entry[TxTable.Columns.TX_ID]),
        sizeOfProcessedTxs: kept.length * CANONICAL_TX.length,
      };
    })) as unknown as typeof processMpfs);
  // The ingestion barriers are faked, so the provider is never read.
  const output = (await Effect.runPromise(
    captureCommitWorkerFailure(
      runCommitBlockHeaderWorkerProgram(
        workerInput,
        () => Effect.void,
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
  return { output, builds, owner: seams.owner };
};

describe("commit worker eviction of a transaction the DA frame cannot hold", () => {
  it("evicts a transaction whose block alone exceeds the exact frame, and the next block commits", async () => {
    // The exact pre-submit check refuses the block of that one transaction
    // and admits the empty block over the same ledger.
    expect(
      await preSubmit([candidateTx(1)], MODE, {
        descriptorBytes: OVERSIZE_DESCRIPTOR_BYTES,
      }),
    ).toMatchObject({ _tag: "Left", left: { message: REFUSAL } });
    expect(await preSubmit([], MODE)).toMatchObject({ _tag: "Right" });

    const run = await runWorker(3, new Set([1]));
    expect(run.builds).toEqual([
      { selected: [1, 2, 3], evicted: {} },
      { selected: [1], evicted: {} },
      { selected: [1, 2, 3], evicted: { 1: COMMIT_REJECT_CODE_DA_OVERSIZE } },
    ]);
    // Every pass builds on a fresh fork of the same base.
    expect(run.owner).toEqual([
      "fork:1",
      "build:1",
      "discard:1",
      "fork:2",
      "build:2",
      "discard:2",
      "fork:3",
      "build:3",
      "discard:3",
    ]);
    expect(run.output).toMatchObject({
      type: "SkippedSubmissionOutput",
      mempoolTxsCount: 2,
    });
  }, 120_000);

  it("evicts a lone oversize transaction without failing the worker", async () => {
    const run = await runWorker(1, new Set([1]));
    expect(run.builds).toEqual([
      { selected: [1], evicted: {} },
      { selected: [1], evicted: { 1: COMMIT_REJECT_CODE_DA_OVERSIZE } },
    ]);
    expect(run.output.type).not.toBe("FailureOutput");
  }, 120_000);

  it("keeps a lone transaction for the exact check when the base ledger's empty block may not fit", async () => {
    const run = await runWorker(1, new Set(), {
      entryCount: 500_000,
      encodedTupleBytes: 500_000 * 148,
    });
    expect(run.builds).toEqual([{ selected: [1], evicted: {} }]);
    expect(run.output).toMatchObject({ mempoolTxsCount: 1 });
  });
});

describe("oversize single normal transaction", () => {
  const id = "01".repeat(32);
  const lowest = LIMIT + DA_PAYLOAD_HEADER_SLACK_BYTES;
  const decide = (
    prefixBytes: readonly number[],
    { mandatory = false, ids = [id], outcome = "exact_check_required" } = {},
  ) =>
    oversizeSingleNormalTx(
      outcome,
      syntheticCommitPrefixMeasurement(ids, prefixBytes, mandatory),
      LIMIT,
    )?.txIdHex;

  it("evicts only when no header can bring the block within the frame", () => {
    expect(decide([1_000, lowest + 1])).toBe(id);
    expect(decide([1_000, lowest])).toBeUndefined();
  });

  it("leaves a block whose overflow is not the transaction's alone", () => {
    expect(decide([LIMIT + 1, lowest + 1])).toBeUndefined();
    expect(decide([1_000, lowest + 1], { mandatory: true })).toBeUndefined();
    expect(
      decide([1_000, 2_000, lowest + 1], { ids: [id, "02".repeat(32)] }),
    ).toBeUndefined();
    expect(decide([1_000, lowest + 1], { outcome: "fits" })).toBeUndefined();
  });

  it("bounds the measured header's slack over a real header", async () => {
    const txs = [candidateTx(2)];
    const exact = await preSubmit(txs, MODE);
    const measured = await Effect.runPromise(
      measureCommitDaPayloadUpperBound({
        ...blockContentFor(txs),
        rejectedTxIds: [],
      }).pipe(
        Effect.provide(Logger.remove(Logger.defaultLogger)),
      ) as Effect.Effect<{ readonly innerBytesUpperBound: number } | undefined>,
    );
    expect(exact._tag).toBe("Right");
    const exactBytes = (exact as { right: number }).right;
    expect(DA_PAYLOAD_HEADER_SLACK_BYTES).toBeGreaterThan(0);
    expect(measured!.innerBytesUpperBound).toBeGreaterThanOrEqual(exactBytes);
    expect(
      measured!.innerBytesUpperBound - DA_PAYLOAD_HEADER_SLACK_BYTES,
    ).toBeLessThanOrEqual(exactBytes);
  });
});
