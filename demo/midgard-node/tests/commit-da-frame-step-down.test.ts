import { encodeMidgardCekProgramMaterialSidecar } from "@al-ft/midgard-core/cek-proof";
import {
  type DaPayloadEmissionMode,
  maxDaPayloadInnerBytes,
} from "@al-ft/midgard-core/da-payload-sizing";
import { Effect, Either, Logger } from "effect";
import { describe, expect, it, vi } from "vitest";

import {
  Columns as TxColumns,
  type EntryWithTimeStamp,
} from "../src/database/utils/tx.js";
import type { UtxoPayloadSizeAggregate } from "../src/mpf/index.js";
import { measureCommitDaPayloadUpperBound } from "../src/workers/commit-block-header/submission.assert-pre-submit-da-payload-size.js";
import {
  type CommitDaFrameMeasurement,
  type CommitTxCandidateSelection,
  emptyBlockDaPayloadUpperBoundBytes,
  planCommitBatchBudgets,
  planCommitDaFrameStepDown,
  selectCommitTxCandidates,
  stepDownCommitSelectionToDaFrame,
} from "../src/workers/utils/commit-block-planner.js";
import {
  blockContentFor,
  type BlockShape,
  LC1_BASE_LEDGER,
  LC1_MEAN_ENTRY_BYTES,
  mkCandidate,
  MODES,
  preSubmit,
  PROGRAM_LIMITS,
  REFUSAL,
} from "./helpers/commit-da-frame-fixtures.js";

// Every normal transaction has its durable program-material sidecar.
vi.mock("../src/database/index.js", async () => {
  const actual = await vi.importActual<
    typeof import("../src/database/index.js")
  >("../src/database/index.js");
  return {
    ...actual,
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
  };
});

const quiet = <A, E>(effect: Effect.Effect<A, E, unknown>) =>
  Effect.runPromise(
    effect.pipe(
      Effect.provide(Logger.remove(Logger.defaultLogger)),
    ) as Effect.Effect<A, E, never>,
  );

const txIdsOf = (txs: readonly EntryWithTimeStamp[]) =>
  txs.map((entry) => entry[TxColumns.TX_ID].toString("hex"));

/**
 * Runs the step-down over a fake block build whose DA content has `shape`,
 * measured by the commit program's real upper-bound measurement.
 */
const stepDown = async (
  candidateSelection: CommitTxCandidateSelection,
  mode: DaPayloadEmissionMode,
  shape: BlockShape,
) => {
  const processed: (readonly EntryWithTimeStamp[])[] = [];
  const measured: (CommitDaFrameMeasurement | undefined)[] = [];
  let rebases = 0;
  const result = await quiet(
    stepDownCommitSelectionToDaFrame({
      candidateSelection,
      baseEmptyBlockInnerBytes: emptyBlockDaPayloadUpperBoundBytes(
        shape.base ?? LC1_BASE_LEDGER,
      ),
      maxInnerBytes: maxDaPayloadInnerBytes(mode),
      process: (selection) =>
        Effect.sync(() => {
          processed.push(selection.candidateTxs);
          return blockContentFor(selection.candidateTxs, shape);
        }),
      measure: (built) =>
        measureCommitDaPayloadUpperBound({
          ...built,
          rejectedTxIds: [],
        }).pipe(Effect.tap((measurement) => measured.push(measurement))),
      rebase: Effect.sync(() => {
        rebases += 1;
      }),
    }),
  );
  return { result, processed, measured, rebases };
};

const threeTransfers = () =>
  selectCommitTxCandidates({
    mempoolTxs: Array.from({ length: 3 }, (_, index) => mkCandidate(index + 1)),
    processedMempoolTxs: [],
  });

// A plain transfer whose retained validation trace is about 350 KB: heavier
// than the planner's per-transaction allowance, as a deep ledger makes it.
const HEAVY_TRANSFER: BlockShape = { witnessValueBytes: 3_000 };

describe("commit DA frame step-down", () => {
  it.each(MODES)(
    "steps an overflowing selection down once and commits the smaller block (%s)",
    async (mode) => {
      const planned = planCommitBatchBudgets({
        candidateSelection: selectCommitTxCandidates({
          mempoolTxs: Array.from({ length: 400 }, (_, index) =>
            mkCandidate(index + 1),
          ),
          processedMempoolTxs: [],
        }),
        limits: PROGRAM_LIMITS(mode),
        baseUtxoPayloadAggregate: LC1_BASE_LEDGER,
      });
      // The transient: the allowance admitted a block the frame cannot carry.
      const unstepped = await preSubmit(
        planned.candidateSelection.candidateTxs,
        mode,
        HEAVY_TRANSFER,
      );
      expect(Either.isLeft(unstepped)).toBe(true);

      const run = await stepDown(
        planned.candidateSelection,
        mode,
        HEAVY_TRANSFER,
      );
      // One build overflows, one rebuild from a shorter prefix fits.
      expect(run.processed).toHaveLength(2);
      expect(run.rebases).toBe(1);
      expect(run.result.passes).toBe(2);
      const finalTxs = run.result.candidateSelection.candidateTxs;
      expect(finalTxs.length).toBeGreaterThan(0);
      expect(finalTxs.length).toBeLessThan(
        planned.candidateSelection.candidateTxs.length,
      );
      expect(txIdsOf(finalTxs)).toEqual(
        txIdsOf(planned.candidateSelection.candidateTxs).slice(
          0,
          finalTxs.length,
        ),
      );
      expect(run.result.processed.processedMempoolTxs).toBe(finalTxs);

      // The block that reaches commit passes the pre-submit check, and the
      // measurement bounded it from above.
      const committed = await preSubmit(finalTxs, mode, HEAVY_TRANSFER);
      expect(Either.isRight(committed)).toBe(true);
      if (Either.isLeft(committed)) return;
      expect(committed.right).toBeLessThanOrEqual(maxDaPayloadInnerBytes(mode));
      expect(run.measured[1]?.innerBytesUpperBound).toBeGreaterThanOrEqual(
        committed.right,
      );

      // The same mempool steps down to the same block.
      const again = await stepDown(
        planned.candidateSelection,
        mode,
        HEAVY_TRANSFER,
      );
      expect(txIdsOf(again.result.candidateSelection.candidateTxs)).toEqual(
        txIdsOf(finalTxs),
      );
    },
  );

  it.each(MODES)(
    "builds a fitting selection once with no rebuild (%s)",
    async (mode) => {
      const selection = threeTransfers();
      const run = await stepDown(selection, mode, {});
      expect(run.processed).toHaveLength(1);
      expect(run.rebases).toBe(0);
      expect(run.result.candidateSelection).toBe(selection);
    },
  );

  it.each(MODES)(
    "drops every transaction from a ledger whose empty block cannot fit, and the commit is still refused (%s)",
    async (mode) => {
      const limit = maxDaPayloadInnerBytes(mode);
      const base: UtxoPayloadSizeAggregate = {
        entryCount: Math.ceil(limit / LC1_MEAN_ENTRY_BYTES),
        encodedTupleBytes: limit,
      };
      const selection = threeTransfers();
      const run = await stepDown(selection, mode, { base });
      // The floor is reached in one step and never below it.
      expect(run.processed).toHaveLength(2);
      expect(run.rebases).toBe(1);
      expect(run.result.candidateSelection.candidateTxs).toEqual([]);
      for (const txs of [selection.candidateTxs, []]) {
        const result = await preSubmit(txs, mode, { base });
        expect(Either.isLeft(result)).toBe(true);
        if (Either.isRight(result)) return;
        expect(result.left.message).toBe(REFUSAL);
        expect(String(result.left.cause)).toContain(
          "post_block_ledger_without_events_exceeds_frame=true",
        );
      }
    },
  );

  it.each(MODES)(
    "commits the withdrawals that shrink an over-frame ledger once its transactions are dropped (%s)",
    async (mode) => {
      const limit = maxDaPayloadInnerBytes(mode);
      // Ten entries past the last ledger an empty block can carry, with
      // withdrawals in the window that remove a hundred of them.
      const brick = Math.floor(
        (limit - emptyBlockDaPayloadUpperBoundBytes(LC1_BASE_LEDGER)) /
          LC1_MEAN_ENTRY_BYTES,
      );
      const base: UtxoPayloadSizeAggregate = {
        entryCount: brick + 10,
        encodedTupleBytes: (brick + 10) * LC1_MEAN_ENTRY_BYTES,
      };
      expect(emptyBlockDaPayloadUpperBoundBytes(base)).toBeGreaterThan(limit);
      const shape: BlockShape = { base, withdrawnEntryCount: 100 };
      const selection = threeTransfers();
      expect(
        Either.isLeft(await preSubmit(selection.candidateTxs, mode, shape)),
      ).toBe(true);
      const run = await stepDown(selection, mode, shape);
      expect(run.processed).toHaveLength(2);
      expect(run.rebases).toBe(1);
      expect(run.result.candidateSelection.candidateTxs).toEqual([]);
      expect(Either.isRight(await preSubmit([], mode, shape))).toBe(true);
    },
  );

  it.each(MODES)(
    "drops every transaction when events alone overflow, and the commit is still refused (%s)",
    async (mode) => {
      const shape: BlockShape = { eventBytes: maxDaPayloadInnerBytes(mode) };
      const selection = selectCommitTxCandidates({
        mempoolTxs: Array.from({ length: 5 }, (_, index) =>
          mkCandidate(index + 1),
        ),
        processedMempoolTxs: [],
      });
      const run = await stepDown(selection, mode, shape);
      expect(run.result.candidateSelection.candidateTxs).toEqual([]);
      expect(run.processed.length).toBeLessThanOrEqual(3 + Math.log2(5));
      expect(run.rebases).toBe(run.processed.length - 1);
      const result = await preSubmit([], mode, shape);
      expect(Either.isLeft(result)).toBe(true);
      if (Either.isRight(result)) return;
      expect(result.left.message).toBe(REFUSAL);
    },
  );
});

describe("commit DA frame step-down decision", () => {
  const decide = (
    innerBytesUpperBound: number,
    acceptedTxCount: number,
    pass: number,
    baseEmptyBlockInnerBytes = 1_000,
  ) =>
    planCommitDaFrameStepDown({
      measurement: { innerBytesUpperBound, acceptedTxCount, rejectedTxIds: [] },
      baseEmptyBlockInnerBytes,
      maxInnerBytes: 100_000,
      pass,
    });

  it("keeps a block that fits, at the limit exactly", () => {
    expect(decide(100_000, 10, 0)).toEqual({ status: "fits" });
  });

  it("goes straight to the floor for an over-frame ledger and stops there", () => {
    expect(decide(200_000, 10, 0, 100_001)).toEqual({
      status: "step_down",
      nextTxCount: 0,
    });
    expect(decide(200_000, 0, 3)).toEqual({
      status: "no_transactions_to_drop",
    });
  });

  it("scales by measured cost on the first pass and halves after", () => {
    // 10 transactions cost 198,000 over the base: 4 fit with the safety margin.
    expect(decide(199_000, 10, 0)).toEqual({
      status: "step_down",
      nextTxCount: 4,
    });
    // A barely overflowing block still drops a transaction.
    expect(decide(100_001, 10, 0)).toEqual({
      status: "step_down",
      nextTxCount: 8,
    });
    expect(decide(100_001, 10, 1)).toEqual({
      status: "step_down",
      nextTxCount: 5,
    });
    expect(decide(100_001, 1, 1)).toEqual({
      status: "step_down",
      nextTxCount: 0,
    });
  });

  it("reaches the empty floor within 3 + log2(n) passes", () => {
    for (const n of [1, 2, 3, 7, 100, 10_000]) {
      let count = n;
      let passes = 0;
      for (;;) {
        const decision = decide(100_001, count, passes);
        passes += 1;
        if (decision.status !== "step_down") break;
        expect(decision.nextTxCount).toBeLessThan(count);
        count = decision.nextTxCount;
      }
      expect(count).toBe(0);
      expect(passes).toBeLessThanOrEqual(3 + Math.log2(n));
    }
  });
});
