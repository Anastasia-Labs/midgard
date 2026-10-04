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
  commitDaFrameStepDownPassBound,
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
      baseUtxoPayloadAggregate: shape.base ?? LC1_BASE_LEDGER,
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
  it("finds an interior-only fitting prefix without discarding unvisited counts", async () => {
    // A robustness vector, not a claimed production serialization witness.
    const selection = selectCommitTxCandidates({
      mempoolTxs: Array.from({ length: 10 }, (_, index) =>
        mkCandidate(index + 1),
      ),
      processedMempoolTxs: [],
    });
    const prefixBytes = Array.from({ length: 11 }, (_, count) =>
      count === 8 ? 900 : 2_000,
    );
    const built: number[] = [];
    const result = await quiet(
      stepDownCommitSelectionToDaFrame({
        candidateSelection: selection,
        baseUtxoPayloadAggregate: { entryCount: 1, encodedTupleBytes: 2_000 },
        maxInnerBytes: 1_000,
        process: (candidate) =>
          Effect.sync(() => {
            built.push(candidate.candidateTxs.length);
            return candidate;
          }),
        measure: (candidate) =>
          Effect.succeed({
            innerBytesUpperBound: prefixBytes[candidate.candidateTxs.length]!,
            acceptedTxCount: candidate.candidateTxs.length,
            rejectedTxIds: [],
            acceptedTxIds: candidate.candidateTxHashes,
            hasMandatoryWork: false,
            prefixes: prefixBytes
              .slice(0, candidate.candidateTxs.length + 1)
              .map((innerBytesUpperBound, index) => ({
                innerBytesUpperBound,
                materialDigest: index.toString(16).padStart(64, "0"),
              })),
          }),
        rebase: Effect.void,
      }),
    );
    expect(result.outcome).toBe("fits");
    expect(result.candidateSelection.candidateTxs).toHaveLength(8);
    expect(built).toEqual([10, 8]);
    expect(built.reduce((sum, count) => sum + count, 0)).toBeLessThanOrEqual(
      30,
    );
  });

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
    "chooses the least expensive legal ordinary prefix for an over-frame ledger, and the commit is still refused (%s)",
    async (mode) => {
      const limit = maxDaPayloadInnerBytes(mode);
      const base: UtxoPayloadSizeAggregate = {
        entryCount: Math.ceil(limit / LC1_MEAN_ENTRY_BYTES),
        encodedTupleBytes: limit,
      };
      const selection = threeTransfers();
      const run = await stepDown(selection, mode, { base });
      // An empty/no-op block cannot replace a legal ordinary candidate.
      expect(run.processed).toHaveLength(2);
      expect(run.rebases).toBe(1);
      expect(run.result.candidateSelection.candidateTxs).toHaveLength(1);
      expect(run.result.outcome).toBe("exact_check_required");
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
      expect(run.processed.length).toBeLessThanOrEqual(
        commitDaFrameStepDownPassBound(5),
      );
      expect(run.rebases).toBe(run.processed.length - 1);
      const result = await preSubmit([], mode, shape);
      expect(Either.isLeft(result)).toBe(true);
      if (Either.isRight(result)) return;
      expect(result.left.message).toBe(REFUSAL);
    },
  );
});

describe("complete commit DA prefix selection", () => {
  const measurement = (
    bytes: readonly number[],
    mandatory = false,
  ): CommitDaFrameMeasurement => ({
    innerBytesUpperBound: bytes.at(-1)!,
    acceptedTxCount: bytes.length - 1,
    acceptedTxIds: bytes
      .slice(1)
      .map((_, index) => mkCandidate(index + 1)[TxColumns.TX_ID]),
    rejectedTxIds: [],
    hasMandatoryWork: mandatory,
    prefixes: bytes.map((innerBytesUpperBound, index) => ({
      innerBytesUpperBound,
      materialDigest: index.toString(16).padStart(64, "0"),
    })),
  });
  it("selects the greatest fitting prefix even if its predecessors and successors overflow", () => {
    expect(
      planCommitDaFrameStepDown({
        measurement: measurement([1, 200, 99, 200, 100, 200]),
        maxInnerBytes: 100,
      }),
    ).toEqual({ status: "step_down", nextTxCount: 4 });
  });
  it("selects the global minimum when every upper bound overflows, preferring greater ties", () => {
    expect(
      planCommitDaFrameStepDown({
        measurement: measurement([1, 200, 150, 200, 150, 201]),
        maxInnerBytes: 100,
      }),
    ).toEqual({ status: "step_down", nextTxCount: 4 });
    expect(
      planCommitDaFrameStepDown({
        measurement: measurement([300, 200, 150]),
        maxInnerBytes: 100,
      }),
    ).toEqual({ status: "exact_check_required" });
  });
  it("never infers an empty-ledger ceiling or chooses a zero/no-op candidate while legal ordinary prefixes remain", () => {
    expect(
      planCommitDaFrameStepDown({
        measurement: measurement([1, 200]),
        maxInnerBytes: 100,
      }),
    ).toEqual({ status: "exact_check_required" });
    expect(
      planCommitDaFrameStepDown({
        measurement: measurement([101, 200], true),
        maxInnerBytes: 100,
      }),
    ).toEqual({ status: "step_down", nextTxCount: 0 });
    expect(
      planCommitDaFrameStepDown({
        measurement: measurement([101], true),
        maxInnerBytes: 100,
      }),
    ).toEqual({ status: "exact_check_required" });
  });
  it("holds missing, duplicated and unsafe accounting instead of guessing unseen prefix bytes", () => {
    const good = measurement([1, 2, 3]);
    for (const bad of [
      { ...good, prefixes: good.prefixes.slice(1) },
      {
        ...good,
        acceptedTxIds: [good.acceptedTxIds[0]!, good.acceptedTxIds[0]!],
      },
      measurement([1, NaN, 3]),
    ])
      expect(
        planCommitDaFrameStepDown({ measurement: bad, maxInnerBytes: 100 }),
      ).toEqual({ status: "incomplete" });
  });
});

describe("commit DA frame step-down over rejected transactions", () => {
  it("steps down over the transactions the measured pass accepted, never a rejected one", async () => {
    const candidateSelection = selectCommitTxCandidates({
      mempoolTxs: Array.from({ length: 6 }, (_, index) =>
        mkCandidate(index + 1),
      ),
      processedMempoolTxs: [],
    });
    const ids = txIdsOf(candidateSelection.candidateTxs);
    const rejectedTxIds = [ids[1], ids[3]].map((id) => Buffer.from(id, "hex"));
    const result = await quiet(
      stepDownCommitSelectionToDaFrame({
        candidateSelection,
        // An empty ledger: its empty block is 1,010 bytes.
        baseUtxoPayloadAggregate: { entryCount: 0, encodedTupleBytes: 0 },
        maxInnerBytes: 100_000,
        process: (selection) => Effect.succeed(selection.candidateTxs),
        // Actual accepted order can differ from the submitted source rows.
        measure: (built) => {
          const accepted =
            built.length === 6
              ? [built[4]!, built[0]!, built[2]!, built[5]!]
              : built;
          const bytes = [1_000, 30_000, 40_000, 50_000, 100_001].slice(
            0,
            accepted.length + 1,
          );
          return Effect.succeed({
            innerBytesUpperBound: bytes.at(-1)!,
            acceptedTxCount: accepted.length,
            acceptedTxIds: accepted.map((entry) => entry[TxColumns.TX_ID]),
            rejectedTxIds: built.length === 6 ? rejectedTxIds : [],
            hasMandatoryWork: false,
            prefixes: bytes.map((innerBytesUpperBound, index) => ({
              innerBytesUpperBound,
              materialDigest: index.toString(16).padStart(64, "0"),
            })),
          });
        },
        rebase: Effect.void,
      }),
    );
    expect(result.passes).toBe(2);
    expect(result.outcome).toBe("fits");
    expect(txIdsOf(result.candidateSelection.candidateTxs)).toEqual([
      ids[4],
      ids[0],
      ids[2],
    ]);
  });
});

it.each(["digest", "bytes", "ids", "rejection", "unavailable"] as const)(
  "holds %s invalidation after at most one rebuild",
  async (change) => {
    const selection = selectCommitTxCandidates({
      mempoolTxs: Array.from({ length: 4 }, (_, index) =>
        mkCandidate(index + 1),
      ),
      processedMempoolTxs: [],
    });
    let pass = 0;
    const result = await quiet(
      stepDownCommitSelectionToDaFrame({
        candidateSelection: selection,
        baseUtxoPayloadAggregate: { entryCount: 0, encodedTupleBytes: 0 },
        maxInnerBytes: 100,
        process: (candidate) =>
          Effect.sync(() => {
            pass += 1;
            return candidate;
          }),
        rebase: Effect.void,
        measure: (candidate) => {
          if (pass === 2 && change === "unavailable")
            return Effect.succeed(undefined);
          const bytes = [1, 80, 90, 150, 200].slice(
            0,
            candidate.candidateTxs.length + 1,
          );
          const prefixes = bytes.map((innerBytesUpperBound, index) => ({
            innerBytesUpperBound,
            materialDigest: index.toString(16).padStart(64, "0"),
          }));
          if (pass === 2 && change === "digest")
            prefixes[prefixes.length - 1]!.materialDigest = "ff".repeat(32);
          if (pass === 2 && change === "bytes")
            prefixes[prefixes.length - 1]!.innerBytesUpperBound += 1;
          return Effect.succeed({
            innerBytesUpperBound:
              prefixes[prefixes.length - 1]!.innerBytesUpperBound,
            acceptedTxCount: candidate.candidateTxs.length,
            acceptedTxIds:
              pass === 2 && change === "ids"
                ? [...candidate.candidateTxHashes].reverse()
                : candidate.candidateTxHashes,
            rejectedTxIds:
              pass === 2 && change === "rejection" ? [Buffer.alloc(32)] : [],
            hasMandatoryWork: false,
            prefixes,
          });
        },
      }),
    );
    expect(result.outcome).toBe("incomplete");
    expect(result.passes).toBe(2);
    expect(pass).toBe(2);
  },
);
