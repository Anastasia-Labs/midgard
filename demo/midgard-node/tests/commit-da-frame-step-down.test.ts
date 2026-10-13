import { encodeMidgardCekProgramMaterialSidecar } from "@al-ft/midgard-core/cek-proof";
import { maxDaPayloadInnerBytes } from "@al-ft/midgard-core/da-payload-sizing";
import { Effect, Either } from "effect";
import { describe, expect, it, vi } from "vitest";

import { Columns as TxColumns } from "../src/database/utils/tx.js";
import {
  type CommitDaFrameMeasurement,
  commitDaFrameStepDownPassBound,
  planCommitBatchBudgets,
  planCommitDaFrameStepDown,
  selectCommitTxCandidates,
  stepDownCommitSelectionToDaFrame,
} from "../src/workers/utils/commit-block-planner.js";
import { registerEventPrefixCases } from "./commit-da-frame-step-down.event-prefix-cases.js";
import {
  HEAVY_TRANSFER,
  quiet,
  stepDown,
  threeTransfers,
  txIdsOf,
} from "./commit-da-frame-step-down.fixture.js";
import {
  type BlockShape,
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

registerEventPrefixCases();
