import { maxDaPayloadInnerBytes } from "@al-ft/midgard-core/da-payload-sizing";
import { Either } from "effect";
import { describe, expect, it } from "vitest";

import {
  EMPTY_BLOCK_DA_PAYLOAD_UPPER_BOUND_BYTES,
  estimatedTxDaPayloadBytes,
  planCommitBatchBudgets,
  selectCommitTxCandidates,
} from "../src/workers/utils/commit-block-planner.js";
import {
  CANONICAL_TX,
  mkCandidate,
  MODES,
  preSubmit,
  PROGRAM_LIMITS,
} from "./helpers/commit-da-frame-fixtures.js";

describe("commit planner DA frame budget", () => {
  it.each(MODES)(
    "fits a full mempool of plain transfers in one frame, identically on consecutive ticks (%s)",
    async (mode) => {
      // A plain transfer carries its two copies and a placeholder
      // descriptor: a few hundred transfers are far from a frame.
      const candidates = Array.from({ length: 400 }, (_, index) =>
        mkCandidate(index + 1),
      );
      const ticks = [];
      for (let tick = 0; tick < 2; tick += 1) {
        const planned = planCommitBatchBudgets({
          candidateSelection: selectCommitTxCandidates({
            mempoolTxs: candidates,
            processedMempoolTxs: [],
          }),
          limits: PROGRAM_LIMITS(mode),
        });
        const result = await preSubmit(
          planned.candidateSelection.candidateTxs,
          mode,
        );
        ticks.push({
          selected: planned.plan.selectedTxCount,
          stopReason: planned.plan.stopReason,
          refusal: Either.isLeft(result) ? result.left.message : undefined,
          innerBytes: Either.getOrUndefined(result),
        });
      }
      // Nothing changes between the ticks, so the plan must not either.
      expect(ticks[1]).toEqual(ticks[0]);
      const [tick] = ticks;
      expect(tick).toMatchObject({
        refusal: undefined,
        selected: candidates.length,
        stopReason: "mempool_exhausted",
      });
      expect(tick?.innerBytes).toBeLessThanOrEqual(
        maxDaPayloadInnerBytes(mode) / 100,
      );
    },
  );

  it.each(MODES)(
    "budgets the empty block before any transaction (%s)",
    (mode) => {
      // A frame that leaves room for exactly forty transactions' allowance
      // past the empty block.
      const programLimits = PROGRAM_LIMITS(mode);
      const perTx = estimatedTxDaPayloadBytes(
        CANONICAL_TX.length,
        programLimits,
      );
      const limits = {
        ...programLimits,
        maxDaPayloadBytes: Math.floor(
          EMPTY_BLOCK_DA_PAYLOAD_UPPER_BOUND_BYTES + 40.5 * perTx,
        ),
      };
      const planned = planCommitBatchBudgets({
        candidateSelection: selectCommitTxCandidates({
          mempoolTxs: Array.from({ length: 100 }, (_, index) =>
            mkCandidate(index + 1),
          ),
          processedMempoolTxs: [],
        }),
        limits,
      });
      expect(planned.plan).toMatchObject({
        selectedTxCount: 40,
        stopReason: "da_payload_budget",
        estimatedDaPayloadBytes:
          EMPTY_BLOCK_DA_PAYLOAD_UPPER_BOUND_BYTES + 40 * perTx,
      });
      expect(planned.prunedTxCount).toBe(60);
    },
  );
});
