import { maxDaPayloadInnerBytes } from "@al-ft/midgard-core/da-payload-sizing";
import { Either } from "effect";
import { describe, expect, it } from "vitest";

import type { UtxoPayloadSizeAggregate } from "../src/mpf/index.js";
import {
  planCommitBatchBudgets,
  selectCommitTxCandidates,
} from "../src/workers/utils/commit-block-planner.js";
import {
  LC1_BASE_LEDGER,
  LC1_MEAN_ENTRY_BYTES,
  mkCandidate,
  MODES,
  preSubmit,
  PROGRAM_LIMITS,
  REFUSAL,
} from "./helpers/commit-da-frame-fixtures.js";

describe("commit planner DA frame budget", () => {
  it.each(MODES)(
    "selects only what the pre-submit check admits, identically on consecutive ticks (%s)",
    async (mode) => {
      // A full mempool of plain transfers: 400 lc1-shaped blocks' worth of
      // DA content is more than one V1 frame holds.
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
          baseUtxoPayloadAggregate: LC1_BASE_LEDGER,
        } as Parameters<typeof planCommitBatchBudgets>[0]);
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
        stopReason: "da_payload_budget",
      });
      expect(tick?.selected).toBeGreaterThan(0);
      expect(tick?.selected).toBeLessThan(candidates.length);
      expect(tick?.innerBytes).toBeLessThanOrEqual(
        maxDaPayloadInnerBytes(mode),
      );
    },
  );

  it.each(MODES)(
    "still refuses at the same check when the base ledger alone cannot fit (%s)",
    async (mode) => {
      const limit = maxDaPayloadInnerBytes(mode);
      const base: UtxoPayloadSizeAggregate = {
        entryCount: Math.ceil(limit / LC1_MEAN_ENTRY_BYTES),
        encodedTupleBytes: limit,
      };
      const candidates = Array.from({ length: 3 }, (_, index) =>
        mkCandidate(index + 1),
      );
      const planned = planCommitBatchBudgets({
        candidateSelection: selectCommitTxCandidates({
          mempoolTxs: candidates,
          processedMempoolTxs: [],
        }),
        limits: PROGRAM_LIMITS(mode),
        baseUtxoPayloadAggregate: base,
      } as Parameters<typeof planCommitBatchBudgets>[0]);
      // The DA budget never trims a selection no block could carry, so the
      // refusal below stays the one that names the ledger.
      expect(planned.plan.selectedTxCount).toBe(candidates.length);
      for (const txs of [planned.candidateSelection.candidateTxs, []]) {
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
});
