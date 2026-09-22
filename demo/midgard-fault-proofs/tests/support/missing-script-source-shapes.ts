/**
 * The source-frontier shapes the missingScriptSource suites pin.
 *
 * The family authenticates every source through the stage-9 Merkle frontier
 * and folds it in `MISSING_SCRIPT_SOURCE_SCAN_BUDGET` batches, so nothing in
 * the proof thread bounds the frontier: the largest provable universe is the
 * largest one a canonical transaction can commit. That is the consensus
 * profile's per-field preimage bound applied to the two source fields — the
 * inline script witnesses (field 6) and the reference inputs (field 1), every
 * one of which resolves to an output carrying a reference script. The counts
 * are re-derived here from the encoders so a consensus or codec change cannot
 * silently shrink the family's coverage; the suite asserts the pins agree.
 */
import {
  encodeMidgardFieldPreimage,
  encodeMidgardSpendInputItem,
  encodeMidgardVersionedScript,
  MIDGARD_CONSENSUS_LIMITS,
  type MidgardVersionedScript,
} from "@al-ft/midgard-core";

import { nativeScriptWitness } from "../../../midgard-validation/tests/validation-fixtures.js";
import { MISSING_SCRIPT_SOURCE_SCAN_BUDGET } from "../../src/missing-script-source/universe-scan.js";

/**
 * A decoy native script whose hash cannot collide with the required one and
 * that always evaluates true when it is an inline witness: `atLeast 0` over
 * one `before` guard at a distinct slot. Slots start above one byte so every
 * decoy item is the same width and the packed bound is exact.
 */
export const decoyMissingScriptSourceScript = (
  index: number,
): MidgardVersionedScript =>
  nativeScriptWitness({
    type: "atLeast",
    required: 0n,
    scripts: [{ type: "before", slot: 256n + BigInt(index) }],
  });

const packedCount = (
  limitBytes: number,
  itemAt: (index: number) => Uint8Array,
): number => {
  let low = 0;
  let high = 1;
  const fits = (count: number) =>
    encodeMidgardFieldPreimage(
      Array.from({ length: count }, (_value, index) => itemAt(index)),
    ).length <= limitBytes;
  while (fits(high)) high *= 2;
  while (high - low > 1) {
    const mid = Math.floor((low + high) / 2);
    if (fits(mid)) low = mid;
    else high = mid;
  }
  return low;
};

/** Largest inline script-witness count field 6 can commit under consensus. */
export const largestInlineSourceCountUnderConsensus = (): number =>
  packedCount(MIDGARD_CONSENSUS_LIMITS.maxScriptWitnessesPreimageBytes, (i) =>
    encodeMidgardVersionedScript(decoyMissingScriptSourceScript(i)),
  );

/** Largest reference-input count field 1 can commit under consensus. */
export const largestReferenceSourceCountUnderConsensus = (): number =>
  packedCount(MIDGARD_CONSENSUS_LIMITS.maxReferenceInputsPreimageBytes, (i) =>
    encodeMidgardSpendInputItem({
      txId: Buffer.alloc(32, 0x90),
      outputIndex: i,
    }),
  );

/** Pinned `largestInlineSourceCountUnderConsensus()` (13-byte items). */
export const MAXIMUM_INLINE_SOURCE_COUNT = 2520;

/** Pinned `largestReferenceSourceCountUnderConsensus()` (40-byte items). */
export const MAXIMUM_REFERENCE_SOURCE_COUNT = 819;

/**
 * The maximum supported source frontier: both source fields at their
 * consensus bound. The scan walks it in
 * `ceil(3339 / 24) = 140` batches; the deepest batches are all-reference
 * descriptors carrying the frontier's full sibling depth.
 */
export const MAXIMUM_SUPPORTED_SOURCE_COUNT =
  MAXIMUM_INLINE_SOURCE_COUNT + MAXIMUM_REFERENCE_SOURCE_COUNT;

/**
 * The smallest frontier that needs more than one scan batch, so a real
 * checkpoint exists to interrupt at and resume from: one full batch plus
 * one partial one, split across both source locations.
 */
export const RESUMABLE_INLINE_SOURCE_COUNT =
  MISSING_SCRIPT_SOURCE_SCAN_BUDGET + 2;
export const RESUMABLE_REFERENCE_SOURCE_COUNT = 4;
