/** The installed script-integrity-hash-mismatch replay next to the item
 * decoding and canonicality families. A leaf whose output, mint item or
 * script witness is not in the shape the purpose walk reads has an earlier
 * first fault: the mismatch member finds nothing in it, and the union still
 * returns the earlier family's finding for the block. */
import { describe, expect, it } from "vitest";

import {
  createCompleteCanonicalReplayUnion,
  MINT_ITEM_NON_CANONICAL_COMPLETE_CANONICAL_REPLAY,
  SCRIPT_INTEGRITY_HASH_MISMATCH_COMPLETE_CANONICAL_REPLAY,
  TRANSACTION_OUTPUT_NON_CANONICAL_COMPLETE_CANONICAL_REPLAY,
  WITNESS_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
} from "../src/workflow/complete-replay.js";
import { unreadableItemViolations } from "./support/unreadable-item-block.js";

const SIHM = SCRIPT_INTEGRITY_HASH_MISMATCH_COMPLETE_CANONICAL_REPLAY;
const OUTPUT = TRANSACTION_OUTPUT_NON_CANONICAL_COMPLETE_CANONICAL_REPLAY;
const MINT = MINT_ITEM_NON_CANONICAL_COMPLETE_CANONICAL_REPLAY;
const WITNESS = WITNESS_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY;

const output = { outputs: [Buffer.alloc(64)] };
const mint = { mintPolicyItems: [Buffer.from("00", "hex")] };
const witness = { scriptWitnesses: [Buffer.from("00", "hex")] };

describe("scriptIntegrityHashMismatch replay beside an item the purpose walk cannot read", () => {
  it("finds nothing in a leaf whose every item reads", async () => {
    await expect(
      unreadableItemViolations(SIHM, { kind: "normal" }, {}),
    ).resolves.toEqual([]);
  });

  // Union members in catalogue order. The output family convicts accepted
  // outputs only; beside a forced-valid leaf the union still completes.
  it.each([
    [
      "an accepted output",
      { kind: "normal" },
      output,
      [OUTPUT, SIHM],
      ["transaction-output-non-canonical"],
    ],
    [
      "an accepted mint item",
      { kind: "normal" },
      mint,
      [SIHM, MINT],
      ["mint-item-non-canonical"],
    ],
    [
      "an accepted script witness",
      { kind: "normal" },
      witness,
      [WITNESS, SIHM],
      ["witness-script-header-malformed"],
    ],
    [
      "a forced-valid output",
      { kind: "forced", verdict: "ForcedTxValid" },
      output,
      [OUTPUT, SIHM],
      [],
    ],
  ] as const)(
    "finds nothing beside %s it cannot read and the union completes",
    async (_label, source, items, members, union) => {
      await expect(
        unreadableItemViolations(SIHM, source, items),
      ).resolves.toEqual([]);
      await expect(
        unreadableItemViolations(
          createCompleteCanonicalReplayUnion([...members]),
          source,
          items,
        ),
      ).resolves.toEqual(union);
    },
  );
});
