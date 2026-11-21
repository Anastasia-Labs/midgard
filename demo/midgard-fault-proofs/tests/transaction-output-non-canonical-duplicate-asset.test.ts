/**
 * transactionOutputNonCanonical over an output value that repeats a policy id
 * or repeats an asset name within one policy. Policy ids and the names under
 * each policy must be distinct and ascending; the scanner holds both with a
 * strict byte-order comparison, so a repeat is a non-canonical output.
 *
 * Accepted: a block that accepts either repeat is convicted from Init and
 * removed, and the distinct ascending twin in the same block is refused at
 * step 04. Forced: the L1 admission screen does not read output values, so a
 * forced transaction carrying either repeat is admissible. The node rejects
 * it with `OutputNonCanonical`, which step 04 refuses to contradict; a forced
 * leaf that accepts it instead is convicted and removed.
 */
import { describe, expect, it } from "vitest";

import {
  distinctAscendingOutput,
  duplicateAssetNameOutput,
  duplicatePolicyOutput,
} from "./support/duplicate-asset-output.js";
import {
  forcedOutputTransaction,
  runAcceptedBlock,
  runForcedLeaf,
} from "./transaction-output-non-canonical-duplicate-asset.scenario.js";

describe("transactionOutputNonCanonical over a value that repeats a key", () => {
  it("convicts an accepted output repeating a policy id and one repeating an asset name, refuses the distinct ascending twin, then removes the block", async () => {
    await runAcceptedBlock([
      {
        label: "repeated policy id",
        output: duplicatePolicyOutput(),
        repeated: true,
      },
      {
        label: "repeated asset name",
        output: duplicateAssetNameOutput(),
        repeated: true,
      },
      {
        label: "distinct ascending",
        output: distinctAscendingOutput(),
        repeated: false,
      },
    ]);
  }, 900_000);

  for (const [label, output] of [
    ["a repeated policy id", duplicatePolicyOutput(4_000_000n)],
    ["a repeated asset name", duplicateAssetNameOutput(4_000_000n)],
  ] as const) {
    it(`admits a forced transaction with ${label}; the node's rejection is refused at step 04 and a leaf accepting it is convicted and removed`, async () => {
      const forced = forcedOutputTransaction(output, 4_000_000n);
      expect(forced.admission).toBeNull();
      expect(await forced.verdict()).toStrictEqual({
        ForcedTxInvalid: {
          reason: { OutputNonCanonical: { output_index: 0n } },
        },
      });
      await runForcedLeaf({ transaction: forced.transaction, accepted: false });
      await runForcedLeaf({ transaction: forced.transaction, accepted: true });
    }, 900_000);
  }
});
