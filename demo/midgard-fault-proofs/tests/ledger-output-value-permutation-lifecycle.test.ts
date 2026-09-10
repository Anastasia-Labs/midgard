import { describe, expect, it } from "vitest";

import {
  buildForgedOperatorSuccessorValidationDisputeFixture,
  runForcedValidationDisputeScenario,
} from "./support/submit-init-emulator-shared.js";

// Authenticated mixed-width asset ordering across descriptor and context
// (docs/fault-proofs/decisions/0003-publishable-semantic-resolvers.md):
// the descriptor asset frontier commits assets in canonical
// (length-then-bytes) key order while the evaluated script context orders
// asset names lexicographically, and `ledger_output_value_v1.asset_step`
// proves that permutation - each step authenticates its membership index
// into the original frontier and, on a same-policy step, a previous
// map-head opening whose reconstruction must equal the accumulated
// within-policy commitment before a strictly smaller name is admitted.
//
// The 1,304-asset shape is the exact mixed-width maximum: names 0, 1 and 2
// bytes wide under one policy, so the two orders are genuinely different
// permutations. The positive journey disputes one ledger-output value step
// that carries a previous-head opening and wins it end to end on the
// emulator; the negatives exchange only that step's own permutation witness
// in the challenger's otherwise honest trace, so every local builder gate
// passes and the refusal is the on-chain membership / head-opening clause
// the selector tests pin (ledger-output-value-v1.test.ak's
// duplicate/out-of-range membership index, omitted/forged head opening and
// non-strictly-smaller head refusals).
describe("mixed-width ledger output value permutation lifecycle", () => {
  it("proves a value step with a previous-head opening at the 1,304-asset output maximum", async () => {
    const result = await runForcedValidationDisputeScenario(
      ({ operatorVkey, now }) =>
        buildForgedOperatorSuccessorValidationDisputeFixture({
          operatorVkey,
          now,
          disputedPhase: "scriptSources",
          scriptSourcesSemanticIndex: 2,
          assetCount: 1304,
          ledgerOutputValueOpening: true,
        }),
    );
    expect(result.awardResult?.txHash).toHaveLength(64);
    expect(result.removal?.transactions.length).toBeGreaterThan(0);
    expect(result.semanticMeasurement!.executionMemory).toBeLessThanOrEqual(
      13_200_000n,
    );
    expect(result.semanticMeasurement!.executionSteps).toBeLessThanOrEqual(
      8_000_000_000n,
    );
    if (process.env.MIDGARD_PRINT_PROOF_FIT === "1") {
      console.info(
        JSON.stringify(
          {
            ledgerOutputNecessityMeasurement: {
              label: "mixed-width 1,304-asset value permutation",
              ...result.semanticMeasurement,
            },
          },
          (_key, value: unknown) =>
            typeof value === "bigint" ? value.toString() : value,
        ),
      );
    }
  }, 900_000);

  it.each(["foreignIndex", "forgedHead", "omittedHead"] as const)(
    "refuses a %s mutation of the value step's permutation witness",
    async (permutationWitnessMutation) => {
      await expect(
        runForcedValidationDisputeScenario(({ operatorVkey, now }) =>
          buildForgedOperatorSuccessorValidationDisputeFixture({
            operatorVkey,
            now,
            disputedPhase: "scriptSources",
            scriptSourcesSemanticIndex: 2,
            assetCount: 1304,
            ledgerOutputValueOpening: true,
            permutationWitnessMutation,
          }),
        ),
      ).rejects.toThrow(/failed script execution/);
    },
    900_000,
  );
});
