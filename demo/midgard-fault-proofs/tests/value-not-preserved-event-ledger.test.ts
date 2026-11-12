/**
 * The ledger value-not-preserved replays a block's earlier events into must
 * be the event ledger every other family reads: an output whose native
 * script does not decode keeps the descriptor the ledger root commits for
 * it.
 */
import { makeOutput } from "@al-ft/midgard-validation/tests/validation-fixtures";
import { describe, expect, it } from "vitest";

import { VALUE_NOT_PRESERVED_COMPLETE_CANONICAL_REPLAY } from "../src/workflow/complete-replay.js";
import {
  block,
  SPENT,
  type Step,
} from "./helpers/event-ledger-committed-steps.js";

/** Spends `SPENT` (2 ADA) into 3 ADA: an accepted imbalance. */
const inflation: Step = {
  input: {
    spendInputs: [SPENT.outRef],
    outputs: [makeOutput(3_000_000n)],
    fee: 7n,
  },
  applied: true,
};

const findings = async (fixture: Awaited<ReturnType<typeof block>>) =>
  (
    await fixture.replay([VALUE_NOT_PRESERVED_COMPLETE_CANONICAL_REPLAY])
  ).detections.map(({ violationId, position }) => ({ violationId, position }));

describe("value-not-preserved's event ledger", () => {
  it("opens a predecessor ledger holding an output whose native script does not decode", async () => {
    const fixture = await block([inflation]);
    expect(await findings(fixture)).toEqual([
      { violationId: "value-not-preserved", position: fixture.position(0) },
    ]);
  });
});
