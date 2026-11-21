import { describe, expect, it } from "vitest";

import { buildProvedDoubleSpendFixture } from "./support/submit-init/double-spend.js";
import { retireFixtureOperatorAfterInactivity } from "./support/submit-init/operator-retirement.js";
import {
  expectRemovedFraudProofState,
  submitRemovalForFixture,
} from "./support/submit-init/removal.js";

describe("fault-proof partial-slash emulator integration", () => {
  it("removes a tail block from the exact partially inactivity-slashed retired tranche", async () => {
    const fixture = await buildProvedDoubleSpendFixture();
    await retireFixtureOperatorAfterInactivity(fixture);

    const removeResult = await submitRemovalForFixture(fixture);

    expect(removeResult.transactions.map((tx) => tx.kind)).toEqual([
      "remove-target",
    ]);
    expect(removeResult.transactions.map((tx) => tx.slashingApproach)).toEqual([
      "SlashRetiredOperator",
    ]);
    await expectRemovedFraudProofState(fixture);
  }, 300_000);
});
