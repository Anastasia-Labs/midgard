import { describe, expect, it } from "vitest";

import { selectedTarget } from "../../src/fault-proofs/fault-decision-bridge.selected-target.js";
import {
  decision,
  headerFixture,
  observation,
} from "./fault-decision-bridge.observation.js";

describe("selected target under a curated catalogue", () => {
  it("skips a fault whose category the deployment's catalogue excludes", () => {
    const current = observation([headerFixture("01"), headerFixture("02")]);
    const [first, second] = current.finalizedHeaders;
    const decisions = [
      decision(first!.headerHash, "minFee"),
      decision(second!.headerHash, "doubleSpend"),
    ];
    // Both installed families are in the selected testing catalogue.
    expect(selectedTarget({ observation: current, decisions })).toMatchObject({
      category: "minFee",
      headerHash: first!.headerHash,
    });
    expect(
      selectedTarget({
        observation: current,
        decisions,
        catalogueCategories: ["doubleSpend"],
      }),
    ).toMatchObject({
      category: "doubleSpend",
      headerHash: second!.headerHash,
    });
    expect(
      selectedTarget({
        observation: current,
        decisions: decisions.slice(0, 1),
        catalogueCategories: ["doubleSpend"],
      }),
    ).toBeNull();
  });
});
