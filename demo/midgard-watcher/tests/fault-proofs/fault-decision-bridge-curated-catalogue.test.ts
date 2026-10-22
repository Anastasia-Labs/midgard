import { SELECTED_DEPLOYMENT_PROFILE } from "@al-ft/midgard-core/deployment-profile";
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

  it("never targets an execution-dispute fault under the selected 900 s profile, which excludes it", () => {
    // The 900 s testing profiles cannot hold the execution dispute, so their
    // catalogue leaves 00000006 out; its fault is passed over, never funded.
    expect(SELECTED_DEPLOYMENT_PROFILE.excluded_fraud_proof_categories).toEqual(
      ["validationTraceDispute"],
    );
    const current = observation([headerFixture("01"), headerFixture("02")]);
    const [first, second] = current.finalizedHeaders;
    expect(
      selectedTarget({
        observation: current,
        decisions: [
          decision(first!.headerHash, "validationTraceDispute"),
          decision(second!.headerHash, "minFee"),
        ],
      }),
    ).toMatchObject({ category: "minFee", headerHash: second!.headerHash });
    expect(
      selectedTarget({
        observation: current,
        decisions: [decision(first!.headerHash, "validationTraceDispute")],
      }),
    ).toBeNull();
  });
});
