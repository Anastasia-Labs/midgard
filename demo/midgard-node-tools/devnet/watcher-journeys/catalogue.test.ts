import { UNINSTALLED_FRAUD_PROOF_CATEGORIES } from "@al-ft/midgard-fault-proofs";
import { FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER } from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  JOURNEY_CATEGORIES,
  JOURNEY_FIXTURE_CANDIDATES,
  JOURNEY_FIXTURE_OWNERS,
  JOURNEY_FIXTURES,
  selectJourneyFixtures,
} from "./catalogue.js";

describe("automatic journey catalogue", () => {
  it("provides exactly one candidate adapter for every installed family", () => {
    expect(
      JOURNEY_FIXTURE_CANDIDATES.map(({ category }) => category).sort(),
    ).toEqual([...JOURNEY_CATEGORIES].sort());
  });
  it("assigns each of the 48 installed families to exactly one fixture owner", () => {
    const uninstalled: readonly string[] = UNINSTALLED_FRAUD_PROOF_CATEGORIES;
    const expected = FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.filter(
      (category) => !uninstalled.includes(category),
    );
    expect(expected).toHaveLength(48);
    expect(JOURNEY_CATEGORIES).toEqual(expected);
    expect(Object.keys(JOURNEY_FIXTURE_OWNERS).sort()).toEqual(
      [...expected].sort(),
    );
    expect(
      Object.values(JOURNEY_FIXTURE_OWNERS).filter(
        (owner) => owner === "transaction",
      ),
    ).toHaveLength(24);
    expect(
      Object.values(JOURNEY_FIXTURE_OWNERS).filter(
        (owner) => owner === "script",
      ),
    ).toHaveLength(12);
    expect(
      Object.values(JOURNEY_FIXTURE_OWNERS).filter(
        (owner) => owner === "history",
      ),
    ).toHaveLength(12);
  });

  it("selects the verified trace, direct, and event fixtures for the live gate", () => {
    expect(
      selectJourneyFixtures(
        "transitionTrace,observersForbiddenOnUntaggedNetwork,fabricatedDeposit,fabricatedWithdrawal,withdrawalMistag",
      ).map(({ category }) => category),
    ).toEqual([
      "transitionTrace",
      "observersForbiddenOnUntaggedNetwork",
      "fabricatedDeposit",
      "fabricatedWithdrawal",
      "withdrawalMistag",
    ]);
  });

  it("registers only distinct assigned installed fixtures", () => {
    const registered = JOURNEY_FIXTURES.map(({ category }) => category);
    expect(new Set(registered).size).toBe(registered.length);
    for (const category of registered)
      expect(JOURNEY_CATEGORIES).toContain(category);
  });

  it.each([
    "validationTraceDispute",
    "unusedScriptWitness",
    "unknown",
    "transitionTrace,",
    "zeroInput,zeroInput",
    "doubleWithdraw",
  ])(
    "rejects unsupported or ambiguous selection %s before live work",
    (selection) => {
      expect(() => selectJourneyFixtures(selection)).toThrow();
    },
  );
});
