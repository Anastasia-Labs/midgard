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
  it("provides exactly one candidate adapter for every non-interactive family", () => {
    expect(
      JOURNEY_FIXTURE_CANDIDATES.map(({ category }) => category).sort(),
    ).toEqual([...JOURNEY_CATEGORIES].sort());
  });
  it("assigns each of the 54 non-interactive families to exactly one fixture owner", () => {
    const expected = FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.filter(
      (category) => category !== "validationTraceDispute",
    );
    expect(expected).toHaveLength(54);
    expect(JOURNEY_CATEGORIES).toEqual(expected);
    expect(Object.keys(JOURNEY_FIXTURE_OWNERS).sort()).toEqual(
      [...expected].sort(),
    );
    expect(
      Object.values(JOURNEY_FIXTURE_OWNERS).filter(
        (owner) => owner === "transaction",
      ),
    ).toHaveLength(25);
    expect(
      Object.values(JOURNEY_FIXTURE_OWNERS).filter(
        (owner) => owner === "script",
      ),
    ).toHaveLength(16);
    expect(
      Object.values(JOURNEY_FIXTURE_OWNERS).filter(
        (owner) => owner === "history",
      ),
    ).toHaveLength(13);
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

  it("registers only distinct assigned non-interactive fixtures", () => {
    const registered = JOURNEY_FIXTURES.map(({ category }) => category);
    expect(new Set(registered).size).toBe(registered.length);
    for (const category of registered)
      expect(JOURNEY_CATEGORIES).toContain(category);
  });

  it.each([
    "validationTraceDispute",
    "unknown",
    "transitionTrace,",
    "zeroInput,zeroInput",
    "doubleWithdraw",
    "crossBlockDuplicateEvent",
  ])(
    "rejects unsupported or ambiguous selection %s before live work",
    (selection) => {
      expect(() => selectJourneyFixtures(selection)).toThrow();
    },
  );
});
