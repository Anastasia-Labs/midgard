import { describe, expect, it } from "vitest";

import { selectJourneyFixtures } from "./catalogue.js";

describe("automatic journey catalogue", () => {
  it("selects the verified trace, direct, dispute, and event fixtures for the live gate", () => {
    expect(
      selectJourneyFixtures(
        "transitionTrace,observersForbiddenOnUntaggedNetwork,validationTraceDispute,fabricatedDeposit,fabricatedWithdrawal,withdrawalMistag,doubleWithdraw",
      ).map(({ category }) => category),
    ).toEqual([
      "transitionTrace",
      "observersForbiddenOnUntaggedNetwork",
      "validationTraceDispute",
      "fabricatedDeposit",
      "fabricatedWithdrawal",
      "withdrawalMistag",
      "doubleWithdraw",
    ]);
  });

  it.each(["unknown", "transitionTrace,", "zeroInput,zeroInput"])(
    "rejects unsupported or ambiguous selection %s before live work",
    (selection) => {
      expect(() => selectJourneyFixtures(selection)).toThrow();
    },
  );
});
