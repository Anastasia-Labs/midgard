import { expect, it } from "vitest";

import { driveDirectDeclarationRegression } from "./support/direct-declaration-regression.js";

/** The unrelated key's declaration Scan is refused before the owner's: with
 * the correct entry ("unsigned-correct") the only missing condition is the
 * prover's signature, so this row fails if declaration Scan stops requiring
 * it. Every accepted transition is listed; the refused one never lands. */
it.each(["wrong-index", "unsigned-correct"] as const)(
  "refuses unrelated-key %s singleton declaration, accepts owner update and permissionless Finish",
  async (attack) => {
    const measurements = await driveDirectDeclarationRegression(attack);
    expect(measurements.map(({ label }) => label)).toStrictEqual([
      "permissionless receive initialization from completed source receipt",
      "permissionless actual singleton grammar",
      "permissionless actual singleton credential H to zero",
      "permissionless authentic exhaustion handoff",
      "owner selects actual receive target",
      "actual singleton H to zero declaration",
      "unrelated key finishes honest sorted declaration",
    ]);
  },
  300_000,
);
