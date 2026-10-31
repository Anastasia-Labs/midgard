import { expect, it } from "vitest";

import { driveDirectDeclarationRegression } from "./support/direct-declaration-regression.js";

/** The unrelated key's declaration Scan is refused before the owner's: with
 * the correct entry ("unsigned-correct") the only missing condition is the
 * prover's signature, so this row fails if declaration Scan stops requiring
 * it. The unrelated key's Finish is refused the same way. Every accepted
 * transition is listed; the refused ones never land. */
it.each(["wrong-index", "unsigned-correct"] as const)(
  "refuses unrelated-key %s singleton declaration and Finish, accepts every owner step",
  async (attack) => {
    const measurements = await driveDirectDeclarationRegression(attack);
    expect(measurements.map(({ label }) => label)).toStrictEqual([
      "receive initialization from completed source receipt",
      "actual singleton grammar",
      "actual singleton credential H to zero",
      "authentic exhaustion handoff",
      "owner selects actual receive target",
      "actual singleton H to zero declaration",
      "owner finishes honest sorted declaration",
    ]);
  },
  300_000,
);
