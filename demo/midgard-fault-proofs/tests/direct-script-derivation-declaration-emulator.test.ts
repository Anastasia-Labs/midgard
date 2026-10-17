import { expect, it } from "vitest";

import { driveDirectDeclarationRegression } from "./support/direct-declaration-regression.js";

it.each(["wrong-index", "unsigned-correct"] as const)(
  "refuses unrelated-key %s singleton declaration, accepts owner update and permissionless Finish",
  async (attack) => {
    expect(await driveDirectDeclarationRegression(attack)).toHaveLength(6);
  },
  300_000,
);
