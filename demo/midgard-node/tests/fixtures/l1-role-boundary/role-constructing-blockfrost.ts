/**
 * A role entry that builds an external provider client itself: the boundary
 * check must flag it (`tests/l1-role-boundary.test.ts`).
 */
import * as LE from "@lucid-evolution/lucid";

export const roleProvider = () =>
  new LE.Blockfrost("https://cardano-preprod.blockfrost.io/api/v0", "x");
