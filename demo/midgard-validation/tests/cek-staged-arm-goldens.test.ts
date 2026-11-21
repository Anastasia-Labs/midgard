import { describe, expect, it } from "vitest";

import * as api from "../src/index.js";
import {
  buildCekStagedArmRow,
  CEK_STAGED_ARM_ROWS,
} from "./fixtures/cek-staged-arm-v1.vectors.mjs";

/**
 * The TypeScript half of the `cek-staged-arm-v1` golden channel: every row is
 * built from `src/` and judged by the TypeScript staged verifier, which must
 * reach the verdict the row declares. The Aiken half
 * (`cek-core-staged-golden.test.ak`) requires each Aiken-built step's witness
 * to hash to the TypeScript witness, spends the step through the staged bytes,
 * integer or product semantics validator and must reach the same verdict.
 * Regenerate it with `pnpm run fixtures:cek-staged-arm-v1:sync`.
 */

describe("cek-staged-arm-v1 golden rows", () => {
  it("covers correct, wrong-result and past-the-bound rows of every group", () => {
    const groups = new Set(
      CEK_STAGED_ARM_ROWS.map(
        (row) =>
          `${api.midgardCekSemanticGroup(BigInt(row.tag))}:${row.accepts}`,
      ),
    );
    expect([...groups].sort()).toEqual([
      "7:false",
      "7:true",
      "8:false",
      "8:true",
      "9:false",
      "9:true",
    ]);
  });

  it.each(CEK_STAGED_ARM_ROWS.map((row) => [row.name, row] as const))(
    "%s",
    (_name, row) => {
      expect(buildCekStagedArmRow(row, api).verdict).toBe(row.accepts);
    },
  );
});
