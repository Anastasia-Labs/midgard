import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";

import { describe, expect, it } from "vitest";

import * as api from "../src/index.js";
import {
  buildCekStagedArmRow,
  CEK_STAGED_ARM_ROWS,
} from "./fixtures/cek-staged-arm-v1.vectors.mjs";

/**
 * The TypeScript half of the `cek-staged-arm-v1` golden channel: every row is
 * rebuilt from `src/` and judged by the TypeScript staged verifier, which
 * must reach the verdict the row declares, and its witness must hash to the
 * value the generated Aiken module requires of the Aiken-built step. The
 * Aiken half (`cek-core-staged-golden.test.ak`) spends each step through the
 * staged bytes, integer or product semantics validator and must reach the
 * same verdict. Regenerate both with `pnpm run fixtures:cek-staged-arm-v1:sync`.
 */

type GoldenRow = {
  readonly name: string;
  readonly tag: number;
  readonly accepts: boolean;
  readonly witnessHash: string;
};

const golden = JSON.parse(
  readFileSync(
    fileURLToPath(
      new URL("./fixtures/cek-staged-arm-v1.generated.json", import.meta.url),
    ),
    "utf8",
  ),
) as { readonly rows: readonly GoldenRow[] };

describe("cek-staged-arm-v1 golden rows", () => {
  it("covers every row, in order", () => {
    expect(golden.rows.map((row) => row.name)).toEqual(
      CEK_STAGED_ARM_ROWS.map((row) => row.name),
    );
  });

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

  it.each(
    CEK_STAGED_ARM_ROWS.map((row, index) => [row.name, row, index] as const),
  )("%s", (_name, row, index) => {
    const built = buildCekStagedArmRow(row, api);
    expect(built.verdict).toBe(row.accepts);
    expect(built.witnessHash).toBe(golden.rows[index]!.witnessHash);
    expect(golden.rows[index]!.accepts).toBe(row.accepts);
  });
});
