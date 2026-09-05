import { createHash } from "node:crypto";
import { readFile } from "node:fs/promises";

import { describe, expect, it } from "vitest";

import {
  buildVanRossemFitLedger,
  type VanRossemFitLedger,
} from "../src/proof-fit/van-rossem-fit-ledger.js";
import { realBlueprintPath } from "./support/emulator/blueprints.js";

describe("minFee consolidated maximum fit ledger", () => {
  it("binds complete shape measurements and reproducible positive margins to the current blueprint", async () => {
    const stored = JSON.parse(
      await readFile(
        new URL(
          "../../../docs/fault-proofs/size-plans/min-fee-wrongful-rejection-v1-fit-ledger.json",
          import.meta.url,
        ),
        "utf8",
      ),
    ) as VanRossemFitLedger;
    const blueprint = await readFile(realBlueprintPath);
    expect(stored.blueprintSha256).toBe(
      createHash("sha256").update(blueprint).digest("hex"),
    );
    expect(stored.compilerVersion).toBe(
      JSON.parse(blueprint.toString()).preamble.compiler.version,
    );
    expect(stored.category).toBe("minFee");
    expect(
      buildVanRossemFitLedger({
        category: stored.category,
        blueprintSha256: stored.blueprintSha256,
        compilerVersion: stored.compilerVersion,
        measurements: stored.entries.map((entry) => ({
          ...entry,
          memoryUnits: BigInt(entry.memoryUnits),
          cpuUnits: BigInt(entry.cpuUnits),
        })),
      }),
    ).toEqual(stored);
    expect(stored.entries).toHaveLength(119);
    const shapes = new Set(stored.entries.map((entry) => entry.maximumShape));
    expect([...shapes].sort()).toEqual(
      [
        "field0-358",
        "field0-378",
        "field0-379",
        "field0-819",
        "all-nine-populated",
        "maximum-proof-64-and-certified-field",
      ].sort(),
    );
    for (const shape of shapes) {
      for (const stage of ["bind", "final", "removal"])
        expect(
          stored.entries.some((entry) => entry.name === `${shape}-${stage}-0`),
        ).toBe(true);
    }
  });
});
