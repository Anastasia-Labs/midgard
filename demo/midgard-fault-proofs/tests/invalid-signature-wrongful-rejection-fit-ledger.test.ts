import { createHash } from "node:crypto";
import { readFile } from "node:fs/promises";

import { describe, expect, it } from "vitest";

import {
  buildVanRossemFitLedger,
  type VanRossemFitLedger,
} from "../src/proof-fit/van-rossem-fit-ledger.js";
import { realBlueprintPath } from "./support/emulator/blueprints.js";

describe("invalidSignature consolidated maximum fit ledger", () => {
  it("binds complete shape measurements and reproducible positive margins to the current blueprint", async () => {
    const stored = JSON.parse(
      await readFile(
        new URL(
          "../../../docs/fault-proofs/size-plans/invalid-signature-wrongful-rejection-v1-fit-ledger.json",
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
    expect(stored.category).toBe("invalidSignature");
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
    expect(stored.entries).toHaveLength(68);
    for (const shape of [
      "0-selected-single",
      "139-selected-single",
      "317-selected-single",
      "0-1-single",
      "0--1-single",
      "317-selected-deep64",
    ]) {
      for (const stage of ["bind", "final", "removal"])
        expect(
          stored.entries.some(
            (entry) => entry.name === `forced-${shape}-${stage}`,
          ),
        ).toBe(true);
    }
    for (const name of [
      "reference-1",
      "reference-2",
      "accepted-init",
      "accepted-step-01",
      "accepted-step-02",
    ])
      expect(stored.entries.some((entry) => entry.name === name)).toBe(true);
  });
});
