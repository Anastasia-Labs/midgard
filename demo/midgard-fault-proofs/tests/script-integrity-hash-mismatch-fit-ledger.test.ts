import { createHash } from "node:crypto";
import { readFile } from "node:fs/promises";

import { describe, expect, it } from "vitest";

import {
  buildVanRossemFitLedger,
  type VanRossemFitLedger,
} from "../src/proof-fit/van-rossem-fit-ledger.js";
import { realBlueprintPath } from "./support/emulator/blueprints.js";

describe("scriptIntegrityHashMismatch measured lifecycle ledger", () => {
  it("binds every language and polarity, publications and maximum authentication to the compiled tree", async () => {
    const stored = JSON.parse(
      await readFile(
        new URL(
          "../../../docs/fault-proofs/size-plans/script-integrity-hash-mismatch-v1-fit-ledger.json",
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
    expect(stored.category).toBe("scriptIntegrityHashMismatch");
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
    const names = new Set(stored.entries.map((entry) => entry.name));
    for (const direction of ["accepted", "forced"]) {
      for (const bitmap of [0, 1, 2, 3]) {
        for (const honest of [false, true]) {
          const prefix = `${direction}-bitmap-${bitmap}-honest-${honest}`;
          const rows = [
            "init",
            "step01",
            "step02",
            "step03",
            "step04-0",
            "step04-1",
            ...Array.from({ length: 5 }, (_, index) => `reference-${index}`),
            ...(honest
              ? ["cancel-honest-terminal"]
              : [
                  "step05",
                  "removal",
                  ...Array.from(
                    { length: 5 },
                    (_, index) => `cancel-step0${index + 1}`,
                  ),
                ]),
          ];
          for (const row of rows)
            expect(names.has(`${prefix}-${row}`), `${prefix}-${row}`).toBe(
              true,
            );
        }
      }
      const maximum = stored.entries.find(
        (entry) => entry.name === `${direction}-bitmap-3-honest-false-step02`,
      )!;
      expect(maximum.maximumShape).toContain(
        "64-branch descriptor MPF and 32-level validation trace",
      );
    }
    expect(
      names.has("accepted-bitmap-3-honest-false-source-proof-chunks"),
    ).toBe(true);
    expect(stored.entries).toHaveLength(241);
  });
});
