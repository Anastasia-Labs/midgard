import { createHash } from "node:crypto";
import { readFileSync } from "node:fs";

import { describe, expect, it } from "vitest";

import {
  buildVanRossemFitLedger,
  type VanRossemFitLedger,
} from "../src/proof-fit/van-rossem-fit-ledger.js";
import { realBlueprintPath } from "./support/emulator/blueprints.js";

describe("ValueAndMint shared-yield fit ledger", () => {
  it("pins every branch and maximum witness to the current compiled contracts", () => {
    const ledger: VanRossemFitLedger = JSON.parse(
      readFileSync(
        new URL(
          "../../../docs/fault-proofs/size-plans/validation-trace-value-and-mint-fit-ledger.json",
          import.meta.url,
        ),
        "utf8",
      ),
    );
    const blueprint = readFileSync(realBlueprintPath);
    expect(ledger.blueprintSha256).toBe(
      createHash("sha256").update(blueprint).digest("hex"),
    );
    expect(ledger.compilerVersion).toBe(
      JSON.parse(blueprint.toString()).preamble.compiler.version,
    );
    expect(ledger.category).toBe("validationTraceDispute/ValueAndMint");
    expect(
      buildVanRossemFitLedger({
        category: ledger.category,
        blueprintSha256: ledger.blueprintSha256,
        compilerVersion: ledger.compilerVersion,
        measurements: ledger.entries.map((e) => ({
          ...e,
          memoryUnits: BigInt(e.memoryUnits),
          cpuUnits: BigInt(e.cpuUnits),
        })),
      }),
    ).toEqual(ledger);
    expect(
      [...new Set(ledger.entries.map((e) => e.maximumShape))].sort(),
    ).toEqual(
      ["replayAsset", "outputAsset", "mintAsset"]
        .flatMap((kind) =>
          ["1", "1304", "maximum-proof"].map((shape) => `${kind}/${shape}`),
        )
        .sort(),
    );
    for (const row of ledger.entries) {
      expect(row.signedBytes, row.name).toBeLessThanOrEqual(15872);
      expect(BigInt(row.memoryUnits), row.name).toBeLessThanOrEqual(13200000n);
      expect(BigInt(row.cpuUnits), row.name).toBeLessThanOrEqual(8000000000n);
    }
    for (const shape of new Set(ledger.entries.map((e) => e.maximumShape))) {
      expect(
        ledger.entries.some(
          (e) => e.maximumShape === shape && e.kind === "publication",
        ),
      ).toBe(true);
      expect(
        ledger.entries.some(
          (e) =>
            e.maximumShape === shape &&
            e.kind === "lifecycle" &&
            BigInt(e.memoryUnits) > 0n,
        ),
      ).toBe(true);
    }
  });
});
