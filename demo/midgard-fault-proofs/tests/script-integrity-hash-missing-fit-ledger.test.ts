import { createHash } from "node:crypto";
import { readFile } from "node:fs/promises";

import { describe, expect, it } from "vitest";

import {
  buildVanRossemFitLedger,
  type VanRossemFitLedger,
} from "../src/proof-fit/van-rossem-fit-ledger.js";
import { realBlueprintPath } from "./support/emulator/blueprints.js";

const ledgerPath = new URL(
  "../../../docs/fault-proofs/size-plans/script-integrity-hash-missing-v1-fit-ledger.json",
  import.meta.url,
);

/**
 * Every transaction the maximum lifecycle submits, by name. The stored ledger
 * is regenerated from that lifecycle's `[script-integrity-hash-missing-max-
 * fit-ledger]` line; this list pins that no row of the maximum shape has gone
 * missing from it.
 */
const MAXIMUM_SHAPE_ROWS = [
  "init",
  "step01",
  "step02",
  "step03-start-staged",
  ...Array.from(
    { length: 9 },
    (_, index) => `script-grammar-resume-${index.toString().padStart(2, "0")}`,
  ),
  "script-grammar-close-start-scan",
  ...Array.from(
    { length: 9 },
    (_, index) => `script-scan-${index.toString().padStart(2, "0")}`,
  ),
  "redeemer-grammar-start",
  ...Array.from(
    { length: 9 },
    (_, index) =>
      `redeemer-grammar-resume-${index.toString().padStart(2, "0")}`,
  ),
  "redeemer-grammar-finish",
  "step04-mint",
  "field-6-certificate",
  "field-8-certificate",
  "state-queue-removal",
  ...Array.from(
    { length: 7 },
    (_, index) => `reference-script-${(index + 1).toString().padStart(2, "0")}`,
  ),
  "field-certificate-reference-script",
  "field-6-chunks-00",
  "field-6-chunks-01",
  "field-8-chunks-00",
  "field-8-chunks-01",
] as const;

describe("scriptIntegrityHashMissing signed Van Rossem fit ledger", () => {
  it("is bound to the fresh locked build and reproduces every maximum row with positive margins", async () => {
    const stored = JSON.parse(
      await readFile(ledgerPath, "utf8"),
    ) as VanRossemFitLedger;
    const blueprintBytes = await readFile(realBlueprintPath);
    const blueprint = JSON.parse(blueprintBytes.toString("utf8")) as {
      readonly preamble: { readonly compiler: { readonly version: string } };
    };
    expect(stored.blueprintSha256).toBe(
      createHash("sha256").update(blueprintBytes).digest("hex"),
    );
    expect(stored.compilerVersion).toBe(blueprint.preamble.compiler.version);
    expect(stored.category).toBe("scriptIntegrityHashMissing");
    // The margins and the ledger digest are derived, never hand-edited: the
    // writer must reproduce the stored file from its raw measurements alone.
    const rebuilt = buildVanRossemFitLedger({
      category: stored.category,
      blueprintSha256: stored.blueprintSha256,
      compilerVersion: stored.compilerVersion,
      measurements: stored.entries.map((entry) => ({
        name: entry.name,
        kind: entry.kind,
        maximumShape: entry.maximumShape,
        signedBytes: entry.signedBytes,
        memoryUnits: BigInt(entry.memoryUnits),
        cpuUnits: BigInt(entry.cpuUnits),
      })),
    });
    expect(rebuilt).toStrictEqual(stored);
    const names = new Set(stored.entries.map((entry) => entry.name));
    for (const row of MAXIMUM_SHAPE_ROWS)
      expect(names.has(row), row).toBe(true);
    for (const entry of stored.entries) {
      expect(entry.signedByteMargin, entry.name).toBeGreaterThan(0);
      expect(BigInt(entry.memoryUnitMargin), entry.name).toBeGreaterThan(0n);
      expect(BigInt(entry.cpuUnitMargin), entry.name).toBeGreaterThan(0n);
      if (entry.kind === "publication") {
        expect(entry.publicationReserveMargin, entry.name).not.toBeNull();
        expect(
          entry.publicationReserveMargin!,
          entry.name,
        ).toBeGreaterThanOrEqual(0);
      }
    }
  });
});
