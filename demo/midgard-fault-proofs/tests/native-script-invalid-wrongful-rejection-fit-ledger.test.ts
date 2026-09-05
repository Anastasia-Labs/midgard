import { createHash } from "node:crypto";
import { readFile } from "node:fs/promises";

import { describe, expect, it } from "vitest";

import {
  buildVanRossemFitLedger,
  type VanRossemFitLedger,
} from "../src/proof-fit/van-rossem-fit-ledger.js";
import { realBlueprintPath } from "./support/emulator/blueprints.js";

describe("native-script-invalid forced fit ledger", () => {
  it("records all bounded field, signer, depth and MPF paths through removal", async () => {
    const ledger = JSON.parse(
      await readFile(
        new URL(
          "../../../docs/fault-proofs/size-plans/native-script-invalid-wrongful-rejection-v1-fit-ledger.json",
          import.meta.url,
        ),
        "utf8",
      ),
    );
    const blueprint = await readFile(realBlueprintPath);
    expect(ledger.blueprintSha256).toBe(
      createHash("sha256").update(blueprint).digest("hex"),
    );
    expect(ledger.compilerVersion).toBe(
      JSON.parse(blueprint.toString("utf8")).preamble.compiler.version,
    );
    expect(ledger.category).toBe("nativeScriptInvalid");
    expect(
      buildVanRossemFitLedger({
        category: ledger.category,
        blueprintSha256: ledger.blueprintSha256,
        compilerVersion: ledger.compilerVersion,
        measurements: (ledger as VanRossemFitLedger).entries.map((entry) => ({
          ...entry,
          memoryUnits: BigInt(entry.memoryUnits),
          cpuUnits: BigInt(entry.cpuUnits),
        })),
      }),
    ).toEqual(ledger);
    expect(
      [
        ...new Set(
          (ledger as VanRossemFitLedger).entries.map(
            (entry) => entry.maximumShape,
          ),
        ),
      ].sort(),
    ).toEqual(
      [
        "direct-28-signers",
        "staged-29-signers",
        "last-raw-field",
        "first-certified-field",
        "maximum-fields-and-64-branch-source",
        "maximum-native-depth",
        "cancel-and-restart-grammar",
        "cancel-and-restart-signer",
      ].sort(),
    );
    const rows = (ledger as VanRossemFitLedger).entries;
    expect(rows).toHaveLength(372);
    for (const row of rows) {
      expect(row.signedBytes).toBeLessThanOrEqual(15872);
      expect(BigInt(row.memoryUnits)).toBeLessThanOrEqual(13200000n);
      expect(BigInt(row.cpuUnits)).toBeLessThanOrEqual(8000000000n);
    }
  });
});
