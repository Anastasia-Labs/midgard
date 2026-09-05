import { createHash } from "node:crypto";
import { readFile } from "node:fs/promises";

import { describe, expect, it } from "vitest";

import {
  buildVanRossemFitLedger,
  type VanRossemFitLedger,
} from "../src/proof-fit/van-rossem-fit-ledger.js";
import { realBlueprintPath } from "./support/submit-init-emulator-shared.js";

describe("missingSignature wrongful rejection fit ledger", () => {
  it("binds the current blueprint and reproduces every measured margin", async () => {
    const saved: VanRossemFitLedger = JSON.parse(
      await readFile(
        new URL(
          "../../../docs/fault-proofs/size-plans/missing-signature-wrongful-rejection-v1-fit-ledger.json",
          import.meta.url,
        ),
        "utf8",
      ),
    );
    const ledger = buildVanRossemFitLedger({
      category: "missingSignature:0000000e:wrongful-rejection:testnet",
      blueprintSha256: createHash("sha256")
        .update(await readFile(realBlueprintPath))
        .digest("hex"),
      compilerVersion: "aiken v1.1.23+5adf783",
      measurements: saved.entries.map((entry) => ({
        ...entry,
        memoryUnits: BigInt(entry.memoryUnits),
        cpuUnits: BigInt(entry.cpuUnits),
      })),
    });
    expect(ledger).toEqual(saved);
    expect(ledger.entries).toHaveLength(144);
    expect(
      new Set(ledger.entries.map((entry) => entry.maximumShape)).size,
    ).toBe(11);
    for (const entry of ledger.entries) {
      expect(BigInt(entry.memoryUnits)).toBeLessThanOrEqual(13_200_000n);
      expect(BigInt(entry.cpuUnits)).toBeLessThanOrEqual(8_000_000_000n);
      expect(entry.signedByteMargin).toBeGreaterThan(0);
    }
  });
});
