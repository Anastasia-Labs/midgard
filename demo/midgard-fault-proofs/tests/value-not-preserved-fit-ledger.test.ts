import { createHash } from "node:crypto";
import { readFile } from "node:fs/promises";

import { describe, expect, it } from "vitest";

import {
  buildVanRossemFitLedger,
  type VanRossemFitLedger,
} from "../src/proof-fit/van-rossem-fit-ledger.js";
import { realBlueprintPath } from "./support/submit-init-emulator-shared.js";

describe("value conservation fit ledger", () => {
  it("binds the current blueprint and reproduces every measured margin", async () => {
    const saved: VanRossemFitLedger = JSON.parse(
      await readFile(
        new URL(
          "../../../docs/fault-proofs/size-plans/value-not-preserved-fit-ledger.json",
          import.meta.url,
        ),
        "utf8",
      ),
    );
    const ledger = buildVanRossemFitLedger({
      category: "valueNotPreserved:00000019:testnet",
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
    expect(ledger.entries.length).toBeGreaterThan(100);
    expect(
      new Set(ledger.entries.map((entry) => entry.maximumShape)).size,
    ).toBe(16);
  });
});
