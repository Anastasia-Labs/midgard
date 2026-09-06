import { createHash } from "node:crypto";
import { readFile } from "node:fs/promises";

import { describe, expect, it } from "vitest";

import {
  buildVanRossemFitLedger,
  type VanRossemFitLedger,
} from "../src/proof-fit/van-rossem-fit-ledger.js";
import { realBlueprintPath } from "./support/emulator/blueprints.js";

describe("withdrawalMistag installed workflow fit ledger", () => {
  it("binds complete maximum evidence and publication margins to the current blueprint", async () => {
    const ledger = JSON.parse(
      await readFile(
        new URL(
          "../../../docs/fault-proofs/size-plans/withdrawal-mistag-workflow-fit-ledger.json",
          import.meta.url,
        ),
        "utf8",
      ),
    ) as VanRossemFitLedger;
    const blueprint = await readFile(realBlueprintPath);
    expect(ledger.blueprintSha256).toBe(
      createHash("sha256").update(blueprint).digest("hex"),
    );
    expect(ledger.compilerVersion).toBe("aiken v1.1.23+5adf783");
    expect(ledger.category).toBe("withdrawalMistag");
    expect(
      buildVanRossemFitLedger({
        category: ledger.category,
        blueprintSha256: ledger.blueprintSha256,
        compilerVersion: ledger.compilerVersion,
        measurements: ledger.entries.map((row) => ({
          name: row.name,
          kind: row.kind,
          maximumShape: row.maximumShape,
          signedBytes: row.signedBytes,
          memoryUnits: BigInt(row.memoryUnits),
          cpuUnits: BigInt(row.cpuUnits),
        })),
      }),
    ).toEqual(ledger);
    const names = new Set(ledger.entries.map((row) => row.name));
    for (const scenario of [
      "valid-marked-invalid",
      "invalid-marked-valid",
      "maximum-payout",
      "maximum-value",
      "asset-boundary",
      "maximum-output",
    ]) {
      for (const stage of [
        "init",
        "step_01",
        "step_02",
        "step_03",
        "step_04",
        "step_05",
        "remove",
      ])
        expect(
          [...names].some((name) => name.startsWith(`${scenario}/${stage}/`)),
        ).toBe(true);
      expect(names.has(`${scenario}/publish/0`)).toBe(true);
      expect(names.has(`${scenario}/publish-removal/0`)).toBe(true);
    }
    for (const scenario of ["maximum-proof", "maximum-asset-names"]) {
      for (const stage of [
        "init",
        "step-01",
        "step-02",
        "step-03",
        "step-04",
        "step-05",
      ])
        expect(names.has(`${scenario}/${stage}`)).toBe(true);
      expect(
        [...names].some((name) => name.startsWith(`${scenario}/remove-`)),
      ).toBe(true);
      expect(
        [...names].some((name) =>
          name.startsWith(`${scenario}/removal-script-`),
        ),
      ).toBe(true);
    }
    for (const row of ledger.entries) {
      expect(row.signedBytes).toBeLessThanOrEqual(15_872);
      expect(BigInt(row.memoryUnits)).toBeLessThanOrEqual(13_200_000n);
      expect(BigInt(row.cpuUnits)).toBeLessThanOrEqual(8_000_000_000n);
      if (
        row.name.includes("publish") ||
        row.name.includes("evidence-") ||
        row.name.includes("script-")
      )
        expect(row.kind).toBe("publication");
    }
  });
});
