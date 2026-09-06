import { createHash } from "node:crypto";
import { readFile } from "node:fs/promises";

import { describe, expect, it } from "vitest";

import {
  buildVanRossemFitLedger,
  type VanRossemFitLedger,
} from "../src/proof-fit/van-rossem-fit-ledger.js";
import { realBlueprintPath } from "./support/emulator/blueprints.js";

describe("crossBlockDuplicateEvent installed workflow fit ledger", () => {
  it("binds complete maximum evidence and publication margins to the current blueprint", async () => {
    const ledger = JSON.parse(
      await readFile(
        new URL(
          "../../../docs/fault-proofs/size-plans/cross-block-duplicate-event-workflow-fit-ledger.json",
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
    expect(ledger.category).toBe("crossBlockDuplicateEvent");
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
    for (const scenario of ["deposit", "withdrawal", "forced-transaction"]) {
      for (const stage of ["init", "step_01", "step_02", "remove"])
        expect(
          [...names].some((name) => name.startsWith(`${scenario}/${stage}/`)),
        ).toBe(true);
      for (const stage of ["init", "step-01", "step-02", "remove"])
        expect(
          [...names].some((name) =>
            name.startsWith(`maximum64/${scenario}/${stage}/`),
          ),
        ).toBe(true);
      expect(names.has(`${scenario}/publish/0`)).toBe(true);
      expect(names.has(`${scenario}/publish/1`)).toBe(true);
    }
    for (const stage of ["cancel-01", "cancel-02"])
      expect(
        [...names].some((name) =>
          name.startsWith(`maximum64/deposit/${stage}/`),
        ),
      ).toBe(true);
    expect(
      ledger.entries.some(
        (row) =>
          row.maximumShape.includes(
            "64 widest MPF branches; authenticated claimed count 1;",
          ) &&
          row.maximumShape.includes(
            "source bytes authenticated by value digest",
          ),
      ),
    ).toBe(true);
  });
});
