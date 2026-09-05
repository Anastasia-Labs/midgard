import { createHash } from "node:crypto";
import { readFile } from "node:fs/promises";

import { describe, expect, it } from "vitest";

import {
  buildVanRossemFitLedger,
  type VanRossemFitLedger,
} from "../src/proof-fit/van-rossem-fit-ledger.js";
import { realBlueprintPath } from "./support/emulator/blueprints.js";

describe("nativeScriptDecoding installed workflow fit ledger", () => {
  it("binds complete maximum evidence and publication margins to the current blueprint", async () => {
    const ledger = JSON.parse(
      await readFile(
        new URL(
          "../../../docs/fault-proofs/size-plans/native-script-decoding-workflow-fit-ledger.json",
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
    expect(ledger.category).toBe("nativeScriptDecoding");
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
      "maximum",
      "wrongful-acceptance",
      "wrongful-rejection",
    ]) {
      for (const stage of [
        "init",
        "step_01",
        "step_02",
        "step_03",
        "step_04",
        "step_05",
        "step_06",
        "remove",
      ])
        expect(
          [...names].some((name) => name.startsWith(`${scenario}/${stage}/`)),
          `${scenario}/${stage}`,
        ).toBe(true);
      for (let index = 0; index < 7; index++)
        expect(names.has(`${scenario}/publish/${index}`)).toBe(true);
    }
    expect(
      ledger.entries.some(
        (row) => row.maximumShape === "16384 raw script bytes",
      ),
    ).toBe(true);
  });
});
