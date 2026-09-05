import { createHash } from "node:crypto";
import { readFile } from "node:fs/promises";

import { describe, expect, it } from "vitest";

import {
  buildVanRossemFitLedger,
  type VanRossemFitLedger,
} from "../src/proof-fit/van-rossem-fit-ledger.js";
import { realBlueprintPath } from "./support/emulator/blueprints.js";

describe("noReferenceInput measured wrongful-rejection fit ledger", () => {
  it("binds complete maximum evidence and publication margins to the current blueprint", async () => {
    const ledger = JSON.parse(
      await readFile(
        new URL(
          "../../../docs/fault-proofs/size-plans/no-reference-input-wrongful-rejection-v1-fit-ledger.json",
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
    expect(ledger.category).toBe("noReferenceInput");
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
    for (const stage of [
      "init",
      "step-1",
      "step-2",
      "step-3",
      "step-4",
      "remove",
      "field-certification",
      "publish-step-0",
      "publish-step-1",
      "publish-step-2",
      "publish-step-3",
    ])
      expect(names.has(`819/818/true/false/${stage}/0`), stage).toBe(true);
    for (const chunk of [0, 1, 2])
      expect(names.has(`819/818/true/false/field-publications/${chunk}`)).toBe(
        true,
      );
    for (const index of [-1, 0, 1])
      for (const stage of [
        "cancel-0",
        "cancel-1",
        "cancel-2",
        "cancel-3",
        "step-4",
        "remove",
      ])
        expect(names.has(`1/${index}/false/false/${stage}/0`)).toBe(true);
    expect(names.has("1/0/false/true/honest-cancel/0")).toBe(true);
  });
});
