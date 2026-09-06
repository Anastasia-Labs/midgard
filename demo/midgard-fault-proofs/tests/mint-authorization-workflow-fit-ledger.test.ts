import { createHash } from "node:crypto";
import { readFile } from "node:fs/promises";

import { describe, expect, it } from "vitest";

import {
  buildVanRossemFitLedger,
  type VanRossemFitLedger,
} from "../src/proof-fit/van-rossem-fit-ledger.js";
import { realBlueprintPath } from "./support/emulator/blueprints.js";

describe("mintAuthorization installed workflow fit ledger", () => {
  it("binds complete maximum evidence and publication margins to the current blueprint", async () => {
    const ledger = JSON.parse(
      await readFile(
        new URL(
          "../../../docs/fault-proofs/size-plans/mint-authorization-workflow-fit-ledger.json",
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
    expect(ledger.category).toBe("mintAuthorization");
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
      "absent-mint",
      "absent-burn",
      "unsatisfied-mint",
      "unsatisfied-burn",
      "maximum-signers",
      "maximum-mint-tail",
      "maximum-native-wide",
      "maximum-native-deep",
      "maximum-native-signers",
      "maximum-witnesses",
      "reference-absent",
      "reference-unsatisfied",
      "maximum-reference-tail",
    ]) {
      const absent =
        scenario.startsWith("absent") ||
        [
          "maximum-mint-tail",
          "maximum-witnesses",
          "reference-absent",
          "maximum-reference-tail",
        ].includes(scenario);
      for (const stage of [
        "init",
        "step_01",
        "step_02",
        "step_03",
        absent ? "step_07" : "step_06",
        "step_05",
        "remove",
      ])
        expect(
          [...names].some((name) => name.startsWith(`${scenario}/${stage}/`)),
          `${scenario}/${stage}`,
        ).toBe(true);
      for (let index = 0; index < 13; index++)
        expect(names.has(`${scenario}/publish/${index}`)).toBe(true);
      expect(
        [...names].some((name) =>
          name.startsWith(`${scenario}/publish-removal/`),
        ),
      ).toBe(true);
    }
    for (const scenario of ["maximum-native-wide", "maximum-native-deep"])
      expect(
        ledger.entries.filter((row) =>
          row.name.startsWith(`${scenario}/step_06/`),
        ).length,
      ).toBeGreaterThan(1000);
    expect(
      ledger.entries.some(
        (row) => row.maximumShape === "32759 raw script bytes",
      ),
    ).toBe(true);
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
        [...names].some((name) => name.startsWith(`maximum-proofs/${stage}/`)),
      ).toBe(true);
    for (let index = 0; index < 13; index++)
      expect(names.has(`maximum-proofs/publish/${index}`)).toBe(true);
    for (const stage of [
      "source-publications",
      "field-publications",
      "field-certificate",
      "claim-publications",
      "publish-removal",
    ])
      expect(
        [...names].some((name) => name.startsWith(`maximum-proofs/${stage}/`)),
      ).toBe(true);
    expect(names.has("cancel-witness-scan/cancel/0")).toBe(true);
    expect(
      ledger.entries.filter((row) =>
        row.name.startsWith("maximum-witnesses/step_07/"),
      ).length,
    ).toBeGreaterThan(100);
    expect(
      ledger.entries.filter((row) =>
        row.name.startsWith("maximum-reference-tail/step_04/"),
      ).length,
    ).toBeGreaterThanOrEqual(820);
  });
});
