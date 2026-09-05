import { createHash } from "node:crypto";
import { readFile } from "node:fs/promises";

import { describe, expect, it } from "vitest";

import {
  buildVanRossemFitLedger,
  type VanRossemFitLedger,
} from "../src/proof-fit/van-rossem-fit-ledger.js";
import { realBlueprintPath } from "./support/emulator/blueprints.js";

describe("minAda measured wrongful-rejection fit ledger", () => {
  it("binds complete maximum evidence and publication margins to the current blueprint", async () => {
    const ledger = JSON.parse(
      await readFile(
        new URL(
          "../../../docs/fault-proofs/size-plans/min-ada-wrongful-rejection-v1-fit-ledger.json",
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
    expect(ledger.category).toBe("minAda");
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
    const names = ledger.entries.map((row) => row.name);
    for (const stage of [
      "family-publications",
      "yield-publications",
      "bind",
      "field-publications",
      "certificate",
      "open",
      "predicate",
      "mint",
      "remove",
      "cancel",
      "scan-checkpoint",
      "restart-bind",
      "restart-open",
    ])
      expect(
        names.some((name) => name.includes(`/${stage}/`)),
        stage,
      ).toBe(true);
    for (const stage of [
      "proof-publications",
      "membership",
      "predecessor",
      "mint",
      "remove",
      "chunked-reward-registration",
    ])
      expect(
        names.some(
          (name) =>
            name.startsWith(
              "maximum-post-utxo-descriptor-and-64-node-roots/",
            ) && name.includes(`/${stage}/`),
        ),
        stage,
      ).toBe(true);
    expect(
      ledger.entries.some(
        (row) =>
          row.maximumShape.includes("assets-1304") &&
          row.maximumShape.includes("output-16384-field-32768-mpf-64"),
      ),
    ).toBe(true);
  });
});
