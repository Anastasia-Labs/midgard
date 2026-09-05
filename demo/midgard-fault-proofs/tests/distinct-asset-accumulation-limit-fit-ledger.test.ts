import { createHash } from "node:crypto";
import { readFile } from "node:fs/promises";

import { describe, expect, it } from "vitest";

import {
  buildVanRossemFitLedger,
  type VanRossemFitLedger,
} from "../src/proof-fit/van-rossem-fit-ledger.js";
import { realBlueprintPath } from "./support/submit-init-emulator-shared.js";

const path = new URL(
  "../../../docs/fault-proofs/size-plans/distinct-asset-accumulation-limit-v1-fit-ledger.json",
  import.meta.url,
);

describe("distinct-asset accumulation Van Rossem fit ledger", () => {
  it("reproduces all maximum arms and margins against the current testnet blueprint", async () => {
    const ledger = JSON.parse(
      await readFile(path, "utf8"),
    ) as VanRossemFitLedger;
    expect(ledger.blueprintSha256).toBe(
      createHash("sha256")
        .update(await readFile(realBlueprintPath))
        .digest("hex"),
    );
    expect(ledger.category).toBe(
      "distinctAssetAccumulationLimit:00000035:testnet",
    );
    expect(ledger.compilerVersion).toBe("aiken v1.1.23+5adf783");
    expect(ledger.entries).toHaveLength(255);
    expect(new Set(ledger.entries.map((row) => row.maximumShape)).size).toBe(
      18,
    );
    expect(
      ledger.entries.filter((row) => row.kind === "publication"),
    ).toHaveLength(111);
    expect(
      ledger.entries.filter(
        (row) => BigInt(row.memoryUnits) > 0n && BigInt(row.cpuUnits) > 0n,
      ),
    ).toHaveLength(144);
    for (const kind of ["input", "output", "mint"])
      for (const forced of [false, true]) {
        const maximum = ledger.entries.filter((row) =>
          row.maximumShape.includes(`'${kind}' forced=${forced} maximum=true`),
        );
        for (const arm of [
          "init",
          "step01",
          "step02",
          `step0${kind === "input" ? 3 : kind === "output" ? 4 : 5}-${kind}-decisive`,
          "step06-permanent-mint",
          "leased-removal",
        ])
          expect(
            maximum.some((row) => row.name.includes(`:${arm}:`)),
            `${kind} ${forced} ${arm}`,
          ).toBe(true);
        expect(
          maximum.filter((row) => row.kind === "publication"),
        ).toHaveLength(forced ? 6 : 7);
      }
    expect(
      ledger.entries.filter((row) => row.name.includes(":cancel-step-")),
    ).toHaveLength(12);
    expect(
      buildVanRossemFitLedger({
        category: ledger.category,
        blueprintSha256: ledger.blueprintSha256,
        compilerVersion: ledger.compilerVersion,
        measurements: ledger.entries.map((row) => ({
          ...row,
          memoryUnits: BigInt(row.memoryUnits),
          cpuUnits: BigInt(row.cpuUnits),
        })),
      }),
    ).toEqual(ledger);
  });
});
