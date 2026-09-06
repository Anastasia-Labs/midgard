import { createHash } from "node:crypto";
import { readFile } from "node:fs/promises";

import { describe, expect, it } from "vitest";

import {
  buildVanRossemFitLedger,
  type VanRossemFitLedger,
} from "../src/proof-fit/van-rossem-fit-ledger.js";
import { realBlueprintPath } from "./support/emulator/blueprints.js";

describe("CEK selection current blueprint fit evidence", () => {
  it.each(["selection", "material-task"])(
    "verifies the %s ledger and its measured margins",
    async (kind) => {
      const ledger: VanRossemFitLedger = JSON.parse(
        await readFile(
          new URL(
            `../../../docs/fault-proofs/size-plans/validation-trace-cek-${kind}-fit-ledger.json`,
            import.meta.url,
          ),
          "utf8",
        ),
      );
      expect(ledger.blueprintSha256).toBe(
        createHash("sha256")
          .update(await readFile(realBlueprintPath))
          .digest("hex"),
      );
      expect(ledger.compilerVersion).toBe("v1.1.23+5adf783");
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
      expect(new Set(ledger.entries.map((row) => row.maximumShape)).size).toBe(
        kind === "selection" ? 6 : 2,
      );
      if (kind === "material-task") {
        expect(ledger.entries.map((row) => row.name)).toContain(
          "program/maximum-task",
        );
        expect(ledger.entries.map((row) => row.name)).toContain(
          "data/maximum-task",
        );
      }
    },
  );
});
