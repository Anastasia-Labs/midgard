import { createHash } from "node:crypto";
import { readFileSync } from "node:fs";

import { describe, expect, it } from "vitest";

import {
  buildVanRossemFitLedger,
  VAN_ROSSEM_FIT_LEDGER_SCHEMA_VERSION,
  type VanRossemFitLedger,
} from "../src/proof-fit/van-rossem-fit-ledger.js";
import { realBlueprintPath } from "./support/submit-init-emulator-shared.js";

/**
 * The stored ledger is written by the lifecycle suite
 * (`MIDGARD_WRITE_FIT_LEDGER=1`) from complete signed emulator measurements.
 * This gate pins that the stored artifact is bound to the fresh blueprint,
 * carries every publication and every maximum-shape lifecycle row the Wave 4
 * gate names, and reproduces byte-for-byte from its own measurements with
 * positive margins.
 */
const REQUIRED_ROWS = [
  "publish-step01",
  "publish-step02",
  "publish-step03",
  "publish-step04",
  "publish-step05",
  "init",
  "step01-accepted",
  "step02-authenticate",
  "step03-open-item",
  "step04-verdict-malformed",
  "step05-mint",
  "remove",
  "cancel-step01",
  "cancel-step02",
  "cancel-step03",
  "cancel-step04",
  "cancel-step05",
  "forced-step01",
  "forced-step02-authenticate",
  "forced-step03-open-item",
  "forced-step04-scan-00",
  "forced-step04-scan-widest-window",
  "forced-step04-close-exact-end",
  "forced-step05-mint",
  "forced-depth-step04-scan-00",
  "forced-depth-step05-mint",
  "forced-sig-step05-mint",
  "honest-forced-step04-close",
  "accepted-empty-step03-bind-close",
  "accepted-empty-step05-mint",
] as const;

describe("executionSourceScriptDecoding signed Van Rossem fit ledger", () => {
  it("is bound to the fresh blueprint and reproduces positive margins at the maximum shapes", () => {
    const stored = JSON.parse(
      readFileSync(
        new URL(
          "../../../docs/fault-proofs/size-plans/execution-source-script-decoding-v1-fit-ledger.json",
          import.meta.url,
        ),
        "utf8",
      ),
    ) as VanRossemFitLedger;
    expect(stored.schemaVersion).toBe(VAN_ROSSEM_FIT_LEDGER_SCHEMA_VERSION);
    expect(stored.category).toBe(
      "executionSourceScriptDecoding:00000031:testnet",
    );
    expect(stored.blueprintSha256).toBe(
      createHash("sha256")
        .update(readFileSync(realBlueprintPath))
        .digest("hex"),
    );
    expect(stored.compilerVersion).toBe("aiken v1.1.23+5adf783");
    const names = new Set(stored.entries.map((entry) => entry.name));
    for (const row of REQUIRED_ROWS) expect(names.has(row), row).toBe(true);
    for (const entry of stored.entries) {
      expect(entry.signedByteMargin, entry.name).toBeGreaterThan(0);
      expect(BigInt(entry.memoryUnitMargin), entry.name).toBeGreaterThan(0n);
      expect(BigInt(entry.cpuUnitMargin), entry.name).toBeGreaterThan(0n);
      if (entry.kind === "publication")
        expect(
          entry.publicationReserveMargin,
          entry.name,
        ).toBeGreaterThanOrEqual(0);
    }
    const reproduced = buildVanRossemFitLedger({
      category: stored.category,
      blueprintSha256: stored.blueprintSha256,
      compilerVersion: stored.compilerVersion,
      measurements: stored.entries.map((entry) => ({
        name: entry.name,
        kind: entry.kind,
        maximumShape: entry.maximumShape,
        signedBytes: entry.signedBytes,
        memoryUnits: BigInt(entry.memoryUnits),
        cpuUnits: BigInt(entry.cpuUnits),
      })),
    });
    expect(reproduced).toStrictEqual(stored);
  });
});
