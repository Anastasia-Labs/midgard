import { createHash } from "node:crypto";
import { readFile } from "node:fs/promises";

import { describe, expect, it } from "vitest";

import {
  VAN_ROSSEM_FIT_LEDGER_SCHEMA_VERSION,
  VAN_ROSSEM_MAX_CPU_UNITS,
  VAN_ROSSEM_MAX_MEMORY_UNITS,
  VAN_ROSSEM_MAX_SIGNED_TX_BYTES,
  VAN_ROSSEM_PUBLICATION_TARGET_BYTES,
  type VanRossemFitLedger,
} from "../src/proof-fit/van-rossem-fit-ledger.js";

const path = new URL(
  "../../../docs/fault-proofs/size-plans/receive-purpose-language-v1-fit-ledger.json",
  import.meta.url,
);

/**
 * The locked testnet blueprint the ledger was measured against
 * (`aiken build --env testnet` with the pinned fork). A regeneration that
 * moves this digest re-measures the ledger through
 * `receive-purpose-language-lifecycle.test.ts` with
 * `MIDGARD_WRITE_FIT_LEDGER=1`.
 */
const PINNED_BLUEPRINT_SHA256 =
  "1a30174645c166bd33f4060f22debfbac175a8fda053e307ab7a893520b413e9";
const PINNED_COMPILER = "aiken v1.1.23+5adf783";

const EXPECTED_ENTRIES = [
  "accepted-cancel-step01",
  "accepted-cancel-step02",
  "accepted-cancel-step03",
  "accepted-init",
  "accepted-remove",
  "accepted-step01",
  "accepted-step02",
  "accepted-step03-mint",
  "forced-init",
  "forced-remove",
  "forced-step01",
  "forced-step02",
  "forced-step03-mint",
  "publish-step01",
  "publish-step02",
  "publish-step03",
] as const;

describe("receivePurposeLanguage Van Rossem fit ledger", () => {
  it("pins the locked testnet blueprint and a positive margin for every publication and maximum-shape lifecycle transaction", async () => {
    const ledger = JSON.parse(
      await readFile(path, "utf8"),
    ) as VanRossemFitLedger;
    expect(ledger.schemaVersion).toBe(VAN_ROSSEM_FIT_LEDGER_SCHEMA_VERSION);
    expect(ledger.category).toBe("receivePurposeLanguage");
    expect(ledger.blueprintSha256).toBe(PINNED_BLUEPRINT_SHA256);
    expect(ledger.compilerVersion).toBe(PINNED_COMPILER);
    expect(ledger.entries.map((entry) => entry.name)).toEqual([
      ...EXPECTED_ENTRIES,
    ]);
    for (const entry of ledger.entries) {
      expect(entry.signedByteMargin, entry.name).toBe(
        VAN_ROSSEM_MAX_SIGNED_TX_BYTES - entry.signedBytes,
      );
      expect(entry.signedByteMargin, entry.name).toBeGreaterThan(0);
      expect(BigInt(entry.memoryUnitMargin), entry.name).toBe(
        VAN_ROSSEM_MAX_MEMORY_UNITS - BigInt(entry.memoryUnits),
      );
      expect(BigInt(entry.memoryUnitMargin), entry.name).toBeGreaterThan(0n);
      expect(BigInt(entry.cpuUnitMargin), entry.name).toBe(
        VAN_ROSSEM_MAX_CPU_UNITS - BigInt(entry.cpuUnits),
      );
      expect(BigInt(entry.cpuUnitMargin), entry.name).toBeGreaterThan(0n);
      if (entry.kind === "publication") {
        expect(entry.publicationReserveMargin, entry.name).toBe(
          VAN_ROSSEM_PUBLICATION_TARGET_BYTES - entry.signedBytes,
        );
        expect(
          entry.publicationReserveMargin,
          entry.name,
        ).toBeGreaterThanOrEqual(0);
      } else {
        expect(entry.publicationReserveMargin, entry.name).toBeNull();
        expect(entry.maximumShape).toMatch(
          /^256 purposes \((plutusV3|native) receive at execution \d+\), 16 validation traces$/u,
        );
        // Every lifecycle row also clears the 20% proof-fit reserve.
        expect(BigInt(entry.memoryUnits), entry.name).toBeLessThanOrEqual(
          (VAN_ROSSEM_MAX_MEMORY_UNITS * 80n) / 100n,
        );
        expect(BigInt(entry.cpuUnits), entry.name).toBeLessThanOrEqual(
          (VAN_ROSSEM_MAX_CPU_UNITS * 80n) / 100n,
        );
      }
    }
    const { ledgerSha256, ...body } = ledger;
    expect(
      createHash("sha256")
        .update(`${JSON.stringify(body, null, 2)}\n`)
        .digest("hex"),
    ).toBe(ledgerSha256);
  });
});
