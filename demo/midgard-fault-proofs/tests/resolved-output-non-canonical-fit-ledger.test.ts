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
  "../../../docs/fault-proofs/size-plans/resolved-output-non-canonical-v1-fit-ledger.json",
  import.meta.url,
);

/**
 * The locked testnet blueprint the ledger was measured against
 * (`aiken build --env testnet` with the pinned fork). A regeneration that
 * moves this digest re-measures the ledger through
 * `resolved-output-non-canonical-lifecycle.test.ts` with
 * `MIDGARD_WRITE_FIT_LEDGER=1`.
 */
const PINNED_BLUEPRINT_SHA256 =
  "7575a8a945eeae2014725a340a33eddeacfa4774554ea11085fe57e49c9d3d6e";
const PINNED_COMPILER = "aiken v1.1.23+5adf783";

const resumeRows = (prefix: string, count: number) =>
  Array.from(
    { length: count },
    (_v, index) =>
      `${prefix}-step04-resume-${index.toString().padStart(2, "0")}`,
  );

/**
 * Both directions at the maximum shape: the 16,384-byte prior-ledger output
 * walked through sixteen step-04 transitions before the accepted direction's
 * structural terminal or the forced direction's `FinalizeCanonical` closure.
 */
const EXPECTED_ENTRIES = [
  "accepted-cancel-step01",
  "accepted-carriage-certificate",
  "accepted-carriage-chunk01",
  "accepted-carriage-chunk02",
  "accepted-carriage-chunk03",
  "accepted-init",
  "accepted-remove-fraudulent-block",
  "accepted-step01",
  "accepted-step02",
  "accepted-step03-prior-membership",
  ...resumeRows("accepted", 16),
  "accepted-step04-terminal",
  "accepted-step05-proof-mint",
  "forced-cancel-step02",
  "forced-carriage-certificate",
  "forced-carriage-chunk01",
  "forced-carriage-chunk02",
  "forced-carriage-chunk03",
  "forced-init",
  "forced-remove-fraudulent-block",
  "forced-step01",
  "forced-step02",
  "forced-step03-prior-membership",
  "forced-step04-finalize-canonical",
  ...resumeRows("forced", 16),
  "forced-step05-proof-mint",
  "publish-step01",
  "publish-step02",
  "publish-step03",
  "publish-step04",
  "publish-step05",
] as const;

describe("resolvedOutputNonCanonical Van Rossem fit ledger", () => {
  it("pins the locked testnet blueprint and a positive margin for every publication and maximum-shape lifecycle transaction", async () => {
    const ledger = JSON.parse(
      await readFile(path, "utf8"),
    ) as VanRossemFitLedger;
    expect(ledger.schemaVersion).toBe(VAN_ROSSEM_FIT_LEDGER_SCHEMA_VERSION);
    expect(ledger.category).toBe("resolvedOutputNonCanonical:00000026:testnet");
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
          /^16,384-byte prior-ledger output at adversarial membership depth and a Certified 800-item input field$/u,
        );
        // Carriage chunk publications carry bytes only; every script-running
        // row also clears the 20% proof-fit reserve.
        if (!/-carriage-chunk\d+$/u.test(entry.name)) {
          expect(BigInt(entry.memoryUnits), entry.name).toBeGreaterThan(0n);
          expect(BigInt(entry.cpuUnits), entry.name).toBeGreaterThan(0n);
        }
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
