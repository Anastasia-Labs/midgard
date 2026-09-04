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
  "../../../docs/fault-proofs/size-plans/field-item-width-illegal-v1-fit-ledger.json",
  import.meta.url,
);

/**
 * The locked testnet blueprint the ledger was measured against
 * (`aiken build --env testnet` with the pinned fork). A regeneration that
 * moves this digest re-measures the ledger through
 * `field-item-width-illegal-lifecycle.test.ts` with
 * `MIDGARD_WRITE_FIT_LEDGER=1`.
 */
const PINNED_BLUEPRINT_SHA256 =
  "172c72d392706de52b5665dc0c1b208354a490298fba5ac0f411597f8454d10b";
const PINNED_COMPILER = "aiken v1.1.23+5adf783";

/**
 * Both field rules, both directions, at the maximum carriage the family can be
 * asked to open: the field-2 shapes are 32,768/32,767-byte fields under three
 * certified chunks, the field-5 shapes ride inline carriage.
 */
const EXPECTED_ENTRIES = [
  "accepted-cancel-step01",
  "accepted-cancel-step02",
  "accepted-cancel-step03",
  "accepted-carriage-certificate",
  "accepted-carriage-chunk01",
  "accepted-carriage-chunk02",
  "accepted-carriage-chunk03",
  "accepted-init",
  "accepted-mint-init",
  "accepted-mint-remove",
  "accepted-mint-step01",
  "accepted-mint-step02",
  "accepted-mint-step03-proof-mint",
  "accepted-remove",
  "accepted-step01",
  "accepted-step02",
  "accepted-step03-proof-mint",
  "forced-carriage-certificate",
  "forced-carriage-chunk01",
  "forced-carriage-chunk02",
  "forced-carriage-chunk03",
  "forced-init",
  "forced-mint-init",
  "forced-mint-remove",
  "forced-mint-step01",
  "forced-mint-step02",
  "forced-mint-step03-proof-mint",
  "forced-remove",
  "forced-step01",
  "forced-step02",
  "forced-step03-proof-mint",
  "publish-step01",
  "publish-step02",
  "publish-step03",
] as const;

describe("fieldItemWidthIllegal Van Rossem fit ledger", () => {
  it("pins the locked testnet blueprint and a positive margin for every publication and maximum-shape lifecycle transaction", async () => {
    const ledger = JSON.parse(
      await readFile(path, "utf8"),
    ) as VanRossemFitLedger;
    expect(ledger.schemaVersion).toBe(VAN_ROSSEM_FIT_LEDGER_SCHEMA_VERSION);
    expect(ledger.category).toBe("fieldItemWidthIllegal:00000021:testnet");
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
      }
    }
    // Every scripted lifecycle transaction was evaluated locally; only the
    // chunk publications carry no redeemer.
    for (const entry of ledger.entries) {
      if (entry.kind === "lifecycle" && !/carriage-chunk/u.test(entry.name)) {
        expect(BigInt(entry.memoryUnits), entry.name).toBeGreaterThan(0n);
        expect(BigInt(entry.cpuUnits), entry.name).toBeGreaterThan(0n);
      }
    }
    // The maximum field-2 shape is the three-chunk certified carriage: two
    // full chunks at exactly the reliable publication target and a tail.
    expect(
      ledger.entries
        .filter((entry) =>
          /^(accepted|forced)-carriage-chunk0[12]$/u.test(entry.name),
        )
        .map((entry) => entry.signedBytes),
    ).toEqual([
      VAN_ROSSEM_PUBLICATION_TARGET_BYTES,
      VAN_ROSSEM_PUBLICATION_TARGET_BYTES,
      VAN_ROSSEM_PUBLICATION_TARGET_BYTES,
      VAN_ROSSEM_PUBLICATION_TARGET_BYTES,
    ]);
  });
});
