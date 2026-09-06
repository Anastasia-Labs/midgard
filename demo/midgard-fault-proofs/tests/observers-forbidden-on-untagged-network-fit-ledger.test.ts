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
  "../../../docs/fault-proofs/size-plans/observers-forbidden-on-untagged-network-v1-fit-ledger.json",
  import.meta.url,
);

/**
 * The locked testnet blueprint the ledger was measured against
 * (`aiken build --env testnet` with the pinned fork). A regeneration that
 * moves this digest re-measures the ledger through
 * `observers-forbidden-on-untagged-network-lifecycle.test.ts` with
 * `MIDGARD_WRITE_FIT_LEDGER=1`.
 */
const PINNED_BLUEPRINT_SHA256 =
  "314b1134813eb745b7f8df611d5643257df64774e2a199461271fb7f479062f8";
const PINNED_COMPILER = "aiken v1.1.23+5adf783";

/**
 * Both directions at the maximum 505-observer certified field (accepted on
 * scalar 255, forced on a tagged scalar), plus the two inline forced
 * polarities: the empty field on scalar 255 and the native-only transaction
 * whose observers the machine never reaches.
 */
const EXPECTED_ENTRIES = [
  "accepted-cancel-step01",
  "accepted-cancel-step02",
  "accepted-carriage-certificate",
  "accepted-carriage-chunk01",
  "accepted-carriage-chunk02",
  "accepted-init",
  "accepted-remove",
  "accepted-step01",
  "accepted-step02-proof-mint",
  "forced-empty-init",
  "forced-empty-remove",
  "forced-empty-step01",
  "forced-empty-step02-proof-mint",
  "forced-native-init",
  "forced-native-remove",
  "forced-native-step01",
  "forced-native-step02-proof-mint",
  "forced-tagged-carriage-certificate",
  "forced-tagged-carriage-chunk01",
  "forced-tagged-carriage-chunk02",
  "forced-tagged-init",
  "forced-tagged-remove",
  "forced-tagged-step01",
  "forced-tagged-step02-proof-mint",
  "publish-step01",
  "publish-step02",
] as const;

describe("observersForbiddenOnUntaggedNetwork Van Rossem fit ledger", () => {
  it("pins the locked testnet blueprint and a positive margin for every publication and maximum-shape lifecycle transaction", async () => {
    const ledger = JSON.parse(
      await readFile(path, "utf8"),
    ) as VanRossemFitLedger;
    expect(ledger.schemaVersion).toBe(VAN_ROSSEM_FIT_LEDGER_SCHEMA_VERSION);
    expect(ledger.category).toBe(
      "observersForbiddenOnUntaggedNetwork:00000024:testnet",
    );
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
    // The maximum field is two certified chunks: the first sits exactly at
    // the reliable publication target in both directions.
    expect(
      ledger.entries
        .filter((entry) =>
          /^(accepted|forced-tagged)-carriage-chunk01$/u.test(entry.name),
        )
        .map((entry) => entry.signedBytes),
    ).toEqual([
      VAN_ROSSEM_PUBLICATION_TARGET_BYTES,
      VAN_ROSSEM_PUBLICATION_TARGET_BYTES,
    ]);
  });
});
