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
  "../../../docs/fault-proofs/size-plans/witness-script-decoding-v1-fit-ledger.json",
  import.meta.url,
);

/**
 * The locked testnet blueprint the ledger was measured against
 * (`aiken build --env testnet` with the pinned fork). A regeneration that
 * moves this digest re-measures the ledger through
 * `witness-script-decoding-lifecycle.test.ts` with
 * `MIDGARD_WRITE_FIT_LEDGER=1`.
 */
const PINNED_BLUEPRINT_SHA256 =
  "e3beed44cd4d16d6cf060a6c31f57f435c52fabc61b09a623b839438474f4880";
const PINNED_COMPILER = "aiken v1.1.23+5adf783";

/**
 * Every measured lifecycle transaction, in the ledger's canonical order:
 * the accepted header, malformed-payload and empty-payload convictions, the
 * four forced contradictions (the node and depth arms at the widest and
 * deepest shapes the 32,768-byte field bound admits), and the four applied
 * publications.
 */
const EXPECTED_ENTRIES = [
  "accepted-empty-init",
  "accepted-empty-remove",
  "accepted-empty-step01",
  "accepted-empty-step02",
  "accepted-empty-step03-close",
  "accepted-empty-step04-proof-mint",
  "accepted-header-cancel-step01",
  "accepted-header-cancel-step02",
  "accepted-header-cancel-step03",
  "accepted-header-cancel-step04",
  "accepted-header-carriage-certificate",
  "accepted-header-carriage-chunk01",
  "accepted-header-carriage-chunk02",
  "accepted-header-carriage-chunk03",
  "accepted-header-init",
  "accepted-header-remove",
  "accepted-header-step01",
  "accepted-header-step02",
  "accepted-header-step03-close",
  "accepted-header-step04-proof-mint",
  "accepted-native-carriage-certificate",
  "accepted-native-carriage-chunk01",
  "accepted-native-carriage-chunk02",
  "accepted-native-carriage-chunk03",
  "accepted-native-init",
  "accepted-native-remove",
  "accepted-native-step01",
  "accepted-native-step02",
  "accepted-native-step03-close",
  "accepted-native-step03-resume-first",
  "accepted-native-step04-proof-mint",
  "forced-depth-deep-carriage-certificate",
  "forced-depth-deep-carriage-chunk01",
  "forced-depth-deep-carriage-chunk02",
  "forced-depth-deep-carriage-chunk03",
  "forced-depth-deep-init",
  "forced-depth-deep-remove",
  "forced-depth-deep-step01",
  "forced-depth-deep-step02",
  "forced-depth-deep-step03-close",
  "forced-depth-deep-step03-resume-first",
  "forced-depth-deep-step03-resume-max",
  "forced-depth-deep-step04-proof-mint",
  "forced-header-init",
  "forced-header-remove",
  "forced-header-step01",
  "forced-header-step02",
  "forced-header-step03-close",
  "forced-header-step04-proof-mint",
  "forced-native-init",
  "forced-native-remove",
  "forced-native-step01",
  "forced-native-step02",
  "forced-native-step03-close",
  "forced-native-step04-proof-mint",
  "forced-node-wide-carriage-certificate",
  "forced-node-wide-carriage-chunk01",
  "forced-node-wide-carriage-chunk02",
  "forced-node-wide-carriage-chunk03",
  "forced-node-wide-init",
  "forced-node-wide-remove",
  "forced-node-wide-step01",
  "forced-node-wide-step02",
  "forced-node-wide-step03-close",
  "forced-node-wide-step03-resume-first",
  "forced-node-wide-step03-resume-max",
  "forced-node-wide-step04-proof-mint",
  "publish-step01",
  "publish-step02",
  "publish-step03",
  "publish-step04",
] as const;

describe("witnessScriptDecoding Van Rossem fit ledger", () => {
  it("pins the locked testnet blueprint and a positive margin for every publication and maximum-shape lifecycle transaction", async () => {
    const ledger = JSON.parse(
      await readFile(path, "utf8"),
    ) as VanRossemFitLedger;
    expect(ledger.schemaVersion).toBe(VAN_ROSSEM_FIT_LEDGER_SCHEMA_VERSION);
    expect(ledger.category).toBe("witnessScriptDecoding:00000022:testnet");
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
    // The maximum field is three certified chunks in every maximum-shape
    // direction; the first two sit exactly at the reliable publication
    // target.
    for (const prefix of [
      "accepted-header",
      "accepted-native",
      "forced-node-wide",
      "forced-depth-deep",
    ]) {
      expect(
        ledger.entries
          .filter((entry) =>
            new RegExp(`^${prefix}-carriage-chunk0[12]$`, "u").test(entry.name),
          )
          .map((entry) => entry.signedBytes),
      ).toEqual([
        VAN_ROSSEM_PUBLICATION_TARGET_BYTES,
        VAN_ROSSEM_PUBLICATION_TARGET_BYTES,
      ]);
    }
  });
});
