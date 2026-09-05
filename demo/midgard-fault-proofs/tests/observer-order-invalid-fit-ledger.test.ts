import { readFile } from "node:fs/promises";

import { describe, expect, it } from "vitest";

import { OBSERVER_ORDER_INVALID_ITEM_BUDGET } from "../src/observer-order-invalid/family.js";
import {
  VAN_ROSSEM_FIT_LEDGER_SCHEMA_VERSION,
  VAN_ROSSEM_MAX_CPU_UNITS,
  VAN_ROSSEM_MAX_MEMORY_UNITS,
  VAN_ROSSEM_MAX_SIGNED_TX_BYTES,
  VAN_ROSSEM_PUBLICATION_TARGET_BYTES,
  type VanRossemFitLedger,
} from "../src/proof-fit/van-rossem-fit-ledger.js";

const path = new URL(
  "../../../docs/fault-proofs/size-plans/observer-order-invalid-v1-fit-ledger.json",
  import.meta.url,
);

/**
 * The locked testnet blueprint the ledger was measured against
 * (`aiken build --env testnet` with the pinned fork). A regeneration that
 * moves this digest re-measures the ledger through
 * `observer-order-invalid-lifecycle.test.ts` with `MIDGARD_WRITE_FIT_LEDGER=1`.
 */
const PINNED_BLUEPRINT_SHA256 =
  "569daa74f2f35c97fcfa3f541a123ca4c7dde3ff113b8fa8dd1bfba8c5499182";
const PINNED_COMPILER = "aiken v1.1.23+5adf783";

/** The §5.4 aggregate-bound field: 1,092 observers over three chunks. */
const MAXIMUM_OBSERVERS = 1092;
const MAXIMUM_SCANS = Math.ceil(
  MAXIMUM_OBSERVERS / OBSERVER_ORDER_INVALID_ITEM_BUDGET,
);

const scans = (prefix: string, count: number) =>
  Array.from(
    { length: count },
    (_, ordinal) =>
      `${prefix}-step03-scan${(ordinal + 1).toString().padStart(2, "0")}`,
  );
const chain = (prefix: string, scanCount: number) => [
  `${prefix}-init`,
  `${prefix}-step01`,
  `${prefix}-step02`,
  ...scans(prefix, scanCount),
  `${prefix}-step04-proof-mint`,
  `${prefix}-remove`,
];
const carriage = (prefix: string) => [
  `${prefix}-carriage-chunk01`,
  `${prefix}-carriage-chunk02`,
  `${prefix}-carriage-chunk03`,
  `${prefix}-carriage-certificate`,
];

/**
 * Both directions at the maximum certified field (accepted with its last
 * pair descending, forced strictly ascending and cited at its last ordinal),
 * every cancel, and the inline shapes: first-pair and middle-duplicate
 * convictions, and the forced contradictions at a middle ordinal, past the
 * field's end, over the empty field, and at ordinal 0.
 */
const EXPECTED_ENTRIES = [
  "publish-step01",
  "publish-step02",
  "publish-step03",
  "publish-step04",
  ...carriage("accepted"),
  "accepted-cancel-step01",
  "accepted-cancel-step02",
  "accepted-cancel-step03",
  "accepted-cancel-step04",
  ...chain("accepted", MAXIMUM_SCANS),
  ...chain("accepted-first", 1),
  ...chain("accepted-duplicate", 1),
  ...carriage("forced-maximum"),
  ...chain("forced-maximum", MAXIMUM_SCANS),
  ...chain("forced-middle", 1),
  ...chain("forced-past-end", 1),
  ...chain("forced-empty", 1),
  ...chain("forced-zero", 1),
];

describe("observerOrderInvalid Van Rossem fit ledger", () => {
  it("pins the locked testnet blueprint and a positive margin for every publication and maximum-shape lifecycle transaction", async () => {
    const ledger = JSON.parse(
      await readFile(path, "utf8"),
    ) as VanRossemFitLedger;
    expect(ledger.schemaVersion).toBe(VAN_ROSSEM_FIT_LEDGER_SCHEMA_VERSION);
    expect(ledger.category).toBe("observerOrderInvalid:00000025:testnet");
    expect(ledger.blueprintSha256).toBe(PINNED_BLUEPRINT_SHA256);
    expect(ledger.compilerVersion).toBe(PINNED_COMPILER);
    expect([...ledger.entries.map((entry) => entry.name)].sort()).toEqual(
      [...EXPECTED_ENTRIES].sort(),
    );
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
    // The maximum field is three certified chunks: the first two sit exactly
    // at the reliable publication target in both directions.
    expect(
      ledger.entries
        .filter((entry) =>
          /^(accepted|forced-maximum)-carriage-chunk0[12]$/u.test(entry.name),
        )
        .map((entry) => entry.signedBytes),
    ).toEqual(
      Array.from({ length: 4 }, () => VAN_ROSSEM_PUBLICATION_TARGET_BYTES),
    );
  });
});
