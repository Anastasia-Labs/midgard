import { createHash } from "node:crypto";
import { readFile } from "node:fs/promises";

import { FRAUD_PROOF_CATALOGUE_CATEGORY_IDS } from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";
describe("invalidSignature measured field-maximum ledger", () => {
  it("checks digest, category, publications and all signed transaction margins", async () => {
    const ledger = JSON.parse(
      await readFile(
        new URL(
          "../../../docs/fault-proofs/size-plans/invalid-signature-wrongful-rejection-v1-fit-ledger.json",
          import.meta.url,
        ),
        "utf8",
      ),
    ) as {
      categoryId: string;
      referencePublications: { reserveMarginBytes: number }[];
      acceptedLifecycle: { bytes: number; memory: string; cpu: string }[];
      forcedLifecycle: { bytes: number; memory: string; cpu: string }[];
      ledgerDigest: string;
    };
    expect(ledger.categoryId).toBe(
      FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.invalidSignature,
    );
    expect(ledger.referencePublications).toHaveLength(2);
    for (const row of ledger.referencePublications)
      expect(row.reserveMarginBytes).toBeGreaterThan(0);
    expect(ledger.forcedLifecycle.length).toBeGreaterThan(40);
    expect(ledger.acceptedLifecycle.length).toBeGreaterThan(0);
    for (const row of [
      ...ledger.acceptedLifecycle,
      ...ledger.forcedLifecycle,
    ]) {
      expect(row.bytes).toBeLessThan(16_384);
      expect(BigInt(row.memory)).toBeLessThan(16_500_000n);
      expect(BigInt(row.cpu)).toBeLessThan(10_000_000_000n);
    }
    const { ledgerDigest, ...body } = ledger;
    expect(
      createHash("sha256").update(JSON.stringify(body)).digest("hex"),
    ).toBe(ledgerDigest);
  });
});
