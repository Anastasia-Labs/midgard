import { createHash } from "node:crypto";
import { readFile } from "node:fs/promises";

import { describe, expect, it } from "vitest";

import {
  MAXIMUM_CERTIFIED_OUTPUT_COUNT,
  MAXIMUM_SUPPORTED_OUTPUT_COUNT,
} from "./support/network-id-shapes.js";

const ledgerPath = new URL(
  "../../../docs/fault-proofs/size-plans/network-id-wrongful-rejection-v1-fit-ledger.json",
  import.meta.url,
);

type LifecycleRow = {
  readonly transactions: number;
  readonly maxSignedBytes: number;
  readonly maxMemory: string;
  readonly maxCpu: string;
};

describe("network-id wrongful-rejection fit ledger", () => {
  it("reproduces its digest and every publication/lifecycle margin", async () => {
    const parsed = JSON.parse(await readFile(ledgerPath, "utf8")) as {
      category: string;
      categoryId: string;
      referencePublications: readonly {
        step: string;
        signedBytes: number;
        reserveMarginBytes: number;
      }[];
      acceptedLifecycle: LifecycleRow;
      forcedBaselineLifecycle: LifecycleRow;
      forcedMaximumLifecycle: LifecycleRow & {
        outputs: number;
        fieldBytes: number;
        carriage: string;
        scanBatch: number;
      };
      forcedCertifiedLifecycle: LifecycleRow & {
        outputs: number;
        fieldBytes: number;
        carriage: string;
        scanBatch: number;
        grammarBatch: number;
      };
      minimumMargins: {
        publicationBytes: number;
        lifecycleBytes: number;
        memory: string;
        cpu: string;
      };
      ledgerDigest: string;
    };
    expect(parsed.category).toBe("networkId");
    expect(parsed.categoryId).toBe("0000001c");
    expect(parsed.referencePublications.map((row) => row.step)).toStrictEqual([
      "step01",
      "step02",
      "forcedStep",
      "forcedScan",
    ]);
    for (const row of parsed.referencePublications) {
      expect(row.signedBytes).toBeLessThanOrEqual(15_872);
      expect(row.reserveMarginBytes).toBe(15_872 - row.signedBytes);
      expect(row.reserveMarginBytes).toBeGreaterThan(0);
    }
    expect(parsed.forcedMaximumLifecycle.outputs).toBe(
      MAXIMUM_SUPPORTED_OUTPUT_COUNT,
    );
    expect(parsed.forcedMaximumLifecycle.carriage).toBe("RawUtxo");
    expect(parsed.forcedCertifiedLifecycle.outputs).toBe(
      MAXIMUM_CERTIFIED_OUTPUT_COUNT,
    );
    expect(parsed.forcedCertifiedLifecycle.carriage).toBe("Certified");
    for (const row of [
      parsed.acceptedLifecycle,
      parsed.forcedBaselineLifecycle,
      parsed.forcedMaximumLifecycle,
      parsed.forcedCertifiedLifecycle,
    ]) {
      expect(row.maxSignedBytes).toBeLessThanOrEqual(16_384);
      expect(BigInt(row.maxMemory)).toBeLessThanOrEqual(13_200_000n);
      expect(BigInt(row.maxCpu)).toBeLessThanOrEqual(8_000_000_000n);
    }
    expect(parsed.minimumMargins.publicationBytes).toBeGreaterThan(0);
    expect(parsed.minimumMargins.lifecycleBytes).toBeGreaterThan(0);
    expect(BigInt(parsed.minimumMargins.memory)).toBeGreaterThan(0n);
    expect(BigInt(parsed.minimumMargins.cpu)).toBeGreaterThan(0n);
    const { ledgerDigest, ...body } = parsed;
    expect(
      createHash("sha256").update(JSON.stringify(body)).digest("hex"),
    ).toBe(ledgerDigest);
  });
});
