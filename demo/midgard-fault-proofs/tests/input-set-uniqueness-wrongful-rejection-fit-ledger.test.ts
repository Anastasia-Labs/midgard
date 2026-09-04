import { createHash } from "node:crypto";
import { readFile } from "node:fs/promises";

import { describe, expect, it } from "vitest";

const ledgerPath = new URL(
  "../../../docs/fault-proofs/size-plans/input-set-uniqueness-wrongful-rejection-v1-fit-ledger.json",
  import.meta.url,
);

describe("input-set-uniqueness wrongful-rejection fit ledger", () => {
  it("reproduces its digest and every publication/lifecycle margin", async () => {
    const parsed = JSON.parse(await readFile(ledgerPath, "utf8")) as {
      category: string;
      categoryId: string;
      referencePublications: readonly { reserveMarginBytes: number }[];
      acceptedLifecycle: {
        maxSignedBytes: number;
        maxMemory: string;
        maxCpu: string;
      };
      forcedMaximumLifecycle: {
        referenceItems: number;
        referenceFieldBytes: number;
        observedCursors: readonly number[];
        maxSignedBytes: number;
        maxMemory: string;
        maxCpu: string;
      };
      ledgerDigest: string;
    };
    expect(parsed.category).toBe("inputSetUniqueness");
    expect(parsed.categoryId).toBe("0000001a");
    expect(
      parsed.referencePublications.every((row) => row.reserveMarginBytes > 0),
    ).toBe(true);
    expect(parsed.forcedMaximumLifecycle.referenceItems).toBe(819);
    expect(parsed.forcedMaximumLifecycle.referenceFieldBytes).toBe(32_763);
    expect(parsed.forcedMaximumLifecycle.observedCursors).toStrictEqual([
      1, 129, 257, 385, 513, 641, 769, 820,
    ]);
    for (const row of [
      parsed.acceptedLifecycle,
      parsed.forcedMaximumLifecycle,
    ]) {
      expect(row.maxSignedBytes).toBeLessThanOrEqual(16_384);
      expect(BigInt(row.maxMemory)).toBeLessThanOrEqual(16_500_000n);
      expect(BigInt(row.maxCpu)).toBeLessThanOrEqual(10_000_000_000n);
    }
    const { ledgerDigest, ...body } = parsed;
    expect(
      createHash("sha256").update(JSON.stringify(body)).digest("hex"),
    ).toBe(ledgerDigest);
  });
});
