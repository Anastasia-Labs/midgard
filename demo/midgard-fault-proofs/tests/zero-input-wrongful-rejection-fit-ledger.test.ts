import { createHash } from "node:crypto";
import { readFile } from "node:fs/promises";

import { describe, expect, it } from "vitest";

const ledgerPath = new URL(
  "../../../docs/fault-proofs/size-plans/zero-input-wrongful-rejection-v1-fit-ledger.json",
  import.meta.url,
);

describe("zeroInput wrongful-rejection fit ledger", () => {
  it("reproduces its digest and every Van Rossem margin", async () => {
    const parsed = JSON.parse(await readFile(ledgerPath, "utf8")) as {
      category: string;
      categoryId: string;
      referencePublications: readonly { reserveMarginBytes: number }[];
      acceptedLifecycle: readonly {
        signedBytes: number;
        memory: string;
        cpu: string;
      }[];
      forcedLifecycle: readonly {
        signedBytes: number;
        memory: string;
        cpu: string;
      }[];
      ledgerDigest: string;
    };
    expect(parsed.category).toBe("zeroInput");
    expect(parsed.categoryId).toBe("00000005");
    expect(
      parsed.referencePublications.every((row) => row.reserveMarginBytes > 0),
    ).toBe(true);
    for (const row of [
      ...parsed.acceptedLifecycle,
      ...parsed.forcedLifecycle,
    ]) {
      expect(row.signedBytes).toBeLessThanOrEqual(16_384);
      expect(BigInt(row.memory)).toBeLessThanOrEqual(16_500_000n);
      expect(BigInt(row.cpu)).toBeLessThanOrEqual(10_000_000_000n);
    }
    const { ledgerDigest, ...body } = parsed;
    expect(
      createHash("sha256").update(JSON.stringify(body)).digest("hex"),
    ).toBe(ledgerDigest);
  });
});
