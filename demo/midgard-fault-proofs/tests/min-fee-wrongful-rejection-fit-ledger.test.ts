import { createHash } from "node:crypto";
import { readFile } from "node:fs/promises";

import { describe, expect, it } from "vitest";

const path = new URL(
  "../../../docs/fault-proofs/size-plans/min-fee-wrongful-rejection-v1-fit-ledger.json",
  import.meta.url,
);
describe("minFee wrongful rejection fit ledger", () => {
  it("pins all field tiers, all nine populated fields, maximum MPF depth, publications and removal within reserve", async () => {
    const ledger = JSON.parse(await readFile(path, "utf8")) as {
      category: string;
      categoryId: string;
      ledgerDigest: string;
      shapes: {
        shape: string;
        stages: {
          completeSignedBytes: number;
          executionMemory: string;
          executionSteps: string;
        }[];
      }[];
    };
    expect(ledger.category).toBe("minFee");
    expect(ledger.categoryId).toBe("00000013");
    expect(ledger.shapes.map((row) => row.shape).sort()).toEqual(
      [
        "field0-358",
        "field0-378",
        "field0-379",
        "field0-819",
        "all-nine-populated",
        "maximum-proof-64-and-certified-field",
      ].sort(),
    );
    for (const shape of ledger.shapes) {
      expect(shape.stages.length).toBeGreaterThanOrEqual(8);
      for (const row of shape.stages) {
        expect(row.completeSignedBytes).toBeLessThanOrEqual(15_872);
        expect(BigInt(row.executionMemory)).toBeLessThanOrEqual(13_200_000n);
        expect(BigInt(row.executionSteps)).toBeLessThanOrEqual(8_000_000_000n);
      }
    }
    const { ledgerDigest, ...body } = ledger;
    expect(
      createHash("sha256").update(JSON.stringify(body)).digest("hex"),
    ).toBe(ledgerDigest);
  });
});
