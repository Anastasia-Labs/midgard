import { readFile } from "node:fs/promises";

import { describe, expect, it } from "vitest";

describe("native-script-invalid forced fit ledger", () => {
  it("records all bounded field, signer, depth and MPF paths through removal", async () => {
    const ledger = JSON.parse(
      await readFile(
        new URL(
          "../../../docs/fault-proofs/size-plans/native-script-invalid-wrongful-rejection-v1-fit-ledger.json",
          import.meta.url,
        ),
        "utf8",
      ),
    );
    expect(ledger.compiler).toBe("aiken v1.1.23+5adf783");
    expect(ledger.environment).toBe("testnet");
    expect(ledger.blueprintSha256).toMatch(/^[0-9a-f]{64}$/u);
    expect(
      ledger.shapes.map((shape: { shape: string }) => shape.shape),
    ).toEqual([
      "direct-28-signers",
      "staged-29-signers",
      "last-raw-field",
      "first-certified-field",
      "maximum-fields-and-64-branch-source",
      "maximum-native-depth",
      "cancel-and-restart-grammar",
      "cancel-and-restart-signer",
    ]);
    const rows = ledger.shapes.flatMap(
      (shape: { stages: unknown[] }) => shape.stages,
    );
    expect(rows.length).toBeGreaterThan(200);
    for (const row of rows) {
      expect(row.completeSignedBytes).toBeLessThanOrEqual(15872);
      expect(BigInt(row.executionMemory)).toBeLessThanOrEqual(13200000n);
      expect(BigInt(row.executionSteps)).toBeLessThanOrEqual(8000000000n);
    }
  });
});
