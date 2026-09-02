import { createHash } from "node:crypto";
import { readFile } from "node:fs/promises";

import { describe, expect, it } from "vitest";

const path = new URL(
  "../../../docs/fault-proofs/size-plans/receive-purpose-language-v1-fit-ledger.json",
  import.meta.url,
);
describe("receivePurposeLanguage signed max-shape fit ledger", () => {
  it("pins the testnet blueprint and retains positive publication/lifecycle/ExUnit margins", async () => {
    const ledger = JSON.parse(await readFile(path, "utf8")) as Record<
      string,
      any
    >;
    expect(ledger.categoryId).toBe("00000034");
    expect(
      ledger.referencePublications.map((row: any) => row.signedBytes),
    ).toEqual([14975, 15736, 2253]);
    expect(
      ledger.referencePublications.every(
        (row: any) => row.reserveMarginBytes >= 0,
      ),
    ).toBe(true);
    for (const name of [
      "forcedLifecycleUpperBounds",
      "acceptedLifecycleUpperBounds",
    ])
      expect(
        ledger[name].every(
          (row: any) => row.byteMargin > 0 && row.memory > 0 && row.cpu > 0,
        ),
      ).toBe(true);
    expect(ledger.focusedAikenMaximum.memoryMargin).toBeGreaterThan(0);
    expect(ledger.focusedAikenMaximum.cpuMargin).toBeGreaterThan(0);
    const { evidenceDigest, ...body } = ledger;
    expect(
      createHash("sha256").update(JSON.stringify(body)).digest("hex"),
    ).toBe(evidenceDigest);
  });
});
