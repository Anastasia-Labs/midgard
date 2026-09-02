import { createHash } from "node:crypto";
import { readFile } from "node:fs/promises";

import { describe, expect, it } from "vitest";

const ledgerPath = new URL(
  "../../../docs/fault-proofs/size-plans/observers-forbidden-on-untagged-network-v1-fit-ledger.json",
  import.meta.url,
);

describe("observersForbiddenOnUntaggedNetwork fit ledger", () => {
  it("reproduces the frozen ledger digest and positive fit margins", async () => {
    const parsed = JSON.parse(await readFile(ledgerPath, "utf8")) as {
      schemaVersion: string;
      categoryId: string;
      maximumShape: { observerCount: number; fieldPreimageBytes: number };
      referencePublications: readonly { reserveMarginBytes: number }[];
      carriage: readonly { ledgerByteMargin: number }[];
      lifecycle: readonly { ledgerByteMargin: number }[];
      ledgerDigest: string;
    };
    expect(parsed.schemaVersion).toBe(
      "midgard-observers-forbidden-on-untagged-network-fit-ledger-v1",
    );
    expect(parsed.categoryId).toBe("00000024");
    expect(parsed.maximumShape).toEqual({
      observerCount: 505,
      fieldPreimageBytes: 15_153,
      carriage: "Certified",
    });
    expect(
      parsed.referencePublications.every((row) => row.reserveMarginBytes >= 0),
    ).toBe(true);
    expect(parsed.carriage.every((row) => row.ledgerByteMargin > 0)).toBe(true);
    expect(parsed.lifecycle.every((row) => row.ledgerByteMargin > 0)).toBe(
      true,
    );
    const { ledgerDigest, ...body } = parsed;
    expect(
      createHash("sha256").update(JSON.stringify(body)).digest("hex"),
    ).toBe(ledgerDigest);
  });
});
