import { midgardFieldCommitment } from "@al-ft/midgard-core";
import {
  acceptedVerdictSubject,
  forcedVerdictSubject,
} from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import { prepareMintItemEvidence } from "../src/mint-item-non-canonical/index.js";
import {
  canonicalMintItems,
  malformedMintItems,
  maximumCanonicalMintItem,
  mintField,
  mintItem,
} from "./support/mint-item-vectors.js";
const evidence = (items: Buffer[], itemIndex = 0) => {
  const fieldPreimage = mintField(...items);
  return prepareMintItemEvidence({
    finding: {
      subject: acceptedVerdictSubject("aa".repeat(32)),
      fieldIndex: 5,
      itemIndex,
    },
    fieldPreimage,
    committedFieldHashHex:
      midgardFieldCommitment(fieldPreimage).toString("hex"),
  });
};
describe("mintItemNonCanonical semantic ownership", () => {
  it.each(malformedMintItems)("proves %s", (_name, hex) =>
    expect(evidence([Buffer.from(hex, "hex")]).canonical).toBe(false),
  );
  it.each(canonicalMintItems.map((item, i) => [i, item] as const))(
    "allows canonical mint/burn %i",
    (_i, item) => expect(evidence([item]).canonical).toBe(true),
  );
  it("owns duplicate and descending policy keys, accepts ascending keys", () => {
    expect(evidence([mintItem(), mintItem()], 1).canonical).toBe(false);
    expect(evidence([mintItem("a14001", "22"), mintItem()], 1).canonical).toBe(
      false,
    );
    expect(evidence([mintItem(), mintItem("a14001", "22")], 1).canonical).toBe(
      true,
    );
  });
  it("leaves envelope, field bound and empty-item faults with their owners", () => {
    expect(() => evidence([Buffer.alloc(0)])).toThrow(/fieldItemWidthIllegal/);
    expect(() => evidence([Buffer.alloc(32765)])).toThrow(
      /committedFieldShape/,
    );
    expect(() => evidence([], 0)).toThrow(/outside/);
  });
  it("scans the maximum field across chunk boundaries with bounded progress", () => {
    const item = maximumCanonicalMintItem();
    expect(item.length).toBe(32764);
    const result = evidence([item]);
    expect(result.canonical).toBe(true);
    expect(result.carriage).toBe("Certified");
    expect(result.scanControls.length).toBeGreaterThan(30);
    const last = Buffer.from(item);
    last[last.length - 1] = 0;
    expect(evidence([last]).canonical).toBe(false);
  });
  it("does not reinterpret another family's forced rejection", () => {
    const field = mintField(mintItem());
    expect(() =>
      prepareMintItemEvidence({
        finding: {
          subject: forcedVerdictSubject({
            transactionId: "aa".repeat(32),
            sourceKey: { transactionId: "bb".repeat(32), outputIndex: 0n },
            rejectionReason: { MintDeclaredAssetLimit: { policy_index: 0n } },
          }),
          fieldIndex: 5,
          itemIndex: 0,
        },
        fieldPreimage: field,
        committedFieldHashHex: midgardFieldCommitment(field).toString("hex"),
      }),
    ).toThrow(/accepted/);
  });
});
