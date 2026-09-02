import {
  encodeMidgardFieldPreimageV1,
  midgardFieldCommitmentV1,
} from "@al-ft/midgard-core";
import {
  acceptedVerdictSubjectV1,
  forcedVerdictSubjectV1,
} from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  classifyTransactionOutputFindingV1,
  prepareTransactionOutputEvidenceV1,
  transactionOutputEvidenceClosesV1,
} from "../src/transaction-output-non-canonical/transaction-output-non-canonical-v1.js";

const txId = "00".repeat(32);
const canonical = Buffer.from(
  "a200581d601111111111111111111111111111111111111111111111111111111101821a004c4b40a0",
  "hex",
);
const malformed = Buffer.from(
  "b80200581d601111111111111111111111111111111111111111111111111111111101821a004c4b40a0",
  "hex",
);
const accepted = acceptedVerdictSubjectV1(txId);
const forced = (index: number) =>
  forcedVerdictSubjectV1({
    transactionId: txId,
    sourceKey: { transactionId: "11".repeat(32), outputIndex: 0n },
    rejectionReason: { OutputNonCanonical: { output_index: BigInt(index) } },
  });
const evidence = (
  subject: typeof accepted,
  item: Buffer,
  extra: readonly Buffer[] = [],
) => {
  const fieldPreimage = encodeMidgardFieldPreimageV1([item, ...extra]);
  return prepareTransactionOutputEvidenceV1({
    finding: { subject, fieldIndex: 2, itemIndex: 0 },
    fieldPreimage,
    committedFieldHashHex:
      midgardFieldCommitmentV1(fieldPreimage).toString("hex"),
  });
};

describe("transactionOutputNonCanonical V1", () => {
  it("convicts both verdict polarities and refuses both honest polarities", () => {
    expect(
      transactionOutputEvidenceClosesV1(evidence(accepted, malformed)),
    ).toBe(true);
    expect(
      transactionOutputEvidenceClosesV1(evidence(forced(0), canonical)),
    ).toBe(true);
    expect(
      transactionOutputEvidenceClosesV1(evidence(accepted, canonical)),
    ).toBe(false);
    expect(
      transactionOutputEvidenceClosesV1(evidence(forced(0), malformed)),
    ).toBe(false);
  });

  it("binds the exact OutputNonCanonical coordinate", () => {
    expect(() =>
      classifyTransactionOutputFindingV1({
        subject: forced(1),
        fieldIndex: 2,
        itemIndex: 0,
      }),
    ).toThrow(/coordinate differs/u);
    const wrong = forcedVerdictSubjectV1({
      transactionId: txId,
      sourceKey: { transactionId: "11".repeat(32), outputIndex: 0n },
      rejectionReason: { FieldPreimageLengthMismatch: { field_index: 2n } },
    });
    expect(() =>
      classifyTransactionOutputFindingV1({
        subject: wrong,
        fieldIndex: 2,
        itemIndex: 0,
      }),
    ).toThrow(/not OutputNonCanonical/u);
  });

  it("refuses substituted commitments and the 16,385-byte adjacent family boundary", () => {
    const fieldPreimage = encodeMidgardFieldPreimageV1([canonical]);
    expect(() =>
      prepareTransactionOutputEvidenceV1({
        finding: { subject: accepted, fieldIndex: 2, itemIndex: 0 },
        fieldPreimage,
        committedFieldHashHex: "ff".repeat(32),
      }),
    ).toThrow(/commitment differs/u);
    const over = encodeMidgardFieldPreimageV1([Buffer.alloc(16_385)]);
    expect(() =>
      prepareTransactionOutputEvidenceV1({
        finding: { subject: accepted, fieldIndex: 2, itemIndex: 0 },
        fieldPreimage: over,
        committedFieldHashHex: midgardFieldCommitmentV1(over).toString("hex"),
      }),
    ).toThrow(/fieldItemWidthIllegal/u);
    const maximumField = encodeMidgardFieldPreimageV1([
      Buffer.alloc(16_384),
      Buffer.alloc(16_377),
    ]);
    expect(maximumField).toHaveLength(32_768);
    expect(
      prepareTransactionOutputEvidenceV1({
        finding: { subject: accepted, fieldIndex: 2, itemIndex: 0 },
        fieldPreimage: maximumField,
        committedFieldHashHex:
          midgardFieldCommitmentV1(maximumField).toString("hex"),
      }).carriage,
    ).toBe("Certified");
    const oversizedField = encodeMidgardFieldPreimageV1([
      Buffer.alloc(16_384),
      Buffer.alloc(16_378),
    ]);
    expect(oversizedField).toHaveLength(32_769);
    expect(() =>
      prepareTransactionOutputEvidenceV1({
        finding: { subject: accepted, fieldIndex: 2, itemIndex: 0 },
        fieldPreimage: oversizedField,
        committedFieldHashHex:
          midgardFieldCommitmentV1(oversizedField).toString("hex"),
      }),
    ).toThrow();
  });

  it("derives inline, RawUtxo, and Certified carriage from authenticated bytes", () => {
    expect(evidence(accepted, malformed).carriage).toBe("Inline");
    expect(evidence(accepted, malformed, [Buffer.alloc(14_300)]).carriage).toBe(
      "RawUtxo",
    );
    const certified = evidence(accepted, malformed, [
      Buffer.alloc(16_384),
      Buffer.alloc(16_300),
    ]);
    expect(certified.fieldPreimageHex.length / 2).toBeLessThanOrEqual(32_768);
    expect(certified.carriage).toBe("Certified");
  });

  it("produces a strict-progress trace and digest-bound output identity", () => {
    const prepared = evidence(accepted, canonical);
    expect(prepared.canonical).toBe(true);
    expect(prepared.scanControls.length).toBeGreaterThan(1);
    expect(prepared.scanControls.at(-1)?.stage).toBe(7);
    expect(prepared.itemHash).toMatch(/^[0-9a-f]{64}$/u);
  });
});
