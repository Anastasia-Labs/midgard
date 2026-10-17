import {
  encodeMidgardFieldPreimage,
  midgardFieldCommitment,
} from "@al-ft/midgard-core";
import {
  acceptedVerdictSubject,
  forcedVerdictSubject,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  encodeFieldItemWidthAuthenticatedItem,
  FieldItemWidthAuthenticatedItemSchema,
  fieldItemWidthEvidenceCloses,
  prepareFieldItemWidthEvidence,
} from "../src/field-item-width-illegal/field-item-width-illegal.js";

const txId = "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f";
const forcedSource = {
  transactionId: "00".repeat(32),
  outputIndex: 4n,
};

const accepted = acceptedVerdictSubject(txId);
describe("fieldItemWidthIllegal exact domain and fixed-item observations", () => {
  const prepare = (
    subject: typeof accepted,
    fieldIndex: bigint,
    itemIndex: bigint,
    preimage?: Buffer,
  ) =>
    preimage === undefined
      ? prepareFieldItemWidthEvidence({
          finding: { subject, fieldIndex, itemIndex },
        })
      : prepareFieldItemWidthEvidence({
          finding: { subject, fieldIndex, itemIndex },
          fieldPreimage: preimage,
          committedFieldHashHex:
            midgardFieldCommitment(preimage).toString("hex"),
        });
  const reasonAt = (fieldIndex: bigint, itemIndex: bigint) =>
    forcedVerdictSubject({
      transactionId: txId,
      sourceKey: forcedSource,
      rejectionReason: {
        FieldItemWidthIllegal: {
          field_index: fieldIndex,
          item_index: itemIndex,
        },
      },
    });

  it.each([
    [-1n, 0n],
    [4n, -1n],
    [1n << 80n, 0n],
  ])(
    "retains signed static-domain coordinates %s/%s without invented field bytes",
    (fieldIndex, itemIndex) => {
      const prepared = prepare(
        reasonAt(fieldIndex, itemIndex),
        fieldIndex,
        itemIndex,
      );
      expect(prepared.fieldIndex).toBe(fieldIndex);
      expect(prepared.itemIndex).toBe(itemIndex);
      expect(fieldItemWidthEvidenceCloses(prepared)).toBe(true);
      expect(
        Data.from(
          encodeFieldItemWidthAuthenticatedItem(prepared),
          FieldItemWidthAuthenticatedItemSchema as never,
        ),
      ).toMatchObject({
        field_index: fieldIndex,
        item_index: itemIndex,
        observation: "ItemOutOfDomain",
      });
    },
  );

  it("authenticates the real field count before refuting a huge positive item", () => {
    const itemIndex = 1n << 80n;
    const preimage = encodeMidgardFieldPreimage([Buffer.alloc(28, 3)]);
    const prepared = prepare(reasonAt(4n, itemIndex), 4n, itemIndex, preimage);
    expect(prepared.fieldPreimageHex).toBe(preimage.toString("hex"));
    expect(fieldItemWidthEvidenceCloses(prepared)).toBe(true);
    expect(
      Data.from(
        encodeFieldItemWidthAuthenticatedItem(prepared),
        FieldItemWidthAuthenticatedItemSchema as never,
      ),
    ).toMatchObject({ observation: "ItemOutOfDomain" });
  });

  it("refutes a legal fixed-field claim and preserves a malformed wrapper as evidence", () => {
    const legal = encodeMidgardFieldPreimage([Buffer.alloc(28, 3)]);
    expect(
      fieldItemWidthEvidenceCloses(prepare(reasonAt(4n, 0n), 4n, 0n, legal)),
    ).toBe(true);
    const malformed = Buffer.from(legal);
    malformed[2] = 0x1b;
    const prepared = prepare(accepted, 4n, 0n, malformed);
    expect(prepared.fieldPreimageHex).toBe(malformed.toString("hex"));
    expect(fieldItemWidthEvidenceCloses(prepared)).toBe(true);
  });

  it("uses canonical first-fault order for observed faults under other claims", () => {
    const malformed = Buffer.from("81581b" + "03".repeat(28), "hex");
    expect(
      fieldItemWidthEvidenceCloses(
        prepare(reasonAt(5n, 0n), 4n, 0n, malformed),
      ),
    ).toBe(true);
    expect(
      fieldItemWidthEvidenceCloses(
        prepare(reasonAt(0n, 0n), 4n, 0n, malformed),
      ),
    ).toBe(false);
    const other = forcedVerdictSubject({
      transactionId: txId,
      sourceKey: forcedSource,
      rejectionReason: "ValueNotPreserved",
    });
    expect(
      fieldItemWidthEvidenceCloses(prepare(other, 4n, 0n, malformed)),
    ).toBe(true);
  });
});
