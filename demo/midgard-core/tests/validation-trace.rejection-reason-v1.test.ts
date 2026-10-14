import { readFileSync } from "node:fs";

import { describe, expect, it } from "vitest";

import {
  decodeMidgardRejectionReasonV1Cbor,
  encodeMidgardRejectionReasonV1Cbor,
  hashMidgardValidationRejectionReasonV1,
  MIDGARD_REJECTION_REASON_V1_ARMS,
  type MidgardRejectionReasonV1,
} from "../src/index.js";

type ReasonVector = {
  readonly arm: number;
  readonly name: string;
  readonly fields: Readonly<Record<string, number>>;
  readonly cbor: string;
  readonly hash: string;
};

// Generated independently of this codec (a stand-alone Plutus Data encoder)
// and pinned by the on-chain `rejection_reason_v1` vector tests.
const vectors = JSON.parse(
  readFileSync(
    new URL("./fixtures/rejection-reason-v1.vectors.json", import.meta.url),
    "utf8",
  ),
) as {
  readonly domain: string;
  readonly arms: readonly ReasonVector[];
  readonly extra: readonly ReasonVector[];
};

const reasonOf = (vector: ReasonVector): MidgardRejectionReasonV1 => {
  const entries = Object.entries(vector.fields);
  return (
    entries.length === 0
      ? vector.name
      : {
          [vector.name]: Object.fromEntries(
            entries.map(([field, value]) => [field, BigInt(value)]),
          ),
        }
  ) as MidgardRejectionReasonV1;
};

describe("RejectionReasonV1 codec", () => {
  it("names every on-chain arm in declaration order", () => {
    expect(MIDGARD_REJECTION_REASON_V1_ARMS.map((arm) => arm.name)).toEqual(
      vectors.arms.map((vector) => vector.name),
    );
    expect(
      MIDGARD_REJECTION_REASON_V1_ARMS.map((arm) => [...arm.fields]),
    ).toEqual(vectors.arms.map((vector) => Object.keys(vector.fields)));
  });

  it.each([...vectors.arms, ...vectors.extra])(
    "encodes and hashes arm $arm ($name) exactly",
    (vector) => {
      const reason = reasonOf(vector);
      expect(encodeMidgardRejectionReasonV1Cbor(reason).toString("hex")).toBe(
        vector.cbor,
      );
      expect(
        hashMidgardValidationRejectionReasonV1(reason).toString("hex"),
      ).toBe(vector.hash);
    },
  );

  it("separates two coordinates of the same arm", () => {
    const first = hashMidgardValidationRejectionReasonV1({
      PlutusExecutionFailed: { purpose_kind: 0n, purpose_index: 1n },
    });
    const second = hashMidgardValidationRejectionReasonV1({
      PlutusExecutionFailed: { purpose_kind: 1n, purpose_index: 0n },
    });
    expect(first.equals(second)).toBe(false);
  });

  it.each([
    ["an unknown arm", "NotAnArm"],
    ["a bare name for an arm with coordinates", "InputNotFound"],
    ["an object for a field-less arm", { EmptyInputs: {} }],
    ["a missing coordinate", { InputNotFound: { source_kind: 0n } }],
    [
      "an extra coordinate",
      { OutputBelowMinAda: { output_index: 0n, asset_index: 0n } },
    ],
    ["a renamed coordinate", { OutputBelowMinAda: { index: 0n } }],
    ["a non-bigint coordinate", { OutputBelowMinAda: { output_index: 0 } }],
    [
      "two arms at once",
      { OutputBelowMinAda: { output_index: 0n }, UnusedRedeemer: {} },
    ],
    [
      "an out-of-range coordinate",
      { OutputBelowMinAda: { output_index: 1n << 64n } },
    ],
  ])("refuses %s", (_label, reason) => {
    expect(() =>
      encodeMidgardRejectionReasonV1Cbor(reason as MidgardRejectionReasonV1),
    ).toThrow();
  });

  it.each([...vectors.arms, ...vectors.extra])(
    "decodes arm $arm ($name) back to the same reason",
    (vector) => {
      const bytes = Buffer.concat([
        Buffer.from(vector.cbor, "hex"),
        Buffer.from([0x40]),
      ]);
      const decoded = decodeMidgardRejectionReasonV1Cbor(bytes);
      expect(decoded.reason).toEqual(reasonOf(vector));
      expect(decoded.nextOffset).toBe(bytes.length - 1);
    },
  );

  it.each([
    ["a constructor past the last arm", "d905299f00ff"],
    ["a constructor between the two tag ranges", "d8809f00ff"],
    ["a missing coordinate", "d905229f00ff"],
    ["an extra coordinate", "d905229f000000ff"],
    ["coordinates on a field-less arm", "d905279f00ff"],
    ["a definite list where coordinates are owed", "d90522820000"],
    ["an indefinite empty list on a field-less arm", "d905279fff"],
    ["an unterminated coordinate list", "d905229f0000"],
    ["a non-minimal coordinate", "d905229f180000ff"],
    ["a non-integer coordinate", "d905229f0040ff"],
    ["a bare integer", "00"],
  ])("refuses to decode %s", (_label, hex) => {
    expect(() =>
      decodeMidgardRejectionReasonV1Cbor(Buffer.from(hex, "hex")),
    ).toThrow();
  });
});
