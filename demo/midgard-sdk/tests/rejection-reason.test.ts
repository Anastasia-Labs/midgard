import {
  encodeMidgardRejectionReasonV1Cbor,
  MIDGARD_REJECTION_REASON_V1_ARMS,
  type MidgardRejectionReasonV1,
} from "@al-ft/midgard-core/validation-trace";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  isRejectionReasonArm,
  RejectionReason,
  rejectionReasonFromStructural,
  RejectionReasonSchema,
  rejectionReasonV1Structural,
} from "../src/index.js";

// One value per arm: arm i's coordinates are i, i+1, ... in field order, and
// a second pass writes the interim -1 sentinel into every coordinate.
const reasonForArm = (
  armIndex: number,
  coordinate: (fieldIndex: number) => bigint,
): MidgardRejectionReasonV1 => {
  const arm = MIDGARD_REJECTION_REASON_V1_ARMS[armIndex]!;
  return (
    arm.fields.length === 0
      ? arm.name
      : {
          [arm.name]: Object.fromEntries(
            arm.fields.map((field, index) => [field, coordinate(index)]),
          ),
        }
  ) as MidgardRejectionReasonV1;
};

const everyArm = MIDGARD_REJECTION_REASON_V1_ARMS.flatMap((arm, armIndex) => [
  {
    name: arm.name,
    reason: reasonForArm(armIndex, (i) => BigInt(armIndex + i)),
  },
  { name: `${arm.name} (sentinel)`, reason: reasonForArm(armIndex, () => -1n) },
]);

type SchemaArm = {
  readonly title: string;
  readonly index: number;
  readonly fields: readonly { readonly title: string }[];
};

describe("RejectionReasonSchema against the core reason codec", () => {
  it.each(everyArm)(
    "writes $name byte-for-byte as the core codec does",
    ({ reason }) => {
      const schemaValue = rejectionReasonFromStructural(reason);
      expect(Data.to(schemaValue, RejectionReason)).toBe(
        encodeMidgardRejectionReasonV1Cbor(reason).toString("hex"),
      );
      const decoded = Data.from(
        encodeMidgardRejectionReasonV1Cbor(reason).toString("hex"),
        RejectionReason,
      );
      expect(rejectionReasonV1Structural(decoded)).toStrictEqual(reason);
    },
  );

  it("declares exactly the core arms, in order, with the same coordinates", () => {
    const schemaArms = (
      RejectionReasonSchema as unknown as {
        readonly anyOf: readonly SchemaArm[];
      }
    ).anyOf;
    expect(
      schemaArms.map((arm) => ({
        index: arm.index,
        name: arm.title,
        fields: arm.fields.map((field) => field.title),
      })),
    ).toStrictEqual(
      MIDGARD_REJECTION_REASON_V1_ARMS.map((arm, index) => ({
        index,
        name: arm.name,
        fields: [...arm.fields],
      })),
    );
  });

  it("recognises exactly the declared arm names", () => {
    for (const arm of MIDGARD_REJECTION_REASON_V1_ARMS) {
      expect(isRejectionReasonArm(arm.name)).toBe(true);
    }
    expect(isRejectionReasonArm("ForcedTxValid")).toBe(false);
    expect(isRejectionReasonArm("executionIndex")).toBe(false);
  });
});
