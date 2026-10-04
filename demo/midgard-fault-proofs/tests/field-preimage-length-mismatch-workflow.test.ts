import { encodeMidgardNativeTxProofFieldLengths } from "@al-ft/midgard-core";
import { describe, expect, it } from "vitest";

import {
  FIELD_PREIMAGE_LENGTH_PHYSICAL_SCRIPTS,
  prepareFieldPreimageLengthWorkflow,
} from "../src/field-preimage-length-mismatch/workflow.js";

const prepared = prepareFieldPreimageLengthWorkflow({
  headerHash: "11".repeat(28),
  transactionId: "22".repeat(32),
  direction: "wrongfulAcceptance",
  fieldIndex: 2,
  fieldPreimageLengthsCbor: encodeMidgardNativeTxProofFieldLengths([
    1, 1, 2, 1, 1, 1, 1, 1, 1,
  ]),
  fieldPreimage: Buffer.from("80", "hex"),
});

describe("field-preimage-length proof preparation", () => {
  it("pins the ordered four-script deployment topology", () => {
    expect(
      FIELD_PREIMAGE_LENGTH_PHYSICAL_SCRIPTS.map(({ role }) => role),
    ).toEqual([
      "firstStep",
      "acceptedAuthenticator",
      "forcedAuthenticator",
      "terminal",
    ]);
  });

  it("selects deterministic carriage and refuses the adjacent over-bound", () => {
    expect(prepared.carriage).toBe("Inline");
    for (const direction of [
      "wrongfulAcceptance",
      "wrongfulRejection",
    ] as const) {
      const prepare = (bytes: number) =>
        prepareFieldPreimageLengthWorkflow({
          headerHash: "11".repeat(28),
          transactionId: "22".repeat(32),
          direction,
          fieldIndex: 2,
          fieldPreimageLengthsCbor: encodeMidgardNativeTxProofFieldLengths([
            1,
            1,
            direction === "wrongfulAcceptance" ? bytes + 1 : bytes,
            1,
            1,
            1,
            1,
            1,
            1,
          ]),
          fieldPreimage: Buffer.alloc(bytes),
          ...(direction === "wrongfulRejection"
            ? {
                forcedRejectionReason: {
                  FieldPreimageLengthMismatch: { field_index: 2n },
                },
              }
            : {}),
        });
      expect(prepare(14_337).carriage).toBe("RawUtxo");
      expect(prepare(32_768).carriage).toBe("Certified");
    }
    expect(() =>
      prepareFieldPreimageLengthWorkflow({
        headerHash: "11".repeat(28),
        transactionId: "22".repeat(32),
        direction: "wrongfulAcceptance",
        fieldIndex: 2,
        fieldPreimageLengthsCbor: encodeMidgardNativeTxProofFieldLengths([
          1, 1, 1, 1, 1, 1, 1, 1, 1,
        ]),
        fieldPreimage: Buffer.alloc(32_769),
      }),
    ).toThrow(/consensus bound/u);
  });

  it("accepts only the exact forced-rejection reason and coordinate", () => {
    const common = {
      headerHash: "11".repeat(28),
      transactionId: "22".repeat(32),
      direction: "wrongfulRejection" as const,
      fieldIndex: 2,
      fieldPreimageLengthsCbor: encodeMidgardNativeTxProofFieldLengths([
        1, 1, 1, 1, 1, 1, 1, 1, 1,
      ]),
      fieldPreimage: Buffer.from("80", "hex"),
    };
    expect(
      prepareFieldPreimageLengthWorkflow({
        ...common,
        forcedRejectionReason: {
          FieldPreimageLengthMismatch: { field_index: 2n },
        },
      }).direction,
    ).toBe("wrongfulRejection");
    expect(() =>
      prepareFieldPreimageLengthWorkflow({
        ...common,
        forcedRejectionReason: "EmptyInputs",
      }),
    ).toThrow(/carry only FieldPreimageLengthMismatch/u);
    expect(() =>
      prepareFieldPreimageLengthWorkflow({
        ...common,
        forcedRejectionReason: {
          FieldPreimageLengthMismatch: { field_index: 3n },
        },
      }),
    ).toThrow(/coordinate differs/u);
  });
});
