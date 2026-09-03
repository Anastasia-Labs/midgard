import { encodeMidgardNativeTxProofFieldLengths } from "@al-ft/midgard-core";
import { describe, expect, it } from "vitest";

import {
  FIELD_PREIMAGE_LENGTH_PHYSICAL_SCRIPTS,
  type FieldPreimageLengthJournal,
  nextFieldPreimageLengthAction,
  prepareFieldPreimageLengthWorkflow,
  reconcileFieldPreimageLengthJournal,
  runFieldPreimageLengthWorkflow,
} from "../src/field-preimage-length-mismatch/workflow-v1.js";

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

const emptyJournal = (): FieldPreimageLengthJournal => ({
  prepared,
  confirmed: [],
  transactionIds: {},
});

describe("field-preimage-length durable workflow", () => {
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

  it("restarts from a persisted transaction identity without resubmitting", async () => {
    let journal = reconcileFieldPreimageLengthJournal({
      journal: emptyJournal(),
      action: "init",
      transactionId: "33".repeat(32),
      confirmedOnChain: false,
    });
    let submits = 0;
    journal = await runFieldPreimageLengthWorkflow({
      load: async () => journal,
      save: async (next) => {
        journal = next;
      },
      submit: async () => {
        submits += 1;
        return "44".repeat(32);
      },
      observeConfirmed: async () => true,
    });
    expect(submits).toBe(4);
    expect(nextFieldPreimageLengthAction(journal)).toBe("complete");
  });

  it("refuses transaction identity mutation during reconciliation", () => {
    const journal = reconcileFieldPreimageLengthJournal({
      journal: emptyJournal(),
      action: "init",
      transactionId: "33".repeat(32),
      confirmedOnChain: false,
    });
    expect(() =>
      reconcileFieldPreimageLengthJournal({
        journal,
        action: "init",
        transactionId: "44".repeat(32),
        confirmedOnChain: true,
      }),
    ).toThrow(/identity changed/u);
  });
});
