import { encodeMidgardNativeTxProofFieldLengths } from "@al-ft/midgard-core";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  fieldPreimageLengthMismatchFaultHolds,
  FieldPreimageLengthState,
  fieldPreimageLengthState,
  FieldPreimageLengthStep01RedeemerSchema,
  FieldPreimageLengthStep02RedeemerSchema,
  FieldPreimageLengthStep02StateSchema,
  FieldPreimageLengthStep03RedeemerSchema,
  fieldPreimageLengthTerminalContradiction,
  prepareFieldPreimageLengthEvidence,
  requireFieldPreimageLengthSubject,
} from "../src/fraud-proof/field-preimage-length-mismatch.js";
import {
  acceptedVerdictSubject,
  forcedVerdictSubject,
} from "../src/fraud-proof/proof-thread-substrate.js";

const TX_ID = "11".repeat(32);
const SOURCE_KEY = {
  transactionId: "22".repeat(32),
  outputIndex: 0n,
};
const lengths = (at: number): Buffer =>
  encodeMidgardNativeTxProofFieldLengths([1, 1, at, 1, 1, 1, 1, 1, 1]);

const evidence = (declared: number, actual: number) =>
  prepareFieldPreimageLengthEvidence({
    transactionId: TX_ID,
    fieldIndex: 2,
    fieldPreimageLengthsCbor: lengths(declared),
    fieldPreimage: Buffer.alloc(actual, 0),
  });

describe("fieldPreimageLengthMismatch V1", () => {
  it("proves wrongful acceptance and wrongful rejection with opposite polarity", () => {
    const accepted = acceptedVerdictSubject(TX_ID);
    const rejected = forcedVerdictSubject({
      transactionId: TX_ID,
      sourceKey: SOURCE_KEY,
      rejectionReason: {
        FieldPreimageLengthMismatch: { field_index: 2n },
      },
    });
    expect(
      fieldPreimageLengthTerminalContradiction({
        subject: accepted,
        evidence: evidence(2, 1),
      }),
    ).toBe(true);
    expect(
      fieldPreimageLengthTerminalContradiction({
        subject: rejected,
        evidence: evidence(1, 1),
      }),
    ).toBe(true);
  });

  it("refuses both honest polarities", () => {
    expect(
      fieldPreimageLengthTerminalContradiction({
        subject: acceptedVerdictSubject(TX_ID),
        evidence: evidence(1, 1),
      }),
    ).toBe(false);
    expect(
      fieldPreimageLengthTerminalContradiction({
        subject: forcedVerdictSubject({
          transactionId: TX_ID,
          sourceKey: SOURCE_KEY,
          rejectionReason: {
            FieldPreimageLengthMismatch: { field_index: 2n },
          },
        }),
        evidence: evidence(2, 1),
      }),
    ).toBe(false);
  });

  it("refuses reason, coordinate, transaction, maximum+1 and malformed length mutations", () => {
    const wrongReason = forcedVerdictSubject({
      transactionId: TX_ID,
      sourceKey: SOURCE_KEY,
      rejectionReason: "EmptyInputs",
    });
    expect(() => requireFieldPreimageLengthSubject(wrongReason, 2)).toThrow(
      /another typed reason/u,
    );
    const wrongCoordinate = forcedVerdictSubject({
      transactionId: TX_ID,
      sourceKey: SOURCE_KEY,
      rejectionReason: {
        FieldPreimageLengthMismatch: { field_index: 3n },
      },
    });
    expect(() => requireFieldPreimageLengthSubject(wrongCoordinate, 2)).toThrow(
      /coordinate/u,
    );
    expect(() =>
      fieldPreimageLengthState({
        subject: acceptedVerdictSubject("33".repeat(32)),
        evidence: evidence(2, 1),
      }),
    ).toThrow(/transaction differs/u);
    expect(() =>
      fieldPreimageLengthMismatchFaultHolds({
        fieldIndex: 2,
        declaredLength: 32_768,
        actualLength: 32_769,
      }),
    ).toThrow(/consensus bound/u);
    expect(() =>
      prepareFieldPreimageLengthEvidence({
        transactionId: TX_ID,
        fieldIndex: 2,
        fieldPreimageLengthsCbor: Buffer.from("80", "hex"),
        fieldPreimage: Buffer.from("80", "hex"),
      }),
    ).toThrow();
  });

  it("decides the length mismatch before interpreting field grammar", () => {
    const malformedField = prepareFieldPreimageLengthEvidence({
      transactionId: TX_ID,
      fieldIndex: 2,
      fieldPreimageLengthsCbor: lengths(1),
      fieldPreimage: Buffer.from("9f0001ff", "hex"),
    });
    expect(malformedField.faultHolds).toBe(true);
    expect(malformedField.actualLength).toBe(4);
  });

  it("has a stable state encoding golden", () => {
    const state = fieldPreimageLengthState({
      subject: acceptedVerdictSubject(TX_ID),
      evidence: evidence(2, 1),
    });
    expect(Data.to(state, FieldPreimageLengthState)).toBe(
      `d8799fd8799f0100005820${TX_ID}40d87a80ff020201ff`,
    );
  });

  it("pins the four-script dispatch, authentication, and terminal ABI", () => {
    const dispatch = {
      Continue: [
        {
          RecordForced: { direction: 1n, input_index: 0n, output_index: 1n },
        },
      ],
    };
    const pending = { PendingForced: { direction: 1n } };
    const authenticate = {
      Continue: [
        {
          AuthenticateAccepted: {
            input_index: 0n,
            output_index: 1n,
            claim: {
              BodyFieldClaim: {
                field_index: 2n,
                carriage: { Inline: { preimage: "80" } },
              },
            },
          },
        },
      ],
    };
    const terminal = {
      Continue: [
        {
          input_index: 0n,
          output_index: 0n,
          fraud_proof_mint_redeemer_index: 0n,
        },
      ],
    };
    expect(
      Data.to(dispatch as never, FieldPreimageLengthStep01RedeemerSchema),
    ).toBe("d87a9fd87a9f010001ffff");
    expect(
      Data.to(pending as never, FieldPreimageLengthStep02StateSchema),
    ).toBe("d87a9f01ff");
    expect(
      Data.to(authenticate as never, FieldPreimageLengthStep02RedeemerSchema),
    ).toBe("d87a9fd8799f0001d8799f02d8799f4180ffffffff");
    expect(
      Data.to(terminal as never, FieldPreimageLengthStep03RedeemerSchema),
    ).toBe("d87a9fd8799f000000ffff");
  });
});
