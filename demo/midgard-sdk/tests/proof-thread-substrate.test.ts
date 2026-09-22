import { describe, expect, it } from "vitest";

import {
  acceptedVerdictSubject,
  bindExactVerdictSubjectReason,
  encodeVerdictSubject,
  forcedVerdictSubject,
  terminalVerdictContradiction,
  verdictSubjectIsCanonical,
} from "@/fraud-proof/proof-thread-substrate.js";

const TX_ID =
  "d4eafc0f14a743a05c3e5cc1aa36f26b420ec12a8a3c4473c5a458cd04c11f23";
const SOURCE_KEY = { transactionId: "77".repeat(32), outputIndex: 0n } as const;
const SOURCE_KEY_CBOR = `d8799f5820${"77".repeat(32)}00ff`;

describe("proof-thread substrate v1 TypeScript twin", () => {
  it("encodes accepted and forced subjects with the exact definite wire", () => {
    expect(
      encodeVerdictSubject(acceptedVerdictSubject(TX_ID)).toString("hex"),
    ).toBe(`860100005820${TX_ID}4080`);
    expect(
      encodeVerdictSubject(
        forcedVerdictSubject({
          transactionId: TX_ID,
          sourceKey: SOURCE_KEY,
          rejectionReason: null,
        }),
      ).toString("hex"),
    ).toBe(`860100015820${TX_ID}5827${SOURCE_KEY_CBOR}80`);
  });

  it("embeds the exact typed reason and all coordinates as Plutus Data", () => {
    const reason = {
      InputNotFound: { source_kind: 1n, input_index: 2n },
    } as const;
    const subject = forcedVerdictSubject({
      transactionId: TX_ID,
      sourceKey: SOURCE_KEY,
      rejectionReason: reason,
    });
    expect(verdictSubjectIsCanonical(subject)).toBe(true);
    expect(encodeVerdictSubject(subject).toString("hex")).toBe(
      `860101015820${TX_ID}5827${SOURCE_KEY_CBOR}81d9050b9f0102ff`,
    );
    expect(bindExactVerdictSubjectReason(subject, reason)).toStrictEqual(
      reason,
    );
    expect(() =>
      bindExactVerdictSubjectReason(subject, {
        InputNotFound: { source_kind: 1n, input_index: 3n },
      }),
    ).toThrow(/coordinate differs/);
  });

  it("enforces direction polarity and rejects non-canonical cross-fields", () => {
    const accepted = acceptedVerdictSubject(TX_ID);
    const rejected = forcedVerdictSubject({
      transactionId: TX_ID,
      sourceKey: SOURCE_KEY,
      rejectionReason: "EmptyInputs",
    });
    expect(terminalVerdictContradiction(accepted, true)).toBe(true);
    expect(terminalVerdictContradiction(accepted, false)).toBe(false);
    expect(terminalVerdictContradiction(rejected, false)).toBe(true);
    expect(terminalVerdictContradiction(rejected, true)).toBe(false);
    expect(verdictSubjectIsCanonical({ ...accepted, source_key: "00" })).toBe(
      false,
    );
  });
});
