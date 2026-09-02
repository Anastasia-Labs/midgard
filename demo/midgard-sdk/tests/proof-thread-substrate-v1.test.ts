import { describe, expect, it } from "vitest";

import {
  acceptedVerdictSubjectV1,
  bindExactVerdictSubjectReasonV1,
  encodeVerdictSubjectV1,
  forcedVerdictSubjectV1,
  terminalVerdictContradictionV1,
  verdictSubjectIsCanonicalV1,
} from "@/fraud-proof/proof-thread-substrate-v1.js";

const TX_ID =
  "d4eafc0f14a743a05c3e5cc1aa36f26b420ec12a8a3c4473c5a458cd04c11f23";
const SOURCE_KEY = { transactionId: "77".repeat(32), outputIndex: 0n } as const;
const SOURCE_KEY_CBOR = `d8799f5820${"77".repeat(32)}00ff`;

describe("proof-thread substrate v1 TypeScript twin", () => {
  it("encodes accepted and forced subjects with the exact definite wire", () => {
    expect(
      encodeVerdictSubjectV1(acceptedVerdictSubjectV1(TX_ID)).toString("hex"),
    ).toBe(`860100005820${TX_ID}4080`);
    expect(
      encodeVerdictSubjectV1(
        forcedVerdictSubjectV1({
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
    const subject = forcedVerdictSubjectV1({
      transactionId: TX_ID,
      sourceKey: SOURCE_KEY,
      rejectionReason: reason,
    });
    expect(verdictSubjectIsCanonicalV1(subject)).toBe(true);
    expect(encodeVerdictSubjectV1(subject).toString("hex")).toBe(
      `860101015820${TX_ID}5827${SOURCE_KEY_CBOR}81d9050b9f0102ff`,
    );
    expect(bindExactVerdictSubjectReasonV1(subject, reason)).toStrictEqual(
      reason,
    );
    expect(() =>
      bindExactVerdictSubjectReasonV1(subject, {
        InputNotFound: { source_kind: 1n, input_index: 3n },
      }),
    ).toThrow(/coordinate differs/);
  });

  it("enforces direction polarity and rejects non-canonical cross-fields", () => {
    const accepted = acceptedVerdictSubjectV1(TX_ID);
    const rejected = forcedVerdictSubjectV1({
      transactionId: TX_ID,
      sourceKey: SOURCE_KEY,
      rejectionReason: "EmptyInputs",
    });
    expect(terminalVerdictContradictionV1(accepted, true)).toBe(true);
    expect(terminalVerdictContradictionV1(accepted, false)).toBe(false);
    expect(terminalVerdictContradictionV1(rejected, false)).toBe(true);
    expect(terminalVerdictContradictionV1(rejected, true)).toBe(false);
    expect(verdictSubjectIsCanonicalV1({ ...accepted, source_key: "00" })).toBe(
      false,
    );
  });
});
