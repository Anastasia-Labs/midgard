import { missingSignatureVkeyHashV1 } from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  assertMissingSignatureFindingProvableV1,
  type MissingSignatureFindingV1,
  MissingSignatureProvabilityV1,
} from "../src/missing-signature/index.js";

const VKEY = "11".repeat(32);
const finding: MissingSignatureFindingV1 = {
  headerHash: "22".repeat(28),
  eventKey: { L2TransactionEventKey: { tx_id: "33".repeat(32) } },
  fraudulentBlockOutRef: `${"44".repeat(32)}#0`,
  txId: "33".repeat(32),
  nativeTxCompactCbor: "80",
  accusedRequiredSignerIndex: 0n,
  accusedRequiredSignerHash: missingSignatureVkeyHashV1(VKEY),
  resolvedVkey: VKEY,
  committedWitnessSetHash: "55".repeat(32),
  provability: MissingSignatureProvabilityV1.MissingWitness,
  estimatedThreadTxCount: 6,
};

describe("missing-signature finding v1", () => {
  it("accepts only the coherent provable classification", () => {
    expect(() =>
      assertMissingSignatureFindingProvableV1(finding),
    ).not.toThrow();
    for (const provability of [
      MissingSignatureProvabilityV1.PresentButInvalid,
      MissingSignatureProvabilityV1.UnknownVkeyPreimage,
      MissingSignatureProvabilityV1.NotAFault,
    ]) {
      expect(() =>
        assertMissingSignatureFindingProvableV1({ ...finding, provability }),
      ).toThrow(/not provable by this family/u);
    }
  });

  it("refuses a wrong vkey, negative ordinal, or malformed compact reference", () => {
    expect(() =>
      assertMissingSignatureFindingProvableV1({
        ...finding,
        resolvedVkey: "66".repeat(32),
      }),
    ).toThrow(/resolved vkey hashes/u);
    expect(() =>
      assertMissingSignatureFindingProvableV1({
        ...finding,
        accusedRequiredSignerIndex: -1n,
      }),
    ).toThrow(/negative/u);
    expect(() =>
      assertMissingSignatureFindingProvableV1({
        ...finding,
        nativeTxCompactCbor: "xyz",
      }),
    ).toThrow(/lowercase hex/u);
  });
});
