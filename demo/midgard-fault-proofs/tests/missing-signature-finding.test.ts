import { missingSignatureVkeyHash } from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  assertMissingSignatureFindingProvable,
  type MissingSignatureFinding,
  MissingSignatureProvability,
} from "../src/missing-signature/index.js";

const VKEY = "11".repeat(32);
const finding: MissingSignatureFinding = {
  headerHash: "22".repeat(28),
  eventKey: { L2TransactionEventKey: { tx_id: "33".repeat(32) } },
  fraudulentBlockOutRef: `${"44".repeat(32)}#0`,
  txId: "33".repeat(32),
  nativeTxCompactCbor: "80",
  accusedRequiredSignerIndex: 0n,
  accusedRequiredSignerHash: missingSignatureVkeyHash(VKEY),
  resolvedVkey: VKEY,
  committedWitnessSetHash: "55".repeat(32),
  provability: MissingSignatureProvability.MissingWitness,
  estimatedThreadTxCount: 6,
};

describe("missing-signature finding v1", () => {
  it("accepts only the coherent provable classification", () => {
    expect(() => assertMissingSignatureFindingProvable(finding)).not.toThrow();
    for (const provability of [
      MissingSignatureProvability.PresentButInvalid,
      MissingSignatureProvability.UnknownVkeyPreimage,
      MissingSignatureProvability.NotAFault,
    ]) {
      expect(() =>
        assertMissingSignatureFindingProvable({ ...finding, provability }),
      ).toThrow(/not provable by this family/u);
    }
  });

  it("refuses a wrong vkey, negative ordinal, or malformed compact reference", () => {
    expect(() =>
      assertMissingSignatureFindingProvable({
        ...finding,
        resolvedVkey: "66".repeat(32),
      }),
    ).toThrow(/resolved vkey hashes/u);
    expect(() =>
      assertMissingSignatureFindingProvable({
        ...finding,
        accusedRequiredSignerIndex: -1n,
      }),
    ).toThrow(/negative/u);
    expect(() =>
      assertMissingSignatureFindingProvable({
        ...finding,
        nativeTxCompactCbor: "xyz",
      }),
    ).toThrow(/lowercase hex/u);
  });
});
