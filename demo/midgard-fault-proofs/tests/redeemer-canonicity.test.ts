import {
  encodeMidgardFieldPreimage,
  encodeMidgardRedeemerWitnessItem,
  midgardFieldCommitment,
} from "@al-ft/midgard-core";
import {
  acceptedVerdictSubject,
  forcedVerdictSubject,
} from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  classifyRedeemerCanonicityFinding,
  prepareRedeemerCanonicityEvidence,
  redeemerCanonicityEvidenceCloses,
} from "../src/redeemer-canonicity/family.js";

const txId = "00".repeat(32);
const accepted = acceptedVerdictSubject(txId);
const rejected = (index: number) =>
  forcedVerdictSubject({
    transactionId: txId,
    sourceKey: { transactionId: "11".repeat(32), outputIndex: 0n },
    rejectionReason: {
      RedeemerMalformed: { redeemer_index: BigInt(index) },
    },
  });

const item = (data: string, index = 0) =>
  encodeMidgardRedeemerWitnessItem({
    purpose: "Spend",
    index: BigInt(index),
    redeemerCbor: Buffer.from(data, "hex"),
    executionUnits: { memory: 1n, steps: 2n },
  });

const evidence = (subject: typeof accepted, data: string, index = 0) => {
  const field = encodeMidgardFieldPreimage([item(data, index)]);
  return prepareRedeemerCanonicityEvidence({
    finding: { subject, redeemerIndex: BigInt(index) },
    fieldPreimage: field,
    committedFieldHashHex: midgardFieldCommitment(field).toString("hex"),
  });
};

describe("redeemerCanonicity V1", () => {
  it("proves malformed accepted and canonical rejected polarities", () => {
    const malformed = evidence(accepted, "1800");
    const canonical = evidence(rejected(0), "00");
    expect(malformed.canonical).toBe(false);
    expect(malformed.dataPlan?.canonical).toBe(false);
    expect(redeemerCanonicityEvidenceCloses(malformed)).toBe(true);
    expect(canonical.canonical).toBe(true);
    expect(canonical.dataPlan?.steps).toHaveLength(1);
    expect(redeemerCanonicityEvidenceCloses(canonical)).toBe(true);
  });

  it("refuses honest verdicts", () => {
    expect(redeemerCanonicityEvidenceCloses(evidence(accepted, "00"))).toBe(
      false,
    );
    expect(
      redeemerCanonicityEvidenceCloses(evidence(rejected(0), "1800")),
    ).toBe(false);
  });

  it("binds the exact reason and coordinate", () => {
    expect(() =>
      classifyRedeemerCanonicityFinding({
        subject: rejected(1),
        redeemerIndex: 0n,
      }),
    ).toThrow(/coordinate changed/u);
    const wrong = forcedVerdictSubject({
      transactionId: txId,
      sourceKey: { transactionId: "11".repeat(32), outputIndex: 0n },
      rejectionReason: {
        RedeemerMissing: { purpose_kind: 0n, purpose_index: 0n },
      },
    });
    expect(() =>
      classifyRedeemerCanonicityFinding({
        subject: wrong,
        redeemerIndex: 0n,
      }),
    ).toThrow(/typed reason/u);
  });

  it("refuses commitment, item, and transaction substitution", () => {
    const field = encodeMidgardFieldPreimage([item("00")]);
    expect(() =>
      prepareRedeemerCanonicityEvidence({
        finding: { subject: accepted, redeemerIndex: 0n },
        fieldPreimage: field,
        committedFieldHashHex: "ff".repeat(32),
      }),
    ).toThrow(/commitment/u);
    expect(() =>
      prepareRedeemerCanonicityEvidence({
        finding: { subject: accepted, redeemerIndex: 1n },
        fieldPreimage: field,
        committedFieldHashHex: midgardFieldCommitment(field).toString("hex"),
      }),
    ).toThrow(/outside/u);
  });
});
