import {
  encodeMidgardFieldPreimageV1,
  encodeMidgardRedeemerWitnessItemV1,
  midgardFieldCommitmentV1,
} from "@al-ft/midgard-core";
import {
  acceptedVerdictSubjectV1,
  forcedVerdictSubjectV1,
} from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  classifyRedeemerCanonicityFindingV1,
  prepareRedeemerCanonicityEvidenceV1,
  redeemerCanonicityEvidenceClosesV1,
  selectCanonicalRedeemerCanonicityEvidenceV1,
} from "../src/redeemer-canonicity/family-v1.js";
import {
  reconcileRedeemerCanonicityStateV1,
  type RedeemerCanonicityDurableStateV1,
  runRedeemerCanonicityWorkflowV1,
} from "../src/redeemer-canonicity/workflow-v1.js";

const txId = "00".repeat(32);
const accepted = acceptedVerdictSubjectV1(txId);
const rejected = (index: number) =>
  forcedVerdictSubjectV1({
    transactionId: txId,
    sourceKey: { transactionId: "11".repeat(32), outputIndex: 0n },
    rejectionReason: {
      RedeemerMalformed: { redeemer_index: BigInt(index) },
    },
  });

const item = (data: string, index = 0) =>
  encodeMidgardRedeemerWitnessItemV1({
    purpose: "Spend",
    index: BigInt(index),
    redeemerCbor: Buffer.from(data, "hex"),
    executionUnits: { memory: 1n, steps: 2n },
  });

const evidence = (subject: typeof accepted, data: string, index = 0) => {
  const field = encodeMidgardFieldPreimageV1([item(data, index)]);
  return prepareRedeemerCanonicityEvidenceV1({
    finding: { subject, redeemerIndex: index },
    fieldPreimage: field,
    committedFieldHashHex: midgardFieldCommitmentV1(field).toString("hex"),
  });
};

describe("redeemerCanonicity V1", () => {
  it("proves malformed accepted and canonical rejected polarities", () => {
    const malformed = evidence(accepted, "1800");
    const canonical = evidence(rejected(0), "00");
    expect(malformed.canonical).toBe(false);
    expect(malformed.trace).toBeNull();
    expect(redeemerCanonicityEvidenceClosesV1(malformed)).toBe(true);
    expect(canonical.canonical).toBe(true);
    expect(canonical.trace?.steps.length).toBeGreaterThan(2);
    expect(redeemerCanonicityEvidenceClosesV1(canonical)).toBe(true);
  });

  it("refuses honest verdicts", () => {
    expect(redeemerCanonicityEvidenceClosesV1(evidence(accepted, "00"))).toBe(
      false,
    );
    expect(
      redeemerCanonicityEvidenceClosesV1(evidence(rejected(0), "1800")),
    ).toBe(false);
  });

  it("binds the exact reason and coordinate", () => {
    expect(() =>
      classifyRedeemerCanonicityFindingV1({
        subject: rejected(1),
        redeemerIndex: 0,
      }),
    ).toThrow(/coordinate changed/u);
    const wrong = forcedVerdictSubjectV1({
      transactionId: txId,
      sourceKey: { transactionId: "11".repeat(32), outputIndex: 0n },
      rejectionReason: {
        RedeemerMissing: { purpose_kind: 0n, purpose_index: 0n },
      },
    });
    expect(() =>
      classifyRedeemerCanonicityFindingV1({
        subject: wrong,
        redeemerIndex: 0,
      }),
    ).toThrow(/typed reason/u);
  });

  it("refuses commitment, item, and transaction substitution", () => {
    const field = encodeMidgardFieldPreimageV1([item("00")]);
    expect(() =>
      prepareRedeemerCanonicityEvidenceV1({
        finding: { subject: accepted, redeemerIndex: 0 },
        fieldPreimage: field,
        committedFieldHashHex: "ff".repeat(32),
      }),
    ).toThrow(/commitment/u);
    expect(() =>
      prepareRedeemerCanonicityEvidenceV1({
        finding: { subject: accepted, redeemerIndex: 1 },
        fieldPreimage: field,
        committedFieldHashHex: midgardFieldCommitmentV1(field).toString("hex"),
      }),
    ).toThrow(/outside/u);
  });

  it("selects the canonical lowest coordinate deterministically", () => {
    const first = evidence(accepted, "1800", 0);
    const secondField = encodeMidgardFieldPreimageV1([
      item("00", 0),
      item("1800", 1),
    ]);
    const second = prepareRedeemerCanonicityEvidenceV1({
      finding: { subject: accepted, redeemerIndex: 1 },
      fieldPreimage: secondField,
      committedFieldHashHex:
        midgardFieldCommitmentV1(secondField).toString("hex"),
    });
    expect(selectCanonicalRedeemerCanonicityEvidenceV1([second, first])).toBe(
      first,
    );
  });
});

describe("redeemerCanonicity durable workflow", () => {
  it("reconstructs each next action from authenticated chain state", async () => {
    const prepared = evidence(rejected(0), "00");
    const entries: RedeemerCanonicityDurableStateV1[] = [];
    let state: RedeemerCanonicityDurableStateV1 = {
      stage: "none",
      decodeCursor: 0,
      txHash: "00".repeat(32),
      outputReference: null,
    };
    const stages = ["step01", "step02", "step03", "proven", "removed"] as const;
    let submitted = 0;
    const result = await runRedeemerCanonicityWorkflowV1({
      evidence: prepared,
      journal: {
        load: async () => entries,
        append: async (_identity, expectedLength, next) => {
          expect(expectedLength).toBe(entries.length);
          entries.push(next);
        },
      },
      actuator: {
        observe: async () => state,
        submit: async ({ action }) => {
          expect(action).toBe(
            ["init", "bind", "decode", "finalize", "remove"][submitted],
          );
          state = {
            stage: stages[submitted]!,
            decodeCursor: submitted === 2 ? 1 : 0,
            txHash: submitted.toString(16).padStart(64, "0"),
            outputReference:
              submitted === 4 ? null : `${submitted.toString()}#0`,
          };
          submitted += 1;
          return state;
        },
      },
    });
    expect(result).toBe("removed");
    expect(entries.map((entry) => entry.stage)).toEqual(stages);
  });

  it("fails closed on checkpoint or stage regression", () => {
    const recorded: RedeemerCanonicityDurableStateV1 = {
      stage: "step02",
      decodeCursor: 2,
      txHash: "11".repeat(32),
      outputReference: "11".repeat(32) + "#0",
    };
    expect(() =>
      reconcileRedeemerCanonicityStateV1({
        journal: [recorded],
        observed: { ...recorded, decodeCursor: 1 },
      }),
    ).toThrow(/checkpoint regressed/u);
    expect(() =>
      reconcileRedeemerCanonicityStateV1({
        journal: [recorded],
        observed: { ...recorded, stage: "step01" },
      }),
    ).toThrow(/chain regressed/u);
  });
});
