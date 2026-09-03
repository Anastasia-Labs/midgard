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
  selectCanonicalRedeemerCanonicityEvidence,
} from "../src/redeemer-canonicity/family.js";
import {
  reconcileRedeemerCanonicityState,
  type RedeemerCanonicityDurableState,
  runRedeemerCanonicityWorkflow,
} from "../src/redeemer-canonicity/workflow.js";

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
    finding: { subject, redeemerIndex: index },
    fieldPreimage: field,
    committedFieldHashHex: midgardFieldCommitment(field).toString("hex"),
  });
};

describe("redeemerCanonicity V1", () => {
  it("proves malformed accepted and canonical rejected polarities", () => {
    const malformed = evidence(accepted, "1800");
    const canonical = evidence(rejected(0), "00");
    expect(malformed.canonical).toBe(false);
    expect(malformed.trace).toBeNull();
    expect(redeemerCanonicityEvidenceCloses(malformed)).toBe(true);
    expect(canonical.canonical).toBe(true);
    expect(canonical.trace?.steps.length).toBeGreaterThan(2);
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
        redeemerIndex: 0,
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
        redeemerIndex: 0,
      }),
    ).toThrow(/typed reason/u);
  });

  it("refuses commitment, item, and transaction substitution", () => {
    const field = encodeMidgardFieldPreimage([item("00")]);
    expect(() =>
      prepareRedeemerCanonicityEvidence({
        finding: { subject: accepted, redeemerIndex: 0 },
        fieldPreimage: field,
        committedFieldHashHex: "ff".repeat(32),
      }),
    ).toThrow(/commitment/u);
    expect(() =>
      prepareRedeemerCanonicityEvidence({
        finding: { subject: accepted, redeemerIndex: 1 },
        fieldPreimage: field,
        committedFieldHashHex: midgardFieldCommitment(field).toString("hex"),
      }),
    ).toThrow(/outside/u);
  });

  it("selects the canonical lowest coordinate deterministically", () => {
    const first = evidence(accepted, "1800", 0);
    const secondField = encodeMidgardFieldPreimage([
      item("00", 0),
      item("1800", 1),
    ]);
    const second = prepareRedeemerCanonicityEvidence({
      finding: { subject: accepted, redeemerIndex: 1 },
      fieldPreimage: secondField,
      committedFieldHashHex:
        midgardFieldCommitment(secondField).toString("hex"),
    });
    expect(selectCanonicalRedeemerCanonicityEvidence([second, first])).toBe(
      first,
    );
  });
});

describe("redeemerCanonicity durable workflow", () => {
  it("reconstructs each next action from authenticated chain state", async () => {
    const prepared = evidence(rejected(0), "00");
    const entries: RedeemerCanonicityDurableState[] = [];
    let state: RedeemerCanonicityDurableState = {
      stage: "none",
      decodeCursor: 0,
      txHash: "00".repeat(32),
      outputReference: null,
    };
    const stages = ["step01", "step02", "step03", "proven", "removed"] as const;
    let submitted = 0;
    const result = await runRedeemerCanonicityWorkflow({
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
    const recorded: RedeemerCanonicityDurableState = {
      stage: "step02",
      decodeCursor: 2,
      txHash: "11".repeat(32),
      outputReference: "11".repeat(32) + "#0",
    };
    expect(() =>
      reconcileRedeemerCanonicityState({
        journal: [recorded],
        observed: { ...recorded, decodeCursor: 1 },
      }),
    ).toThrow(/checkpoint regressed/u);
    expect(() =>
      reconcileRedeemerCanonicityState({
        journal: [recorded],
        observed: { ...recorded, stage: "step01" },
      }),
    ).toThrow(/chain regressed/u);
  });
});
