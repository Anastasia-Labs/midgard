import {
  buildMidgardBoundedItem,
  buildMidgardValidationMerkleMembership,
  encodeMidgardFieldPreimage,
  encodeMidgardRedeemerWitnessItem,
  hashMidgardRedeemerItemLeaf,
  hashMidgardScriptExecutionLeaf,
  hashMidgardScriptPurposeLeaf,
  type MidgardRedeemerPurpose,
} from "@al-ft/midgard-core";
import {
  acceptedVerdictSubject,
  forcedVerdictSubject,
} from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  classifyUnusedRedeemerFinding,
  prepareUnusedRedeemerEvidence,
  unusedRedeemerAccountabilityRoute,
  unusedRedeemerEvidenceCloses,
  type UnusedRedeemerFinding,
  type UnusedRedeemerSelectionOpening,
} from "../src/unused-redeemer/family-v1.js";

const txId = "11".repeat(32);
const scriptHash = "22".repeat(28);
const sourceLeaf = "33".repeat(32);
const subjectHex = "aa";
const purposeKinds = { Spend: 0, Mint: 1, Reward: 2, Receive: 3 } as const;

const item = (purpose: MidgardRedeemerPurpose, pointer = 0n) => ({
  purpose,
  index: pointer,
  redeemerCbor: Buffer.from("01", "hex"),
  executionUnits: { memory: 1n, steps: 1n },
});
const preimage = (purpose: MidgardRedeemerPurpose, pointer = 0n) =>
  encodeMidgardFieldPreimage([
    encodeMidgardRedeemerWitnessItem(item(purpose, pointer)),
  ]);
const targetLeaf = (purpose: MidgardRedeemerPurpose, pointer = 0n) => {
  const bytes = encodeMidgardRedeemerWitnessItem(item(purpose, pointer));
  return hashMidgardRedeemerItemLeaf({
    redeemerIndex: 0,
    itemCommitment: buildMidgardBoundedItem({
      fieldIndex: 8,
      itemIndex: 0,
      bytes,
    }).commitment,
  });
};
const selection = ({
  purpose,
  pointer = 0,
  selected,
}: {
  purpose: keyof typeof purposeKinds;
  pointer?: number;
  selected: boolean;
}): UnusedRedeemerSelectionOpening => {
  const purposeKind = purposeKinds[purpose];
  const purposeLeaf = hashMidgardScriptPurposeLeaf({
    purposeKind,
    purposeIndex: BigInt(pointer),
    scriptHash: Buffer.from(scriptHash, "hex"),
    subject: Buffer.from(subjectHex, "hex"),
  });
  const redeemerLeaf = selected
    ? targetLeaf(purpose, BigInt(pointer))
    : Buffer.alloc(0);
  const executionLeaf = hashMidgardScriptExecutionLeaf({
    languageTag: selected ? 3 : 0,
    purposeLeaf,
    sourceLeaf: Buffer.from(sourceLeaf, "hex"),
    redeemerLeaf,
  });
  return {
    frontierIndex: 0,
    purposeKind,
    purposeIndex: pointer,
    scriptHashHex: scriptHash,
    purposeSubjectHex: subjectHex,
    purposeMembership: buildMidgardValidationMerkleMembership([purposeLeaf], 0),
    languageTag: selected ? 3 : 0,
    sourceLeafHex: sourceLeaf,
    redeemerLeafHex: redeemerLeaf.toString("hex"),
    executionMembership: buildMidgardValidationMerkleMembership(
      [executionLeaf],
      0,
    ),
  };
};
const finding = (forced: boolean): UnusedRedeemerFinding => ({
  subject: forced
    ? forcedVerdictSubject({
        transactionId: txId,
        sourceKey: { transactionId: "44".repeat(32), outputIndex: 0n },
        rejectionReason: { UnusedRedeemer: { redeemer_index: 0n } },
      })
    : acceptedVerdictSubject(txId),
  redeemerIndex: 0,
});
const prepare = (
  purpose: keyof typeof purposeKinds,
  forced: boolean,
  selected: boolean,
) =>
  prepareUnusedRedeemerEvidence({
    finding: finding(forced),
    fieldPreimage: preimage(purpose),
    universe: {
      schemaVersion: "midgard-committed-redeemer-universe-v1",
      transactionId: txId,
      universeDigest: "55".repeat(32),
      selections: [selection({ purpose, selected })],
    },
  });

describe("unusedRedeemer V1", () => {
  it("proves accepted unused and forced used directions", () => {
    const unused = prepare("Spend", false, false);
    const used = prepare("Spend", true, true);
    expect(unused.unused).toBe(true);
    expect(used.unused).toBe(false);
    expect(unusedRedeemerEvidenceCloses(unused)).toBe(true);
    expect(unusedRedeemerEvidenceCloses(used)).toBe(true);
  });

  it.each(["Spend", "Mint", "Reward", "Receive"] as const)(
    "reverse matches %s selections",
    (purpose) => {
      expect(prepare(purpose, true, true).matchedSelectionIndex).toBe(0);
    },
  );

  it("refuses another reason and coordinate", () => {
    expect(() =>
      classifyUnusedRedeemerFinding({ ...finding(true), redeemerIndex: 1 }),
    ).toThrow(/reason|coordinate/u);
  });

  it("refuses substituted execution membership", () => {
    const opening = selection({ purpose: "Spend", selected: false });
    expect(() =>
      prepareUnusedRedeemerEvidence({
        finding: finding(false),
        fieldPreimage: preimage("Spend"),
        universe: {
          schemaVersion: "midgard-committed-redeemer-universe-v1",
          transactionId: txId,
          universeDigest: "55".repeat(32),
          selections: [{ ...opening, sourceLeafHex: "66".repeat(32) }],
        },
      }),
    ).toThrow(/frontier changed/u);
  });

  it("routes a fabricated committed frontier to trace invalidity", () => {
    expect(
      unusedRedeemerAccountabilityRoute({
        committedFrontierIsCanonical: false,
        evidence: prepare("Spend", false, false),
      }),
    ).toBe("validationTraceInvalid");
  });
});
