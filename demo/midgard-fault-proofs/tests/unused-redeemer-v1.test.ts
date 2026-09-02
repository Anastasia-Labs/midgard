import {
  buildMidgardBoundedItemV1,
  buildMidgardValidationMerkleMembershipV1,
  encodeMidgardFieldPreimageV1,
  encodeMidgardRedeemerWitnessItemV1,
  hashMidgardRedeemerItemLeafV1,
  hashMidgardScriptExecutionLeafV1,
  hashMidgardScriptPurposeLeafV1,
  type MidgardRedeemerPurposeV1,
} from "@al-ft/midgard-core";
import {
  acceptedVerdictSubjectV1,
  forcedVerdictSubjectV1,
} from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  classifyUnusedRedeemerFindingV1,
  prepareUnusedRedeemerEvidenceV1,
  unusedRedeemerAccountabilityRouteV1,
  unusedRedeemerEvidenceClosesV1,
  type UnusedRedeemerFindingV1,
  type UnusedRedeemerSelectionOpeningV1,
} from "../src/unused-redeemer/family-v1.js";

const txId = "11".repeat(32);
const scriptHash = "22".repeat(28);
const sourceLeaf = "33".repeat(32);
const subjectHex = "aa";
const purposeKinds = { Spend: 0, Mint: 1, Reward: 2, Receive: 3 } as const;

const item = (purpose: MidgardRedeemerPurposeV1, pointer = 0n) => ({
  purpose,
  index: pointer,
  redeemerCbor: Buffer.from("01", "hex"),
  executionUnits: { memory: 1n, steps: 1n },
});
const preimage = (purpose: MidgardRedeemerPurposeV1, pointer = 0n) =>
  encodeMidgardFieldPreimageV1([
    encodeMidgardRedeemerWitnessItemV1(item(purpose, pointer)),
  ]);
const targetLeaf = (purpose: MidgardRedeemerPurposeV1, pointer = 0n) => {
  const bytes = encodeMidgardRedeemerWitnessItemV1(item(purpose, pointer));
  return hashMidgardRedeemerItemLeafV1({
    redeemerIndex: 0,
    itemCommitment: buildMidgardBoundedItemV1({
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
}): UnusedRedeemerSelectionOpeningV1 => {
  const purposeKind = purposeKinds[purpose];
  const purposeLeaf = hashMidgardScriptPurposeLeafV1({
    purposeKind,
    purposeIndex: BigInt(pointer),
    scriptHash: Buffer.from(scriptHash, "hex"),
    subject: Buffer.from(subjectHex, "hex"),
  });
  const redeemerLeaf = selected
    ? targetLeaf(purpose, BigInt(pointer))
    : Buffer.alloc(0);
  const executionLeaf = hashMidgardScriptExecutionLeafV1({
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
    purposeMembership: buildMidgardValidationMerkleMembershipV1(
      [purposeLeaf],
      0,
    ),
    languageTag: selected ? 3 : 0,
    sourceLeafHex: sourceLeaf,
    redeemerLeafHex: redeemerLeaf.toString("hex"),
    executionMembership: buildMidgardValidationMerkleMembershipV1(
      [executionLeaf],
      0,
    ),
  };
};
const finding = (forced: boolean): UnusedRedeemerFindingV1 => ({
  subject: forced
    ? forcedVerdictSubjectV1({
        transactionId: txId,
        sourceKey: { transactionId: "44".repeat(32), outputIndex: 0n },
        rejectionReason: { UnusedRedeemer: { redeemer_index: 0n } },
      })
    : acceptedVerdictSubjectV1(txId),
  redeemerIndex: 0,
});
const prepare = (
  purpose: keyof typeof purposeKinds,
  forced: boolean,
  selected: boolean,
) =>
  prepareUnusedRedeemerEvidenceV1({
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
    expect(unusedRedeemerEvidenceClosesV1(unused)).toBe(true);
    expect(unusedRedeemerEvidenceClosesV1(used)).toBe(true);
  });

  it.each(["Spend", "Mint", "Reward", "Receive"] as const)(
    "reverse matches %s selections",
    (purpose) => {
      expect(prepare(purpose, true, true).matchedSelectionIndex).toBe(0);
    },
  );

  it("refuses another reason and coordinate", () => {
    expect(() =>
      classifyUnusedRedeemerFindingV1({ ...finding(true), redeemerIndex: 1 }),
    ).toThrow(/reason|coordinate/u);
  });

  it("refuses substituted execution membership", () => {
    const opening = selection({ purpose: "Spend", selected: false });
    expect(() =>
      prepareUnusedRedeemerEvidenceV1({
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
      unusedRedeemerAccountabilityRouteV1({
        committedFrontierIsCanonical: false,
        evidence: prepare("Spend", false, false),
      }),
    ).toBe("validationTraceInvalid");
  });
});
