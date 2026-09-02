import {
  encodeMidgardFieldPreimageV1,
  encodeMidgardMintPolicyItemV1,
  midgardFieldCommitmentV1,
} from "@al-ft/midgard-core";
import {
  acceptedVerdictSubjectV1,
  forcedVerdictSubjectV1,
} from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  classifyMintDeclaredAssetLimitFindingV1,
  decodeMintDeclaredPolicyHeaderV1,
  foldMintDeclaredAssetLimitV1,
  MINT_DECLARED_ASSET_LIMIT_CATEGORY_ID_V1,
  MINT_DECLARED_ASSET_LIMIT_CATEGORY_V1,
  mintDeclaredAssetLimitEvidenceClosesV1,
  prepareMintDeclaredAssetLimitEvidenceV1,
} from "../src/mint-declared-asset-limit/family-v1.js";
import {
  admitProductionMintDeclaredAssetLimitArtifactV1,
  buildProductionMintDeclaredAssetLimitArtifactV1,
  productionMintDeclaredAssetLimitArtifactDigestV1,
} from "../src/mint-declared-asset-limit/production-artifact-v1.js";
import {
  MINT_DECLARED_ASSET_LIMIT_VIOLATION_ID_V1,
  type MintDeclaredAssetLimitReplayDetectionV1,
  selectCanonicalMintDeclaredAssetLimitDetectionV1,
} from "../src/mint-declared-asset-limit/replay-v1.js";
import {
  encodeMintDeclaredGrammarCheckpointV1,
  encodeMintDeclaredWalkCheckpointV1,
  hashMintDeclaredGrammarCheckpointV1,
  hashMintDeclaredWalkCheckpointV1,
  planMintDeclaredAssetLimitStagedWalkV1,
} from "../src/mint-declared-asset-limit/staged-plan-v1.js";

const txId = "00".repeat(31).concat("01");
const accepted = acceptedVerdictSubjectV1(txId);
const rejected = (policyIndex: number) =>
  forcedVerdictSubjectV1({
    transactionId: txId,
    sourceKey: { transactionId: "11".repeat(32), outputIndex: 0n },
    rejectionReason: {
      MintDeclaredAssetLimit: { policy_index: BigInt(policyIndex) },
    },
  });

const singleton = (policyByte: number) =>
  encodeMidgardMintPolicyItemV1({
    policyId: Buffer.alloc(28, policyByte),
    assets: [{ assetName: Buffer.alloc(0), quantity: 1n }],
  });

const crossing = (policyByte: number, padding = 0): Buffer =>
  Buffer.concat([
    Buffer.from([0x82, 0x58, 0x1c]),
    Buffer.alloc(28, policyByte),
    Buffer.from([0xb9, 0x40, 0x01]),
    Buffer.alloc(Math.max(1, padding), 0),
  ]);

const evidence = (subject: typeof accepted, item: Buffer) => {
  const field = encodeMidgardFieldPreimageV1([item]);
  return prepareMintDeclaredAssetLimitEvidenceV1({
    finding: { subject, policyIndex: 0 },
    fieldPreimage: field,
    committedFieldHashHex: midgardFieldCommitmentV1(field).toString("hex"),
  });
};

describe("mintDeclaredAssetLimit V1 semantics", () => {
  it("freezes category identity and reads the exact pre-body header", () => {
    expect(MINT_DECLARED_ASSET_LIMIT_CATEGORY_V1).toBe(
      "mintDeclaredAssetLimit",
    );
    expect(MINT_DECLARED_ASSET_LIMIT_CATEGORY_ID_V1).toBe("0000002c");
    const header = decodeMintDeclaredPolicyHeaderV1(crossing(1));
    expect(header.policyId.toString("hex")).toBe("01".repeat(28));
    expect(header.declaredCount).toBe(16_385);
  });

  it("proves accepted first crossing before target-body decoding", () => {
    const result = evidence(accepted, crossing(1));
    expect(result.crossing).toBe(true);
    expect(result.accumulatedCount).toBe(0);
    expect(mintDeclaredAssetLimitEvidenceClosesV1(result)).toBe(true);
  });

  it("proves exact forced contradiction only after a complete target item", () => {
    const result = evidence(rejected(0), singleton(1));
    expect(result.crossing).toBe(false);
    expect(result.accumulatedCount).toBe(1);
    expect(mintDeclaredAssetLimitEvidenceClosesV1(result)).toBe(true);
  });

  it("refuses honest accepted and forced polarities", () => {
    expect(
      mintDeclaredAssetLimitEvidenceClosesV1(evidence(accepted, singleton(1))),
    ).toBe(false);
    expect(
      mintDeclaredAssetLimitEvidenceClosesV1(rejectedEvidence(crossing(1))),
    ).toBe(false);
  });

  it("binds the forced reason and policy coordinate exactly", () => {
    expect(() =>
      classifyMintDeclaredAssetLimitFindingV1({
        subject: rejected(2),
        policyIndex: 1,
      }),
    ).toThrow(/coordinate changed/u);
    const wrongReason = forcedVerdictSubjectV1({
      transactionId: txId,
      sourceKey: { transactionId: "11".repeat(32), outputIndex: 0n },
      rejectionReason: { OutputNonCanonical: { output_index: 0n } },
    });
    expect(() =>
      classifyMintDeclaredAssetLimitFindingV1({
        subject: wrongReason,
        policyIndex: 0,
      }),
    ).toThrow(/not MintDeclaredAssetLimit/u);
  });

  it("refuses commitment, target item, and first-crossing substitutions", () => {
    const field = encodeMidgardFieldPreimageV1([singleton(1)]);
    expect(() =>
      prepareMintDeclaredAssetLimitEvidenceV1({
        finding: { subject: accepted, policyIndex: 0 },
        fieldPreimage: field,
        committedFieldHashHex: "ff".repeat(32),
      }),
    ).toThrow(/do not match/u);
    expect(() =>
      prepareMintDeclaredAssetLimitEvidenceV1({
        finding: { subject: accepted, policyIndex: 1 },
        fieldPreimage: field,
        committedFieldHashHex: midgardFieldCommitmentV1(field).toString("hex"),
      }),
    ).toThrow(/outside field 5/u);
    expect(() =>
      foldMintDeclaredAssetLimitV1([crossing(1), crossing(2)], 1),
    ).toThrow(/earlier policy/u);
  });

  it("fits and classifies the exact 32,768-byte certified frontier", () => {
    const target = crossing(1, 32_730);
    expect(target.length).toBe(32_764);
    const maximum = evidence(accepted, target);
    expect(maximum.fieldPreimageHex.length / 2).toBe(32_768);
    expect(maximum.carriage).toBe("Certified");
    expect(maximum.crossing).toBe(true);
  });

  it("derives deterministic field-5 grammar and semantic restart checkpoints", () => {
    const items = Array.from({ length: 49 }, (_, index) =>
      singleton(index + 1),
    );
    const field = encodeMidgardFieldPreimageV1(items);
    const input = {
      transactionId: txId,
      fieldPreimageCbor: field.toString("hex"),
      policyIndex: 48,
    } as const;
    const first = planMintDeclaredAssetLimitStagedWalkV1(input);
    const restarted = planMintDeclaredAssetLimitStagedWalkV1(input);
    expect(first).toEqual(restarted);
    expect(first.grammar).toHaveLength(3);
    expect(first.walk).toHaveLength(3);
    const grammarBytes = encodeMintDeclaredGrammarCheckpointV1(
      first.grammar[0]!,
    );
    const walkBytes = encodeMintDeclaredWalkCheckpointV1(first.walk[0]!);
    expect(grammarBytes).toHaveLength(87);
    expect(walkBytes).toHaveLength(53);
    expect(grammarBytes[36]).toBe(5);
    expect(walkBytes[36]).toBe(5);
    expect(hashMintDeclaredGrammarCheckpointV1(first.grammar[0]!)).toMatch(
      /^[0-9a-f]{64}$/u,
    );
    expect(hashMintDeclaredWalkCheckpointV1(first.walk[0]!)).toMatch(
      /^[0-9a-f]{64}$/u,
    );
  });

  it("selects the earliest authenticated detection independent of traversal order", () => {
    const detection = (
      position: bigint,
      detectionId: string,
    ): MintDeclaredAssetLimitReplayDetectionV1 => ({
      detectionId,
      headerHash: "22".repeat(28),
      violationId: MINT_DECLARED_ASSET_LIMIT_VIOLATION_ID_V1,
      position,
      transactionId: txId,
      policyIndex: 0,
      source: "accepted",
      direction: "wrongfulAcceptance",
    });
    expect(
      selectCanonicalMintDeclaredAssetLimitDetectionV1([
        detection(9n, "z"),
        detection(2n, "b"),
        detection(2n, "a"),
      ]).detectionId,
    ).toBe("a");
  });

  it("reconstructs its staged artifact and refuses field substitution", () => {
    const prepared = evidence(accepted, crossing(1));
    const artifact = buildProductionMintDeclaredAssetLimitArtifactV1({
      headerHash: "22".repeat(28),
      detectionId: "mint-declared-asset-limit:accepted:0:test:0",
      position: 0n,
      evidence: prepared,
      sourceKind: "accepted",
      nativeTxCompactCbor: "80",
      witnessSetCompactCbor: "80",
      l2TransactionSourceCbor: "80",
      transactionsPhasRoot: "33".repeat(32),
      transactionMembershipCbor: "80",
    });
    const admitted = admitProductionMintDeclaredAssetLimitArtifactV1(artifact);
    expect(admitted.evidence).toEqual(prepared);
    expect(productionMintDeclaredAssetLimitArtifactDigestV1(artifact)).toMatch(
      /^[0-9a-f]{64}$/u,
    );
    expect(() =>
      admitProductionMintDeclaredAssetLimitArtifactV1({
        ...artifact,
        fieldPreimageCbor: `${artifact.fieldPreimageCbor.slice(0, -2)}ff`,
      }),
    ).toThrow(/commitment changed/u);
  });
});

const rejectedEvidence = (item: Buffer) => evidence(rejected(0), item);
