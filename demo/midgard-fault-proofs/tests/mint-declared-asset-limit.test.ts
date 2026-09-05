import {
  encodeMidgardFieldPreimage,
  encodeMidgardMintPolicyItem,
  midgardFieldCommitment,
} from "@al-ft/midgard-core";
import {
  acceptedVerdictSubject,
  buildMintDeclaredAssetLimitFaultProofContracts,
  forcedVerdictSubject,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  admitMintDeclaredAssetLimitArtifact,
  buildMintDeclaredAssetLimitArtifact,
  mintDeclaredAssetLimitArtifactDigest,
} from "../src/mint-declared-asset-limit/artifact.js";
import {
  applyMintDeclaredAssetLimitScripts,
  MINT_DECLARED_ASSET_LIMIT_BLUEPRINT_TITLES,
} from "../src/mint-declared-asset-limit/contracts.js";
import {
  advanceMintDeclaredFold,
  classifyMintDeclaredAssetLimitFinding,
  decodeMintDeclaredPolicyHeader,
  foldMintDeclaredAssetLimit,
  initialMintDeclaredFoldCursor,
  MINT_DECLARED_ASSET_LIMIT_CATEGORY,
  MINT_DECLARED_ASSET_LIMIT_CATEGORY_ID,
  mintDeclaredAssetLimitEvidenceCloses,
  MintDeclaredAssetLimitFoldStateSchema,
  mintDeclaredFoldStateData,
  prepareMintDeclaredAssetLimitEvidence,
} from "../src/mint-declared-asset-limit/family.js";
import {
  MINT_DECLARED_ASSET_LIMIT_VIOLATION_ID,
  type MintDeclaredAssetLimitReplayDetection,
  selectCanonicalMintDeclaredAssetLimitDetection,
} from "../src/mint-declared-asset-limit/replay.js";
import {
  encodeMintDeclaredGrammarCheckpoint,
  encodeMintDeclaredWalkCheckpoint,
  hashMintDeclaredGrammarCheckpoint,
  hashMintDeclaredWalkCheckpoint,
  planMintDeclaredAssetLimitStagedWalk,
} from "../src/mint-declared-asset-limit/staged-plan.js";
import { buildRegisteredChainFixture } from "./support/emulator/registered-chain.js";

const txId = "00".repeat(31).concat("01");
const accepted = acceptedVerdictSubject(txId);
const rejected = (policyIndex: number) =>
  forcedVerdictSubject({
    transactionId: txId,
    sourceKey: { transactionId: "11".repeat(32), outputIndex: 0n },
    rejectionReason: {
      MintDeclaredAssetLimit: { policy_index: BigInt(policyIndex) },
    },
  });

const singleton = (policyByte: number) =>
  encodeMidgardMintPolicyItem({
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
  const field = encodeMidgardFieldPreimage([item]);
  return prepareMintDeclaredAssetLimitEvidence({
    finding: { subject, policyIndex: 0 },
    fieldPreimage: field,
    committedFieldHashHex: midgardFieldCommitment(field).toString("hex"),
  });
};

describe("mintDeclaredAssetLimit V1 semantics", () => {
  it("freezes category identity and reads the exact pre-body header", () => {
    expect(MINT_DECLARED_ASSET_LIMIT_CATEGORY).toBe("mintDeclaredAssetLimit");
    expect(MINT_DECLARED_ASSET_LIMIT_CATEGORY_ID).toBe("0000002c");
    const header = decodeMintDeclaredPolicyHeader(crossing(1));
    expect(header.policyId.toString("hex")).toBe("01".repeat(28));
    expect(header.declaredCount).toBe(16_385);
  });

  it("proves accepted first crossing before target-body decoding", () => {
    const result = evidence(accepted, crossing(1));
    expect(result.crossing).toBe(true);
    expect(result.accumulatedCount).toBe(0);
    expect(mintDeclaredAssetLimitEvidenceCloses(result)).toBe(true);
  });

  it("proves exact forced contradiction only after a complete target item", () => {
    const result = evidence(rejected(0), singleton(1));
    expect(result.crossing).toBe(false);
    expect(result.accumulatedCount).toBe(1);
    expect(mintDeclaredAssetLimitEvidenceCloses(result)).toBe(true);
  });

  it("refuses honest accepted and forced polarities", () => {
    expect(
      mintDeclaredAssetLimitEvidenceCloses(evidence(accepted, singleton(1))),
    ).toBe(false);
    expect(
      mintDeclaredAssetLimitEvidenceCloses(rejectedEvidence(crossing(1))),
    ).toBe(false);
  });

  it("binds the forced reason and policy coordinate exactly", () => {
    expect(() =>
      classifyMintDeclaredAssetLimitFinding({
        subject: rejected(2),
        policyIndex: 1,
      }),
    ).toThrow(/coordinate changed/u);
    const wrongReason = forcedVerdictSubject({
      transactionId: txId,
      sourceKey: { transactionId: "11".repeat(32), outputIndex: 0n },
      rejectionReason: { OutputNonCanonical: { output_index: 0n } },
    });
    expect(() =>
      classifyMintDeclaredAssetLimitFinding({
        subject: wrongReason,
        policyIndex: 0,
      }),
    ).toThrow(/not MintDeclaredAssetLimit/u);
  });

  it("refuses commitment, target item, and first-crossing substitutions", () => {
    const field = encodeMidgardFieldPreimage([singleton(1)]);
    expect(() =>
      prepareMintDeclaredAssetLimitEvidence({
        finding: { subject: accepted, policyIndex: 0 },
        fieldPreimage: field,
        committedFieldHashHex: "ff".repeat(32),
      }),
    ).toThrow(/do not match/u);
    expect(() =>
      prepareMintDeclaredAssetLimitEvidence({
        finding: { subject: accepted, policyIndex: 1 },
        fieldPreimage: field,
        committedFieldHashHex: midgardFieldCommitment(field).toString("hex"),
      }),
    ).toThrow(/outside field 5/u);
    expect(() =>
      foldMintDeclaredAssetLimit([crossing(1), crossing(2)], 1),
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
    const field = encodeMidgardFieldPreimage(items);
    const input = {
      transactionId: txId,
      fieldPreimageCbor: field.toString("hex"),
      policyIndex: 48,
    } as const;
    const first = planMintDeclaredAssetLimitStagedWalk(input);
    const restarted = planMintDeclaredAssetLimitStagedWalk(input);
    expect(first).toEqual(restarted);
    expect(first.grammar).toHaveLength(3);
    // 192 units: 21 singleton policies (9 units each) per fold transaction,
    // twice, then the last seven including the bound singleton.
    expect(first.walk).toHaveLength(3);
    expect(first.walk[0]!.checkpoint.nextItemIndex).toBe(21);
    expect(first.walk[0]!.cursor.accumulatedCount).toBe(21);
    expect(first.walk[0]!.cursor.activePolicy).toBe("");
    // The bound item is itself a singleton: a complete non-crossing fold.
    expect(first.walk[2]!.cursor.outcome).toBe(2);
    expect(first.walk[2]!.cursor.accumulatedCount).toBe(49);
    expect(first.crossing).toBe(false);
    const grammarBytes = encodeMintDeclaredGrammarCheckpoint(first.grammar[0]!);
    const walkBytes = encodeMintDeclaredWalkCheckpoint(
      first.walk[0]!.checkpoint,
    );
    expect(grammarBytes).toHaveLength(87);
    expect(walkBytes).toHaveLength(53);
    expect(grammarBytes[36]).toBe(5);
    expect(walkBytes[36]).toBe(5);
    expect(hashMintDeclaredGrammarCheckpoint(first.grammar[0]!)).toMatch(
      /^[0-9a-f]{64}$/u,
    );
    expect(hashMintDeclaredWalkCheckpoint(first.walk[0]!.checkpoint)).toMatch(
      /^[0-9a-f]{64}$/u,
    );
  });

  it("consumes a wide prior policy across fold transactions without moving the walk", () => {
    const wide = encodeMidgardMintPolicyItem({
      policyId: Buffer.alloc(28, 0),
      assets: Array.from({ length: 600 }, (_, index) => ({
        assetName: Buffer.from([index >> 8, index & 255]),
        quantity: 1n,
      })),
    });
    const items = [wide, singleton(1), crossing(2)];
    const field = encodeMidgardFieldPreimage(items);
    const plan = planMintDeclaredAssetLimitStagedWalk({
      transactionId: txId,
      fieldPreimageCbor: field.toString("hex"),
      policyIndex: 2,
    });
    // 8 + 184 assets, 192, 192, then 32 assets close the wide policy and the
    // singleton and the target header follow in the same budget.
    expect(
      plan.walk.map((snapshot) => snapshot.cursor.accumulatedCount),
    ).toEqual([184, 376, 568, 601]);
    expect(plan.walk[0]!.checkpoint).toEqual(plan.initialWalk);
    expect(plan.walk[0]!.cursor.activePolicy).toBe("00".repeat(28));
    expect(plan.walk[0]!.cursor.assetsRemaining).toBe(416);
    expect(plan.walk[2]!.checkpoint).toEqual(plan.initialWalk);
    expect(plan.walk[3]!.cursor.outcome).toBe(1);
    expect(plan.crossing).toBe(true);
    // Straight fold and staged fold agree.
    expect(foldMintDeclaredAssetLimit(items, 2)).toEqual({
      crossing: true,
      accumulatedCount: 601,
      targetPolicyId: "02".repeat(28),
      targetDeclaredCount: 16_385,
    });
    // A partial resume from any snapshot reaches the same terminal state.
    const resumed = advanceMintDeclaredFold({
      cursor: plan.walk[0]!.cursor,
      nextItemIndex: 0,
      items,
      target: plan.target,
      budget: 17,
    });
    expect(resumed.cursor.accumulatedCount).toBe(201);
    expect(resumed.nextItemIndex).toBe(0);
    expect(() =>
      advanceMintDeclaredFold({
        cursor: plan.walk[0]!.cursor,
        nextItemIndex: 1,
        items,
        target: plan.target,
      }),
    ).toThrow(/asset name/u);
  });

  it("encodes the fold state exactly as the Aiken golden vector", () => {
    const item = Buffer.concat([
      Buffer.from([0x82, 0x58, 0x1c]),
      Buffer.alloc(28, 0),
      Buffer.from("a3400141000142000001", "hex"),
    ]);
    const target = {
      policyIndex: 0,
      targetPolicyId: "00".repeat(28),
      targetDeclaredCount: 3,
    };
    const partial = advanceMintDeclaredFold({
      cursor: initialMintDeclaredFoldCursor(),
      nextItemIndex: 0,
      items: [item],
      target,
      budget: 10,
    });
    expect(partial.cursor).toEqual({
      accumulatedCount: 2,
      previousPolicy: "",
      activePolicy: "00".repeat(28),
      itemCursor: 37,
      assetsRemaining: 1,
      policyAssetCursor: 2,
      previousAsset: "00",
      outcome: 0,
    });
    const encoded = Data.to(
      mintDeclaredFoldStateData({
        subject: {
          version: 1n,
          direction: 1n,
          source_kind: 1n,
          transaction_id:
            "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f",
          source_key: "01",
          rejection_reason: { MintDeclaredAssetLimit: { policy_index: 0n } },
        } as never,
        target,
        cursor: partial.cursor,
        checkpointHash: "aa".repeat(32),
      }) as never,
      MintDeclaredAssetLimitFoldStateSchema as never,
    );
    expect(encoded).toBe(
      "d8799fd8799f0101015820000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f4101d8799fd905179f00ffffff00581c" +
        "00".repeat(28) +
        "035820" +
        "aa".repeat(32) +
        "0240581c" +
        "00".repeat(28) +
        "18250102410000ff",
    );
  });

  it("reads the machine's array head and refuses a malformed prior body", () => {
    const wideHead = Buffer.concat([
      Buffer.from([0x98, 0x02, 0x58, 0x1c]),
      Buffer.alloc(28, 1),
      Buffer.from("a14001", "hex"),
    ]);
    expect(decodeMintDeclaredPolicyHeader(wideHead).declaredCount).toBe(1);
    expect(foldMintDeclaredAssetLimit([wideHead], 0).crossing).toBe(false);
    const trailing = Buffer.concat([singleton(1), Buffer.from([0])]);
    expect(() =>
      foldMintDeclaredAssetLimit([trailing, crossing(2)], 1),
    ).toThrow(/declared count/u);
    const zero = Buffer.concat([
      Buffer.from([0x82, 0x58, 0x1c]),
      Buffer.alloc(28, 1),
      Buffer.from("a14000", "hex"),
    ]);
    expect(() => foldMintDeclaredAssetLimit([zero, crossing(2)], 1)).toThrow(
      /quantity is zero/u,
    );
  });

  it("selects the earliest authenticated detection independent of traversal order", () => {
    const detection = (
      position: bigint,
      detectionId: string,
    ): MintDeclaredAssetLimitReplayDetection => ({
      detectionId,
      headerHash: "22".repeat(28),
      violationId: MINT_DECLARED_ASSET_LIMIT_VIOLATION_ID,
      position,
      transactionId: txId,
      policyIndex: 0,
      source: "accepted",
      direction: "wrongfulAcceptance",
    });
    expect(
      selectCanonicalMintDeclaredAssetLimitDetection([
        detection(9n, "z"),
        detection(2n, "b"),
        detection(2n, "a"),
      ]).detectionId,
    ).toBe("a");
  });

  it("reconstructs its staged artifact and refuses field substitution", () => {
    const prepared = evidence(accepted, crossing(1));
    const artifact = buildMintDeclaredAssetLimitArtifact({
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
    const admitted = admitMintDeclaredAssetLimitArtifact(artifact);
    expect(admitted.evidence).toEqual(prepared);
    expect(mintDeclaredAssetLimitArtifactDigest(artifact)).toMatch(
      /^[0-9a-f]{64}$/u,
    );
    expect(() =>
      admitMintDeclaredAssetLimitArtifact({
        ...artifact,
        fieldPreimageCbor: `${artifact.fieldPreimageCbor.slice(0, -2)}ff`,
      }),
    ).toThrow(/commitment changed/u);
  });
});

const rejectedEvidence = (item: Buffer) => evidence(rejected(0), item);

describe("mintDeclaredAssetLimit registered-chain parity", () => {
  it("applies the same chain the SDK registers for the same shared policies", async () => {
    const fixture = await buildRegisteredChainFixture(
      buildMintDeclaredAssetLimitFaultProofContracts,
    );
    const registered = fixture.contracts.mintDeclaredAssetLimit;
    const applied = applyMintDeclaredAssetLimitScripts(fixture.applyParams);
    expect(applied.map((step) => step.spendingScriptHash)).toStrictEqual(
      registered.steps.map((step) => step.spendingScriptHash),
    );
    expect(registered.firstStep.spendingScriptHash).toBe(
      applied[0].spendingScriptHash,
    );
    expect(applied.map((step) => step.blueprintTitle)).toStrictEqual([
      ...MINT_DECLARED_ASSET_LIMIT_BLUEPRINT_TITLES,
    ]);
  });
});
