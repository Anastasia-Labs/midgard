import { encodeMidgardCekProgramMaterialSidecar } from "@al-ft/midgard-core/cek-proof";
import {
  computeMidgardNativeTxId,
  decodeMidgardNativeTxFullFromCanonicalCbor,
} from "@al-ft/midgard-core/codec";
import { h32 } from "@al-ft/midgard-test-support/hex";
import {
  makeNativeTx,
  outRefFromByte,
} from "@al-ft/midgard-validation/tests/validation-fixtures";
import { RejectCodes } from "@al-ft/midgard-validation/types";
import { describe, expect, it } from "vitest";

import { watcherSha256CanonicalJson } from "../../src/storage/durable-store.js";
import {
  evaluateWatcherPhaseAQueuedTxs,
  makeWatcherPhaseAConfig,
  WATCHER_PHASE_A_VERIFIER_SCHEMA_VERSION,
  watcherPhaseAQueuedTxs,
  WatcherPhaseAVerifierError,
} from "../../src/verification/phase-a-verifier.js";
import { WATCHER_RULE_BUNDLE_VALIDATION_PHASE_PRIORITY } from "../../src/verification/rule-bundle.js";
import { canonicalVerdict } from "./phase-a-verifier.adjacent-boundary.js";
import {
  CONFIG,
  EMPTY_SIDECAR,
  fromNativeTx,
  KEY,
  ORPHAN_MATERIAL_DA_ENTRY,
  ORPHAN_MATERIAL_ENTRY,
  queuedTx,
  RULE_BUNDLE,
  RULE_BUNDLE_COMMITMENT,
} from "./phase-a-verifier.base-header.js";
import { buildBlock, evaluateBlock } from "./phase-a-verifier.build-block.js";

// ---------------------------------------------------------------------------
// Rejection ordering and W23 selection
// ---------------------------------------------------------------------------

describe("deterministic ordering", () => {
  it("emits rejections in block order and selects by W23 phase priority", () => {
    const lateStage = fromNativeTx({
      requiredSignerItems: [Buffer.alloc(28, 0x5a)],
    });
    const earlyStage = fromNativeTx({ auxiliaryDataHash: Buffer.alloc(32, 1) });
    const result = evaluateWatcherPhaseAQueuedTxs({
      queuedTxs: [lateStage, earlyStage],
      config: CONFIG,
    });
    expect(result.rejections.map((rejection) => rejection.index)).toStrictEqual(
      [0, 1],
    );
    expect(result.rejections[0]!.stage).toBe("signatures");
    expect(result.rejections[1]!.stage).toBe("canonicalDecode");
    // Block order puts the signatures rejection first, but the W23 rule
    // selects the lowest validation phase, which is the second transaction.
    expect(result.selectedRejection).toStrictEqual(result.rejections[1]);
    expect(result.rejectionSelection).toBe(
      RULE_BUNDLE.validation.rejectionSelection,
    );
  });

  it("breaks a phase tie by canonical block order", () => {
    const first = fromNativeTx({ auxiliaryDataHash: Buffer.alloc(32, 1) });
    const second = fromNativeTx({ auxiliaryDataHash: Buffer.alloc(32, 2) });
    const result = evaluateWatcherPhaseAQueuedTxs({
      queuedTxs: [first, second],
      config: CONFIG,
    });
    expect(result.selectedRejection!.index).toBe(0);
  });

  it("uses the W23 phase priority for stagePriority", () => {
    const result = evaluateWatcherPhaseAQueuedTxs({
      queuedTxs: [fromNativeTx({ requiredSignerItems: [Buffer.alloc(28, 1)] })],
      config: CONFIG,
    });
    expect(result.rejections[0]!.stagePriority).toBe(
      WATCHER_RULE_BUNDLE_VALIDATION_PHASE_PRIORITY.indexOf("signatures"),
    );
  });
});

// ---------------------------------------------------------------------------
// Positive block path
// ---------------------------------------------------------------------------

describe("block verification", () => {
  it("accepts an all-valid block with a stable resultDigest", async () => {
    const fixture = await buildBlock({
      txCbors: [
        makeNativeTx({ privateKey: KEY, spendInputs: [outRefFromByte(1)] })
          .txCbor,
        makeNativeTx({ privateKey: KEY, spendInputs: [outRefFromByte(2)] })
          .txCbor,
      ],
    });
    expect(fixture.reconstruction.action).toBe("accept");
    const first = await evaluateBlock(fixture);
    const second = await evaluateBlock(fixture);
    expect(first.schemaVersion).toBe(WATCHER_PHASE_A_VERIFIER_SCHEMA_VERSION);
    expect(first.action).toBe("accept");
    expect(first.reasonCodes).toStrictEqual([]);
    expect(first.rejections).toStrictEqual([]);
    expect(first.selectedRejection).toBeNull();
    expect(first.transactionCount).toBe(2);
    expect(first.acceptedCount).toBe(2);
    expect(first.headerHash).toBe(fixture.headerHash);
    expect(first.reconstructionDigest).toBe(
      fixture.reconstruction.resultDigest,
    );
    expect(first.ruleBundleCommitment).toBe(RULE_BUNDLE_COMMITMENT);
    expect(first.resultDigest).toBe(second.resultDigest);
    expect(Object.isFrozen(first)).toBe(true);
  });

  it("binds the digest to the verdict content", async () => {
    const accepted = await buildBlock({
      txCbors: [makeNativeTx({ privateKey: KEY }).txCbor],
    });
    const rejected = await buildBlock({
      txCbors: [
        makeNativeTx({ privateKey: KEY, invalidVkeyWitness: true }).txCbor,
      ],
    });
    const first = await evaluateBlock(accepted);
    const second = await evaluateBlock(rejected);
    expect(second.action).toBe("reject");
    expect(second.reasonCodes).toStrictEqual(["phase_a_rejection"]);
    expect(second.rejections[0]!.code).toBe(RejectCodes.InvalidSignature);
    expect(first.resultDigest).not.toBe(second.resultDigest);
    const { resultDigest: _digest, ...withoutDigest } = second;
    expect(watcherSha256CanonicalJson(withoutDigest)).toBe(second.resultDigest);
  });

  it("matches the canonical verdict for a mixed block", async () => {
    const cbors = [
      makeNativeTx({ privateKey: KEY, spendInputs: [outRefFromByte(3)] })
        .txCbor,
      makeNativeTx({
        privateKey: KEY,
        spendInputs: [outRefFromByte(4)],
        invalidVkeyWitness: true,
      }).txCbor,
      makeNativeTx({
        privateKey: KEY,
        spendInputs: [outRefFromByte(5), outRefFromByte(5)],
      }).txCbor,
    ];
    const fixture = await buildBlock({ txCbors: cbors });
    const result = await evaluateBlock(fixture);
    expect(result.action).toBe("reject");
    expect(result.transactionCount).toBe(3);

    // Rebuild the canonical inputs independently and compare verdict by
    // verdict. The watcher record must be the canonical record, reshaped.
    const byTxId = new Map(
      cbors.map((cbor) => [
        computeMidgardNativeTxId(
          decodeMidgardNativeTxFullFromCanonicalCbor(cbor),
        ).toString("hex"),
        cbor,
      ]),
    );
    const orderedTxIds = fixture.payload.block_body.transactions.map(
      ([key]) => key,
    );
    orderedTxIds.forEach((txId, index) => {
      const canonical = canonicalVerdict(
        queuedTx(Buffer.from(txId, "hex"), byTxId.get(txId)!, {
          arrivalSeq: BigInt(index),
        }),
        makeWatcherPhaseAConfig({
          header: fixture.header,
          ruleBundle: RULE_BUNDLE,
        }),
      );
      const watcher =
        result.rejections.find((rejection) => rejection.index === index) ??
        null;
      if (canonical.accepted) {
        expect(watcher).toBeNull();
        expect(result.acceptedTxIds).toContain(txId);
        return;
      }
      expect(watcher).toMatchObject({
        txId,
        code: canonical.rejected!.code,
        stage: canonical.rejected!.consensusPhase,
        detail: canonical.rejected!.detail,
      });
    });
  });

  it("reads minFee and network id from the L1-committed header", async () => {
    const fixture = await buildBlock({
      txCbors: [makeNativeTx({ privateKey: KEY, fee: 0n }).txCbor],
      headerOverrides: { minFeeB: 1n },
    });
    const result = await evaluateBlock(fixture);
    expect(result.action).toBe("reject");
    expect(result.rejections[0]!.code).toBe(RejectCodes.MinFee);
  });
});

// ---------------------------------------------------------------------------
// Program-material projection
// ---------------------------------------------------------------------------

describe("program-material projection", () => {
  it("does not reject a plain transaction for unrelated block material", async () => {
    const fixture = await buildBlock({
      txCbors: [makeNativeTx({ privateKey: KEY }).txCbor],
      programMaterial: [ORPHAN_MATERIAL_DA_ENTRY],
    });
    const result = await evaluateBlock(fixture);
    expect(result.action).toBe("accept");

    // Control: handing the unprojected block-wide set to the canonical
    // validator is exactly what the projection exists to avoid.
    const fullSidecar = encodeMidgardCekProgramMaterialSidecar([
      ORPHAN_MATERIAL_ENTRY,
    ]);
    const tx = makeNativeTx({ privateKey: KEY });
    const control = canonicalVerdict(
      queuedTx(tx.txId, tx.txCbor, {
        programMaterialSidecarCbor: fullSidecar,
      }),
      CONFIG,
    );
    expect(control.rejected!.code).toBe(RejectCodes.CekProgramMaterial);
  });

  it("derives an empty sidecar when the block carries no material", () => {
    const tx = makeNativeTx({ privateKey: KEY });
    const [queued] = watcherPhaseAQueuedTxs({
      transactions: [{ txId: tx.txId.toString("hex"), txCbor: tx.txCbor }],
      programMaterial: [],
    });
    expect(queued!.programMaterialSidecarCbor).toStrictEqual(EMPTY_SIDECAR);
    expect(queued!.arrivalSeq).toBe(0n);
  });

  it("keeps the complete block material when canonical narrowing throws", () => {
    // The narrowing fallback is the one place the watcher could silently make
    // Phase A more permissive than the operator's own admission. Feeding bytes
    // the canonical decoder rejects forces the fallback and pins it to the
    // complete block-wide set, not the empty one.
    const [queued] = watcherPhaseAQueuedTxs({
      transactions: [{ txId: h32(0x11), txCbor: Buffer.from([0xff]) }],
      programMaterial: [ORPHAN_MATERIAL_DA_ENTRY],
    });
    expect(queued!.programMaterialSidecarCbor).toStrictEqual(
      encodeMidgardCekProgramMaterialSidecar([ORPHAN_MATERIAL_ENTRY]),
    );
    expect(queued!.programMaterialSidecarCbor).not.toStrictEqual(EMPTY_SIDECAR);
  });

  it("numbers derived queued transactions by canonical block position", () => {
    const first = makeNativeTx({
      privateKey: KEY,
      spendInputs: [outRefFromByte(11)],
    });
    const second = makeNativeTx({
      privateKey: KEY,
      spendInputs: [outRefFromByte(12)],
    });
    const derived = watcherPhaseAQueuedTxs({
      transactions: [
        { txId: first.txId.toString("hex"), txCbor: first.txCbor },
        { txId: second.txId.toString("hex"), txCbor: second.txCbor },
      ],
      programMaterial: [],
    });
    expect(derived.map((entry) => entry.arrivalSeq)).toStrictEqual([0n, 1n]);
    expect(derived.map((entry) => entry.createdAt.getTime())).toStrictEqual([
      0, 0,
    ]);
  });

  it("fails closed on undecodable block program material", () => {
    expect(() =>
      watcherPhaseAQueuedTxs({
        transactions: [],
        programMaterial: [[h32(1), "00"]],
      }),
    ).toThrow(WatcherPhaseAVerifierError);
  });
});
