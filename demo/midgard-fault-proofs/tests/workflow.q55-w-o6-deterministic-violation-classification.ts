import * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import { type CanonicalBlockEvidence } from "../src/evidence/canonical-block-evidence.js";
import { eventKeyFingerprint } from "../src/transition-trace/reconstruct.js";
import { withdrawalMistagDetectionId } from "../src/withdrawal-mistag/replay.js";
import {
  type CanonicalViolationDetection,
  classifyCanonicalBlockViolations,
  FRAUD_PROOF_CLASSIFICATION_RULES,
} from "../src/workflow/classification.js";
import { DOUBLE_WITHDRAW_COMPLETE_CANONICAL_REPLAY } from "../src/workflow/complete-replay.js";
import {
  acceptedTransactionSubject,
  withdrawalSubject,
} from "../src/workflow/detection-subject.js";
import { h32 } from "./helpers/canonical-block-evidence-fixture.js";
import {
  canonicalEvidence,
  canonicalEvidenceWithEvents,
  detection,
} from "./workflow.make-adapter.js";

/**
 * Authenticated block evidence whose only source events are withdrawal leaves
 * draining one L2 output, untraced, so their event order is the list order:
 * the leaf at index 1 is the one applied second.
 */
const sameOutputWithdrawalEvidence = async (
  validities: readonly SDK.WithdrawalInfo["validity"][],
): Promise<CanonicalBlockEvidence> => {
  const evidence = await canonicalEvidence();
  const withdrawals = validities.map((validity, index) => {
    const key: SDK.OutputReference = {
      transactionId: (0x11 * (index + 1)).toString(16).repeat(32),
      outputIndex: 0n,
    };
    const value: SDK.WithdrawalInfo = {
      body: {
        l2_outref: { transactionId: "7e".repeat(32), outputIndex: 1n },
        l2_owner: "9c".repeat(28),
        l2_value: new Map(),
        l1_address: {
          paymentCredential: { PublicKeyCredential: ["2b".repeat(28)] },
          stakeCredential: null,
        },
        l1_datum: "NoDatum",
      },
      signature: ["ad".repeat(32), "be".repeat(64)],
      validity,
    };
    return {
      key,
      value,
      keyBytes: Buffer.from(SDK.committedWithdrawalKeyBytes(key), "hex"),
      valueBytes: Buffer.from(SDK.committedWithdrawalValueBytes(value), "hex"),
    };
  });
  const sourceEvents = withdrawals.map((entry) => {
    const eventKey = { WithdrawalEventKey: { withdrawal_id: entry.key } };
    return {
      phase: "Withdrawal" as const,
      eventKey,
      fingerprint: eventKeyFingerprint(eventKey),
      entry,
    };
  });
  return {
    ...evidence,
    reconstruction: {
      ...evidence.reconstruction,
      withdrawals,
      sourceEvents,
      sourceEventsByFingerprint: new Map(
        sourceEvents.map((source) => [source.fingerprint, source] as const),
      ),
    },
  };
};

/** The finding the withdrawal-mistag replay reports for one leaf. */
const withdrawalMistagAt = (
  evidence: CanonicalBlockEvidence,
  index: number,
): CanonicalViolationDetection => ({
  ...withdrawalSubject(evidence.reconstruction.withdrawals[index]!.key),
  violationId: SDK.WITHDRAWAL_MISTAG_VIOLATION_ID,
  headerHash: evidence.headerHash,
  detectionId: withdrawalMistagDetectionId(index),
  position: BigInt(index),
});

const doubleWithdrawDetections = async (evidence: CanonicalBlockEvidence) =>
  (await DOUBLE_WITHDRAW_COMPLETE_CANONICAL_REPLAY.replay(evidence)).detections;

describe("Q55/W-O6 deterministic violation classification", () => {
  it("covers every registered family exactly once in catalogue order", () => {
    expect(
      FRAUD_PROOF_CLASSIFICATION_RULES.map((rule) => rule.category),
    ).toEqual(SDK.FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER);
    const identifiers = FRAUD_PROOF_CLASSIFICATION_RULES.flatMap((rule) => [
      ...rule.violationIds,
    ]);
    expect(new Set(identifiers).size).toBe(identifiers.length);
  });

  it("selects the earliest event, then stable family order", async () => {
    const [evidence, txIds] = await canonicalEvidenceWithEvents(3);
    const classification = await classifyCanonicalBlockViolations({
      evidence,
      detections: [
        // A lower reported position never outranks an earlier event.
        detection(evidence, "mint-authorization", {
          ...acceptedTransactionSubject(txIds[2]!),
          detectionId: "mint-late",
          position: 0n,
        }),
        detection(evidence, "invalid-range", {
          ...acceptedTransactionSubject(txIds[1]!),
          detectionId: "range-first",
          position: 9n,
        }),
        detection(evidence, "double-spend", {
          ...acceptedTransactionSubject(txIds[0]!, txIds[1]!),
          detectionId: "double-first",
          position: 9n,
        }),
      ],
    });
    expect(classification).toMatchObject({
      decision: "fault_detected",
      category: "doubleSpend",
      selected: { detectionId: "double-first" },
    });
  });

  // Owner ruling 2026-11-10: two payable leaves naming one L2 output make the
  // later one also a "valid marked, output absent" withdrawalMistag finding
  // at the same event; the watcher proves the pair instead.
  it("prefers doubleWithdraw over withdrawalMistag on a payable same-output pair's later leaf", async () => {
    const evidence = await sameOutputWithdrawalEvidence([
      "WithdrawalIsValid",
      "WithdrawalIsValid",
    ]);
    const pair = await doubleWithdrawDetections(evidence);
    expect(pair).toHaveLength(1);
    for (const detections of [
      [withdrawalMistagAt(evidence, 1), ...pair],
      [...pair, withdrawalMistagAt(evidence, 1)],
    ])
      expect(
        await classifyCanonicalBlockViolations({ evidence, detections }),
      ).toMatchObject({
        decision: "fault_detected",
        category: "doubleWithdraw",
        selected: { violationId: SDK.DOUBLE_WITHDRAW_VIOLATION_ID },
      });
  });

  it("keeps withdrawalMistag for a mistagged leaf that is not a payable pair's later leaf", async () => {
    // The pair's earlier leaf is mistagged on its own: its event precedes the
    // pair's, so the mistag is the block's first fault.
    const paired = await sameOutputWithdrawalEvidence([
      "WithdrawalIsValid",
      "WithdrawalIsValid",
    ]);
    expect(
      await classifyCanonicalBlockViolations({
        evidence: paired,
        detections: [
          ...(await doubleWithdrawDetections(paired)),
          withdrawalMistagAt(paired, 0),
        ],
      }),
    ).toMatchObject({
      category: "withdrawalMistag",
      selected: { detectionId: withdrawalMistagDetectionId(0) },
    });
    // The earlier leaf is not payable, so there is no pair to prefer: the
    // later leaf's mistag stands alone.
    const unpaired = await sameOutputWithdrawalEvidence([
      "NonExistentWithdrawalUtxo",
      "WithdrawalIsValid",
    ]);
    expect(await doubleWithdrawDetections(unpaired)).toEqual([]);
  });

  it("maps unknown earliest violations to unprovable_gap, never verified", async () => {
    const [evidence, txIds] = await canonicalEvidenceWithEvents(2);
    const classification = await classifyCanonicalBlockViolations({
      evidence,
      detections: [
        detection(evidence, "unknown-launch-fault", {
          ...acceptedTransactionSubject(txIds[0]!),
          detectionId: "gap",
          position: 0n,
        }),
        detection(evidence, "double-spend", {
          ...acceptedTransactionSubject(txIds[1]!),
          detectionId: "later-proof",
          position: 1n,
        }),
      ],
    });
    expect(classification).toMatchObject({
      decision: "unprovable_gap",
      selected: {
        detectionId: "gap",
        reason: "unregistered_violation",
      },
    });
  });

  it("does not promote an empty partial detector result to verified", async () => {
    const evidence = await canonicalEvidence();
    await expect(
      classifyCanonicalBlockViolations({ evidence, detections: [] }),
    ).resolves.toMatchObject({ decision: "no_fault_detected" });
  });

  it("rejects duplicate detector identities and cross-header detections", async () => {
    const evidence = await canonicalEvidence();
    const duplicate = detection(evidence);
    await expect(
      classifyCanonicalBlockViolations({
        evidence,
        detections: [duplicate, duplicate],
      }),
    ).rejects.toThrow("duplicate canonical violation detectionId");
    await expect(
      classifyCanonicalBlockViolations({
        evidence,
        detections: [
          detection(evidence, "double-spend", { headerHash: h32(9) }),
        ],
      }),
    ).rejects.toThrow("targets header");
  });
});
