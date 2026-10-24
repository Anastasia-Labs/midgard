import "./replay-prerequisite.complete-replay-proof-prerequisites.js";

import {
  committedWithdrawalKeyBytes,
  committedWithdrawalValueBytes,
  FABRICATED_DEPOSIT_VIOLATION_ID,
  FABRICATED_WITHDRAWAL_VIOLATION_ID,
  type OutputReference,
  WITHDRAWAL_MISTAG_VIOLATION_ID,
  type WithdrawalInfo,
} from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import { type CanonicalBlockEvidence } from "../src/evidence/canonical-block-evidence.js";
import { eventKeyFingerprint } from "../src/transition-trace/reconstruct.js";
import { type CanonicalViolationDetection } from "../src/workflow/classification.js";
import { DOUBLE_WITHDRAW_COMPLETE_CANONICAL_REPLAY } from "../src/workflow/complete-replay.js";
import { withdrawalSubject } from "../src/workflow/detection-subject.js";
import {
  assertReplayPrerequisiteCovered,
  CanonicalReplayPrerequisiteError,
  replayPrerequisiteFailure,
} from "../src/workflow/replay-prerequisite.js";

/** Two payable leaves draining one L2 output, as the reconstruction admits them. */
const doubleWithdrawEvidence = () => {
  const info: WithdrawalInfo = {
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
    validity: "WithdrawalIsValid",
  };
  const leaf = (id: OutputReference) => ({
    key: id,
    value: info,
    keyBytes: Buffer.from(committedWithdrawalKeyBytes(id), "hex"),
    valueBytes: Buffer.from(committedWithdrawalValueBytes(info), "hex"),
  });
  const withdrawals = [
    leaf({ transactionId: "11".repeat(32), outputIndex: 0n }),
    leaf({ transactionId: "22".repeat(32), outputIndex: 0n }),
  ];
  const events = withdrawals.map((entry) => {
    const eventKey = { WithdrawalEventKey: { withdrawal_id: entry.key } };
    return [
      eventKeyFingerprint(eventKey),
      { phase: "Withdrawal", eventKey, entry },
    ] as const;
  });
  const evidence = {
    headerHash: "44".repeat(28),
    payloadEnvelopeSha256: "66".repeat(32),
    payloadSha256: "77".repeat(32),
    transactions: [],
    reconstruction: {
      withdrawals,
      forcedTransactions: [],
      sourceEventsByFingerprint: new Map(events),
    },
  } as unknown as CanonicalBlockEvidence;
  return { evidence, eventKey: (leaf: number) => events[leaf]![1].eventKey };
};

describe("withdrawal replay prerequisites", () => {
  it("lets a double-withdraw finding cover either payable leaf it names", async () => {
    const { evidence, eventKey } = doubleWithdrawEvidence();
    const decision =
      await DOUBLE_WITHDRAW_COMPLETE_CANONICAL_REPLAY.replay(evidence);
    expect(decision.detections).toHaveLength(1);
    for (const leaf of [0, 1]) {
      const failure = replayPrerequisiteFailure(
        evidence.headerHash,
        eventKey(leaf),
        "present_spend_input",
      ).failures[0]!;
      expect(() =>
        assertReplayPrerequisiteCovered(evidence, failure, decision.detections),
      ).not.toThrow();
      expect(() =>
        assertReplayPrerequisiteCovered(evidence, failure, []),
      ).toThrow(CanonicalReplayPrerequisiteError);
    }
  });

  it("lets only the mistag finding at the exact leaf cover a withdrawal", () => {
    const { evidence, eventKey } = doubleWithdrawEvidence();
    const failure = replayPrerequisiteFailure(
      evidence.headerHash,
      eventKey(1),
      "present_spend_input",
    ).failures[0]!;
    const mistag = (position: bigint): CanonicalViolationDetection => ({
      ...withdrawalSubject(
        evidence.reconstruction.withdrawals[Number(position)]!.key,
      ),
      headerHash: evidence.headerHash,
      detectionId: `withdrawal-mistag:${position.toString()}`,
      violationId: WITHDRAWAL_MISTAG_VIOLATION_ID,
      position,
    });
    expect(() =>
      assertReplayPrerequisiteCovered(evidence, failure, [mistag(1n)]),
    ).not.toThrow();
    expect(() =>
      assertReplayPrerequisiteCovered(evidence, failure, [mistag(0n)]),
    ).toThrow(CanonicalReplayPrerequisiteError);
    const other = replayPrerequisiteFailure(
      evidence.headerHash,
      eventKey(1),
      "accepted_terminal",
    ).failures[0]!;
    expect(() =>
      assertReplayPrerequisiteCovered(evidence, other, [mistag(1n)]),
    ).toThrow(CanonicalReplayPrerequisiteError);
  });

  it("lets only the fabricated-withdrawal finding at the exact leaf cover an absent or mismatched origin", () => {
    const { evidence, eventKey } = doubleWithdrawEvidence();
    const fabricated = (position: bigint): CanonicalViolationDetection => ({
      ...withdrawalSubject(
        evidence.reconstruction.withdrawals[Number(position)]!.key,
      ),
      headerHash: evidence.headerHash,
      detectionId: `${FABRICATED_WITHDRAWAL_VIOLATION_ID}:${position.toString()}`,
      violationId: FABRICATED_WITHDRAWAL_VIOLATION_ID,
      position,
    });
    for (const prerequisite of [
      "present_source_origin",
      "matching_source_origin",
    ] as const) {
      const failure = replayPrerequisiteFailure(
        evidence.headerHash,
        eventKey(1),
        prerequisite,
      ).failures[0]!;
      expect(() =>
        assertReplayPrerequisiteCovered(evidence, failure, [fabricated(1n)]),
      ).not.toThrow();
      for (const unrelated of [
        [],
        [fabricated(0n)],
        [{ ...fabricated(1n), headerHash: "99".repeat(28) }],
        [{ ...fabricated(1n), violationId: WITHDRAWAL_MISTAG_VIOLATION_ID }],
        [{ ...fabricated(1n), violationId: FABRICATED_DEPOSIT_VIOLATION_ID }],
      ])
        expect(() =>
          assertReplayPrerequisiteCovered(evidence, failure, unrelated),
        ).toThrow(CanonicalReplayPrerequisiteError);
    }
  });
});

/** A deposit source frontier, as the reconstruction admits it. */
export const depositEvidence = () => {
  const deposits = [0, 1].map((index) => {
    const key: OutputReference = {
      transactionId: (0x30 + index).toString(16).repeat(32),
      outputIndex: BigInt(index),
    };
    return {
      key,
      value: {},
      keyBytes: Buffer.alloc(0),
      valueBytes: Buffer.alloc(0),
    };
  });
  const sourceEvents = deposits.map((entry) => {
    const eventKey = { DepositEventKey: { deposit_id: entry.key } };
    return {
      phase: "Deposit",
      fingerprint: eventKeyFingerprint(eventKey),
      eventKey,
      entry,
    };
  });
  const evidence = {
    headerHash: "45".repeat(28),
    payloadEnvelopeSha256: "66".repeat(32),
    payloadSha256: "77".repeat(32),
    transactions: [],
    reconstruction: {
      deposits,
      withdrawals: [],
      forcedTransactions: [],
      sourceEvents,
      sourceEventsByFingerprint: new Map(
        sourceEvents.map((source) => [source.fingerprint, source] as const),
      ),
    },
  } as unknown as CanonicalBlockEvidence;
  return {
    evidence,
    eventKey: (index: number) => sourceEvents[index]!.eventKey,
    depositId: (index: number) => deposits[index]!.key,
  };
};
