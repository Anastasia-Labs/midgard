import {
  DOUBLE_WITHDRAW_VIOLATION_ID,
  EventKey,
  FABRICATED_DEPOSIT_VIOLATION_ID,
  FABRICATED_WITHDRAWAL_VIOLATION_ID,
  outOfWindowSourceEventFault,
  type TransitionFault,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import type { TransitionTraceDetection } from "../src/transition-trace/detect.js";
import { provenTransitionEventKeyCbor } from "../src/transition-trace/replay-authority.js";
import { type CanonicalViolationDetection } from "../src/workflow/classification.js";
import { completeReplayer } from "../src/workflow/complete-replay.detect-double-withdraws.js";
import { createCompleteCanonicalReplayUnion } from "../src/workflow/complete-replay.js";
import {
  depositSubject,
  eventKeyCborSubject,
} from "../src/workflow/detection-subject.js";
import {
  assertReplayPrerequisiteCovered,
  CanonicalReplayPrerequisiteError,
  replayPrerequisiteFailure,
} from "../src/workflow/replay-prerequisite.js";
import { depositEvidence } from "./replay-prerequisite.withdrawal-replay-prerequisites.js";

const fabricatedDepositFinding = (
  evidence: ReturnType<typeof depositEvidence>["evidence"],
  depositId: ReturnType<typeof depositEvidence>["depositId"],
  position: bigint,
): CanonicalViolationDetection => ({
  ...depositSubject(depositId(Number(position))),
  headerHash: evidence.headerHash,
  detectionId: `${FABRICATED_DEPOSIT_VIOLATION_ID}:${position.toString()}`,
  violationId: FABRICATED_DEPOSIT_VIOLATION_ID,
  position,
});

describe("deposit replay prerequisites", () => {
  it("lets only the fabricated-deposit finding at the exact leaf cover a repeated deposit", () => {
    const { evidence, eventKey, depositId } = depositEvidence();
    const failure = replayPrerequisiteFailure(
      evidence.headerHash,
      eventKey(1),
      "prior_transition_effect",
    ).failures[0]!;
    const finding = (position: bigint) =>
      fabricatedDepositFinding(evidence, depositId, position);
    expect(() =>
      assertReplayPrerequisiteCovered(evidence, failure, [finding(1n)]),
    ).not.toThrow();
    // Without the fabricated-deposit finding at this leaf the repeated
    // deposit stays undischarged and the block fails closed.
    for (const unrelated of [
      [],
      [finding(0n)],
      [{ ...finding(1n), headerHash: "99".repeat(28) }],
      [{ ...finding(1n), violationId: FABRICATED_WITHDRAWAL_VIOLATION_ID }],
      [{ ...finding(1n), violationId: DOUBLE_WITHDRAW_VIOLATION_ID }],
    ])
      expect(() =>
        assertReplayPrerequisiteCovered(evidence, failure, unrelated),
      ).toThrow(CanonicalReplayPrerequisiteError);
  });

  it("classifies a repeated deposit through the replay union only when the fabricated-deposit finding names its leaf", async () => {
    const { evidence, eventKey, depositId } = depositEvidence();
    // The transition replay finds the repeated deposit's output already in
    // the ledger and records the prerequisite instead of a finding.
    const transition = completeReplayer(["transitionTrace"], async () => {
      throw replayPrerequisiteFailure(
        evidence.headerHash,
        eventKey(1),
        "prior_transition_effect",
      );
    });
    const fabricated = (findings: readonly CanonicalViolationDetection[]) =>
      completeReplayer(["fabricatedDeposit"], async () => findings);
    const finding = fabricatedDepositFinding(evidence, depositId, 1n);

    const covered = createCompleteCanonicalReplayUnion([
      transition,
      fabricated([finding]),
    ]);
    const decision = await covered.replay(evidence);
    expect(decision.detections).toEqual([finding]);

    for (const findings of [
      [],
      [fabricatedDepositFinding(evidence, depositId, 0n)],
    ]) {
      const uncovered = createCompleteCanonicalReplayUnion([
        transition,
        fabricated(findings),
      ]);
      const refused = await uncovered.replay(evidence).catch((error) => error);
      expect(refused).toBeInstanceOf(CanonicalReplayPrerequisiteError);
      expect((refused as CanonicalReplayPrerequisiteError).failures).toEqual([
        {
          headerHash: evidence.headerHash,
          eventKeyCbor: Data.to(eventKey(1), EventKey),
          prerequisite: "prior_transition_effect",
        },
      ]);
    }
  });

  it("names the committed event an out-of-window transition finding opens", () => {
    const { evidence, eventKey, depositId } = depositEvidence();
    const failure = replayPrerequisiteFailure(
      evidence.headerHash,
      eventKey(0),
      "prior_transition_effect",
    ).failures[0]!;
    const fault: TransitionFault = outOfWindowSourceEventFault({
      OutOfWindowDeposit: {
        source_membership: {
          domain: "d",
          root: "00".repeat(32),
          phas_root: "00".repeat(32),
          count: 1n,
          key: depositId(0),
          value: {},
          proof: [],
        },
      },
    } as never);
    const detection = {
      buildable: true,
      kind: "outOfWindowSourceEvent",
      invariant: "source_event_is_within_block_window",
      diagnostic: "",
      fault,
      proof: {},
    } as unknown as TransitionTraceDetection;
    const proven = provenTransitionEventKeyCbor(detection);
    expect(proven).toBe(Data.to(eventKey(0), EventKey));
    const finding = {
      ...eventKeyCborSubject(proven),
      headerHash: evidence.headerHash,
      violationId: "transition-trace",
      detectionId: "transition-trace:0:outOfWindowSourceEvent",
      position: 0n,
      provenTransitionEventKeyCbor: proven,
    };
    expect(() =>
      assertReplayPrerequisiteCovered(evidence, failure, [finding]),
    ).not.toThrow();
    expect(() =>
      assertReplayPrerequisiteCovered(
        evidence,
        replayPrerequisiteFailure(
          evidence.headerHash,
          eventKey(1),
          "prior_transition_effect",
        ).failures[0]!,
        [finding],
      ),
    ).toThrow(CanonicalReplayPrerequisiteError);
    expect(
      provenTransitionEventKeyCbor({ ...detection, buildable: false } as never),
    ).toBeUndefined();
  });

  it("lets only the fabricated-deposit finding at the exact leaf cover an absent or mismatched origin", () => {
    const { evidence, eventKey, depositId } = depositEvidence();
    const fabricated = (position: bigint): CanonicalViolationDetection => ({
      ...depositSubject(depositId(Number(position))),
      headerHash: evidence.headerHash,
      detectionId: `${FABRICATED_DEPOSIT_VIOLATION_ID}:${position.toString()}`,
      violationId: FABRICATED_DEPOSIT_VIOLATION_ID,
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
      // An origin the fabricated family does not convict yields no finding,
      // so the prerequisite stays undischarged and the block fails closed
      // rather than being convicted.
      for (const unrelated of [
        [],
        [fabricated(0n)],
        [{ ...fabricated(1n), headerHash: "99".repeat(28) }],
        [
          {
            ...fabricated(1n),
            violationId: FABRICATED_WITHDRAWAL_VIOLATION_ID,
          },
        ],
      ])
        expect(() =>
          assertReplayPrerequisiteCovered(evidence, failure, unrelated),
        ).toThrow(CanonicalReplayPrerequisiteError);
    }
    // A deposit's other prerequisite kinds are not dischargeable by this
    // family.
    expect(() =>
      assertReplayPrerequisiteCovered(
        evidence,
        replayPrerequisiteFailure(
          evidence.headerHash,
          eventKey(1),
          "present_spend_input",
        ).failures[0]!,
        [fabricated(1n)],
      ),
    ).toThrow(CanonicalReplayPrerequisiteError);
  });
});
