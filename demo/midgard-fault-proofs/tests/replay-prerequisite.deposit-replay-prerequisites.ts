import {
  CROSS_BLOCK_DUPLICATE_EVENT_VIOLATION_ID,
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
import {
  assertReplayPrerequisiteCovered,
  CanonicalReplayPrerequisiteError,
  replayPrerequisiteFailure,
} from "../src/workflow/replay-prerequisite.js";
import { depositEvidence } from "./replay-prerequisite.withdrawal-replay-prerequisites.js";

describe("deposit replay prerequisites", () => {
  it("lets the cross-block duplicate finding at the source position cover a repeated deposit", () => {
    const { evidence, eventKey } = depositEvidence();
    const failure = replayPrerequisiteFailure(
      evidence.headerHash,
      eventKey(1),
      "prior_transition_effect",
    ).failures[0]!;
    const finding = (position: bigint): CanonicalViolationDetection => ({
      headerHash: evidence.headerHash,
      violationId: CROSS_BLOCK_DUPLICATE_EVENT_VIOLATION_ID,
      detectionId: `cross-block-duplicate-event:${position}`,
      position,
    });
    expect(() =>
      assertReplayPrerequisiteCovered(evidence, failure, [finding(1n)]),
    ).not.toThrow();
    for (const unrelated of [
      [],
      [finding(0n)],
      [{ ...finding(1n), headerHash: "99".repeat(28) }],
      [{ ...finding(1n), violationId: "fabricated-deposit" }],
    ])
      expect(() =>
        assertReplayPrerequisiteCovered(evidence, failure, unrelated),
      ).toThrow(CanonicalReplayPrerequisiteError);
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
    const { evidence, eventKey } = depositEvidence();
    const fabricated = (position: bigint): CanonicalViolationDetection => ({
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
      // An origin absent only because it was consumed or settled yields no
      // fabricated finding, so the prerequisite stays undischarged and the
      // block fails closed rather than being convicted.
      for (const unrelated of [
        [],
        [fabricated(0n)],
        [{ ...fabricated(1n), headerHash: "99".repeat(28) }],
        [
          {
            ...fabricated(1n),
            violationId: CROSS_BLOCK_DUPLICATE_EVENT_VIOLATION_ID,
          },
        ],
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
    // The other prerequisite kinds are not dischargeable by this family.
    expect(() =>
      assertReplayPrerequisiteCovered(
        evidence,
        replayPrerequisiteFailure(
          evidence.headerHash,
          eventKey(1),
          "prior_transition_effect",
        ).failures[0]!,
        [fabricated(1n)],
      ),
    ).toThrow(CanonicalReplayPrerequisiteError);
  });
});
