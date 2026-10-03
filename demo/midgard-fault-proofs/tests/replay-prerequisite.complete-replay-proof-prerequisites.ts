import {
  decodeMidgardNativeTxFullFromCanonicalCbor,
  encodeCbor,
} from "@al-ft/midgard-core";
import {
  EMPTY_MERKLE_TREE_ROOT,
  EventKey,
  FABRICATED_DEPOSIT_VIOLATION_ID,
  FABRICATED_WITHDRAWAL_VIOLATION_ID,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { canonicalBlockEvidenceFromVerifiedPayload } from "../src/evidence/canonical-block-evidence.js";
import { assertRetainedReplayTerminal } from "../src/transition-trace/replay-terminal.js";
import { buildRetainedValidationClaimWitness } from "../src/transition-trace/witnesses.js";
import {
  type CanonicalViolationDetection,
  classifyCanonicalBlockViolations,
} from "../src/workflow/classification.js";
import { DOUBLE_SPEND_COMPLETE_CANONICAL_REPLAY } from "../src/workflow/complete-replay.js";
import {
  acceptedTransactionSubject,
  BLOCK_SUBJECT,
  forcedTransactionSubject,
} from "../src/workflow/detection-subject.js";
import {
  assertReplayPrerequisiteCovered,
  CanonicalReplayPrerequisiteError,
  collectReplayFindings,
  completeReplayFindings,
  replayPrerequisiteFailure,
} from "../src/workflow/replay-prerequisite.js";
import {
  authenticatedHeaderObservation,
  buildCanonicalBlockFixture,
  buildFixtureTransaction,
  outRefCbor,
} from "./helpers/canonical-block-evidence-fixture.js";
import { buildDecodingBlockFixture } from "./support/native-script-decoding-emulator.js";
import { transitionTraceAcceptedRetainedFixture } from "./support/transition-trace-retained.js";

export const evidenceFor = async (
  block: Pick<
    Awaited<ReturnType<typeof buildCanonicalBlockFixture>>,
    "header" | "headerHash" | "payloadEnvelopeCbor"
  >,
) =>
  canonicalBlockEvidenceFromVerifiedPayload({
    observation: authenticatedHeaderObservation(block),
    payloadEnvelopeCbor: block.payloadEnvelopeCbor,
    daProvenance: {
      trustClass: "public_or_permissionless_da",
      sourceId: "replay-prerequisite",
      grade: "security",
    },
  });

const fixture = async () => {
  const block = await buildCanonicalBlockFixture({
    transactions: [1n, 2n].map((fee) =>
      buildFixtureTransaction({ spendInputs: [outRefCbor(0x31, 0n)], fee }),
    ),
  });
  const evidence = await evidenceFor(block);
  const event = (position: number) => ({
    L2TransactionEventKey: { tx_id: evidence.transactions[position]!.nodeTxId },
  });
  /** A finding whose declared subject is the normal transaction at `at`. */
  const finding = (
    violationId: string,
    at: number,
  ): CanonicalViolationDetection => ({
    ...acceptedTransactionSubject(evidence.transactions[at]!.nodeTxId),
    headerHash: evidence.headerHash,
    detectionId: `${violationId}:${at}`,
    violationId,
    position: BigInt(at),
  });
  return { evidence, event, finding };
};

/**
 * An advisory the complete replay emits when predecessor context is
 * unavailable. It has no classification rule, so it never discharges.
 */
export const ADVISORY_VIOLATION_ID =
  "authenticated-predecessor-context-unavailable";

describe("complete replay proof prerequisites", () => {
  it("covers an unavailable prior effect with a registered finding at or before its step", async () => {
    const { evidence, event, finding } = await fixture();
    const failure = replayPrerequisiteFailure(
      evidence.headerHash,
      event(0),
      "prior_transition_effect",
    ).failures[0]!;
    const proof = {
      ...finding("transition-trace", 0),
      provenTransitionEventKeyCbor: Data.to(event(0), EventKey),
    };
    for (const covering of [
      proof,
      // Any registered family at the same step makes the block removable.
      finding("invalid-signature", 0),
      // A block-level finding precedes every event.
      { ...proof, ...BLOCK_SUBJECT },
    ])
      expect(() =>
        assertReplayPrerequisiteCovered(evidence, failure, [covering]),
      ).not.toThrow();
    for (const unrelated of [
      finding("transition-trace", 1),
      { ...proof, violationId: ADVISORY_VIOLATION_ID },
      { ...proof, headerHash: "99".repeat(32) },
    ])
      expect(() =>
        assertReplayPrerequisiteCovered(evidence, failure, [unrelated]),
      ).toThrow(CanonicalReplayPrerequisiteError);
  });

  it("covers a blocked event with any registered finding at or before its step", async () => {
    const { evidence, event, finding } = await fixture();
    const failure = (position: number) =>
      replayPrerequisiteFailure(
        evidence.headerHash,
        event(position),
        "present_spend_input",
      ).failures[0]!;
    expect(() =>
      assertReplayPrerequisiteCovered(evidence, failure(0), []),
    ).toThrow(CanonicalReplayPrerequisiteError);
    for (const covering of [
      finding("non-existent-input", 0),
      finding("invalid-signature", 0),
    ])
      expect(() =>
        assertReplayPrerequisiteCovered(evidence, failure(0), [covering]),
      ).not.toThrow();
    // A finding at an earlier step covers a later blocked event.
    expect(() =>
      assertReplayPrerequisiteCovered(evidence, failure(1), [
        finding("invalid-signature", 0),
      ]),
    ).not.toThrow();
  });

  it("does not let a finding at a later step cover an earlier blocked event", async () => {
    const { evidence, event, finding } = await fixture();
    const failure = replayPrerequisiteFailure(
      evidence.headerHash,
      event(0),
      "present_spend_input",
    ).failures[0]!;
    // Reported at position 0, but its subject is the later transaction.
    const later = { ...finding("non-existent-input", 1), position: 0n };
    expect(() =>
      assertReplayPrerequisiteCovered(evidence, failure, [later]),
    ).toThrow(CanonicalReplayPrerequisiteError);
  });

  it("does not let an unregistered finding at the same step cover", async () => {
    const { evidence, event, finding } = await fixture();
    const failure = replayPrerequisiteFailure(
      evidence.headerHash,
      event(0),
      "present_spend_input",
    ).failures[0]!;
    expect(() =>
      assertReplayPrerequisiteCovered(evidence, failure, [
        finding(ADVISORY_VIOLATION_ID, 0),
      ]),
    ).toThrow(CanonicalReplayPrerequisiteError);
  });

  it("orders a double-spend proof at its later spender", async () => {
    const { evidence, event } = await fixture();
    const decision =
      await DOUBLE_SPEND_COMPLETE_CANONICAL_REPLAY.replay(evidence);
    expect(decision.detections).toHaveLength(1);
    expect(decision.detections[0]).toMatchObject({
      frontier: "accepted",
      subjectEventKeyCbors: [0, 1].map((position) =>
        Data.to(event(position), EventKey),
      ),
    });
    const failure = (position: number) =>
      replayPrerequisiteFailure(
        evidence.headerHash,
        event(position),
        "present_spend_input",
      ).failures[0]!;
    // The proof convicts the second spender's transition, so it covers that
    // event, but not the first spender's, which precedes it.
    expect(() =>
      assertReplayPrerequisiteCovered(
        evidence,
        failure(1),
        decision.detections,
      ),
    ).not.toThrow();
    expect(() =>
      assertReplayPrerequisiteCovered(
        evidence,
        failure(0),
        decision.detections,
      ),
    ).toThrow(CanonicalReplayPrerequisiteError);
  });

  it("decides a normal-transaction fault when a forced transaction shares its ordinal", async () => {
    const normal = buildFixtureTransaction({
      spendInputs: [outRefCbor(0x41, 0n)],
      fee: 1n,
    });
    const forced = buildFixtureTransaction({
      spendInputs: [outRefCbor(0x42, 0n)],
      fee: 2n,
    });
    const orderKey = { transactionId: "31".repeat(32), outputIndex: 0n };
    const block = await buildDecodingBlockFixture({
      operatorVkey: "b1".repeat(28),
      startTime: 10n,
      priorLedgerRoot: EMPTY_MERKLE_TREE_ROOT,
      subject: {
        kind: "forced",
        nativeTx: decodeMidgardNativeTxFullFromCanonicalCbor(
          forced.canonicalCbor,
        ),
        orderKey,
        verdict: "ForcedTxValid",
      },
      additionalTransactions: [
        decodeMidgardNativeTxFullFromCanonicalCbor(normal.canonicalCbor),
      ],
    });
    const evidence = await evidenceFor(block);
    // Both frontiers hold an event at ordinal 0.
    expect(evidence.transactions[0]!.nodeTxId).toBe(normal.txId);
    expect(evidence.reconstruction.forcedTransactions).toHaveLength(1);
    const normalFailure = replayPrerequisiteFailure(
      evidence.headerHash,
      { L2TransactionEventKey: { tx_id: normal.txId } },
      "accepted_terminal",
    ).failures[0]!;
    const normalFinding: CanonicalViolationDetection = {
      ...acceptedTransactionSubject(normal.txId),
      detectionId: `invalid-range:accepted:0:${normal.txId}`,
      headerHash: evidence.headerHash,
      violationId: "invalid-range",
      position: 0n,
    };
    const forcedFinding: CanonicalViolationDetection = {
      ...forcedTransactionSubject(orderKey),
      detectionId: "invalid-range:forced:0",
      headerHash: evidence.headerHash,
      violationId: "invalid-range",
      position: 0n,
    };
    expect(() =>
      assertReplayPrerequisiteCovered(evidence, normalFailure, [normalFinding]),
    ).not.toThrow();
    // Forced transactions precede normal ones, so a forced finding also
    // covers the normal event...
    expect(() =>
      assertReplayPrerequisiteCovered(evidence, normalFailure, [forcedFinding]),
    ).not.toThrow();
    // ...but a normal finding never covers the earlier forced event.
    const forcedFailure = replayPrerequisiteFailure(
      evidence.headerHash,
      { ForcedTransactionEventKey: { tx_order_id: orderKey } },
      "accepted_terminal",
    ).failures[0]!;
    expect(() =>
      assertReplayPrerequisiteCovered(evidence, forcedFailure, [normalFinding]),
    ).toThrow(CanonicalReplayPrerequisiteError);
    await expect(
      classifyCanonicalBlockViolations({
        evidence,
        detections: [normalFinding],
        minimumConfirmationDepth: 1,
      }),
    ).resolves.toMatchObject({
      decision: "fault_detected",
      category: "invalidRange",
      selected: normalFinding,
    });
  });

  it("preserves an earlier transition finding when a later direct event blocks replay", async () => {
    const { evidence, event, finding } = await fixture();
    const earlier = finding("transition-trace", 0);
    const later = finding("non-existent-input", 1);
    const blocked = replayPrerequisiteFailure(
      evidence.headerHash,
      event(1),
      "present_spend_input",
    );
    let observed: CanonicalReplayPrerequisiteError | undefined;
    try {
      await collectReplayFindings([
        Promise.resolve(earlier),
        Promise.reject(blocked),
      ]);
    } catch (error) {
      if (!(error instanceof CanonicalReplayPrerequisiteError)) throw error;
      observed = error;
    }
    expect(observed?.detections).toEqual([earlier]);
    const detections = [...observed!.detections, later];
    for (const failure of observed!.failures)
      assertReplayPrerequisiteCovered(evidence, failure, detections);
    const result = await classifyCanonicalBlockViolations({
      evidence,
      detections,
      minimumConfirmationDepth: 1,
    });
    expect(result).toMatchObject({
      decision: "fault_detected",
      category: "transitionTrace",
      selected: earlier,
    });
  });

  it("classifies a fabricated event as its own family instead of aborting", async () => {
    const { evidence } = await fixture();
    // Decision 0007: the fabricated families are ordinary provable findings,
    // so a block that fabricates an event still classifies as a fault.
    for (const violationId of [
      FABRICATED_DEPOSIT_VIOLATION_ID,
      FABRICATED_WITHDRAWAL_VIOLATION_ID,
    ]) {
      // The fixture commits no deposit or withdrawal, so this synthetic
      // finding is block-level.
      const detection = {
        ...BLOCK_SUBJECT,
        headerHash: evidence.headerHash,
        detectionId: `${violationId}:0`,
        violationId,
        position: 0n,
      };
      await expect(
        classifyCanonicalBlockViolations({
          evidence,
          detections: [detection],
          minimumConfirmationDepth: 1,
        }),
      ).resolves.toMatchObject({
        decision: "fault_detected",
        category:
          violationId === FABRICATED_DEPOSIT_VIOLATION_ID
            ? "fabricatedDeposit"
            : "fabricatedWithdrawal",
        selected: detection,
      });
    }
  });

  it("allows an empty complete scan only when no prerequisite remains", async () => {
    const { evidence, event } = await fixture();
    expect(completeReplayFindings([], [])).toEqual([]);
    const failure = replayPrerequisiteFailure(
      evidence.headerHash,
      event(0),
      "accepted_terminal",
    );
    expect(() => completeReplayFindings([], failure.failures)).toThrow(
      CanonicalReplayPrerequisiteError,
    );
    const corruption = new Error("Malformed retained witness");
    await expect(
      collectReplayFindings([Promise.reject(corruption)]),
    ).rejects.toBe(corruption);
  });

  it("refuses malformed terminal bytes even when their claimed endpoint is accepted", async () => {
    const block = await transitionTraceAcceptedRetainedFixture({
      operatorVkey: "b1".repeat(28),
      now: 1_700_000_000_000,
      honest: true,
    });
    const evidence = await evidenceFor(block.current);
    const eventKey = {
      L2TransactionEventKey: { tx_id: evidence.transactions[0]!.nodeTxId },
    };
    const retained = await buildRetainedValidationClaimWitness({
      reconstruction: evidence.reconstruction,
      eventKey,
    });
    expect(() => assertRetainedReplayTerminal(retained)).not.toThrow();
    for (const terminalWorkWitnessCbor of [
      "80",
      encodeCbor([
        3n,
        Buffer.alloc(0),
        Buffer.alloc(32),
        Buffer.from("80", "hex"),
      ]).toString("hex"),
      encodeCbor([
        1n,
        Buffer.from("E_INVALID_FIELD_TYPE"),
        Buffer.alloc(32),
        Buffer.from("80", "hex"),
      ]).toString("hex"),
    ])
      expect(() =>
        assertRetainedReplayTerminal({ ...retained, terminalWorkWitnessCbor }),
      ).toThrow(/terminal/iu);
  });
});
