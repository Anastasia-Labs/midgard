import {
  decodeMidgardNativeTxFullFromCanonicalCbor,
  encodeCbor,
} from "@al-ft/midgard-core";
import {
  committedWithdrawalKeyBytes,
  committedWithdrawalValueBytes,
  CROSS_BLOCK_DUPLICATE_EVENT_VIOLATION_ID,
  EMPTY_MERKLE_TREE_ROOT,
  EventKey,
  FABRICATED_DEPOSIT_VIOLATION_ID,
  FABRICATED_WITHDRAWAL_VIOLATION_ID,
  outOfWindowSourceEventFault,
  type OutputReference,
  type TransitionFault,
  WITHDRAWAL_MISTAG_VIOLATION_ID,
  type WithdrawalInfo,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  type CanonicalBlockEvidence,
  canonicalBlockEvidenceFromVerifiedPayload,
} from "../src/evidence/canonical-block-evidence.js";
import type { TransitionTraceDetection } from "../src/transition-trace/detect.js";
import { eventKeyFingerprint } from "../src/transition-trace/reconstruct.js";
import { provenTransitionEventKeyCbor } from "../src/transition-trace/replay-authority.js";
import { assertRetainedReplayTerminal } from "../src/transition-trace/replay-terminal.js";
import { buildRetainedValidationClaimWitness } from "../src/transition-trace/witnesses.js";
import {
  type CanonicalViolationDetection,
  classifyCanonicalBlockViolations,
} from "../src/workflow/classification.js";
import {
  DOUBLE_SPEND_COMPLETE_CANONICAL_REPLAY,
  DOUBLE_WITHDRAW_COMPLETE_CANONICAL_REPLAY,
} from "../src/workflow/complete-replay.js";
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

const evidenceFor = async (
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
  const finding = (
    violationId: string,
    position: bigint,
  ): CanonicalViolationDetection => ({
    headerHash: evidence.headerHash,
    detectionId: `${violationId}:${position}`,
    violationId,
    position,
  });
  return { evidence, event, finding };
};

describe("complete replay proof prerequisites", () => {
  it("requires exact semantic transition provenance for an unavailable prior effect", async () => {
    const { evidence, event, finding } = await fixture();
    const failure = replayPrerequisiteFailure(
      evidence.headerHash,
      event(0),
      "prior_transition_effect",
    ).failures[0]!;
    const proof = {
      ...finding("transition-trace", 0n),
      provenTransitionEventKeyCbor: Data.to(event(0), EventKey),
    };
    expect(() =>
      assertReplayPrerequisiteCovered(evidence, failure, [proof]),
    ).not.toThrow();
    for (const unrelated of [
      finding("transition-trace", 0n),
      { ...proof, provenTransitionEventKeyCbor: Data.to(event(1), EventKey) },
      { ...proof, violationId: "invalid-signature" },
      { ...proof, headerHash: "99".repeat(32) },
    ])
      expect(() =>
        assertReplayPrerequisiteCovered(evidence, failure, [unrelated]),
      ).toThrow(CanonicalReplayPrerequisiteError);
  });
  it("requires a direct finding for the same event and the exact failed domain", async () => {
    const { evidence, event, finding } = await fixture();
    const failure = replayPrerequisiteFailure(
      evidence.headerHash,
      event(0),
      "present_spend_input",
    ).failures[0]!;
    expect(() =>
      assertReplayPrerequisiteCovered(evidence, failure, []),
    ).toThrow(CanonicalReplayPrerequisiteError);
    expect(() =>
      assertReplayPrerequisiteCovered(evidence, failure, [
        finding("non-existent-input", 1n),
      ]),
    ).toThrow(CanonicalReplayPrerequisiteError);
    expect(() =>
      assertReplayPrerequisiteCovered(evidence, failure, [
        finding("invalid-signature", 0n),
      ]),
    ).toThrow(CanonicalReplayPrerequisiteError);
    expect(() =>
      assertReplayPrerequisiteCovered(evidence, failure, [
        finding("non-existent-input", 0n),
      ]),
    ).not.toThrow();
  });

  it("recognizes both authenticated subjects of a double-spend proof", async () => {
    const { evidence, event } = await fixture();
    const decision =
      await DOUBLE_SPEND_COMPLETE_CANONICAL_REPLAY.replay(evidence);
    expect(decision.detections).toHaveLength(1);
    for (const position of [0, 1]) {
      const failure = replayPrerequisiteFailure(
        evidence.headerHash,
        event(position),
        "present_spend_input",
      ).failures[0]!;
      expect(() =>
        assertReplayPrerequisiteCovered(evidence, failure, decision.detections),
      ).not.toThrow();
    }
  });

  it("fails closed when normal and forced findings share an ambiguous ordinal", async () => {
    const normal = buildFixtureTransaction({
      spendInputs: [outRefCbor(0x41, 0n)],
      fee: 1n,
    });
    const forced = buildFixtureTransaction({
      spendInputs: [outRefCbor(0x42, 0n)],
      fee: 2n,
    });
    const block = await buildDecodingBlockFixture({
      operatorVkey: "b1".repeat(28),
      startTime: 10n,
      priorLedgerRoot: EMPTY_MERKLE_TREE_ROOT,
      subject: {
        kind: "forced",
        nativeTx: decodeMidgardNativeTxFullFromCanonicalCbor(
          forced.canonicalCbor,
        ),
        orderKey: { transactionId: "31".repeat(32), outputIndex: 0n },
        verdict: "ForcedTxValid",
      },
      additionalTransactions: [
        decodeMidgardNativeTxFullFromCanonicalCbor(normal.canonicalCbor),
      ],
    });
    const evidence = await evidenceFor(block);
    const failure = replayPrerequisiteFailure(
      evidence.headerHash,
      { L2TransactionEventKey: { tx_id: normal.txId } },
      "accepted_terminal",
    ).failures[0]!;
    expect(() =>
      assertReplayPrerequisiteCovered(evidence, failure, [
        {
          detectionId: "invalid-range:forced:0",
          headerHash: evidence.headerHash,
          violationId: "invalid-range",
          position: 0n,
        },
      ]),
    ).toThrow(CanonicalReplayPrerequisiteError);
  });

  it("preserves an earlier transition finding when a later direct event blocks replay", async () => {
    const { evidence, event, finding } = await fixture();
    const earlier = finding("transition-trace", 0n);
    const later = finding("non-existent-input", 1n);
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
      const detection = {
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
const depositEvidence = () => {
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
        event_ref_input_index: 0n,
        event_asset_name: "ab".repeat(32),
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
