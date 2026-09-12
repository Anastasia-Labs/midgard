import {
  decodeMidgardNativeTxFullFromCanonicalCbor,
  encodeCbor,
} from "@al-ft/midgard-core";
import {
  committedWithdrawalKeyBytes,
  committedWithdrawalValueBytes,
  EMPTY_MERKLE_TREE_ROOT,
  EventKey,
  type OutputReference,
  WITHDRAWAL_MISTAG_VIOLATION_ID,
  type WithdrawalInfo,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  type CanonicalBlockEvidence,
  canonicalBlockEvidenceFromVerifiedPayload,
} from "../src/evidence/canonical-block-evidence.js";
import { eventKeyFingerprint } from "../src/transition-trace/reconstruct.js";
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
});
