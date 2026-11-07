import {
  decodeMidgardNativeTxFullFromCanonicalCbor,
  encodeMidgardSpendInputItem,
  encodeMidgardTxOutput,
} from "@al-ft/midgard-core";
import {
  type DaPayloadEntry,
  EMPTY_MERKLE_TREE_ROOT,
  EventKey,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { canonicalBlockEvidenceFromVerifiedPayload } from "../src/evidence/canonical-block-evidence.js";
import { detectTransitionTraceFaults } from "../src/transition-trace/detect.js";
import { eventKeyFingerprint } from "../src/transition-trace/reconstruct.js";
import {
  provenTransitionEventKeyCbor,
  transitionTraceDetectionId,
} from "../src/transition-trace/replay-authority.js";
import {
  type CanonicalViolationDetection,
  classifyCanonicalBlockViolations,
} from "../src/workflow/classification.js";
import {
  acceptedTransactionSubject,
  eventKeyCborSubject,
  forcedTransactionSubject,
} from "../src/workflow/detection-subject.js";
import {
  assertReplayPrerequisiteCovered,
  CanonicalReplayPrerequisiteError,
  replayPrerequisiteFailure,
} from "../src/workflow/replay-prerequisite.js";
import {
  authenticatedHeaderObservation,
  buildFixtureTransaction,
  outRefCbor,
} from "./helpers/canonical-block-evidence-fixture.js";
import { buildDecodingBlockFixture } from "./support/native-script-decoding-emulator.js";

const ORDER_KEY = { transactionId: "31".repeat(32), outputIndex: 0n };

const l2EventKey = (txId: string) => ({
  L2TransactionEventKey: { tx_id: txId },
});

/**
 * One forced transaction beside three normal ones, stepped in canonical source
 * order, so the trace and the committed lists agree. `commitEventToStep` lets the block commit a different
 * `event_to_step`, given the committed normal transaction ids in list order.
 */
const forcedAndNormalEvidence = async (
  commitEventToStep: (
    txIds: readonly string[],
  ) => (entries: readonly DaPayloadEntry[]) => readonly DaPayloadEntry[] = () =>
    (entries) =>
      entries,
) => {
  const normal = [0, 1, 2]
    .map((index) =>
      buildFixtureTransaction({
        spendInputs: [outRefCbor(0x41 + index, 0n)],
        fee: BigInt(index + 1),
      }),
    )
    .sort((left, right) =>
      left.txId < right.txId ? -1 : left.txId > right.txId ? 1 : 0,
    );
  const txIds = normal.map(({ txId }) => txId);
  const forced = buildFixtureTransaction({
    spendInputs: [outRefCbor(0x51, 0n)],
    fee: 9n,
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
      orderKey: ORDER_KEY,
      verdict: "ForcedTxValid",
    },
    additionalTransactions: normal.map(({ canonicalCbor }) =>
      decodeMidgardNativeTxFullFromCanonicalCbor(canonicalCbor),
    ),
    commitEventToStep: commitEventToStep(txIds),
  });
  const evidence = await canonicalBlockEvidenceFromVerifiedPayload({
    observation: authenticatedHeaderObservation(block),
    payloadEnvelopeCbor: block.payloadEnvelopeCbor,
    daProvenance: {
      trustClass: "public_or_permissionless_da",
      sourceId: "detection-event-order",
      grade: "security",
    },
  });
  // The committed transaction list is keyed by id, so it is `txIds`.
  expect(evidence.transactions.map(({ nodeTxId }) => nodeTxId)).toEqual(txIds);
  const found = (
    violationId: string,
    detectionId: string,
    position: bigint,
    subject: Pick<
      CanonicalViolationDetection,
      "frontier" | "subjectEventKeyCbors"
    >,
  ): CanonicalViolationDetection => ({
    ...subject,
    headerHash: evidence.headerHash,
    detectionId,
    violationId,
    position,
  });
  /** The transition-trace family's findings, as its complete replay maps them. */
  const traceFindings = async (): Promise<CanonicalViolationDetection[]> =>
    (await detectTransitionTraceFaults(evidence.reconstruction)).map(
      (detection, index) => ({
        violationId: "transition-trace",
        headerHash: evidence.headerHash,
        ...eventKeyCborSubject(provenTransitionEventKeyCbor(detection)),
        detectionId: transitionTraceDetectionId(index, detection.kind),
        position: BigInt(index),
        provenTransitionEventKeyCbor: provenTransitionEventKeyCbor(detection),
      }),
    );
  return { evidence, txIds, found, traceFindings };
};

const classify = (
  evidence: Awaited<ReturnType<typeof forcedAndNormalEvidence>>["evidence"],
  detections: readonly CanonicalViolationDetection[],
) =>
  classifyCanonicalBlockViolations({
    evidence,
    detections,
    minimumConfirmationDepth: 1,
  });

/**
 * Leaves `txId` unmapped. Admission requires one `event_to_step` member per
 * event, so the block keeps the member count by re-keying the entry to an
 * event it does not contain.
 */
const withoutEntryFor =
  (txId: string) =>
  (entries: readonly DaPayloadEntry[]): readonly DaPayloadEntry[] =>
    entries.map(
      ([key, value]): DaPayloadEntry =>
        key === eventKeyFingerprint(l2EventKey(txId))
          ? [eventKeyFingerprint(l2EventKey("cd".repeat(32))), value]
          : [key, value],
    );

const withValuesSwapped =
  (left: string, right: string) =>
  (entries: readonly DaPayloadEntry[]): readonly DaPayloadEntry[] => {
    const leftKey = eventKeyFingerprint(l2EventKey(left));
    const rightKey = eventKeyFingerprint(l2EventKey(right));
    const value = (key: string) => entries.find(([k]) => k === key)![1];
    return entries.map(
      ([key, entryValue]): DaPayloadEntry =>
        key === leftKey
          ? [key, value(rightKey)]
          : key === rightKey
            ? [key, value(leftKey)]
            : [key, entryValue],
    );
  };

describe("classification on the transition-trace event order", () => {
  it("steps the honest fixture without a transition-trace finding", async () => {
    const { traceFindings } = await forcedAndNormalEvidence();
    expect(await traceFindings()).toEqual([]);
  });

  it("selects the earliest event across frontiers, not the lowest position", async () => {
    const { evidence, txIds, found } = await forcedAndNormalEvidence();
    // Reported at ordinal 3, as the forced frontier is offset past the three
    // normal transactions, but forced transactions precede normal ones.
    const forcedFault = found(
      "unused-redeemer",
      "unused-redeemer:forced:3",
      3n,
      forcedTransactionSubject(ORDER_KEY),
    );
    const normalFault = found(
      "invalid-range",
      `invalid-range:accepted:0:${txIds[0]!}`,
      0n,
      acceptedTransactionSubject(txIds[0]!),
    );
    await expect(
      classify(evidence, [normalFault, forcedFault]),
    ).resolves.toMatchObject({
      decision: "fault_detected",
      category: "unusedRedeemer",
      selected: forcedFault,
    });
  });

  it("orders a transition-trace finding by its event, not its list index", async () => {
    const { evidence, txIds, found } = await forcedAndNormalEvidence();
    // The first entry of the transition-trace list proves the last normal
    // transition; the zero-input fault is at an earlier normal transaction.
    const traceFault = found(
      "transition-trace",
      "transition-trace:0:invalidOneStepTransition",
      0n,
      eventKeyCborSubject(Data.to(l2EventKey(txIds[2]!), EventKey)),
    );
    const earlierFault = found(
      "zero-input",
      `zero-input:accepted:1:${txIds[1]!}`,
      1n,
      acceptedTransactionSubject(txIds[1]!),
    );
    await expect(
      classify(evidence, [traceFault, earlierFault]),
    ).resolves.toMatchObject({
      decision: "fault_detected",
      category: "zeroInput",
      selected: earlierFault,
    });
  });

  it("decides a block whose event_to_step omits an event instead of aborting", async () => {
    let omitted = "";
    const { evidence, found, traceFindings } = await forcedAndNormalEvidence(
      (txIds) => {
        omitted = txIds[1]!;
        return withoutEntryFor(omitted);
      },
    );
    const structural = await traceFindings();
    expect(structural.length).toBeGreaterThan(0);
    expect(structural[0]).toMatchObject({
      detectionId: expect.stringContaining("eventToStepMismatch"),
      frontier: "block",
    });
    const onOmitted = found(
      "invalid-range",
      `invalid-range:accepted:1:${omitted}`,
      1n,
      acceptedTransactionSubject(omitted),
    );
    await expect(
      classify(evidence, [onOmitted, ...structural]),
    ).resolves.toMatchObject({
      decision: "fault_detected",
      category: "transitionTrace",
      selected: structural[0],
    });
    // The omitted event is still an event of the block: another family's
    // finding on it is ordered, and it can be covered.
    await expect(classify(evidence, [onOmitted])).resolves.toMatchObject({
      decision: "fault_detected",
      category: "invalidRange",
      selected: onOmitted,
    });
    const failure = replayPrerequisiteFailure(
      evidence.headerHash,
      l2EventKey(omitted),
      "accepted_terminal",
    ).failures[0]!;
    for (const covering of [[onOmitted], structural])
      expect(() =>
        assertReplayPrerequisiteCovered(evidence, failure, covering),
      ).not.toThrow();
  });

  it("orders by the trace when event_to_step swaps two events", async () => {
    const { evidence, txIds, found, traceFindings } =
      await forcedAndNormalEvidence((ids) =>
        withValuesSwapped(ids[0]!, ids[2]!),
      );
    const structural = await traceFindings();
    expect(structural.length).toBeGreaterThan(0);
    expect(structural.every(({ frontier }) => frontier === "block")).toBe(true);
    const first = found(
      "invalid-range",
      `invalid-range:accepted:0:${txIds[0]!}`,
      0n,
      acceptedTransactionSubject(txIds[0]!),
    );
    const last = found(
      "zero-input",
      `zero-input:accepted:2:${txIds[2]!}`,
      2n,
      acceptedTransactionSubject(txIds[2]!),
    );
    await expect(
      classify(evidence, [last, first, ...structural]),
    ).resolves.toMatchObject({
      decision: "fault_detected",
      category: "transitionTrace",
      selected: structural[0],
    });
    // Without the structural finding, the trace still decides (it steps the
    // transactions in list order here): the committed map would have put the
    // last transaction first.
    await expect(classify(evidence, [last, first])).resolves.toMatchObject({
      decision: "fault_detected",
      category: "invalidRange",
      selected: first,
    });
    const failure = (txId: string) =>
      replayPrerequisiteFailure(
        evidence.headerHash,
        l2EventKey(txId),
        "accepted_terminal",
      ).failures[0]!;
    expect(() =>
      assertReplayPrerequisiteCovered(evidence, failure(txIds[0]!), [last]),
    ).toThrow(CanonicalReplayPrerequisiteError);
    expect(() =>
      assertReplayPrerequisiteCovered(evidence, failure(txIds[2]!), [first]),
    ).not.toThrow();
  });

  it("orders by the trace when a transaction spends an output of a later-listed one", async () => {
    // P is applied first and D spends P's output, but D's id sorts first, so
    // the committed list is [D, P] while the trace steps P before D.
    const producer = buildFixtureTransaction({
      spendInputs: [outRefCbor(0x61, 0n)],
      outputs: [
        encodeMidgardTxOutput({
          address: Buffer.concat([Buffer.from([0x60]), Buffer.alloc(28, 0x62)]),
          value: { lovelace: 2_000_000n, assets: new Map() },
        }),
      ],
      fee: 7n,
    });
    const spendsProducer = encodeMidgardSpendInputItem({
      txId: Buffer.from(producer.txId, "hex"),
      outputIndex: 0,
    });
    const dependent = [...Array(64).keys()]
      .map((index) =>
        buildFixtureTransaction({
          spendInputs: [spendsProducer],
          fee: BigInt(100 + index),
        }),
      )
      .find(({ txId }) => txId < producer.txId);
    if (dependent === undefined)
      throw new Error("no dependent fee sorts before the producer");
    const block = await buildDecodingBlockFixture({
      operatorVkey: "b1".repeat(28),
      startTime: 10n,
      priorLedgerRoot: EMPTY_MERKLE_TREE_ROOT,
      subject: {
        kind: "forced",
        nativeTx: decodeMidgardNativeTxFullFromCanonicalCbor(
          buildFixtureTransaction({
            spendInputs: [outRefCbor(0x51, 0n)],
            fee: 9n,
          }).canonicalCbor,
        ),
        orderKey: ORDER_KEY,
        verdict: "ForcedTxValid",
      },
      additionalTransactions: [producer, dependent].map(({ canonicalCbor }) =>
        decodeMidgardNativeTxFullFromCanonicalCbor(canonicalCbor),
      ),
    });
    const evidence = await canonicalBlockEvidenceFromVerifiedPayload({
      observation: authenticatedHeaderObservation(block),
      payloadEnvelopeCbor: block.payloadEnvelopeCbor,
      daProvenance: {
        trustClass: "public_or_permissionless_da",
        sourceId: "detection-event-order",
        grade: "security",
      },
    });
    expect(evidence.transactions.map(({ nodeTxId }) => nodeTxId)).toEqual([
      dependent.txId,
      producer.txId,
    ]);
    const stepOf = (txId: string) =>
      evidence.reconstruction.transitionTrace.find(
        ({ value }) =>
          eventKeyFingerprint(value.event_key) ===
          eventKeyFingerprint(l2EventKey(txId)),
      )!.key;
    expect(stepOf(producer.txId)).toBeLessThan(stepOf(dependent.txId));
    expect(await detectTransitionTraceFaults(evidence.reconstruction)).toEqual(
      [],
    );
    const found = (
      violationId: string,
      txId: string,
      position: bigint,
    ): CanonicalViolationDetection => ({
      ...acceptedTransactionSubject(txId),
      headerHash: evidence.headerHash,
      detectionId: `${violationId}:accepted:${position.toString()}:${txId}`,
      violationId,
      position,
    });
    const onProducer = found("invalid-range", producer.txId, 1n);
    const onDependent = found("zero-input", dependent.txId, 0n);
    const failure = (txId: string) =>
      replayPrerequisiteFailure(
        evidence.headerHash,
        l2EventKey(txId),
        "prior_transition_effect",
      ).failures[0]!;
    expect(() =>
      assertReplayPrerequisiteCovered(evidence, failure(dependent.txId), [
        onProducer,
      ]),
    ).not.toThrow();
    expect(() =>
      assertReplayPrerequisiteCovered(evidence, failure(producer.txId), [
        onDependent,
      ]),
    ).toThrow(CanonicalReplayPrerequisiteError);
    await expect(
      classify(evidence, [onDependent, onProducer]),
    ).resolves.toMatchObject({
      decision: "fault_detected",
      category: "invalidRange",
      selected: onProducer,
    });
  });

  it("fails closed on a finding whose event is not an event of the block", async () => {
    const { evidence, txIds, found } = await forcedAndNormalEvidence();
    const stray = found(
      "invalid-range",
      "invalid-range:accepted:0:stray",
      0n,
      acceptedTransactionSubject("ab".repeat(32)),
    );
    const present = found(
      "zero-input",
      `zero-input:accepted:1:${txIds[1]!}`,
      1n,
      acceptedTransactionSubject(txIds[1]!),
    );
    await expect(classify(evidence, [present, stray])).rejects.toThrow(
      /is not a source event of this block/u,
    );
    await expect(
      classify(evidence, [
        present,
        found(
          "invalid-range",
          "invalid-range:forced:0:stray",
          0n,
          forcedTransactionSubject({ ...ORDER_KEY, outputIndex: 1n }),
        ),
      ]),
    ).rejects.toThrow(/is not a source event of this block/u);
  });
});
