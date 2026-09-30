import { decodeMidgardNativeTxFullFromCanonicalCbor } from "@al-ft/midgard-core";
import { EMPTY_MERKLE_TREE_ROOT, EventKey } from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { canonicalBlockEvidenceFromVerifiedPayload } from "../src/evidence/canonical-block-evidence.js";
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
  authenticatedHeaderObservation,
  buildFixtureTransaction,
  outRefCbor,
} from "./helpers/canonical-block-evidence-fixture.js";
import { buildDecodingBlockFixture } from "./support/native-script-decoding-emulator.js";

const ORDER_KEY = { transactionId: "31".repeat(32), outputIndex: 0n };

/** One forced transaction beside three normal ones. */
const forcedAndNormalEvidence = async () => {
  const normal = [0, 1, 2].map((index) =>
    buildFixtureTransaction({
      spendInputs: [outRefCbor(0x41 + index, 0n)],
      fee: BigInt(index + 1),
    }),
  );
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
  // The fixture steps the forced transaction first, then the normal ones in
  // the order given, while the committed transaction vector is keyed by id.
  const txIds = normal.map(({ txId }) => txId);
  const vectorIndex = (txId: string) =>
    BigInt(
      evidence.transactions.findIndex(({ nodeTxId }) => nodeTxId === txId),
    );
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
  return { evidence, txIds, vectorIndex, found };
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

describe("classification on the authenticated event order", () => {
  it("selects the earliest step across frontiers, not the lowest position", async () => {
    const { evidence, found } = await forcedAndNormalEvidence();
    const first = evidence.transactions[0]!.nodeTxId;
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
      `invalid-range:accepted:0:${first}`,
      0n,
      acceptedTransactionSubject(first),
    );
    await expect(
      classify(evidence, [normalFault, forcedFault]),
    ).resolves.toMatchObject({
      decision: "fault_detected",
      category: "unusedRedeemer",
      selected: forcedFault,
    });
  });

  it("orders a transition-trace finding by its event's step, not its list index", async () => {
    const { evidence, txIds, vectorIndex, found } =
      await forcedAndNormalEvidence();
    // The first entry of the transition-trace list proves the last normal
    // transition, while the zero-input fault sits at the first normal step
    // but at a later ordinal of the id-keyed transaction vector.
    const traceFault = found(
      "transition-trace",
      "transition-trace:0:invalidOneStepTransition",
      0n,
      eventKeyCborSubject(
        Data.to({ L2TransactionEventKey: { tx_id: txIds[2]! } }, EventKey),
      ),
    );
    const earlierFault = found(
      "zero-input",
      `zero-input:accepted:${vectorIndex(txIds[0]!)}:${txIds[0]!}`,
      vectorIndex(txIds[0]!),
      acceptedTransactionSubject(txIds[0]!),
    );
    expect(earlierFault.position).toBeGreaterThan(traceFault.position);
    await expect(
      classify(evidence, [traceFault, earlierFault]),
    ).resolves.toMatchObject({
      decision: "fault_detected",
      category: "zeroInput",
      selected: earlierFault,
    });
  });

  it("fails closed on a finding whose event the authenticated trace does not map", async () => {
    const { evidence, txIds, found } = await forcedAndNormalEvidence();
    const stray = found(
      "invalid-range",
      "invalid-range:accepted:0:stray",
      0n,
      acceptedTransactionSubject("ab".repeat(32)),
    );
    const mapped = found(
      "zero-input",
      `zero-input:accepted:1:${txIds[1]!}`,
      1n,
      acceptedTransactionSubject(txIds[1]!),
    );
    await expect(classify(evidence, [mapped, stray])).rejects.toThrow(
      /no step in the authenticated transition trace/u,
    );
    await expect(
      classify(evidence, [
        mapped,
        found(
          "invalid-range",
          "invalid-range:forced:0:stray",
          0n,
          forcedTransactionSubject({ ...ORDER_KEY, outputIndex: 1n }),
        ),
      ]),
    ).rejects.toThrow(/no step in the authenticated transition trace/u);
  });
});
