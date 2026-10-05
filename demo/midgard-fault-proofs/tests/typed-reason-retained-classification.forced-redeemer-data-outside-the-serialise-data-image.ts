import {
  decodeMidgardNativeTxFullFromCanonicalCbor,
  encodeMidgardRedeemerWitnessItem,
} from "@al-ft/midgard-core/codec";
import { GENESIS_HEADER_HASH } from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import { canonicalBlockEvidenceFromVerifiedPayload } from "../src/evidence/canonical-block-evidence.js";
import { classifyCanonicalBlockViolations } from "../src/workflow/classification.js";
import {
  admitCompleteCanonicalReplayPredecessor,
  admitValidationTraceReplayContext,
  createCompleteCanonicalReplayUnion,
  REDEEMER_CANONICITY_COMPLETE_CANONICAL_REPLAY,
  requireCompleteCanonicalReplayDecision,
  VALIDATION_TRACE_DISPUTE_COMPLETE_CANONICAL_REPLAY,
} from "../src/workflow/complete-replay.js";
import { forcedTransactionSubject } from "../src/workflow/detection-subject.js";
import {
  authenticatedHeaderObservation,
  buildCanonicalBlockFixture,
  buildFixtureTransaction,
  outRefCbor,
} from "./helpers/canonical-block-evidence-fixture.js";
import {
  buildRetainedValidationBlockFixture,
  captureRetainedPlutusIdentityOrigins,
} from "./support/retained-reason-classifier.js";
import {
  base,
  output,
  policy,
} from "./typed-reason-retained-classification.reason-case.js";

const daProvenance = {
  trustClass: "public_or_permissionless_da",
  sourceId: "retained-fixture/emulator",
  grade: "security",
} as const;

const orderKey = { transactionId: "52".repeat(32), outputIndex: 0n };

/**
 * Replays and classifies a block whose only event is a forced transaction the
 * operator committed ForcedTxValid, carrying the given redeemer data. The
 * forced origin is admitted through the real raw-L1 event capture.
 */
const classifyAcceptedForcedRedeemer = async (redeemerData: string) => {
  const predecessor = await buildCanonicalBlockFixture({
    transactions: [],
    prevHeaderHash: GENESIS_HEADER_HASH,
    utxos: [{ key: outRefCbor(71, 0n), value: output() }],
  });
  const transaction = buildFixtureTransaction({
    ...base,
    redeemerWitnesses: [
      encodeMidgardRedeemerWitnessItem({
        purpose: "Spend",
        index: 0n,
        redeemerCbor: Buffer.from(redeemerData, "hex"),
        executionUnits: { memory: 1n, steps: 2n },
      }),
    ],
  });
  const block = await buildRetainedValidationBlockFixture({
    subject: {
      kind: "forced",
      nativeTx: decodeMidgardNativeTxFullFromCanonicalCbor(
        transaction.canonicalCbor,
      ),
      orderKey,
      verdict: "ForcedTxValid",
    },
    priorLedgerRoot: predecessor.header.utxosRoot,
    prevHeaderHash: predecessor.headerHash,
    blockEndTimeMs: 1_750_000_000_000,
    blockSlot: 100n,
  });
  const evidence = await canonicalBlockEvidenceFromVerifiedPayload({
    observation: authenticatedHeaderObservation(block),
    payloadEnvelopeCbor: block.payloadEnvelopeCbor,
    daProvenance,
    minimumConfirmationDepth: policy.confirmationDepth,
  });
  const admittedPredecessor = await admitCompleteCanonicalReplayPredecessor({
    value: {
      observation: authenticatedHeaderObservation(predecessor),
      payloadEnvelopeCborHex: predecessor.payloadEnvelopeCbor.toString("hex"),
      daProvenance,
    },
    currentEvidence: evidence,
    minimumConfirmationDepth: policy.confirmationDepth,
  });
  const transitionTraceEvents = await captureRetainedPlutusIdentityOrigins({
    block,
    transaction,
    orderKey,
  });
  const context = {
    predecessor: admittedPredecessor,
    transitionTraceEvents,
    validationTraceReplay: await admitValidationTraceReplayContext({
      evidence,
      predecessor: admittedPredecessor,
      transitionTraceEvents,
    }),
  };
  const replayer = createCompleteCanonicalReplayUnion([
    VALIDATION_TRACE_DISPUTE_COMPLETE_CANONICAL_REPLAY,
    REDEEMER_CANONICITY_COMPLETE_CANONICAL_REPLAY,
  ]);
  // A replay prerequisite error is frozen, which hides its message from the
  // test reporter; restate it so a failure names the unresolved prerequisite.
  const decision = await replayer.replay(evidence, context).catch((error) => {
    throw new Error(
      `complete replay did not decide the block: ${error instanceof Error ? `${error.name}: ${error.message}` : String(error)}`,
    );
  });
  const detections = requireCompleteCanonicalReplayDecision({
    evidence,
    replayer,
    decision,
    context,
  });
  return await classifyCanonicalBlockViolations({
    evidence,
    detections,
    minimumConfirmationDepth: policy.confirmationDepth,
  });
};

describe("accepted forced redeemer data outside the serialiseData image", () => {
  it("convicts a forced transaction committed ForcedTxValid whose redeemer data is not canonical", async () => {
    // The redeemer rule binds a committed forced verdict exactly as it binds
    // an accepted L2 verdict: accepting non-canonical data is a wrongful
    // acceptance at the forced event, and that finding also covers the
    // validation replay's missing trace for the same event.
    const classification = await classifyAcceptedForcedRedeemer("d8798101");
    expect(classification.decision).toBe("fault_detected");
    if (classification.decision !== "fault_detected")
      throw new Error("unreachable");
    expect(classification.category).toBe("redeemerCanonicity");
    expect(classification.selected).toMatchObject({
      violationId: "redeemer-malformed",
      position: 0n,
      ...forcedTransactionSubject(orderKey),
    });
    expect(classification.selected.detectionId).toMatch(
      /^redeemer-malformed:forced:0:0:/u,
    );
  });

  it("leaves a forced transaction committed ForcedTxValid with canonical redeemer data healthy", async () => {
    const classification = await classifyAcceptedForcedRedeemer("d87980");
    expect(classification.decision).toBe("no_fault_detected");
  });
});
