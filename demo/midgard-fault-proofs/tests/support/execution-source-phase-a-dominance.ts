import {
  encodeMidgardForcedTxCanonical,
  materializeMidgardForcedTxFromCanonical,
} from "@al-ft/midgard-core/codec/forced";
import { expect } from "vitest";

import { buildExecutionSourceMachineAuthentication } from "../../src/execution-source-script-decoding/index.js";
import { WitnessScriptDecodingResultClasses } from "../../src/witness-script-decoding/index.js";
import { makeHarness } from "../witness-script-decoding-lifecycle.make-harness.js";
import {
  acceptedEvidence,
  forcedEvidence,
  reasonOf,
  scanHonestAcceptedToClose,
  shapeOf,
} from "../witness-script-decoding-lifecycle.scan-honest-accepted-to-close.js";
import { expectOnchainRefusal } from "./emulator/expect-onchain-refusal.js";
import { nodeForcedVerdict } from "./emulator/node-forced-verdict.js";
import { buildSubjectReplay } from "./execution-source-script-decoding-emulator.js";
import {
  buildRetainedValidationBlockFixture,
  retainValidationTrace,
} from "./retained-reason-classifier.js";
import { witnessSetCarriageOf } from "./witness-script-decoding-raw.js";

/** The public replay must reject malformed inline bytes before execution. */
export const malformedInlinePhaseAReplay = async (
  item: Buffer,
  direction: "accepted" | "forced" = "accepted",
) => {
  const replay = await buildSubjectReplay({
    blockContext: {
      operatorVkey: "b1".repeat(28),
      startTime: 1_749_999_941_000n,
    },
    direction,
    item: { kind: "raw", item },
  });
  expect(replay.canonical.verdict).toBe("rejected");
  expect(replay.canonical.code).toBe("E_INVALID_FIELD_TYPE");
  expect(
    replay.canonical.trace.witnesses.some(
      ({ phase }) => phase === "phaseANativeScripts",
    ),
  ).toBe(true);
  expect(
    replay.canonical.trace.witnesses.some(
      ({ auxiliary }) => auxiliary?.kind === "nativeExecutionDescriptor",
    ),
  ).toBe(false);
  const reason = reasonOf("WitnessNativeScriptMalformed", 0n);
  expect(
    await nodeForcedVerdict({
      transactionId: replay.transaction.txId,
      forcedCanonicalCbor: encodeMidgardForcedTxCanonical(
        materializeMidgardForcedTxFromCanonical(replay.transaction.tx),
      ),
    }),
  ).toStrictEqual({ ForcedTxInvalid: { reason } });
  await expect(
    buildExecutionSourceMachineAuthentication({
      trace: replay.canonical.trace,
      eventKey: replay.eventKey,
      claimedVerdict: direction === "accepted" ? "accepted" : "rejected",
      claimedRejectionCode:
        direction === "accepted" ? null : "E_INVALID_FIELD_TYPE",
    }),
  ).rejects.toThrow("replay has no native execution descriptor");
  const shape = {
    ...shapeOf(
      "malformed inline native witness with a well-formed spend input",
      [item],
      0n,
    ),
    nativeTx: replay.transaction.tx,
    txId: replay.transaction.txId.toString("hex"),
    carriage: witnessSetCarriageOf(replay.transaction.tx),
  };
  return { ...replay, shape, reason };
};

/** Exercise the actual earlier proof family, preserving both polarities. */
export const proveMalformedInlinePhaseAWitness = async (
  item: Buffer,
  direction: "accepted" | "forced",
) => {
  const { shape, reason } = await malformedInlinePhaseAReplay(item, direction);
  const h = await makeHarness();
  if (direction === "accepted") {
    const block = await h.acceptedBlock(shape);
    const evidence = acceptedEvidence(shape);
    expect(evidence.resultClass).toBe(
      WitnessScriptDecodingResultClasses.NativeMalformed,
    );
    const bound = await h.step01Accepted(
      await h.init(block.setup.fraudulentBlockOutRef, null, shape.label),
      block.inclusionOf(shape),
      block.setup.fraudulentBlockOutRef,
      0n,
    );
    const opened = await h.step02(bound, shape, evidence);
    const closed = await h.scanToClose(
      opened.nextThreadOutRef,
      evidence,
      null,
      shape.label,
    );
    await h.step04(closed.threadOutRef, evidence, null, shape.label);
    await h.expectThreadsGone(block.setup.headerHash);
    await h.remove(block.setup.headerHash, null, shape.label);
  } else {
    const block = await h.forcedBlock(shape, reason);
    const evidence = forcedEvidence(shape, block.orderKey, reason);
    const bound = await h.step01Forced(
      await h.init(block.setup.fraudulentBlockOutRef, null, shape.label),
      block,
      evidence.finding.witnessSetHash,
      0n,
    );
    const opened = await h.step02(bound, shape, evidence);
    const closed = await scanHonestAcceptedToClose(
      h,
      opened.nextThreadOutRef,
      evidence,
      acceptedEvidence(shape),
    );
    // The authenticated rejection equals the scanner verdict; mint must refuse.
    await expectOnchainRefusal(() => h.step04Raw(closed), {
      refusedBy: "fraud_proofs/witness_script_decoding/step_04",
      check: /^Validator returned false$/u,
    });
  }
};

export const malformedInlineRetainedFixture = async (
  direction: "accepted" | "honest",
) => {
  const replay = await malformedInlinePhaseAReplay(
    Buffer.from("820043820700", "hex"),
    direction === "accepted" ? "accepted" : "forced",
  );
  const traceEntries = retainValidationTrace({
    trace: replay.canonical.trace,
    eventKey: replay.eventKey,
    claim:
      direction === "accepted"
        ? { verdict: "accepted" }
        : { verdict: "rejected", reason: replay.reason },
  });
  return await buildRetainedValidationBlockFixture({
    subject:
      direction === "accepted"
        ? { kind: "normal", nativeTx: replay.transaction.tx }
        : {
            kind: "forced",
            nativeTx: replay.transaction.tx,
            orderKey: replay.orderKey,
            verdict: { ForcedTxInvalid: { reason: replay.reason } },
          },
    priorLedgerRoot: replay.priorLedgerRoot,
    descriptorEntries: traceEntries.descriptorEntries,
    retainedEntries: traceEntries.retainedEntries,
    blockEndTimeMs: 1_750_000_001_000,
    blockSlot: 0n,
  });
};
