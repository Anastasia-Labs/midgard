import { computeMidgardNativeTxId } from "@al-ft/midgard-core";
import {
  encodeMidgardForcedTxCanonical,
  materializeMidgardForcedTxFromCanonical,
} from "@al-ft/midgard-core/codec/forced";
import { describe, expect, it } from "vitest";

import { WitnessScriptDecodingResultClasses } from "../src/witness-script-decoding/index.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { nodeForcedVerdict } from "./support/emulator/node-forced-verdict.js";
import { decodingItemFromPayload } from "./support/native-script-decoding-emulator.js";
import { smallCanonicalItem } from "./support/witness-script-decoding-raw.js";
import { makeHarness } from "./witness-script-decoding-lifecycle.make-harness.js";
import {
  acceptedEvidence,
  forcedEvidence,
  reasonOf,
  scanHonestAcceptedToClose,
  shapeOf,
} from "./witness-script-decoding-lifecycle.scan-honest-accepted-to-close.js";

/**
 * A forced WitnessNativeScriptMalformed reason names a field-6 item, and
 * witnessScriptDecoding reopens exactly that item. The fixture puts a sound
 * native script before the malformed one. The verdict is the one the node's
 * classifier writes, so the suite fails if the writer names any item but the
 * one the proof finds malformed: the sound item one position early convicts;
 * the written item is refused on chain.
 */

const shape = shapeOf(
  "all[sig], then [0, h'820700']: a sound native script before a malformed one (Inline)",
  [smallCanonicalItem(), decodingItemFromPayload(Buffer.from("820700", "hex"))],
  1_009n,
);

const writtenScriptIndex = async (): Promise<bigint> => {
  const forced = materializeMidgardForcedTxFromCanonical(shape.nativeTx);
  const verdict = await nodeForcedVerdict({
    transactionId: computeMidgardNativeTxId(forced),
    forcedCanonicalCbor: encodeMidgardForcedTxCanonical(forced),
  });
  expect(verdict).toStrictEqual({
    ForcedTxInvalid: { reason: reasonOf("WitnessNativeScriptMalformed", 1n) },
  });
  return 1n;
};

/**
 * A block committing the fixture under `WitnessNativeScriptMalformed {
 * written + offset }`, with the thread opened on that item.
 */
const setupScenario = async (offset: bigint) => {
  const index = (await writtenScriptIndex()) + offset;
  const reason = reasonOf("WitnessNativeScriptMalformed", index);
  const h = await makeHarness();
  const forced = await h.forcedBlock(shape, reason);
  const evidence = forcedEvidence(
    shape,
    forced.orderKey,
    reason,
    Number(index),
  );
  const bound = await h.step01Forced(
    await h.init(forced.setup.fraudulentBlockOutRef, null, shape.label),
    forced,
    evidence.finding.witnessSetHash,
    index,
  );
  const opened = await h.step02(bound, shape, evidence);
  return { h, forced, evidence, opened };
};

describe("forced WitnessNativeScriptMalformed coordinate the node writes", () => {
  it("convicts a coordinate one item early, where the native script is sound", async () => {
    const { h, forced, evidence, opened } = await setupScenario(-1n);
    expect(evidence.resultClass).toBe(
      WitnessScriptDecodingResultClasses.NoFault,
    );
    const closed = await h.scanToClose(
      opened.nextThreadOutRef,
      evidence,
      null,
      shape.label,
    );
    await h.step04(closed.threadOutRef, evidence, null, shape.label);
    await h.expectThreadsGone(forced.setup.headerHash);
    await h.remove(forced.setup.headerHash, null, shape.label);
  }, 600_000);

  it("refuses the written coordinate on chain", async () => {
    const { h, evidence, opened } = await setupScenario(0n);
    // The written item is the malformed one: the rejection holds.
    expect(evidence.resultClass).toBe(
      WitnessScriptDecodingResultClasses.NativeMalformed,
    );
    expect(evidence.resultClass).toBe(evidence.finding.accusedClass);
    // The prover's planner has no wrongful rejection to scan for, so the
    // scan runs on the arguments an accepted-direction twin of the item plans.
    const closed = await scanHonestAcceptedToClose(
      h,
      opened.nextThreadOutRef,
      evidence,
      acceptedEvidence(shape, 1),
    );
    // The step authenticates the closed scan and the terminal rule, which
    // convicts only a class that differs from the accused one, returns false.
    await expectOnchainRefusal(
      () => h.step04Raw(closed),
      /^Validator returned false$/u,
    );
  }, 600_000);
});
