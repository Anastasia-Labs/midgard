import { compareOutRefs } from "@al-ft/midgard-core/out-ref";
import type * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import { WitnessScriptDecodingResultClasses } from "../src/witness-script-decoding/index.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { recordPlutusItem } from "./support/script-decode-emulator.js";
import { makeHarness } from "./witness-script-decoding-lifecycle.make-harness.js";
import {
  acceptedEvidence,
  expectReplayDetection,
  forcedEvidence,
  plutusShape,
  undecodablePlutusShape,
} from "./witness-script-decoding-lifecycle.scan-honest-accepted-to-close.js";

/**
 * Plutus field-6 items close through the decode record of the script they
 * carry: step 02 reads the record as a reference input and takes its class.
 * `undecodablePlutusShape` carries bytes Cardano refuses with class 1, so its
 * result class is 101; `plutusShape` carries a program that decodes.
 */

const undecodableReason = (cls: bigint): SDK.RejectionReason =>
  ({
    PlutusScriptUndecodable: { item_kind: 0n, item_index: 0n, class: cls },
  }) as SDK.RejectionReason;

describe("witnessScriptDecoding Plutus items", () => {
  it("convicts an accepted undecodable Plutus item, refusing a missing, foreign or misplaced record first", async () => {
    const h = await makeHarness();
    const shape = undecodablePlutusShape();
    const { block, setup, inclusionOf } = await h.acceptedBlock(shape);
    const evidence = acceptedEvidence(shape);
    expect(evidence.resultClass).toBe(101);
    expectReplayDetection(block, [shape], evidence, [
      "witness-plutus-script-undecodable",
    ]);
    const emulator = h.harness.emulator;
    const record = await recordPlutusItem(emulator, h.lucid, shape.items[0]!);
    const foreign = await recordPlutusItem(
      emulator,
      h.lucid,
      plutusShape().items[0]!,
    );
    if (record === undefined || foreign === undefined)
      throw new Error("Plutus items take a record");
    const published = await h.publishField(shape, null);

    const bound = await h.step01Accepted(
      await h.init(setup.fraudulentBlockOutRef, null),
      inclusionOf(shape),
      setup.fraudulentBlockOutRef,
      0n,
    );
    await expectOnchainRefusal(() =>
      h.step02Raw(bound, shape, evidence, published),
    );
    await expectOnchainRefusal(() =>
      h.step02Raw(bound, shape, evidence, published, {
        scriptDecodeRecordUtxo: foreign,
      }),
    );
    const sorted = [...published.carriageUtxos, h.ref(1), record].sort(
      compareOutRefs,
    );
    await expectOnchainRefusal(() =>
      h.step02Raw(bound, shape, evidence, published, {
        scriptDecodeRecordUtxo: record,
        recordReferenceInputIndex: BigInt(sorted.indexOf(h.ref(1))),
      }),
    );
    // The same opening with the record in place is accepted.
    const control = await h.step02Raw(bound, shape, evidence, published, {
      scriptDecodeRecordUtxo: record,
    });
    await h.cancel(control.nextThreadOutRef, 2);

    const init = await h.init(
      setup.fraudulentBlockOutRef,
      "accepted-plutus-init",
    );
    const opened = await h.step02(
      await h.step01Accepted(
        init,
        inclusionOf(shape),
        setup.fraudulentBlockOutRef,
        0n,
        "accepted-plutus-step01",
      ),
      shape,
      evidence,
      "accepted-plutus-step02",
    );
    expect(opened.scanState.result_class).toBe(101n);
    const closed = await h.scanToClose(
      opened.nextThreadOutRef,
      evidence,
      "accepted-plutus",
    );
    await h.step04(
      closed.threadOutRef,
      evidence,
      "accepted-plutus-step04-proof-mint",
    );
    await h.expectThreadsGone(setup.headerHash);
    await h.remove(setup.headerHash, "accepted-plutus-remove");
  }, 900_000);

  it("contradicts a wrongful undecodable rejection of a decodable Plutus item", async () => {
    const h = await makeHarness();
    const shape = plutusShape();
    const reason = undecodableReason(1n);
    const forced = await h.forcedBlock(shape, reason);
    const evidence = forcedEvidence(shape, forced.orderKey, reason);
    expect(evidence.finding.accusedClass).toBe(101);
    expect(evidence.resultClass).toBe(
      WitnessScriptDecodingResultClasses.NoFault,
    );
    const opened = await h.step02(
      await h.step01Forced(
        await h.init(forced.setup.fraudulentBlockOutRef, "forced-plutus-init"),
        forced,
        evidence.finding.witnessSetHash,
        0n,
        "forced-plutus-step01",
      ),
      shape,
      evidence,
      "forced-plutus-step02",
    );
    const closed = await h.scanToClose(
      opened.nextThreadOutRef,
      evidence,
      "forced-plutus",
    );
    await h.step04(
      closed.threadOutRef,
      evidence,
      "forced-plutus-step04-proof-mint",
    );
    await h.expectThreadsGone(forced.setup.headerHash);
    await h.remove(forced.setup.headerHash, "forced-plutus-remove");
  }, 900_000);

  it("refuses to contradict an honest undecodable rejection", async () => {
    const h = await makeHarness();
    const shape = undecodablePlutusShape();
    const reason = undecodableReason(1n);
    const forced = await h.forcedBlock(shape, reason);
    const evidence = forcedEvidence(shape, forced.orderKey, reason);
    expect(evidence.resultClass).toBe(evidence.finding.accusedClass);
    const opened = await h.step02(
      await h.step01Forced(
        await h.init(forced.setup.fraudulentBlockOutRef, null),
        forced,
        evidence.finding.witnessSetHash,
        0n,
      ),
      shape,
      evidence,
    );
    const closed = await h.scanToClose(opened.nextThreadOutRef, evidence, null);
    await expectOnchainRefusal(() => h.step04Raw(closed.threadOutRef));
    await h.cancel(closed.threadOutRef, 3);
  }, 900_000);
});
