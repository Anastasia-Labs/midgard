import {
  computeMidgardNativeTxId,
  encodeMidgardFieldPreimage,
  encodeMidgardSpendInputItem,
  materializeMidgardNativeTxFromCanonical,
} from "@al-ft/midgard-core";
import {
  encodeMidgardForcedTxCanonical,
  materializeMidgardForcedTxFromCanonical,
} from "@al-ft/midgard-core/codec/forced";
import { describe, expect, it } from "vitest";

import { observerOrderInvalidEvidenceCloses } from "../src/observer-order-invalid/family.js";
import {
  evidenceOf,
  forcedBlock,
  forcedSuccess,
  reasonAt,
  stagedOf,
} from "./observer-order-invalid-lifecycle.forced-success.js";
import { makeHarness } from "./observer-order-invalid-lifecycle.make-harness.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { makeNativeTx } from "./support/emulator/native-tx.js";
import { nodeForcedVerdict } from "./support/emulator/node-forced-verdict.js";
import {
  observerAt,
  type ObserverFieldShape,
} from "./support/observer-order-invalid-raw.js";

/**
 * A forced ObserverOrderInvalid reason names the field-3 ordinal whose
 * observer does not strictly follow its predecessor, and
 * observerOrderInvalid reopens exactly that ordinal. The fixture's observers
 * ascend at ordinal 1 and descend at ordinal 2, and the transaction spends an
 * input so the classifier reaches the observer check. The verdict is the one
 * the node's classifier writes, so the suite fails if the writer names any
 * ordinal but the offending one: one ordinal early is ordered and convicts;
 * the written ordinal is refused on chain.
 */

const observers = [observerAt(0), observerAt(2), observerAt(1)];

const shape: ObserverFieldShape = (() => {
  const fieldPreimage = encodeMidgardFieldPreimage(observers);
  const base = makeNativeTx({
    spendInputCbors: [
      encodeMidgardSpendInputItem({
        txId: Buffer.alloc(32, 0x5a),
        outputIndex: 0,
      }),
    ],
    fee: 19n,
  });
  const nativeTx = materializeMidgardNativeTxFromCanonical({
    version: base.version,
    validity: base.validity,
    body: { ...base.body, requiredObserversPreimageCbor: fieldPreimage },
    witnessSet: base.witnessSet,
  });
  return Object.freeze({
    label: "3 observers, ascending at ordinal 1, descending at ordinal 2",
    observers: Object.freeze([...observers]),
    observerCount: observers.length,
    fee: 19n,
    nativeTx,
    fieldPreimage,
  });
})();

const writtenObserverIndex = async (): Promise<number> => {
  const forced = materializeMidgardForcedTxFromCanonical(shape.nativeTx);
  const verdict = await nodeForcedVerdict({
    transactionId: computeMidgardNativeTxId(forced),
    forcedCanonicalCbor: encodeMidgardForcedTxCanonical(forced),
  });
  expect(verdict).toStrictEqual({ ForcedTxInvalid: { reason: reasonAt(2) } });
  return 2;
};

describe("forced ObserverOrderInvalid coordinate the node writes", () => {
  it("convicts a coordinate one ordinal early, where the observers ascend", async () => {
    // Init, bind, open, scan, proof mint and removal of the committing block.
    await forcedSuccess(
      "coordinate-early",
      shape,
      (await writtenObserverIndex()) - 1,
    );
  }, 900_000);

  it("refuses the written coordinate on chain", async () => {
    const index = await writtenObserverIndex();
    const h = await makeHarness();
    const { setup, finding, source } = await forcedBlock(h, shape, index);
    const evidence = evidenceOf(shape, finding);
    const staged = stagedOf(shape, index);
    // The written ordinal descends: the rejection holds.
    expect(evidence.violation).toBe(true);
    expect(observerOrderInvalidEvidenceCloses(evidence)).toBe(false);
    const bound = await h.step01Forced(
      h.threadOf(
        (await h.init(setup.fraudulentBlockOutRef, setup.headerHash)).result,
      ),
      finding,
      source,
    );
    await h.publishField(shape);
    let cursor = (
      await h.step02(bound.result.nextThreadOutRef, evidence, shape, staged)
    ).result.nextThreadOutRef;
    for (let ordinal = 0; ordinal < staged.walk.length; ordinal += 1)
      cursor = (await h.step03(cursor, evidence, shape, staged, ordinal)).result
        .nextThreadOutRef;
    // The scan decided a violation at the cited ordinal. Step 04 returns the
    // terminal rule, which convicts a forced rejection only when that ordinal
    // is ordered, so it returns false.
    await expectOnchainRefusal(() => h.step04Raw(cursor), {
      refusedBy: "fraud_proofs/observer_order_invalid/step_04",
      check: /^Validator returned false$/u,
    });
  }, 900_000);
});
