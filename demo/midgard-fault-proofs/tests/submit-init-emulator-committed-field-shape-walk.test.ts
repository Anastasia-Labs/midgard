/**
 * committed-field-shape step 01 across two batches: the walk's successor is
 * exactly the one the validator derives, on the first step and on a resumed
 * one, and only the thread's prover may advance it.
 */
import { outRefLabel } from "@al-ft/midgard-core";
import {
  CommittedFieldShapeStep01Datum,
  type CommittedFieldShapeStep02State,
  type CommittedFieldShapeWalkState,
  MIDGARD_ENVELOPE_BATCH_ITEMS,
  MIDGARD_FIELD_SHAPE_VERDICT_ADMISSIBLE,
  MIDGARD_FIELD_SHAPE_VERDICT_FIELD_BYTE_BOUND,
  MIDGARD_FIELD_SHAPE_VERDICT_WRONG_STRIDE,
  midgardCommittedFieldShapeVerdict,
} from "@al-ft/midgard-sdk";
import {
  credentialToAddress,
  Data,
  scriptHashToCredential,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  prepareCommittedFieldShapeFromCanonicalTx,
  submitCommittedFieldShapeInit,
  submitCommittedFieldShapeStep01,
  submitCommittedFieldShapeStep02,
} from "../src/index.js";
import {
  committedFieldShapeInlineClaim,
  type CommittedFieldShapeRawSuccessor,
  expectCommittedFieldShapeOnchainRefusal,
  fundCommittedFieldShapeOutsider,
  makeCommittedFieldShapeEmulatorHarness,
  publishCommittedFieldShapeReferenceScripts,
  setupCommittedFieldShapeScenario,
  submitRawCommittedFieldShapeStep01,
} from "./support/committed-field-shape-emulator.js";
import {
  expectSingleUtxoWithUnit,
  network,
} from "./support/submit-init-emulator-shared.js";

const B = MIDGARD_ENVELOPE_BATCH_ITEMS;

describe("committed-field-shape batched walk", () => {
  it("accepts only the derived successor at each step and only from the prover", async () => {
    const harness = await makeCommittedFieldShapeEmulatorHarness();
    const {
      realBlueprint,
      proverLucid,
      proverSigner,
      contracts,
      catalogue,
      committedFieldShape,
      category,
      witnessReferenceScripts,
    } = harness;
    // B + 1 empty items at slot 0: the first batch reads B and owes one, the
    // resumed batch reads the last and finds the stride wrong.
    const scenario = await setupCommittedFieldShapeScenario({
      harness,
      kind: "two-batch-wrong-stride",
    });
    if (scenario.canonicalTx === null) {
      throw new Error("two-batch scenario must be canonical");
    }
    const { committedPreimage } = scenario;
    expect(committedPreimage).toHaveLength(3 + B + 1);
    expect(midgardCommittedFieldShapeVerdict(0, committedPreimage)).toBe(
      MIDGARD_FIELD_SHAPE_VERDICT_WRONG_STRIDE,
    );
    const [step01Ref, step02Ref] =
      await publishCommittedFieldShapeReferenceScripts({
        lucid: harness.funderLucid,
        contracts: committedFieldShape,
      });
    const init = await submitCommittedFieldShapeInit({
      lucid: proverLucid,
      blueprint: realBlueprint,
      network,
      contracts: committedFieldShape,
      category,
      catalogue: {
        policyId: contracts.fraudProofCatalogue.policyId,
        spendingScriptAddress:
          contracts.fraudProofCatalogue.spendingScriptAddress,
        root: catalogue.root,
      },
      signer: proverSigner,
      fraudulentBlockOutRef: scenario.setup.fraudulentBlockOutRef,
      witnessReferenceScripts,
    });
    const unit = init.computationThreadUnit;
    const stepAddress = (step: 0 | 1) =>
      committedFieldShape.steps[step].spendingScriptAddress;
    const threadAt = (step: 0 | 1) =>
      expectSingleUtxoWithUnit(proverLucid, stepAddress(step), unit);
    const foreignScript = credentialToAddress(
      network,
      scriptHashToCredential("ab".repeat(28)),
    );

    const claim = committedFieldShapeInlineClaim({
      fieldIndex: scenario.fieldIndex,
      preimage: committedPreimage,
    });
    const subject = {
      bad_tx_id: scenario.nativeTxId,
      field_index: BigInt(scenario.fieldIndex),
    };
    const cursor = (
      remaining: number,
      offset: number,
    ): CommittedFieldShapeWalkState => ({
      ...subject,
      remaining: BigInt(remaining),
      offset: BigInt(offset),
    });
    const verdict = (code: number): CommittedFieldShapeStep02State => ({
      ...subject,
      verdict: BigInt(code),
    });
    const raw = (
      threadOutRef: string,
      successor: CommittedFieldShapeRawSuccessor,
      signer = proverSigner,
      lucid = proverLucid,
    ) =>
      submitRawCommittedFieldShapeStep01({
        harness,
        threadOutRef,
        scenario,
        claim,
        successor,
        referenceScriptUtxo: step01Ref,
        signer,
        lucid,
      });

    // First step: the derived successor is the cursor after B items.
    const firstStep = outRefLabel(await threadAt(0));
    const afterFirst = cursor(1, 3 + B);
    const firstForgeries: readonly CommittedFieldShapeRawSuccessor[] = [
      { step: 1, state: cursor(0, 3 + B) },
      { step: 1, state: cursor(2, 3 + B) },
      { step: 1, state: cursor(1, 3 + B + 1) },
      { step: 1, state: cursor(1, 3 + B - 1) },
      {
        step: 1,
        state: { ...afterFirst, field_index: subject.field_index + 1n },
      },
      { step: 1, state: { ...afterFirst, bad_tx_id: "00".repeat(32) } },
      { step: 1, state: afterFirst, address: stepAddress(1) },
      { step: 1, state: afterFirst, address: foreignScript },
      { step: 2, state: verdict(MIDGARD_FIELD_SHAPE_VERDICT_WRONG_STRIDE) },
      { step: 2, state: verdict(MIDGARD_FIELD_SHAPE_VERDICT_FIELD_BYTE_BOUND) },
      { step: 2, state: verdict(MIDGARD_FIELD_SHAPE_VERDICT_ADMISSIBLE) },
    ];
    for (const successor of firstForgeries) {
      await expectCommittedFieldShapeOnchainRefusal(() =>
        raw(firstStep, successor),
      );
    }
    expect(outRefLabel(await threadAt(0))).toBe(firstStep);

    const prepared = prepareCommittedFieldShapeFromCanonicalTx({
      tx: scenario.canonicalTx,
      fieldIndex: scenario.fieldIndex,
    });
    const first = await submitCommittedFieldShapeStep01({
      lucid: proverLucid,
      blueprint: realBlueprint,
      contracts: committedFieldShape,
      categoryId: category.categoryId,
      network,
      signer: proverSigner,
      threadOutRef: firstStep,
      stateQueueBlockOutRef: scenario.setup.fraudulentBlockOutRef,
      txInclusion: scenario.inclusion,
      prepared,
      referenceScriptUtxo: step01Ref,
      witnessReferenceScripts,
    });
    expect(first.complete).toBe(false);
    expect(first.step02State).toBeNull();
    expect(first.nextStepAddress).toBe(stepAddress(0));
    const resumed = await threadAt(0);
    expect(outRefLabel(resumed)).toBe(first.nextThreadOutRef);
    expect(Data.from(resumed.datum!, CommittedFieldShapeStep01Datum)).toEqual({
      fraud_prover: proverSigner.paymentKeyHash,
      data: afterFirst,
    });

    // Resumed step: the derived successor is the verdict at step 02.
    const convicting = verdict(MIDGARD_FIELD_SHAPE_VERDICT_WRONG_STRIDE);
    await fundCommittedFieldShapeOutsider(harness);
    expect(harness.outsiderSigner.paymentKeyHash).not.toBe(
      proverSigner.paymentKeyHash,
    );
    await expectCommittedFieldShapeOnchainRefusal(() =>
      raw(
        first.nextThreadOutRef,
        { step: 2, state: convicting },
        harness.outsiderSigner,
        harness.outsiderLucid,
      ),
    );
    const resumedForgeries: readonly CommittedFieldShapeRawSuccessor[] = [
      { step: 2, state: convicting, address: stepAddress(0) },
      { step: 2, state: convicting, address: foreignScript },
      { step: 2, state: verdict(MIDGARD_FIELD_SHAPE_VERDICT_ADMISSIBLE) },
      { step: 2, state: verdict(MIDGARD_FIELD_SHAPE_VERDICT_FIELD_BYTE_BOUND) },
      { step: 1, state: cursor(0, committedPreimage.length) },
      { step: 1, state: afterFirst },
      {
        step: 1,
        state: cursor(0, committedPreimage.length),
        address: stepAddress(1),
      },
    ];
    for (const successor of resumedForgeries) {
      await expectCommittedFieldShapeOnchainRefusal(() =>
        raw(first.nextThreadOutRef, successor),
      );
    }
    expect(outRefLabel(await threadAt(0))).toBe(first.nextThreadOutRef);

    // The outsider's transaction differs from this one only in who signs.
    const second = await raw(first.nextThreadOutRef, {
      step: 2,
      state: convicting,
    });
    const verdictThread = await threadAt(1);
    expect(outRefLabel(verdictThread)).toBe(second.nextThreadOutRef);
    await expect(
      proverLucid.utxosAtWithUnit(stepAddress(0), unit),
    ).resolves.toHaveLength(0);

    const step02 = await submitCommittedFieldShapeStep02({
      lucid: proverLucid,
      contracts: committedFieldShape,
      categoryId: category.categoryId,
      signer: proverSigner,
      threadOutRef: second.nextThreadOutRef,
      referenceScriptUtxo: step02Ref,
      witnessReferenceScripts,
    });
    await expectSingleUtxoWithUnit(
      proverLucid,
      step02.fraudProofAddress,
      step02.fraudProofUnit,
    );
  }, 600_000);
});
