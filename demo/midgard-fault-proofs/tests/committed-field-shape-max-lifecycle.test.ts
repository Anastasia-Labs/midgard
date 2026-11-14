/**
 * committedFieldShape at the consensus field bound: 32,768-byte committed
 * slots carried by §8.6 certified chunks and walked in batches. The largest
 * off-stride slot is convicted from Init through removal; the same shape at a
 * walked slot walks to verdict 0 and no forwarded verdict convicts it; the
 * honest maximum stride-correct slot refuses a forwarded conviction; the
 * costliest single batch fits one step.
 */
import { MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES } from "@al-ft/midgard-core";
import {
  advanceCommittedFieldShapeWalk,
  CommittedFieldShapeStep01Datum,
  CommittedFieldShapeStep02Datum,
  type CommittedFieldShapeWalkState,
  FraudProofTokenDatum,
  MIDGARD_ENVELOPE_BATCH_ITEMS,
  MIDGARD_FIELD_SHAPE_VERDICT_ADMISSIBLE,
  MIDGARD_FIELD_SHAPE_VERDICT_FIELD_BYTE_BOUND,
  MIDGARD_FIELD_SHAPE_VERDICT_NOT_AN_ENVELOPE,
  MIDGARD_FIELD_SHAPE_VERDICT_WRONG_STRIDE,
  midgardCommittedFieldShapeVerdict,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  prepareCommittedFieldShapeFromCanonicalTx,
  submitCommittedFieldShapeStep01,
  submitCommittedFieldShapeStep02,
  submitRemoveFraudulentBlock,
} from "../src/index.js";
import { expectStageFit } from "./support/certified-committed-field.js";
import {
  type CommittedFieldShapeRawSuccessor,
  expectCommittedFieldShapeOnchainRefusal,
  LARGEST_ITEM_COUNT,
  makeCommittedFieldShapeEmulatorHarness,
  MAX_SLOT0_ITEM_COUNT,
  submitRawCommittedFieldShapeStep01,
  submitRawCommittedFieldShapeStep02,
} from "./support/committed-field-shape-emulator.js";
import {
  blockStays,
  certifiedClaimFor,
  LABEL,
  openThread,
  threadsAt,
} from "./support/committed-field-shape-emulator.max-shape-thread.js";
import {
  buildRemovalDeploymentInfo,
  captureEmulatorSubmission,
  expectSingleUtxoWithUnit,
  network,
  publishRemovalReferenceScripts,
} from "./support/submit-init-emulator-shared.js";

const B = MIDGARD_ENVELOPE_BATCH_ITEMS;

describe("committed-field-shape at the consensus field bound", () => {
  it("convicts the largest off-stride slot in batches, mints proof and removes the block", async () => {
    const harness = await makeCommittedFieldShapeEmulatorHarness();
    const { scenario, step01Ref, step02Ref, init, certified } =
      await openThread(harness, "largest-wrong-stride");
    if (scenario.canonicalTx === null) {
      throw new Error("largest wrong-stride scenario must be canonical");
    }
    expect(scenario.fieldIndex).toBe(0);
    expect(scenario.committedPreimage).toHaveLength(
      MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES,
    );
    const prepared = prepareCommittedFieldShapeFromCanonicalTx({
      tx: scenario.canonicalTx,
      fieldIndex: 0,
    });
    expect(prepared.evidence.committedPreimageByteCount).toBe(32_768);
    expect(prepared.evidence.verdict).toBe(
      MIDGARD_FIELD_SHAPE_VERDICT_WRONG_STRIDE,
    );

    // A redeemer-carried claim cannot address a certified field.
    await expect(
      submitCommittedFieldShapeStep01({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        contracts: harness.committedFieldShape,
        categoryId: harness.category.categoryId,
        network,
        signer: harness.proverSigner,
        threadOutRef: init.nextThreadOutRef,
        stateQueueBlockOutRef: scenario.setup.fraudulentBlockOutRef,
        txInclusion: scenario.inclusion,
        prepared,
        referenceScriptUtxo: step01Ref,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    ).rejects.toThrow();

    let threadOutRef = init.nextThreadOutRef;
    const completions: boolean[] = [];
    for (;;) {
      const step = await captureEmulatorSubmission(harness.emulator, () =>
        submitCommittedFieldShapeStep01({
          lucid: harness.proverLucid,
          blueprint: harness.realBlueprint,
          contracts: harness.committedFieldShape,
          categoryId: harness.category.categoryId,
          network,
          signer: harness.proverSigner,
          threadOutRef,
          stateQueueBlockOutRef: scenario.setup.fraudulentBlockOutRef,
          txInclusion: scenario.inclusion,
          prepared,
          publishedFieldCarriageUtxos: certified.chunks,
          fieldCertificateUtxo: certified.certificateUtxo,
          referenceScriptUtxo: step01Ref,
          witnessReferenceScripts: harness.witnessReferenceScripts,
        }),
      );
      expectStageFit(
        LABEL,
        `step-01-${(completions.length + 1).toString()}`,
        step.measurement,
      );
      expect(step.measurement.referenceInputCount).toBeGreaterThanOrEqual(
        certified.referenceInputs.length + 3,
      );
      completions.push(step.result.complete);
      expect(step.result.step02State === null).toBe(!step.result.complete);
      threadOutRef = step.result.nextThreadOutRef;
      if (step.result.complete) break;
    }
    expect(completions).toHaveLength(Math.ceil(LARGEST_ITEM_COUNT / B));
    expect(completions.slice(0, -1).every((complete) => !complete)).toBe(true);
    const verdictThread = await expectSingleUtxoWithUnit(
      harness.proverLucid,
      harness.committedFieldShape.steps[1].spendingScriptAddress,
      init.computationThreadUnit,
    );
    expect(
      Data.from(verdictThread.datum!, CommittedFieldShapeStep02Datum),
    ).toStrictEqual({
      fraud_prover: harness.proverSigner.paymentKeyHash,
      data: {
        bad_tx_id: scenario.nativeTxId,
        field_index: 0n,
        verdict: BigInt(MIDGARD_FIELD_SHAPE_VERDICT_WRONG_STRIDE),
      },
    });

    const step02 = await captureEmulatorSubmission(harness.emulator, () =>
      submitCommittedFieldShapeStep02({
        lucid: harness.proverLucid,
        contracts: harness.committedFieldShape,
        categoryId: harness.category.categoryId,
        signer: harness.proverSigner,
        threadOutRef,
        referenceScriptUtxo: step02Ref,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
    expectStageFit(LABEL, "step-02-mint", step02.measurement);
    const fraudProofUtxo = await expectSingleUtxoWithUnit(
      harness.proverLucid,
      step02.result.fraudProofAddress,
      step02.result.fraudProofUnit,
    );
    expect(
      Data.from(fraudProofUtxo.datum!, FraudProofTokenDatum),
    ).toStrictEqual({ fraud_prover: harness.proverSigner.paymentKeyHash });

    const removalReferences = await publishRemovalReferenceScripts({
      lucid: harness.proverLucid,
      contracts: harness.contracts,
    });
    const removeNow = BigInt(harness.emulator.now());
    const removal = await captureEmulatorSubmission(harness.emulator, () =>
      submitRemoveFraudulentBlock({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        deploymentInfo: buildRemovalDeploymentInfo(
          harness.contracts,
          harness.catalogue,
          { removalReferenceScripts: removalReferences.published },
        ),
        network,
        signer: harness.proverSigner,
        fraudCategory: "committedFieldShape",
        fraudulentHeaderHash: scenario.setup.headerHash,
        awaitConfirmation: true,
        requireReferenceScripts: true,
        validFrom: removeNow > 120_000n ? removeNow - 120_000n : 0n,
        validTo: removeNow + 300_000n,
      }),
    );
    removal.measurements.forEach((measurement, index) =>
      expectStageFit(LABEL, `remove-${(index + 1).toString()}`, measurement),
    );
    expect(removal.result.fraudCategoryId).toBe(harness.category.categoryId);
    await expect(
      harness.proverLucid.utxosAtWithUnit(
        harness.contracts.stateQueue.spendingScriptAddress,
        scenario.setup.stateQueueBlockUnit,
      ),
    ).resolves.toHaveLength(0);
  }, 1_800_000);

  it("walks the same shape at a walked slot to verdict 0 and refuses every other verdict for it", async () => {
    const harness = await makeCommittedFieldShapeEmulatorHarness();
    const { scenario, step01Ref, step02Ref, init, certified } =
      await openThread(harness, "largest-walked");
    expect(scenario.fieldIndex).toBe(2);
    expect(scenario.committedPreimage).toHaveLength(
      MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES,
    );
    expect(
      midgardCommittedFieldShapeVerdict(2, scenario.committedPreimage),
    ).toBe(MIDGARD_FIELD_SHAPE_VERDICT_ADMISSIBLE);
    const unit = init.computationThreadUnit;
    const claim = certifiedClaimFor(harness, scenario, certified);
    const raw = (
      threadOutRef: string,
      successor: CommittedFieldShapeRawSuccessor,
    ) =>
      captureEmulatorSubmission(harness.emulator, () =>
        submitRawCommittedFieldShapeStep01({
          harness,
          threadOutRef,
          scenario,
          claim,
          successor,
          referenceScriptUtxo: step01Ref,
          carriageReferenceInputs: certified.referenceInputs,
        }),
      );
    const subject = {
      bad_tx_id: scenario.nativeTxId,
      field_index: BigInt(scenario.fieldIndex),
    };

    let threadOutRef = init.nextThreadOutRef;
    let previous: CommittedFieldShapeWalkState | null = null;
    let steps = 0;
    for (;;) {
      const transition = advanceCommittedFieldShapeWalk({
        badTxId: scenario.nativeTxId,
        fieldIndex: scenario.fieldIndex,
        committedPreimage: scenario.committedPreimage,
        previous,
      });
      steps += 1;
      if (transition.kind === "verdict") {
        expect(transition.state.verdict).toBe(
          BigInt(MIDGARD_FIELD_SHAPE_VERDICT_ADMISSIBLE),
        );
        // Every other code, forwarded from the last batch, is refused.
        for (const forged of [
          MIDGARD_FIELD_SHAPE_VERDICT_NOT_AN_ENVELOPE,
          MIDGARD_FIELD_SHAPE_VERDICT_FIELD_BYTE_BOUND,
          MIDGARD_FIELD_SHAPE_VERDICT_WRONG_STRIDE,
        ]) {
          await expectCommittedFieldShapeOnchainRefusal(() =>
            raw(threadOutRef, {
              step: 2,
              state: { ...transition.state, verdict: BigInt(forged) },
            }),
          );
        }
        expect(await threadsAt(harness, 0, unit)).toBe(1);
        expect(await threadsAt(harness, 1, unit)).toBe(0);
        // The identical transaction forwarding the truthful verdict lands.
        const truthful = await raw(threadOutRef, {
          step: 2,
          state: transition.state,
        });
        expectStageFit(
          LABEL,
          `honest-step-01-${steps.toString()}`,
          truthful.measurement,
        );
        threadOutRef = truthful.result.nextThreadOutRef;
        break;
      }
      if (previous === null) {
        // Before any batch is read, a convicting verdict is refused too.
        await expectCommittedFieldShapeOnchainRefusal(() =>
          raw(threadOutRef, {
            step: 2,
            state: {
              ...subject,
              verdict: BigInt(MIDGARD_FIELD_SHAPE_VERDICT_FIELD_BYTE_BOUND),
            },
          }),
        );
      }
      const step = await raw(threadOutRef, {
        step: 1,
        state: transition.state,
      });
      expectStageFit(
        LABEL,
        `honest-step-01-${steps.toString()}`,
        step.measurement,
      );
      const thread = await expectSingleUtxoWithUnit(
        harness.proverLucid,
        harness.committedFieldShape.steps[0].spendingScriptAddress,
        unit,
      );
      expect(
        Data.from(thread.datum!, CommittedFieldShapeStep01Datum).data,
      ).toStrictEqual(transition.state);
      threadOutRef = step.result.nextThreadOutRef;
      previous = transition.state;
    }
    expect(steps).toBe(Math.ceil(LARGEST_ITEM_COUNT / B));
    expect(await threadsAt(harness, 0, unit)).toBe(0);
    expect(await threadsAt(harness, 1, unit)).toBe(1);

    // Verdict 0 finalizes nothing.
    await expectCommittedFieldShapeOnchainRefusal(() =>
      submitRawCommittedFieldShapeStep02({
        harness,
        threadOutRef,
        referenceScriptUtxo: step02Ref,
      }),
    );
    expect(await threadsAt(harness, 1, unit)).toBe(1);
    await blockStays(harness, scenario);
  }, 1_800_000);

  it("refuses a convicting verdict forwarded against the honest maximum stride-correct slot", async () => {
    const harness = await makeCommittedFieldShapeEmulatorHarness();
    const { scenario, step01Ref, step02Ref, init, certified } =
      await openThread(harness, "max-honest");
    // The largest stride-correct slot-0 field: 819 items of 40 bytes, one
    // batch.
    expect(scenario.committedPreimage).toHaveLength(
      3 + MAX_SLOT0_ITEM_COUNT * 40,
    );
    expect(
      midgardCommittedFieldShapeVerdict(0, scenario.committedPreimage),
    ).toBe(MIDGARD_FIELD_SHAPE_VERDICT_ADMISSIBLE);
    const unit = init.computationThreadUnit;
    const forward = (verdict: number) =>
      submitRawCommittedFieldShapeStep01({
        harness,
        threadOutRef: init.nextThreadOutRef,
        scenario,
        claim: certifiedClaimFor(harness, scenario, certified),
        successor: {
          step: 2,
          state: {
            bad_tx_id: scenario.nativeTxId,
            field_index: 0n,
            verdict: BigInt(verdict),
          },
        },
        referenceScriptUtxo: step01Ref,
        carriageReferenceInputs: certified.referenceInputs,
      });

    await expectCommittedFieldShapeOnchainRefusal(() =>
      forward(MIDGARD_FIELD_SHAPE_VERDICT_WRONG_STRIDE),
    );
    expect(await threadsAt(harness, 0, unit)).toBe(1);
    expect(await threadsAt(harness, 1, unit)).toBe(0);

    // The identical transaction forwarding the truthful verdict lands, so the
    // refusal above is the verdict equality and nothing else.
    const truthful = await captureEmulatorSubmission(harness.emulator, () =>
      forward(MIDGARD_FIELD_SHAPE_VERDICT_ADMISSIBLE),
    );
    expectStageFit(LABEL, "honest-step-01", truthful.measurement);
    expect(await threadsAt(harness, 0, unit)).toBe(0);
    expect(await threadsAt(harness, 1, unit)).toBe(1);

    await expectCommittedFieldShapeOnchainRefusal(() =>
      submitRawCommittedFieldShapeStep02({
        harness,
        threadOutRef: truthful.result.nextThreadOutRef,
        referenceScriptUtxo: step02Ref,
      }),
    );
    expect(await threadsAt(harness, 1, unit)).toBe(1);
    await blockStays(harness, scenario);
  }, 900_000);

  it("decides the costliest single batch in one step under the transaction limits", async () => {
    const harness = await makeCommittedFieldShapeEmulatorHarness();
    const { scenario, step01Ref, init, certified } = await openThread(
      harness,
      "worst-batch-wrong-stride",
    );
    if (scenario.canonicalTx === null) {
      throw new Error("worst-batch scenario must be canonical");
    }
    expect(scenario.committedPreimage.length).toBeLessThanOrEqual(
      MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES,
    );
    const prepared = prepareCommittedFieldShapeFromCanonicalTx({
      tx: scenario.canonicalTx,
      fieldIndex: scenario.fieldIndex,
    });
    expect(prepared.evidence.verdict).toBe(
      MIDGARD_FIELD_SHAPE_VERDICT_WRONG_STRIDE,
    );
    const step = await captureEmulatorSubmission(harness.emulator, () =>
      submitCommittedFieldShapeStep01({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        contracts: harness.committedFieldShape,
        categoryId: harness.category.categoryId,
        network,
        signer: harness.proverSigner,
        threadOutRef: init.nextThreadOutRef,
        stateQueueBlockOutRef: scenario.setup.fraudulentBlockOutRef,
        txInclusion: scenario.inclusion,
        prepared,
        publishedFieldCarriageUtxos: certified.chunks,
        fieldCertificateUtxo: certified.certificateUtxo,
        referenceScriptUtxo: step01Ref,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
    expectStageFit(LABEL, "worst-batch-step-01", step.measurement);
    expect(step.result.complete).toBe(true);
    expect(step.result.step02State?.verdict).toBe(
      BigInt(MIDGARD_FIELD_SHAPE_VERDICT_WRONG_STRIDE),
    );
  }, 900_000);
});
