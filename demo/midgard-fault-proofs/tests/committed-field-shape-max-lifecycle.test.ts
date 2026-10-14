/**
 * committedFieldShape at the consensus field bound: a 32,768-byte committed
 * slot carried by §8.6 certified chunks, proven from Init through removal, and
 * the honest maximum commitment that no forwarded verdict can convict.
 */
import {
  deriveMidgardNativeTxWitnessSetCompact,
  encodeMidgardNativeTxWitnessSetCompact,
  MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES,
} from "@al-ft/midgard-core";
import {
  type CommittedFieldClaim,
  CommittedFieldShapeStep02Datum,
  FraudProofTokenDatum,
  MIDGARD_FIELD_SHAPE_VERDICT_ADMISSIBLE,
  MIDGARD_FIELD_SHAPE_VERDICT_WRONG_STRIDE,
  midgardCommittedFieldShapeVerdict,
} from "@al-ft/midgard-sdk";
import { Data, type UTxO } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { faultProofRawFieldCarriage } from "../src/field-opening.js";
import {
  prepareCommittedFieldShapeFromCanonicalTx,
  submitCommittedFieldShapeInit,
  submitCommittedFieldShapeStep01,
  submitCommittedFieldShapeStep02,
  submitRemoveFraudulentBlock,
} from "../src/index.js";
import {
  type CertifiedCommittedField,
  expectStageFit,
  publishCertifiedCommittedField,
} from "./support/certified-committed-field.js";
import {
  type CommittedFieldShapeEmulatorHarness,
  type CommittedFieldShapeScenario,
  expectCommittedFieldShapeOnchainRefusal,
  makeCommittedFieldShapeEmulatorHarness,
  MAX_SLOT0_ITEM_COUNT,
  publishCommittedFieldShapeReferenceScripts,
  setupCommittedFieldShapeScenario,
  submitRawCommittedFieldShapeStep01,
  submitRawCommittedFieldShapeStep02,
} from "./support/committed-field-shape-emulator.js";
import {
  buildRemovalDeploymentInfo,
  captureEmulatorSubmission,
  expectSingleUtxoWithUnit,
  network,
  publishRemovalReferenceScripts,
} from "./support/submit-init-emulator-shared.js";

const LABEL = "committed-field-shape-max";

const initThread = async (
  harness: CommittedFieldShapeEmulatorHarness,
  scenario: CommittedFieldShapeScenario,
) =>
  await captureEmulatorSubmission(harness.emulator, () =>
    submitCommittedFieldShapeInit({
      lucid: harness.proverLucid,
      blueprint: harness.realBlueprint,
      network,
      contracts: harness.committedFieldShape,
      category: harness.category,
      catalogue: {
        policyId: harness.contracts.fraudProofCatalogue.policyId,
        spendingScriptAddress:
          harness.contracts.fraudProofCatalogue.spendingScriptAddress,
        root: harness.catalogue.root,
      },
      signer: harness.proverSigner,
      fraudulentBlockOutRef: scenario.setup.fraudulentBlockOutRef,
      witnessReferenceScripts: harness.witnessReferenceScripts,
    }),
  );

const certifySlot0 = async (
  harness: CommittedFieldShapeEmulatorHarness,
  scenario: CommittedFieldShapeScenario,
): Promise<CertifiedCommittedField> =>
  await publishCertifiedCommittedField({
    emulator: harness.emulator,
    lucid: harness.proverLucid,
    signer: harness.proverSigner,
    certificate: harness.contracts.fieldPreimageCertificate,
    nativeTxId: scenario.nativeTxId,
    fieldIndex: 0,
    preimage: scenario.committedPreimage,
    compactCbor: scenario.compactCbor,
    witnessSetCompactCbor: encodeMidgardNativeTxWitnessSetCompact(
      deriveMidgardNativeTxWitnessSetCompact(scenario.fullTx!.witnessSet),
    ).toString("hex"),
    label: LABEL,
  });

const threadsAt = async (
  harness: CommittedFieldShapeEmulatorHarness,
  stepIndex: 0 | 1,
  unit: string,
): Promise<number> =>
  (
    await harness.proverLucid.utxosAtWithUnit(
      harness.committedFieldShape.steps[stepIndex].spendingScriptAddress,
      unit,
    )
  ).length;

describe("committed-field-shape at the consensus field bound", () => {
  it("convicts a 32,768-byte off-stride slot through certified carriage, mints proof and removes the block", async () => {
    const harness = await makeCommittedFieldShapeEmulatorHarness();
    const scenario = await setupCommittedFieldShapeScenario({
      harness,
      kind: "max-wrong-stride",
    });
    if (scenario.canonicalTx === null) {
      throw new Error("maximum wrong-stride scenario must be canonical");
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

    const [step01Reference, step02Reference] =
      await publishCommittedFieldShapeReferenceScripts({
        lucid: harness.funderLucid,
        contracts: harness.committedFieldShape,
      });
    const init = await initThread(harness, scenario);
    expectStageFit(LABEL, "init", init.measurement);
    const certified = await certifySlot0(harness, scenario);
    expect(certified.plan.tier).toBe("Certified");
    expect(certified.chunks.length).toBeGreaterThan(1);

    // A redeemer-carried claim cannot address a certified field.
    await expect(
      submitCommittedFieldShapeStep01({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        contracts: harness.committedFieldShape,
        categoryId: harness.category.categoryId,
        network,
        signer: harness.proverSigner,
        threadOutRef: init.result.nextThreadOutRef,
        stateQueueBlockOutRef: scenario.setup.fraudulentBlockOutRef,
        txInclusion: scenario.inclusion,
        prepared,
        referenceScriptUtxo: step01Reference,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    ).rejects.toThrow();

    const step01 = await captureEmulatorSubmission(harness.emulator, () =>
      submitCommittedFieldShapeStep01({
        lucid: harness.proverLucid,
        blueprint: harness.realBlueprint,
        contracts: harness.committedFieldShape,
        categoryId: harness.category.categoryId,
        network,
        signer: harness.proverSigner,
        threadOutRef: init.result.nextThreadOutRef,
        stateQueueBlockOutRef: scenario.setup.fraudulentBlockOutRef,
        txInclusion: scenario.inclusion,
        prepared,
        publishedFieldCarriageUtxos: certified.chunks,
        fieldCertificateUtxo: certified.certificateUtxo,
        referenceScriptUtxo: step01Reference,
        witnessReferenceScripts: harness.witnessReferenceScripts,
      }),
    );
    expectStageFit(LABEL, "step-01", step01.measurement);
    expect(step01.measurement.referenceInputCount).toBeGreaterThanOrEqual(
      certified.referenceInputs.length + 3,
    );
    const secondStep = await expectSingleUtxoWithUnit(
      harness.proverLucid,
      harness.committedFieldShape.steps[1].spendingScriptAddress,
      init.result.computationThreadUnit,
    );
    expect(
      Data.from(secondStep.datum!, CommittedFieldShapeStep02Datum),
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
        threadOutRef: step01.result.nextThreadOutRef,
        referenceScriptUtxo: step02Reference,
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
    expect(removal.result.fraudCategoryId).toBe("00000012");
    await expect(
      harness.proverLucid.utxosAtWithUnit(
        harness.contracts.stateQueue.spendingScriptAddress,
        scenario.setup.stateQueueBlockUnit,
      ),
    ).resolves.toHaveLength(0);
  }, 900_000);

  it("refuses a convicting verdict forwarded against an honest maximum slot at the verdict check", async () => {
    const harness = await makeCommittedFieldShapeEmulatorHarness();
    const scenario = await setupCommittedFieldShapeScenario({
      harness,
      kind: "max-honest",
    });
    // The largest stride-correct slot-0 field: 819 items of 40 bytes.
    expect(scenario.committedPreimage).toHaveLength(
      3 + MAX_SLOT0_ITEM_COUNT * 40,
    );
    expect(
      midgardCommittedFieldShapeVerdict(0, scenario.committedPreimage),
    ).toBe(MIDGARD_FIELD_SHAPE_VERDICT_ADMISSIBLE);
    const [step01Reference, step02Reference] =
      await publishCommittedFieldShapeReferenceScripts({
        lucid: harness.funderLucid,
        contracts: harness.committedFieldShape,
      });
    const init = await initThread(harness, scenario);
    const unit = init.result.computationThreadUnit;
    const certified = await certifySlot0(harness, scenario);
    const certifiedClaim = (
      referenceInputs: readonly UTxO[],
    ): CommittedFieldClaim => ({
      BodyFieldClaim: {
        field_index: 0n,
        carriage: faultProofRawFieldCarriage({
          plan: certified.plan,
          referenceInputs,
          certificatePolicyId:
            harness.contracts.fieldPreimageCertificate.policyId,
          label: LABEL,
        }),
      },
    });
    const forward = (verdict: number) =>
      submitRawCommittedFieldShapeStep01({
        harness,
        threadOutRef: init.result.nextThreadOutRef,
        scenario,
        claim: certifiedClaim,
        forwardedState: {
          bad_tx_id: scenario.nativeTxId,
          field_index: 0n,
          verdict: BigInt(verdict),
        },
        referenceScriptUtxo: step01Reference,
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
        referenceScriptUtxo: step02Reference,
      }),
    );
    expect(await threadsAt(harness, 1, unit)).toBe(1);
    await expect(
      harness.proverLucid.utxosAtWithUnit(
        harness.contracts.stateQueue.spendingScriptAddress,
        scenario.setup.stateQueueBlockUnit,
      ),
    ).resolves.toHaveLength(1);
  }, 900_000);
});
