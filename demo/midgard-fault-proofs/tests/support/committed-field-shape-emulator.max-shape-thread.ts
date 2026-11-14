/**
 * Thread setup shared by the committed-field-shape max-shape lifecycle: one
 * scenario, both step scripts published, Init, and the field certified.
 */
import {
  deriveMidgardNativeTxWitnessSetCompact,
  encodeMidgardNativeTxWitnessSetCompact,
} from "@al-ft/midgard-core";
import { type CommittedFieldClaim } from "@al-ft/midgard-sdk";
import { type UTxO } from "@lucid-evolution/lucid";
import { expect } from "vitest";

import { faultProofRawFieldCarriage } from "../../src/field-opening.js";
import { submitCommittedFieldShapeInit } from "../../src/index.js";
import {
  type CertifiedCommittedField,
  expectStageFit,
  publishCertifiedCommittedField,
} from "./certified-committed-field.js";
import {
  type CommittedFieldShapeEmulatorHarness,
  type CommittedFieldShapeScenario,
  type CommittedFieldShapeScenarioKind,
  publishCommittedFieldShapeReferenceScripts,
  setupCommittedFieldShapeScenario,
} from "./committed-field-shape-emulator.js";
import {
  captureEmulatorSubmission,
  network,
} from "./submit-init-emulator-shared.js";

export const LABEL = "committed-field-shape-max";

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

const certify = async (
  harness: CommittedFieldShapeEmulatorHarness,
  scenario: CommittedFieldShapeScenario,
): Promise<CertifiedCommittedField> => {
  const certified = await publishCertifiedCommittedField({
    emulator: harness.emulator,
    lucid: harness.proverLucid,
    signer: harness.proverSigner,
    certificate: harness.contracts.fieldPreimageCertificate,
    nativeTxId: scenario.nativeTxId,
    fieldIndex: scenario.fieldIndex,
    preimage: scenario.committedPreimage,
    compactCbor: scenario.compactCbor,
    witnessSetCompactCbor: encodeMidgardNativeTxWitnessSetCompact(
      deriveMidgardNativeTxWitnessSetCompact(scenario.fullTx!.witnessSet),
    ).toString("hex"),
    label: LABEL,
  });
  expect(certified.plan.tier).toBe("Certified");
  expect(certified.chunks.length).toBeGreaterThan(1);
  return certified;
};

/** Sets up the scenario, publishes both step scripts, inits and certifies. */
export const openThread = async (
  harness: CommittedFieldShapeEmulatorHarness,
  kind: CommittedFieldShapeScenarioKind,
) => {
  const scenario = await setupCommittedFieldShapeScenario({ harness, kind });
  const [step01Ref, step02Ref] =
    await publishCommittedFieldShapeReferenceScripts({
      lucid: harness.funderLucid,
      contracts: harness.committedFieldShape,
    });
  const init = await initThread(harness, scenario);
  expectStageFit(LABEL, "init", init.measurement);
  const certified = await certify(harness, scenario);
  return { scenario, step01Ref, step02Ref, init: init.result, certified };
};

export const certifiedClaimFor =
  (
    harness: CommittedFieldShapeEmulatorHarness,
    scenario: CommittedFieldShapeScenario,
    certified: CertifiedCommittedField,
  ) =>
  (referenceInputs: readonly UTxO[]): CommittedFieldClaim => ({
    BodyFieldClaim: {
      field_index: BigInt(scenario.fieldIndex),
      carriage: faultProofRawFieldCarriage({
        plan: certified.plan,
        referenceInputs,
        certificatePolicyId:
          harness.contracts.fieldPreimageCertificate.policyId,
        label: LABEL,
      }),
    },
  });

export const threadsAt = async (
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

export const blockStays = async (
  harness: CommittedFieldShapeEmulatorHarness,
  scenario: CommittedFieldShapeScenario,
) =>
  await expect(
    harness.proverLucid.utxosAtWithUnit(
      harness.contracts.stateQueue.spendingScriptAddress,
      scenario.setup.stateQueueBlockUnit,
    ),
  ).resolves.toHaveLength(1);
