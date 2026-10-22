import { outRefLabel } from "@al-ft/midgard-core";
import { expect } from "vitest";

import { inputExistenceWithPublishedFields } from "../src/input-existence/field-carriage.js";
import {
  inputExistenceExecutionContext,
  publishInputExistenceSelectionCarriage,
} from "./support/input-existence-execution-context.js";
import {
  neSubmitStep01,
  neSubmitStep02,
  submitInit,
} from "./support/legacy-submit-emulator.js";
import { buildNonExistentInputFixture } from "./support/submit-init-emulator-fixtures.js";
import {
  captureEmulatorSubmission,
  type CompleteSignedTransactionMeasurement,
  expectSingleUtxoWithUnit,
  makeFaultProofEmulatorHarness,
  network,
} from "./support/submit-init-emulator-shared.js";

/** Real shared source/selection stages, including each required publication. */
export const runNoInputCardinalityJourney = async (
  cardinality: number,
  { publishCarriage = false }: { readonly publishCarriage?: boolean } = {},
): Promise<{
  readonly stages: Record<string, CompleteSignedTransactionMeasurement>;
  readonly carriageTiers: Record<string, string>;
}> => {
  const harness = await makeFaultProofEmulatorHarness({
    contractOptions: {
      realNonExistentInput: true,
      alwaysFraudProofCatalogue: true,
    },
  });
  const fixture = await buildNonExistentInputFixture({
    spendInputCardinality: cardinality,
  });
  expect(fixture.inputsPreimage.length).toBe(cardinality);
  expect(fixture.badInputIndex).toBe(BigInt(cardinality - 1));
  const context = await inputExistenceExecutionContext({
    harness,
    nativeTx: fixture.nativeTx,
    field: 0,
    inputIndex: fixture.badInputIndex,
  });
  const init = await submitInit({
    lucid: harness.proverLucid,
    blueprint: harness.realBlueprint,
    deploymentInfo: context.deploymentInfo,
    network,
    signer: harness.proverSigner,
    fraudCategory: "nonExistentInput",
    fraudulentBlockOutRef: context.setup.fraudulentBlockOutRef,
    witnessReferenceScripts: harness.witnessReferenceScripts,
    awaitConfirmation: true,
  });
  const firstThread = await expectSingleUtxoWithUnit(
    harness.proverLucid,
    init.firstStepAddress,
    init.computationThreadUnit,
  );
  const first = await captureEmulatorSubmission(harness.emulator, async () =>
    neSubmitStep01({
      ...context.arguments,
      threadOutRef: outRefLabel(firstThread),
    }),
  );
  const selected = publishCarriage
    ? inputExistenceWithPublishedFields(
        context.prepared,
        harness.proverSigner.paymentKeyHash,
      )
    : context.prepared;
  const tier = selected.fieldPlan.plan.tier;
  const second = await captureEmulatorSubmission(harness.emulator, async () => {
    const carriage = await publishInputExistenceSelectionCarriage(
      harness,
      context.prepared,
      publishCarriage,
    );
    return neSubmitStep02({
      ...context.arguments,
      ...carriage,
      threadOutRef: first.result.nextThreadOutRef,
    });
  });
  // This slice selects one field. A non-inline opening has one publication
  // at these cardinalities, and the final measurement is the selection itself.
  expect(second.measurements.length).toBe(tier === "Inline" ? 1 : 2);
  return {
    stages: {
      "step-01": first.measurement,
      ...(tier === "Inline"
        ? {}
        : { "step-02-carriage": second.measurements[0]! }),
      "step-02": second.measurement,
    },
    carriageTiers: { "step-02": tier },
  };
};
