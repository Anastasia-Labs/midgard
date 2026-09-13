import type { UTxO } from "@lucid-evolution/lucid";
import { expect } from "vitest";

import { withdrawalMistagStepPayloadCbor } from "../../src/withdrawal-mistag/submit-withdrawal-mistag-steps.js";
import { structuredDataPublicationPlan } from "../../src/workflow/structured-data-preimage.js";
import { captureEmulatorSubmission } from "./emulator/measurement.js";
import {
  driveWithdrawalMistagToFraud,
  makeWithdrawalMistagEmulatorHarness,
  publishWithdrawalMistagScripts,
  removeWithdrawalMistagBlock,
  setupWithdrawalMistagScenario,
} from "./withdrawal-mistag-emulator.js";

export const runWithdrawalMistagMaximumProof = async (
  record: (
    label: string,
    measurement: import("./emulator/measurement.js").CompleteSignedTransactionMeasurement,
  ) => void,
  maximumAssetNames = false,
) => {
  const harness = await makeWithdrawalMistagEmulatorHarness();
  const scenario = await setupWithdrawalMistagScenario({
    harness,
    direction: "valid-marked-invalid",
    outputBytes: 16384,
    assetCount: 100,
    payoutDatumBytes: 12000,
    proofLevels: 64,
    maximumAssetNames,
  });
  const refsByStep: UTxO[][] = [];
  const published = new Map<string, UTxO>();
  for (const step of [0, 1, 2, 3] as const) {
    const planned = structuredDataPublicationPlan(
      withdrawalMistagStepPayloadCbor(scenario.prepared, step),
    );
    const refs: UTxO[] = [];
    for (const datum of planned.publicationDatums) {
      let ref = published.get(datum);
      if (ref === undefined) {
        const captured = await captureEmulatorSubmission(
          harness.emulator,
          async () => {
            const unsigned = await harness.proverLucid
              .newTx()
              .pay.ToAddressWithData(
                harness.proverSigner.address,
                { kind: "inline", value: datum },
                { lovelace: 100_000_000n },
              )
              .complete({ localUPLCEval: true });
            const signed = await unsigned.sign.withWallet().complete();
            const txHash = await signed.submit();
            await harness.proverLucid.awaitTx(txHash);
            return (
              await harness.proverLucid.utxosAt(harness.proverSigner.address)
            ).find((utxo) => utxo.txHash === txHash && utxo.datum === datum)!;
          },
        );
        captured.measurements.forEach((measurement, index) =>
          record(`evidence-${step}-${published.size}-${index}`, measurement),
        );
        expect(captured.measurement.completeSignedBytes).toBeLessThanOrEqual(
          15872,
        );
        expect(captured.result).toBeDefined();
        ref = captured.result;
        published.set(datum, ref);
      }
      refs.push(ref);
    }
    refsByStep.push(refs);
  }
  const publication = await captureEmulatorSubmission(harness.emulator, () =>
    publishWithdrawalMistagScripts({ harness }),
  );
  publication.measurements.forEach((measurement, index) =>
    record(`script-${index}`, measurement),
  );
  const scripts = publication.result;
  const lifecycle = await driveWithdrawalMistagToFraud({
    harness,
    scenario,
    refs: scripts.refs,
    evidenceReferences: refsByStep,
  });
  for (const [label, measurement] of Object.entries(
    lifecycle.transactionMeasurements,
  )) {
    record(label, measurement);
    expect(measurement.completeSignedBytes).toBeLessThanOrEqual(15872);
    expect(measurement.executionMemory, label).toBeLessThanOrEqual(13_200_000n);
    expect(measurement.executionSteps, label).toBeLessThanOrEqual(
      8_000_000_000n,
    );
  }
  const removal = await captureEmulatorSubmission(harness.emulator, () =>
    removeWithdrawalMistagBlock({ harness, scenario }),
  );
  removal.measurements.forEach((measurement, index) =>
    record(
      `${index === removal.measurements.length - 1 ? "remove" : "removal-script"}-${index}`,
      measurement,
    ),
  );
  expect(removal.result.fraudCategory).toBe("withdrawalMistag");
};
