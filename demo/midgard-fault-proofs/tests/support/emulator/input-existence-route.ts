/** The registered four-step input-existence route on the emulator: publish
 * the family's step scripts, open the thread with `submitInit`, run the steps
 * before the decisive one, then either convict at step 04 or hand the refused
 * step (step 04 unless `atStep` names step 03) to the caller's refusal check
 * and confirm the thread is kept and no proof minted. The caller runs
 * `expectOnchainRefusal` itself so a traced pin is checked against the file
 * that declares it. `submitStep` replaces the local submitter, for a prover
 * that carries evidence the local submitter would not admit. */
import type { UTxO } from "@lucid-evolution/lucid";
import { expect } from "vitest";

import type { PreparedInputExistence } from "../../../src/input-existence/prepare.js";
import { submitInputExistenceStep } from "../../../src/input-existence/submit.js";
import { submitInit } from "../../../src/submit-init.js";
import {
  buildRemovalDeploymentInfo,
  network,
  publishPlainReferenceScriptUtxo,
} from "../submit-init-emulator-shared.js";
import type { makeFaultProofEmulatorHarness } from "./harness.js";
import {
  captureEmulatorSubmission,
  type CompleteSignedTransactionMeasurement,
} from "./measurement.js";
import { TRACED_REFUSALS } from "./traced-refusals.js";

type Harness = Awaited<ReturnType<typeof makeFaultProofEmulatorHarness>>;

export const inputExistenceCategory = (field: 0 | 1) =>
  field === 0 ? "nonExistentInput" : "noReferenceInput";

// The traced-refusal run swaps in verbose-traced scripts only to attribute a
// refusal to its check; the plain run is the one that proves every script and
// transaction fits.
const checkFit = (measurement: CompleteSignedTransactionMeasurement) => {
  if (TRACED_REFUSALS) return;
  expect(measurement.completeSignedBytes).toBeLessThanOrEqual(16_384);
  expect(measurement.executionMemory).toBeLessThanOrEqual(13_200_000n);
  expect(measurement.executionSteps).toBeLessThanOrEqual(8_000_000_000n);
};

export const runInputExistenceRoute = async ({
  h,
  field,
  prepared,
  fraudulentBlockOutRef,
  submitStep = submitInputExistenceStep,
  outcome,
}: {
  readonly h: Harness;
  readonly field: 0 | 1;
  readonly prepared: PreparedInputExistence;
  readonly fraudulentBlockOutRef: string;
  readonly submitStep?: typeof submitInputExistenceStep;
  readonly outcome:
    | Readonly<{ kind: "conviction" }>
    | Readonly<{
        kind: "refusal";
        atStep?: 2 | 3;
        refuse: (step: () => Promise<unknown>) => Promise<unknown>;
      }>;
}) => {
  const category = inputExistenceCategory(field);
  const family = h.contracts.fraudProofContracts[category];
  if (!TRACED_REFUSALS)
    for (const step of family.steps)
      expect(step.spendingScript.script.length / 2).toBeLessThanOrEqual(16_384);
  const stepReferences: UTxO[] = [];
  for (const [index, step] of family.steps.entries())
    stepReferences.push(
      (
        await publishPlainReferenceScriptUtxo({
          lucid: h.funderLucid,
          script: step.spendingScript,
          label: `input existence ${category} step${index + 1}`,
        })
      ).utxo,
    );
  const ct = (
    await publishPlainReferenceScriptUtxo({
      lucid: h.funderLucid,
      script: h.contracts.computationThread.mintingScript,
      label: "input existence CT mint",
    })
  ).utxo;
  const fp = (
    await publishPlainReferenceScriptUtxo({
      lucid: h.funderLucid,
      script: h.contracts.fraudProof.mintingScript,
      label: "input existence proof mint",
    })
  ).utxo;
  const init = await captureEmulatorSubmission(h.emulator, () =>
    submitInit({
      lucid: h.proverLucid,
      blueprint: h.realBlueprint,
      deploymentInfo: buildRemovalDeploymentInfo(h.contracts, h.catalogue),
      network,
      signer: h.proverSigner,
      fraudCategory: category,
      fraudulentBlockOutRef,
      witnessReferenceScripts: h.witnessReferenceScripts,
    }),
  );
  checkFit(init.measurement);
  let threadOutRef = `${init.result.txHash}#${init.result.firstStepOutputIndex}`;
  const send = (stepIndex: 0 | 1 | 2 | 3) =>
    submitStep({
      lucid: h.proverLucid,
      blueprint: h.realBlueprint,
      network,
      contracts: {
        hubOraclePolicyId: h.contracts.hubOracle.policyId,
        steps: family.steps,
        computationThread: h.contracts.computationThread,
        fraudProof: h.contracts.fraudProof,
      },
      categoryId: h.catalogue.categories[category].categoryId,
      signer: h.proverSigner,
      threadOutRef,
      prepared,
      stepIndex,
      stateQueueBlockOutRef: fraudulentBlockOutRef,
      referenceScripts: {
        steps: stepReferences,
        computationThreadMint: ct,
        fraudProofMint: fp,
      },
      witnessReferenceScripts: h.witnessReferenceScripts,
    });
  const decisive = outcome.kind === "refusal" ? (outcome.atStep ?? 3) : 3;
  for (const index of decisive === 2
    ? ([0, 1] as const)
    : ([0, 1, 2] as const)) {
    const submitted = await captureEmulatorSubmission(h.emulator, () =>
      send(index),
    );
    checkFit(submitted.measurement);
    threadOutRef = submitted.result.nextThreadOutRef;
  }
  if (outcome.kind === "refusal") {
    await outcome.refuse(() => send(decisive));
    // A refusal must preserve the refused step's thread and mint no proof.
    const [txHash, index] = threadOutRef.split("#");
    expect(
      await h.proverLucid.utxosByOutRef([
        { txHash: txHash!, outputIndex: Number(index) },
      ]),
    ).toHaveLength(1);
    return;
  }
  const final = await captureEmulatorSubmission(h.emulator, () => send(3));
  checkFit(final.measurement);
  expect(final.result.fraudProofOutRef).toBe(final.result.nextThreadOutRef);
  const [txHash, index] = final.result.fraudProofOutRef!.split("#");
  const [proof] = await h.proverLucid.utxosByOutRef([
    { txHash: txHash!, outputIndex: Number(index) },
  ]);
  expect(proof!.address).toBe(h.contracts.fraudProof.spendingScriptAddress);
  expect(
    Object.entries(proof!.assets).filter(([unit]) =>
      unit.startsWith(h.contracts.fraudProof.policyId),
    ),
  ).toHaveLength(1);
  expect(
    Object.entries(proof!.assets).filter(([unit]) =>
      unit.startsWith(h.contracts.computationThread.policyId),
    ),
  ).toHaveLength(0);
};
