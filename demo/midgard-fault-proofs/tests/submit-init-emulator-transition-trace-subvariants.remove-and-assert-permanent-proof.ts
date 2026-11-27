import { outRefLabel } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk/fraud-proof";
import { Data, getAddressDetails } from "@lucid-evolution/lucid";
import { expect } from "vitest";

import { submitRemoveFraudulentBlock } from "../src/remove-fraudulent-block.submit-remove-fraudulent-block.js";
import { buildTransitionTraceProof } from "../src/transition-trace/submit.submit-transition-trace-proof.js";
import {
  type DeploymentInfo,
  type Harness,
  type Setup,
} from "./submit-init-emulator-transition-trace-subvariants.setup-challenge.js";
import { network } from "./support/emulator/blueprints.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  expectSingleUtxoWithUnit,
} from "./support/emulator/emulator-context.js";
import { expectStateQueueHeaderOrder } from "./support/submit-init/queue-assertions.js";
import type { SubmittedStep } from "./support/submit-steps.js";

export const removeAndAssertPermanentProof = async ({
  harness,
  setup,
  deploymentInfo,
  proofResult,
}: {
  readonly harness: Harness;
  readonly setup: Setup;
  readonly deploymentInfo: DeploymentInfo;
  readonly proofResult: SubmittedStep<typeof buildTransitionTraceProof>;
}) => {
  const proofUtxo = await expectSingleUtxoWithUnit(
    harness.proverLucid,
    proofResult.fraudProofAddress,
    proofResult.fraudProofUnit,
  );
  const paymentCredential = getAddressDetails(
    await harness.proverLucid.wallet().address(),
  ).paymentCredential;
  expect(paymentCredential?.type).toBe("Key");
  expect(Data.from(proofUtxo.datum!, SDK.FraudProofTokenDatum)).toEqual({
    fraud_prover: paymentCredential!.hash,
  });

  const now = BigInt(harness.emulator.now());
  const removal = await submitRemoveFraudulentBlock({
    lucid: harness.proverLucid,
    blueprint: harness.realBlueprint,
    deploymentInfo,
    network,
    signer: harness.proverSigner,
    fraudCategory: "transitionTrace",
    fraudulentHeaderHash: setup.headerHash,
    awaitConfirmation: true,
    requireReferenceScripts: true,
    validFrom: now > 120_000n ? now - 120_000n : 0n,
    validTo: now + 300_000n,
  });
  expect(removal.transactions.map(({ kind }) => kind)).toEqual([
    "remove-target",
  ]);
  await expectStateQueueHeaderOrder({
    lucid: harness.funderLucid,
    contracts: harness.contracts,
    expectedHeaderHashes: [],
  });
  await expect(
    harness.funderLucid.utxosAtWithUnit(
      harness.contracts.stateQueue.spendingScriptAddress,
      setup.stateQueueBlockUnit,
    ),
  ).resolves.toHaveLength(0);
  const retained = await expectSingleUtxoWithUnit(
    harness.proverLucid,
    proofResult.fraudProofAddress,
    proofResult.fraudProofUnit,
  );
  expect(outRefLabel(retained)).toBe(outRefLabel(proofUtxo));
  expect(retained.assets[proofResult.fraudProofUnit]).toBe(1n);
};

export const alignedHeaderStart = async (
  harness: Harness,
  leadTime = 120_000,
) =>
  alignUnixTimeToEmulatorSlotBoundary(
    harness.funderLucid,
    harness.emulator.now() + leadTime,
  ) - 1;
