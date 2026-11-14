import { STATE_QUEUE_NODE_ASSET_NAME_PREFIX } from "@al-ft/midgard-sdk";
import type { UTxO } from "@lucid-evolution/lucid";
import { expect } from "vitest";

import type { PreparedNetworkIdAcceptedForced } from "../src/network-id/accepted-forced.js";
import {
  createNetworkIdWorkflowAdapter,
  networkIdAcceptedForcedArtifactFromPrepared,
  type NetworkIdWorkflowAdapterConfig,
} from "../src/network-id/workflow-adapter.js";
import {
  FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
  type FraudProofWorkflowIdentity,
} from "../src/workflow/journal.js";
import { workflowPreflightTransaction } from "../src/workflow/transaction-boundary.js";
import {
  type Harness,
  network,
} from "./network-id-wrongful-rejection-lifecycle.commit-forced-block.js";
import { measureCompleteSignedTransaction } from "./support/emulator/measurement.js";
import { expectProofFit } from "./support/emulator/proof-fit.js";
import {
  publishPlainReferenceScriptUtxo,
  publishRemovalReferenceScripts,
} from "./support/emulator/reference-scripts.js";
import { buildRemovalDeploymentInfo } from "./support/emulator/removal-deployment.js";

/** Physical adapter/restart route over the emulator's normalized-provider surface. */
export const runAcceptedForcedAdapter = async ({
  harness,
  refs,
  prepared,
}: {
  readonly harness: Harness;
  readonly refs: readonly [UTxO, UTxO, UTxO, UTxO];
  readonly prepared: PreparedNetworkIdAcceptedForced;
}) => {
  const removalRefs = await publishRemovalReferenceScripts({
    lucid: harness.proverLucid,
    contracts: harness.contracts,
  });
  const certificateRef = await publishPlainReferenceScriptUtxo({
    lucid: harness.proverLucid,
    script: harness.networkId.fieldPreimageCertificateMintingScript,
    label: "network-id field certificate",
  });
  const baseDeployment = buildRemovalDeploymentInfo(
    harness.contracts,
    harness.catalogue,
    { removalReferenceScripts: removalRefs.published },
  );
  const deploymentInfo = {
    ...baseDeployment,
    contracts: {
      ...baseDeployment.contracts,
      fraudProofNetworkId: {
        scriptHash: harness.networkId.steps[0].spendingScriptHash,
      },
      fraudProofNetworkIdStep02: {
        scriptHash: harness.networkId.steps[1].spendingScriptHash,
      },
      fraudProofNetworkIdForcedStep: {
        scriptHash: harness.networkId.forcedStep!.spendingScriptHash,
      },
      fraudProofNetworkIdForcedScan: {
        scriptHash: harness.networkId.forcedScan!.spendingScriptHash,
      },
    },
  };
  const now = BigInt(harness.emulator.now());
  const config: NetworkIdWorkflowAdapterConfig = {
    lucid: harness.proverLucid,
    blueprint: harness.realBlueprint,
    network,
    contracts: harness.networkId,
    stateQueueAddress: harness.contracts.stateQueue.spendingScriptAddress,
    category: harness.category,
    catalogue: {
      policyId: harness.contracts.fraudProofCatalogue.policyId,
      spendingScriptAddress:
        harness.contracts.fraudProofCatalogue.spendingScriptAddress,
      root: harness.catalogue.root,
    },
    signer: harness.proverSigner,
    stepReferenceScripts: [refs[0], refs[1]],
    forcedStepReferenceScript: refs[2],
    forcedScanReferenceScript: refs[3],
    fieldPreimageCertificateReferenceScript: certificateRef.utxo,
    witnessReferenceScripts: harness.witnessReferenceScripts,
    removal: {
      deploymentInfo,
      category: "networkId",
      requireReferenceScripts: true,
      isCurrentHead: async () => true,
      validFrom: now - 120_000n,
      validTo: now + 300_000n,
    },
  };
  const identity: FraudProofWorkflowIdentity = {
    schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
    deploymentFingerprint: "a1".repeat(32),
    category: "networkId",
    target: {
      kind: "state_queue_header" as const,
      headerHash: prepared.headerHash,
    },
  };
  const artifact = networkIdAcceptedForcedArtifactFromPrepared(prepared);
  const measurements = [];
  for (const kind of [
    "init",
    "forced_step01",
    "forced_bind",
    "step02",
    "remove",
  ]) {
    // Reconstruct the adapter and reopen JSON after every confirmed step.
    const adapter = createNetworkIdWorkflowAdapter(config);
    const context = {
      identity,
      workflowId: "a2".repeat(32),
      artifact: JSON.parse(JSON.stringify(artifact)),
      entries: [],
    };
    const observed = await adapter.observe(context);
    expect(observed.kind).toBe("action_required");
    if (observed.kind !== "action_required")
      throw new Error(
        "accepted forced workflow did not require its next action",
      );
    expect(observed.action.input.kind).toBe(kind);
    const preflight = await adapter.preflight({
      ...context,
      action: observed.action,
    });
    const transaction = workflowPreflightTransaction(preflight);
    if (transaction === undefined)
      throw new Error(
        "preflight did not capture its actual signed transaction",
      );
    const measurement = measureCompleteSignedTransaction(transaction.toCBOR());
    expectProofFit({
      stage: `adapter:${kind}`,
      measurement,
      maxTxExMem: harness.emulator.protocolParameters.maxTxExMem,
      maxTxExSteps: harness.emulator.protocolParameters.maxTxExSteps,
    });
    expect(measurement.plutusV3ScriptCount).toBe(0);
    measurements.push({
      stage: kind,
      bytes: measurement.completeSignedBytes,
      memory: measurement.executionMemory.toString(),
      cpu: measurement.executionSteps.toString(),
    });
    const submitted = await adapter.submit({
      ...context,
      action: observed.action,
      preflight,
    });
    expect(submitted.kind).toBe("submitted");
    if (submitted.kind !== "submitted")
      throw new Error("adapter submission was not confirmed by emulator");
    await harness.proverLucid.awaitTx(submitted.txHash);
    expect(
      await adapter.reconcile({
        ...context,
        action: observed.action,
        txHash: submitted.txHash,
      }),
    ).toMatchObject({ kind: "confirmed", txHash: submitted.txHash });
  }
  const proofUnit = `${harness.networkId.fraudProof.policyId}${harness.category.categoryId}${prepared.headerHash}`;
  const stateQueueUnit = `${harness.contracts.stateQueue.policyId}${STATE_QUEUE_NODE_ASSET_NAME_PREFIX}${prepared.headerHash}`;
  expect(
    await harness.proverLucid.utxosAtWithUnit(
      harness.networkId.fraudProof.spendingScriptAddress,
      proofUnit,
    ),
  ).toHaveLength(1);
  expect(
    await harness.proverLucid.utxosAtWithUnit(
      harness.contracts.stateQueue.spendingScriptAddress,
      stateQueueUnit,
    ),
  ).toHaveLength(0);
  console.info(`[network-id-accepted-adapter] ${JSON.stringify(measurements)}`);
};
