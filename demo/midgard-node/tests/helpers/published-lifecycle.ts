import { createHash } from "node:crypto";

import { makeDeploymentMarker } from "@al-ft/midgard-core/deployment-manifest-identity";
import { paymentCredentialOf } from "@lucid-evolution/lucid";
import { expect, vi } from "vitest";

import { ContractDeploymentIdentity } from "../../src/services/midgard-contracts.js";
import {
  configureEmulatorDaRuntimeManifest,
  type EmulatorFixture,
  makeGlobalsService,
  makeLucidRuntimeService,
} from "../deposit-flow-emulator-shared.js";
import { type ConfirmedTransactionObservation } from "./confirmed-transaction-observations.js";
import { restorePublishedPrefix } from "./published-lifecycle.restore.js";

const hash = (value: string) =>
  createHash("sha256").update(value).digest("hex");

/** Actual published deployment adapted to the existing node pipeline; it is
 * initialized exactly once and uses its own configured DA cosigner. The
 * deployment prefix is built once per run for each protection duration
 * (`restorePublishedPrefix`), read once per file, and restored into a fresh
 * emulator, fresh lucid instances and fresh records for every lifecycle.
 * `receipts` holds every transaction confirmed since the restore, in
 * confirmation order, each within the deployment's per-transaction limits. */
export const openPublishedLifecycle = async (
  eventHistoryProtectionDurationMs?: bigint,
) => {
  let onConfirmed: (
    observations: readonly ConfirmedTransactionObservation[],
  ) => Promise<void> = async () => {};
  const {
    prefix,
    emulator,
    lucid,
    publisherLucid,
    depositorLucid,
    publications,
    observation,
    deployment,
  } = await restorePublishedPrefix(
    eventHistoryProtectionDurationMs,
    (observations) => onConfirmed(observations),
  );
  const { accounts, depositorAccount, contracts } = prefix;
  const { manifest } = deployment;
  vi.useFakeTimers({ toFake: ["Date"] });
  vi.setSystemTime(emulator.now());
  const deploymentInfoSha256 = hash(JSON.stringify(deployment.deploymentInfo));
  await configureEmulatorDaRuntimeManifest({ manifest, deploymentInfoSha256 });
  const identity = ContractDeploymentIdentity.make({
    kind: "manifest",
    manifest,
    manifestId: manifest.manifestId,
    deploymentMarker: makeDeploymentMarker(manifest.manifestId),
    l1Finality: manifest.l1Finality,
    consensusProfile: manifest.consensusProfile,
  });
  const reference = (role: string) => {
    const result = deployment.references.get(role);
    if (result === undefined)
      throw new Error(`Missing published lifecycle role ${role}`);
    return result;
  };
  const fixture: EmulatorFixture = {
    emulator,
    emulatorCreationTimeMs: prefix.emulatorCreationTimeMs,
    contracts,
    operatorAccount: accounts.operator,
    depositorAccount,
    referenceScriptsAccount: accounts.publisher,
    operatorLucid: lucid,
    depositorLucid,
    referenceScriptsLucid: publisherLucid,
    operatorKeyHash: paymentCredentialOf(await lucid.wallet().address()).hash,
    runtimeOverrides: {
      deploymentIdentity: identity,
      daCosignerSeedPhrase: accounts.cosigner.seedPhrase,
    },
    referenceScripts: {
      deposit: { depositMinting: reference("depositMint") },
      withdrawal: { withdrawalMinting: reference("withdrawalMint") },
      init: {
        depositHistory: reference("depositMint"),
        withdrawalHistory: reference("withdrawalMint"),
        daParamsGovernorMinting: reference("daParamsGovernorMint"),
        hubOracleMinting: reference("hubOracleMint"),
        schedulerMinting: reference("schedulerMint"),
        stateQueueMinting: reference("stateQueueMint"),
        registeredOperatorsMinting: reference("registeredOperatorsMint"),
        activeOperatorsMinting: reference("activeOperatorsMint"),
        retiredOperatorsMinting: reference("retiredOperatorsMint"),
        fraudProofCatalogueMinting: reference("fraudProofCatalogueMint"),
        daBondPoolMinting: reference("daBondPoolMint"),
      },
    },
  };
  const lucidService = await makeLucidRuntimeService(fixture);
  const globals = await makeGlobalsService();
  const receipts: ConfirmedTransactionObservation[] = [];
  onConfirmed = async (observations) => {
    const limits = manifest.cardanoProtocolParameters.snapshot;
    for (const observation of observations) {
      expect(observation.measurement.completeSignedBytes).toBeLessThanOrEqual(
        Number(limits.maxTxSize),
      );
      expect(observation.measurement.executionMemory).toBeLessThanOrEqual(
        BigInt(limits.maxTxExUnits.memory),
      );
      expect(observation.measurement.executionSteps).toBeLessThanOrEqual(
        BigInt(limits.maxTxExUnits.steps),
      );
      receipts.push(observation);
    }
  };
  return {
    fixture,
    lucidService,
    globals,
    deployment,
    deploymentInfoSha256,
    observer: observation,
    publications,
    receipts,
  };
};
