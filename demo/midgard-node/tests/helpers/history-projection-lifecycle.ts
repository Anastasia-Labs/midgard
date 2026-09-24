import { createHash } from "node:crypto";

import { makeDeploymentMarker } from "@al-ft/midgard-core/deployment-manifest-identity";
import * as SDK from "@al-ft/midgard-sdk";
import {
  generateEmulatorAccount,
  Lucid,
  paymentCredentialOf,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect, vi } from "vitest";

import { projectEventHistoryBlock } from "../../src/l1-event-history-projection.js";
import {
  decodeBoundEventHistoryLedgerSnapshot,
  eventHistoryGenesisLosslessSha256,
  makeEventHistorySourceBinding,
} from "../../src/l1-event-history-source.js";
import { ContractDeploymentIdentity } from "../../src/services/midgard-contracts.js";
import {
  activateOperatorProgram,
  registerOperatorProgram,
} from "../../src/transactions/register-active-operator.js";
import {
  configureEmulatorDaRuntimeManifest,
  type EmulatorFixture,
  initializeNodeRuntime,
  makeGlobalsService,
  makeLucidRuntimeService,
  REGISTRATION_ACTIVATION_DELAY_SLOTS,
  REQUIRED_BOND_LOVELACE,
  resetActiveRuntimePaths,
} from "../deposit-flow-emulator-shared.js";
import {
  type AcceptedHistoryObservation,
  captureConfirmedHistoryObservations,
  historyOutputObservation,
} from "./history-projection-observations.js";
import {
  createPublishedWorkflowDeploymentAccounts,
  publishWorkflowDeployment,
} from "./published-workflow-deployment.js";

const hash = (value: string) =>
  createHash("sha256").update(value).digest("hex");
const label = (value: { txHash: string; outputIndex: number }) =>
  `${value.txHash}#${value.outputIndex}`;

/** Actual published deployment adapted to the existing node pipeline; it is
 * initialized exactly once and uses its own configured DA cosigner. */
export const openHistoryProjectionLifecycle = async () => {
  await resetActiveRuntimePaths();
  await initializeNodeRuntime();
  const accounts = createPublishedWorkflowDeploymentAccounts();
  const deployment = await publishWorkflowDeployment({
    accounts,
    network: "Preprod",
  });
  const {
    emulator,
    operatorLucid: lucid,
    publisherLucid,
    contracts,
    manifest,
  } = deployment;
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
  const depositorAccount = generateEmulatorAccount({ lovelace: 0n });
  const depositorLucid = await Lucid(emulator, "Preprod");
  depositorLucid.selectWallet.fromSeed(depositorAccount.seedPhrase);
  const fund = await lucid
    .newTx()
    .pay.ToAddress(depositorAccount.address, { lovelace: 1_000_000_000n })
    .complete({ localUPLCEval: true });
  const funded = await fund.sign.withWallet().complete();
  expect(await lucid.awaitTx(await funded.submit())).toBe(true);
  lucid.overrideUTxOs(await lucid.utxosAt(await lucid.wallet().address()));
  vi.setSystemTime(emulator.now());
  const reference = (role: string) => {
    const result = deployment.references.get(role);
    if (result === undefined)
      throw new Error(`Missing published lifecycle role ${role}`);
    return result;
  };
  const fixture: EmulatorFixture = {
    emulator,
    emulatorCreationTimeMs: emulator.now(),
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
      },
    },
  };
  await Effect.runPromise(
    registerOperatorProgram(
      lucid,
      contracts,
      REQUIRED_BOND_LOVELACE,
      publisherLucid,
    ),
  );
  emulator.awaitSlot(REGISTRATION_ACTIVATION_DELAY_SLOTS);
  vi.setSystemTime(emulator.now());
  await Effect.runPromise(
    activateOperatorProgram(
      lucid,
      contracts,
      REQUIRED_BOND_LOVELACE,
      publisherLucid,
    ),
  );
  vi.setSystemTime(emulator.now());
  const lucidService = await makeLucidRuntimeService(fixture);
  const globals = await makeGlobalsService();
  const binding = await Effect.runPromise(
    makeEventHistorySourceBinding({
      contracts,
      identity,
      network: "Preprod",
      ogmiosUrl: "http://projection-lifecycle-emulator.invalid:1337",
      expectedGenesisLosslessSha256: eventHistoryGenesisLosslessSha256({
        scope: "synthetic emulator transport",
        initializationTxHash: deployment.initialization.txHash,
      }),
    }),
  );
  const histories = SDK.requireEventHistoryContracts(contracts);
  const addresses = [
    ...new Set([
      binding.hubAddress,
      ...Object.values(binding.deployments).flatMap((item) => [
        item.address,
        item.retentionAddress,
      ]),
    ]),
  ];
  const readCapture = async (point: { slot: number; id: string }) =>
    Effect.runPromise(
      decodeBoundEventHistoryLedgerSnapshot(
        {
          point: { slot: point.slot, id: point.id },
          addresses,
          outputs: (
            await Promise.all(
              addresses.map((address) => lucid.utxosAt(address)),
            )
          )
            .flat()
            .map(historyOutputObservation),
        },
        binding,
      ),
    );
  let capture = await readCapture({
    slot: emulator.slot,
    id: hash(
      `projection-lifecycle-start:${deployment.initialization.txHash}:${emulator.slot}`,
    ),
  });
  const receipts: (AcceptedHistoryObservation & {
    projectedSnapshotDigest: string;
    providerSnapshotDigest: string;
  })[] = [];
  const transitions: Awaited<
    ReturnType<typeof projectEventHistoryBlock>
  >["transitions"][number]["transition"][] = [];
  const observer = captureConfirmedHistoryObservations(
    lucid,
    emulator,
    async (observations) => {
      const point = {
        slot: emulator.slot,
        height: emulator.blockHeight,
        id: hash(
          `projection-lifecycle-block:${observations.map(({ transaction }) => transaction.txHash).join(":")}`,
        ),
      };
      const archives = new Map(
        observations.map((observation) => [
          observation.transaction.txHash,
          new Map(
            observation.historical.map((output) => [label(output), output]),
          ),
        ]),
      );
      const projected = await projectEventHistoryBlock({
        previous: capture,
        block: {
          parent: capture.history.ledger.point.id,
          point,
          transactions: observations.map(({ transaction }) => transaction),
        },
        binding,
        histories,
        resolveReference: (transactionHash, ref) =>
          archives.get(transactionHash)?.get(label(ref)),
        slotToUnixTime: lucid.slotToUnixTime,
      });
      const actual = await readCapture(point);
      expect(projected.capture.snapshotDigest).toBe(actual.snapshotDigest);
      expect(projected.capture.history.deposits).toEqual(
        actual.history.deposits,
      );
      expect(projected.capture.history.withdrawals).toEqual(
        actual.history.withdrawals,
      );
      for (const observation of observations) {
        const limits = manifest.cardanoProtocolParameters.snapshot;
        expect(observation.measurement.completeSignedBytes).toBeLessThanOrEqual(
          Number(limits.maxTxSize),
        );
        expect(observation.measurement.executionMemory).toBeLessThanOrEqual(
          BigInt(limits.maxTxExUnits.memory),
        );
        expect(observation.measurement.executionSteps).toBeLessThanOrEqual(
          BigInt(limits.maxTxExUnits.steps),
        );
        receipts.push({
          ...observation,
          projectedSnapshotDigest: projected.capture.snapshotDigest,
          providerSnapshotDigest: actual.snapshotDigest,
        });
      }
      transitions.push(
        ...projected.transitions.map(({ transition }) => transition),
      );
      capture = projected.capture;
    },
  );
  return {
    fixture,
    lucidService,
    globals,
    deployment,
    deploymentInfoSha256,
    observer,
    receipts,
    transitions,
    capture: () => capture,
  };
};
