import { mkdtemp } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { makeDeploymentMarker } from "@al-ft/midgard-core/deployment-manifest-identity";
import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  Emulator,
  generateEmulatorAccount,
  paymentCredentialOf,
  SLOT_CONFIG_NETWORK,
  unixTimeToEnclosingSlot,
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
import { TEST_CARDANO_PROTOCOL_PARAMETERS } from "./cardano-protocol-parameters.js";
import {
  type AcceptedHistoryObservation,
  captureConfirmedHistoryObservations,
  historyOutputObservation,
} from "./history-projection-observations.js";
import {
  hash,
  label,
  type RecordedHistoryBatch,
} from "./history-source-owner-emulator.recorded-history-batch.js";
import {
  createMainnetEmulatorLucid,
  MAINNET_PROTOCOL_PARAMETERS,
} from "./mainnet-protocol-parameters.js";
import {
  createPublishedWorkflowDeploymentAccounts,
  publishWorkflowDeploymentOnChain,
} from "./published-workflow-deployment.js";
import { loadRealMidgardContractsForTest } from "./real-midgard-contracts.js";
import { DEFAULT_PUBLICATION_SCHEDULE } from "./reference-publication-chain.js";

/** Actual published deployment adapted to the existing node pipeline; it is
 * initialized exactly once and uses its own configured DA cosigner. */
export const openHistorySourceOwnerLifecycle = async (
  eventHistoryProtectionDurationMs?: bigint,
) => {
  await resetActiveRuntimePaths();
  await initializeNodeRuntime();
  const accounts = createPublishedWorkflowDeploymentAccounts();
  let onBatch: (
    observations: readonly AcceptedHistoryObservation[],
  ) => Promise<void> = async () => {};
  const p = MAINNET_PROTOCOL_PARAMETERS;
  const emulator = new Emulator([accounts.operator, accounts.publisher], p);
  emulator.time = 1_788_739_200_000;
  emulator.slot = unixTimeToEnclosingSlot(
    emulator.time,
    SLOT_CONFIG_NETWORK.Preprod,
  );
  emulator.blockHeight = Math.floor(emulator.slot / 20);
  const operatorLucid = await createMainnetEmulatorLucid(emulator, "Preprod");
  const publisher = await createMainnetEmulatorLucid(emulator, "Preprod");
  operatorLucid.selectWallet.fromSeed(accounts.operator.seedPhrase);
  publisher.selectWallet.fromSeed(accounts.publisher.seedPhrase);
  const batches: RecordedHistoryBatch[] = [];
  const publications = new Map<
    string,
    { signedCbor: string; observedSlot: number }
  >();
  let preparedContracts: Awaited<
    ReturnType<typeof loadRealMidgardContractsForTest>
  >;
  let observation:
    | ReturnType<typeof captureConfirmedHistoryObservations>
    | undefined;
  const published = await publishWorkflowDeploymentOnChain({
    eventHistoryProtectionDurationMs,
    accounts,
    network: "Preprod",
    operatorLucid,
    publisherLucid: publisher,
    chain: {
      now: () => emulator.now(),
      delaySlots: (slots) => emulator.awaitSlot(slots),
      awaitLedgerTime: (time) => {
        const slots = Math.ceil((time - emulator.now()) / 1000);
        if (slots > 0) emulator.awaitSlot(slots);
      },
    },
    protocolParameters: {
      ...TEST_CARDANO_PROTOCOL_PARAMETERS,
      minFeeA: String(p.minFeeA),
      minFeeB: String(p.minFeeB),
      coinsPerUtxoByte: String(p.coinsPerUtxoByte),
      collateralPercentage: String(p.collateralPercentage),
      maxCollateralInputs: String(p.maxCollateralInputs),
      maxTxSize: String(p.maxTxSize),
      maxValueSize: String(p.maxValSize),
      maxTxExUnits: {
        memory: String(p.maxTxExMem),
        steps: String(p.maxTxExSteps),
      },
    },
    publicationJournalPath: join(
      await mkdtemp(join(tmpdir(), "midgard-history-owner-")),
      "transactions.ndjson",
    ),
    publicationSchedule: DEFAULT_PUBLICATION_SCHEDULE,
    publicationSynchronize: async () => emulator.slot,
    onPrepared: async ({ nonce, authPolicy }) => {
      preparedContracts = await loadRealMidgardContractsForTest(
        nonce,
        authPolicy,
        eventHistoryProtectionDurationMs,
      );
    },
    onPublication: ({ signedCbor, outRef }) => {
      expect(
        CML.hash_transaction(
          CML.Transaction.from_cbor_hex(signedCbor).body(),
        ).to_hex(),
      ).toBe(outRef.txHash);
      publications.set(outRef.txHash, {
        signedCbor,
        observedSlot: emulator.slot,
      });
    },
    onInitialization: () => {
      const pair = SDK.requireEventHistoryContracts(preparedContracts);
      const addresses = [
        preparedContracts.hubOracle.spendingScriptAddress,
        ...Object.values(pair).flatMap((h) => [
          h.list.spendingScriptAddress,
          h.retention.spendingScriptAddress,
        ]),
        // A real node serves any address at an acquired point; recording the
        // queue lets an exact-point recovery capture of it be served.
        preparedContracts.stateQueue.spendingScriptAddress,
      ];
      observation = captureConfirmedHistoryObservations(
        operatorLucid,
        emulator,
        async (observations) => {
          batches.push({
            observations,
            observedSlot: emulator.slot,
            observedHeight: emulator.blockHeight,
            outputs: (
              await Promise.all(
                addresses.map((address) => operatorLucid.utxosAt(address)),
              )
            )
              .flat()
              .map(historyOutputObservation),
          });
          await onBatch(observations);
        },
      );
    },
  });
  const deployment = { ...published, emulator };
  const {
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
  const depositorLucid = await createMainnetEmulatorLucid(emulator, "Preprod");
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
        daBondPoolMinting: reference("daBondPoolMint"),
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
  const genesis = {
    scope: "synthetic emulator transport",
    initializationTxHash: deployment.initialization.txHash,
  };
  const binding = await Effect.runPromise(
    makeEventHistorySourceBinding({
      contracts,
      identity,
      network: "Preprod",
      expectedGenesisLosslessSha256: eventHistoryGenesisLosslessSha256(genesis),
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
  onBatch = async (observations) => {
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
    expect(projected.capture.history.deposits).toEqual(actual.history.deposits);
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
  };
  return {
    fixture,
    lucidService,
    globals,
    deployment,
    deploymentInfoSha256,
    observer: observation!,
    batches,
    publications,
    binding,
    genesis,
    receipts,
    transitions,
    capture: () => capture,
  };
};
