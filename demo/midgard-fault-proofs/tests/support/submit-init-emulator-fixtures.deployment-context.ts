import { mkdtemp, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { type MidgardNativeTxFull } from "@al-ft/midgard-core";
import {
  decodeMidgardNativeTxCanonical,
  materializeMidgardNativeTxFromCanonical,
} from "@al-ft/midgard-core/codec";
import { wrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import { DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE } from "@al-ft/midgard-core/deployment-manifest-identity";
import * as SDK from "@al-ft/midgard-sdk";
import {
  Emulator,
  generateEmulatorAccount,
  Lucid,
  paymentCredentialOf,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect } from "vitest";

import { resolveProverSigner } from "../../src/index.js";
import {
  buildCanonicalL2BlockFixture,
  buildFixtureTransaction,
} from "../helpers/canonical-block-evidence-fixture.js";
import { requireUtxoWithUnit } from "./emulator/emulator-context.js";
import { measureCompleteSignedTransaction } from "./emulator/measurement.js";
import { createReferenceScriptPublisher } from "./emulator/reference-script-publisher.js";
import { onboardEmulatorOperator } from "./emulator/setup-tx.onboard-emulator-operator.js";
import { setupUnits } from "./emulator/setup-tx.setup-units.js";
import {
  submitHeaderCommitTx,
  submitSchedulerAppointmentTx,
} from "./emulator/setup-tx.submit-header-commit-tx.js";
import {
  outputReferenceCbor,
  tx1InputsPreimage,
  tx2InputsPreimage,
} from "./submit-init-emulator-fixtures.expect-state-queue-header-order.js";
import {
  alwaysSucceedsBlueprintPath,
  buildCatalogueDeploymentInfo,
  buildMinimalFaultProofContracts,
  EMULATOR_PROTOCOL_PARAMETERS,
  fundedProverEmulatorAccount,
  network,
  publishOperatorLifecycleReferenceScripts,
  readBlueprint,
  realBlueprintPath,
  registerPhasMembershipRewardAccount,
} from "./submit-init-emulator-shared.js";

export const buildProvedFixtureDeploymentContext = async (
  headerMinimumFee: bigint,
  coherentCanonicalEvidence = false,
) => {
  if (coherentCanonicalEvidence) return buildCanonicalDeploymentContext();
  const realBlueprint = readBlueprint(realBlueprintPath);
  const alwaysBlueprint = readBlueprint(alwaysSucceedsBlueprintPath);
  const funder = generateEmulatorAccount({ lovelace: 40_000_000_000n });
  const prover = fundedProverEmulatorAccount(20_000_000_000n);
  const emulator = new Emulator([funder, prover], EMULATOR_PROTOCOL_PARAMETERS);
  const funderLucid = await Lucid(emulator, "Custom");
  const proverLucid = await Lucid(emulator, "Custom");
  funderLucid.selectWallet.fromSeed(funder.seedPhrase);
  const proverSigner = resolveProverSigner({
    network,
    walletSeedPhrase: prover.seedPhrase,
  });
  // Selected through the signer so the prover Lucid instance and every
  // `signer.selectWallet(lucid)` call site address the same funded wallet.
  proverSigner.selectWallet(proverLucid);
  await registerPhasMembershipRewardAccount(funderLucid, realBlueprint);
  const { nonceUtxo, referenceScriptAuth, referenceScriptPublisher } =
    await createReferenceScriptPublisher(funderLucid, emulator.now());
  const baseContracts = {
    ...(await buildMinimalFaultProofContracts(
      realBlueprint,
      alwaysBlueprint,
      nonceUtxo,
      {
        realMinFee: headerMinimumFee > 0n,
        referenceScriptAuthPolicyId: referenceScriptAuth.policyId,
      },
    )),
    referenceScriptAuth,
    referenceScriptPublisher,
  };
  // Operator registration and activation source their four directory
  // validators from published reference scripts. Published from the prover
  // wallet before the header clock is sampled so the funder's nonce UTxO
  // survives and the whole fixture timeline shifts uniformly.
  const contracts = {
    ...baseContracts,
    operatorLifecycleReferenceScripts:
      await publishOperatorLifecycleReferenceScripts({
        lucid: proverLucid,
        contracts: baseContracts,
      }),
  };
  const catalogue = await buildCatalogueDeploymentInfo(contracts.fraudProofs);
  return {
    realBlueprint,
    emulator,
    funderLucid,
    proverLucid,
    proverSigner,
    nonceUtxo,
    contracts,
    catalogue,
    canonical: undefined,
  };
};

const buildCanonicalDeploymentContext = async () => {
  const { publishWorkflowDeploymentOnChain } = await import(
    "midgard-node/tests/helpers/published-workflow-deployment.publish-workflow-deployment-on-chain"
  );
  const { TEST_CARDANO_PROTOCOL_PARAMETERS } = await import(
    "midgard-node/tests/helpers/cardano-protocol-parameters"
  );
  const { DEFAULT_PUBLICATION_SCHEDULE } = await import(
    "midgard-node/tests/helpers/reference-publication-chain"
  );
  const realBlueprint = readBlueprint(realBlueprintPath);
  const funder = generateEmulatorAccount({ lovelace: 40_000_000_000n });
  const prover = fundedProverEmulatorAccount(20_000_000_000n);
  const publisher = generateEmulatorAccount({ lovelace: 4_000_000_000_000n });
  const cosigner = generateEmulatorAccount({ lovelace: 0n });
  const emulator = new Emulator(
    [funder, prover, publisher],
    EMULATOR_PROTOCOL_PARAMETERS,
  );
  const funderLucid = await Lucid(emulator, "Custom");
  const proverLucid = await Lucid(emulator, "Custom");
  const publisherLucid = await Lucid(emulator, "Custom");
  funderLucid.selectWallet.fromSeed(funder.seedPhrase);
  publisherLucid.selectWallet.fromSeed(publisher.seedPhrase);
  const proverSigner = resolveProverSigner({
    network,
    walletSeedPhrase: prover.seedPhrase,
  });
  proverSigner.selectWallet(proverLucid);
  const parameters = await emulator.getProtocolParameters();
  expect(parameters.minFeeA.toString()).toBe(
    TEST_CARDANO_PROTOCOL_PARAMETERS.minFeeA,
  );
  expect(parameters.minFeeB.toString()).toBe(
    TEST_CARDANO_PROTOCOL_PARAMETERS.minFeeB,
  );
  expect(parameters.coinsPerUtxoByte.toString()).toBe(
    TEST_CARDANO_PROTOCOL_PARAMETERS.coinsPerUtxoByte,
  );
  expect(parameters.maxTxSize.toString()).toBe(
    TEST_CARDANO_PROTOCOL_PARAMETERS.maxTxSize,
  );
  expect(parameters.maxTxExMem.toString()).toBe(
    TEST_CARDANO_PROTOCOL_PARAMETERS.maxTxExUnits.memory,
  );
  expect(parameters.maxTxExSteps.toString()).toBe(
    TEST_CARDANO_PROTOCOL_PARAMETERS.maxTxExUnits.steps,
  );
  expect(parameters.collateralPercentage.toString()).toBe(
    TEST_CARDANO_PROTOCOL_PARAMETERS.collateralPercentage,
  );
  expect(parameters.maxCollateralInputs.toString()).toBe(
    TEST_CARDANO_PROTOCOL_PARAMETERS.maxCollateralInputs,
  );
  expect(parameters.maxValSize.toString()).toBe(
    TEST_CARDANO_PROTOCOL_PARAMETERS.maxValueSize,
  );
  expect(parameters.minFeeRefScriptCostPerByte).toBe(15);
  expect(parameters.priceMem).toBe(0.0577);
  expect(parameters.priceStep).toBe(0.0000721);
  const journalDirectory = await mkdtemp(
    join(tmpdir(), "midgard-permissionless-publications-"),
  );
  const publicationCbor = new Map<string, string>();
  let deployment: Awaited<ReturnType<typeof publishWorkflowDeploymentOnChain>>;
  try {
    deployment = await publishWorkflowDeploymentOnChain({
      network,
      accounts: { operator: funder, publisher, cosigner },
      operatorLucid: funderLucid,
      publisherLucid,
      chain: {
        now: () => emulator.now(),
        delaySlots: (slots) => emulator.awaitSlot(slots),
        awaitLedgerTime: (target) => {
          while (emulator.now() < target) emulator.awaitSlot(1);
        },
      },
      protocolParameters: TEST_CARDANO_PROTOCOL_PARAMETERS,
      publicationJournalPath: join(journalDirectory, "transactions.ndjson"),
      publicationSchedule: DEFAULT_PUBLICATION_SCHEDULE,
      publicationSynchronize: async () => emulator.slot,
      onPublication: ({ signedCbor, outRef }) => {
        publicationCbor.set(
          `${outRef.txHash}#${outRef.outputIndex}`,
          signedCbor,
        );
      },
    });
  } finally {
    await rm(journalDirectory, { recursive: true, force: true });
  }
  const reference = (name: keyof typeof deployment.manifest.contracts) => {
    const utxo = deployment.references.get(name);
    if (
      utxo === undefined ||
      utxo.scriptRef === undefined ||
      utxo.scriptRef === null
    )
      throw new Error(`Canonical publication omitted ${name}`);
    expect(validatorToScriptHash(utxo.scriptRef)).toBe(
      deployment.manifest.contracts[name].scriptHash,
    );
    return utxo;
  };
  const role = (
    name: keyof typeof DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE,
  ) => ({
    name,
    utxo: reference(
      DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE[name],
    ),
  });
  const contracts = {
    ...deployment.contracts,
    operatorLifecycleReferenceScripts: {
      registered: [
        role("registered-operators spending"),
        role("registered-operators minting"),
      ],
      active: [
        role("active-operators spending"),
        role("active-operators minting"),
      ],
      initial: [
        role("registered-operators minting"),
        role("active-operators minting"),
        role("hub-oracle minting"),
        role("fraud-proof-catalogue minting"),
        role("scheduler minting"),
        role("state-queue minting"),
        role("retired-operators minting"),
      ],
    },
  };
  const published = {
    correctionLockSpend: reference("correctionLockSpend"),
    stateQueueSpend: reference("stateQueueSpend"),
    stateQueueMint: reference("stateQueueMint"),
    stateQueueFraudRemovalWithdraw: reference("stateQueueFraudRemovalWithdraw"),
    activeOperatorsSpend: reference("activeOperatorsSpend"),
    activeOperatorsMint: reference("activeOperatorsMint"),
    retiredOperatorsSpend: reference("retiredOperatorsSpend"),
    retiredOperatorsMint: reference("retiredOperatorsMint"),
    schedulerSpend: reference("schedulerSpend"),
  };
  const measurement = (utxo: (typeof published)[keyof typeof published]) => {
    const cbor = publicationCbor.get(`${utxo.txHash}#${utxo.outputIndex}`);
    if (cbor === undefined)
      throw new Error("Actual publication CBOR was not captured");
    return measureCompleteSignedTransaction(cbor);
  };
  const removalReferenceScriptPublications = {
    published,
    measurements: {
      correctionLockSpend: measurement(published.correctionLockSpend),
      stateQueueSpend: measurement(published.stateQueueSpend),
      stateQueueMint: measurement(published.stateQueueMint),
      stateQueueFraudRemovalWithdraw: measurement(
        published.stateQueueFraudRemovalWithdraw,
      ),
      activeOperatorsSpend: measurement(published.activeOperatorsSpend),
      activeOperatorsMint: measurement(published.activeOperatorsMint),
      retiredOperatorsSpend: measurement(published.retiredOperatorsSpend),
      retiredOperatorsMint: measurement(published.retiredOperatorsMint),
      schedulerSpend: measurement(published.schedulerSpend),
    },
  };
  const doubleSpendStepReferenceScripts = {
    fraudProofDoubleSpend: {
      utxo: reference("fraudProofDoubleSpend"),
      scriptHash:
        contracts.fraudProofContracts.doubleSpend.steps[0]!.spendingScriptHash,
    },
    fraudProofDoubleSpendStep02: {
      utxo: reference("fraudProofDoubleSpendStep02"),
      scriptHash:
        contracts.fraudProofContracts.doubleSpend.steps[1]!.spendingScriptHash,
    },
    fraudProofDoubleSpendStep03: {
      utxo: reference("fraudProofDoubleSpendStep03"),
      scriptHash:
        contracts.fraudProofContracts.doubleSpend.steps[2]!.spendingScriptHash,
    },
    fraudProofDoubleSpendStep04: {
      utxo: reference("fraudProofDoubleSpendStep04"),
      scriptHash:
        contracts.fraudProofContracts.doubleSpend.steps[3]!.spendingScriptHash,
    },
  };
  const witnessReferenceScripts = {
    computationThreadMint: reference("computationThreadMint"),
    fraudProofMint: reference("fraudProofMint"),
    phasMembershipWithdraw: reference("phasMembershipWithdraw"),
  };
  // Release the confirmed deployment chain’s wallet view before onboarding.
  funderLucid.clearUTxOOverride();
  await onboardEmulatorOperator({
    lucid: funderLucid,
    contracts,
    operatorKeyHash: paymentCredentialOf(funder.address).hash,
    registrationSlots: 2,
    awaitActivation: (time) => {
      while (BigInt(emulator.now()) <= time) emulator.awaitSlot(1);
    },
  });
  const initializedRoot = await requireUtxoWithUnit(
    funderLucid,
    contracts.stateQueue.spendingScriptAddress,
    contracts.stateQueue.policyId + SDK.STATE_QUEUE_ROOT_ASSET_NAME,
    "canonical initialized state queue",
  );
  const genesis = await Effect.runPromise(
    SDK.getConfirmedStateFromStateQueueDatum(
      await Effect.runPromise(
        SDK.getLinkedListNodeViewFromUTxO(initializedRoot),
      ),
    ),
  );
  const canonicalTransactions = [tx1InputsPreimage, tx2InputsPreimage].map(
    (inputs, index) =>
      buildFixtureTransaction({
        spendInputs: inputs.map(outputReferenceCbor),
        fee: BigInt(index),
      }),
  );
  while (BigInt(emulator.now()) <= genesis.data.endTime) emulator.awaitSlot(1);
  const canonical = await buildCanonicalL2BlockFixture(
    canonicalTransactions,
    genesis.data,
  );
  const nativeTransactions: readonly [
    MidgardNativeTxFull,
    MidgardNativeTxFull,
  ] = [
    materializeMidgardNativeTxFromCanonical(
      decodeMidgardNativeTxCanonical(canonicalTransactions[0]!.canonicalCbor),
    ),
    materializeMidgardNativeTxFromCanonical(
      decodeMidgardNativeTxCanonical(canonicalTransactions[1]!.canonicalCbor),
    ),
  ];
  return {
    realBlueprint,
    emulator,
    funderLucid,
    proverLucid,
    proverSigner,
    nonceUtxo: undefined,
    contracts,
    catalogue: await buildCatalogueDeploymentInfo(contracts.fraudProofs),
    canonical: {
      ...canonical,
      genesisEndTime: genesis.data.endTime,
      manifest: deployment.manifest,
      nativeTransactions,
      removalReferenceScriptPublications,
      doubleSpendStepReferenceScripts,
      witnessReferenceScripts,
    },
  };
};

export const commitInitializedProvedFixtureHeader = async ({
  lucid,
  contracts,
  header,
  schedulerStartTime,
}: {
  readonly schedulerStartTime: bigint;
  readonly lucid: Parameters<typeof submitHeaderCommitTx>[0]["lucid"];
  readonly contracts: Parameters<typeof submitHeaderCommitTx>[0]["contracts"];
  readonly header: SDK.Header;
}): Promise<
  Awaited<
    ReturnType<
      typeof import("./emulator/setup-tx.submit-setup-tx.js").submitSetupTx
    >
  >
> => {
  await Effect.runPromise(
    SDK.validateHeaderTransitionCommitmentsProgram(header),
  );
  const headerHash = await Effect.runPromise(SDK.hashBlockHeader(header));
  const units = setupUnits(contracts, header, headerHash);
  const snapshot = await Effect.runPromise(
    SDK.fetchOperatorDirectorySnapshotProgram(lucid, contracts),
  );
  const get = (address: string, unit: string) =>
    requireUtxoWithUnit(lucid, address, unit, "canonical initialized setup");
  const hubOracle = snapshot.hubOracle.utxo;
  const scheduler = await get(
    contracts.scheduler.spendingScriptAddress,
    units.scheduler,
  );
  const correctionLock = await get(
    contracts.correctionLock.spendingScriptAddress,
    units.correctionLock,
  );
  const stateQueueRoot = await get(
    contracts.stateQueue.spendingScriptAddress,
    units.stateQueueRoot,
  );
  const activeOperatorNode = await get(
    contracts.activeOperators.spendingScriptAddress,
    units.activeOperatorNode,
  );
  const activeOperatorsRoot = await get(
    contracts.activeOperators.spendingScriptAddress,
    units.activeOperatorsRoot,
  );
  const retiredOperatorsRoot = await get(
    contracts.retiredOperators.spendingScriptAddress,
    units.retiredOperatorsRoot,
  );
  const registeredOperatorsRoot = await get(
    contracts.registeredOperators.spendingScriptAddress,
    units.registeredOperatorsRoot,
  );
  const appointedScheduler = await submitSchedulerAppointmentTx({
    lucid,
    contracts,
    header: { ...header, startTime: schedulerStartTime },
    units,
    schedulerUtxo: scheduler,
    activeOperatorNode,
    registeredOperatorsRoot,
  });
  const committed = await submitHeaderCommitTx({
    lucid,
    contracts,
    header,
    headerHash,
    units,
    hubOracleUtxo: hubOracle,
    correctionLockUtxo: correctionLock,
    stateQueueRootUtxo: stateQueueRoot,
    appointedSchedulerUtxo: appointedScheduler,
    activeOperatorNode,
  });
  return {
    fraudulentBlockOutRef: `${committed.fraudulentBlockUtxo.txHash}#${committed.fraudulentBlockUtxo.outputIndex}`,
    headerHash,
    stateQueueBlockUnit: units.stateQueueBlock,
    stateQueueRootUnit: units.stateQueueRoot,
    hubOracle,
    scheduler: appointedScheduler,
    activeOperatorsRoot,
    activeOperatorsRootUnit: units.activeOperatorsRoot,
    retiredOperatorsRoot,
    retiredOperatorsRootUnit: units.retiredOperatorsRoot,
    activeOperatorNode: committed.continuedActiveOperatorNode,
    activeOperatorNodeUnit: units.activeOperatorNode,
    registeredOperatorsRoot,
  };
};

export const bindCanonicalFixtureHeader = async (
  canonical: NonNullable<
    Awaited<ReturnType<typeof buildProvedFixtureDeploymentContext>>["canonical"]
  >,
  header: SDK.Header,
  headerHash: string,
) => {
  const payload = {
    ...canonical.payload,
    block_body: {
      ...canonical.payload.block_body,
      header,
      header_hash: headerHash,
    },
  };
  return {
    ...canonical,
    header,
    headerHash,
    payload,
    payloadEnvelopeCbor: await wrapDaPayload(SDK.encodeDaPayload(payload), {
      mode: "identity",
    }),
  };
};
