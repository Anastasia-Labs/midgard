import { readFile, writeFile } from "node:fs/promises";
import { join } from "node:path";

import {
  CML,
  Emulator,
  generateEmulatorAccount,
  SLOT_CONFIG_NETWORK,
  unixTimeToEnclosingSlot,
} from "@lucid-evolution/lucid";
import { expect, vi } from "vitest";

import {
  activateOperatorProgram,
  registerOperatorProgram,
} from "../../src/transactions/register-active-operator.js";
import {
  initializeNodeRuntime,
  REGISTRATION_ACTIVATION_DELAY_SLOTS,
  REQUIRED_BOND_LOVELACE,
  resetActiveRuntimePaths,
  runNodeDatabaseEffect,
} from "../deposit-flow-emulator-shared.js";
import { resetApplicationTables } from "../utils.js";
import { TEST_CARDANO_PROTOCOL_PARAMETERS } from "./cardano-protocol-parameters.js";
import {
  captureConfirmedTransactions,
  type ConfirmedTransactionObservation,
} from "./confirmed-transaction-observations.js";
import { emulatorState, recreateLucid } from "./emulator-snapshot.js";
import { runWithoutFollower } from "./intent-journal.js";
import {
  createMainnetEmulatorLucid,
  MAINNET_PROTOCOL_PARAMETERS,
} from "./mainnet-protocol-parameters.js";
import {
  createPublishedWorkflowDeploymentAccounts,
  type PublishedWorkflowChain,
  publishWorkflowDeploymentOnChain,
} from "./published-workflow-deployment.js";
import { DEFAULT_PUBLICATION_SCHEDULE } from "./reference-publication-chain.js";
import { makeJournalDirectory } from "./run-journal-directory.js";
import {
  loadOrCreateRunSharedFixture,
  makeRunScratchDirectory,
} from "./run-shared-fixture-directory.js";

const preprodEmulatorLucid = (emulator: Emulator) =>
  createMainnetEmulatorLucid(emulator, "Preprod");

/** Freezes `value` and every object reachable through its data properties.
 * Byte arrays cannot be frozen and are left as they are. */
const deepFreeze = <T>(value: T, seen = new WeakSet<object>()): T => {
  if (
    (typeof value !== "object" && typeof value !== "function") ||
    value === null ||
    ArrayBuffer.isView(value) ||
    seen.has(value)
  )
    return value;
  seen.add(value);
  for (const key of Reflect.ownKeys(value)) {
    const property = Object.getOwnPropertyDescriptor(value, key);
    if (property !== undefined && "value" in property)
      deepFreeze(property.value, seen);
  }
  return Object.freeze(value);
};

type DeployedPrefix = Awaited<ReturnType<typeof deployPublishedPrefix>>;

/**
 * The deterministic chain prefix every lifecycle starts from: the actual
 * published deployment (reference scripts, atomic initialization, reward
 * registrations), a funded depositor, and the operator registered and
 * activated through the production programs. It is captured as plain data
 * the moment activation confirms, before any caller sees it.
 */
const deployPublishedPrefix = async (
  eventHistoryProtectionDurationMs?: bigint,
) => {
  await resetActiveRuntimePaths();
  await initializeNodeRuntime();
  const accounts = createPublishedWorkflowDeploymentAccounts();
  const p = MAINNET_PROTOCOL_PARAMETERS;
  const emulator = new Emulator([accounts.operator, accounts.publisher], p);
  emulator.time = 1_788_739_200_000;
  emulator.slot = unixTimeToEnclosingSlot(
    emulator.time,
    SLOT_CONFIG_NETWORK.Preprod,
  );
  emulator.blockHeight = Math.floor(emulator.slot / 20);
  const creation = { time: emulator.time, slot: emulator.slot };
  const operatorLucid = await createMainnetEmulatorLucid(emulator, "Preprod");
  const publisher = await createMainnetEmulatorLucid(emulator, "Preprod");
  operatorLucid.selectWallet.fromSeed(accounts.operator.seedPhrase);
  publisher.selectWallet.fromSeed(accounts.publisher.seedPhrase);
  const publications = new Map<
    string,
    { signedCbor: string; observedSlot: number }
  >();
  let observation: ReturnType<typeof captureConfirmedTransactions> | undefined;
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
      await makeJournalDirectory("midgard-published-lifecycle-"),
      "transactions.ndjson",
    ),
    publicationSchedule: DEFAULT_PUBLICATION_SCHEDULE,
    publicationSynchronize: async () => emulator.slot,
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
      // From initialization on, every confirmed transaction is observed.
      observation = captureConfirmedTransactions(
        operatorLucid,
        emulator,
        async () => {},
      );
    },
  });
  const { operatorLucid: lucid, publisherLucid, contracts } = published;
  vi.useFakeTimers({ toFake: ["Date"] });
  vi.setSystemTime(emulator.now());
  const depositorCreation = { time: emulator.time, slot: emulator.slot };
  const depositorAccount = generateEmulatorAccount({ lovelace: 0n });
  const depositorLucid = await createMainnetEmulatorLucid(emulator, "Preprod");
  depositorLucid.selectWallet.fromSeed(depositorAccount.seedPhrase);
  const fund = await lucid
    .newTx()
    .pay.ToAddress(depositorAccount.address, { lovelace: 1_000_000_000n })
    .complete({ localUPLCEval: true });
  const funded = await fund.sign.withWallet().complete();
  expect(await lucid.awaitTx(await funded.submit())).toBe(true);
  vi.setSystemTime(emulator.now());
  const emulatorCreationTimeMs = emulator.now();
  await runWithoutFollower(
    registerOperatorProgram(
      lucid,
      contracts,
      REQUIRED_BOND_LOVELACE,
      publisherLucid,
    ),
  );
  emulator.awaitSlot(REGISTRATION_ACTIVATION_DELAY_SLOTS);
  vi.setSystemTime(emulator.now());
  await runWithoutFollower(
    activateOperatorProgram(
      lucid,
      contracts,
      REQUIRED_BOND_LOVELACE,
      publisherLucid,
    ),
  );
  if (observation === undefined)
    throw new Error("Initialization never installed the transaction observer");
  const {
    chain: _chain,
    operatorLucid: _operator,
    publisherLucid: _publisher,
    ...deploymentData
  } = published;
  const snapshot = {
    accounts: structuredClone(accounts),
    depositorAccount: structuredClone(depositorAccount),
    creation,
    depositorCreation,
    emulatorCreationTimeMs,
    emulator: emulatorState(emulator),
    // Validators carry applied scripts only and every lifecycle of the file
    // shares them, so they are frozen: a mutation throws instead of leaking
    // into the next lifecycle. Every other deployment record is copied per
    // lifecycle.
    contracts: deepFreeze(contracts),
    deployment: structuredClone({ ...deploymentData, contracts: undefined }),
    publicationJournal: await readFile(published.publicationJournalPath),
    publications: structuredClone(publications),
    observation: observation.state(),
  };
  observation.restore();
  vi.useRealTimers();
  return snapshot;
};

/** Every lifecycle of one file restores the same deployment, so rows an
 * earlier lifecycle wrote would otherwise be found by the next one.
 * Returning every application table to its freshly migrated contents gives a
 * restored lifecycle a fresh node's starting state. The migrations' seed rows (the
 * `commit_build_calibration` singleton) are restored with it: a bare TRUNCATE
 * would leave them missing for every later file on this worker's database. */
const clearRestoredDeploymentRows = () =>
  runNodeDatabaseEffect(resetApplicationTables);

/** One deployment per protection duration per run: the first file to need
 * it deploys and shares it while files that start at the same time wait for
 * it (`run-shared-fixture-directory.ts`), and every later file restores that
 * same plain-data prefix. The deployment path reads no per-file setting and
 * no module a test file mocks. Without the package's global setup each file
 * deploys its own, as before. */
const loadOrDeployPublishedPrefix = async (
  eventHistoryProtectionDurationMs: bigint | undefined,
): Promise<DeployedPrefix> => {
  const { shared } = await loadOrCreateRunSharedFixture(
    `published-prefix-${String(eventHistoryProtectionDurationMs)}`,
    async () => ({
      shared: await deployPublishedPrefix(eventHistoryProtectionDurationMs),
      created: undefined,
    }),
  );
  deepFreeze(shared.contracts);
  return shared;
};

/** Every lifecycle opened by one test file restores the same captured prefix,
 * read or deployed once per file. */
const deployments = new Map<string, Promise<DeployedPrefix>>();

/** A fresh emulator, fresh lucid instances, a fresh observer and fresh
 * records holding exactly the state the deployment prefix left, with an
 * empty node database and fresh runtime paths. */
export const restorePublishedPrefix = async (
  eventHistoryProtectionDurationMs: bigint | undefined,
  onConfirmed: (
    observations: readonly ConfirmedTransactionObservation[],
  ) => Promise<void>,
) => {
  const key = String(eventHistoryProtectionDurationMs);
  let deploying = deployments.get(key);
  if (deploying === undefined) {
    deploying = loadOrDeployPublishedPrefix(eventHistoryProtectionDurationMs);
    deployments.set(key, deploying);
    deploying.catch(() => deployments.delete(key));
  }
  const prefix = await deploying;
  await resetActiveRuntimePaths();
  await initializeNodeRuntime();
  await clearRestoredDeploymentRows();
  const { accounts, depositorAccount, contracts } = prefix;
  const emulator = new Emulator([], MAINNET_PROTOCOL_PARAMETERS);
  Object.assign(emulator, structuredClone(prefix.emulator));
  const lucid = await recreateLucid(
    emulator,
    prefix.creation,
    accounts.operator.seedPhrase,
    preprodEmulatorLucid,
  );
  const publisherLucid = await recreateLucid(
    emulator,
    prefix.creation,
    accounts.publisher.seedPhrase,
    preprodEmulatorLucid,
  );
  const depositorLucid = await recreateLucid(
    emulator,
    prefix.depositorCreation,
    depositorAccount.seedPhrase,
    preprodEmulatorLucid,
  );
  const publications = structuredClone(prefix.publications);
  const observation = captureConfirmedTransactions(
    lucid,
    emulator,
    onConfirmed,
    prefix.observation,
  );
  // Each lifecycle gets its own copy of the deployment's journal (31 MB),
  // removed with the run rather than left behind in the temporary directory.
  const publicationJournalPath = join(
    await makeRunScratchDirectory("midgard-published-lifecycle-"),
    "transactions.ndjson",
  );
  await writeFile(publicationJournalPath, prefix.publicationJournal, {
    mode: 0o600,
  });
  const restored = structuredClone(prefix.deployment);
  // Typed as the published deployment's chain, whose waits may be
  // asynchronous on other chains; callers await them.
  const chain: PublishedWorkflowChain = {
    now: () => emulator.now(),
    delaySlots: (slots) => emulator.awaitSlot(slots),
    awaitLedgerTime: (time) => {
      const slots = Math.ceil((time - emulator.now()) / 1000);
      if (slots > 0) emulator.awaitSlot(slots);
    },
  };
  const deployment = {
    ...restored,
    publicationJournalPath,
    contracts,
    chain,
    operatorLucid: lucid,
    publisherLucid,
    emulator,
  };
  return {
    prefix,
    emulator,
    lucid,
    publisherLucid,
    depositorLucid,
    publications,
    observation,
    deployment,
  };
};
