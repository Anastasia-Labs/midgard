import { mkdtemp, readFile, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  Emulator,
  generateEmulatorAccount,
  type LucidEvolution,
  SLOT_CONFIG_NETWORK,
  unixTimeToEnclosingSlot,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
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
  type AcceptedHistoryObservation,
  captureConfirmedHistoryObservations,
  historyOutputObservation,
} from "./history-projection-observations.js";
import { type RecordedHistoryBatch } from "./history-source-owner-emulator.recorded-history-batch.js";
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

/** Plain data of an emulator ledger: every own non-function field. The
 * observer's wrapped `submitTx`/`awaitTx` are the only own functions. */
type EmulatorState = Record<string, unknown>;

const emulatorState = (emulator: Emulator): EmulatorState =>
  structuredClone(
    Object.fromEntries(
      Object.entries(emulator).filter(
        ([, value]) => typeof value !== "function",
      ),
    ),
  );

/** The UTxO view a wallet has pinned with `overrideUTxOs`, or `undefined`
 * when it reads the provider. The wallet object keeps the pin private, so the
 * provider read is answered with a sentinel for this one call. */
const pinnedWalletUtxos = async (
  lucid: LucidEvolution,
  emulator: Emulator,
): Promise<UTxO[] | undefined> => {
  const sentinel: UTxO[] = [];
  const own = Object.getOwnPropertyDescriptor(emulator, "getUtxos");
  emulator.getUtxos = () => Promise.resolve(sentinel);
  try {
    const utxos = await lucid.wallet().getUtxos();
    return utxos === sentinel ? undefined : structuredClone(utxos);
  } finally {
    if (own === undefined)
      delete (emulator as Partial<Pick<Emulator, "getUtxos">>).getUtxos;
    else Object.defineProperty(emulator, "getUtxos", own);
  }
};

/** A lucid instance created exactly as the deployment created its own: the
 * emulator lucid's slot config is read from the emulator at creation. */
const recreateLucid = async (
  emulator: Emulator,
  at: { readonly time: number; readonly slot: number },
  seedPhrase: string,
  pinned: readonly UTxO[] | undefined,
) => {
  const { time, slot } = emulator;
  emulator.time = at.time;
  emulator.slot = at.slot;
  let lucid: LucidEvolution;
  try {
    lucid = await createMainnetEmulatorLucid(emulator, "Preprod");
  } finally {
    emulator.time = time;
    emulator.slot = slot;
  }
  lucid.selectWallet.fromSeed(seedPhrase);
  if (pinned !== undefined) lucid.overrideUTxOs(structuredClone([...pinned]));
  return lucid;
};

type DeployedHistorySource = Awaited<ReturnType<typeof deployHistorySource>>;

/**
 * The deterministic chain prefix every lifecycle starts from: the actual
 * published deployment (reference scripts, atomic initialization, reward
 * registrations), a funded depositor, and the operator registered and
 * activated through the production programs. It is captured as plain data
 * the moment activation confirms, before any caller sees it.
 */
const deployHistorySource = async (
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
  const batches: RecordedHistoryBatch[] = [];
  const publications = new Map<
    string,
    { signedCbor: string; observedSlot: number }
  >();
  let preparedContracts: Awaited<
    ReturnType<typeof loadRealMidgardContractsForTest>
  >;
  let observedAddresses: string[] | undefined;
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
      observedAddresses = addresses;
      observation = captureConfirmedHistoryObservations(
        operatorLucid,
        emulator,
        recordHistoryBatch(batches, operatorLucid, emulator, addresses),
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
  lucid.overrideUTxOs(await lucid.utxosAt(await lucid.wallet().address()));
  vi.setSystemTime(emulator.now());
  const emulatorCreationTimeMs = emulator.now();
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
  if (observation === undefined || observedAddresses === undefined)
    throw new Error("Initialization never installed the history observer");
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
    wallets: {
      operator: await pinnedWalletUtxos(lucid, emulator),
      publisher: await pinnedWalletUtxos(publisherLucid, emulator),
      depositor: await pinnedWalletUtxos(depositorLucid, emulator),
    },
    // Validators carry applied scripts only and are never mutated; every
    // other deployment record is copied per lifecycle.
    contracts,
    deployment: structuredClone({ ...deploymentData, contracts: undefined }),
    publicationJournal: await readFile(published.publicationJournalPath),
    batches: structuredClone(batches),
    publications: structuredClone(publications),
    observedAddresses,
    observation: observation.state(),
  };
  observation.restore();
  vi.useRealTimers();
  return snapshot;
};

/** Every lifecycle of one file restores the same deployment, so rows an
 * earlier lifecycle keyed by its binding (census frontier, journal, receipts)
 * would otherwise be found by the next one. When each lifecycle published its
 * own deployment, those rows were keyed to an older binding and never read.
 * Returning every application table to its freshly migrated contents gives a
 * restored lifecycle that same starting state. The migrations' seed rows (the
 * `commit_build_calibration` singleton) are restored with it: a bare TRUNCATE
 * would leave them missing for every later file on this worker's database. */
const clearRestoredDeploymentRows = () =>
  runNodeDatabaseEffect(resetApplicationTables);

/** Every lifecycle opened by one test file restores the same captured prefix
 * (one deployment per protection duration per file). */
const deployments = new Map<string, Promise<DeployedHistorySource>>();

const recordHistoryBatch =
  (
    batches: RecordedHistoryBatch[],
    operatorLucid: LucidEvolution,
    emulator: Emulator,
    addresses: readonly string[],
    onBatch: () => (
      observations: readonly AcceptedHistoryObservation[],
    ) => Promise<void> = () => async () => {},
  ) =>
  async (observations: readonly AcceptedHistoryObservation[]) => {
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
    await onBatch()(observations);
  };

/** A fresh emulator, fresh lucid instances, a fresh observer and fresh
 * records holding exactly the state the deployment prefix left, with an
 * empty node database and fresh runtime paths. */
export const restoreHistorySource = async (
  eventHistoryProtectionDurationMs: bigint | undefined,
  onBatch: () => (
    observations: readonly AcceptedHistoryObservation[],
  ) => Promise<void>,
) => {
  const key = String(eventHistoryProtectionDurationMs);
  let deploying = deployments.get(key);
  if (deploying === undefined) {
    deploying = deployHistorySource(eventHistoryProtectionDurationMs);
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
    prefix.wallets.operator,
  );
  const publisherLucid = await recreateLucid(
    emulator,
    prefix.creation,
    accounts.publisher.seedPhrase,
    prefix.wallets.publisher,
  );
  const depositorLucid = await recreateLucid(
    emulator,
    prefix.depositorCreation,
    depositorAccount.seedPhrase,
    prefix.wallets.depositor,
  );
  const batches = structuredClone(prefix.batches);
  const publications = structuredClone(prefix.publications);
  const observation = captureConfirmedHistoryObservations(
    lucid,
    emulator,
    recordHistoryBatch(
      batches,
      lucid,
      emulator,
      prefix.observedAddresses,
      onBatch,
    ),
    prefix.observation,
  );
  const publicationJournalPath = join(
    await mkdtemp(join(tmpdir(), "midgard-history-owner-")),
    "transactions.ndjson",
  );
  await writeFile(publicationJournalPath, prefix.publicationJournal, {
    mode: 0o600,
  });
  const restored = structuredClone(prefix.deployment);
  const deployment = {
    ...restored,
    publicationJournalPath,
    contracts,
    chain: {
      now: () => emulator.now(),
      delaySlots: (slots: number) => emulator.awaitSlot(slots),
      awaitLedgerTime: (time: number) => {
        const slots = Math.ceil((time - emulator.now()) / 1000);
        if (slots > 0) emulator.awaitSlot(slots);
      },
    },
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
    batches,
    publications,
    observation,
    deployment,
  };
};
