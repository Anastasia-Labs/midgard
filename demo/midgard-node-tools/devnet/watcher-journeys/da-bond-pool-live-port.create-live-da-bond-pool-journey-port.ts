import { execFileSync } from "node:child_process";
import { randomBytes } from "node:crypto";
import { existsSync, mkdirSync } from "node:fs";
import { join, resolve } from "node:path";
import { setTimeout as pause } from "node:timers/promises";

import { openAvailabilityOperationJournal } from "@al-ft/midgard-core/availability-operation-journal";
import { depositEventsRetainedBlock } from "@al-ft/midgard-fault-proofs/test-support/transition-trace-retained";
import * as SDK from "@al-ft/midgard-sdk";
import {
  credentialToAddress,
  Data,
  Lucid,
  paymentCredentialOf,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";
import { createScalusEvaluator } from "@lucid-evolution/scalus-uplc";
import {
  DA_L1_SUBMITTER_MIN_PLAIN_ADA_LOVELACE,
  DEFAULT_L1_SUBMITTER_PREFLIGHT,
} from "da-committee-node/config";
import { Effect } from "effect";
import {
  buildAvailabilityCommandTransaction,
  planAvailabilityCommandAction,
} from "midgard-node/commands/availability-challenge";
import {
  authenticatedManifestReference,
  availabilityDeploymentFromManifest,
  availabilityParametersFromManifest,
  manifestReferenceScriptAuthPolicy,
  mintingValidatorOf,
  spendingValidatorOf,
} from "midgard-node/commands/availability-challenge-deployment";
import { availabilityCommandCanonicalSource } from "midgard-node/commands/availability-challenge-source";
import {
  type DaBondContext,
  daBondStatusCommand,
} from "midgard-node/commands/da-bond";
import { daLocalSigners } from "midgard-node/da/local-signers";
import { fetchKupoSpend } from "midgard-node/l1-kupmios";
import {
  authenticWatcherDaBondPool,
  deriveWatcherDaBondPoolObservation,
} from "midgard-watcher";
import { createPublishedWatcherBlockActor } from "midgard-watcher/tests/support/published-block-actor";

import {
  readJourneyArtifact,
  writeJourneyArtifact,
  writeJourneyFile,
} from "./artifacts.js";
import {
  createDaBondPoolCli,
  DaBondCliProcessError,
  spawnDaBondCliProcess,
} from "./da-bond-pool-cli-process.js";
import {
  buildDaBondPoolCommitteeEnv,
  createDaBondPoolCommitteeObserver,
  DA_BOND_POOL_INHERITED_ENV,
  daBondPoolCommitteeExpectedView,
  daBondPoolCommitteeLifecycle,
  daBondPoolCommitteeSyncBoundMs,
  spawnDaBondPoolCommitteeNode,
  worktreeDerivedPort,
} from "./da-bond-pool-committee-process.js";
import {
  type DaBondPoolCommitteeRuntimeEvidence,
  daBondPoolCommitteeSettings,
  planDaBondPoolCommitteeRuntime,
  produceDaBondPoolCommitteeRuntime,
  readWorktreePortOffset,
  reuseDaBondPoolCommitteeRuntime,
  spawnDaBondPoolRuntimeProcess,
  verifyDaBondPoolCommitteeRuntime,
} from "./da-bond-pool-committee-runtime.js";
import type {
  DaBondPoolJourneyAlerts,
  DaBondPoolJourneyResume,
  DaBondPoolJourneySnapshot,
  DaBondPoolJourneyStep,
} from "./da-bond-pool-journey.js";
import {
  availabilityAttemptRecovery,
  awaitAvailabilityInclusion,
  awaitQuietJournal,
  landAvailabilitySubmission,
  MAX_LAPSED_REPLANS,
  prepareAvailabilityAttempt,
  settleExpiredCommitReads,
} from "./da-bond-pool-live-port.await-availability-inclusion.js";
import {
  attestWithinLedgerValidity,
  type AvailabilityRequest,
  commitWithinLedgerValidity,
  DA_BOND_POOL_COMMITTEE_SUBMITTER_SECRETS,
  daBondPoolCommitteeL1,
  DaBondPoolCommitteeUnavailableError,
  DaBondPoolJourneyQueueNotEmptyError,
  DaBondPoolJourneyResumeMismatchError,
  JOURNEY_DEPLOYMENT_MANIFEST,
  type JourneyBlock,
  type LiveDaBondPoolJourneyPort,
  type LiveDaBondPoolJourneyPortOptions,
  loadOrCreateSeed,
  REPOSITORY_ROOT,
  requireResumableQueue,
} from "./da-bond-pool-live-port.commit-within-ledger-validity.js";
import {
  absentBlockStatus,
  attestRefusalResult,
  awaitTimeBudgetMs,
  findJourneyDaemons,
  nextJourneyBlockInterval,
  planDaBondOwnerQuorum,
  readLinuxProcesses,
  requireJourneySeed,
} from "./da-bond-pool-live-port.da-bond-pool-apply-refusal.js";
import {
  ACTION_DEPTH_TIMEOUT_MS,
  assertDistinctChallengerKey,
  COMMIT_FRESH_TIP_MS,
  COMMIT_VALIDITY_ATTEMPTS,
  DA_BOND_POOL_CHALLENGER_SECRET,
  DaBondJourneySigningMaterialError,
  daBondPoolChallengerFundingShortfall,
  daBondPoolJourneyDirectory,
  daBondPoolJourneyParamsOf,
  INCLUSION_TIMEOUT_MS,
  JOURNAL_QUIET_TIMEOUT_MS,
  JOURNEY_ACCOUNTS_SECRET,
  journeyEndpointsFromRunEnv,
  kupoMatchesEverything,
  type LiveJourneyContext,
  MAX_REMOVAL_STEPS,
  MAX_RESPONSE_TRANSACTIONS,
  MAX_SETTLEMENTS,
  MAX_TRANSIENT_RETRIES,
  outRefOf,
  planDaBondPoolChallengerFunding,
  POLL_MS,
  selectDaBondPoolChallengerCoins,
} from "./da-bond-pool-live-port.select-da-bond-pool-challenger-coins.js";
import {
  decodeJourneyTransaction,
  isTransientCanonicalError,
  summarizeDaBondPoolTimeout,
} from "./da-bond-pool-live-port.summarize-da-bond-pool-timeout.js";
import { describeErrorChain } from "./error-chain.js";
import { readJourneyCadence } from "./journey-timing.js";
import {
  awaitLedgerTipSlot,
  readOgmiosTipSlot,
  retryOgmiosTransport,
} from "./ledger-tip.js";
import { JOURNEY_ACTION_DEPTH } from "./live-context.js";

/**
 * Builds the live port. Checks every precondition (no daemon, Kupo indexes
 * everything, root-only queue, owner quorum held) and funds the challenger
 * before returning.
 */
export const createLiveDaBondPoolJourneyPort = async (
  context: LiveJourneyContext,
  options: LiveDaBondPoolJourneyPortOptions = {},
): Promise<LiveDaBondPoolJourneyPort> => {
  const { deployment, accounts, provider, customNetwork, runDirectory } =
    context;
  const { manifest, contracts, chain } = deployment;
  const log =
    options.log ??
    ((line: string) => console.info(`DA bond pool journey: ${line}`));
  // The availability journal needs an absolute, normalized path.
  const artifactDirectory = resolve(
    options.artifactDirectory ?? daBondPoolJourneyDirectory(runDirectory),
  );
  mkdirSync(artifactDirectory, { recursive: true });
  // A resumed run keeps the earlier run's state (journals, committee
  // database and cursor) and writes its evidence apart, so it never
  // overwrites that run's records.
  const evidenceDirectory =
    options.resume === undefined
      ? artifactDirectory
      : join(
          artifactDirectory,
          `resume-${new Date().toISOString().replaceAll(":", "-")}`,
        );
  mkdirSync(evidenceDirectory, { recursive: true });

  // Preconditions that need no chain read.
  const endpoints = journeyEndpointsFromRunEnv(context.runEnv);
  if (
    endpoints.kupoUrl !== context.kupoUrl ||
    endpoints.ogmiosUrl !== context.ogmiosUrl
  )
    throw new Error("Journey context endpoints differ from its run.env");
  const listProcesses = options.listProcesses ?? readLinuxProcesses;
  // The journey daemons must be exactly the adapter's own committee node.
  const checkDaemons = (admitted: ReadonlySet<number>): void => {
    const daemons = findJourneyDaemons(listProcesses(), runDirectory, admitted);
    if (daemons.length > 0)
      throw new Error(
        `Refusing to run the DA bond pool journey while a watcher or DA committee daemon other than its own committee node runs against ${runDirectory}: ${daemons.join("; ")}. Stop the session watcher first: it would contest the journey's challenges, and a committee node holding the payload would answer the withheld block.`,
      );
  };
  checkDaemons(new Set());
  const patternsResponse = await fetch(`${context.kupoUrl}/patterns`, {
    signal: AbortSignal.timeout(20_000),
  });
  if (!patternsResponse.ok)
    throw new Error(
      `Kupo refused its pattern list: HTTP ${patternsResponse.status.toString()}`,
    );
  const patterns: unknown = await patternsResponse.json();
  if (!kupoMatchesEverything(patterns))
    throw new Error(
      `The DA bond pool journey needs Kupo to match "*" (every address); it matches ${JSON.stringify(patterns)}`,
    );

  const accountsSource = join(runDirectory, JOURNEY_ACCOUNTS_SECRET);
  const operatorSeed = requireJourneySeed(accounts, "operator", accountsSource);
  const cosignerSeed = requireJourneySeed(accounts, "cosigner", accountsSource);
  const availabilitySeed = requireJourneySeed(
    accounts,
    "availability",
    accountsSource,
  );
  const daSignerConfig = {
    NETWORK: "Custom" as const,
    L1_OPERATOR_SEED_PHRASE: operatorSeed,
    DA_COSIGNER_SEED_PHRASE: cosignerSeed,
  };
  const seedByRole: Readonly<Record<string, string>> = {
    operator: operatorSeed,
    cosigner: cosignerSeed,
  };
  const localSigners = daLocalSigners(daSignerConfig);

  const newLucid = () =>
    Lucid(provider, "Custom", {
      slotConfig: customNetwork.slotConfig,
      evaluator: createScalusEvaluator(),
    });
  const readLucid = await newLucid();
  const network = readLucid.config().network;
  if (network === undefined || network !== manifest.network)
    throw new Error("Journey Lucid network differs from the deployment");

  // Clock and depth.
  const tipTime = async (): Promise<number> =>
    readLucid.slotToUnixTime(await readOgmiosTipSlot(context.ogmiosUrl));
  const blockHeight = chain.blockHeight;
  if (blockHeight === undefined)
    throw new Error("The journey chain does not report its block height");
  const awaitActionDepth = async (): Promise<void> => {
    const target =
      (await retryOgmiosTransport(blockHeight)) + JOURNEY_ACTION_DEPTH;
    const deadline = Date.now() + ACTION_DEPTH_TIMEOUT_MS;
    for (;;) {
      const height = await retryOgmiosTransport(blockHeight);
      if (height >= target) return;
      if (Date.now() > deadline)
        throw new Error(
          `The chain stalled at block ${height.toString()} before depth ${JOURNEY_ACTION_DEPTH.toString()}`,
        );
      await pause(1_000);
    }
  };

  // The state queue must hold only its root.
  const sortedQueue = () =>
    SDK.fetchSortedStateQueueUTxOs(readLucid, {
      stateQueueAddress: contracts.stateQueue.spendingScriptAddress,
      stateQueuePolicyId: contracts.stateQueue.policyId,
    });
  const initialQueue = await sortedQueue();
  let resumedB2: JourneyBlock | undefined;
  if (options.resume === undefined) {
    if (initialQueue.length !== 1)
      throw new DaBondPoolJourneyQueueNotEmptyError(initialQueue.length - 1);
  } else {
    const b2Path = join(artifactDirectory, "block-B2.json");
    if (!existsSync(b2Path))
      throw new DaBondPoolJourneyResumeMismatchError(`${b2Path} is missing`);
    resumedB2 = await readJourneyArtifact<JourneyBlock>(b2Path);
    requireResumableQueue(
      initialQueue
        .slice(1)
        .map(({ datum }) =>
          datum.key === "Empty" ? "Empty" : datum.key.Key.key,
        ),
      resumedB2.headerHash,
    );
  }

  // The da-bond command context, as loadDaBondContext builds it.
  const authPolicy = manifestReferenceScriptAuthPolicy(manifest);
  const bondLucid = await newLucid();
  const poolSpending = await authenticatedManifestReference(
    bondLucid,
    manifest,
    authPolicy,
    "daBondPoolSpend",
    "da-bond-pool spending",
  );
  const poolMinting = await authenticatedManifestReference(
    bondLucid,
    manifest,
    authPolicy,
    "daBondPoolMint",
    "da-bond-pool minting",
  );
  const poolValidator: SDK.AuthenticatedValidator = {
    ...spendingValidatorOf(network, poolSpending.scriptRef),
    ...mintingValidatorOf(poolMinting.scriptRef),
  };
  if (
    poolValidator.policyId !== contracts.daBondPool.policyId ||
    poolValidator.spendingScriptAddress !==
      contracts.daBondPool.spendingScriptAddress
  )
    throw new Error(
      "The manifest's DA bond pool references differ from the deployment",
    );
  const governorSpend = manifest.contracts.daParamsGovernorSpend?.scriptHash;
  const governorMint = manifest.contracts.daParamsGovernorMint?.scriptHash;
  if (governorSpend === undefined || governorMint === undefined)
    throw new Error("Deployment omits the DA params governor");
  const parameters = availabilityParametersFromManifest(manifest);
  const journeyParams = daBondPoolJourneyParamsOf(
    parameters,
    manifest.deploymentProfile.timing,
  );
  const daParamsGovernor = {
    address: credentialToAddress(network, {
      type: "Script",
      hash: governorSpend,
    }),
    unit: toUnit(governorMint, SDK.DA_PARAMS_ASSET_NAME),
  };
  // The da-bond commands read a synchronous clock; the adapter sets it to the
  // ledger tip before each command, since the node checks validity bounds
  // against the tip rather than the wall clock.
  let bondNow = await tipTime();
  const refreshBondNow = async () => {
    bondNow = await tipTime();
  };
  const bondContext: DaBondContext = {
    lucid: bondLucid,
    network,
    manifestId: manifest.manifestId,
    poolValidator,
    poolSpendingReference: poolSpending,
    parameters,
    daParamsGovernor,
    withdrawDelayMs: BigInt(journeyParams.withdrawDelayMs),
    now: () => bondNow,
    // Pool transactions go through the da-bond CLI processes only (P18).
    submit: async () => {
      throw new Error(
        "The live DA bond pool journey submits pool transactions only through the da-bond CLI",
      );
    },
  };

  // The owner quorum must be keys this run holds.
  const daParamsUtxos = await readLucid.utxosAtWithUnit(
    daParamsGovernor.address,
    daParamsGovernor.unit,
  );
  if (daParamsUtxos.length !== 1 || typeof daParamsUtxos[0]!.datum !== "string")
    throw new Error("Expected one DA params UTxO with an inline datum");
  const daParams = Data.from(daParamsUtxos[0]!.datum, SDK.DaParamsDatum);
  const quorum = planDaBondOwnerQuorum({
    owners: daParams.owners,
    updateThreshold: daParams.update_threshold,
    held: localSigners.map((signer) => ({
      role: signer.role,
      keyHash: signer.keyHashHex,
    })),
    source: `${accountsSource} (operator and cosigner seed phrases)`,
  });
  const availabilityAddress = accounts.availability.address;

  // The committee node's DA libp2p runtime (P31): fresh libp2p keys and the
  // committee-target runtime manifest from the real
  // `da-libp2p-generate-manifest` process, over the finalized deployment
  // manifest's DA committee. It runs before any transaction of this journey.
  const committeeDirectory = join(artifactDirectory, "committee");
  mkdirSync(committeeDirectory, { recursive: true });
  const manifestPath = join(runDirectory, JOURNEY_DEPLOYMENT_MANIFEST);
  const committeeBin = join(
    REPOSITORY_ROOT,
    "demo/da-committee-node/dist/index.js",
  );
  const cliBin = join(REPOSITORY_ROOT, "demo/midgard-node/dist/index.js");
  const missing = [manifestPath, committeeBin, cliBin].filter(
    (path) => !existsSync(path),
  );
  if (missing.length > 0)
    throw new DaBondPoolCommitteeUnavailableError(
      `missing ${missing.join(", ")}`,
    );
  const inheritedEnv = Object.fromEntries(
    DA_BOND_POOL_INHERITED_ENV.flatMap((name) => {
      const value = process.env[name];
      return value === undefined ? [] : [[name, value]];
    }),
  );
  let committeeRuntime: DaBondPoolCommitteeRuntimeEvidence;
  try {
    const runtimePlan = planDaBondPoolCommitteeRuntime({
      runDirectory,
      deployment: manifest,
      portOffset: readWorktreePortOffset(REPOSITORY_ROOT),
    });
    mkdirSync(join(runDirectory, "secrets"), { recursive: true, mode: 0o700 });
    committeeRuntime =
      options.resume === undefined
        ? await produceDaBondPoolCommitteeRuntime({
            plan: runtimePlan,
            command: [process.execPath, cliBin],
            env: {
              ...inheritedEnv,
              MIDGARD_CONFIG_MODE: "disabled",
              MIDGARD_DOTENV_MODE: "disabled",
            },
            cwd: committeeDirectory,
            run: spawnDaBondPoolRuntimeProcess(120_000),
          })
        : reuseDaBondPoolCommitteeRuntime({
            plan: runtimePlan,
            recordedEvidencePath: join(committeeDirectory, "runtime.json"),
          });
  } catch (cause) {
    throw new DaBondPoolCommitteeUnavailableError(
      `its DA libp2p runtime could not be produced: ${cause instanceof Error ? cause.message : String(cause)}`,
      { cause },
    );
  }
  if (options.resume === undefined)
    await writeJourneyArtifact(
      join(committeeDirectory, "runtime.json"),
      committeeRuntime,
    );
  log(
    `${options.resume === undefined ? "generated" : "reused"} the committee runtime manifest ${committeeRuntime.outPath} (sha256 ${committeeRuntime.outputSha256}); the observer is member ${committeeRuntime.observer.signerIndex.toString()} (${committeeRuntime.observer.peerId})`,
  );
  // The committee's evidence: its records, logs and settings.
  const committeeEvidenceDirectory = join(evidenceDirectory, "committee");
  mkdirSync(committeeEvidenceDirectory, { recursive: true });
  const runtimeManifestPath = committeeRuntime.outPath;
  const libp2pKeySource = committeeRuntime.observer.libp2pKeySource;

  // The challenger: a fresh key, distinct from every operational key.
  const challengerSeed = loadOrCreateSeed(
    join(runDirectory, DA_BOND_POOL_CHALLENGER_SECRET),
  );
  const challengerLucid = await newLucid();
  challengerLucid.selectWallet.fromSeed(challengerSeed, {
    addressType: "Enterprise",
  });
  const challengerAddress = await challengerLucid.wallet().address();
  const challengerKey = paymentCredentialOf(challengerAddress).hash;
  assertDistinctChallengerKey(challengerKey, {
    operator: paymentCredentialOf(accounts.operator.address).hash,
    publisher: paymentCredentialOf(accounts.publisher.address).hash,
    cosigner: paymentCredentialOf(accounts.cosigner.address).hash,
    availability: paymentCredentialOf(availabilityAddress).hash,
    "reference-script deployer": paymentCredentialOf(
      manifest.referenceScriptDeployAddress,
    ).hash,
  });
  const protocol = challengerLucid.config().protocolParameters;
  if (protocol === undefined)
    throw new Error("The challenger needs live ledger parameters");
  const plan = planDaBondPoolChallengerFunding({
    parameters,
    collateralPercentage: protocol.collateralPercentage,
    ...(options.operatingLovelace === undefined
      ? {}
      : { operatingLovelace: options.operatingLovelace }),
  });
  const shortfall = daBondPoolChallengerFundingShortfall({
    utxos: await challengerLucid.wallet().getUtxos(),
    address: challengerAddress,
    plan,
  });
  if (shortfall.length > 0) {
    log(
      `funding challenger ${challengerAddress} from the availability account: ${shortfall.join(", ")} lovelace`,
    );
    const funder = await newLucid();
    funder.selectWallet.fromSeed(availabilitySeed);
    let fundingTx = funder.newTx();
    for (const lovelace of shortfall)
      fundingTx = fundingTx.pay.ToAddress(challengerAddress, { lovelace });
    const signed = await (await fundingTx.complete()).sign
      .withWallet()
      .complete();
    const txHash = await signed.submit();
    await funder.awaitTx(txHash);
    await awaitActionDepth();
    await writeJourneyArtifact(join(evidenceDirectory, "challenger.json"), {
      challengerAddress,
      challengerKeyHash: challengerKey,
      fundingTxId: txHash,
      outputs: shortfall,
      plan,
    });
  }

  // The committee node (P27): its submitter keys, database and environment.
  const submitterKey = async (secret: string) => {
    const path = join(runDirectory, secret);
    const seed = loadOrCreateSeed(path);
    const lucid = await newLucid();
    // The node selects its submitter wallets from the seed with Lucid's
    // default (base) address, so the adapter reads the same address.
    lucid.selectWallet.fromSeed(seed);
    const address = await lucid.wallet().address();
    return {
      source: `file:${path}`,
      keyHash: paymentCredentialOf(address).hash,
      address,
    };
  };
  const l1Submitter = await submitterKey(
    DA_BOND_POOL_COMMITTEE_SUBMITTER_SECRETS.l1,
  );
  const availabilitySubmitter = await submitterKey(
    DA_BOND_POOL_COMMITTEE_SUBMITTER_SECRETS.availability,
  );
  const { nativeLedger: nativeLedgerPaths, l1Origin: committeeL1Origin } =
    await daBondPoolCommitteeL1({
      runDirectory,
      networkMagic: customNetwork.networkMagic,
      nonceTxHash: manifest.hubOracleOneShot.txHash,
    });
  const postgres = {
    database: context.runEnv.MIDGARD_PHASE4_POSTGRES_DATABASE,
    user: context.runEnv.MIDGARD_PHASE4_POSTGRES_USER,
    password: context.runEnv.MIDGARD_PHASE4_POSTGRES_PASSWORD,
    port: context.runEnv.MIDGARD_PHASE4_POSTGRES_PORT,
    project: context.runEnv.MIDGARD_PHASE4_COMPOSE_PROJECT,
  };
  const postgresMissing = Object.entries(postgres)
    .filter(([, value]) => value === undefined || value === "")
    .map(([name]) => name);
  if (postgresMissing.length > 0)
    throw new DaBondPoolCommitteeUnavailableError(
      `run.env lacks the devnet Postgres ${postgresMissing.join(", ")}`,
    );
  // A resumed run restarts the node on the database its earlier run left,
  // as the normal run's restart before step 6 does.
  const recordedCommittee =
    options.resume === undefined
      ? undefined
      : await readJourneyArtifact<{ database?: unknown }>(
          join(committeeDirectory, "committee.json"),
        );
  if (
    recordedCommittee !== undefined &&
    (typeof recordedCommittee.database !== "string" ||
      !/^da_bond_pool_committee_[0-9a-f]{8}$/u.test(recordedCommittee.database))
  )
    throw new DaBondPoolJourneyResumeMismatchError(
      `${join(committeeDirectory, "committee.json")} records no committee database`,
    );
  const committeeDatabase =
    (recordedCommittee?.database as string | undefined) ??
    `da_bond_pool_committee_${randomBytes(4).toString("hex")}`;
  const committeeDatabaseUrl = new URL(
    `postgres://127.0.0.1:${postgres.port!}/${committeeDatabase}`,
  );
  committeeDatabaseUrl.username = postgres.user!;
  committeeDatabaseUrl.password = postgres.password!;
  const apiPort = worktreeDerivedPort(
    REPOSITORY_ROOT,
    "da-bond-pool-committee-api",
  );
  // P27(4): the node's sync wait is bounded by its own poll cadence plus
  // twice the ideal time for the release confirmation depth on this devnet.
  const committeePollIntervalMs = 2_000;
  const cadence = await readJourneyCadence(runDirectory, {
    authenticatedConfirmationDepth: manifest.l1Finality.confirmationDepth,
  });
  // The observer also derives from it how long each stop watches the
  // submitter addresses: one node poll plus the same finality lag, so a
  // last-tick submission cannot land unseen.
  const committeeCadence = {
    pollIntervalMs: committeePollIntervalMs,
    confirmationDepth: cadence.confirmationDepth,
    slotLengthMs: cadence.slotLengthSeconds * 1000,
    activeSlotsCoeff: cadence.activeSlotsCoeff,
  };
  const committeeSyncTimeoutMs =
    daBondPoolCommitteeSyncBoundMs(committeeCadence);
  const committee = buildDaBondPoolCommitteeEnv({
    settings: daBondPoolCommitteeSettings({
      runtimeManifestPath,
      deploymentManifestPath: manifestPath,
      network: manifest.network,
      networkMagic: customNetwork.networkMagic,
      l1Origin: committeeL1Origin,
      finalityDepth: manifest.l1Finality.confirmationDepth,
      nativeLedger: nativeLedgerPaths,
    }),
    l1Submitter,
    availabilitySubmitter,
    operationalKeyHashes: {
      operator: paymentCredentialOf(accounts.operator.address).hash,
      publisher: paymentCredentialOf(accounts.publisher.address).hash,
      cosigner: paymentCredentialOf(accounts.cosigner.address).hash,
      availability: paymentCredentialOf(availabilityAddress).hash,
      "reference-script deployer": paymentCredentialOf(
        manifest.referenceScriptDeployAddress,
      ).hash,
      challenger: challengerKey,
    },
    libp2pKeySource,
    journalPath: join(committeeDirectory, "availability-journal.sqlite"),
    databaseUrl: committeeDatabaseUrl.toString(),
    apiHost: "127.0.0.1",
    apiPort,
    pollIntervalMs: committeePollIntervalMs,
    inherited: process.env,
  });
  // P27(8), P31(6): the node's own configuration loader accepts this
  // environment and the runtime manifest's peer set admits its libp2p
  // identity, before the node's database is created.
  try {
    await verifyDaBondPoolCommitteeRuntime(committee.env);
  } catch (cause) {
    throw new DaBondPoolCommitteeUnavailableError(
      `its configuration is refused: ${cause instanceof Error ? cause.message : String(cause)}`,
      { cause },
    );
  }
  if (recordedCommittee === undefined)
    execFileSync(
      "docker",
      [
        "exec",
        "-i",
        `${postgres.project!}-postgres-1`,
        "psql",
        "-U",
        postgres.user!,
        "-d",
        postgres.database!,
        "-v",
        "ON_ERROR_STOP=1",
      ],
      {
        input: `CREATE DATABASE ${committeeDatabase};`,
        stdio: ["pipe", "pipe", "pipe"],
      },
    );
  // Fund each submitter for the node's preflight: the plain ADA of one round
  // and a collateral coin, from the availability account, once.
  const submitterFunding = [
    DA_L1_SUBMITTER_MIN_PLAIN_ADA_LOVELACE,
    DEFAULT_L1_SUBMITTER_PREFLIGHT.minCollateralLovelace * 2n,
  ];
  const unfunded: string[] = [];
  for (const { address } of [l1Submitter, availabilitySubmitter])
    if ((await readLucid.utxosAt(address)).length === 0) unfunded.push(address);
  if (unfunded.length > 0) {
    const funder = await newLucid();
    funder.selectWallet.fromSeed(availabilitySeed);
    let fundingTx = funder.newTx();
    for (const address of unfunded)
      for (const lovelace of submitterFunding)
        fundingTx = fundingTx.pay.ToAddress(address, { lovelace });
    const signed = await (await fundingTx.complete()).sign
      .withWallet()
      .complete();
    const txHash = await signed.submit();
    await funder.awaitTx(txHash);
    await awaitActionDepth();
    log(`funded committee submitters ${unfunded.join(", ")}: ${txHash}`);
  }
  await writeJourneyArtifact(
    join(committeeEvidenceDirectory, "committee.json"),
    {
      l1SubmitterAddress: l1Submitter.address,
      availabilitySubmitterAddress: availabilitySubmitter.address,
      database: committeeDatabase,
      apiPort,
      observerSignerIndex: committeeRuntime.observer.signerIndex,
      observerPeerId: committeeRuntime.observer.peerId,
      runtimeManifestSha256: committeeRuntime.outputSha256,
      env: committee.recorded,
    },
  );
  let committeeRecords = 0;
  const committeeNode = createDaBondPoolCommitteeObserver({
    spawn: () =>
      spawnDaBondPoolCommitteeNode({
        argv: [process.execPath, committeeBin],
        env: committee.env,
        cwd: committeeEvidenceDirectory,
        logDirectory: committeeEvidenceDirectory,
        apiUrl: `http://127.0.0.1:${apiPort.toString()}`,
      }),
    checkDaemons,
    submitterUtxos: async () =>
      (
        await Promise.all(
          [l1Submitter.address, availabilitySubmitter.address].map((address) =>
            readLucid.utxosAt(address),
          ),
        )
      )
        .flat()
        .map(outRefOf),
    expectedView: async () =>
      daBondPoolCommitteeExpectedView(
        await port.poolSnapshot(),
        journeyParams.daBond,
      ),
    env: committee.recorded,
    record: async (entry) => {
      committeeRecords += 1;
      await writeJourneyArtifact(
        join(
          committeeEvidenceDirectory,
          `${committeeRecords.toString().padStart(3, "0")}-${entry.kind}.json`,
        ),
        entry,
      );
    },
    syncTimeoutMs: committeeSyncTimeoutMs,
    startTimeoutMs: 180_000,
    pollMs: POLL_MS,
    stopBoundMs: 30_000,
    nodeCadence: committeeCadence,
  });

  // The da-bond CLI (P18).
  let cliChains = 0;
  const cli = createDaBondPoolCli({
    run: spawnDaBondCliProcess({
      cwd: evidenceDirectory,
      timeoutMs: INCLUSION_TIMEOUT_MS,
      inheritedNames: new Set(DA_BOND_POOL_INHERITED_ENV),
    }),
    command: [process.execPath, cliBin],
    manifestPath,
    kupoUrl: context.kupoUrl,
    ogmiosUrl: context.ogmiosUrl,
    env: {
      ...inheritedEnv,
      MIDGARD_CONFIG_MODE: "disabled",
      MIDGARD_DOTENV_MODE: "disabled",
      L1_NODE_SOCKET_PATH: nativeLedgerPaths.socket,
      L1_NODE_CONFIG_PATH: nativeLedgerPaths.config,
      L1_NATIVE_CHAIN_SYNC_BINARY_PATH: nativeLedgerPaths.binary,
    },
    workDirectory: (label) => {
      const directory = join(
        evidenceDirectory,
        "cli",
        `${new Date().toISOString().replaceAll(":", "-")}-${label.replaceAll(" ", "-")}`,
      );
      mkdirSync(directory, { recursive: true });
      return directory;
    },
    // The adapter's own read: the pool output now sits at the transaction.
    confirm: async (txHash) => {
      const deadline = Date.now() + INCLUSION_TIMEOUT_MS;
      for (;;) {
        const holders = await readLucid.utxosAtWithUnit(
          poolValidator.spendingScriptAddress,
          SDK.daBondPoolUnit(poolValidator.policyId),
        );
        if (holders.length === 1 && holders[0]!.txHash === txHash) {
          await awaitActionDepth();
          return true;
        }
        if (Date.now() > deadline) return false;
        await pause(POLL_MS);
      }
    },
    record: async (label, runs) => {
      cliChains += 1;
      await writeJourneyArtifact(
        join(
          evidenceDirectory,
          "cli",
          `${cliChains.toString().padStart(3, "0")}-${label.replaceAll(" ", "-")}.json`,
        ),
        runs,
      );
    },
  });
  mkdirSync(join(evidenceDirectory, "cli"), { recursive: true });

  // The availability command flow, composed from the CLI's steps.
  const availabilityDeployment = await availabilityDeploymentFromManifest(
    challengerLucid,
    manifest,
  );
  const buildContext = {
    daChallengeWindowMs: BigInt(
      manifest.deploymentProfile.timing.da_challenge_window_ms,
    ),
    daAttestationPolicyId: manifest.contracts.daAttestationMint?.scriptHash,
    kupoUrl: context.kupoUrl,
  };
  const source = availabilityCommandCanonicalSource({
    lucid: challengerLucid,
    kupoUrl: context.kupoUrl,
    ogmiosUrl: context.ogmiosUrl,
  });
  const retryTransient = async <T>(
    label: string,
    action: () => Promise<T>,
  ): Promise<T> => {
    for (let attempt = 1; ; attempt += 1) {
      try {
        return await action();
      } catch (error) {
        if (
          !isTransientCanonicalError(error) ||
          attempt >= MAX_TRANSIENT_RETRIES
        )
          throw error;
        log(`${label}: Kupo is catching up with Ogmios; retrying`);
        await pause(POLL_MS);
      }
    }
  };
  // Blocks: commit and attest through the published-block actor. It is built
  // before the journal opens, so no later await can leave the journal open.
  const actor = await createPublishedWatcherBlockActor({
    deployment,
    lucid: deployment.operatorLucid,
    daSignerConfig,
    onStage: (stage) => log(stage),
  });
  let canonicalAnchor = await retryTransient("canonical anchor", () =>
    source.readBoundary(),
  );
  const journal = openAvailabilityOperationJournal(
    join(artifactDirectory, "availability-journal.sqlite"),
  );
  const operationContext: SDK.DaAvailabilityOperationContext = {
    deploymentIdentity: manifest.manifestId,
    actor: challengerKey,
    journal,
    stateQueuePolicyId: availabilityDeployment.contracts.stateQueue.policyId,
    minimumConfirmationDepth: manifest.l1Finality.confirmationDepth,
    transactionLimits: SDK.daAvailabilityOperationLimits(
      challengerLucid,
      availabilityDeployment.parameters,
    ),
    assertActuationCurrent: async () => {
      await source.assertCanonicalAncestor(canonicalAnchor);
    },
    observe: source.observe,
    submit: (cbor) => provider.submitTx(cbor),
  };
  const quietJournal = (): Promise<void> =>
    awaitQuietJournal({
      reconcile: () => SDK.reconcileDaAvailabilityOperations(operationContext),
      timeoutMs: JOURNAL_QUIET_TIMEOUT_MS,
      pollMs: POLL_MS,
      wait: pause,
    });
  const awaitIncluded = (txId: string): Promise<void> =>
    awaitAvailabilityInclusion({
      txId,
      reconcile: () => SDK.reconcileDaAvailabilityOperations(operationContext),
      journalRecord: (id) => journal.findTransaction(id) ?? undefined,
      timeoutMs: INCLUSION_TIMEOUT_MS,
      pollMs: POLL_MS,
      wait: pause,
      log,
    });
  const canonicalSnapshot = (headerHash: string) =>
    retryTransient("availability snapshot", async () => {
      for (let attempt = 1; ; attempt += 1) {
        const before = await source.readBoundary();
        const snapshot = await SDK.fetchDaAvailabilityChallengeSnapshot(
          challengerLucid,
          availabilityDeployment,
          headerHash,
        );
        const after = await source.readBoundary();
        if (before.pointId === after.pointId) return snapshot;
        if (attempt >= MAX_TRANSIENT_RETRIES)
          throw new Error(
            "Availability state kept changing during canonical discovery",
          );
      }
    });

  const payloadFiles = new Map<string, string>();
  /** A ledger tip fresh enough for a sixty-second backdated lower bound. */
  const awaitFreshTip = async (): Promise<void> => {
    await chain.awaitLedgerTime(chain.now() - COMMIT_FRESH_TIP_MS);
  };
  /**
   * Lands one availability action through the journal: reconcile, wait for a
   * fresh ledger tip, snapshot, plan (it must be one of `expected`), build,
   * sign, submit, wait for inclusion, then wait out the action depth. A
   * transaction that lapsed unminted is re-planned from a fresh snapshot
   * (`availabilityAttemptRecovery`); a refused first broadcast of a journaled
   * transaction is waited on, not re-planned (`availabilitySubmissionToAwait`).
   */
  const landAvailability = async (
    headerHash: string,
    requested: AvailabilityRequest,
    expected: readonly SDK.DaAvailabilityTransactionAction[],
    onBuilt?: (
      built: SDK.BuiltDaAvailabilityTransaction,
      snapshot: SDK.DaAvailabilityChallengeSnapshot,
    ) => void,
  ): Promise<
    Readonly<{
      txId: string;
      operation: SDK.DaAvailabilityTransactionAction;
      snapshot: SDK.DaAvailabilityChallengeSnapshot;
    }>
  > => {
    let reconciledOthers = 0;
    let lapses = 0;
    for (let attempt = 1; ; attempt += 1) {
      let built: SDK.BuiltDaAvailabilityTransaction | undefined;
      try {
        canonicalAnchor = await prepareAvailabilityAttempt({
          quietJournal,
          awaitFreshTip,
          readBoundary: () => source.readBoundary(),
        });
        const snapshot = await canonicalSnapshot(headerHash);
        const operation = planAvailabilityCommandAction(
          requested,
          snapshot,
          Date.now(),
        );
        if (!expected.includes(operation))
          throw new Error(
            `Availability ${requested} of ${headerHash} plans ${operation}, expected ${expected.join(" or ")}`,
          );
        const reserved = new Set(journal.reservedOutRefs(challengerKey));
        const coins = selectDaBondPoolChallengerCoins({
          utxos: await challengerLucid.wallet().getUtxos(),
          address: challengerAddress,
          plan,
          reserved,
        });
        const need = (coin: UTxO | undefined, name: string): string => {
          if (coin === undefined)
            throw new Error(
              `The challenger wallet ${challengerAddress} has no ${name} coin for ${operation}`,
            );
          return outRefOf(coin);
        };
        const payloadFile = payloadFiles.get(headerHash);
        const submission = await landAvailabilitySubmission({
          label: `${operation} ${headerHash}`,
          execute: () =>
            SDK.runDaAvailabilityOperation(operationContext, {
              headerHash,
              action: operation,
              completesWorkflow:
                operation === "timeout"
                  ? snapshot.descendant === undefined
                  : undefined,
              build: async () => {
                built = await buildAvailabilityCommandTransaction(
                  challengerLucid,
                  availabilityDeployment,
                  buildContext,
                  snapshot,
                  operation,
                  {
                    headerHash,
                    collateralOutRef: need(coins.collateral, "collateral"),
                    ...(operation === "open"
                      ? { fundingOutRef: need(coins.openFunding, "exact Open") }
                      : operation === "remove" || operation === "prune"
                        ? { fundingOutRef: need(coins.operating, "operating") }
                        : {}),
                    ...(operation === "publish" && payloadFile !== undefined
                      ? { payloadFile }
                      : {}),
                  },
                  challengerKey,
                  reserved,
                );
                onBuilt?.(built, snapshot);
                return built;
              },
            }),
          builtTxId: () =>
            (built as SDK.BuiltDaAvailabilityTransaction | undefined)?.txId,
          journalRecord: (id) => journal.findTransaction(id) ?? undefined,
          awaitIncluded,
          log,
        });
        if (submission.kind === "reconciled") {
          // The executor reconciled an earlier intent (a finalized anchor
          // still short of its confirmation depth) instead of building.
          reconciledOthers += 1;
          if (reconciledOthers > 60)
            throw new Error(
              `Availability ${requested} of ${headerHash} kept waiting on earlier intents: ${JSON.stringify(submission.result)}`,
            );
          await pause(POLL_MS * 5);
          attempt -= 1;
          continue;
        }
        const { txId } = submission;
        await awaitActionDepth();
        return { txId, operation, snapshot };
      } catch (error) {
        const builtTxId = (
          built as SDK.BuiltDaAvailabilityTransaction | undefined
        )?.txId;
        const recovery = availabilityAttemptRecovery(error, {
          journaled:
            builtTxId !== undefined &&
            journal.findTransaction(builtTxId) !== null,
          lapses,
          attempt,
          maxTransientAttempts: MAX_TRANSIENT_RETRIES,
        });
        if (recovery === "throw") throw error;
        if (recovery === "replan") {
          lapses += 1;
          log(
            `availability ${requested}: ${describeErrorChain(error)}; planning again from a fresh snapshot (${lapses.toString()} of ${MAX_LAPSED_REPLANS.toString()})`,
          );
        } else log(`availability ${requested}: ${String(error)}; retrying`);
        await pause(POLL_MS);
      }
    }
  };

  const blocks = new Map<string, JourneyBlock>(
    resumedB2 === undefined ? [] : [[resumedB2.headerHash, resumedB2]],
  );
  let onboarded = false;
  const headerUnit = (headerHash: string) =>
    toUnit(
      contracts.stateQueue.policyId,
      SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + headerHash,
    );
  const confirmedHeaderHash = async (): Promise<string> => {
    const [root] = await sortedQueue();
    if (root === undefined) throw new Error("The state queue has no root");
    return (
      await Effect.runPromise(
        SDK.getConfirmedStateFromStateQueueDatum(root.datum),
      )
    ).data.headerHash;
  };

  const feePayer = paymentCredentialOf(availabilityAddress).hash;
  const withdrawKeys = [
    ...quorum.map((key) => ({ role: key.role, seed: seedByRole[key.role] })),
    ...(quorum.some((key) => key.keyHash === feePayer)
      ? []
      : [{ role: "fee-payer", seed: availabilitySeed }]),
  ].map(({ role, seed }) => {
    if (seed === undefined)
      throw new DaBondJourneySigningMaterialError(
        [`the ${role} seedPhrase in ${accountsSource}`],
        "It witnesses the pool withdrawal steps.",
      );
    return { role, seed };
  });
  const withdraw = async (
    step: "begin" | "cancel" | "complete",
    complete?: Readonly<{ amount: bigint }>,
  ) => {
    const result = await cli.withdraw({
      step,
      feeAddress: availabilityAddress,
      signers: quorum.map((key) => key.keyHash),
      witnesses: withdrawKeys,
      ...(complete === undefined
        ? {}
        : { complete: { amount: complete.amount, to: availabilityAddress } }),
    });
    log(`withdraw ${step}: ${result.txId}`);
    return result;
  };

  const committeeLifecycle = async (
    phase: "before" | "after",
    step: DaBondPoolJourneyStep,
  ): Promise<void> => {
    const action = daBondPoolCommitteeLifecycle(phase, step);
    if (action === "start") {
      const pid = await committeeNode.start();
      log(
        `committee node started ${phase} step ${step.toString()}: pid ${pid.toString()}`,
      );
    } else if (action === "stop") {
      const exit = await committeeNode.stop();
      log(
        `committee node stopped ${phase} step ${step.toString()}: code ${String(exit.exitCode)}`,
      );
    }
  };

  const resume: DaBondPoolJourneyResume | undefined =
    resumedB2 === undefined
      ? undefined
      : {
          afterStep: 5,
          b2: {
            label: resumedB2.label,
            headerHash: resumedB2.headerHash,
            committedAt: Number(resumedB2.header.endTime),
          },
        };

  const port: LiveDaBondPoolJourneyPort = {
    artifactDirectory: evidenceDirectory,
    ...(resume === undefined ? {} : { resume }),
    manifestId: manifest.manifestId,
    networkMagic: customNetwork.networkMagic,
    challengerAddress,
    committeeRunning: () => committeeNode.running(),
    dispose: async () => {
      try {
        const exit = await committeeNode.teardown();
        if (exit !== undefined)
          log(
            `committee node stopped: code ${String(exit.exitCode)}, signal ${String(exit.signal)}${exit.killed ? ", killed" : ""}`,
          );
      } finally {
        journal.close();
      }
    },

    // P27(3): started before steps 1 and 6, stopped before step 2 and at
    // the end of step 6 (`daBondPoolCommitteeLifecycle`); every stop checks
    // exit 0 on SIGTERM, no daemon left and unchanged submitter UTxOs, so a
    // node that dies or spends fails the step it ran in.
    beforeStep: async (step) => {
      checkDaemons(committeeNode.admitted());
      await committeeLifecycle("before", step);
    },
    afterStep: (step) => committeeLifecycle("after", step),
    // A resumed run starts where the earlier run's checked stop before step
    // 2 left it: the node stopped, B2 Attested, the pool Bonded and backing
    // a bond.
    resumeBeforeStep: async (step) => {
      checkDaemons(committeeNode.admitted());
      if (resume === undefined || step !== 2)
        throw new Error(
          `The DA bond pool journey port resumes only before step 2, not step ${step.toString()}`,
        );
      if (committeeNode.running())
        throw new DaBondPoolJourneyResumeMismatchError(
          "the committee node already runs before step 2",
        );
      const status = await port.blockStatus(resume.b2.headerHash);
      if (status !== "Attested")
        throw new DaBondPoolJourneyResumeMismatchError(
          `B2 ${resume.b2.headerHash} is ${status}, not Attested`,
        );
      const pool = await port.poolSnapshot();
      if (pool.state !== "bonded" || pool.backing < journeyParams.daBond)
        throw new DaBondPoolJourneyResumeMismatchError(
          `the pool is ${pool.state} with backing ${pool.backing.toString()}, not Bonded with a bond of ${journeyParams.daBond.toString()}`,
        );
    },

    params: async () => journeyParams,

    now: tipTime,

    poolSnapshot: async (): Promise<DaBondPoolJourneySnapshot> => {
      const holders = await readLucid.utxosAtWithUnit(
        poolValidator.spendingScriptAddress,
        SDK.daBondPoolUnit(poolValidator.policyId),
      );
      if (holders.length === 0)
        return { state: "missing", lovelace: 0n, backing: 0n };
      await refreshBondNow();
      const status = await daBondStatusCommand(bondContext);
      return {
        state: status.state,
        lovelace: BigInt(status.lovelace),
        backing: BigInt(status.backing),
        ...("unlockAt" in status && status.unlockAt !== undefined
          ? { unlockAt: Number(status.unlockAt) }
          : {}),
        utxoRef: status.poolOutRef,
      };
    },

    observeAlerts: async (): Promise<DaBondPoolJourneyAlerts> => {
      const nowMs = await tipTime();
      const poolAddress = poolValidator.spendingScriptAddress;
      const policyId = poolValidator.policyId;
      const watcher = deriveWatcherDaBondPoolObservation({
        pool: authenticWatcherDaBondPool({
          utxos: await readLucid.utxosAt(poolAddress),
          policyId,
          address: poolAddress,
        }),
        policyId,
        parameters,
        nowMs: BigInt(nowMs),
      });
      const committee = await committeeNode.observe();
      return {
        watcher: {
          underBacked: watcher.alerts.underBacked,
          withdrawing: watcher.alerts.withdrawing,
        },
        committee: {
          readinessReasons: committee.readinessReasons,
          events: committee.events,
          process: committee.process,
        },
      };
    },

    commitBlock: async (intent) => {
      if (!onboarded || !(await actor.operatorActive())) {
        // Activation can backdate its lower bound sixty seconds too.
        await awaitFreshTip();
        await actor.onboardOperator();
        onboarded = true;
      }
      type CommitAttempt = Readonly<{
        block: Awaited<ReturnType<typeof depositEventsRetainedBlock>>;
        interval: ReturnType<typeof nextJourneyBlockInterval>;
        txId: string;
      }>;
      // The attempt the actor last signed, to settle it if it expires.
      let signed: (CommitAttempt & Readonly<{ anchor: UTxO }>) | undefined;
      const { block, interval, txId } = await commitWithinLedgerValidity({
        label: `commit ${intent.label}`,
        awaitFreshTip,
        maxAttempts: COMMIT_VALIDITY_ATTEMPTS,
        log,
        settleExpired: async (error) => {
          if (signed === undefined || signed.txId !== error.txHash) throw error;
          const attempt = signed;
          // From its upper bound on the ledger refuses it, so once the tip is
          // there and Kupo agrees, its absence is final.
          await chain.awaitLedgerTime(error.expiryMs);
          for (let read = 1; ; read += 1) {
            const before = await retryTransient("expired commit", () =>
              source.readBoundary(),
            );
            const headers = await readLucid.utxosAtWithUnit(
              contracts.stateQueue.spendingScriptAddress,
              headerUnit(attempt.block.headerHash),
            );
            // Kupo keeps spent matches, so this names the transaction that
            // spent the anchor even after a later Apply re-spent the header.
            const anchorSpend = await fetchKupoSpend({
              kupoUrl: context.kupoUrl,
              outRef: {
                txHash: attempt.anchor.txHash,
                outputIndex: attempt.anchor.outputIndex,
              },
            });
            const after = await retryTransient("expired commit", () =>
              source.readBoundary(),
            );
            const decision = settleExpiredCommitReads({
              txId: attempt.txId,
              stable: before.pointId === after.pointId,
              anchorSpentBy: anchorSpend?.transactionId ?? null,
              headerHolders: headers.map((utxo) => utxo.txHash),
              read,
              maxReads: MAX_TRANSIENT_RETRIES,
            });
            if (decision === "reread") continue;
            if (decision === "adopt")
              return {
                block: attempt.block,
                interval: attempt.interval,
                txId: attempt.txId,
              };
            if (decision === "absent") return undefined;
            // Anything else spent the anchor or holds the header: a conflict,
            // not a lapse.
            log(
              `commit ${intent.label}: expired ${attempt.txId} ${decision}: anchor spent by ${anchorSpend?.transactionId ?? "nothing"}, header held by ${JSON.stringify(headers.map((utxo) => utxo.txHash))}`,
            );
            throw error;
          }
        },
        // The actor pinned the wallet before the commit and never saw it
        // land; drop the pin so the next build reads the live wallet.
        refreshWallet: async () => {
          deployment.operatorLucid.clearUTxOOverride();
        },
        submit: async (attempt) => {
          // The refused attempt spent nothing; drop the pinned wallet view so
          // this build reads the live wallet.
          if (attempt > 1) deployment.operatorLucid.clearUTxOOverride();
          const queue = await sortedQueue();
          const root = queue[0];
          const tail = queue.at(-1);
          if (root === undefined || tail === undefined)
            throw new Error("The state queue has no root");
          let predecessor: {
            headerHash: string;
            utxosRoot: string;
            endTime: bigint;
          };
          if (queue.length === 1) {
            const genesis = (
              await Effect.runPromise(
                SDK.getConfirmedStateFromStateQueueDatum(root.datum),
              )
            ).data;
            // Genesis closes a real interval; the first header must end after it.
            await chain.awaitLedgerTime(Number(genesis.endTime) + 1);
            predecessor = {
              headerHash: genesis.headerHash,
              utxosRoot: genesis.utxoRoot,
              endTime: genesis.endTime,
            };
          } else {
            const key = tail.datum.key;
            if (key === "Empty") throw new Error("The queue tail has no key");
            const header = await Effect.runPromise(
              SDK.getHeaderFromStateQueueDatum(tail.datum),
            );
            predecessor = {
              headerHash: key.Key.key,
              utxosRoot: header.utxosRoot,
              endTime: header.endTime,
            };
          }
          const interval = nextJourneyBlockInterval({
            predecessorEndTime: predecessor.endTime,
            nowMs: chain.now(),
          });
          const block = await depositEventsRetainedBlock({
            operatorVkey: actor.operatorVkey,
            startTime: interval.startTime,
            endTime: interval.endTime,
            blockSlot: BigInt(
              deployment.operatorLucid.unixTimeToSlot(Number(interval.endTime)),
            ),
            prevHeaderHash: predecessor.headerHash,
            prevUtxosRoot: predecessor.utxosRoot,
            priorLedger: [],
            events: [],
          });
          const txId = await actor.commit(
            block,
            tail.utxo,
            queue.length > 1 ? queue[1]!.utxo : undefined,
            async ({ txHash }) => {
              signed = { block, interval, txId: txHash, anchor: tail.utxo };
            },
          );
          return { block, interval, txId };
        },
      });
      const committed: JourneyBlock = {
        label: intent.label,
        header: block.header,
        headerHash: block.headerHash,
        payloadEnvelopeCbor: Buffer.from(block.payloadEnvelopeCbor),
      };
      blocks.set(block.headerHash, committed);
      await writeJourneyArtifact(
        join(evidenceDirectory, `block-${intent.label}.json`),
        { ...committed, responder: intent.responder, commitTxId: txId },
      );
      log(`commit ${intent.label} ${block.headerHash}: ${txId}`);
      await awaitActionDepth();
      return {
        headerHash: block.headerHash,
        txId,
        headerEndTime: Number(interval.endTime),
      };
    },

    attest: async (headerHash) => {
      const block = blocks.get(headerHash);
      if (block === undefined)
        throw new Error(`Block ${headerHash} was not committed by this port`);
      const outcome = await attestWithinLedgerValidity({
        label: `attest ${block.label}`,
        attest: () => actor.attest(block),
        refusal: (error) => {
          const refused = attestRefusalResult(error);
          if (refused !== undefined)
            log(`Apply ${block.label} refused: ${JSON.stringify(refused)}`);
          return refused;
        },
        awaitFreshTip,
        // A refused transaction spent nothing; drop the pinned wallet view
        // so the next build reads the live wallet.
        refreshWallet: async () => {
          deployment.operatorLucid.clearUTxOOverride();
        },
        maxAttempts: COMMIT_VALIDITY_ATTEMPTS,
        log,
      });
      if (outcome.kind === "refused") return outcome;
      if (outcome.kind !== "attested")
        throw new Error(
          `Block ${block.label} was corrected before its Apply landed`,
        );
      const appliedAt = await tipTime();
      await awaitActionDepth();
      return { kind: "applied", txId: outcome.txHash, appliedAt };
    },

    open: async (headerHash) => {
      checkDaemons(committeeNode.admitted());
      const { txId } = await landAvailability(headerHash, "open", ["open"]);
      const snapshot = await canonicalSnapshot(headerHash);
      const record = snapshot.recordDatum;
      if (record === undefined)
        throw new Error(`Open ${txId} left no challenge record`);
      return { txId, responseDeadline: Number(record.response_deadline) };
    },

    respondAll: async (headerHash) => {
      const block = blocks.get(headerHash);
      if (block === undefined)
        throw new Error(`Block ${headerHash} was not committed by this port`);
      const payloadFile = join(evidenceDirectory, `payload-${headerHash}.cbor`);
      await writeJourneyFile(payloadFile, block.payloadEnvelopeCbor);
      payloadFiles.set(headerHash, payloadFile);
      const txIds: string[] = [];
      for (;;) {
        const snapshot = await canonicalSnapshot(headerHash);
        if (!snapshot.tranches.some(({ datum }) => "Active" in datum))
          return { txIds };
        if (txIds.length >= MAX_RESPONSE_TRANSACTIONS)
          throw new Error(
            `Responding to ${headerHash} took more than ${MAX_RESPONSE_TRANSACTIONS.toString()} publications`,
          );
        txIds.push(
          (await landAvailability(headerHash, "respond", ["publish"])).txId,
        );
      }
    },

    settle: async (headerHash) => {
      const txIds: string[] = [];
      for (;;) {
        const snapshot = await canonicalSnapshot(headerHash);
        const record = snapshot.recordDatum;
        const terminal = snapshot.terminalDatum;
        if (
          record === undefined ||
          terminal === undefined ||
          terminal.next_tranche_index >=
            BigInt(record.commitment.tranche_descriptors.length)
        )
          return { txIds };
        if (txIds.length >= MAX_SETTLEMENTS)
          throw new Error(
            `Settling ${headerHash} took more than ${MAX_SETTLEMENTS.toString()} settlements`,
          );
        txIds.push(
          (await landAvailability(headerHash, "settle", ["settle"])).txId,
        );
      }
    },

    close: async (headerHash) => ({
      txId: (await landAvailability(headerHash, "close", ["close"])).txId,
    }),

    awaitTime: async (posixMs) => {
      const nowMs = await tipTime();
      if (nowMs >= posixMs) return;
      const enclosing = readLucid.unixTimeToSlot(posixMs);
      const targetSlot =
        readLucid.slotToUnixTime(enclosing) < posixMs
          ? enclosing + 1
          : enclosing;
      log(
        `waiting ${Math.ceil((posixMs - nowMs) / 1000).toString()} s for the tip to reach ${new Date(posixMs).toISOString()}`,
      );
      await awaitLedgerTipSlot({
        targetSlot,
        readTipSlot: () => readOgmiosTipSlot(context.ogmiosUrl),
        timeoutMs: awaitTimeBudgetMs(posixMs, nowMs),
        pollMs: 1_000,
      });
    },

    timeout: async (headerHash) => {
      const { txId, snapshot } = await landAvailability(
        headerHash,
        "timeout",
        ["timeout"],
        (built, planned) => {
          const pool = planned.pool;
          if (pool === undefined)
            throw new Error("Timeout snapshot holds no DA bond pool");
          const expectedFeePart = SDK.planDaBondPoolSlash({
            poolLovelace: pool.assets.lovelace ?? 0n,
            parameters,
          }).feePart;
          if (built.timeoutFeePartLovelace !== expectedFeePart)
            throw new Error(
              `Timeout fee part ${String(built.timeoutFeePartLovelace)} differs from the pool slash plan ${expectedFeePart.toString()}`,
            );
        },
      );
      const record = journal.findTransaction(txId);
      if (record === null)
        throw new Error(`Timeout ${txId} is missing from the journal`);
      const pool = snapshot.pool;
      if (pool === undefined || typeof pool.datum !== "string")
        throw new Error("Timeout snapshot holds no DA bond pool datum");
      const landed = decodeJourneyTransaction(record.intent.signedCbor);
      const summary = summarizeDaBondPoolTimeout({
        txId,
        fee: landed.fee,
        inputs: landed.inputs,
        outputs: landed.outputs,
        pool: {
          outRef: outRefOf(pool),
          address: poolValidator.spendingScriptAddress,
          unit: SDK.daBondPoolUnit(poolValidator.policyId),
          lovelace: pool.assets.lovelace ?? 0n,
          datum: pool.datum,
        },
        challengerAddress,
        ...(snapshot.terminalDatum === undefined
          ? {}
          : {
              challengerRemainingLovelace:
                snapshot.terminalDatum.remaining_challenger_lovelace,
            }),
      });
      await writeJourneyArtifact(
        join(evidenceDirectory, `timeout-${headerHash}.json`),
        summary,
      );
      return summary;
    },

    removeOrPrune: async (headerHash) => {
      const txIds: string[] = [];
      for (;;) {
        const present = await readLucid.utxosAtWithUnit(
          contracts.stateQueue.spendingScriptAddress,
          headerUnit(headerHash),
        );
        if (present.length === 0) return { txIds };
        if (txIds.length >= MAX_REMOVAL_STEPS)
          throw new Error(
            `Removing ${headerHash} took more than ${MAX_REMOVAL_STEPS.toString()} steps`,
          );
        txIds.push(
          (await landAvailability(headerHash, "timeout", ["remove", "prune"]))
            .txId,
        );
      }
    },

    topUp: async (amount) => {
      const result = await cli.topUp({ amount, walletSeed: availabilitySeed });
      log(`top-up ${amount.toString()}: ${result.txId}`);
      return { txId: result.txId, cli: result.cli };
    },

    beginWithdraw: async () => {
      const { txId, cli: evidence, output } = await withdraw("begin");
      const status = output.status as { unlockAt?: unknown } | undefined;
      const unlockAt = Number(status?.unlockAt);
      if (!Number.isSafeInteger(unlockAt))
        throw new DaBondCliProcessError(
          `da-bond assemble for withdraw begin ${txId} printed no status.unlockAt`,
          [evidence.submit],
        );
      return { txId, unlockAt, cli: evidence };
    },

    cancelWithdraw: async () => {
      const { txId, cli: evidence } = await withdraw("cancel");
      return { txId, cli: evidence };
    },

    completeWithdraw: async (amount) => {
      const { txId, cli: evidence } = await withdraw("complete", { amount });
      return { txId, cli: evidence };
    },

    blockStatus: async (headerHash) => {
      const outputs = await readLucid.utxosAtWithUnit(
        contracts.stateQueue.spendingScriptAddress,
        headerUnit(headerHash),
      );
      if (outputs.length > 1)
        throw new Error(
          `Header ${headerHash} is held by several queue outputs`,
        );
      const output = outputs[0];
      if (output === undefined)
        return absentBlockStatus(headerHash, await confirmedHeaderHash());
      const view = await Effect.runPromise(
        SDK.getLinkedListNodeViewFromUTxO(output),
      );
      const node = await Effect.runPromise(
        SDK.getStateQueueNodeFromStateQueueDatum(view),
      );
      return SDK.daAvailabilityStateQueueStatusKind(node.da_attestation);
    },
  };
  return port;
};
