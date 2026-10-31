import { createHash } from "node:crypto";
import { mkdir, readFile } from "node:fs/promises";
import { basename, join, resolve } from "node:path";

import {
  DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE,
  type DeploymentManifest,
  verifyFinalizedDeploymentManifest,
} from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  type AllowList,
  requireAllowListed,
} from "@al-ft/midgard-core/deployment-profile";
import { NativeLedgerKupmios } from "@al-ft/midgard-l1-follower/native-ledger-kupmios";
import {
  nativeLedgerAuthoritySource,
  readNativeLedgerGenesis,
} from "@al-ft/midgard-l1-follower/native-reward-account";
import {
  type Network,
  type OutRef,
  paymentCredentialOf,
  SLOT_CONFIG_NETWORK,
  type UTxO,
  validatorToScriptHash,
  walletFromSeed,
} from "@lucid-evolution/lucid";
import { createScalusEvaluator } from "@lucid-evolution/scalus-uplc";
import { midgardContractsFromDeploymentManifest } from "midgard-node/services/midgard-contracts.midgard-contracts-from-deployment-manifest";

import { l1KupmiosEnvironment } from "../../src/environment.js";
import { loadStackConfig } from "../../src/full-stack/config.js";
import { stackPaths } from "../../src/full-stack/deployment.js";
import {
  hostL1Node,
  nativeLedgerPaths,
} from "../../src/full-stack/native-ledger.js";
import { StackProcesses } from "../../src/full-stack/process.js";
import {
  releasePaths,
  verifyStackRelease,
} from "../../src/full-stack/release.js";
import { readRuntimeConfiguration } from "../../src/full-stack/runtime.js";
import { deriveStackWallets } from "../../src/full-stack/wallets.js";
import { journeyLucid } from "./journey-lucid.js";
import {
  assertJourneyNetwork,
  type JourneyNetwork,
  journeyNetwork,
} from "./journey-network.js";
import { journeyWorkflowChain } from "./live-context.js";
import {
  readStackDaProducerManifest,
  stackDaProducerManifestPath,
} from "./stack-da-publication.js";
import { readStackShelleyGenesisText } from "./stack-genesis.js";

/**
 * The stack's node `.env` names of the drill's own keys. Their values are
 * secrets: they are read only to select a wallet and never logged.
 * - `operator`: operator-2's key; the drill signs as operator-2 only while
 *   operator-2's node is stopped.
 * - `ledgerOwner`: owns the L2 inputs the fault transactions spend.
 * - `funding`: pays the drill's L1 fees and operator-2's re-onboarding bonds.
 */
export const STACK_JOURNEY_SEED_ENV = Object.freeze({
  operator: "STACK_OPERATOR2_SEED",
  ledgerOwner: "STACK_DRILL_LEDGER_OWNER_SEED",
  funding: "STACK_DRILL_FUNDING_SEED",
} as const);

export type StackJourneyRole = keyof typeof STACK_JOURNEY_SEED_ENV;

/**
 * The stack node `.env` name of operator-2's DA producer libp2p key source:
 * the producer identity the committee admits for operator-2's node, which the
 * drill publishes its payloads as. Its value is a secret.
 */
export const STACK_JOURNEY_DA_PRODUCER_KEY_ENV = "STACK_OPERATOR2_DA_TRANSPORT";

const RUN_ID = /^[A-Za-z0-9][A-Za-z0-9._-]{0,63}$/u;

const keyHashOf = (address: string): string => {
  const credential = paymentCredentialOf(address);
  if (credential.type !== "Key")
    throw new Error("Stack journey wallet is not key-controlled");
  return credential.hash;
};

const requiredSeed = (
  env: Readonly<Record<string, string>>,
  name: string,
): string => {
  const value = env[name];
  if (value === undefined || value.trim() === "")
    throw new Error(`Set ${name} in the stack node environment`);
  return value;
};

/**
 * The wallet of the seed phrase `env[name]` holds. A malformed phrase is
 * refused by its variable's name alone, never by the parser's error.
 */
const requiredSeedWallet = (
  env: Readonly<Record<string, string>>,
  name: string,
  network: Network,
): Readonly<{ seedPhrase: string; address: string }> => {
  const seedPhrase = requiredSeed(env, name);
  let address: string;
  try {
    address = walletFromSeed(seedPhrase, { network }).address;
  } catch {
    throw new Error(`${name} is not a valid seed phrase`);
  }
  return { seedPhrase, address };
};

/**
 * The Compose project the stack's services run in: the `COMPOSE_PROJECT_NAME`
 * the operator Compose wrapper or the env file sets, else Compose's own
 * default, the node directory's name normalized.
 */
export const stackComposeProject = (
  nodeRoot: string,
  env: Readonly<Record<string, string | undefined>>,
): string => {
  const given = env.COMPOSE_PROJECT_NAME;
  const project =
    given !== undefined && given !== ""
      ? given
      : basename(resolve(nodeRoot))
          .toLowerCase()
          .replace(/[^a-z0-9_-]/gu, "")
          .replace(/^[_-]+/u, "");
  if (!/^[a-z0-9][a-z0-9_-]*$/u.test(project))
    throw new Error(
      `Stack Compose project ${JSON.stringify(project)} is not a Compose project name`,
    );
  return project;
};

/**
 * Where a stack-mode journey finds everything it reads, from the stack's own
 * configuration and environment: the files the e2e-stack writes, its L1
 * endpoints and local node, the watcher's Compose project, process
 * configuration and prover address, and the drill's
 * wallets (addresses and key hashes only, never their seeds). The journey's
 * own directory is `<runDirectory>/fault-journeys/<runId>`, off `/tmp`.
 */
export const resolveStackJourneyLayout = (
  processes: StackProcesses,
  runId: string,
) => {
  if (!RUN_ID.test(runId))
    throw new Error(
      "Stack journey run id must be a short name of letters, digits, '.', '_' or '-'",
    );
  const { config, env } = processes;
  const network = journeyNetwork().network;
  const stackWallets = deriveStackWallets(config, env);
  const proverAddress = stackWallets.prover;
  if (proverAddress === undefined)
    throw new Error("The stack configures no watcher prover wallet");
  const honestOperator = stackWallets.operator;
  if (honestOperator === undefined)
    throw new Error("The stack configures no operator wallet");
  const wallets = Object.fromEntries(
    (Object.keys(STACK_JOURNEY_SEED_ENV) as StackJourneyRole[]).map((role) => {
      const seedEnv = STACK_JOURNEY_SEED_ENV[role];
      const { address } = requiredSeedWallet(env, seedEnv, network);
      return [role, { seedEnv, address, keyHash: keyHashOf(address) }];
    }),
  ) as Record<
    StackJourneyRole,
    Readonly<{ seedEnv: string; address: string; keyHash: string }>
  >;
  const stackKeys = new Map(
    Object.entries(stackWallets).map(([role, address]) => [
      keyHashOf(address),
      role,
    ]),
  );
  const drillKeys = new Set<string>();
  for (const [role, wallet] of Object.entries(wallets)) {
    const shared = stackKeys.get(wallet.keyHash);
    if (shared !== undefined || drillKeys.has(wallet.keyHash))
      throw new Error(
        `Drill ${role} key (${wallet.seedEnv}) is shared with ${shared === undefined ? "another drill wallet" : `the stack's ${shared} wallet`}`,
      );
    drillKeys.add(wallet.keyHash);
  }
  const node = hostL1Node(processes);
  const { kupoUrl, ogmiosUrl } = l1KupmiosEnvironment(env);
  return Object.freeze({
    runId,
    stackRunDirectory: config.runDirectory,
    directory: join(config.runDirectory, "fault-journeys", runId),
    manifestPath: stackPaths(processes).manifest,
    kupoUrl,
    ogmiosUrl,
    node: Object.freeze({
      socketPath: node.socketPath,
      binaryPath: node.binaryPath,
      nodeConfigPath: join(
        nativeLedgerPaths(processes).directory,
        "config.json",
      ),
    }),
    watcher: Object.freeze({
      composeProject: stackComposeProject(config.nodeRoot, env),
      processConfigPath: join(
        config.watcher.configDirectory,
        "watcher-process.json",
      ),
    }),
    proverAddress,
    honestOperatorKeyHash: keyHashOf(honestOperator),
    wallets: Object.freeze(wallets),
  });
};

export type StackJourneyLayout = ReturnType<typeof resolveStackJourneyLayout>;

/**
 * Refuses a deployment whose operator allow-list leaves out either operator
 * the drill needs: our node's (the honest successor) and operator-2's (the
 * drill's faulty producer). They must be two distinct keys.
 */
export const assertStackJourneyOperators = (
  operators: AllowList,
  keyHashes: Readonly<{ honest: string; drill: string }>,
): void => {
  if (keyHashes.honest === keyHashes.drill)
    throw new Error(
      "The drill operator key is our node's own operator key; use operator-2's",
    );
  requireAllowListed("operators", keyHashes.honest, operators);
  requireAllowListed("operators", keyHashes.drill, operators);
};

/**
 * The stack's deployment as the journey trusts it: the finalized manifest of
 * the compiled profile, on the node's network, with the release's blueprint
 * that manifest commits to.
 */
export const readStackJourneyDeployment = async (
  input: Readonly<{
    manifestPath: string;
    blueprintPath: string;
    handshakeMagic: number;
  }>,
) => {
  const manifest = JSON.parse(
    await readFile(input.manifestPath, "utf8"),
  ) as DeploymentManifest;
  const verified = verifyFinalizedDeploymentManifest(manifest);
  assertJourneyNetwork(journeyNetwork(), {
    manifestNetwork: verified.network,
    manifestProfileDigest: verified.deploymentProfileDigest,
    handshakeMagic: input.handshakeMagic,
  });
  const blueprintJson = await readFile(input.blueprintPath, "utf8");
  if (
    createHash("sha256").update(blueprintJson).digest("hex") !==
    manifest.artifacts.blueprintHash
  )
    throw new Error("Stack release blueprint differs from its manifest");
  return { manifest, blueprintJson };
};

const parseOutRef = (role: string, label: string): OutRef => {
  const match = /^([0-9a-f]{64})#(0|[1-9][0-9]*)$/u.exec(label);
  if (match === null)
    throw new Error(`Deployment reference ${role} has an invalid out-ref`);
  return { txHash: match[1]!, outputIndex: Number(match[2]) };
};

/**
 * The manifest's confirmed reference scripts, keyed by contract, each read
 * from L1 at its recorded out-ref and checked to carry the recorded script
 * and its role's authentication token.
 */
export const resolveStackJourneyReferences = async (
  manifest: Pick<DeploymentManifest, "referenceScripts">,
  contractByRole: Readonly<Record<string, string>>,
  utxosByOutRef: (outRefs: OutRef[]) => Promise<UTxO[]>,
): Promise<Map<string, UTxO>> => {
  const entries = Object.entries(manifest.referenceScripts);
  const outRefs = entries.map(([role, reference]) =>
    parseOutRef(role, reference.outRef),
  );
  const found = await utxosByOutRef(outRefs);
  const references = new Map<string, UTxO>();
  entries.forEach(([role, reference], index) => {
    const outRef = outRefs[index]!;
    const contract = contractByRole[role];
    if (contract === undefined)
      throw new Error(`Deployment reference role ${role} names no contract`);
    const matches = found.filter(
      (utxo) =>
        utxo.txHash === outRef.txHash &&
        utxo.outputIndex === outRef.outputIndex,
    );
    const utxo = matches[0];
    if (
      matches.length !== 1 ||
      utxo?.scriptRef == null ||
      validatorToScriptHash(utxo.scriptRef) !== reference.scriptHash ||
      utxo.assets[reference.roleUnit] !== 1n
    )
      throw new Error(
        `Missing or unauthenticated deployment reference ${role}`,
      );
    references.set(contract, utxo);
  });
  return references;
};

/** The slot clock of the stack's network: Preprod's, or a devnet's genesis. */
const stackSlotConfig = async (
  network: JourneyNetwork["network"],
  nodeConfigPath: string,
) => {
  if (network !== "Custom") return { ...SLOT_CONFIG_NETWORK[network] };
  const genesis = JSON.parse(await readStackShelleyGenesisText(nodeConfigPath));
  return {
    zeroTime: Date.parse(genesis.systemStart),
    zeroSlot: 0,
    slotLength: genesis.slotLength * 1000,
  };
};

/**
 * Opens a stack-mode journey over a running e2e-stack: its configuration and
 * environment, its signed watcher release (the deployment authority in this
 * mode; the journey mints none), its manifest and L1. Every role but the
 * adversary is the stack's own service; the drill signs as operator-2.
 */
export const openStackJourneyContext = async (
  configPath: string,
  runId: string,
) => {
  const { config, env } = await loadStackConfig(configPath);
  const processes = new StackProcesses(config, env);
  const layout = resolveStackJourneyLayout(processes, runId);
  const release = await verifyStackRelease(processes);
  const { operationsEndpoint } = await readRuntimeConfiguration(processes);
  const network = journeyNetwork();
  const genesis = await readNativeLedgerGenesis({
    network: network.network,
    nodeConfigPath: layout.node.nodeConfigPath,
  });
  const { blueprint: blueprintPath } = await releasePaths(processes);
  const { manifest, blueprintJson } = await readStackJourneyDeployment({
    manifestPath: layout.manifestPath,
    blueprintPath,
    handshakeMagic: genesis.networkMagic,
  });
  assertStackJourneyOperators(
    manifest.deploymentProfile.allow_lists.operators,
    {
      honest: layout.honestOperatorKeyHash,
      drill: layout.wallets.operator.keyHash,
    },
  );
  const slotConfig = await stackSlotConfig(
    network.network,
    layout.node.nodeConfigPath,
  );
  const provider = new NativeLedgerKupmios(
    layout.kupoUrl,
    layout.ogmiosUrl,
    nativeLedgerAuthoritySource({
      authorityNodeId: "watcher-journey-node",
      binaryPath: layout.node.binaryPath,
      network: network.network,
      nodeConfigPath: layout.node.nodeConfigPath,
      socketPath: layout.node.socketPath,
      timeoutMs: 10_000,
    }),
    // Slow block inclusion must not abort an otherwise healthy shared session.
    { awaitTxTimeoutMs: 10 * 60_000 },
  );
  const references = await resolveStackJourneyReferences(
    manifest,
    DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE,
    (outRefs) => provider.getUtxosByOutRef(outRefs),
  );
  const contracts = midgardContractsFromDeploymentManifest(
    network.network,
    manifest,
    layout.manifestPath,
  );
  const lucidNetwork = {
    kupoUrl: layout.kupoUrl,
    ogmiosUrl: layout.ogmiosUrl,
    customNetwork: { slotConfig },
  };
  const accounts = Object.fromEntries(
    (Object.keys(STACK_JOURNEY_SEED_ENV) as StackJourneyRole[]).map((role) => [
      role,
      {
        seedPhrase: requiredSeedWallet(
          env,
          layout.wallets[role].seedEnv,
          network.network,
        ).seedPhrase,
        address: layout.wallets[role].address,
      },
    ]),
  ) as Record<StackJourneyRole, { seedPhrase: string; address: string }>;
  const operatorLucid = await journeyLucid(
    provider,
    lucidNetwork,
    createScalusEvaluator(),
  );
  const publisherLucid = await journeyLucid(
    provider,
    lucidNetwork,
    createScalusEvaluator(),
  );
  operatorLucid.selectWallet.fromSeed(accounts.operator.seedPhrase);
  publisherLucid.selectWallet.fromSeed(accounts.funding.seedPhrase);
  const daProducerManifest = await readStackDaProducerManifest({
    manifestPath: stackDaProducerManifestPath(config.runDirectory),
    operatorKey: {
      name: STACK_JOURNEY_DA_PRODUCER_KEY_ENV,
      source: requiredSeed(env, STACK_JOURNEY_DA_PRODUCER_KEY_ENV),
    },
    honestProducerKey: {
      name: config.da.producerTransportEnv,
      source: requiredSeed(env, config.da.producerTransportEnv),
    },
  });
  await mkdir(layout.directory, { recursive: true, mode: 0o700 });
  return {
    mode: "stack" as const,
    /** The journey's own directory in the stack run, as a devnet run directory. */
    runDirectory: layout.directory,
    layout,
    stackConfig: config,
    release,
    /** The stack's own watcher, which the journey attaches to and never runs. */
    watcher: Object.freeze({ ...layout.watcher, operationsEndpoint }),
    customNetwork: { networkMagic: genesis.networkMagic, slotConfig },
    genesisIdentitySha256: genesis.genesisIdentitySha256,
    kupoUrl: layout.kupoUrl,
    ogmiosUrl: layout.ogmiosUrl,
    provider,
    accounts,
    /** Operator-2's DA producer publication to the stack's committee. */
    daProducer: Object.freeze({ manifest: daProducerManifest }),
    deployment: {
      manifest,
      contracts,
      references,
      blueprintJson,
      blueprintPath,
      operatorLucid,
      publisherLucid,
      chain: journeyWorkflowChain({
        lucid: operatorLucid,
        slotLength: slotConfig.slotLength,
        ogmiosUrl: layout.ogmiosUrl,
      }),
    },
  };
};

export type StackJourneyContext = Awaited<
  ReturnType<typeof openStackJourneyContext>
>;
