import { createHash, randomBytes } from "node:crypto";
import { existsSync, mkdirSync, readFileSync } from "node:fs";
import { join } from "node:path";

import { authorityReadinessProbe } from "./authority-readiness.js";
import { LOCAL_AUTHORITY_ID } from "./da.js";
import type { DeployContext } from "./deploy.js";
import { writeDurableFile, writeOnceFile } from "./durable.js";
import type { HistoryChildRole } from "./history-child-evidence.js";
import { historyRecordedBinding } from "./history-recorded-binding.js";
import type { HistoryReadinessSpecification } from "./history-role-context.js";
import { type Layout, type RunEnv, servicePorts } from "./layout.js";
import type { HubOracleOneShot } from "./node-env.js";
import type { ServiceSpec } from "./supervisor.js";
import {
  finishWatcherAuthorityProvisioning,
  FRESH_AUTHORITY_PROFILE,
  prepareWatcherAuthorityProvisioning,
} from "./watcher-authority-provisioning.js";
import {
  ensureHistoryProviders,
  HISTORY_ROLES,
  historyTransportEnvironment,
} from "./watcher-history.js";
import {
  ensureWatcherReleaseBundle,
  loadWatcherModule,
  readFinalizedManifest,
  releasePaths,
} from "./watcher-release.js";

type SecretSource = { readonly kind: "file"; readonly path: string };

const SECRETS = {
  record: "trusted-head-record.key",
  rollback: "rollback-authority.key",
  bearer: "http-bearer.key",
  prover: "prover.seed",
  availability: "availability.seed",
} as const;

const hex32 = () => randomBytes(32).toString("hex");

/**
 * The watcher's five secrets, one regular 0600 file each, no trailing
 * newline, written once. The wallet seeds are the run's own funded
 * `watcherProver` / `watcherAvailability` identities; the three keys are
 * random and belong to this run only.
 */
const ensureSecrets = (context: DeployContext, allowMissing: boolean) => {
  const { layout, identities } = context;
  const random = (name: string): SecretSource => {
    const path = layout.watcherSecret(name);
    if (!existsSync(path)) {
      if (!allowMissing)
        throw new Error(
          "established watcher secret is missing; it is never regenerated",
        );
      writeDurableFile(path, hex32(), 0o600);
    }
    return { kind: "file", path };
  };
  const fixed = (name: string, value: string): SecretSource => {
    const path = layout.watcherSecret(name);
    if (!existsSync(path) && !allowMissing)
      throw new Error(
        "established watcher secret is missing; it is never regenerated",
      );
    writeOnceFile(path, value, 0o600);
    return { kind: "file", path };
  };
  const secrets = {
    record: random(SECRETS.record),
    rollback: random(SECRETS.rollback),
    bearer: random(SECRETS.bearer),
    prover: fixed(SECRETS.prover, identities.seeds.watcherProver),
    availability: fixed(
      SECRETS.availability,
      identities.seeds.watcherAvailability,
    ),
  };
  const values = Object.values(secrets).map((source) =>
    readFileSync(source.path, "utf8"),
  );
  if (new Set(values).size !== values.length)
    throw new Error("the watcher's secrets are not pairwise distinct");
  return secrets;
};

const customNetwork = (layout: Layout) => {
  const genesis = JSON.parse(readFileSync(layout.shelleyGenesis, "utf8")) as {
    networkMagic: number;
    systemStart: string;
    slotLength: number;
  };
  const slotLength = Math.round(genesis.slotLength * 1000);
  if (
    Math.abs(slotLength - genesis.slotLength * 1000) > 1e-6 ||
    slotLength <= 0
  )
    throw new Error(
      "the shelley genesis slot length is not a whole number of milliseconds",
    );
  return {
    networkMagic: genesis.networkMagic,
    slotConfig: {
      zeroTime: Date.parse(genesis.systemStart),
      zeroSlot: 0,
      slotLength,
    },
  };
};

/** The public retained-DA reader, as the committee's runtime manifest announces it. */
const retainedDaPeer = (layout: Layout) => {
  const manifest = JSON.parse(
    readFileSync(layout.committeeManifest(0), "utf8"),
  ) as {
    public_retained_da?: { announce_multiaddrs?: readonly string[] };
  };
  const multiaddr = manifest.public_retained_da?.announce_multiaddrs?.[0];
  if (multiaddr === undefined)
    throw new Error(
      `${layout.committeeManifest(0)} announces no public retained-DA address`,
    );
  return { identity: "devnet-public-retained-da", multiaddr };
};

const endpoints = (run: RunEnv) => {
  const ports = servicePorts(run);
  return {
    authority: `http://127.0.0.1:${ports.watcherAuthority}`,
    operations: `http://127.0.0.1:${ports.watcherOperations}`,
  };
};

/** The watcher's runtime configuration, exactly as its process config embeds it. */
const watcherConfigInput = (
  context: DeployContext,
  schemaVersion: string,
  confirmationDepth: number,
  secrets: ReturnType<typeof ensureSecrets>,
) => {
  const { layout, run } = context;
  return {
    schemaVersion,
    mode: "acceptance",
    targetNetwork: "Custom",
    customNetwork: customNetwork(layout),
    l1: {
      source: {
        sourceMode: "local_node",
        authorityNodeId: LOCAL_AUTHORITY_ID,
        chainSync: {
          kind: "cardano_node_socket",
          socketPath: layout.cardanoSocket,
          nodeConfigPath: layout.hostCardanoConfig,
          genesisConfigPath: layout.shelleyGenesis,
          genesisIdentitySha256: createHash("sha256")
            .update(readFileSync(layout.shelleyGenesis))
            .digest("hex"),
        },
        queryServices: [
          {
            kind: "ogmios",
            identity: "devnet-ogmios",
            endpoint: `http://127.0.0.1:${run.ogmiosPort}`,
          },
          {
            kind: "kupo",
            identity: "devnet-kupo",
            endpoint: `http://127.0.0.1:${run.kupoPort}`,
          },
        ],
      },
      requestTimeoutMs: 30_000,
      maxConcurrency: 8,
      finality: {
        depth: confirmationDepth,
        rollback: {
          beforeFinality: "rewind",
          afterFinality: "quarantine",
          maxDepth: confirmationDepth,
        },
      },
    },
    da: {
      peers: [retainedDaPeer(layout)],
      requestTimeoutMs: 30_000,
      maxConcurrency: 8,
    },
    storage: {
      driver: "sqlite",
      path: join(layout.watcherData, "watcher.sqlite"),
      rollbackAuthorityKeySource: secrets.rollback,
    },
    proverWallet: { keySource: secrets.prover },
    deadlines: {
      daFetchMs: 60_000,
      daPublishMs: 60_000,
      proofConstructMs: 300_000,
      proofSubmitMs: 120_000,
    },
  };
};

const configText = (value: unknown) => `${JSON.stringify(value, null, 2)}\n`;

/**
 * Everything the watcher, its trusted-head authority and its history
 * providers read, generated once under the run's stack directory: the
 * trust-root key, the secrets, the signed release bundle, the providers'
 * identities, and the runtime, authority and process configurations. Every
 * configuration is checked by the watcher's own parser before it is written.
 * A second call verifies and changes nothing; a release signed for another
 * deployment, or a configuration that would now come out differently, is
 * refused rather than replaced.
 */
export const ensureWatcherRelease = async (
  context: DeployContext,
  oneShot: HubOracleOneShot,
  initializeAuthority = false,
): Promise<void> => {
  const { layout, run, artifacts } = context;
  const manifest = readFinalizedManifest(layout);
  if (
    manifest.hubOracleOneShot.outRef !==
    `${oneShot.txHash}#${oneShot.outputIndex}`
  )
    throw new Error(
      `${layout.contractManifest} belongs to another hub-oracle nonce`,
    );
  const watcher = await loadWatcherModule(layout);
  const identity = await ensureWatcherReleaseBundle(layout, watcher, manifest);
  const releaseFinality = await watcher
    .watcherDeploymentReleaseFinalityAuthority(identity)
    .verifyForWorkflow({ deploymentFingerprint: manifest.manifestId });
  const history = ensureHistoryProviders(
    layout,
    run,
    releaseFinality,
    manifest.manifestId,
  );
  const secrets = Object.fromEntries(
    Object.entries(SECRETS).map(([name, file]) => [
      name,
      { kind: "file" as const, path: layout.watcherSecret(file) },
    ]),
  ) as Record<keyof typeof SECRETS, SecretSource>;
  const paths = releasePaths(layout);
  const { authority, operations } = endpoints(run);

  const watcherInput = watcherConfigInput(
    context,
    watcher.WATCHER_CONFIG_SCHEMA_VERSION,
    manifest.l1Finality.confirmationDepth,
    secrets,
  );
  watcher.parseWatcherConfig(watcherInput);
  const policy = watcher.makeWatcherFinalityPolicy(watcherInput, identity);
  if (policy === null)
    throw new Error(
      "the watcher refused a finality policy for this configuration and release",
    );
  const authorityInput = {
    schemaVersion:
      watcher.WATCHER_TRUSTED_HEAD_AUTHORITY_PROCESS_CONFIG_SCHEMA_VERSION,
    policy,
    liveRecordLimit: FRESH_AUTHORITY_PROFILE.liveRecordLimit,
    directory: join(layout.watcherData, "trusted-head"),
    endpoint: authority,
    recordAuthenticationKeySource: secrets.record,
    httpBearerSecretSource: secrets.bearer,
  };
  const authorityConfig = watcher.parseWatcherTrustedHeadAuthorityProcessConfig(
    JSON.parse(JSON.stringify(authorityInput)),
  );
  const descriptorPath = join(layout.watcher, "authority-provisioning.json");
  const prepared = prepareWatcherAuthorityProvisioning({
    config: authorityConfig,
    descriptorPath,
    secretPaths: Object.values(secrets).map((source) => source.path),
    protectedPaths: [
      layout.watcherRuntimeConfig,
      layout.watcherAuthorityConfig,
      layout.watcherProcessConfig,
      layout.watcherData,
    ],
    initialize: initializeAuthority,
  });
  ensureSecrets(context, prepared.allowMissingSecrets);
  mkdirSync(layout.watcherData, { recursive: true, mode: 0o700 });
  const processInput = {
    schemaVersion: watcher.WATCHER_PROCESS_CONFIG_SCHEMA_VERSION,
    watcherConfig: watcherInput,
    watcherRuntimeConfigPath: layout.watcherRuntimeConfig,
    deploymentAuthorityPath: paths.authority,
    ruleBundlePath: paths.rules,
    fundingProfileBundlePath: paths.fundingProfiles,
    l1NodeTransportBinaryPath: artifacts.transportBinary,
    trustedHeadAuthorityEndpoint: authority,
    operationsEndpoint: operations,
    httpBearerSecretSource: secrets.bearer,
    workflowJournalDirectory: join(layout.watcherData, "workflows"),
    availability: {
      keySource: secrets.availability,
      journalPath: join(layout.watcherData, "availability.sqlite"),
      minimumFundingLovelace: "100000000",
    },
    faultProofInfrastructure: {
      manifestPath: paths.manifest,
      blueprintPath: paths.blueprint,
      deploymentInfoPath: paths.deploymentInfo,
      historicalNativeScriptHistory: history,
    },
  };
  watcher.parseWatcherProcessConfig(processInput);
  writeOnceFile(layout.watcherRuntimeConfig, configText(watcherInput));
  writeOnceFile(layout.watcherAuthorityConfig, configText(authorityInput));
  writeOnceFile(layout.watcherProcessConfig, configText(processInput));
  await finishWatcherAuthorityProvisioning({
    prepared,
    config: authorityConfig,
    configPath: layout.watcherAuthorityConfig,
    descriptorPath,
    cliPath: join(layout.watcherRoot, "dist/cli.js"),
  });
};

const WATCHER_ENV = {
  MALLOC_MMAP_THRESHOLD_: "131072",
  MIDGARD_CONFIG_MODE: "disabled",
  MIDGARD_DOTENV_MODE: "disabled",
} as const;

/** True once the trusted-head authority answers an authenticated identity read. */
export const authorityAnswers = async (
  layout: Layout,
  run: RunEnv,
): Promise<boolean> => {
  const bearer = readFileSync(layout.watcherSecret(SECRETS.bearer), "utf8");
  try {
    const response = await fetch(`${endpoints(run).authority}/v1/identity`, {
      headers: { authorization: `Bearer ${bearer}` },
      signal: AbortSignal.timeout(5_000),
    });
    await response.arrayBuffer();
    return response.ok;
  } catch {
    return false;
  }
};

/**
 * The watcher's processes, in dependency order: the trusted-head authority,
 * the history providers with their tunnel and feeder, then the watcher, which
 * starts only once the authority answers.
 *
 * The authority serves only bearer-authenticated routes, so it has no
 * liveness URL; it is restarted when it exits. The history providers speak
 * TLS under their own self-signed identities, so they too are watched by exit
 * only. The watcher's operations endpoint needs no credentials, but it opens
 * only after the watcher's startup catch-up, so the watcher gets a long start
 * grace.
 */
export const watcherServiceSpecs = (
  context: DeployContext,
  _oneShot: HubOracleOneShot,
): ServiceSpec[] => {
  const { layout, run } = context;
  const cli = join(layout.watcherRoot, "dist/cli.js");
  const controller = join(layout.toolsRoot, "dist/devnet-stack.js");
  const { operations } = endpoints(run);
  const tunnel = `http://127.0.0.1:${servicePorts(run).historyTunnel}`;
  const binding = historyRecordedBinding(layout, run, "Custom");
  const historyReadiness = (
    role: HistoryChildRole,
  ): HistoryReadinessSpecification => ({
    role,
    runId: run.runId,
    deploymentFingerprint: binding.manifest.manifestId,
    publicBindingDigest: binding.digest,
    expectedNetwork: "Custom",
  });
  const own = (name: string, args: readonly string[]): ServiceSpec => ({
    name,
    command: process.execPath,
    args: [controller, ...args, "--run-dir", layout.runDir],
    cwd: layout.toolsRoot,
    env: {},
  });
  return [
    {
      name: "watcher-authority",
      readyProbe: authorityReadinessProbe(layout, run),
      command: process.execPath,
      args: [cli, "authority", "--config", layout.watcherAuthorityConfig],
      cwd: layout.watcherRoot,
      env: { ...WATCHER_ENV },
    },
    ...HISTORY_ROLES.map((role) => ({
      ...own(`watcher-history-${role}`, [
        "history-archive",
        "--provider",
        role,
      ]),
      historyReadiness: historyReadiness(
        role === "a" ? "history-archive-a" : "history-archive-b",
      ),
    })),
    {
      ...own("watcher-history-tunnel", ["history-tunnel"]),
      historyReadiness: historyReadiness("history-tunnel"),
      healthUrl: `${tunnel}/healthz`,
    },
    {
      ...own("watcher-history-recorder", ["history-recorder"]),
      historyReadiness: historyReadiness("history-recorder"),
    },
    {
      name: "watcher",
      command: process.execPath,
      args: [cli, "start", "--config", layout.watcherProcessConfig],
      cwd: layout.watcherRoot,
      env: { ...WATCHER_ENV, ...historyTransportEnvironment(layout, run) },
      healthUrl: `${operations}/v1/status`,
      readyUrl: `${operations}/readyz`,
      startGraceMs: 60 * 60_000,
      prestart: () => authorityAnswers(layout, run),
    },
  ];
};
