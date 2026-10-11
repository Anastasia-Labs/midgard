import { createHash, randomBytes } from "node:crypto";
import { existsSync, mkdirSync, readFileSync } from "node:fs";
import { join } from "node:path";

import type { L1Origin } from "@al-ft/midgard-core/l1-origin";

import {
  buildWatcherConfig,
  DEVNET_WATCHER_LIMITS,
} from "../watcher-config.js";
import { LOCAL_AUTHORITY_ID } from "./da.js";
import type { DeployContext } from "./deploy.js";
import { recordedL1Origin } from "./deployment-origin.js";
import { writeDurableFile, writeOnceFile } from "./durable.js";
import { type Layout, type RunEnv, servicePorts } from "./layout.js";
import type { HubOracleOneShot } from "./node-env.js";
import type { ServiceSpec } from "./supervisor.js";
import {
  ensureWatcherReleaseBundle,
  loadWatcherModule,
  readFinalizedManifest,
  releasePaths,
  type WatcherModule,
} from "./watcher-release.js";

type SecretSource = { readonly kind: "file"; readonly path: string };

const SECRETS = {
  rollback: "rollback-authority.key",
  prover: "prover.seed",
  availability: "availability.seed",
} as const;

const hex32 = () => randomBytes(32).toString("hex");

/**
 * The watcher's three secrets, one regular 0600 file each, no trailing
 * newline, written once. The wallet seeds are the run's own funded
 * `watcherProver` / `watcherAvailability` identities; the rollback key is
 * random and belongs to this run only.
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
    rollback: random(SECRETS.rollback),
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

const operationsEndpoint = (run: RunEnv) =>
  `http://127.0.0.1:${servicePorts(run).watcherOperations}`;

/** Where the watcher's secrets live; `ensureSecrets` writes them. */
const secretSources = (layout: Layout) =>
  Object.fromEntries(
    Object.entries(SECRETS).map(([name, file]) => [
      name,
      { kind: "file" as const, path: layout.watcherSecret(file) },
    ]),
  ) as Record<keyof typeof SECRETS, SecretSource>;

/** The watcher configuration facts only the deployment fixes. */
type DeploymentFacts = {
  readonly origin: L1Origin | undefined;
  readonly confirmationDepth: number;
  readonly retainedDaPeer: { identity: string; multiaddr: string };
};

/**
 * The watcher's runtime and process configurations for this run, each
 * checked by the watcher's own parser.
 */
const watcherConfigs = (
  context: Pick<DeployContext, "layout" | "run" | "artifacts">,
  watcher: WatcherModule,
  deployment: DeploymentFacts,
) => {
  const { layout, run, artifacts } = context;
  const secrets = secretSources(layout);
  const paths = releasePaths(layout);
  const { input: watcherInput } = buildWatcherConfig(watcher, {
    mode: "acceptance",
    network: { targetNetwork: "Custom", customNetwork: customNetwork(layout) },
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
    },
    origin: deployment.origin,
    finalityDepth: deployment.confirmationDepth,
    daPeers: [deployment.retainedDaPeer],
    storagePath: join(layout.watcherData, "watcher.sqlite"),
    rollbackAuthorityKeySource: secrets.rollback,
    proverKeySource: secrets.prover,
    limits: DEVNET_WATCHER_LIMITS,
  });
  const processInput = {
    schemaVersion: watcher.WATCHER_PROCESS_CONFIG_SCHEMA_VERSION,
    watcherConfig: watcherInput,
    watcherRuntimeConfigPath: layout.watcherRuntimeConfig,
    deploymentAuthorityPath: paths.authority,
    ruleBundlePath: paths.rules,
    fundingProfileBundlePath: paths.fundingProfiles,
    l1NodeTransportBinaryPath: artifacts.transportBinary,
    operationsEndpoint: operationsEndpoint(run),
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
    },
  };
  watcher.parseWatcherProcessConfig(processInput);
  return { watcherInput, processInput };
};

const configText = (value: unknown) => `${JSON.stringify(value, null, 2)}\n`;

/**
 * Everything the watcher reads, generated once under the run's stack
 * directory: the trust-root key, the secrets, the signed release bundle, and
 * the runtime and process configurations. Every configuration is checked by the watcher's
 * own parser before it is written. A second call verifies and changes
 * nothing; a release signed for another deployment, or a configuration that
 * would now come out differently, is refused rather than replaced. Secrets
 * are generated only for a fresh watcher (no configuration and no data yet);
 * an established watcher's missing secret is refused, never regenerated.
 */
export const ensureWatcherRelease = async (
  context: DeployContext,
  oneShot: HubOracleOneShot,
): Promise<void> => {
  const { layout } = context;
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
  await watcher
    .watcherDeploymentReleaseFinalityAuthority(identity)
    .verifyForWorkflow({ deploymentFingerprint: manifest.manifestId });
  const { watcherInput, processInput } = watcherConfigs(context, watcher, {
    // The follower starts at the run's recorded origin.
    origin: recordedL1Origin(layout, oneShot),
    confirmationDepth: manifest.l1Finality.confirmationDepth,
    retainedDaPeer: retainedDaPeer(layout),
  });
  const fresh = ![
    layout.watcherRuntimeConfig,
    layout.watcherProcessConfig,
    layout.watcherData,
  ].some(existsSync);
  ensureSecrets(context, fresh);
  mkdirSync(layout.watcherData, { recursive: true, mode: 0o700 });
  writeOnceFile(layout.watcherRuntimeConfig, configText(watcherInput));
  writeOnceFile(layout.watcherProcessConfig, configText(processInput));
};

const WATCHER_ENV = {
  MALLOC_MMAP_THRESHOLD_: "131072",
  MIDGARD_CONFIG_MODE: "disabled",
  MIDGARD_DOTENV_MODE: "disabled",
} as const;

/**
 * The watcher's process. Its operations endpoint needs no credentials, but it
 * opens only after the watcher's startup catch-up, so the watcher gets a long
 * start grace.
 */
export const watcherServiceSpecs = (
  context: Pick<DeployContext, "layout" | "run">,
): ServiceSpec[] => {
  const { layout, run } = context;
  const cli = join(layout.watcherRoot, "dist/cli.js");
  const operations = operationsEndpoint(run);
  return [
    {
      name: "watcher",
      command: process.execPath,
      args: [cli, "start", "--config", layout.watcherProcessConfig],
      cwd: layout.watcherRoot,
      env: { ...WATCHER_ENV },
      healthUrl: `${operations}/v1/status`,
      readyUrl: `${operations}/readyz`,
      startGraceMs: 60 * 60_000,
    },
  ];
};
