import { existsSync, readFileSync } from "node:fs";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import { parse as parseDotenv } from "dotenv";

/** Every path the devnet controller owns, derived from one run directory. */
export type Layout = ReturnType<typeof makeLayout>;

const repoRootFromBundle = (): string => {
  // dist/devnet-stack.js -> demo/midgard-node-tools -> demo -> repository.
  const here = dirname(fileURLToPath(import.meta.url));
  for (const candidate of [
    resolve(here, "../../.."),
    resolve(here, "../../../.."),
  ])
    if (existsSync(join(candidate, "onchain/aiken/aiken.toml")))
      return candidate;
  throw new Error(`cannot locate the repository root from ${here}`);
};

export const makeLayout = (runDir: string) => {
  const repoRoot = repoRootFromBundle();
  const demo = join(repoRoot, "demo");
  const phase4Root = join(demo, "midgard-node-tools/devnet/phase4-process");
  const state = join(runDir, "stack");
  return {
    runDir,
    repoRoot,
    nodeRoot: join(demo, "midgard-node"),
    daRoot: join(demo, "da-committee-node"),
    watcherRoot: join(demo, "midgard-watcher"),
    transportRoot: join(demo, "l1-node-transport"),
    toolsRoot: join(demo, "midgard-node-tools"),
    phase4Root,
    phase4Scripts: join(phase4Root, "scripts"),
    composeFile: join(phase4Root, "compose.yaml"),
    runEnv: join(runDir, "run.env"),
    /** Restart policy + socket mode for the L1 containers (written by us). */
    supervisedComposeFile: join(state, "compose.supervised.yaml"),
    state,
    identities: join(state, "identities.json"),
    journal: join(state, "journal.json"),
    lock: join(state, "controller.lock"),
    logs: join(state, "logs"),
    stepLogs: join(state, "logs/steps"),
    bin: join(state, "bin"),
    artifacts: join(state, "artifacts"),
    blueprint: join(state, "artifacts/plutus.json"),
    cardanoSocket: join(runDir, "cardano/ipc/node.socket"),
    hostCardanoConfig: join(runDir, "config/host-config.json"),
    deploymentInfo: join(runDir, "deploymentInfo"),
    contractManifest: join(
      runDir,
      "deploymentInfo/contract-deployment-info.json",
    ),
    deploymentRunState: join(
      runDir,
      "deploymentInfo/deployment-run-state.json",
    ),
    producerManifest: join(runDir, "deploymentInfo/da-producer-manifest.json"),
    committeeManifest: (index: number) =>
      join(runDir, `deploymentInfo/da-committee-${index}-manifest.json`),
    nodeData: join(state, "node"),
    committeeData: (index: number) => join(state, `da-committee-${index}`),
    supervisorPid: join(state, "supervisor.pid"),
    /** Digest of the service set the running supervisor was started with. */
    supervisorSpecs: join(state, "supervisor.specs"),
    /** Local live query discovery only; never durable readiness authority. */
    historyDaemonDescriptor: join(state, "history-daemon.json"),
    supervisorEvents: join(state, "logs/supervisor.ndjson"),
    serviceLog: (name: string) => join(state, `logs/${name}.log`),
    journeyDir: join(state, "journey"),
    /** One JSON line per chaos drill; `.restore.json` beside it is an owed restore. */
    drillsLog: join(state, "logs/drills.ndjson"),
    shelleyGenesis: join(runDir, "genesis/shelley-genesis.json"),
    /** The watcher's signed release, secrets, configs and stores. */
    watcher: join(state, "watcher"),
    /** ed25519 key that signs the watcher's deployment identity; never rotated. */
    watcherTrustRootKey: join(state, "watcher/trust-root.pem"),
    watcherSecret: (name: string) => join(state, `watcher/secrets/${name}`),
    watcherRelease: join(state, "watcher/release"),
    watcherRuntimeConfig: join(state, "watcher/config/watcher.json"),
    watcherProcessConfig: join(state, "watcher/config/watcher-process.json"),
    watcherAuthorityConfig: join(
      state,
      "watcher/config/authority-process.json",
    ),
    watcherData: join(state, "watcher/data"),
    /** Historical native-script history providers and their shared CA bundle. */
    watcherHistory: join(state, "watcher/history"),
    watcherHistoryArchive: (role: string) =>
      join(state, `watcher/history/${role}`),
    watcherHistoryCa: join(state, "watcher/history/archive-ca.pem"),
    watcherHistoryProviders: join(state, "watcher/history/providers.json"),
    /** The history recorder's index of state-queue commits by header hash. */
    watcherHistoryCommits: join(state, "watcher/history/commits"),
  };
};

export type RunEnv = {
  readonly runId: string;
  readonly composeProject: string;
  readonly networkMagic: number;
  readonly ogmiosPort: number;
  readonly kupoPort: number;
  readonly postgresPort: number;
  readonly postgresUser: string;
  readonly postgresPassword: string;
  readonly postgresDatabase: string;
  readonly cardanoImage: string;
  readonly postgresImage: string;
  /** Host-port offset of this checkout; every service port shifts by it. */
  readonly portOffset: number;
};

export const readRunEnv = (layout: Layout): RunEnv => {
  const env = parseDotenv(readFileSync(layout.runEnv, "utf8"));
  const need = (name: string): string => {
    const value = env[name];
    if (value === undefined || value === "")
      throw new Error(`${layout.runEnv} is missing ${name}`);
    return value;
  };
  if (resolve(need("MIDGARD_PHASE4_RUN_DIR")) !== resolve(layout.runDir))
    throw new Error("run.env belongs to a different run directory");
  const ogmiosPort = Number(need("MIDGARD_PHASE4_OGMIOS_PORT"));
  return {
    runId: need("MIDGARD_PHASE4_RUN_ID"),
    composeProject: need("MIDGARD_PHASE4_COMPOSE_PROJECT"),
    networkMagic: Number(need("MIDGARD_PHASE4_NETWORK_MAGIC")),
    ogmiosPort,
    kupoPort: Number(need("MIDGARD_PHASE4_KUPO_PORT")),
    postgresPort: Number(need("MIDGARD_PHASE4_POSTGRES_PORT")),
    postgresUser: need("MIDGARD_PHASE4_POSTGRES_USER"),
    postgresPassword: need("MIDGARD_PHASE4_POSTGRES_PASSWORD"),
    postgresDatabase: need("MIDGARD_PHASE4_POSTGRES_DATABASE"),
    cardanoImage: need("MIDGARD_PHASE4_CARDANO_NODE_IMAGE"),
    postgresImage: need("MIDGARD_PHASE4_POSTGRES_IMAGE"),
    portOffset: ogmiosPort - 2337,
  };
};

/** Host ports of the Midgard services, shifted like the L1 ports. */
export const servicePorts = (run: RunEnv) => {
  const ports = {
    nodeHttp: 3000 + run.portOffset,
    nodeMetrics: 9464 + run.portOffset,
    committeeApi: (index: number) => 8787 + run.portOffset + index,
    committeeLibp2p: (index: number) => 39001 + run.portOffset + 3 * index,
    producerLibp2p: 39002 + run.portOffset,
    retainedLibp2p: 39003 + run.portOffset,
    watcherAuthority: 7401 + run.portOffset,
    watcherOperations: 7402 + run.portOffset,
    historyArchive: (index: number) => 7403 + run.portOffset + index,
    historyTunnel: 7405 + run.portOffset,
    publicRetainedDaHealth: 7406 + run.portOffset,
  };
  const allocated = [
    ports.nodeHttp,
    ports.nodeMetrics,
    ports.committeeApi(0),
    ports.committeeApi(1),
    ports.committeeLibp2p(0),
    ports.committeeLibp2p(1),
    ports.producerLibp2p,
    ports.retainedLibp2p,
    ports.watcherAuthority,
    ports.watcherOperations,
    ports.historyArchive(0),
    ports.historyArchive(1),
    ports.historyTunnel,
    ports.publicRetainedDaHealth,
    run.ogmiosPort,
    run.kupoPort,
    run.postgresPort,
  ];
  if (
    allocated.some(
      (port) => !Number.isInteger(port) || port < 1 || port > 65535,
    )
  )
    throw new Error("devnet service ports must be valid TCP ports");
  if (new Set(allocated).size !== allocated.length)
    throw new Error("devnet service ports must not overlap");
  return ports;
};
