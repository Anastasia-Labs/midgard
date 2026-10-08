import { createHash } from "node:crypto";
import { mkdir, readFile } from "node:fs/promises";
import { join, resolve } from "node:path";

import { verifyFinalizedDeploymentManifest } from "@al-ft/midgard-core/deployment-manifest-identity";
import { parse } from "dotenv";
import { parseWatcherProcessConfig } from "midgard-watcher";

import { localUrl } from "./config.js";
import { stackPaths } from "./deployment.js";
import { readJsonIfPresent, writeDurableJson } from "./journal.js";
import { nativeLedgerPaths } from "./native-ledger.js";
import type { StackProcesses } from "./process.js";
import { prepareStackRelease } from "./release.js";

/** Release signatures and measured funding profiles are inputs, never fabricated. */
export async function generateWatcherServices(
  processes: StackProcesses,
  publicDaMultiaddr: string,
) {
  const { watcher } = processes.config;
  if (
    !resolve(watcher.configDirectory).startsWith(
      `${resolve(processes.config.runDirectory)}/`,
    )
  )
    throw new Error(
      "Generated watcher configuration must live inside the run directory",
    );
  const template = parseWatcherProcessConfig(
    await readJsonIfPresent(watcher.processTemplate),
  );
  if (template.watcherConfig.l1.source.sourceMode !== "local_node")
    throw new Error("Require local-node watcher chain sync");
  const release = await prepareStackRelease(processes);
  const manifest = verifyFinalizedDeploymentManifest(
    await readJsonIfPresent(stackPaths(processes).manifest),
  );
  if (release.deploymentIdentity.manifestId !== manifest.manifestId)
    throw new Error(
      "Watcher release artifacts must be signed for this exact deployment",
    );
  const env = parse(await readFile(watcher.composeEnvFile));
  for (const key of [
    "WATCHER_ROLLBACK_KEY_FILE",
    "WATCHER_PROVER_KEY_FILE",
    "WATCHER_AVAILABILITY_KEY_FILE",
  ])
    if (!env[key])
      throw new Error(`Missing ${key} in watcher Compose environment`);
  await mkdir(watcher.configDirectory, { recursive: true, mode: 0o700 });
  const l1Directory = nativeLedgerPaths(processes).directory;
  const genesis = await readFile(join(l1Directory, "shelley-genesis.json"));
  const genesisIdentitySha256 = createHash("sha256")
    .update(genesis)
    .digest("hex");
  const watcherConfig = {
    ...template.watcherConfig,
    l1: {
      ...template.watcherConfig.l1,
      source: {
        ...template.watcherConfig.l1.source,
        chainSync: {
          ...template.watcherConfig.l1.source.chainSync,
          genesisIdentitySha256,
        },
      },
    },
    da: {
      ...template.watcherConfig.da,
      peers: [{ identity: "public-retained-da", multiaddr: publicDaMultiaddr }],
    },
  };
  const processConfig = parseWatcherProcessConfig({
    ...template,
    watcherConfig,
  });
  localUrl(processConfig.operationsEndpoint);
  await writeDurableJson(
    join(watcher.configDirectory, "watcher-process.json"),
    processConfig,
  );
  await writeDurableJson(
    join(watcher.configDirectory, "watcher-runtime.json"),
    processConfig.watcherConfig,
  );
  const source = await renderWatcherCompose(processes, {
    env,
    operationsEndpoint: processConfig.operationsEndpoint,
    l1Directory,
  });
  return { ...source, operationsEndpoint: processConfig.operationsEndpoint };
}

const SECRET_MOUNTS = {
  "/run/secrets/rollback_key": "WATCHER_ROLLBACK_KEY_FILE",
  "/run/secrets/prover_key": "WATCHER_PROVER_KEY_FILE",
  "/run/secrets/availability_key": "WATCHER_AVAILABILITY_KEY_FILE",
} as const;
const explicitPort = (url: string) => {
  const port = new URL(url).port;
  if (!port) throw new Error(`Watcher endpoint ${url} needs an explicit port`);
  return port;
};

/**
 * Renders the watcher's own Compose file from the validated watcher env file
 * alone: the node stack's environment would otherwise take precedence over it.
 */
export async function renderWatcherCompose(
  processes: StackProcesses,
  input: {
    env: Record<string, string>;
    operationsEndpoint: string;
    l1Directory: string;
  },
) {
  const { config } = processes;
  const ports = {
    WATCHER_OPERATIONS_PORT: explicitPort(input.operationsEndpoint),
  };
  const taken = [
    Number(new URL(config.endpoint).port),
    config.da.ports.database,
    config.da.ports.retainedTransport,
    ...config.da.members.flatMap((_, index) => [
      config.da.ports.committeeApiBase + index,
      config.da.ports.committeeTransportBase + index,
    ]),
  ];
  if (taken.includes(Number(ports.WATCHER_OPERATIONS_PORT)))
    throw new Error(
      "The watcher operations port must differ from node and DA ports",
    );
  const ipc = join(config.nodeRoot, "cardano/ipc");
  const source = (await processes.command(
    "watcher-compose-configuration",
    "docker",
    [
      "compose",
      "--env-file",
      config.watcher.composeEnvFile,
      "-f",
      "compose.yaml",
      "config",
      "--format",
      "json",
    ],
    {
      ...input.env,
      MIDGARD_L1_CONFIG_DIR: input.l1Directory,
      MIDGARD_L1_IPC_DIR: ipc,
      ...ports,
    },
    resolve(config.nodeRoot, "../midgard-watcher"),
    "host",
  )) as {
    services: Record<
      string,
      {
        image?: string;
        volumes: { source: string; target: string; type: string }[];
      }
    >;
    volumes: Record<string, Record<string, unknown>>;
  };
  const volume = (target: string) => {
    const found = source.services.watcher!.volumes.find(
      (value) => value.target === target,
    );
    if (!found) throw new Error(`Watcher Compose is missing ${target}`);
    return found;
  };
  // Checked equals mounted: every secret comes from the validated env file.
  for (const [target, key] of Object.entries(SECRET_MOUNTS))
    if (resolve(volume(target).source) !== resolve(input.env[key]!))
      throw new Error(
        `Watcher ${target} is not ${key} from ${config.watcher.composeEnvFile}`,
      );
  for (const [target, path] of [
    [
      "/etc/midgard/watcher-process.json",
      join(config.watcher.configDirectory, "watcher-process.json"),
    ],
    [
      "/etc/midgard/watcher-runtime.json",
      join(config.watcher.configDirectory, "watcher-runtime.json"),
    ],
    ["/etc/midgard/bundles", config.watcher.releaseDirectory],
    ["/cardano-config", input.l1Directory],
    ["/ipc", ipc],
  ] as const)
    volume(target).source = path;
  // One image per Compose project, as for the node and DA images.
  source.services.watcher!.image = "midgard-watcher:${COMPOSE_PROJECT_NAME}";
  // Project-scoped on purpose: the stack's watcher keeps its own state, never
  // a standalone midgard-watcher deployment's volumes.
  for (const value of Object.values(source.volumes)) delete value.name;
  return source;
}
