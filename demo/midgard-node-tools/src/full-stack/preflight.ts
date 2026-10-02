import { createHash } from "node:crypto";
import { readFile } from "node:fs/promises";
import { createRequire } from "node:module";
import { basename, join, relative, resolve } from "node:path";

import { loadDaLibp2pIdentity } from "@al-ft/midgard-core/da-libp2p-identity";
import { validateMnemonic } from "bip39";
import { parse } from "dotenv";

import { configureStackCommittee } from "./committee.js";
import type { StackConfig } from "./config.js";
import { deriveStackWallets } from "./wallets.js";

const WATCHER_IDENTITY_KEY_FILES = [
  "WATCHER_RECORD_KEY_FILE",
  "WATCHER_ROLLBACK_KEY_FILE",
  "WATCHER_PROVER_KEY_FILE",
  "WATCHER_AVAILABILITY_KEY_FILE",
] as const;
const inside = (path: string, directory: string) => {
  const offset = relative(directory, path);
  return offset === "" || (!offset.startsWith("..") && offset !== path);
};

/** Stack-only secrets use their own namespace, so blanking them never blanks a node setting. */
function assertStackSecretNames(config: StackConfig) {
  const names = [
    config.wallets.user!.seedEnv,
    config.wallets.recipient!.seedEnv,
    config.wallets.prover!.seedEnv,
    config.wallets.availability!.seedEnv,
    config.da.submitterSeedEnv,
    config.da.producerTransportEnv,
    config.da.retainedTransportEnv,
    config.da.databasePasswordEnv,
    config.da.readerPasswordEnv,
    ...config.da.members.flatMap((member) => [
      member.seedEnv,
      member.transportEnv,
    ]),
  ];
  for (const name of names)
    if (!name.startsWith("STACK_"))
      throw new Error(
        `${name} must start with STACK_; stack secrets must not name a node, DA or watcher setting`,
      );
  if (new Set(names).size !== names.length)
    throw new Error("Stack secret variables must be distinct");
}

/** Generated secret files must stay out of every Docker build context. */
function assertPrivateDirectories(config: StackConfig) {
  const context = resolve(config.nodeRoot, "..");
  const ignored = join(config.nodeRoot, "logs");
  for (const [label, path] of [
    ["runDirectory", config.runDirectory],
    ["releaseDirectory", config.watcher.releaseDirectory],
  ] as const)
    if (inside(path, context) && !inside(path, ignored))
      throw new Error(
        `${label} is inside the Docker build context ${context}; use a directory under ${ignored} or outside the checkout`,
      );
}

/** Host tools read these, so the node environment must never set them. */
function assertHostEnvironmentUntouched(env: Record<string, string>) {
  for (const key of Object.keys(env))
    if (
      ["PATH", "HOME", "NODE_OPTIONS", "BASH_ENV", "ENV"].includes(key) ||
      key.startsWith("LD_") ||
      key.startsWith("DOCKER_")
    )
      throw new Error(
        `The stack environment must not set host variable ${key}`,
      );
}

function assertHostPorts(config: StackConfig, env: Record<string, string>) {
  const postgres = env.MIDGARD_POSTGRES_HOST_PORT ?? "";
  if (!/^[1-9][0-9]*$/.test(postgres) || ["5433", "55433"].includes(postgres))
    throw new Error(
      "Set MIDGARD_POSTGRES_HOST_PORT to this stack's own Postgres port; 5433 and 55433 belong to test databases",
    );
  // The operator Compose wrapper owns the list of host ports and their defaults.
  const { OPERATOR_HOST_PORTS } = createRequire(import.meta.url)(
    join(config.nodeRoot, "scripts/operator-compose.mjs"),
  ) as {
    OPERATOR_HOST_PORTS: readonly { variable: string; mainPort: number }[];
  };
  const operator = new Map(
    OPERATOR_HOST_PORTS.map(({ variable, mainPort }) => [
      Number(
        env[variable] ??
          (variable === "MIDGARD_NODE_API_HOST_PORT" ? env.PORT : undefined) ??
          mainPort,
      ),
      variable,
    ]),
  );
  const ports = config.da.ports;
  for (const [label, port] of [
    ["database", ports.database],
    ["retainedTransport", ports.retainedTransport],
    ...config.da.members.flatMap((_, index) => [
      [`committee ${index} API`, ports.committeeApiBase + index] as const,
      [
        `committee ${index} transport`,
        ports.committeeTransportBase + index,
      ] as const,
    ]),
  ] as const)
    if (operator.has(port))
      throw new Error(
        `DA ${label} port ${port} collides with ${operator.get(port)}`,
      );
}

/**
 * Every check that needs no build, network or service, so `--check` and a
 * full run refuse the same configurations before anything is built or spent.
 * Sets the derived committee variables in `env`.
 */
export async function checkStackEnvironment(
  config: StackConfig,
  env: Record<string, string>,
) {
  // The canonical Compose file uses this env_file even for provider-only startup.
  if (config.envFile !== join(config.nodeRoot, ".env"))
    throw new Error(
      "envFile must be the node directory's .env used by the existing Compose stack",
    );
  assertStackSecretNames(config);
  assertPrivateDirectories(config);
  assertHostEnvironmentUntouched(env);
  assertHostPorts(config, env);
  for (const key of [
    ...Object.values(config.wallets).map((wallet) => wallet.seedEnv),
    ...config.da.members.map((member) => member.seedEnv),
    ...(env.DA_COSIGNER_SEED_PHRASE ? ["DA_COSIGNER_SEED_PHRASE"] : []),
  ])
    if (!validateMnemonic(env[key]!))
      throw new Error(`Invalid wallet mnemonic in ${key}`);
  deriveStackWallets(config, env);
  await configureStackCommittee(config, env);
  const peers = await Promise.all(
    [
      config.da.producerTransportEnv,
      config.da.retainedTransportEnv,
      ...config.da.members.map((member) => member.transportEnv),
    ].map((key) => loadDaLibp2pIdentity(env[key]!)),
  );
  if (new Set(peers.map((peer) => peer.peerId)).size !== peers.length)
    throw new Error(
      "DA producer, retained service and members need distinct persistent identities",
    );
}

/**
 * Binds what identifies the deployment, and nothing an operator may need to
 * change to resume it: timeouts, journey size, budgets, ports, templates and
 * the watcher bearer are free to change between runs.
 */
export async function stackIntentDigest(
  config: StackConfig,
  env: Record<string, string>,
) {
  const hash = createHash("sha256");
  const bind = (label: string, value: string | Buffer) =>
    hash.update(
      `${label}\0${createHash("sha256").update(value).digest("hex")}\n`,
    );
  bind("network", env.NETWORK!);
  bind("profile", env.MIDGARD_DEPLOYMENT_PROFILE!);
  bind("nodeRoot", config.nodeRoot);
  bind("runDirectory", config.runDirectory);
  for (const [role, wallet] of Object.entries(config.wallets).sort())
    bind(`wallet ${role}`, env[wallet.seedEnv]!);
  // Committee indexes follow the sorted keys, not the configured order.
  bind(
    "committee",
    JSON.stringify(
      config.da.members
        .map((member) => [env[member.seedEnv]!, env[member.transportEnv]!])
        .sort(),
    ),
  );
  bind("producer transport", env[config.da.producerTransportEnv]!);
  bind("retained transport", env[config.da.retainedTransportEnv]!);
  for (const key of [
    "DA_THRESHOLD",
    "DA_COSIGNER_SEED_PHRASE",
    "DA_OWNERS_HEX",
  ])
    bind(key, env[key] ?? "");
  const watcherSecrets = parse(await readFile(config.watcher.composeEnvFile));
  for (const key of WATCHER_IDENTITY_KEY_FILES)
    bind(key, await readFile(watcherSecrets[key]!));
  if (config.watcher.releaseInput) {
    const release = JSON.parse(
      await readFile(config.watcher.releaseInput, "utf8"),
    ) as { signingKeyFile: string; programCommitments: unknown };
    // Measurements remain replaceable until their bundle is signed for the deployment.
    bind("release programs", JSON.stringify(release.programCommitments));
    bind("release signer", await readFile(release.signingKeyFile));
  } else {
    const template = JSON.parse(
      await readFile(config.watcher.processTemplate, "utf8"),
    ) as {
      deploymentAuthorityPath: string;
      ruleBundlePath: string;
      fundingProfileBundlePath: string;
      faultProofInfrastructure: {
        manifestPath: string;
        blueprintPath: string;
        deploymentInfoPath: string;
      };
    };
    for (const input of [
      template.deploymentAuthorityPath,
      template.ruleBundlePath,
      template.fundingProfileBundlePath,
      template.faultProofInfrastructure.manifestPath,
      template.faultProofInfrastructure.blueprintPath,
      template.faultProofInfrastructure.deploymentInfoPath,
    ])
      bind(
        `release ${basename(input)}`,
        await readFile(join(config.watcher.releaseDirectory, basename(input))),
      );
  }
  return hash.digest("hex");
}
