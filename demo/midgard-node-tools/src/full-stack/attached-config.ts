import { execFile } from "node:child_process";
import { createHash } from "node:crypto";
import { readFile } from "node:fs/promises";
import { createRequire } from "node:module";
import { isAbsolute, join, resolve } from "node:path";
import { promisify } from "node:util";

import { loadDaLibp2pIdentity } from "@al-ft/midgard-core/da-libp2p-identity";
import { SELECTED_DEPLOYMENT_PROFILE } from "@al-ft/midgard-core/deployment-profile";
import { paymentCredentialOf } from "@lucid-evolution/lucid";
import { validateMnemonic } from "bip39";
import { parse } from "dotenv";

import { localUrl, parseStackConfig, type StackConfig } from "./config.js";
import {
  assertHostEnvironmentUntouched,
  assertHostPorts,
  assertPrivateDirectories,
} from "./preflight.js";
import { deriveStackWallets } from "./wallets.js";

/**
 * A second instance's node wallets: the four a stack's node runs, by the
 * same variable names, each checked and funded as the first instance's are.
 */
const NODE_WALLETS: Record<string, string> = {
  operator: "L1_OPERATOR_SEED_PHRASE",
  merge: "L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX",
  reference: "L1_REFERENCE_SCRIPT_SEED_PHRASE",
  settlement: "L1_SETTLEMENT_SEED_PHRASE",
};
/** The host ports a second instance's node and Postgres publish. */
const NODE_HOST_PORTS = [
  "MIDGARD_NODE_API_HOST_PORT",
  "MIDGARD_NODE_METRICS_HOST_PORT",
  "MIDGARD_NODE_DA_HOST_PORT",
  "MIDGARD_POSTGRES_HOST_PORT",
];
const FIELDS = [
  "attachTo",
  "nodeRoot",
  "envFile",
  "runDirectory",
  "endpoint",
  "timeoutMs",
  "wallets",
  "producerTransportEnv",
];

/** A second-instance configuration names the stack configuration it attaches to. */
export const isAttachedStackConfig = (value: unknown) =>
  value !== null &&
  typeof value === "object" &&
  Object.prototype.hasOwnProperty.call(value, "attachTo");

function absolute(value: unknown, label: string) {
  if (typeof value !== "string" || !isAbsolute(value))
    throw new Error(`${label} must be an absolute path`);
  return resolve(value);
}

/**
 * A second instance's configuration over the stack it attaches to: its own
 * node directory, environment file, run directory, endpoint and node wallets,
 * and its own DA producer identity, by variable name. Everything else is the
 * first instance's.
 */
export function parseAttachedStackConfig(
  value: unknown,
  primary: StackConfig,
): StackConfig {
  const input = value as Record<string, unknown>;
  if (Object.keys(input).sort().join() !== [...FIELDS].sort().join())
    throw new Error(
      `A second-instance configuration has exactly the fields ${FIELDS.join(", ")}`,
    );
  if (primary.public !== undefined)
    throw new Error(
      "A second instance attaches only to a preprod-testing stack, not to a public one",
    );
  const wallets = input.wallets as Record<string, unknown>;
  if (
    wallets === null ||
    typeof wallets !== "object" ||
    Object.keys(wallets).sort().join() !==
      Object.keys(NODE_WALLETS).sort().join()
  )
    throw new Error(
      `A second instance configures exactly the node wallets ${Object.keys(NODE_WALLETS).join(", ")}`,
    );
  const nodeWallets = Object.fromEntries(
    Object.entries(NODE_WALLETS).map(([role, seedEnv]) => {
      const wallet = wallets[role] as Record<string, unknown>;
      if (
        Object.keys(wallet ?? {})
          .sort()
          .join() !== "minimumLovelace,seedEnv" ||
        wallet.seedEnv !== seedEnv ||
        typeof wallet.minimumLovelace !== "string" ||
        !/^[1-9][0-9]*$/.test(wallet.minimumLovelace)
      )
        throw new Error(
          `Wallet ${role} must use ${seedEnv} with a positive minimumLovelace`,
        );
      return [role, { seedEnv, minimumLovelace: wallet.minimumLovelace }];
    }),
  );
  const producerTransportEnv = input.producerTransportEnv;
  if (
    typeof producerTransportEnv !== "string" ||
    !/^STACK_[A-Z0-9_]+$/.test(producerTransportEnv)
  )
    throw new Error(
      "producerTransportEnv must name a STACK_ variable, which the node's environment never carries",
    );
  if (
    typeof input.timeoutMs !== "number" ||
    !Number.isSafeInteger(input.timeoutMs) ||
    input.timeoutMs <= 0
  )
    throw new Error("timeoutMs must be a positive integer");
  const config: StackConfig = {
    ...primary,
    nodeRoot: absolute(input.nodeRoot, "nodeRoot"),
    envFile: absolute(input.envFile, "envFile"),
    runDirectory: absolute(input.runDirectory, "runDirectory"),
    endpoint: localUrl(String(input.endpoint)),
    timeoutMs: input.timeoutMs,
    wallets: nodeWallets,
    da: { ...primary.da, producerTransportEnv },
    attachedTo: primary,
  };
  if (config.envFile !== join(config.nodeRoot, ".env"))
    throw new Error("envFile must be the node directory's .env");
  if (
    config.nodeRoot === primary.nodeRoot ||
    config.runDirectory === primary.runDirectory
  )
    throw new Error(
      "A second instance needs its own node directory and run directory",
    );
  return config;
}

/** The node environment Compose reads, with the wrapper's derived ports. */
async function composeEnvironment(config: StackConfig) {
  const env = parse(await readFile(config.envFile));
  const derived = await promisify(execFile)(
    "bash",
    [
      "scripts/operator-compose.sh",
      "--print-env",
      "--env-file",
      config.envFile,
    ],
    {
      cwd: config.nodeRoot,
      env: { ...env, PATH: process.env.PATH, HOME: process.env.HOME },
    },
  );
  return { ...env, ...parse(derived.stdout) };
}

/** A host port as Compose publishes it: the variable, else the main checkout's. */
function hostPort(
  config: StackConfig,
  env: Record<string, string>,
  variable: string,
) {
  const { OPERATOR_HOST_PORTS } = createRequire(import.meta.url)(
    join(config.nodeRoot, "scripts/operator-compose.mjs"),
  ) as {
    OPERATOR_HOST_PORTS: readonly { variable: string; mainPort: number }[];
  };
  return Number(
    env[variable] ??
      (variable === "MIDGARD_NODE_API_HOST_PORT" ? env.PORT : undefined) ??
      OPERATOR_HOST_PORTS.find((port) => port.variable === variable)!.mainPort,
  );
}

/**
 * Loads a second instance that attaches to a running preprod-testing stack:
 * its node joins that deployment as another operator, reading the stack's
 * Cardano node, and publishes to the stack's committee, which must admit its
 * producer (`da.admittedProducers` of the stack's configuration).
 */
export async function loadAttachedStackConfig(path: string) {
  const value = JSON.parse(await readFile(path, "utf8")) as Record<
    string,
    unknown
  >;
  const attachTo = absolute(value.attachTo, "attachTo");
  const primary = parseStackConfig(
    JSON.parse(await readFile(attachTo, "utf8")),
  );
  const config = parseAttachedStackConfig(value, primary);
  const env = await composeEnvironment(config);
  const compiled: string = SELECTED_DEPLOYMENT_PROFILE.name;
  if (
    env.NETWORK !== "Preprod" ||
    env.MIDGARD_DEPLOYMENT_PROFILE !== "preprod-testing" ||
    compiled !== "preprod-testing" ||
    env.RUN_GENESIS_ON_STARTUP !== "false"
  )
    throw new Error(
      "A second instance requires Preprod, MIDGARD_DEPLOYMENT_PROFILE=preprod-testing in a checkout compiled for it, and RUN_GENESIS_ON_STARTUP=false",
    );
  for (const key of ["MIN_FEE_A", "MIN_FEE_B"])
    if (!/^[0-9]+$/.test(env[key] ?? ""))
      throw new Error(`Set exact ${key} in the node environment`);
  assertHostEnvironmentUntouched(env);
  // Its own Postgres port, and no port of the stack's DA services.
  assertHostPorts(config, env);
  // It writes only its run directory: the release is the stack's.
  assertPrivateDirectories(config, { runDirectory: config.runDirectory });
  if (
    Number(new URL(config.endpoint).port || 80) !==
    hostPort(config, env, "MIDGARD_NODE_API_HOST_PORT")
  )
    throw new Error("Node endpoint does not match Compose host port");
  const primaryEnv = await composeEnvironment(primary);
  for (const variable of NODE_HOST_PORTS)
    if (
      hostPort(config, env, variable) ===
      hostPort(primary, primaryEnv, variable)
    )
      throw new Error(
        `${variable} is the port of the stack this instance attaches to; give the second instance its own`,
      );
  if (
    (env.COMPOSE_PROJECT_NAME ?? "midgard-node") ===
    (primaryEnv.COMPOSE_PROJECT_NAME ?? "midgard-node")
  )
    throw new Error(
      "A second instance needs its own Compose project: run it from another checkout",
    );
  for (const seedEnv of Object.values(NODE_WALLETS))
    if (!validateMnemonic(env[seedEnv] ?? ""))
      throw new Error(`Invalid wallet mnemonic in ${seedEnv}`);
  const own = deriveStackWallets(config, env);
  // Operator-2 is another operator: none of its wallets may spend from or
  // sign as any wallet of the stack it attaches to.
  const primaryCredentials = Object.entries(
    deriveStackWallets(primary, primaryEnv),
  ).map(([role, address]) => [role, paymentCredentialOf(address).hash]);
  for (const [role, address] of Object.entries(own)) {
    const credential = paymentCredentialOf(address).hash;
    const shared = primaryCredentials.find(([, hash]) => hash === credential);
    if (shared !== undefined)
      throw new Error(
        `Operator-2's ${role} wallet shares a payment key with the stack's ${shared[0]} wallet`,
      );
  }
  const transport = env[config.da.producerTransportEnv] ?? "";
  if (transport === "" || transport.startsWith("file:"))
    throw new Error(
      `Set ${config.da.producerTransportEnv} to a persistent seed/hex transport identity`,
    );
  // The committee accepts payloads from the peers its configuration admits.
  const { peerId } = await loadDaLibp2pIdentity(transport);
  const admittedProducer = `/ip4/127.0.0.1/tcp/${hostPort(config, env, "MIDGARD_NODE_DA_HOST_PORT")}/p2p/${peerId}`;
  if (!(primary.da.admittedProducers ?? []).includes(admittedProducer))
    throw new Error(
      `The stack's committee does not admit this node: add ${admittedProducer} to da.admittedProducers in ${attachTo} and run that stack again`,
    );
  return {
    config,
    env,
    admittedProducer,
    intentDigest: attachedIntentDigest(config, env),
  };
}

/** Binds the deployment this instance joins and its identities. */
function attachedIntentDigest(
  config: StackConfig,
  env: Record<string, string>,
) {
  const hash = createHash("sha256");
  const bind = (label: string, value: string) =>
    hash.update(
      `${label}\0${createHash("sha256").update(value).digest("hex")}\n`,
    );
  bind("network", env.NETWORK!);
  bind("profile", env.MIDGARD_DEPLOYMENT_PROFILE!);
  bind("nodeRoot", config.nodeRoot);
  bind("runDirectory", config.runDirectory);
  bind("attached nodeRoot", config.attachedTo!.nodeRoot);
  bind("attached runDirectory", config.attachedTo!.runDirectory);
  for (const [role, wallet] of Object.entries(config.wallets).sort())
    bind(`wallet ${role}`, env[wallet.seedEnv]!);
  bind("producer transport", env[config.da.producerTransportEnv]!);
  return hash.digest("hex");
}
