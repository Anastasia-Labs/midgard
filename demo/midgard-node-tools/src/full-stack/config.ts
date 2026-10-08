import { execFile } from "node:child_process";
import { constants } from "node:fs";
import { access, readFile } from "node:fs/promises";
import { isAbsolute, resolve } from "node:path";
import { promisify } from "node:util";

import { parse } from "dotenv";

import { l1ProviderFailoverEnabled } from "../environment.js";
import { checkStackEnvironment, stackIntentDigest } from "./preflight.js";

export type StackConfig = {
  nodeRoot: string;
  envFile: string;
  runDirectory: string;
  endpoint: string;
  timeoutMs: number;
  wallets: Record<string, { seedEnv: string; minimumLovelace: string }>;
  da: {
    ports: {
      database: number;
      committeeApiBase: number;
      committeeTransportBase: number;
      retainedTransport: number;
    };
    members: { seedEnv: string; transportEnv: string }[];
    producerTransportEnv: string;
    retainedTransportEnv: string;
    submitterSeedEnv: string;
    databasePasswordEnv: string;
    readerPasswordEnv: string;
  };
  watcher: {
    processTemplate: string;
    releaseDirectory: string;
    releaseInput: string | null;
    configDirectory: string;
    composeEnvFile: string;
  };
  journey: {
    depositLovelace: string;
    transferLovelace: string;
    cycles: number;
  };
};

function record(value: unknown, label: string): Record<string, unknown> {
  if (!value || typeof value !== "object" || Array.isArray(value))
    throw new Error(`${label} must be an object`);
  return value as Record<string, unknown>;
}
function exact(value: unknown, keys: readonly string[], label: string) {
  const result = record(value, label);
  if (Object.keys(result).sort().join() !== [...keys].sort().join())
    throw new Error(`${label} has missing or unknown fields`);
  return result;
}
function text(value: unknown, label: string): string {
  if (typeof value !== "string" || !value.trim())
    throw new Error(`${label} must be a nonempty string`);
  return value;
}
function positive(value: unknown, label: string): number {
  if (typeof value !== "number" || !Number.isSafeInteger(value) || value <= 0)
    throw new Error(`${label} must be a positive integer`);
  return value;
}
function amount(value: unknown, label: string) {
  const result = text(value, label);
  if (!/^[1-9][0-9]*$/.test(result))
    throw new Error(`${label} must be positive lovelace`);
  return result;
}
function envName(value: unknown): string {
  const name = text(value, "environment variable");
  if (!/^[A-Z][A-Z0-9_]*$/.test(name))
    throw new Error("Invalid environment variable name");
  return name;
}
export function localUrl(value: string): string {
  const url = new URL(value);
  if (
    !["http:", "ws:"].includes(url.protocol) ||
    !["127.0.0.1", "localhost", "[::1]"].includes(url.hostname) ||
    url.username ||
    url.password
  )
    throw new Error(
      "The full-stack command requires a local provider/operations URL",
    );
  return value.replace(/\/$/, "");
}
export function parseStackConfig(value: unknown): StackConfig {
  const input = exact(
    value,
    [
      "nodeRoot",
      "envFile",
      "runDirectory",
      "endpoint",
      "timeoutMs",
      "wallets",
      "da",
      "watcher",
      "journey",
    ],
    "stack configuration",
  );
  const path = (key: string) => {
    const result = text(input[key], key);
    if (!isAbsolute(result)) throw new Error(`${key} must be absolute`);
    return resolve(result);
  };
  const wallets = Object.fromEntries(
    Object.entries(record(input.wallets, "wallets")).map(([role, raw]) => {
      const wallet = exact(
        raw,
        ["seedEnv", "minimumLovelace"],
        `wallet ${role}`,
      );
      return [
        role,
        {
          seedEnv: envName(wallet.seedEnv),
          minimumLovelace: amount(wallet.minimumLovelace, role),
        },
      ];
    }),
  );
  for (const role of [
    "operator",
    "merge",
    "reference",
    "settlement",
    "user",
    "recipient",
    "daSubmitter",
    "prover",
    "availability",
  ])
    if (!wallets[role]) throw new Error(`Missing ${role} wallet budget`);
  for (const [role, seedEnv] of Object.entries({
    operator: "L1_OPERATOR_SEED_PHRASE",
    merge: "L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX",
    reference: "L1_REFERENCE_SCRIPT_SEED_PHRASE",
    settlement: "L1_SETTLEMENT_SEED_PHRASE",
  }))
    if (wallets[role]!.seedEnv !== seedEnv)
      throw new Error(`Wallet ${role} must use ${seedEnv}`);
  const da = exact(
    input.da,
    [
      "ports",
      "members",
      "producerTransportEnv",
      "retainedTransportEnv",
      "submitterSeedEnv",
      "databasePasswordEnv",
      "readerPasswordEnv",
    ],
    "DA configuration",
  );
  if (
    !Array.isArray(da.members) ||
    da.members.length === 0 ||
    da.members.length > 256
  )
    throw new Error("Configure between one and 256 DA members");
  const members = da.members.map((raw) => {
    const member = exact(raw, ["seedEnv", "transportEnv"], "DA member");
    return {
      seedEnv: envName(member.seedEnv),
      transportEnv: envName(member.transportEnv),
    };
  });
  const rawPorts = exact(
    da.ports,
    [
      "database",
      "committeeApiBase",
      "committeeTransportBase",
      "retainedTransport",
    ],
    "DA ports",
  );
  const ports = Object.fromEntries(
    Object.entries(rawPorts).map(([name, raw]) => [
      name,
      positive(raw, `DA port ${name}`),
    ]),
  ) as StackConfig["da"]["ports"];
  const usedPorts = [
    ports.database,
    ports.retainedTransport,
    ...members.flatMap((_, index) => [
      ports.committeeApiBase + index,
      ports.committeeTransportBase + index,
    ]),
  ];
  if (
    usedPorts.some((port) => port > 65535) ||
    new Set(usedPorts).size !== usedPorts.length
  )
    throw new Error("DA ports must be distinct TCP ports");
  const watcher = exact(
    input.watcher,
    [
      "processTemplate",
      "releaseDirectory",
      "releaseInput",
      "configDirectory",
      "composeEnvFile",
    ],
    "watcher configuration",
  );
  for (const value of Object.values(watcher).filter((value) => value !== null))
    if (!isAbsolute(text(value, "watcher path")))
      throw new Error("Watcher paths must be absolute");
  const journey = exact(
    input.journey,
    ["depositLovelace", "transferLovelace", "cycles"],
    "journey",
  );
  const depositLovelace = amount(journey.depositLovelace, "deposit");
  const transferLovelace = amount(journey.transferLovelace, "transfer");
  if (BigInt(depositLovelace) <= BigInt(transferLovelace) + 2_000_000n)
    throw new Error("Deposit must cover the transfer, change and L2 fee");
  return {
    nodeRoot: path("nodeRoot"),
    envFile: path("envFile"),
    runDirectory: path("runDirectory"),
    endpoint: localUrl(text(input.endpoint, "endpoint")),
    timeoutMs: positive(input.timeoutMs, "timeoutMs"),
    wallets,
    da: {
      ports,
      members,
      producerTransportEnv: envName(da.producerTransportEnv),
      retainedTransportEnv: envName(da.retainedTransportEnv),
      submitterSeedEnv: envName(da.submitterSeedEnv),
      databasePasswordEnv: envName(da.databasePasswordEnv),
      readerPasswordEnv: envName(da.readerPasswordEnv),
    },
    watcher: watcher as StackConfig["watcher"],
    journey: {
      depositLovelace,
      transferLovelace,
      cycles: positive(journey.cycles, "cycles"),
    },
  };
}

export async function loadStackConfig(path: string) {
  const config = parseStackConfig(JSON.parse(await readFile(path, "utf8")));
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
  Object.assign(env, parse(derived.stdout));
  if (
    Number(new URL(config.endpoint).port || 80) !==
    Number(env.MIDGARD_NODE_API_HOST_PORT ?? env.PORT ?? 3000)
  )
    throw new Error("Node endpoint does not match Compose host port");
  for (const [key, port] of [
    ["L1_KUPO_KEY", env.KUPO_PORT ?? 1442],
    ["L1_OGMIOS_KEY", env.OGMIOS_PORT ?? 1337],
  ] as const)
    if (Number(new URL(env[key]!).port || 80) !== Number(port))
      throw new Error(`${key} does not match the local Compose provider port`);

  if (
    env.NETWORK !== "Preprod" ||
    env.MIDGARD_DEPLOYMENT_PROFILE !== "preprod-testing" ||
    env.L1_PROVIDER !== "Kupmios" ||
    l1ProviderFailoverEnabled(env.L1_PROVIDER_FAILOVER) ||
    env.RUN_GENESIS_ON_STARTUP !== "false"
  )
    throw new Error(
      "Require Preprod with preprod-testing profile, local Kupmios, no failover, and RUN_GENESIS_ON_STARTUP=false",
    );
  localUrl(env.L1_KUPO_KEY!);
  localUrl(env.L1_OGMIOS_KEY!);
  for (const key of ["MIN_FEE_A", "MIN_FEE_B"])
    if (!/^[0-9]+$/.test(env[key] ?? ""))
      throw new Error(`Set exact ${key} in the node environment`);
  for (const wallet of Object.values(config.wallets))
    if (!env[wallet.seedEnv]) throw new Error(`Missing ${wallet.seedEnv}`);
  for (const key of [
    config.da.producerTransportEnv,
    config.da.retainedTransportEnv,
    config.da.submitterSeedEnv,
    config.da.databasePasswordEnv,
    config.da.readerPasswordEnv,
    ...config.da.members.flatMap((member) => [
      member.seedEnv,
      member.transportEnv,
    ]),
  ])
    if (!env[key]) throw new Error(`Missing ${key}`);
  if (env[config.da.databasePasswordEnv] === env[config.da.readerPasswordEnv])
    throw new Error("DA reader and writer passwords must differ");
  if (config.da.submitterSeedEnv !== config.wallets.daSubmitter!.seedEnv)
    throw new Error("DA submitter wallet budget does not match its signer");
  for (const key of [
    config.da.producerTransportEnv,
    config.da.retainedTransportEnv,
    ...config.da.members.map((member) => member.transportEnv),
  ])
    if (env[key]!.startsWith("file:"))
      throw new Error(
        "Use persistent seed/hex transport identities; container file sources are not portable",
      );
  if (
    BigInt(config.wallets.user!.minimumLovelace) <
    BigInt(config.journey.depositLovelace) * BigInt(config.journey.cycles) +
      10_000_000n
  )
    throw new Error("User funding budget must cover all deposits and fees");
  const watcherSecrets = parse(await readFile(config.watcher.composeEnvFile));
  for (const key of [
    "WATCHER_ROLLBACK_KEY_FILE",
    "WATCHER_PROVER_KEY_FILE",
    "WATCHER_AVAILABILITY_KEY_FILE",
  ]) {
    const path = watcherSecrets[key];
    if (!path || !isAbsolute(path))
      throw new Error(`Set absolute ${key} in watcher Compose environment`);
    await access(path, constants.R_OK);
  }
  if (config.watcher.releaseInput) {
    const release = JSON.parse(
      await readFile(config.watcher.releaseInput, "utf8"),
    ) as { signingKeyFile?: string; programCommitments?: unknown };
    if (!release.signingKeyFile || !isAbsolute(release.signingKeyFile))
      throw new Error("Release input needs an absolute signingKeyFile");
    record(release.programCommitments, "Release program commitments");
  }
  await checkStackEnvironment(config, env);
  return { config, env, intentDigest: await stackIntentDigest(config, env) };
}
