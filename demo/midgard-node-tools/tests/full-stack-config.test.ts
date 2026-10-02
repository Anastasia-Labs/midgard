import { generateKeyPairSync } from "node:crypto";
import { mkdir, mkdtemp, readFile, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { fileURLToPath } from "node:url";

import { paymentCredentialOf, walletFromSeed } from "@lucid-evolution/lucid";
import { entropyToMnemonic } from "bip39";
import { parse } from "dotenv";
import { afterEach, describe, expect, it, vi } from "vitest";

import { configureStackCommittee } from "../src/full-stack/committee.js";
import { loadStackConfig, parseStackConfig } from "../src/full-stack/config.js";
import { runStackController } from "../src/full-stack/controller.js";

const operatorCompose = fileURLToPath(
  new URL("../../midgard-node/scripts/operator-compose.mjs", import.meta.url),
);
const directories: string[] = [];
afterEach(async () => {
  await Promise.all(
    directories
      .splice(0)
      .map((path) => rm(path, { recursive: true, force: true })),
  );
});
async function fixture() {
  const directory = await mkdtemp(join(tmpdir(), "midgard-stack-config-"));
  directories.push(directory);
  const sample = JSON.parse(
    await readFile(
      new URL("../config/preprod-stack.example.json", import.meta.url),
      "utf8",
    ),
  );
  sample.nodeRoot = directory;
  sample.envFile = join(directory, ".env");
  sample.runDirectory = join(directory, "logs/run");
  await mkdir(join(directory, "scripts"));
  // The wrapper prints no derived overrides in the main checkout.
  await writeFile(
    join(directory, "scripts/operator-compose.sh"),
    "#!/bin/sh\nexit 0\n",
  );
  await writeFile(
    join(directory, "scripts/operator-compose.mjs"),
    `export { OPERATOR_HOST_PORTS } from ${JSON.stringify(operatorCompose)};\n`,
  );
  for (const key of Object.keys(sample.watcher))
    if (key !== "configDirectory" && key !== "releaseDirectory")
      sample.watcher[key] = join(directory, key);
  sample.watcher.configDirectory = join(directory, "logs/run/watcher");
  sample.watcher.releaseDirectory = join(directory, "logs/run/release");
  await writeFile(sample.watcher.processTemplate, "{}");
  await writeFile(sample.watcher.authorityTemplate, "{}");
  const secretNames = [
    "WATCHER_RECORD_KEY_FILE",
    "WATCHER_ROLLBACK_KEY_FILE",
    "WATCHER_PROVER_KEY_FILE",
    "WATCHER_AVAILABILITY_KEY_FILE",
    "WATCHER_BEARER_FILE",
  ];
  const secretFiles = Object.fromEntries(
    secretNames.map((key, index) => [key, join(directory, `secret-${index}`)]),
  );
  sample.watcher.bearerFile = secretFiles.WATCHER_BEARER_FILE;
  for (const path of Object.values(secretFiles))
    await writeFile(path, "a".repeat(64));
  await writeFile(
    sample.watcher.composeEnvFile,
    Object.entries(secretFiles)
      .map(([key, path]) => `${key}=${path}`)
      .join("\n"),
  );
  const signingKeyFile = join(directory, "signer.pem");
  const privateKey = generateKeyPairSync("ed25519").privateKey;
  await writeFile(
    signingKeyFile,
    privateKey.export({ format: "pem", type: "pkcs8" }),
  );
  const release = {
    signingKeyFile,
    programCommitments: { program: "a".repeat(64) },
    fundingProfiles: [] as unknown[],
  };
  await writeFile(sample.watcher.releaseInput, JSON.stringify(release));
  const config = parseStackConfig(sample);
  const env: Record<string, string> = {
    NETWORK: "Preprod",
    MIDGARD_DEPLOYMENT_PROFILE: "preprod-testing",
    L1_PROVIDER: "Kupmios",
    RUN_GENESIS_ON_STARTUP: "false",
    L1_KUPO_KEY: "http://127.0.0.1:1442",
    L1_OGMIOS_KEY: "http://127.0.0.1:1337",
    MIN_FEE_A: "10",
    MIN_FEE_B: "10",
    DA_THRESHOLD: "2",
    MIDGARD_POSTGRES_HOST_PORT: "25433",
  };
  for (const [index, wallet] of Object.values(config.wallets).entries())
    env[wallet.seedEnv] = entropyToMnemonic(
      index.toString(16).padStart(32, "0"),
    );
  for (const [index, member] of config.da.members.entries())
    env[member.seedEnv] = entropyToMnemonic(
      (index + 10).toString(16).padStart(32, "0"),
    );
  env.DA_OWNERS_HEX = ["operator", "reference"]
    .map(
      (role) =>
        paymentCredentialOf(
          walletFromSeed(env[config.wallets[role]!.seedEnv]!, {
            network: "Preprod",
          }).address,
        ).hash,
    )
    .sort()
    .join("");
  for (const [index, key] of [
    config.da.producerTransportEnv,
    config.da.retainedTransportEnv,
    ...config.da.members.map((member) => member.transportEnv),
  ].entries())
    env[key] = `seed:${(index + 1).toString(16).padStart(64, "0")}`;
  env[config.da.databasePasswordEnv] = "writer-test-password";
  env[config.da.readerPasswordEnv] = "reader-test-password";
  const envText = () =>
    Object.entries(env)
      .map(([key, value]) => `${key}=${value}`)
      .join("\n");
  await writeFile(config.envFile, envText());
  const path = join(directory, "stack.json");
  const writeConfig = () => writeFile(path, JSON.stringify(config));
  await writeConfig();
  return { config, directory, env, envText, path, release, writeConfig };
}

describe("stack intent", () => {
  it("binds deployment identity", async () => {
    const value = await fixture();
    const first = (await loadStackConfig(value.path)).intentDigest;
    expect(first).toMatch(/^[0-9a-f]{64}$/);
    value.env[value.config.wallets.user!.seedEnv] = entropyToMnemonic(
      "a2".repeat(16),
    );
    await writeFile(value.config.envFile, value.envText());
    const second = (await loadStackConfig(value.path)).intentDigest;
    expect(second).not.toBe(first);
    const watcherEnv = parse(
      await readFile(value.config.watcher.composeEnvFile),
    );
    await writeFile(watcherEnv.WATCHER_RECORD_KEY_FILE!, "c".repeat(64));
    expect((await loadStackConfig(value.path)).intentDigest).not.toBe(second);
  });
  it("lets operational settings change between runs", async () => {
    const value = await fixture();
    const first = (await loadStackConfig(value.path)).intentDigest;
    value.config.timeoutMs += 1;
    value.config.journey.cycles += 1;
    value.config.wallets.user!.minimumLovelace = "900000000";
    await value.writeConfig();
    value.env.MIN_FEE_A = "11";
    await writeFile(value.config.envFile, value.envText());
    await writeFile(value.config.watcher.bearerFile, "b".repeat(64));
    expect((await loadStackConfig(value.path)).intentDigest).toBe(first);
  });
});
it("permits measured profiles to be corrected before signing while binding the signer and programs", async () => {
  const value = await fixture();
  const first = await loadStackConfig(value.path);
  value.release.fundingProfiles.push({
    measured: "deployment-specific measurement",
  });
  await writeFile(
    value.config.watcher.releaseInput!,
    JSON.stringify(value.release),
  );
  expect((await loadStackConfig(value.path)).intentDigest).toBe(
    first.intentDigest,
  );
  value.release.programCommitments.program = "b".repeat(64);
  await writeFile(
    value.config.watcher.releaseInput!,
    JSON.stringify(value.release),
  );
  expect((await loadStackConfig(value.path)).intentDigest).not.toBe(
    first.intentDigest,
  );
});
it("rejects failover, wrong Compose ports and invalid wallet seeds before spending", async () => {
  const value = await fixture();
  value.env.L1_PROVIDER_FAILOVER = "remote";
  await writeFile(value.config.envFile, value.envText());
  await expect(loadStackConfig(value.path)).rejects.toThrow("no failover");
  delete value.env.L1_PROVIDER_FAILOVER;
  value.env.L1_KUPO_KEY = "http://127.0.0.1:9999";
  await writeFile(value.config.envFile, value.envText());
  await expect(loadStackConfig(value.path)).rejects.toThrow(
    "Compose provider port",
  );
  value.env.L1_KUPO_KEY = "http://127.0.0.1:1442";
  value.env[value.config.wallets.user!.seedEnv] = "invalid wallet words";
  await writeFile(value.config.envFile, value.envText());
  await expect(loadStackConfig(value.path)).rejects.toThrow(
    "Invalid wallet mnemonic",
  );
});

it("assigns the same sorted committee indexes independently of input order", async () => {
  const value = await fixture();
  const first = await configureStackCommittee(value.config, value.env);
  value.config.da.members.reverse();
  const second = await configureStackCommittee(value.config, value.env);
  expect(second).toEqual(first);
  expect(first.map((member) => member.signerIndex)).toEqual([0, 1]);
  expect(first.map((member) => member.daVkey).join("")).toBe(
    value.env.DA_COMMITTEE_HEX,
  );
});

it("refuses duplicate committee keys, governed threshold violations and malformed owners before deployment", async () => {
  const value = await fixture();
  const seedEnv = value.config.da.members[1]!.seedEnv;
  const original = value.env[seedEnv]!;
  value.env[seedEnv] = value.env[value.config.da.members[0]!.seedEnv]!;
  await expect(
    configureStackCommittee(value.config, value.env),
  ).rejects.toThrow("distinct signing keys");
  value.env[seedEnv] = original;
  value.env.DA_THRESHOLD = "1";
  await expect(
    configureStackCommittee(value.config, value.env),
  ).rejects.toThrow("Invalid DA threshold configuration");
  value.env.DA_THRESHOLD = "2";
  value.env.DA_OWNERS_HEX = "ff";
  await expect(
    configureStackCommittee(value.config, value.env),
  ).rejects.toThrow("Invalid DA owner configuration");
});

it("accepts L1_PROVIDER_FAILOVER written as off", async () => {
  for (const off of ["false", "", " FALSE "]) {
    const value = await fixture();
    value.env.L1_PROVIDER_FAILOVER = off;
    await writeFile(value.config.envFile, value.envText());
    await expect(loadStackConfig(value.path)).resolves.toHaveProperty(
      "intentDigest",
    );
  }
});

type Fixture = Awaited<ReturnType<typeof fixture>>;
describe("configuration refused before any build or spend", () => {
  const seedOf = (value: Fixture, role: string) =>
    value.config.wallets[role]!.seedEnv;
  const rows: [string, (value: Fixture) => void | Promise<void>, string][] = [
    [
      "a deposit that cannot cover the transfer and fees",
      ({ config }) => {
        config.journey.depositLovelace = config.journey.transferLovelace;
      },
      "Deposit must cover",
    ],
    [
      "overlapping DA ports",
      ({ config }) => {
        config.da.ports.database = config.da.ports.committeeApiBase;
      },
      "DA ports must be distinct",
    ],
    [
      "a DA port beyond TCP",
      ({ config }) => {
        config.da.ports.committeeTransportBase = 65535;
      },
      "DA ports must be distinct",
    ],
    [
      "a DA port an operator service publishes",
      ({ config }) => {
        config.da.ports.database = 1442;
      },
      "collides with KUPO_PORT",
    ],
    [
      "a DA threshold above the committee size",
      ({ env }) => {
        env.DA_THRESHOLD = "3";
      },
      "Invalid DA threshold configuration",
    ],
    [
      "a zero DA threshold",
      ({ env }) => {
        env.DA_THRESHOLD = "0";
      },
      "DA_THRESHOLD must be a positive integer",
    ],
    [
      "a non-numeric DA threshold",
      ({ env }) => {
        env.DA_THRESHOLD = "two";
      },
      "DA_THRESHOLD must be a positive integer",
    ],
    [
      "equal DA database passwords",
      ({ config, env }) => {
        env[config.da.readerPasswordEnv] = env[config.da.databasePasswordEnv]!;
      },
      "passwords must differ",
    ],
    [
      "a container file transport",
      ({ config, env }) => {
        env[config.da.members[0]!.transportEnv] = "file:/run/secrets/key";
      },
      "persistent seed/hex transport",
    ],
    [
      "a user budget below every deposit",
      ({ config }) => {
        config.wallets.user!.minimumLovelace = config.journey.depositLovelace;
      },
      "User funding budget",
    ],
    [
      "two wallet roles sharing a key",
      (value) => {
        value.env[seedOf(value, "recipient")] =
          value.env[seedOf(value, "user")]!;
      },
      "shares a payment key",
    ],
    [
      "two DA services sharing a transport identity",
      ({ config, env }) => {
        env[config.da.members[1]!.transportEnv] =
          env[config.da.members[0]!.transportEnv]!;
      },
      "distinct persistent identities",
    ],
    [
      "a preset committee that differs from the members",
      ({ env }) => {
        env.DA_COMMITTEE_HEX = "ab".repeat(32);
      },
      "differ from the configured sorted committee",
    ],
    [
      "an env file other than the node directory's",
      async ({ config, directory, envText }) => {
        config.envFile = join(directory, "other.env");
        await writeFile(config.envFile, envText());
      },
      "envFile must be the node directory's .env",
    ],
    [
      "a DA member seed that shadows a node wallet",
      ({ config, env }) => {
        config.da.members[0]!.seedEnv = "L1_OPERATOR_SEED_PHRASE";
        env.L1_OPERATOR_SEED_PHRASE = entropyToMnemonic("10".repeat(16));
      },
      "must start with STACK_",
    ],
    [
      "one variable naming two stack secrets",
      ({ config }) => {
        config.da.members[1]!.transportEnv = config.da.members[0]!.transportEnv;
      },
      "Stack secret variables must be distinct",
    ],
    [
      "a run directory inside the Docker build context",
      ({ config, directory }) => {
        config.runDirectory = join(directory, "preprod-stack");
      },
      "inside the Docker build context",
    ],
    [
      "the shared test Postgres port",
      ({ env }) => {
        env.MIDGARD_POSTGRES_HOST_PORT = "5433";
      },
      "MIDGARD_POSTGRES_HOST_PORT",
    ],
    [
      "no explicit Postgres port",
      ({ env }) => {
        delete env.MIDGARD_POSTGRES_HOST_PORT;
      },
      "MIDGARD_POSTGRES_HOST_PORT",
    ],
    [
      "a node environment that sets a host tool variable",
      ({ env }) => {
        env.NODE_OPTIONS = "--require /tmp/hook.js";
      },
      "must not set host variable NODE_OPTIONS",
    ],
  ];
  it.each(rows)("refuses %s", async (_, mutate, message) => {
    const value = await fixture();
    await mutate(value);
    await writeFile(value.config.envFile, value.envText());
    await value.writeConfig();
    await expect(loadStackConfig(value.path)).rejects.toThrow(message);
  });
});

describe("--check", () => {
  it("runs the offline environment checks and refuses a shared wallet key", async () => {
    const value = await fixture();
    const log = vi.spyOn(console, "log").mockImplementation(() => {});
    try {
      await runStackController({ config: value.path, check: true });
      expect(log).toHaveBeenCalledOnce();
      value.env[value.config.wallets.recipient!.seedEnv] =
        value.env[value.config.wallets.user!.seedEnv]!;
      await writeFile(value.config.envFile, value.envText());
      await expect(
        runStackController({ config: value.path, check: true }),
      ).rejects.toThrow("shares a payment key");
    } finally {
      log.mockRestore();
    }
  });
  it("refuses a relative configuration path", () =>
    expect(
      runStackController({ config: "stack.json", check: true }),
    ).rejects.toThrow("--config must be an absolute path"));
});
