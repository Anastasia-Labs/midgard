import { generateKeyPairSync } from "node:crypto";
import { mkdir, mkdtemp, readFile, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { paymentCredentialOf, walletFromSeed } from "@lucid-evolution/lucid";
import { entropyToMnemonic } from "bip39";
import { afterEach, expect, it } from "vitest";

import { configureStackCommittee } from "../src/full-stack/committee.js";
import { loadStackConfig, parseStackConfig } from "../src/full-stack/config.js";

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
  sample.runDirectory = join(directory, "run");
  await mkdir(join(directory, "scripts"));
  // The wrapper prints no derived overrides in the main checkout.
  await writeFile(
    join(directory, "scripts/operator-compose.sh"),
    "#!/bin/sh\nexit 0\n",
  );
  for (const key of Object.keys(sample.watcher))
    if (key !== "configDirectory" && key !== "releaseDirectory")
      sample.watcher[key] = join(directory, key);
  sample.watcher.configDirectory = join(directory, "run/watcher");
  sample.watcher.releaseDirectory = join(directory, "run/release");
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
  for (const key of [
    config.da.producerTransportEnv,
    config.da.retainedTransportEnv,
    ...config.da.members.map((member) => member.transportEnv),
  ])
    env[key] = `seed:${"01".repeat(32)}`;
  env[config.da.databasePasswordEnv] = "writer-test-password";
  env[config.da.readerPasswordEnv] = "reader-test-password";
  const envText = () =>
    Object.entries(env)
      .map(([key, value]) => `${key}=${value}`)
      .join("\n");
  await writeFile(config.envFile, envText());
  const path = join(directory, "stack.json");
  await writeFile(path, JSON.stringify(config));
  return { config, env, envText, path, release };
}

it("binds wallet and watcher secrets without copying their values into the digest", async () => {
  const value = await fixture();
  const first = await loadStackConfig(value.path);
  expect(first.intentDigest).toMatch(/^[0-9a-f]{64}$/);
  value.env[value.config.wallets.user!.seedEnv] = entropyToMnemonic(
    "02".repeat(16),
  );
  await writeFile(value.config.envFile, value.envText());
  expect((await loadStackConfig(value.path)).intentDigest).not.toBe(
    first.intentDigest,
  );
  await writeFile(value.config.watcher.bearerFile, "b".repeat(64));
  expect((await loadStackConfig(value.path)).intentDigest).not.toBe(
    first.intentDigest,
  );
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
