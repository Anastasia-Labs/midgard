import { generateKeyPairSync } from "node:crypto";
import { mkdir, mkdtemp, readFile, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { fileURLToPath } from "node:url";

import { paymentCredentialOf, walletFromSeed } from "@lucid-evolution/lucid";
import { entropyToMnemonic } from "bip39";

import { parseStackConfig } from "../src/full-stack/config.js";

const operatorCompose = fileURLToPath(
  new URL("../../midgard-node/scripts/operator-compose.mjs", import.meta.url),
);
const directories: string[] = [];
/** Removes every configuration fixture; call it after each test. */
export async function removeConfigFixtures() {
  await Promise.all(
    directories
      .splice(0)
      .map((path) => rm(path, { recursive: true, force: true })),
  );
}

/** A complete preprod-testing stack configuration and node environment from an example. */
export async function configFixture(example = "preprod-stack.example.json") {
  const directory = await mkdtemp(join(tmpdir(), "midgard-stack-config-"));
  directories.push(directory);
  const sample = JSON.parse(
    await readFile(new URL(`../config/${example}`, import.meta.url), "utf8"),
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
  const secretNames = [
    "WATCHER_ROLLBACK_KEY_FILE",
    "WATCHER_PROVER_KEY_FILE",
    "WATCHER_AVAILABILITY_KEY_FILE",
  ];
  const secretFiles = Object.fromEntries(
    secretNames.map((key, index) => [key, join(directory, `secret-${index}`)]),
  );
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
    RUN_GENESIS_ON_STARTUP: "false",
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
