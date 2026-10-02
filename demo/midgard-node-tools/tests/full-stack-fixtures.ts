import { mkdtemp, readFile, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { entropyToMnemonic } from "bip39";

import {
  parseStackConfig,
  type StackConfig,
} from "../src/full-stack/config.js";
import { StackProcesses } from "../src/full-stack/process.js";

export type RecordedCall = {
  id: string;
  args: readonly string[];
  overrides: Record<string, string>;
  env: Record<string, string>;
  scope: "stack" | "host";
};
/** Records every command instead of running it; responses are keyed by command id. */
export class RecordingProcesses extends StackProcesses {
  calls: RecordedCall[] = [];
  responses: Record<string, unknown | ((call: RecordedCall) => unknown)> = {};
  override async command(
    id: string,
    _command: string,
    args: readonly string[],
    overrides: Record<string, string> = {},
    _cwd?: string,
    scope: "stack" | "host" = "stack",
  ) {
    const call = { id, args, overrides, env: { ...this.env }, scope };
    this.calls.push(call);
    const response = this.responses[id];
    return typeof response === "function"
      ? await response(call)
      : (response ?? null);
  }
  override async hostDatabaseIdentity() {
    this.calls.push({
      id: "host-database-identity",
      args: [],
      overrides: {},
      env: { ...this.env },
      scope: "host",
    });
    return this.responses["host-database-identity"] as string;
  }
}

const temporary: string[] = [];
export async function removeStackFixtures() {
  await Promise.all(
    temporary
      .splice(0)
      .map((path) => rm(path, { recursive: true, force: true })),
  );
}
/** The example configuration rooted in a fresh temporary directory. */
export async function stackFixture(
  edit: (sample: Record<string, any>, directory: string) => void = () => {},
) {
  const directory = await mkdtemp(join(tmpdir(), "midgard-stack-fixture-"));
  temporary.push(directory);
  const sample = JSON.parse(
    await readFile(
      new URL("../config/preprod-stack.example.json", import.meta.url),
      "utf8",
    ),
  );
  Object.assign(sample, {
    nodeRoot: directory,
    envFile: join(directory, ".env"),
    runDirectory: join(directory, "run"),
  });
  Object.assign(sample.watcher, {
    bearerFile: join(directory, "bearer"),
    composeEnvFile: join(directory, "watcher.env"),
    configDirectory: join(directory, "run/watcher"),
  });
  edit(sample, directory);
  return { directory, config: parseStackConfig(sample) };
}
/** Distinct keys for every configured secret, as a valid stack environment has. */
export function stackEnvironment(config: StackConfig) {
  const env: Record<string, string> = {
    DA_THRESHOLD: String(config.da.members.length),
    DA_COSIGNER_SEED_PHRASE: entropyToMnemonic("ff".repeat(16)),
    MIDGARD_POSTGRES_HOST_PORT: "25433",
    [config.da.producerTransportEnv]: `seed:${"01".repeat(32)}`,
    [config.da.retainedTransportEnv]: `seed:${"02".repeat(32)}`,
    [config.da.databasePasswordEnv]: "writer-test-password",
    [config.da.readerPasswordEnv]: "reader-test-password",
  };
  Object.values(config.wallets).forEach((wallet, index) => {
    env[wallet.seedEnv] = entropyToMnemonic(
      (index + 1).toString(16).padStart(32, "0"),
    );
  });
  config.da.members.forEach((member, index) => {
    env[member.seedEnv] = entropyToMnemonic(
      (index + 0x40).toString(16).padStart(32, "0"),
    );
    env[member.transportEnv] =
      `seed:${(index + 0x40).toString(16).padStart(64, "0")}`;
  });
  return env;
}
