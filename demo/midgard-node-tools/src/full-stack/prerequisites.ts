import { createHash } from "node:crypto";
import { readFile } from "node:fs/promises";
import { join, resolve } from "node:path";

import { parse } from "dotenv";

import { readJsonIfPresent } from "./journal.js";
import {
  configureHostNativeLedger,
  nativeLedgerPaths,
  shareWithContainers,
} from "./native-ledger.js";
import type { StackProcesses } from "./process.js";

/** `compose ps --format json` prints one JSON object per line only from Compose 2.21. */
export function assertComposeVersion(report: unknown) {
  const version = (report as { version?: unknown } | null)?.version;
  const match =
    typeof version === "string" ? /^v?(\d+)\.(\d+)\./.exec(version) : null;
  const [major, minor] = match ? [Number(match[1]), Number(match[2])] : [0, 0];
  if (major < 2 || (major === 2 && minor < 21))
    throw new Error(
      `Docker Compose 2.21 or newer is required; found ${String(version)}`,
    );
}

export async function prepareStackPrerequisites(processes: StackProcesses) {
  // loadStackConfig ran every offline check; these need the fresh builds.
  const watcherEnv = parse(
    await readFile(processes.config.watcher.composeEnvFile),
  );
  const workspace = resolve(processes.config.nodeRoot, "..");
  assertComposeVersion(
    await processes.command(
      "compose-version",
      "docker",
      ["compose", "version", "--format", "json"],
      {},
      workspace,
      "host",
    ),
  );
  await processes.command(
    "contracts-build",
    "pnpm",
    ["deployment:build", "preprod-testing"],
    {},
    workspace,
    "host",
  );
  for (const name of [
    "@al-ft/lucid-midgard",
    "@al-ft/l1-node-transport",
    "@al-ft/midgard-core",
    "@al-ft/midgard-sdk",
    "@al-ft/midgard-validation",
    "@al-ft/midgard-fault-proofs",
    // The node's L1 follower, and the origin scan of the deployment steps.
    "@al-ft/midgard-l1-follower",
    "midgard-node",
    "midgard-watcher",
  ])
    await processes.command(
      `build-${name.replaceAll("/", "-")}`,
      "pnpm",
      ["--filter", name, "build"],
      {},
      workspace,
      "host",
    );
  await processes.command(
    "native-l1-node-transport-build",
    "pnpm",
    ["--filter", "@al-ft/l1-node-transport", "native:build"],
    { CGO_ENABLED: "0", GOTOOLCHAIN: "go1.26.5" },
    workspace,
    "host",
  );
  // The node and DA containers mount this binary and run it as their own users.
  await shareWithContainers(nativeLedgerPaths(processes).binary, true);
  await processes.command(
    "native-owner-build",
    "pnpm",
    ["--filter", "midgard-node", "native:mpf-owner:build"],
    {},
    workspace,
    "host",
  );
  const owner = join(
    processes.config.nodeRoot,
    "native/mpf-event-flat-wasm/target/release/architecture-g-owner",
  );
  processes.env.MPF_NATIVE_OWNER_BINARY_PATH = owner;
  processes.env.MPF_NATIVE_OWNER_BINARY_SHA256 = createHash("sha256")
    .update(await readFile(owner))
    .digest("hex");
  configureHostNativeLedger(processes);
  await processes.compose("base-compose-check", ["config", "--quiet"]);
  const { readReleaseInput, releasePaths } = await import("./release.js");
  const {
    loadWatcherSecretText,
    decodeWatcherAuthenticationKey32,
    decodeWatcherHttpBearerSecret,
  } = await import("midgard-watcher");
  for (const key of ["WATCHER_RECORD_KEY_FILE", "WATCHER_ROLLBACK_KEY_FILE"])
    decodeWatcherAuthenticationKey32(
      await loadWatcherSecretText({
        kind: "file",
        path: watcherEnv[key]!,
      }),
    );
  decodeWatcherHttpBearerSecret(
    await loadWatcherSecretText({
      kind: "file",
      path: watcherEnv.WATCHER_BEARER_FILE!,
    }),
  );
  const { walletFromSeed } = await import("@lucid-evolution/lucid");
  for (const [role, key] of [
    ["prover", "WATCHER_PROVER_KEY_FILE"],
    ["availability", "WATCHER_AVAILABILITY_KEY_FILE"],
  ]) {
    const actual = await loadWatcherSecretText({
      kind: "file",
      path: watcherEnv[key!]!,
    });
    const expected = processes.env[processes.config.wallets[role!]!.seedEnv]!;
    if (
      walletFromSeed(actual, {
        network: "Preprod",
        addressType: "Enterprise",
      }).address !==
      walletFromSeed(expected, {
        network: "Preprod",
        addressType: "Enterprise",
      }).address
    )
      throw new Error(`Watcher ${role} secret differs from its funded wallet`);
  }
  const paths = await releasePaths(processes);
  if ((await readJsonIfPresent(paths.authority)) === undefined) {
    if (!processes.config.watcher.releaseInput)
      throw new Error("Fresh setup needs measured watcher release inputs");
    await readReleaseInput(processes.config.watcher.releaseInput);
  }
  return { profile: "preprod-testing", inputsChecked: true };
}
