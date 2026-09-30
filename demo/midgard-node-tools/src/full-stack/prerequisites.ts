import { createHash } from "node:crypto";
import { access, readFile } from "node:fs/promises";
import { join, resolve } from "node:path";

import { parse } from "dotenv";

import { readJsonIfPresent } from "./journal.js";
import { configureHostNativeLedger } from "./native-ledger.js";
import type { StackProcesses } from "./process.js";

export async function prepareStackPrerequisites(processes: StackProcesses) {
  // All input checks precede any on-chain spending.
  const watcherEnv = parse(
    await readFile(processes.config.watcher.composeEnvFile),
  );
  for (const key of [
    "WATCHER_RECORD_KEY_FILE",
    "WATCHER_ROLLBACK_KEY_FILE",
    "WATCHER_PROVER_KEY_FILE",
    "WATCHER_AVAILABILITY_KEY_FILE",
    "WATCHER_BEARER_FILE",
  ])
    await access(watcherEnv[key] ?? "");
  // The canonical Compose file uses this env_file even for provider-only startup.
  if (
    resolve(processes.config.envFile) !==
    join(processes.config.nodeRoot, ".env")
  )
    throw new Error(
      "envFile must be the node directory's .env used by the existing Compose stack",
    );
  const workspace = resolve(processes.config.nodeRoot, "..");
  await processes.command(
    "contracts-build",
    "pnpm",
    ["deployment:build", "preprod-testing"],
    {},
    workspace,
  );
  for (const name of [
    "@al-ft/lucid-midgard",
    "@al-ft/midgard-core",
    "@al-ft/midgard-sdk",
    "@al-ft/midgard-validation",
    "@al-ft/midgard-fault-proofs",
    "midgard-node",
    "midgard-watcher",
  ])
    await processes.command(
      `build-${name.replaceAll("/", "-")}`,
      "pnpm",
      ["--filter", name, "build"],
      {},
      workspace,
    );
  await processes.command(
    "native-chain-sync-build",
    "pnpm",
    ["--filter", "midgard-watcher", "native:build"],
    { CGO_ENABLED: "0", GOTOOLCHAIN: "go1.25.7" },
    workspace,
  );
  await processes.command(
    "native-owner-build",
    "pnpm",
    ["--filter", "midgard-node", "native:mpf-owner:build"],
    {},
    workspace,
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
  const { deriveStackWallets } = await import("./wallets.js");
  deriveStackWallets(processes.config, processes.env);
  const { configureStackCommittee } = await import("./committee.js");
  await configureStackCommittee(processes.config, processes.env);
  const { loadDaLibp2pIdentity } = await import(
    "@al-ft/midgard-core/da-libp2p-identity"
  );
  const peers = await Promise.all(
    [
      processes.config.da.producerTransportEnv,
      processes.config.da.retainedTransportEnv,
      ...processes.config.da.members.map((member) => member.transportEnv),
    ].map((key) => loadDaLibp2pIdentity(processes.env[key]!)),
  );
  if (new Set(peers.map((peer) => peer.peerId)).size !== peers.length)
    throw new Error(
      "DA producer, retained service and members need distinct persistent identities",
    );
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
