import { chmod, mkdir, readdir } from "node:fs/promises";
import { join, resolve } from "node:path";

import type { StackProcesses } from "./process.js";

export function nativeLedgerPaths(processes: StackProcesses) {
  return {
    directory: join(processes.config.watcher.configDirectory, "cardano"),
    socketDirectory: join(processes.config.nodeRoot, "cardano/ipc"),
    binary: resolve(
      processes.config.nodeRoot,
      "../midgard-watcher/dist/native/midgard-chain-sync",
    ),
  };
}
/** The one chain-sync binary, built on the host with the pinned Go toolchain. */
const CONTAINER_CHAIN_SYNC_BINARY = "/app/native-ledger/midgard-chain-sync";
export function containerChainSync(processes: StackProcesses) {
  return {
    path: CONTAINER_CHAIN_SYNC_BINARY,
    volume: `${nativeLedgerPaths(processes).binary}:${CONTAINER_CHAIN_SYNC_BINARY}:ro`,
  };
}
/**
 * Containers run as their own users, and the controller's umask makes every
 * file it creates private; these public files must stay readable to them.
 */
export async function shareWithContainers(path: string, executable = false) {
  const entries = await readdir(path, { withFileTypes: true }).catch(
    (error: NodeJS.ErrnoException) => {
      if (error.code === "ENOTDIR") return undefined;
      throw error;
    },
  );
  await chmod(path, entries !== undefined || executable ? 0o755 : 0o644);
  for (const entry of entries ?? [])
    if (!entry.isSymbolicLink())
      await shareWithContainers(join(path, entry.name));
}
export function configureHostNativeLedger(processes: StackProcesses) {
  const paths = nativeLedgerPaths(processes);
  const env = {
    L1_NODE_SOCKET_PATH: join(paths.socketDirectory, "node.socket"),
    L1_NODE_CONFIG_PATH: join(paths.directory, "config.json"),
    L1_NATIVE_CHAIN_SYNC_BINARY_PATH: paths.binary,
  };
  Object.assign(processes.env, env);
  return env;
}
export async function exportLocalCardanoConfig(processes: StackProcesses) {
  const paths = nativeLedgerPaths(processes);
  await mkdir(paths.directory, { recursive: true, mode: 0o700 });
  await processes.compose("cardano-config-export", [
    "cp",
    "cardano-node:/opt/cardano/config/preprod/.",
    paths.directory,
  ]);
  await shareWithContainers(paths.directory);
}
export function containerNativeLedger(processes: StackProcesses) {
  const paths = nativeLedgerPaths(processes);
  return {
    env: {
      L1_NODE_SOCKET_PATH: "/ipc/node.socket",
      L1_NODE_CONFIG_PATH: "/cardano-config/config.json",
      L1_NATIVE_CHAIN_SYNC_BINARY_PATH: containerChainSync(processes).path,
    },
    volumes: [
      `${paths.socketDirectory}:/ipc`,
      `${paths.directory}:/cardano-config:ro`,
      containerChainSync(processes).volume,
    ],
  };
}
